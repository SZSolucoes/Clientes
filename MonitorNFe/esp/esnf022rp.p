/*******************************************************************************
**
**       Programa: esp/esnf021rp.p
**       Data....: 28/07/2016.
**       Autor...: SZ Solucoes.
**       Objetivo: Verifica‡Æo da Situa‡Æo do XML conforme parametro de dias.
**   
*******************************************************************************/
{include/i-prgvrs.i ESNF021 2.12.00.001}  /*** 010001 ***/

{utp/ut-glob.i}
{utp/utapi019.i}
{METHOD/dbotterr.i} /* definicao RowErrors */

DEFINE TEMP-TABLE tt-param NO-UNDO
    FIELD destino          AS INTEGER
    FIELD arquivo          AS CHAR FORMAT "x(35)"
    FIELD usuario          AS CHAR FORMAT "x(12)"
    FIELD data-exec        AS DATE
    FIELD hora-exec        AS INTEGER
    FIELD classifica       AS INTEGER
    FIELD desc-classifica  AS CHAR FORMAT "x(40)"
    FIELD modelo-rtf       AS CHAR FORMAT "x(35)"
    FIELD l-habilitaRtf    AS LOG
    FIELD dt-emissao-ini   AS DATE
    FIELD dt-emissao-fim   AS DATE.

DEFINE TEMP-TABLE tt-digita NO-UNDO
    FIELD ordem            AS INTEGER   FORMAT ">>>>9"
    FIELD exemplo          AS CHARACTER FORMAT "x(30)"
    INDEX id ordem.

DEFINE NEW GLOBAL SHARED VARIABLE v_log_eai_habilit AS LOG NO-UNDO.

DEFINE VARIABLE h-axsep004            AS HANDLE      NO-UNDO.
DEFINE VARIABLE l-spp-nfe             AS LOGICAL     NO-UNDO.
DEFINE VARIABLE l-funcao-tss-sincrono AS LOGICAL     NO-UNDO.

DEF VAR c-destino           AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR c-arquivo           AS CHAR FORMAT "x(40)" NO-UNDO.
DEF VAR c-titulo            AS CHAR FORMAT "x(25)" NO-UNDO. 
DEF NEW SHARED VAR c-modulo AS CHAR FORMAT "x(18)" NO-UNDO. 
DEF VAR c-aux-arq           AS CHAR                NO-UNDO.
DEF VAR l-reprocessado      AS LOGICAL             NO-UNDO.
DEF VAR c-situacao          AS CHAR                NO-UNDO.
DEF VAR c-dest-email        AS CHARACTER           NO-UNDO.
DEF VAR h-boes003           AS HANDLE              NO-UNDO.

DEFINE TEMP-TABLE tt-nfe003 NO-UNDO LIKE nfe003
       FIELD r-rowid as rowid.

DEF TEMP-TABLE tt_log_erro  NO-UNDO 
    FIELD ttv_num_cod_erro  AS INTEGER   INITIAL ?
    FIELD ttv_des_msg_ajuda AS CHARACTER INITIAL ?
    FIELD ttv_des_msg_erro  AS CHARACTER INITIAL ?.

DEF TEMP-TABLE tt-raw-digita FIELD raw-digita AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

FOR EACH tt-raw-digita:       
   CREATE tt-digita.  
   RAW-TRANSFER raw-digita TO tt-digita.
END.

{include/i-rpvar.i}

ASSIGN c-programa = "ESNF022"
       c-versao   = " "
       c-revisao  = " ".

FIND tt-param NO-ERROR.

ASSIGN 
    c-destino      = {varinc/var00002.i 04 tt-param.destino}
    c-arquivo      = tt-param.arquivo
    c-titulo-relat = c-titulo
    c-sistema      = c-modulo.

{include/i-rpcab.i}
{include/i-rpout.i}

VIEW FRAME f-cabec.
VIEW FRAME f-rodape.

DEF STREAM s-out.

ASSIGN c-aux-arq = SESSION:TEMP-DIR + "lista_Xml_Importados_Cancelados" + STRING(TIME) + ".csv".

EMPTY TEMP-TABLE tt-nfe003.

/* Gera temp-table com Xmls importados no periodo informado*/
FOR EACH nfe003 NO-LOCK
    WHERE nfe003.idi-orig-trad  = 2
      /*AND nfe003.idi-situacao   <> 5*/
      AND nfe003.dt-emissao    >= tt-param.dt-emissao-ini
      AND nfe003.dt-emissao    <= tt-param.dt-emissao-fim:

    /* Quando o xml de industrializacao vem com 2 cfops e gerado 2 documentos acrescentando o 9 no inicio da chave de acesso */
    if substring(nfe003.ch-acesso-comp-nfe,1,1) = "9" then
        next.

    CREATE tt-nfe003.
    BUFFER-COPY nfe003 TO tt-nfe003.
    ASSIGN tt-nfe003.r-rowid = ROWID(nfe003).

END.

FOR EACH tt-nfe003:

    ASSIGN l-spp-nfe = CAN-FIND(FIRST funcao WHERE 
                                      funcao.cd-funcao = "SPP-NFE":U AND funcao.ativo = YES).
      
    ASSIGN l-funcao-tss-sincrono = CAN-FIND(FIRST funcao NO-LOCK
                                            WHERE funcao.cd-funcao = "SPP-INTEG-TSS-SINCRONO":U
                                              AND funcao.ativo).
    
    IF l-spp-nfe AND  (v_log_eai_habilit OR l-funcao-tss-sincrono) THEN DO:

       RUN adapters/xml/ep2/axsep004.p PERSISTENT SET h-axsep004.
       
    END.
    FIND FIRST estabelec NO-LOCK 
         WHERE estabelec.cod-estabel = tt-nfe003.cod-estabel NO-ERROR.
    
    EMPTY TEMP-TABLE tt_log_erro.
    IF VALID-HANDLE(h-axsep004) AND (v_log_eai_habilit OR l-funcao-tss-sincrono) THEN DO:
        RUN pi-Recebe-Versao-Nfe IN h-axsep004 (INPUT TRIM(SUBSTRING(estabelec.char-1,173,10))). 

        RUN pi-Recebe-Estab IN h-axsep004 (tt-nfe003.cod-estabel).
        
        RUN piUpsert IN h-axsep004 (INPUT "upd":U, 
                                    INPUT tt-nfe003.ch-acesso-comp-nfe, /*Chave NFE*/
                                    OUTPUT TABLE tt_log_erro).
    END.
    

    FOR each tt_log_erro:
       
        FOR FIRST nfe003 EXCLUSIVE-LOCK
            WHERE ROWID(nfe003) = tt-nfe003.r-rowid.
            
            IF  TT_LOG_ERRO.TTV_DES_MSG_ERRO BEGINS "(100) Autorizado o uso da NF-e" then do: 

                ASSIGN nfe003.sit-sefaz = 1.
                
            end.
            ELSE do:
                
                ASSIGN nfe003.sit-sefaz = 2.

                FIND FIRST nfe019 NO-LOCK NO-ERROR.

                IF AVAIL nfe019
                    AND nfe019.email <> "" THEN DO:

                    RUN pi-EnviaEmail.
                    
                END.
            end.
        END.
    END.
    IF VALID-HANDLE(h-axsep004) THEN 
        DELETE PROCEDURE h-axsep004.
    
END.


PROCEDURE pi-EnviaEmail:

    DEFINE VARIABLE c-assunto       AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE c-destino-email AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE c-email-empresa AS CHARACTER   NO-UNDO.

    def var c-subject        as char format "X(255)"    no-undo.    
    def var c-body-mail      as char format "X(255)"    no-undo.

    EMPTY TEMP-TABLE tt-envio2.
    EMPTY TEMP-TABLE tt-mensagem.    

    FOR FIRST param_email FIELDS(cod_servid_e_mail  num_porta log_servid_exchange) NO-LOCK: END.

    find emitente no-lock
        where emitente.cod-emitente = nfe003.cod-emitente no-error.

    if avail emitente then 
        assign c-assunto = "XML com Uso NÆo Autorizado, Fornecedor:" + string(emitente.nome-abrev).
    else
        assign c-assunto = "XML com Uso NÆo Autorizado!".
    
    
    ASSIGN c-email-empresa = "nfe@ynowa.com.br" . /* verificar e-mail empresa*/

    FIND FIRST nfe019 NO-LOCK NO-ERROR.

        ASSIGN c-destino-email = nfe019.email.

    IF  c-destino-email <> "":U THEN DO:

        CREATE tt-envio2.
        ASSIGN tt-envio2.versao-integracao = 1
               tt-envio2.exchange          = no
               tt-envio2.remetente         = c-email-empresa
               tt-envio2.destino           = c-destino-email  
               tt-envio2.copia             = ""
               tt-envio2.assunto           = c-assunto
               tt-envio2.importancia       = 2
               tt-envio2.formato           = "Texto"
               tt-envio2.arq-anexo         = "".

        ASSIGN c-body-mail = "Emitente: " + STRING(nfe003.cod-emitente) + " " + "Nota Fiscal: " + STRING(nfe003.nro-docto) + " " + "Serie: " + STRING(nfe003.serie) + " " + "Situa‡Æo:" + STRING("XML com Uso NÆo Autorizado") + " " + "Chave: " + STRING(nfe003.ch-acesso-comp-nfe) + CHR(10).

        CREATE tt-mensagem.
        ASSIGN tt-mensagem.seq-mensagem = 1
               tt-mensagem.mensagem     = c-body-mail.

        RUN pi-blat(INPUT TABLE tt-envio2).
        
    END.

END PROCEDURE.

PROCEDURE pi-blat:
    DEFINE INPUT PARAM TABLE FOR tt-envio2.
    
    DEF VAR c-arquivo AS CHAR NO-UNDO.
    DEF VAR c-bat     AS CHAR NO-UNDO.
    DEFINE VARIABLE v-cod-lin-comando AS CHARACTER NO-UNDO.
    
    FIND FIRST tt-envio2 NO-ERROR.
    IF NOT AVAIL tt-envio2 THEN 
        RETURN "NOK".

    FOR FIRST param_email FIELDS(cod_servid_e_mail  num_porta log_servid_exchange) NO-LOCK: END.

    ASSIGN c-arquivo = "dtsemail" + STRING(ETIME) + ".txt"
           c-bat     = "dtsemail" + STRING(ETIME) + ".bat".

    ASSIGN tt-envio2.destino = REPLACE(tt-envio2.destino,CHR(10),",")       
           tt-envio2.copia = REPLACE(tt-envio2.copia,CHR(10),",")
           tt-envio2.destino = REPLACE(tt-envio2.destino,";",",")       
           tt-envio2.copia = REPLACE(tt-envio2.copia,";",",").

    ASSIGN tt-envio2.assunto = REPLACE(tt-envio2.assunto,"%","%%").
    
    IF tt-envio2.assunto = "" OR tt-envio2.assunto = ? THEN DO:
        {utp/ut-liter.i Conte£do_do_assunto * R}
        RUN utp/ut-msgs.p (INPUT "msg", INPUT 54, INPUT RETURN-VALUE).
        RUN pi-criar-erros (INPUT 54, INPUT RETURN-VALUE, INPUT "").
        RETURN "NOK". 
    END.                      
           
    OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + c-arquivo) CONVERT TARGET "iso8859-1". /* tech1139 - FO 1334.236 - 11/07/2006  */
    FOR EACH tt-mensagem USE-INDEX i-seq-mensagem:
        PUT UNFORMATTED REPLACE(tt-mensagem.mensagem,CHR(13),CHR(10)).
    END.
    OUTPUT CLOSE.
    FILE-INFO:FILE-NAME = SEARCH("interfac/mail/blat.exe").
    ASSIGN v-cod-lin-comando = "@" + "~"" + FILE-INFO:FULL-PATHNAME + "~""
                               + " "
                               + "~"" + SESSION:TEMP-DIRECTORY + c-arquivo + "~"" /* tech1139 - FO 1334.236 - 11/07/2006  */
                               + " -s" 
                               + ' "' 
                               + tt-envio2.assunto 
                               + '"' 
                               + " -f "
                               + tt-envio2.remetente
                               + " -t " 
                               + tt-envio2.destino 
                               + " -server " 
                               + "~"" + param_email.cod_servid_e_mail + "~""
                               + " -port " + "25" 
                               + " -mime"
                               + " -q"
                               + " -noh"
                               + " -noh2"
                               + " -debug -u nfe.ynowa@ynowa.com.br ". /* servidor\e-mail e senha */

    IF tt-envio2.copia <> "" THEN  
        ASSIGN v-cod-lin-comando = v-cod-lin-comando + " -c " + tt-envio2.copia.
    IF tt-envio2.formato <> "TEXTO" AND tt-envio2.formato <> "" THEN DO:
        IF tt-envio2.formato = "ENRICHED" THEN  
            ASSIGN v-cod-lin-comando = v-cod-lin-comando + " -enriched ".
        IF tt-envio2.formato = "HTML" THEN  
            ASSIGN v-cod-lin-comando = v-cod-lin-comando + " -html ".    	
    END.    
    DEFINE VARIABLE i-cont AS INTEGER     NO-UNDO.
/*     MESSAGE "tt-envio2.arq-anexo" tt-envio2.arq-anexo VIEW-AS ALERT-BOX INFO BUTTONS OK. */
    IF tt-envio2.arq-anexo <> "" THEN DO:
        DO i-cont = 1 TO NUM-ENTRIES(tt-envio2.arq-anexo):
            IF OPSYS = "UNIX" THEN DO:
                ASSIGN tt-envio2.arq-anexo = REPLACE(tt-envio2.arq-anexo, "~\~\", "/").
            END.
            ELSE DO:
                ASSIGN tt-envio2.arq-anexo = REPLACE(tt-envio2.arq-anexo, "/", CHR(92)).
            END.
            ASSIGN v-cod-lin-comando = v-cod-lin-comando + " -attach ~"" + ENTRY(i-cont,tt-envio2.arq-anexo) + "~"".
        END.   
    END.

    OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + c-bat) CONVERT TARGET "IBM850". /* tech1139 - FO 1334.236 - 11/07/2006  */
    PUT UNFORMATTED v-cod-lin-comando.
    OUTPUT CLOSE.

    DOS SILENT VALUE("~"" + SESSION:TEMP-DIRECTORY + c-bat + "~""). 

    /*OS-DELETE VALUE(SESSION:TEMP-DIRECTORY + c-arquivo).
    OS-DELETE VALUE(SESSION:TEMP-DIRECTORY + c-bat). 
    MESSAGE SESSION:TEMP-DIRECTORY VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

    RETURN "OK".

END.  /*procedure pi-blat*/

RETURN 'OK'.

