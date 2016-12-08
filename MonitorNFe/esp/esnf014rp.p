/*****************************************************************************
**       Programa: esnf014rp.p
**       Data....: 11/10/2012
**       Autor...: SZ Solu‡äes
*******************************************************************************/
{include/i-prgvrs.i ESNF014RP 2.06.00.001}  /*** 010001 ***/

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
    FIELD emit-ini         AS INT
    FIELD emit-fim         AS INT
    FIELD data-ini         AS DATE FORMAT 99/99/9999
    FIELD data-fim         AS DATE FORMAT 99/99/9999
    FIELD rd-sefaz         AS INT.

DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita AS RAW.

DEFINE VARIABLE h-acomp             AS HANDLE  NO-UNDO.

DEF INPUT PARAM raw-param AS RAW NO-UNDO.
DEF INPUT PARAM TABLE FOR tt-raw-digita.

DEFINE VARIABLE chexcelapplication AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chworkbook         AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chworksheet        AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE arquivo-at         AS CHAR NO-UNDO.
DEFINE VARIABLE arquivo-cp         AS CHAR NO-UNDO.
DEFINE VARIABLE arquivo-nv         AS CHAR NO-UNDO.
DEFINE VARIABLE i-linha            AS INT  NO-UNDO.
DEFINE VARIABLE ilinha            AS INTEGER INITIAL 1.
DEFINE VARIABLE ilinha2           AS INTEGER INITIAL 1.

DEFINE VARIABLE c-desc-forn AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-situacao  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-desc      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-docto      AS CHARACTER   NO-UNDO.


/* Propriedade : HorizontalAlignment (Alinhamento Horizontal) ******************/
&global-define xlHAlignCenter       -4108  /* 01 - Centralizado */
&global-define xlHAlignLeft         -4131  /* 04 - Esquerda */

/* Propriedade : LineStyle  (Estilo de Linha) **********************************/
&global-define xlContinuous     1       /* 01 - Continua */

{include/i-rpvar.i}
/* {cdp/cdcfgmat.i} */
/* {utp/ut-glob.i} */

create tt-param.
raw-transfer raw-param to tt-param.

ASSIGN c-programa = "ESNF014RP"
       c-versao   = "2.06"
       c-revisao  = "001".

{utp/ut-liter.i Relatorio_Notas/Fornecedor * r}
ASSIGN c-titulo-relat = RETURN-VALUE.

{include/i-rpcab.i} 
{include/i-rpout.i}

VIEW FRAME f-cabec.
VIEW FRAME f-rodape.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Relatorio_Notas/Fornecedor *}

FIND tt-param NO-LOCK NO-ERROR.

IF AVAIL tt-param THEN DO:

    ASSIGN ilinha = 5.

    CREATE "Excel.Application" chexcelapplication.
    chexcelapplication:VISIBLE = NO.
    chexcelapplication:displayalerts = NO.
    chexcelapplication:Workbooks:OPEN(SEARCH("modelo\esnf014-forn.xls")).
          
    FOR EACH nfe003 NO-LOCK
        WHERE nfe003.cod-emitente >= tt-param.emit-ini 
          AND nfe003.cod-emitente <= tt-param.emit-fim
          AND nfe003.dt-emissao   >= tt-param.data-ini
          AND nfe003.dt-emissao   <= tt-param.data-fim
       BREAK BY nfe003.dt-emissao
             BY nfe003.nro-docto :

          IF tt-param.rd-sefaz = 1 AND nfe003.sit-sefaz = 1 THEN NEXT.

          IF tt-param.rd-sefaz = 2 AND nfe003.sit-sefaz = 2 THEN NEXT.


          FIND emitente NO-LOCK
              WHERE emitente.cod-emitente = nfe003.cod-emitente  NO-ERROR.
          
          IF AVAIL emitente THEN
              ASSIGN c-desc-forn = emitente.nome-abrev.

          ASSIGN c-situacao = {esinc/i01es003.i 04 nfe003.idi-situacao}.

          IF nfe003.sit-sefaz = 1 THEN
              ASSIGN c-desc = "Uso Autorizado".
          ELSE
              ASSIGN c-desc = "Uso NÆo Autorizado".
         
          RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
      
          ASSIGN chexcelapplication:range("A" + STRING(ilinha)):VALUE = c-desc
                 chexcelapplication:range("B" + STRING(ilinha)):numberformat = "@"
                 chexcelapplication:range("B" + STRING(ilinha)):VALUE = nfe003.nro-docto
                 chexcelapplication:range("C" + STRING(ilinha)):VALUE = nfe003.serie-docto
                 chexcelapplication:range("D" + STRING(ilinha)):VALUE = nfe003.dt-emissao
                 chexcelapplication:range("E" + STRING(ilinha)):VALUE = nfe003.cod-emitente
                 chexcelapplication:range("F" + STRING(ilinha)):VALUE = c-desc-forn
                 chexcelapplication:range("G" + STRING(ilinha)):VALUE = nfe003.cod-estabel
                 chexcelapplication:range("H" + STRING(ilinha)):VALUE = c-situacao
                 ilinha = ilinha + 1.                                                                    
    END.
END.

chexcelapplication:ActiveSheet:Cells:Select. /* Deixar a coluna com o tamanho da celula*/
chexcelapplication:ActiveSheet:Cells:EntireColumn:AutoFit. /* Deixar a coluna com o tamanho da celula*/

RUN pi-finalizar IN h-acomp.
chexcelapplication:ActiveWorkbook:SaveAs(SESSION:TEMP-DIRECTORY + "esnf014" + STRING(TIME) + ".xls",,,,,,).
chexcelapplication:VISIBLE = TRUE.
RELEASE OBJECT chexcelapplication.


