/********************************************************************************
**
** Programa: escd099
** Objetivo: Importaá∆o Dados Clientes
**    Autor: Edson de Souza
**     Data: 29/11/2016 - 1.00.00.001
**
*******************************************************************************/
{include/i-prgvrs.i escd099rp 2.06.00.000}

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    FIELD arq-entrada      AS CHAR FORMAT "x(35)"
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG.

DEFINE TEMP-TABLE tt-digita NO-UNDO
    FIELD ordem            AS INTEGER   FORMAT ">>>>9"
    FIELD exemplo          AS CHARACTER FORMAT "x(30)"
    INDEX id ordem.

DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita AS raw.

DEFINE INPUT PARAMETER raw-param AS raw NO-UNDO.
DEFINE INPUT PARAMETER table FOR tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param NO-ERROR.

FOR EACH tt-raw-digita NO-LOCK:  
    CREATE tt-digita.                   
    RAW-TRANSFER tt-raw-digita.raw-digita TO  tt-digita. 
END.

def temp-table tt-erro no-undo
    field i-seq    as int
    field mensagem as char format "x(255)".

DEF TEMP-TABLE tt-importacao NO-UNDO
    FIELD cod-cliente     AS INT 
    FIELD limite-credito  AS INT 
    FIELD cond-pagto      AS char  
    FIELD tipo-credito    AS CHAR
    FIELD tipo-avaliacao  AS CHAR 
    FIELD port-corrigido  AS INT
    FIELD cobranca        AS CHAR
    FIELD considera-pefin AS CHAR.

/* Definicao de Variaveis locais */
DEF VAR h-acomp        AS HANDLE  NO-UNDO.

/* Definiá∆o de buffers locais */
DEF BUFFER bf-tt-erro FOR tt-erro.

/*----- DEFINICAO DE VARIAVEL GLOBAL -----*/
{include/i-rpvar.i}

/* vari†veis excel */
DEFINE  VARIABLE chexcelapplication AS COM-HANDLE           NO-UNDO.
DEFINE  VARIABLE chexcelas          AS COM-HANDLE           NO-UNDO.
DEFINE  VARIABLE i-linha            AS INTEGER INITIAL 3    NO-UNDO.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp ("Importando Registros...").

/* In°cio da geraá∆o de dados */
FIND FIRST tt-param.

CREATE "excel.APPLICATION" chexcelapplication.
chexcelas = chexcelapplication:workbooks:OPEN(tt-param.arq-entrada).

chexcelapplication:VISIBLE = NO.

assign i-linha = 3.

REPEAT WHILE chExcelApplication:Range("A" + STRING(i-linha)):VALUE <> ?:

    CREATE tt-importacao.
    ASSIGN tt-importacao.cod-cliente     = chExcelApplication:Range("A" + STRING(i-linha)):VALUE
           tt-importacao.limite-credito  = chExcelApplication:Range("C" + STRING(i-linha)):VALUE
           tt-importacao.cond-pagto      = trim(chExcelApplication:Range("D" + STRING(i-linha)):VALUE)
           tt-importacao.tipo-credito    = chExcelApplication:Range("E" + STRING(i-linha)):VALUE
           tt-importacao.tipo-avaliacao  = chExcelApplication:Range("F" + STRING(i-linha)):VALUE
           tt-importacao.port-corrigido  = chExcelApplication:Range("G" + STRING(i-linha)):VALUE
           tt-importacao.cobranca        = chExcelApplication:Range("H" + STRING(i-linha)):VALUE
           tt-importacao.considera-pefin = chExcelApplication:Range("I" + STRING(i-linha)):VALUE
           i-linha                       = i-linha + 1.
           
           
    if tt-importacao.cond-pagto = '0' then
        assign tt-importacao.cond-pagto = '1'.
        
    if tt-importacao.cond-pagto = '28/42' then
        assign tt-importacao.cond-pagto = '17'.    

    RUN pi-acompanhar IN h-acomp ("Preparando importaá∆o: " + string(tt-importacao.cod-cliente)).

END.
          
FOR EACH tt-importacao NO-LOCK:

    /* Valida se o registro existe */
    FIND emitente
        WHERE emitente.cod-emitente = tt-importacao.cod-cliente EXCLUSIVE-LOCK NO-ERROR.

    IF NOT AVAIL emitente THEN DO:

        CREATE tt-erro.

        FIND LAST bf-tt-erro NO-LOCK NO-ERROR.
        IF AVAIL bf-tt-erro THEN
            ASSIGN tt-erro.i-seq = bf-tt-erro.i-seq + 1.
        ELSE
            ASSIGN tt-erro.i-seq = 1.

        ASSIGN tt-erro.mensagem = "Importaá∆o n∆o realizada, cliente c¢digo " + string(tt-importacao.cod-cliente) + ". Motivo: n∆o localizado registro na tabela emitente.".

    END.
    ELSE DO:

        RUN pi-acompanhar IN h-acomp ("Emitente: " + string(emitente.cod-emitente)).

        IF tt-importacao.limite-credito <> ? THEN
            ASSIGN emitente.lim-credito  = tt-importacao.limite-credito.         

        ASSIGN emitente.portador     = tt-importacao.port-corrigido
               emitente.cod-cond-pag = int(tt-importacao.cond-pagto) NO-ERROR.

        CASE tt-importacao.cobranca.
            WHEN "Desconto" THEN
                ASSIGN emitente.modalidade = 1.
            WHEN "Cauá∆o" THEN
                ASSIGN emitente.modalidade = 2.
            WHEN "Judicial" THEN
                ASSIGN emitente.modalidade = 3.
            WHEN "Repres" THEN
                ASSIGN emitente.modalidade = 4.
            WHEN "Carteira" THEN
                ASSIGN emitente.modalidade = 5.
            WHEN "Vendor" THEN
                ASSIGN emitente.modalidade = 6.
            WHEN "Cheque" THEN
                ASSIGN emitente.modalidade = 7.
            WHEN "Nota Promiss¢ria" THEN
                ASSIGN emitente.modalidade = 8.
        END CASE.

        CASE tt-importacao.tipo-avaliacao.
            WHEN "N∆o Avalia" THEN
                ASSIGN emitente.ind-aval = 1.
            WHEN "Somente Atrasos Duplicatas" THEN
                ASSIGN emitente.ind-aval = 2.
            WHEN "Limites e Atrasos Duplicatas" THEN
                ASSIGN emitente.ind-aval = 3.
        END CASE.

        CASE tt-importacao.tipo-credito.
            WHEN "Normal" THEN
                ASSIGN emitente.ind-cre-cli = 1.
            WHEN "Autom†tico" THEN
                ASSIGN emitente.ind-cre-cli = 2.
            WHEN "S¢ Imp Ped" THEN
                ASSIGN emitente.ind-cre-cli = 3.
            WHEN "Suspenso" THEN
                ASSIGN emitente.ind-cre-cli = 4.
            WHEN "Pg a Vista" THEN
                ASSIGN emitente.ind-cre-cli = 5.
        END CASE.
    
        /* Campos referentes ao ems5 */
        FIND clien_financ
            WHERE clien_financ.cdn_cliente = tt-importacao.cod-cliente EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL clien_financ THEN DO:

            FIND FIRST es_clien_financ
                WHERE es_clien_financ.cod_empresa = clien_financ.cod_empresa
                  AND es_clien_financ.cdn_cliente = clien_financ.cdn_cliente EXCLUSIVE-LOCK NO-ERROR.

            IF AVAIL es_clien_financ THEN
                ASSIGN es_clien_financ.LOG_permis_negativ = IF tt-importacao.considera-pefin = "SIM" THEN YES ELSE NO
                       es_clien_financ.LOG_clien_negatvdo = IF tt-importacao.considera-pefin = "SIM" THEN YES ELSE NO.

        END.
        ELSE DO:

            CREATE tt-erro.

            FIND LAST bf-tt-erro NO-LOCK NO-ERROR.
            IF AVAIL bf-tt-erro THEN
                ASSIGN tt-erro.i-seq = bf-tt-erro.i-seq + 1.
            ELSE
                ASSIGN tt-erro.i-seq = 1.
           
            ASSIGN tt-erro.mensagem = "Importaá∆o n∆o realizada, cliente c¢digo " + string(tt-importacao.cod-cliente) + ". Motivo: n∆o localizado registro na tabela clien_financ.".

        END.
    END.
END.

RELEASE OBJECT chexcelas.
RELEASE OBJECT chexcelapplication.

/* In°cio da impress∆o dos erros durante a execuá∆o da importaá∆o */
{include/i-rpcab.i} 

ASSIGN c-programa     = "ESCD099"
       c-empresa      = ""
       c-versao       = "1.00"
       c-revisao      = "001".

{utp/ut-liter.i "Importaá∆o Dados Clientes" *}
ASSIGN c-titulo-relat = TRIM(RETURN-VALUE).

{include/i-rpout.i}
VIEW FRAME f-cabec.
VIEW FRAME f-dados.

PUT UNFORMATTED "Sequencia"  AT 1.
PUT UNFORMATTED "Informaá∆o" AT 25.
PUT UNFORMATTED SKIP.
PUT UNFORMATTED FILL("-",150).
PUT UNFORMATTED SKIP.

FOR EACH tt-erro:
    
    RUN pi-acompanhar IN h-acomp("Imprimindo erros: " + string(tt-erro.i-seq)).

    PUT UNFORMATTED tt-erro.i-seq AT 1.
    PUT UNFORMATTED tt-erro.mensagem AT 25.
    PUT UNFORMATTED SKIP.

END.

VIEW FRAME f-rodape.

RUN pi-finalizar in h-acomp.

RETURN "OK".


