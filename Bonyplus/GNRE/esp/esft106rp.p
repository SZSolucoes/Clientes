/*****************************************************************************
**       Programa: ESFT106RP.p
**       Data....: 04/01/2017
**       Objetivo: Relatorio De Lotes GNRE 
**       Vers’o..: 2.12.00.001 
*******************************************************************************/

{include/i-prgvrs.i ESFT106RP 2.12.00.001}  /*** 010001 ***/

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG
    field cod-estabel-ini    as char
    field cod-estabel-fim    as char
    field serie-ini        as char
    field serie-fim        as char
    field nr-nota-fis-ini  as char
    field nr-nota-fis-fim  as char
    field uf-ini           as char
    field uf-fim           as char
    field cod-lote-ini     as char
    field cod-lote-fim     as char
    field dt-lote-ini      as date
    field dt-lote-fim      as date
    field cod-receita-ini  as int
    field cod-receita-fim  as int
    field cod-usuario-ini  as char
    field cod-usuario-fim  as char.

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
DEFINE VARIABLE c-desc-sit         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLinha             AS INTEGER.
DEFINE VARIABLE c-des-situacao       AS CHAR NO-UNDO.

/* Propriedade : HorizontalAlignment (Alinhamento Horizontal) ******************/
&global-define xlHAlignCenter       -4108  /* 01 - Centralizado */
&global-define xlHAlignLeft         -4131  /* 04 - Esquerda */

/* Propriedade : LineStyle  (Estilo de Linha) **********************************/
&global-define xlContinuous     1       /* 01 - Continua */ 

{include/i-rpvar.i}
{cdp/cdcfgmat.i}
{utp/ut-glob.i}

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

ASSIGN c-programa = "ESFT106RP"
       c-versao   = "2.12"
       c-revisao  = "001".

{utp/ut-liter.i Relatorio_Lote_GNRE * r}
ASSIGN c-titulo-relat = RETURN-VALUE.

{utp/ut-liter.i Lote GNRE * r}
ASSIGN c-sistema = RETURN-VALUE.

{include/i-rpcab.i} 
{include/i-rpout.i}

VIEW FRAME f-cabec.
VIEW FRAME f-rodape.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Lote_GNRE *}

CREATE "Excel.Application" chexcelapplication.
chexcelapplication:VISIBLE = FALSE.
chexcelapplication:displayalerts = NO.
chexcelapplication:Workbooks:OPEN(SEARCH("layout/esft106.xlsx")).

RUN pi-inicializar IN h-acomp (INPUT  RETURN-VALUE ).

ASSIGN ilinha = 2.

FOR EACH es-nota-fiscal-gnre  NO-LOCK                                                     
    WHERE es-nota-fiscal-gnre.cod-estabel    >= tt-param.cod-estabel-ini                         
      AND es-nota-fiscal-gnre.cod-estabel    <= tt-param.cod-estabel-fim                         
      AND es-nota-fiscal-gnre.serie          >= tt-param.serie-ini                        
      AND es-nota-fiscal-gnre.serie          <= tt-param.serie-fim                        
      AND es-nota-fiscal-gnre.nr-nota-fis    >= tt-param.nr-nota-fis-ini                   
      AND es-nota-fiscal-gnre.nr-nota-fis    <= tt-param.nr-nota-fis-fim                   
      AND es-nota-fiscal-gnre.cod-lote       >= tt-param.cod-lote-ini                      
      AND es-nota-fiscal-gnre.cod-lote       <= tt-param.cod-lote-fim   
      AND es-nota-fiscal-gnre.cod-receita    >= tt-param.cod-receita-ini                       
      AND es-nota-fiscal-gnre.cod-receita    <= tt-param.cod-receita-fim,
    EACH es-lote-gnre
    WHERE es-lote-gnre.cod-lote     = es-nota-fiscal-gnre.cod-lote
      AND es-lote-gnre.dt-lote      >= tt-param.dt-lote-ini
      AND es-lote-gnre.dt-lote      <= tt-param.dt-lote-fim
      AND es-lote-gnre.cod-usuario  >= tt-param.cod-usuario-ini
      AND es-lote-gnre.cod-usuario  <= tt-param.cod-usuario-fim
    BREAK BY es-nota-fiscal-gnre.nr-nota-fis:

    FIND nota-fiscal NO-LOCK
        WHERE nota-fiscal.cod-estabel      = es-nota-fiscal-gnre.cod-estabel
          AND nota-fiscal.serie            = es-nota-fiscal-gnre.serie
          AND nota-fiscal.nr-nota-fis      = es-nota-fiscal-gnre.nr-nota-fis
          AND nota-fiscal.estado           >= uf-ini 
          AND nota-fiscal.estado           <= uf-fim  NO-ERROR.

    IF NOT AVAIL nota-fiscal THEN
        NEXT.

    FIND emitente NO-LOCK
        WHERE emitente.cod-emitente = nota-fiscal.cod-emitente NO-ERROR.

    CASE es-lote-gnre.idi-situacao:
        WHEN 1 THEN
            ASSIGN c-des-situacao = 'Pendente'.
        WHEN 2 THEN
            ASSIGN c-des-situacao = 'Enviado'.
        WHEN 3 THEN
            ASSIGN c-des-situacao = 'Processo'.
        WHEN 4 THEN
            ASSIGN c-des-situacao = 'Integrado EMS5'.
        OTHERWISE
            ASSIGN c-des-situacao = ''.
    END CASE.

    RUN pi-acompanhar IN h-acomp (INPUT es-lote-gnre.cod-lote).

    ASSIGN chexcelapplication:range("A" + STRING(iLinha)):VALUE = nota-fiscal.estado
           chexcelapplication:range("B" + STRING(iLinha)):VALUE = es-nota-fiscal-gnre.cod-estabel
           chexcelapplication:range("C" + STRING(iLinha)):VALUE = es-nota-fiscal-gnre.serie      
           chexcelapplication:range("D" + STRING(iLinha)):VALUE = es-nota-fiscal-gnre.nr-nota-fis  
           chexcelapplication:range("E" + STRING(iLinha)):VALUE = nota-fiscal.vl-tot-nota 
           chexcelapplication:range("F" + STRING(iLinha)):VALUE = IF AVAIL emitente THEN emitente.nome-abrev ELSE ""
           chexcelapplication:range("G" + STRING(iLinha)):VALUE = nota-fiscal.cidade
           chexcelapplication:range("H" + STRING(iLinha)):VALUE = STRING(nota-fiscal.dt-emis-nota,"99/99/9999")
           chexcelapplication:range("I" + STRING(iLinha)):VALUE = es-nota-fiscal-gnre.cod-receita
           chexcelapplication:range("J" + STRING(iLinha)):VALUE = es-nota-fiscal-gnre.vl-receita
           chexcelapplication:range("K" + STRING(iLinha)):VALUE = es-lote-gnre.cod-lote
           chexcelapplication:range("L" + STRING(iLinha)):VALUE = c-des-situacao
           chexcelapplication:range("M" + STRING(iLinha)):VALUE = es-lote-gnre.num-recibo
           chexcelapplication:range("N" + STRING(iLinha)):VALUE = es-lote-gnre.cod-usuario
           chexcelapplication:range("O" + STRING(iLinha)):VALUE = STRING(es-lote-gnre.dt-lote,"99/99/9999")
           chexcelapplication:range("P" + STRING(iLinha)):VALUE = es-lote-gnre.dt-proc
           chexcelapplication:range("Q" + STRING(iLinha)):VALUE = STRING(es-lote-gnre.dt-pagto,"99/99/9999")
           chexcelapplication:range("R" + STRING(iLinha)):VALUE = STRING(es-lote-gnre.dt-vencto,"99/99/9999")
           iLinha = iLinha + 1.
    
    
END.

chexcelapplication:ActiveSheet:Cells:Select. /* Deixar a coluna com o tamanho da celula*/
chexcelapplication:ActiveSheet:Cells:EntireColumn:AutoFit. /* Deixar a coluna com o tamanho da celula*/

RUN pi-finalizar IN h-acomp.  

chexcelapplication:VISIBLE = TRUE.
RELEASE OBJECT chexcelapplication.

RETURN "OK":U. 

