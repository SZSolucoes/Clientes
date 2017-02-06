/*****************************************************************************
 ** PROGRAMA..: ESOF001crp.P
 ** OBJETIVO..: Relat¢rio com Dados ESOF001.w
 ** AUTOR.....: SZ Solu‡äes
 ** CLIENTE...: Sonepar
 ** VERSAO....: 2.12.00.000 - Janeiro/2017
 ** ALTERA€åES:
 ******************************************************************************/
 {cdp/cd0666.i}
/*************************** Temp-Table *************************************/
DEFINE TEMP-TABLE tt-doc-fiscal NO-UNDO LIKE doc-fiscal
    FIELD numero-dt-nota AS CHAR FORMAT "x(400)"
    FIELD valor-tot-doc-fiscal LIKE doc-fiscal.vl-mercad
    FIELD valor-tot-integrado LIKE doc-fiscal.vl-mercad
    FIELD valor-tot-diferenca LIKE doc-fiscal.vl-mercad.

DEF INPUT PARAMETER TABLE FOR tt-doc-fiscal.

/*************************** VARIµVEIS *************************************/
DEFINE VARIABLE chexcelapplication    AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chworkbook            AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chworksheet           AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE arquivo-at            AS CHAR NO-UNDO.
DEFINE VARIABLE arquivo-cp            AS CHAR NO-UNDO.
DEFINE VARIABLE arquivo-nv            AS CHAR NO-UNDO.
DEFINE VARIABLE i-linha               AS INT  NO-UNDO.
DEFINE VARIABLE c-desc-sit            AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLinha                AS INTEGER.
DEFINE VARIABLE icont                 AS INTEGER     NO-UNDO.

DEFINE VARIABLE h-acomp               AS HANDLE  NO-UNDO.

/* Propriedade : HorizontalAlignment (Alinhamento Horizontal) ******************/
&global-define xlHAlignCenter       -4108  /* 01 - Centralizado */
&global-define xlHAlignLeft         -4131  /* 04 - Esquerda */

/* Propriedade : LineStyle  (Estilo de Linha) **********************************/
&global-define xlContinuous     1       /* 01 - Continua */ 

{include/i-rpvar.i}
{cdp/cdcfgmat.i}
{utp/ut-glob.i}

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

CREATE "Excel.Application" chexcelapplication.
chexcelapplication:VISIBLE = FALSE.
chexcelapplication:displayalerts = NO.
chexcelapplication:Workbooks:OPEN(SEARCH("modelos/esfo001crp.xls")).

RUN pi-inicializar IN h-acomp (INPUT  RETURN-VALUE ).  

ASSIGN ilinha = 2.

FOR EACH tt-doc-fiscal NO-LOCK
    BREAK BY tt-doc-fiscal.cod-estabel
          BY tt-doc-fiscal.dt-emis-doc
          BY tt-doc-fiscal.nr-doc-fis:

    RUN pi-inicializar IN h-acomp (INPUT "Nota Fiscal:" + "" + tt-doc-fiscal.nr-doc-fis).

    ASSIGN chexcelapplication:range("A" + STRING(iLinha)):VALUE = tt-doc-fiscal.cod-estabel
           chexcelapplication:range("B" + STRING(iLinha)):VALUE = STRING( tt-doc-fiscal.dt-docto,"99/99/9999")
           chexcelapplication:range("C" + STRING(iLinha)):VALUE = tt-doc-fiscal.nome-ab-emi
           chexcelapplication:range("D" + STRING(iLinha)):VALUE = tt-doc-fiscal.cod-emitente
           chexcelapplication:range("E" + STRING(iLinha)):VALUE = STRING(tt-doc-fiscal.nr-doc-fis)
           chexcelapplication:range("F" + STRING(iLinha)):VALUE = tt-doc-fiscal.serie
           chexcelapplication:range("G" + STRING(iLinha)):VALUE = tt-doc-fiscal.nat-operacao
           chexcelapplication:range("H" + STRING(iLinha)):VALUE = tt-doc-fiscal.esp-docto
           chexcelapplication:range("I" + STRING(iLinha)):VALUE = {diinc/i07di037.i 04 tt-doc-fiscal.ind-ori-doc}.

    ASSIGN chexcelapplication:range("L" + STRING(iLinha)):numberformat = "#.##0,000" 
           chexcelapplication:range("L" + STRING(iLinha)):VALUE = tt-doc-fiscal.valor-tot-doc-fiscal
           chexcelapplication:range("M" + STRING(iLinha)):numberformat = "#.##0,000"
           chexcelapplication:range("M" + STRING(iLinha)):VALUE = tt-doc-fiscal.valor-tot-integrado 
           chexcelapplication:range("N" + STRING(iLinha)):numberformat = "#.##0,000"
           chexcelapplication:range("N" + STRING(iLinha)):VALUE = tt-doc-fiscal.valor-tot-diferenca 
           /*iLinha = iLinha + 1*/.

    ASSIGN icont = 0.
    FOR EACH nota-fisc-adc NO-LOCK
        WHERE nota-fisc-adc.cod-estab        = tt-doc-fiscal.cod-estabel
          AND nota-fisc-adc.cod-serie        = tt-doc-fiscal.serie
          AND nota-fisc-adc.cod-nota-fisc    = tt-doc-fiscal.nr-doc-fis
          AND nota-fisc-adc.cdn-emitente     = tt-doc-fiscal.cod-emitente
          AND nota-fisc-adc.cod-natur-operac = tt-doc-fiscal.nat-operacao
        BREAK BY tt-doc-fiscal.nr-doc-fis:

        ASSIGN chexcelapplication:range("J" + STRING(iLinha)):VALUE = nota-fisc-adc.cod-docto-referado
               chexcelapplication:range("K" + STRING(iLinha)):VALUE = STRING(nota-fisc-adc.dat-docto-referado,"99/99/9999")
               iLinha = iLinha + 1.
        
    END.

    FIND FIRST nota-fisc-adc NO-LOCK
        WHERE nota-fisc-adc.cod-estab        = tt-doc-fiscal.cod-estabel
          AND nota-fisc-adc.cod-serie        = tt-doc-fiscal.serie
          AND nota-fisc-adc.cod-nota-fisc    = tt-doc-fiscal.nr-doc-fis
          AND nota-fisc-adc.cdn-emitente     = tt-doc-fiscal.cod-emitente
          AND nota-fisc-adc.cod-natur-operac = tt-doc-fiscal.nat-operacao NO-ERROR.
    IF NOT AVAIL nota-fisc-adc THEN
        ASSIGN iLinha = iLinha + 1.
    
END.

chexcelapplication:ActiveSheet:Cells:Select. /* Deixar a coluna com o tamanho da celula*/
chexcelapplication:ActiveSheet:Cells:EntireColumn:AutoFit. /* Deixar a coluna com o tamanho da celula*/

RUN pi-finalizar IN h-acomp.        

chexcelapplication:VISIBLE = TRUE.
RELEASE OBJECT chexcelapplication.     

RETURN "OK":U.
