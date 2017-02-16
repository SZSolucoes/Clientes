/*****************************************************************************
 ** PROGRAMA..: UPC-PD4000a.P
 ** OBJETIVO..: Programa Chamador de UPC
 ** AUTOR.....: SZ SOLU∞ÜES
 ** CLIENTE...: HAFELE
 ** VERSAO....: 2.12.00.001 - 20/01/2017
 ** ALTERA∞ÜES:
 ******************************************************************************/
{include/i-prgvrs.i UPC-PD4000A 2.12.00.001}

/* Variaveis de ParÅmetros */ 
DEFINE INPUT PARAMETER p-ind-event   AS CHAR          NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object  AS CHAR          NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object  AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame   AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table   AS CHAR          NO-UNDO.
DEFINE INPUT PARAMETER p-row-table   AS ROWID         NO-UNDO.

/*RUN upc/upc-pd4000.p (INPUT p-ind-event ,
                      INPUT p-ind-object,
                      INPUT p-wgh-object,
                      INPUT p-wgh-frame ,
                      INPUT p-cod-table ,
                      INPUT p-row-table ).

IF RETURN-VALUE = "NOK":U THEN
    RETURN "NOK":U.*/

RUN upc/upc-pd4000-sz.p (INPUT p-ind-event ,
                         INPUT p-ind-object,
                         INPUT p-wgh-object,
                         INPUT p-wgh-frame ,
                         INPUT p-cod-table ,
                         INPUT p-row-table ).

RETURN "OK".

/* Fim do Programa */
