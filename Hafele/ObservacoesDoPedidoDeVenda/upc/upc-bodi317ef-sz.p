
/*****************************************************************************
 ** PROGRAMA..: UPC-BODI317EF.P
 ** OBJETIVO..: UPC BO de Efetiva‡Æo do C lculo da Nota - BODI317EF
 ** AUTOR.....: SZ SOLU°†ES
 ** CLIENTE...: HAFELE
 ** VERSAO....: 2.12.00.001 - 20/01/2017
 ** ALTERA°†ES:
 ******************************************************************************/
{include/i-prgvrs.i UPC-BODI317EF 2.12.00.001}
{include/i-epc200.i} /** Defini‡Æo tt-EPC **/

/*** Defini‡Æo de Parƒmetros ***/
DEFINE INPUT PARAM  p-ind-event AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAM TABLE FOR tt-epc.

DEFINE BUFFER bf-nota-fiscal FOR nota-fiscal.

IF p-ind-event = "endEfetivaNota" THEN DO:

    FIND FIRST tt-epc NO-LOCK WHERE
               tt-epc.cod-event = "EndEfetivaNota"         AND
               tt-epc.cod-parameter = "ROWID(nota-fiscal)" NO-ERROR.

    IF AVAIL tt-epc THEN DO:
    
        FIND FIRST bf-nota-fiscal NO-LOCK WHERE
             ROWID(bf-nota-fiscal) = TO-ROWID(tt-epc.val-parameter) NO-ERROR.

        IF  AVAIL bf-nota-fiscal THEN DO:

            FIND ext-emitente NO-LOCK
                WHERE ext-emitente.cod-emitente = bf-nota-fiscal.cod-emitente NO-ERROR.

            IF AVAIL ext-emitente
                AND ext-emitente.observacao-complentar <> "" THEN 

                ASSIGN bf-nota-fiscal.observ-nota = TRIM(bf-nota-fiscal.observ-nota) + " / " + ext-emitente.observacao-pedido  + " / " + ext-emitente.observacao-complentar .
        END.
    END.
END.

RETURN "OK":U.

/*Fim UPC-BODI317EF-A*/
