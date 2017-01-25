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

DEFINE INPUT PARAM p-ind-event AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAM TABLE FOR tt-epc.

/*NÆo remover este codigo*/
RUN upc/upc-bodi317ef-1.p (INPUT p-ind-event,
                           INPUT-OUTPUT TABLE tt-epc).



/*Executar aqui a upc do programa BODI317EF*/
RUN upc/upc-bodi317ef-sz.p (INPUT p-ind-event,
                            INPUT-OUTPUT TABLE tt-epc).


RETURN "OK".

/*Fim UPC-BODI317EF*/
