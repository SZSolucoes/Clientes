/*****************************************************************************
 ** PROGRAMA..: upc-cd1510z01.P
 ** OBJETIVO..: AUXILIA A UPC NA ATUALIZA€ÇO CLIENTES - CD01510
 ** AUTOR.....: SZ SOLU€åES
 ** CLIENTE...: BONYPLUS
 ** VERSAO....: 2.06.00.001 - 08/08/2012 - JChaves.
 ** ALTERA€åES:
 ******************************************************************************/ 
 
DEF VAR wh-pesquisa AS WIDGET-HANDLE NO-UNDO.
DEF VAR l-implanta  AS LOGICAL NO-UNDO.
DEF INPUT PARAM wh-nat-oper AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-wgh-object AS HANDLE NO-UNDO.

    {include/zoomvar.i &prog-zoom    = inzoom/z01in245.w
                       &proghandle   = p-wgh-object
                       &campohandle  = wh-nat-oper
                       &campozoom    = "nat-operacao"}

   
