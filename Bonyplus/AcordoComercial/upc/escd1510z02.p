/*****************************************************************************
 ** PROGRAMA..: upc-cd1510z02.P
 ** OBJETIVO..: AUXILIA A UPC NA ATUALIZA��O CLIENTES - CD01510
 ** AUTOR.....: SZ SOLU��ES
 ** CLIENTE...: BONYPLUS
 ** VERSAO....: 2.06.00.001 - 08/08/2012 - JChaves.
 ** ALTERA��ES:
 ******************************************************************************/ 
 
DEF VAR wh-pesquisa AS WIDGET-HANDLE NO-UNDO.
DEF VAR l-implanta  AS LOGICAL NO-UNDO.
DEF INPUT PARAM wh-cod-estab AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-wgh-object AS HANDLE NO-UNDO.

    {include/zoomvar.i &prog-zoom    = adzoom/z01ad107.w
                       &proghandle   = p-wgh-object
                       &campohandle  = wh-cod-estab
                       &campozoom    = "cod-estabel"}

   
