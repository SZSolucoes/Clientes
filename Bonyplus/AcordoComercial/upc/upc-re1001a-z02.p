/*****************************************************************************
 ** PROGRAMA..: UPC-RE1001A-Z02.P
 ** OBJETIVO..: AUXILIA A UPC DE INCLUSAO DE NOTAS FISCAIS - RE1001A.W
 ** AUTOR.....: SZsolucoes
 ** CLIENTE...: Bonyplus
 ** VERSAO....: 2.12.00.001 - 03/11/2016 
 ******************************************************************************/

DEF VAR wh-pesquisa AS WIDGET-HANDLE NO-UNDO.
DEF VAR l-implanta  AS LOGICAL NO-UNDO.
DEF INPUT PARAM wh-acordo AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-wgh-object AS HANDLE NO-UNDO.

    {include/zoomvar.i &prog-zoom    = esp/escm109-z01.w
                       &proghandle   = p-wgh-object
                       &campohandle  = wh-acordo
                       &campozoom    = "nr-acordo-comerc"}

   

