/*****************************************************************************
 ** PROGRAMA..: UPC-RE0402RP.P
 ** OBJETIVO..: UPC NA DESATUALIZAÇÃO DE DOCUMENTOS - RE1001
 ** AUTOR.....: SZ SOLU€åES
 ** CLIENTE...: BONYPLUS
 ** VERSAO....: 2.12.00.001 - 23/11/2016 - Jeferson M.
 ** ALTERA€åES:
 ******************************************************************************/

/*************************** PAR¶METROS PADRÇO *************************************/
{include/i-epc200.i re0402rp}

DEFINE INPUT PARAM  p-ind-event AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAM TABLE FOR tt-epc.

IF p-ind-event = "Fim-Atualizacao" THEN DO:

    FIND FIRST tt-epc
        WHERE tt-epc.cod-event     = p-ind-event
          AND tt-epc.cod-parameter = "docum-est-rowid"  NO-ERROR.
    IF AVAIL tt-epc THEN DO:

        FOR FIRST docum-est NO-LOCK
            WHERE ROWID(docum-est) = TO-ROWID(tt-epc.val-parameter):

            FOR EACH es-tit_ap EXCLUSIVE-LOCK
                WHERE es-tit_ap.cod_estab       = docum-est.cod-estabel
                  AND es-tit_ap.cdn_fornecedor  = docum-est.cod-emitente
                  AND es-tit_ap.cod_espec_docto = "DP"
                  AND es-tit_ap.cod_ser_docto   = docum-est.serie-docto
                  AND es-tit_ap.cod_tit_ap      = docum-est.nro-docto :

                  DELETE es-tit_ap.
                 
            END.

        END.
    END.
END.


