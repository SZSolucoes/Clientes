/*****************************************************************************
 ** PROGRAMA..: trd-doc-fisico.P
 ** OBJETIVO..: UPC NA TRIGGER DE DELETE NA TABELA doc-fisico
 ** AUTOR.....: SZ SOLU€åES
 ** CLIENTE...: 
 ** VERSAO....: 2.06.00.001 - 31/07/2014 - Jeferson M.
 ** ALTERA€åES:
 ******************************************************************************/
DEF PARAM BUFFER p-table     FOR doc-fisico.

IF AVAIL p-table THEN DO:

    FOR EACH nfe003 EXCLUSIVE-LOCK
        WHERE nfe003.nro-docto       = p-table.nro-docto
          AND nfe003.serie-docto     = p-table.serie-docto
          AND nfe003.cod-emitente    = p-table.cod-emitente
          AND nfe003.dt-emissao      = p-table.dt-emissao:

        ASSIGN nfe003.idi-situacao = 4. /* Eliminado no Recebimento */

    END.

END.

RETURN "OK".
