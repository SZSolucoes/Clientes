/***********************************************************************************
**       Programa: ESCM108a.p
**       Data....: 26/10/2016.
**       Autor...: SZ Solu‡oes
**       Objetivo: Exporta‡Æo de Dados do Acordo Comercial EMS2 
**       VersÆo: 001 - Desenvolvimento Inicial
************************************************************************************/
DEFINE INPUT PARAM arq-entrada            AS CHAR      NO-UNDO.
DEFINE INPUT PARAM cod-emitente-ini       AS INT       NO-UNDO. 
DEFINE INPUT PARAM cod-emitente-fim       AS INT      NO-UNDO. 

DEFINE VARIABLE c-diretorio1              AS CHAR   NO-UNDO.
DEFINE VARIABLE h-acomp                   AS HANDLE NO-UNDO.
DEFINE VARIABLE d-valor                   AS DECIMAL     NO-UNDO.

ASSIGN c-diretorio1       = arq-entrada + "\Export_Import_Acordo_comercial.csv".
    
DEFINE STREAM s-arq.
DEFINE STREAM s-acordo.

OUTPUT STREAM s-acordo TO value(c-diretorio1) NO-CONVERT.

PUT STREAM s-acordo UNFORMATTED
    "C¢digo;Cliente;Permite Acordo;Valor Maximo Acordo Mensal;Area Cliente;Percentual Maximo);" SKIP.
   
PUT STREAM s-acordo SKIP.

    FOR EACH emitente NO-LOCK
        WHERE emitente.cod-emitente >= cod-emitente-ini
          AND emitente.cod-emitente <= cod-emitente-fim 
          AND emitente.identific     = 1 :

        FIND ext-emitente NO-LOCK
            WHERE ext-emitente.cod-emitente = emitente.cod-emitente NO-ERROR.

        IF AVAIL ext-emitente THEN DO:

            EXPORT STREAM s-acordo DELIMITER ";"
                emitente.cod-emitente                                           /*Cod.Emitente*/
                emitente.nome-abrev                                             /* Nome Emitente*/
                STRING(ext-emitente.acordo-comerc,"sim/nÆo")                    /*Permite Acordo*/                               
                STRING(ext-emitente.vl-max-acordo,">>>,>>>,>>>,>>>,>>>,>>9.99") /*Valor Maximo*/
                ext-emitente.cod-area                                           /*Area Cliente*/
                string(ext-emitente.percentual-maximo,">>9,99").                /*Percentual Maximo*/

        END.
        ELSE DO:
            EXPORT STREAM s-acordo DELIMITER ";"
                emitente.cod-emitente               /*Cod.Emitente*/
                emitente.nome-abrev                 /* Nome Emitente*/
                "NÆo"                               /*Permite Acordo*/                               
                string("0.000")                      /*Valor Maximo*/
                string("0")                         /*Area Cliente*/
                string("0.00") .                  /*Percentual Maximo*/
        END.

    END.

OUTPUT STREAM s-acordo CLOSE.


