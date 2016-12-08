/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
/*****************************************************************************
**
**       PROGRAMA: cd9043.i
**
**       DATA....: Dezembro de 2003
**
**       AUTOR...: Ana Cristina Spieker - Manufatura - DATASUL S.A.
**
**       OBJETIVO: Defini»’o da temp-table de erros utilizadas nos adapters
**                 do EAI e outros programas 
**
*****************************************************************************/

/** Erros **/
DEFINE TEMP-TABLE RowErrors NO-UNDO
    FIELD ErrorSequence    AS INTEGER
    FIELD ErrorNumber      AS INTEGER
    FIELD ErrorDescription AS CHARACTER
    FIELD ErrorParameters  AS CHARACTER
    FIELD ErrorType        AS CHARACTER
    FIELD ErrorHelp        AS CHARACTER
    FIELD ErrorSubType     AS CHARACTER
    index seq ErrorSequence.
