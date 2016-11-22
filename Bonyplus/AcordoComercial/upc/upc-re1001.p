/*****************************************************************************
 ** PROGRAMA..: UPC-RE1001.P
 ** OBJETIVO..: UPC NA INCLUSAO DE NOTAS FISCAIS - RE1001.W
 ** AUTOR.....: SZsolucoes
 ** CLIENTE...: Bonyplus
 ** VERSAO....: 2.12.00.001 - 07/11/2016 
 ******************************************************************************/
 
/*************************** PAR¶METROS PADRÇO *************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

/***************** Defini‡Æo de Vari veis Globais ***************/ 
DEFINE NEW GLOBAL SHARED VARIABLE wh-re1001-txt-acordo           AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-re1001-fill-acordo          AS WIDGET-HANDLE  NO-UNDO.

/***************** Defini‡Æo de Vari veis locais ****************/
DEFINE VARIABLE c-objeto       AS CHARACTER NO-UNDO.
DEFINE VARIABLE h-objeto       AS HANDLE    NO-UNDO.

IF p-wgh-object <> ? THEN
    ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:FILE-NAME,'/'),
                                      p-wgh-object:FILE-NAME,'/').

/* MESSAGE "p-ind-event..:" p-ind-event            SKIP  */
/*         "p-ind-object.:" p-ind-object           SKIP  */
/*         "p-wgh-object.:" p-wgh-object:FILE-NAME SKIP  */
/*         "p-wgh-frame..:" p-wgh-frame:NAME       SKIP  */
/*         "p-cod-table..:" STRING(p-cod-table)    SKIP  */
/*         "p-row-table..:" STRING(p-row-table)    SKIP  */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                */

IF  p-ind-event = "before-initialize"
AND p-ind-object= "container" THEN DO:

    ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD
           h-objeto = h-objeto:FIRST-CHILD.

    CREATE TEXT wh-re1001-txt-acordo
    ASSIGN FRAME        = p-wgh-frame
           FORMAT       = "x(20)"
           WIDTH        = 12.29
           SCREEN-VALUE = "Acordo Comercial:"
           ROW          = 4.15
           COL          = 47.5
           VISIBLE      = YES.

    CREATE FILL-IN wh-re1001-fill-acordo  
    ASSIGN FRAME             =  p-wgh-frame
           SIDE-LABEL-HANDLE =  wh-re1001-txt-acordo:HANDLE
           LABEL             = "Acordo Comercial:"
           DATA-TYPE         = "CHARACTER"
           FORMAT            = "x(12)"
           WIDTH             = 10.1
           ROW               = 4.0
           COL               = 60 
           HEIGHT            = 0.88 
           VISIBLE           = YES
           SENSITIVE         = NO.
END.
     
IF  p-ind-event  = "AFTER-DISPLAY"
AND p-ind-object = "CONTAINER"  THEN DO:

    IF VALID-HANDLE(wh-re1001-fill-acordo) THEN DO:
  
        FIND FIRST docum-est NO-LOCK
             WHERE ROWID(docum-est) = p-row-table NO-ERROR.
        IF AVAIL docum-est THEN DO:
        
            FIND ems2custom.ext-docum-est NO-LOCK
                 WHERE ext-docum-est.serie-docto  = docum-est.serie-docto
                   AND ext-docum-est.nro-docto    = docum-est.nro-docto
                   AND ext-docum-est.cod-emitente = docum-est.cod-emitente
                   AND ext-docum-est.nat-operacao = docum-est.nat-operacao NO-ERROR.
            IF AVAIL ext-docum-est THEN 
        
                ASSIGN wh-re1001-fill-acordo:SCREEN-VALUE = ext-docum-est.nr-acordo-comerc.
        
            ELSE
        
                ASSIGN wh-re1001-fill-acordo:SCREEN-VALUE = "".
                
        END.

    END.

END.

RETURN "OK".
