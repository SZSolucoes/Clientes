
/*****************************************************************************
 ** PROGRAMA..: UPC-RE1001A1.P
 ** OBJETIVO..: UPC NA ALTERAÄ«O DE NOTAS FISCAIS - RE1001A.W
 ** AUTOR.....: SZsolucoes
 ** CLIENTE...: Bonyplus
 ** VERSAO....: 2.12.00.001 - 07/11/2016 
 ******************************************************************************/
/***************** Definiá∆o de Parametros **********************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

/***************** Definiá∆o de Vari†veis Globais ***************/
DEF NEW GLOBAL SHARED VAR h-folder                 AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h_campo1                 AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h_campo2                 AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h_campo3                 AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h_campo4                 AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h_campo5                 AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h_campo6                 AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re1001-txt-acordo     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re1001a1-fill-acordo    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re1001a1-cod-estabel  AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR esre1001a1-tb-arq-transp AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR esre1001a1-tb-remessa    AS WIDGET-HANDLE NO-UNDO.

/***************** Definiá∆o de Vari†veis locais ****************/

DEFINE VAR c-objeto       AS CHAR          NO-UNDO.
DEFINE VAR c-folder       AS CHAR          NO-UNDO.
DEFINE VAR h_frame        AS WIDGET-HANDLE NO-UNDO.
DEFINE VAR h-objeto       AS HANDLE        NO-UNDO.
DEFINE VAR wh-pesquisa    AS WIDGET-HANDLE NO-UNDO.

/***************** Main Block ***********************************/

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


IF p-ind-event   = "BEFORE-INITIALIZE"
AND p-ind-object = "CONTAINER" 
AND c-objeto     = "RE1001A1.w" THEN DO:

    ASSIGN h_frame = p-wgh-frame:FIRST-CHILD
           h_frame = h_frame:FIRST-CHILD.
    
    DO WHILE h_frame <> ?:
       IF h_frame:TYPE <> "field-group" THEN DO:
          IF h_frame:NAME = "fPage1" THEN
             ASSIGN h_campo1 = h_frame.
          IF h_frame:NAME = "fPage2" THEN
             ASSIGN h_campo2 = h_frame.
          IF h_frame:NAME = "fPage3" THEN
             ASSIGN h_campo3 = h_frame.
          IF h_frame:NAME = "fPage4" THEN
             ASSIGN h_campo4 = h_frame.
          IF h_frame:NAME = "fPage5" THEN
             ASSIGN h_campo5 = h_frame.
          IF h_frame:NAME = "fPage6" THEN DO:
             ASSIGN h_campo6 = h_frame.
             LEAVE.
          END.
          ASSIGN h_frame = h_frame:NEXT-SIBLING.
       END.
       ELSE DO:
          ASSIGN h_frame = h_frame:FIRST-CHILD.
       END.
    END.
    
    ASSIGN h_campo1:HIDDEN = YES
           h_campo2:HIDDEN = YES
           h_campo3:HIDDEN = YES
           h_campo4:HIDDEN = YES
           h_campo5:HIDDEN = YES
           h_campo6:HIDDEN = YES.

    CREATE TEXT wh-re1001-txt-acordo
    ASSIGN FRAME        = h_campo1
           FORMAT       = "x(20)"
           WIDTH        = 12.29
           SCREEN-VALUE = "Acordo Comercial:"
           ROW          = 4.24
           COL          = 47.5
           VISIBLE      = YES.
    
    CREATE FILL-IN wh-re1001a1-fill-acordo  
    ASSIGN FRAME             =  h_campo1
           SIDE-LABEL-HANDLE =  wh-re1001-txt-acordo:HANDLE
           LABEL             = "Acordo Comercial:"
           DATA-TYPE         = "CHARACTER"
           FORMAT            = "x(12)"
           WIDTH             = 10.1
           ROW               = 4.2
           COL               = 60 
           HEIGHT            = 0.88 
           VISIBLE           = YES
           SENSITIVE         = YES.
    
    wh-re1001a1-fill-acordo:LOAD-MOUSE-POINTER('image/lupa.cur').
    
    /* Zoom acordo Comercial*/
    ON 'F5' OF wh-re1001a1-fill-acordo OR 'mouse-select-dblclick' OF wh-re1001a1-fill-acordo PERSISTENT
         RUN upc/upc-re1001a1.p ("zoom-acordo",?,?,?,?,?).

END.

IF p-ind-event  = "AFTER-INITIALIZE" AND 
   p-ind-object = "CONTAINER"        THEN DO:

   FIND FIRST docum-est WHERE
        ROWID(docum-est) = p-row-table NO-ERROR.
   IF AVAIL docum-est THEN DO:            

       FIND ems2custom.ext-docum-est NO-LOCK
           WHERE ext-docum-est.serie-docto  = docum-est.serie-docto
             AND ext-docum-est.nro-docto    = docum-est.nro-docto
             AND ext-docum-est.cod-emitente = docum-est.cod-emitente
             AND ext-docum-est.nat-operacao = docum-est.nat-operacao NO-ERROR.

           IF AVAIL ext-docum-est THEN 
        
               ASSIGN wh-re1001a1-fill-acordo:SCREEN-VALUE = ext-docum-est.nr-acordo-comerc.
      
   END. 

END.

IF p-ind-event   = "BEFORE-ASSIGN" 
AND p-ind-object = "CONTAINER" 
/*AND c-objeto     = "RE1001A1.w"*/ THEN DO:

    IF VALID-HANDLE(wh-re1001a1-fill-acordo) THEN DO:
    
        IF wh-re1001a1-fill-acordo:SCREEN-VALUE <> "" THEN DO:
            
            FIND es-acordo-comerc NO-LOCK
                WHERE es-acordo-comerc.nr-acordo-comerc = wh-re1001a1-fill-acordo:SCREEN-VALUE NO-ERROR.
            IF NOT AVAIL es-acordo-comerc THEN DO:
                RUN utp/ut-msgs.p (INPUT "show":U,
                                   INPUT 56,
                                   INPUT "Acordo Comercial").
                APPLY  "ENTRY" TO wh-re1001a1-fill-acordo.
                UNDO,RETURN ERROR.
            END.
            
            IF es-acordo-comerc.nr-acordo-comerc <> "" THEN DO:

                FIND docum-est
                    WHERE ROWID(docum-est) = p-row-table NO-LOCK NO-ERROR.

                IF AVAIL docum-est THEN DO:
                    IF es-acordo-comerc.cod-emitente <> docum-est.cod-emitente THEN DO:

                        RUN utp/ut-msgs.p (INPUT "show":U,
                                           INPUT 17006,
                                           INPUT "Acordo Comercial N∆o Pertence ao Emitente do Documento Fiscal!").
                        APPLY  "ENTRY" TO wh-re1001a1-fill-acordo.
                        UNDO,RETURN ERROR.

                    END.
                    
                END.
            
                IF es-acordo-comerc.impresso = NO THEN DO:
                    RUN utp/ut-msgs.p (INPUT "show":U,
                                       INPUT 17006,
                                       INPUT "Acordo Comercial n∆o est† impresso! ~~ O Acordo Comercial deve estar impresso para efetuar o recebimento!").
                    APPLY  "ENTRY" TO wh-re1001a1-fill-acordo.
                    UNDO,RETURN ERROR.
                END.

                IF es-acordo-comerc.sit-acordo-contabil <> 2 THEN DO: /* Autorizado Cont†bil */
                    RUN utp/ut-msgs.p (INPUT "show":U,
                                       INPUT 17006,
                                       INPUT "Acordo Comercial n∆o Autorizado! ~~ O Acordo Comercial deve estar Autorizado Cont†bil No Monitor ESCM110!").
                    APPLY  "ENTRY" TO wh-re1001a1-fill-acordo.
                    UNDO,RETURN ERROR.
                END.
            
                FIND FIRST es-acordo-pendencia NO-LOCK
                    WHERE es-acordo-pendencia.nr-acordo-comerc = es-acordo-comerc.nr-acordo-comerc 
                      AND es-acordo-pendencia.ind-situacao <> 1 NO-ERROR. /* N∆o aprovado*/
            
                IF AVAIL es-acordo-pendencia THEN DO:
                    RUN utp/ut-msgs.p (INPUT "show":U,
                                       INPUT 17006,
                                       INPUT "Acordo Comercial n∆o Aprovado! ~~ Favor verificar a situaá∆o do Acordo Comercial, ele n∆o est† Aprovado!").
                    APPLY  "ENTRY" TO wh-re1001a1-fill-acordo.
                    UNDO,RETURN ERROR.
                END.
            
            END.
        END.
    END.
END.

IF p-ind-event  = "AFTER-ASSIGN" AND 
   p-ind-object = "CONTAINER"        THEN DO:

   FIND FIRST docum-est WHERE
        ROWID(docum-est) = p-row-table NO-ERROR.
   IF AVAIL docum-est THEN DO:            
      
       FIND ems2custom.ext-docum-est EXCLUSIVE-LOCK
            WHERE ext-docum-est.serie-docto  = docum-est.serie-docto
              AND ext-docum-est.nro-docto    = docum-est.nro-docto
              AND ext-docum-est.cod-emitente = docum-est.cod-emitente
              AND ext-docum-est.nat-operacao = docum-est.nat-operacao NO-ERROR.

      IF NOT AVAIL ext-docum-est THEN DO:
          CREATE ext-docum-est.
          ASSIGN ext-docum-est.serie-docto  = docum-est.serie-docto 
                 ext-docum-est.nro-docto    = docum-est.nro-docto   
                 ext-docum-est.cod-emitente = docum-est.cod-emitente
                 ext-docum-est.nat-operacao = docum-est.nat-operacao.

      END.

      ASSIGN ext-docum-est.nr-acordo-comerc  = wh-re1001a1-fill-acordo:SCREEN-VALUE.

   END. /* if avail docum-est */

END.

/* Zoom Acordo Comercial*/
IF p-ind-event = "zoom-acordo" THEN DO:

    DEFINE VARIABLE c-prog AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE h-campo AS HANDLE      NO-UNDO.
    DEFINE VARIABLE c-campo AS CHARACTER   NO-UNDO.

    RUN upc\upc-re1001a-z02.p(INPUT wh-re1001a1-fill-acordo, INPUT wh-pesquisa).
    
END.

RETURN "OK".
