/*****************************************************************************
 ** PROGRAMA..: UPC-RE1001A.P
 ** OBJETIVO..: UPC NA INCLUSAO DE NOTAS FISCAIS - RE1001A.W
 ** AUTOR.....: SZ - Chaves, Jeferson
 ** CLIENTE...: Bonyplus
 ** VERSAO....: 2.11.00.001 - 28/02/2014 
 ******************************************************************************/

/*************************** PAR∂METROS PADR«O *************************************/
DEFINE INPUT PARAMETER p-ind-event   AS CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object  AS CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object  AS HANDLE         NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame   AS WIDGET-HANDLE  NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table   AS CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER p-row-table   AS ROWID          NO-UNDO.
/*************************** GLOBAL VARIABLE DEFINITIONS **************************/
DEFINE NEW GLOBAL SHARED VARIABLE wh-re1001a-fill-depos           AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-re1001a-txt-depos            AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-re1001a-nat-opr-falso        AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-re1001a-nat-operacao         AS WIDGET-HANDLE  NO-UNDO. 
DEFINE NEW GLOBAL SHARED VARIABLE wh-re1001a-cb-cod-observa       AS WIDGET-HANDLE  NO-UNDO.  
DEFINE NEW GLOBAL SHARED VARIABLE wh-re1001a-nro-docto            AS WIDGET-HANDLE  NO-UNDO.  
DEFINE NEW GLOBAL SHARED VARIABLE wh-re1001a-dt-emissao           AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-re1001a-cod-estabel          AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-re1001a-txt-acordo           AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-re1001a-fill-acordo          AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-re1001a-dt-trans             AS WIDGET-HANDLE  NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE r-re1001a-ext-docum-est         AS ROWID NO-UNDO.
/*************************** LOCAL VARIABLE DEFINITIONS **************************/
DEFINE VARIABLE c-objeto       AS CHARACTER     NO-UNDO.
DEFINE VARIABLE h-objeto       AS HANDLE        NO-UNDO.
DEFINE VARIABLE wh-pesquisa    AS WIDGET-HANDLE NO-UNDO.

IF p-wgh-object <> ? THEN
    ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:FILE-NAME,'/'),
                                      p-wgh-object:FILE-NAME,'/').

/* MESSAGE "Evento..........:" string(p-ind-event)    SKIP  */
/*         "Objeto..........:" string(p-ind-object)   SKIP  */
/*         "Handle do Objeto:" string(p-wgh-object)   SKIP  */
/*         "Handle da Frame.:" string(p-wgh-frame)    SKIP  */
/*         "Tabela..........:" p-cod-table            SKIP  */
/*         "Rowid...........:" string(p-row-table)    SKIP  */
/*         "p-wgh-object....:" p-wgh-object:FILE-NAME SKIP  */
/*         "c-objeto........:" c-objeto VIEW-AS ALERT-BOX.  */

IF p-ind-event  = "AFTER-INITIALIZE":U 
AND p-ind-object = "CONTAINER" 
AND c-objeto     = "RE1001A.w" THEN DO:

    ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD
           h-objeto = h-objeto:FIRST-CHILD.

    DO  WHILE VALID-HANDLE(h-objeto):
        IF  h-objeto:TYPE <> "field-group" THEN DO:

            IF h-objeto:NAME = "nat-operacao" THEN 
                ASSIGN wh-re1001a-nat-operacao = h-objeto.
            
            IF h-objeto:NAME = "i-nro-docto" THEN 
                ASSIGN wh-re1001a-nro-docto = h-objeto.

            IF h-objeto:NAME = "cb-cod-observa" THEN 
                ASSIGN wh-re1001a-cb-cod-observa = h-objeto.

            IF h-objeto:NAME = "cod-estabel" THEN 
                ASSIGN wh-re1001a-cod-estabel = h-objeto.

            IF h-objeto:NAME = "dt-emissao" THEN 
                ASSIGN wh-re1001a-dt-emissao = h-objeto.

            IF h-objeto:NAME = "dt-trans" THEN 
                ASSIGN wh-re1001a-dt-trans = h-objeto.

            ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
        END.
        ELSE DO:
            ASSIGN h-objeto = h-objeto:first-child.
        END.
    END.


    CREATE TEXT wh-re1001a-txt-depos
    ASSIGN FRAME        = p-wgh-frame
           FORMAT       = "x(20)"
           WIDTH        = 9.5
           SCREEN-VALUE = "Dep¢sito:"
           ROW          = 9.0
           COL          = 69
           VISIBLE      = YES.
    
    CREATE FILL-IN wh-re1001a-fill-depos  
    ASSIGN FRAME             =  p-wgh-frame
           SIDE-LABEL-HANDLE =  wh-re1001a-txt-depos:HANDLE
           LABEL             = "Dep¢sito:"
           DATA-TYPE         = "CHARACTER"
           FORMAT            = "x(3)"
           WIDTH             = 6
           ROW               = 8.90
           COL               = 75.5 
           HEIGHT            = 0.88
           VISIBLE           = YES.

    CREATE FILL-IN wh-re1001a-nat-opr-falso
    ASSIGN NAME              = 'nat-opr-false':u       
           FRAME             = wh-re1001a-nat-operacao:FRAME       
           WIDTH             = wh-re1001a-nat-operacao:WIDTH       
           HEIGHT            = wh-re1001a-nat-operacao:HEIGHT      
           COLUMN            = wh-re1001a-nat-operacao:COL         
           ROW               = wh-re1001a-nat-operacao:ROW         
           FONT              = wh-re1001a-nat-operacao:FONT        
           FGCOLOR           = wh-re1001a-nat-operacao:FGCOLOR     
           HELP              = wh-re1001a-nat-operacao:HELP        
           TOOLTIP           = wh-re1001a-nat-operacao:TOOLTIP     
           VISIBLE           = YES                
           SENSITIVE         = wh-re1001a-nat-operacao:SENSITIVE   
           SCREEN-VALUE      = wh-re1001a-nat-operacao:SCREEN-VALUE
           TAB-STOP          = YES.

    /* Acordo Comercial */
    CREATE TEXT wh-re1001a-txt-acordo
    ASSIGN FRAME        = p-wgh-frame
           FORMAT       = "x(20)"
           WIDTH        = 12.29
           SCREEN-VALUE = "Acordo Comercial:"
           ROW          = 10.1
           COL          = 59.25
           VISIBLE      = YES.

    CREATE FILL-IN wh-re1001a-fill-acordo  
    ASSIGN FRAME             =  p-wgh-frame
           SIDE-LABEL-HANDLE =  wh-re1001a-txt-acordo:HANDLE
           LABEL             = "Acordo Comercial:"
           DATA-TYPE         = "CHARACTER"
           FORMAT            = "x(12)"
           WIDTH             = 10.1
           ROW               = 9.92
           COL               = 71.57 
           HEIGHT            = 0.88 
           VISIBLE           = YES
           SENSITIVE         = YES.

    wh-re1001a-nat-operacao:SENSITIVE = NO.
    wh-re1001a-nat-opr-falso:MOVE-AFTER-TAB-ITEM(wh-re1001a-nro-docto).           
    wh-re1001a-nat-opr-falso:MOVE-BEFORE-TAB-ITEM(wh-re1001a-cb-cod-observa).       
    wh-re1001a-nat-opr-falso:LOAD-MOUSE-POINTER('image/lupa.cur').
    wh-re1001a-fill-acordo:LOAD-MOUSE-POINTER('image/lupa.cur').
    wh-re1001a-fill-acordo:MOVE-AFTER-TAB-ITEM(wh-re1001a-dt-trans).

    ON 'F5' OF wh-re1001a-nat-opr-falso PERSISTENT
             RUN upc/upc-re1001a.p ("f5-nat-opr",?,?,?,?,?).
    ON "MOUSE-SELECT-DBLCLICK" OF wh-re1001a-nat-opr-falso PERSISTENT
             RUN upc/upc-re1001a.p ("f5-nat-opr",?,?,?,?,?).

    ON 'LEAVE' OF wh-re1001a-nat-opr-falso PERSISTENT
             RUN upc/upc-re1001a.p ("leave-nat-opr",?,?,?,?,?).

    /* Zoom acordo Comercial*/
    ON 'F5' OF wh-re1001a-fill-acordo OR 'mouse-select-dblclick' OF wh-re1001a-fill-acordo PERSISTENT
         RUN upc/upc-re1001a.p ("zoom-acordo",?,?,?,?,?).
END.

IF p-ind-event = "f5-nat-opr" THEN DO:

    RUN upc\upc-re1001a-z01.p(INPUT wh-re1001a-nat-opr-falso, INPUT wh-pesquisa).

/*     APPLY 'F5' TO wh-re1001a-nat-operacao. */

    ASSIGN wh-re1001a-nat-opr-falso:SCREEN-VALUE = wh-re1001a-nat-operacao:SCREEN-VALUE.
END.

/* Zoom Acordo Comercial*/
IF p-ind-event = "zoom-acordo" THEN DO:

    DEFINE VARIABLE c-prog AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE h-campo AS HANDLE      NO-UNDO.
    DEFINE VARIABLE c-campo AS CHARACTER   NO-UNDO.

    RUN upc\upc-re1001a-z02.p(INPUT wh-re1001a-fill-acordo, INPUT wh-pesquisa).
    
END.

IF p-ind-event = "leave-nat-opr" THEN DO:

    ASSIGN wh-re1001a-nat-operacao:SCREEN-VALUE = wh-re1001a-nat-opr-falso:SCREEN-VALUE.
    
    
    IF VALID-HANDLE(wh-re1001a-nat-operacao) THEN DO:
        FIND FIRST natur-oper NO-LOCK
            WHERE natur-oper.nat-operacao = wh-re1001a-nat-operacao:SCREEN-VALUE NO-ERROR.
        IF AVAIL natur-oper AND natur-oper.tipo-compra = 3 THEN DO:
            ASSIGN wh-re1001a-fill-depos:SENSITIVE = YES
                   wh-re1001a-fill-depos:SCREEN-VALUE = "".

            wh-re1001a-fill-depos:MOVE-AFTER-TAB-ITEM(wh-re1001a-cod-estabel).
            wh-re1001a-fill-depos:MOVE-BEFORE-TAB-ITEM(wh-re1001a-dt-emissao).
        END.
        ELSE DO:
            ASSIGN wh-re1001a-fill-depos:SENSITIVE = NO
                   wh-re1001a-fill-depos:SCREEN-VALUE = "".

        END.
    END.

    APPLY "leave" TO wh-re1001a-nat-operacao.
END.

IF p-ind-event   = "AFTER-DISABLE" 
AND p-ind-object = "CONTAINER" 
AND c-objeto     = "RE1001A1.w" THEN DO:
    
    IF VALID-HANDLE(wh-re1001a-fill-depos) THEN 
        ASSIGN wh-re1001a-fill-depos:SENSITIVE = NO.

END.

IF p-ind-event   = "BEFORE-ASSIGN" 
AND p-ind-object = "CONTAINER" 
AND c-objeto     = "RE1001A.w" THEN DO:

    IF VALID-HANDLE(wh-re1001a-fill-depos) AND wh-re1001a-fill-depos:SENSITIVE = YES THEN DO:
        FIND FIRST deposito NO-LOCK
            WHERE deposito.cod-depos = wh-re1001a-fill-depos:SCREEN-VALUE NO-ERROR.
        IF NOT AVAIL deposito THEN DO:
            RUN utp/ut-msgs.p (INPUT "show":U,
                               INPUT 530,
                               INPUT "").
            UNDO,RETURN ERROR.
        END.
    END.

    IF VALID-HANDLE(wh-re1001a-fill-acordo) THEN DO:

        IF wh-re1001a-fill-acordo:SCREEN-VALUE <> "" THEN DO:
            
            FIND es-acordo-comerc NO-LOCK
                WHERE es-acordo-comerc.nr-acordo-comerc = wh-re1001a-fill-acordo:SCREEN-VALUE NO-ERROR.
            IF NOT AVAIL es-acordo-comerc THEN DO:
                RUN utp/ut-msgs.p (INPUT "show":U,
                                   INPUT 56,
                                   INPUT "Acordo Comercial").
                APPLY  "ENTRY" TO wh-re1001a-fill-acordo.
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
                        APPLY  "ENTRY" TO wh-re1001a-fill-acordo.
                        UNDO,RETURN ERROR.

                    END.
                    
                END.
            
                IF es-acordo-comerc.impresso = NO THEN DO:
                    RUN utp/ut-msgs.p (INPUT "show":U,
                                       INPUT 17006,
                                       INPUT "Acordo Comercial n∆o est† impresso! ~~ O Acordo Comercial deve estar impresso para efetuar o recebimento!").
                    APPLY  "ENTRY" TO wh-re1001a-fill-acordo.
                    UNDO,RETURN ERROR.
                END.

                IF es-acordo-comerc.sit-acordo-contabil <> 2 THEN DO: /* Autorizado Cont†bil */
                    RUN utp/ut-msgs.p (INPUT "show":U,
                                       INPUT 17006,
                                       INPUT "Acordo Comercial n∆o Autorizado! ~~ O Acordo Comercial deve estar Autorizado Cont†bil No Monitor ESCM110!").
                    APPLY  "ENTRY" TO wh-re1001a-fill-acordo.
                    UNDO,RETURN ERROR.
                END.
            
                FIND FIRST es-acordo-pendencia NO-LOCK
                    WHERE es-acordo-pendencia.nr-acordo-comerc = es-acordo-comerc.nr-acordo-comerc 
                      AND es-acordo-pendencia.ind-situacao <> 1 NO-ERROR. /* N∆o aprovado*/
            
                IF AVAIL es-acordo-pendencia THEN DO:
                    RUN utp/ut-msgs.p (INPUT "show":U,
                                       INPUT 17006,
                                       INPUT "Acordo Comercial n∆o Aprovado! ~~ Favor verificar a situaá∆o do Acordo Comercial, ele n∆o est† Aprovado!").
                    APPLY  "ENTRY" TO wh-re1001a-fill-acordo.
                    UNDO,RETURN ERROR.
                END.


            
            END.
        END.
    END.
END.

IF p-ind-event   = "AFTER-ASSIGN" 
AND p-ind-object = "CONTAINER" 
AND c-objeto     = "RE1001A.w" THEN DO:

    IF (wh-re1001a-fill-depos:SCREEN-VALUE <> ""
        OR wh-re1001a-fill-acordo:SCREEN-VALUE <> "") THEN DO:
        FIND docum-est
            WHERE ROWID(docum-est) = p-row-table NO-LOCK NO-ERROR.
        IF AVAIL docum-est THEN DO:
            FIND ems2custom.ext-docum-est OF docum-est EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAIL ext-docum-est THEN DO:
                CREATE ext-docum-est.
                ASSIGN ext-docum-est.nat-operacao     = wh-re1001a-nat-operacao:SCREEN-VALUE
                       ext-docum-est.serie            = docum-est.serie
                       ext-docum-est.cod-emitente     = docum-est.cod-emitente
                       ext-docum-est.nro-docto        = docum-est.nro-docto
                       ext-docum-est.cod-depos        = STRING(wh-re1001a-fill-depos:SCREEN-VALUE)
                       ext-docum-est.nr-acordo-comerc = STRING(wh-re1001a-fill-acordo:SCREEN-VALUE).
            END.
            ASSIGN ext-docum-est.cod-depos        = STRING(wh-re1001a-fill-depos:SCREEN-VALUE) 
                   ext-docum-est.nr-acordo-comerc = STRING(wh-re1001a-fill-acordo:SCREEN-VALUE).
                   r-re1001a-ext-docum-est        = ROWID(ext-docum-est).
        END.
    END.
END.
