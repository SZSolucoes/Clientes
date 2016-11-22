/*****************************************************************************
 ** PROGRAMA..: ESCD1510-U01.P
 ** OBJETIVO..: UPC NA ATUALIZA€ÇO CLIENTES - CD01510
 ** AUTOR.....: SZ SOLU€åES
 ** CLIENTE...: BONYPLUS
 ** VERSAO....: 2.06.00.001 - 28/10/2010 - Jeferson M.
 ** ALTERA€åES:
 ******************************************************************************/

/*************************** PAR¶METROS PADRÇO *************************************/
DEFINE INPUT PARAMETER p-ind-event   AS CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object  AS CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object  AS HANDLE         NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame   AS WIDGET-HANDLE  NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table   AS CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER p-row-table   AS ROWID          NO-UNDO.

/*************************** GLOBAL VARIABLE DEFINITIONS **************************/
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd1510-label                AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd1510-campo                AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd1510a-label               AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd1510a-campo               AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd1510-atacado              AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd1510-campo-atacado        AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-retangulo-desconto          AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-nr-tabpre                   AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-moeda-fatur                 AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-ind-fat-par                 AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd1510b-label                AS WIDGET-HANDLE  NO-UNDO. /*Label Estabelecimento*/
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd1510b-campo                AS WIDGET-HANDLE  NO-UNDO. /*Fill Estabelecimento*/
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd1510c-label                AS WIDGET-HANDLE  NO-UNDO. /*Label Cliente Especial*/
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd1510c-campo                AS WIDGET-HANDLE  NO-UNDO. /*Toggle Cliente Especial*/
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd1510-lb-valid-dupli        AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd1510-valid-dupli           AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd1510-txt-duplic            AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-retangulo-desc-nf            AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd1510-txt-desc-nf           AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd1510c-nf                   AS WIDGET-HANDLE  NO-UNDO. /*Toggle desconto NF*/


DEFINE NEW GLOBAL SHARED VARIABLE wh-cd1510-regime-label         AS WIDGET-HANDLE  NO-UNDO. /*Label Regime Especial*/
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd1510-regime-campo         AS WIDGET-HANDLE  NO-UNDO. /*Campo Regime Especial*/

/*DEFINE NEW GLOBAL SHARED VARIABLE wh-txt-demonst-cd1510          AS WIDGET-HANDLE  NO-UNDO. /*Label Valor Demonstradora*/
DEFINE NEW GLOBAL SHARED VARIABLE wh-val-demonst-cd1510          AS WIDGET-HANDLE  NO-UNDO. /*Fill Valor Demonstradora*/*/
 

def var wh-pesquisa as widget-handle no-undo.
def var l-implanta as logical no-undo.

DEFINE NEW GLOBAL SHARED VARIABLE adm-broker-hdl                 AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE h-folder                       AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE h-viewer                       AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE h-viewer1                      AS HANDLE NO-UNDO.

/*************************** LOCAL VARIABLE DEFINITIONS **************************/

DEFINE VARIABLE h-frame        AS WIDGET-HANDLE  NO-UNDO. 
DEFINE VARIABLE wh-objeto      AS WIDGET-HANDLE  NO-UNDO.
DEFINE VARIABLE wgh-child      AS WIDGET-HANDLE  NO-UNDO.
DEFINE VARIABLE wgh-grupo      AS WIDGET-HANDLE  NO-UNDO.
DEFINE VARIABLE h-handle       AS HANDLE         NO-UNDO.
DEFINE VARIABLE h-object       AS HANDLE         NO-UNDO.
DEFINE VARIABLE h-object1      AS HANDLE         NO-UNDO.
DEFINE VARIABLE h-objeto       AS HANDLE         NO-UNDO.
DEFINE VARIABLE c-objeto       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-folder       AS CHARACTER      NO-UNDO.
DEFINE VARIABLE c-objects      AS CHARACTER      NO-UNDO.
DEFINE VARIABLE i-objects      AS INTEGER        NO-UNDO.
DEFINE VARIABLE l-record       AS LOGICAL        NO-UNDO INITIAL NO.
DEFINE VARIABLE l-group-assign AS LOGICAL        NO-UNDO INITIAL NO.
DEFINE VARIABLE l-state        AS LOGICAL        NO-UNDO INITIAL NO.

DEFINE VARIABLE c-nat-operacao AS CHARACTER   NO-UNDO.

    /*MESSAGE "Evento..........:" string(p-ind-event)    skip
            "Objeto..........:" string(p-ind-object)   skip
            "Handle do Objeto:" string(p-wgh-object)   skip
            "Handle da Frame.:" string(p-wgh-frame)    skip
            "Tabela..........:" p-cod-table            skip
            "Rowid...........:" string(p-row-table)    skip
            "p-wgh-object....:" p-wgh-object:file-name skip
            "c-objeto..........:" c-objeto view-as alert-box.*/

IF  p-ind-event  = "BEFORE-INITIALIZE"
AND p-ind-object = "CONTAINER" THEN DO:
 
   /*Aumentar o tamanho da Frame*/
    ASSIGN
        p-wgh-frame:PARENT:WIDTH = p-wgh-frame:PARENT:WIDTH + 6.0
        p-wgh-frame:WIDTH        = p-wgh-frame:WIDTH + 6.0.
        
END.

IF p-ind-event  = "before-initialize"
AND p-ind-object = "viewer"
AND p-wgh-object:FILE-NAME = "advwr/v19ad098.w" THEN DO:

     ASSIGN wgh-child = p-wgh-frame:FIRST-CHILD
            wgh-child = wgh-child:FIRST-CHILD.
     DO WHILE VALID-HANDLE(wgh-child):
         IF wgh-child:TYPE = "fill-in" THEN DO:
            IF wgh-child:NAME = "nr-tabpre" THEN
                 ASSIGN wh-nr-tabpre = wgh-child:HANDLE.
            ELSE
                IF wgh-child:NAME = "i-moeda-fatur" THEN
                 ASSIGN wh-moeda-fatur = wgh-child:HANDLE.
         END.
         ELSE
            IF wgh-child:TYPE = "toggle-box" THEN DO:
                IF wgh-child:NAME = "ind-fat-par" THEN
                     ASSIGN wh-ind-fat-par = wgh-child:HANDLE.
            END.                     
         ASSIGN wgh-child = wgh-child:NEXT-SIBLING.
     END.   

     IF VALID-HANDLE(p-wgh-frame) THEN DO:
     CREATE TEXT  wh-cd1510-label
     ASSIGN FRAME              = p-wgh-frame
            FORMAT             = "x(21)"
            WIDTH              = 33
            SCREEN-VALUE       = "Campo Novo:"
            ROW                = 2.15
            COL                = 45
            VISIBLE            = YES.

     create FILL-IN wh-cd1510-campo
     ASSIGN FRAME             =  p-wgh-frame
            SIDE-LABEL-HANDLE =  wh-cd1510-label:HANDLE
            LABEL             = "Natureza Bonifica‡Æo:"
            DATA-TYPE         = "character"
            FORMAT            = "x(06)"
            WIDTH             = 10.57
            ROW               = 2
            COL               = 60.49
            HEIGHT            = 0.88.

     wh-cd1510-campo:MOVE-AFTER-TAB-ITEM(wh-nr-tabpre).            

     /* Lupa */
    IF wh-cd1510-campo:LOAD-MOUSE-POINTER("image/lupa.cur") THEN .

     /* Zoom */
    ON 'F5' OF wh-cd1510-campo OR 'mouse-select-dblclick' OF wh-cd1510-campo PERSISTENT
         RUN upc/escd1510-u01.p ("zoomcampo",?,?,?,?,?).
        

    END.

    IF VALID-HANDLE(p-wgh-frame) THEN DO:
        ASSIGN p-wgh-frame:HEIGHT = p-wgh-frame:HEIGHT + 0.7.

        CREATE TEXT  wh-cd1510-regime-label
        ASSIGN FRAME              = p-wgh-frame
               FORMAT             = "x(22)"
               WIDTH              = 16
               SCREEN-VALUE       = "Regime Especial:"
               ROW                = 10.8
               COL                = 9.5
               HEIGHT             = 0.6
               VISIBLE            = YES.
        
        create FILL-IN wh-cd1510-regime-campo
        ASSIGN FRAME             =  p-wgh-frame
               SIDE-LABEL-HANDLE =  wh-cd1510-regime-label:HANDLE
               LABEL             = "Regime Especial:"
               DATA-TYPE         = "character"
               FORMAT            = "x(30)"
               WIDTH             = 20.57
               ROW               = 10.8
               COL               = 22
               HEIGHT            = 0.88.
    END.
END.

IF p-ind-event = "zoomcampo" OR p-ind-event = "zoomcampoB" THEN DO:
    DEFINE VARIABLE c-prog AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE h-campo AS HANDLE      NO-UNDO.
    DEFINE VARIABLE c-campo AS CHARACTER   NO-UNDO.

    IF p-ind-event = "zoomcampo" THEN
        RUN upc\escd1510z01.p(INPUT wh-cd1510-campo, INPUT wh-pesquisa).
    ELSE
        RUN upc\escd1510z02.p(INPUT wh-cd1510b-campo, INPUT wh-pesquisa).
END.

IF  p-ind-event  = "after-validate"
AND p-ind-object = "VIEWER"
AND p-wgh-object:FILE-NAME = "advwr/v19ad098.w" THEN DO:

    IF VALID-HANDLE(p-wgh-frame) THEN DO:
        FIND natur-oper
            WHERE natur-oper.nat-operacao = STRING(wh-cd1510-campo:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF NOT AVAIL natur-oper 
            AND wh-cd1510-campo:SCREEN-VALUE <> "" THEN DO:
           RUN utp/ut-msgs.p(INPUT "show",
                             INPUT "56",
                             INPUT "Natureza de Opera‡Æo").
           APPLY 'entry' TO wh-cd1510-campo.
           RETURN "nok".
        END.
    END.
END.

IF p-ind-event = "display"
AND p-ind-object = "viewer"
AND p-wgh-object:FILE-NAME = "advwr/v19ad098.w" THEN DO:

    IF VALID-HANDLE(p-wgh-frame)THEN DO:
        FIND emitente 
            WHERE ROWID(emitente) = p-row-table NO-LOCK NO-ERROR.
        IF AVAIL emitente THEN DO:
            FIND ext-emitente 
                WHERE ext-emitente.cod-emitente = emitente.cod-emitente EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL ext-emitente THEN 
                ASSIGN wh-cd1510-campo:SCREEN-VALUE = ext-emitente.natur-bonif
                       wh-cd1510-regime-campo:SCREEN-VALUE = ext-emitente.regime-especial.
            ELSE
                ASSIGN wh-cd1510-campo:SCREEN-VALUE = ""
                       wh-cd1510-regime-campo:SCREEN-VALUE = "".
                       
        END.    
    END.
END.
            
IF  p-ind-event  = "ENABLE"
AND p-ind-object = "VIEWER"
AND p-wgh-object:FILE-NAME = "advwr/v19ad098.w" THEN DO:
 
    IF VALID-HANDLE(p-wgh-frame) THEN DO:
       ASSIGN wh-cd1510-campo:SENSITIVE = YES.
    END.

    IF VALID-HANDLE(wh-cd1510-regime-campo) THEN
        ASSIGN wh-cd1510-regime-campo:SENSITIVE = YES.
END.     

IF  p-ind-event  = "disable"
AND p-ind-object = "VIEWER"
AND p-wgh-object:FILE-NAME = "advwr/v19ad098.w" THEN DO:

    IF VALID-HANDLE(p-wgh-frame) THEN DO:
       ASSIGN wh-cd1510-campo:SENSITIVE = NO.

    END.

    IF VALID-HANDLE(wh-cd1510-regime-campo) THEN
        ASSIGN wh-cd1510-regime-campo:SENSITIVE = NO.
END.

IF  p-ind-event  = "ASSIGN"
AND p-ind-object = "VIEWER"
AND p-wgh-object:FILE-NAME = "advwr/v19ad098.w" THEN DO:

    FIND FIRST emitente NO-LOCK
       WHERE ROWID(emitente) = p-row-table NO-ERROR.
    IF AVAIL emitente THEN DO:
        FIND FIRST ext-emitente 
           WHERE ext-emitente.cod-emitente = emitente.cod-emitente EXCLUSIVE-LOCK NO-ERROR.

        IF NOT AVAIL ext-emitente THEN DO:
            CREATE ext-emitente.
            ASSIGN ext-emitente.cod-emitente = emitente.cod-emitente
                   ext-emitente.natur-bonif  = STRING(wh-cd1510-campo:SCREEN-VALUE).

        END.

        ASSIGN ext-emitente.natur-bonif = STRING(wh-cd1510-campo:SCREEN-VALUE). 


        IF VALID-HANDLE(wh-cd1510-regime-campo) AND AVAIL ext-emitente THEN
            ASSIGN ext-emitente.regime-especial = wh-cd1510-regime-campo:SCREEN-VALUE.
     
    END.
END.

/* Aba Descontos */

IF p-ind-event  = "before-initialize"
AND p-ind-object = "viewer"
AND p-wgh-object:FILE-NAME = "advwr/v53ad098.w" THEN DO:

    CREATE RECTANGLE wh-retangulo-desconto
        ASSIGN FRAME         = p-wgh-frame
               WIDTH         = 34.3
               HEIGHT        = 2.40
               ROW           = 6.38
               COL           = 38.29
               VISIBLE       = YES
               SENSITIVE     = YES
               FGCOLOR      = 7.
    
     IF VALID-HANDLE(p-wgh-frame) THEN DO:
         CREATE TEXT  wh-cd1510-txt-duplic
         ASSIGN FRAME        = p-wgh-frame
                FORMAT       = "x(22)"
                WIDTH        = 15
                SCREEN-VALUE = "Desconto Duplicata"
                ROW          = 6.15
                COL          = 41.5
                VISIBLE      = YES
                FGCOLOR      = ?.    
     
         CREATE TEXT  wh-cd1510a-label
         ASSIGN FRAME        = p-wgh-frame
                FORMAT       = "x(22)"
                WIDTH        = 21
                SCREEN-VALUE = "Campo Novo:"
                ROW          = 6.85
                COL          = 50
                VISIBLE      = YES
                FGCOLOR      = 7.

         CREATE FILL-IN wh-cd1510a-campo
         ASSIGN FRAME             =  p-wgh-frame
                SIDE-LABEL-HANDLE =  wh-cd1510a-label:HANDLE
                LABEL             = "% Desconto:"
                DATA-TYPE         = "decimal"
                FORMAT            = ">>9.99"
                WIDTH             = 10
                ROW               = 6.70
                COL               = 59
                HEIGHT            = 0.88.
                
         CREATE TEXT  wh-cd1510-lb-valid-dupli
         ASSIGN FRAME        = p-wgh-frame
                FORMAT       = "x(22)"
                WIDTH        = 21
                SCREEN-VALUE = "Campo Novo:"
                ROW          = 7.75
                COL          = 45.40
                VISIBLE      = YES
                FGCOLOR      = 7.

         CREATE FILL-IN wh-cd1510-valid-dupli
         ASSIGN FRAME             =  p-wgh-frame
                SIDE-LABEL-HANDLE =  wh-cd1510-lb-valid-dupli:HANDLE
                LABEL             = "Validade Desconto:"
                DATA-TYPE         = "date"
                FORMAT            = "99/99/9999"
                WIDTH             = 10
                ROW               = 7.70
                COL               = 59
                HEIGHT            = 0.88.

         /* Desconto direto na nota-fiscal  */
         CREATE RECTANGLE wh-retangulo-desc-nf
         ASSIGN FRAME         = p-wgh-frame
                WIDTH         = 34.4
                HEIGHT        = 1
                ROW           = 9.10
                COL           = 38.29
                VISIBLE       = YES
                SENSITIVE     = YES
                FGCOLOR       = 7.

         CREATE TOGGLE-BOX wh-cd1510c-nf
         ASSIGN FRAME        = p-wgh-frame
                WIDTH        = 16
                LABEL        = "Inibir Desconto NF"
                HEIGHT       = 0.88
                COL          = 41.5
                ROW          = 9.12
                VISIBLE      = YES
                FONT         = 4
                SENSITIVE    = NO
                HIDDEN       = NO
                NAME         = "tg-desc-nf"
                CHECKED      = NO. 
                 

    END.
END.

IF p-ind-event = "display"
AND p-ind-object = "viewer"
AND p-wgh-object:FILE-NAME = "advwr/v53ad098.w" THEN DO:

    IF VALID-HANDLE(p-wgh-frame) THEN DO:
        FIND FIRST emitente NO-LOCK
            WHERE ROWID(emitente) = p-row-table NO-ERROR.
        IF AVAIL emitente THEN DO:
            FIND FIRST ext-emitente 
                WHERE ext-emitente.cod-emitente = emitente.cod-emitente EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL ext-emitente THEN 
                ASSIGN wh-cd1510a-campo:SCREEN-VALUE = STRING(ext-emitente.desc-duplicata)
                       wh-cd1510-valid-dupli:screen-value = if ext-emitente.dt-valid-dupli = ? then "" else string(ext-emitente.dt-valid-dupli)
                       wh-cd1510c-nf:CHECKED = ext-emitente.desconto-nf.
            ELSE
                ASSIGN wh-cd1510a-campo:SCREEN-VALUE = STRING(0)
                       wh-cd1510-valid-dupli:screen-value = ""
                       wh-cd1510c-nf:CHECKED = NO.
        END.    
    END.
END.
            
IF  p-ind-event  = "ENABLE"
AND p-ind-object = "VIEWER"
AND p-wgh-object:FILE-NAME = "advwr/v53ad098.w" THEN DO:
 
    IF VALID-HANDLE(p-wgh-frame) THEN DO:

       ASSIGN wh-cd1510a-campo:SENSITIVE = YES
              wh-cd1510a-label:FGCOLOR   = ?              
              wh-cd1510-valid-dupli:sensitive = yes
              wh-cd1510-valid-dupli:fgcolor   = ?
              wh-cd1510-lb-valid-dupli:FGCOLOR = ?
              wh-cd1510c-nf:SENSITIVE = YES.
              
    END.
END.     

IF  p-ind-event  = "disable"
AND p-ind-object = "VIEWER"
AND p-wgh-object:FILE-NAME = "advwr/v53ad098.w" THEN DO:
    IF VALID-HANDLE(p-wgh-frame) THEN DO:
       ASSIGN wh-cd1510a-campo:SENSITIVE = NO
              wh-cd1510a-label:FGCOLOR   = 7
              wh-cd1510-valid-dupli:sensitive = no
              wh-cd1510-valid-dupli:fgcolor = 7
              wh-cd1510-lb-valid-dupli:FGCOLOR = 7
              wh-cd1510c-nf:SENSITIVE = NO.
    END.
END.

IF  p-ind-event  = "ASSIGN"
AND p-ind-object = "VIEWER"
AND p-wgh-object:FILE-NAME = "advwr/v53ad098.w" THEN DO:

    FIND FIRST emitente NO-LOCK 
         WHERE ROWID(emitente) = p-row-table NO-ERROR.
    IF AVAIL emitente THEN DO:
        FIND FIRST ext-emitente 
             WHERE ext-emitente.cod-emitente = emitente.cod-emitente EXCLUSIVE-LOCK NO-ERROR.

        IF NOT AVAIL ext-emitente THEN DO:
            CREATE ext-emitente.
            ASSIGN ext-emitente.cod-emitente = emitente.cod-emitente.
        END.
                    
        ASSIGN ext-emitente.desc-duplicata = DEC(wh-cd1510a-campo:SCREEN-VALUE)
               ext-emitente.dt-valid-dupli = date(wh-cd1510-valid-dupli:SCREEN-VALUE)
               ext-emitente.desconto-nf    = wh-cd1510c-nf:CHECKED.            
    END.

END.

IF p-ind-event  = "before-initialize"
AND p-ind-object = "viewer"
AND p-wgh-object:FILE-NAME = "advwr/v20ad098.w" THEN DO:
    
    IF VALID-HANDLE(p-wgh-frame) THEN DO:
        CREATE TEXT  wh-cd1510b-label
            ASSIGN FRAME        = p-wgh-frame
                   FORMAT       = "x(7)"
                   WIDTH        = 8
                   SCREEN-VALUE = "Campo Novo:"
                   ROW          = 1.55
                   COL          = 46.50
                   VISIBLE      = YES.
        
        CREATE FILL-IN wh-cd1510b-campo
            ASSIGN FRAME             =  p-wgh-frame
                   SIDE-LABEL-HANDLE =  wh-cd1510b-label:HANDLE
                   LABEL             = "Estab:"
                   DATA-TYPE         = "character"
                   FORMAT            = "x(3)"
                   WIDTH             = 7
                   ROW               = 1.42
                   COL               = 51.3
                   HEIGHT            = 0.88.
                   
                   
            /* wh-cd1510b-campo:MOVE-AFTER-TAB-ITEM(wh-moeda-fatur). */
            /* Lupa */
            IF wh-cd1510b-campo:LOAD-MOUSE-POINTER("image/lupa.cur") THEN .
        
             /* Zoom */
            ON 'F5' OF wh-cd1510b-campo OR 'mouse-select-dblclick' OF wh-cd1510b-campo PERSISTENT
                RUN upc/escd1510-u01.p ("zoomcampoB",?,?,?,?,?). 
        
        CREATE TOGGLE-BOX wh-cd1510-campo-atacado
            ASSIGN FRAME             = p-wgh-frame
                   WIDTH             = 10
                   LABEL             = "Atacado"
                   HEIGHT            = 1
                   COL               = 42
                   ROW               = 4.40
                   VISIBLE           = YES
                   FONT              = 4
                   SENSITIVE         = NO
                   HIDDEN            = NO
                   NAME              = "tg-cliente-atacado"
                   CHECKED           = NO. 
        
        CREATE TOGGLE-BOX wh-cd1510c-campo
            ASSIGN FRAME             = p-wgh-frame
                   WIDTH             = 14
                   LABEL             = "Cliente Especial"
                   HEIGHT            = 1
                   COL               = 50.5
                   ROW               = 4.40
                   VISIBLE           = YES
                   FONT              = 4
                   SENSITIVE         = NO
                   HIDDEN            = NO
                   NAME              = "tg-cliente-especial"
                   CHECKED           = NO. 
       
    END.
END.

IF  p-ind-event  = "after-validate"
AND p-ind-object = "VIEWER"
AND p-wgh-object:FILE-NAME = "advwr/v20ad098.w" THEN DO:

    IF VALID-HANDLE(p-wgh-frame) THEN DO:
        FIND estabelec
            WHERE estabelec.cod-estabel = wh-cd1510b-campo:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF NOT AVAIL estabelec
            AND wh-cd1510b-campo:SCREEN-VALUE <> "" THEN DO:
            RUN utp/ut-msgs.p(INPUT "show",
                             INPUT "56",
                             INPUT "Estabelecimento").
           APPLY 'entry' TO wh-cd1510b-campo.
           RETURN "nok".
        END.
    END.
END.

IF p-ind-event = "display"
AND p-ind-object = "viewer"
AND p-wgh-object:FILE-NAME = "advwr/v20ad098.w" THEN DO:

    IF VALID-HANDLE(p-wgh-frame)THEN DO:
        FIND emitente 
            WHERE ROWID(emitente) = p-row-table NO-LOCK NO-ERROR.
        IF AVAIL emitente THEN DO:
            FIND ext-emitente 
                WHERE ext-emitente.cod-emitente = emitente.cod-emitente EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL ext-emitente THEN 
                ASSIGN wh-cd1510-campo-atacado:CHECKED = ext-emitente.atacado
                       wh-cd1510b-campo:SCREEN-VALUE = ext-emitente.cod-estabel
                       wh-cd1510c-campo:CHECKED = ext-emitente.cliente-espec.
            ELSE
                ASSIGN wh-cd1510-campo-atacado:CHECKED = NO
                       wh-cd1510b-campo:SCREEN-VALUE = ""
                       wh-cd1510c-campo:CHECKED  = NO.
            
        END.    
    END.
END.
            
IF  p-ind-event  = "ENABLE"
AND p-ind-object = "VIEWER"
AND p-wgh-object:FILE-NAME = "advwr/v20ad098.w" THEN DO:
 
    IF VALID-HANDLE(p-wgh-frame) THEN DO:
       ASSIGN wh-cd1510-campo-atacado:SENSITIVE = YES
              wh-cd1510b-campo:SENSITIVE = YES
              wh-cd1510c-campo:SENSITIVE = yes.
              /*wh-val-demonst-cd1510:sensitive = yes.*/
    END.
END.     

IF  p-ind-event  = "disable"
AND p-ind-object = "VIEWER"
AND p-wgh-object:FILE-NAME = "advwr/v20ad098.w" THEN DO:

    IF VALID-HANDLE(p-wgh-frame) THEN DO:
       ASSIGN wh-cd1510-campo-atacado:SENSITIVE = NO
              wh-cd1510b-campo:SENSITIVE = NO
              wh-cd1510c-campo:SENSITIVE = no.

    END.
END.

IF  p-ind-event  = "ASSIGN"
AND p-ind-object = "VIEWER"
AND p-wgh-object:FILE-NAME = "advwr/v20ad098.w" THEN DO:

    IF VALID-HANDLE(p-wgh-frame) THEN DO:

        FIND FIRST emitente NO-LOCK
            WHERE ROWID(emitente) = p-row-table NO-ERROR.
        IF AVAIL emitente THEN DO:
            FIND FIRST ext-emitente 
                WHERE ext-emitente.cod-emitente = emitente.cod-emitente EXCLUSIVE-LOCK NO-ERROR.
            
            IF AVAIL ext-emitente THEN 
                ASSIGN ext-emitente.atacado       = wh-cd1510-campo-atacado:CHECKED
                       ext-emitente.cod-estabel   = wh-cd1510b-campo:SCREEN-VALUE
                       ext-emitente.cliente-espec = wh-cd1510c-campo:CHECKED. 
            ELSE DO:
               CREATE ext-emitente.
               ASSIGN ext-emitente.cod-emitente = emitente.cod-emitente
                      ext-emitente.atacado      = wh-cd1510-campo-atacado:CHECKED
                      ext-emitente.cod-estabel  = wh-cd1510b-campo:SCREEN-VALUE
                      ext-emitente.cliente-espec = wh-cd1510c-campo:CHECKED.
            END.               
            
        END.
    END.
  
END.

/* Folder EDI - 27/03/2014 */
IF  p-ind-event  = "INITIALIZE" 
AND p-ind-object = "CONTAINER"
AND p-wgh-object:FILE-NAME = 'cdp/cd1510.w':U THEN DO:

    RUN get-link-handle IN adm-broker-hdl (INPUT p-wgh-object,
                                           INPUT "PAGE-SOURCE":U,
                                           OUTPUT c-folder).

    ASSIGN h-folder = WIDGET-HANDLE(c-folder) NO-ERROR.
       
    IF VALID-HANDLE(h-folder) THEN DO:

        RUN init-pages IN p-wgh-object (INPUT '1,2,3,4') NO-ERROR. 

        RUN create-folder-page IN h-folder (INPUT 5, 
                                            INPUT "EDI":U).
        RUN create-folder-label IN h-folder (INPUT 5, 
                                             INPUT "EDI":U).

        RUN select-page IN p-wgh-object (INPUT 5).
        RUN init-object IN p-wgh-object (INPUT "esvwr/v01es001.w":U, /* Nome do Objeto Viewer */
                                         INPUT p-wgh-frame,
                                         INPUT "Layout = ":U,
                                         OUTPUT h-viewer).
        RUN set-position IN h-viewer ( 7.10, 4.20).

        RUN SET-SIZE IN h-folder (INPUT p-wgh-frame:PARENT:HEIGHT ,
                                  p-wgh-frame:PARENT:WIDTH + 2.0 ).

        RUN get-link-handle IN adm-broker-hdl (INPUT p-wgh-object, 
                                               INPUT "CONTAINER-TARGET":U, 
                                               OUTPUT c-objects).

        DO i-objects = 1 TO NUM-ENTRIES(c-objects):
               ASSIGN h-object = WIDGET-HANDLE(ENTRY(i-objects, c-objects)).
               IF INDEX(h-object:PRIVATE-DATA, "q03ad098") <> 0 AND  /* query principal */
               NOT l-record THEN DO:
                   ASSIGN l-record = YES.
                   RUN add-link IN adm-broker-hdl (INPUT h-object, 
                                                   INPUT "Record":U,
                                                   INPUT h-viewer).
               END.
               IF INDEX(h-object:PRIVATE-DATA, "v19ad098") <> 0 AND /* viewer principal */
               NOT l-group-assign THEN DO:
                   ASSIGN l-group-assign = YES.
                   RUN add-link IN adm-broker-hdl (INPUT h-object, 
                                                   INPUT "Group-Assign":U,
                                                   INPUT h-viewer).
               END.
               IF INDEX(h-object:PRIVATE-DATA, "p-cadsim") <> 0 AND /* botoes comandos */
               NOT l-state THEN DO:
                   ASSIGN l-state = YES.
                   RUN add-link IN adm-broker-hdl (INPUT h-object, 
                                                   INPUT "State":U,
                                                   INPUT h-viewer).
               END.
        END.

        /* Folder Acordo Comercial */
        RUN create-folder-page IN h-folder (INPUT 6, 
                                            INPUT "Acordo Comercial":U).
        RUN create-folder-label IN h-folder (INPUT 6, 
                                             INPUT "Acordo Comercial":U).

        RUN select-page IN p-wgh-object (INPUT 6).
        RUN init-object IN p-wgh-object (INPUT "esvwr/v01es002.w":U, /* Nome do Objeto Viewer */
                                         INPUT p-wgh-frame,
                                         INPUT "Layout = ":U,
                                         OUTPUT h-viewer1).
        RUN set-position IN h-viewer1 ( 7.10, 7.20).

        RUN get-link-handle IN adm-broker-hdl (INPUT p-wgh-object, 
                                               INPUT "CONTAINER-TARGET":U, 
                                               OUTPUT c-objects).

       ASSIGN l-record       = NO
              l-group-assign = NO
              l-state        = NO.

       DO i-objects = 1 TO NUM-ENTRIES(c-objects):
            
               ASSIGN h-object1 = WIDGET-HANDLE(ENTRY(i-objects, c-objects)).
               IF INDEX(h-object1:PRIVATE-DATA, "q03ad098") <> 0 AND  /* query principal */
               NOT l-record THEN DO:
                   ASSIGN l-record = YES.
                   RUN add-link IN adm-broker-hdl (INPUT h-object1, 
                                                   INPUT "Record":U,
                                                   INPUT h-viewer1).
               END.
               
               IF INDEX(h-object1:PRIVATE-DATA, "v19ad098") <> 0 AND /* viewer principal */
               NOT l-group-assign THEN DO:
                   ASSIGN l-group-assign = YES.
                   RUN add-link IN adm-broker-hdl (INPUT h-object1, 
                                                   INPUT "Group-Assign":U,
                                                   INPUT h-viewer1).
               END.
               IF INDEX(h-object1:PRIVATE-DATA, "p-cadsim") <> 0 AND /* botoes comandos */
               NOT l-state THEN DO:
                   ASSIGN l-state = YES.
                   RUN add-link IN adm-broker-hdl (INPUT h-object1, 
                                                   INPUT "State":U,
                                                   INPUT h-viewer1).
               END.
        END.

        RUN dispatch IN h-viewer ("initialize":U).
        RUN dispatch IN h-viewer1 ("initialize":U).
        RUN select-page IN p-wgh-object (INPUT 5). 
        RUN select-page IN p-wgh-object (INPUT 6). 
        RUN select-page IN p-wgh-object (INPUT 1).
        
    END. 
END.
RETURN "ok".
