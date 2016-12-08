&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME wWindow


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-nfe016 NO-UNDO LIKE nfe016
       field r-rowid as rowid.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWindow 
/*:T*******************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESNF004D 2.00.00.000}

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        ESNF004D
&GLOBAL-DEFINE Version        2.00.00.000

&GLOBAL-DEFINE WindowType     Detail

&GLOBAL-DEFINE Folder         NO
&GLOBAL-DEFINE InitialPage    1
&GLOBAL-DEFINE FolderLabels   

&GLOBAL-DEFINE page0Widgets   btOK btCancel btHelp2

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAM p-chave-acesso AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE NEW GLOBAL SHARED VARIABLE adm-broker-hdl AS HANDLE NO-UNDO.

DEFINE VARIABLE h-boes015       AS HANDLE      NO-UNDO.
DEFINE VARIABLE h-boad049       AS HANDLE      NO-UNDO.
DEFINE VARIABLE h-boin245       AS HANDLE      NO-UNDO.
DEFINE VARIABLE c-conta-aux     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-formato-conta AS CHARACTER   NO-UNDO.
DEFINE VARIABLE wh-pesquisa     AS HANDLE      NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rtToolBar c-serie-docto c-nro-docto ~
c-nat-operacao c-conta-contabil btOK btCancel btHelp2 
&Scoped-Define DISPLAYED-OBJECTS c-serie-docto c-nro-docto c-nat-operacao ~
c-desc-natur c-conta-contabil c-desc-conta 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWindow AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON btHelp2 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON btOK 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE c-conta-contabil AS CHARACTER FORMAT "x(17)" 
     LABEL "Conta Cont†bil" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88.

DEFINE VARIABLE c-desc-conta AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-natur AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 49.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-nat-operacao AS CHARACTER FORMAT "x(6)" 
     LABEL "Natureza Operaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 10.29 BY .88.

DEFINE VARIABLE c-nro-docto AS CHARACTER FORMAT "x(16)" 
     LABEL "Documento":R17 
     VIEW-AS FILL-IN 
     SIZE 12.86 BY .88.

DEFINE VARIABLE c-serie-docto AS CHARACTER FORMAT "x(5)" 
     LABEL "SÇrie":R7 
     VIEW-AS FILL-IN 
     SIZE 7.86 BY .88.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.42
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     c-serie-docto AT ROW 1.42 COL 19.43 COLON-ALIGNED WIDGET-ID 8
     c-nro-docto AT ROW 2.42 COL 19.43 COLON-ALIGNED WIDGET-ID 6
     c-nat-operacao AT ROW 3.42 COL 19.43 COLON-ALIGNED WIDGET-ID 4
     c-desc-natur AT ROW 3.42 COL 30 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     c-conta-contabil AT ROW 4.42 COL 19.43 COLON-ALIGNED WIDGET-ID 2
     c-desc-conta AT ROW 4.42 COL 38 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     btOK AT ROW 5.79 COL 2
     btCancel AT ROW 5.79 COL 13
     btHelp2 AT ROW 5.79 COL 80
     rtToolBar AT ROW 5.58 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 6.13
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tt-nfe016 T "?" NO-UNDO movnfe nfe016
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWindow ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 6.13
         WIDTH              = 90
         MAX-HEIGHT         = 31.04
         MAX-WIDTH          = 205.72
         VIRTUAL-HEIGHT     = 31.04
         VIRTUAL-WIDTH      = 205.72
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWindow 
/* ************************* Included-Libraries *********************** */

{window/window.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWindow
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME fpage0
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN c-desc-conta IN FRAME fpage0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-desc-natur IN FRAME fpage0
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWindow)
THEN wWindow:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fpage0
/* Query rebuild information for FRAME fpage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fpage0 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWindow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWindow wWindow
ON END-ERROR OF wWindow
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWindow wWindow
ON WINDOW-CLOSE OF wWindow
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancel wWindow
ON CHOOSE OF btCancel IN FRAME fpage0 /* Cancelar */
DO:
    ASSIGN c-conta-contabil:FORMAT IN FRAME fpage0 = "x(17)".

    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btHelp2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btHelp2 wWindow
ON CHOOSE OF btHelp2 IN FRAME fpage0 /* Ajuda */
DO:
    {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOK wWindow
ON CHOOSE OF btOK IN FRAME fpage0 /* OK */
DO:
    DEF BUFFER bnfe003 FOR nfe003.

    ASSIGN c-conta-contabil:FORMAT IN FRAME fpage0 = "x(17)".

    FIND nfe003 NO-LOCK WHERE
         nfe003.ch-acesso-comp-nfe = p-chave-acesso AND
         nfe003.idi-orig-trad      = 2 NO-ERROR.
    IF AVAIL nfe003 THEN DO:
        FIND FIRST bnfe003 NO-LOCK WHERE 
             bnfe003.cod-emitente = nfe003.cod-emitente AND
             bnfe003.serie-docto = c-serie-docto:SCREEN-VALUE IN FRAME {&FRAME-NAME} AND
             bnfe003.nro-docto   = c-nro-docto:SCREEN-VALUE IN FRAME {&FRAME-NAME} AND
            (bnfe003.idi-sit     = 6 
             OR bnfe003.idi-sit     = 7) NO-ERROR.
        IF NOT AVAIL bnfe003 THEN DO:
            RUN utp/ut-msgs.p ("show", 17006,
                               "Nota Fiscal informada Ç inv†lida.~~" +
                               "Documento n∆o importado ou situaá∆o difere de ~"Liberado para Integraá∆o~"").
            RETURN NO-APPLY.
        END.
    END.

    RUN efetivaMovimento.

    IF RETURN-VALUE = "NOK":U THEN
        RETURN NO-APPLY.

    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-conta-contabil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-conta-contabil wWindow
ON ENTRY OF c-conta-contabil IN FRAME fpage0 /* Conta Cont†bil */
DO:
    ASSIGN c-conta-contabil:FORMAT IN FRAME fPage0 = c-formato-conta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-conta-contabil wWindow
ON F5 OF c-conta-contabil IN FRAME fpage0 /* Conta Cont†bil */
DO:
    /*--- ZOOM SMART OBJECT ---*/
    ASSIGN l-implanta = NO.
    {include/zoomvar.i &prog-zoom=adzoom/z01ad049.w
                       &campo=c-conta-contabil
                       &campozoom=conta-contabil
                       &campo2=c-desc-conta
                       &campozoom2=titulo
                       &frame=fPage0}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-conta-contabil wWindow
ON LEAVE OF c-conta-contabil IN FRAME fpage0 /* Conta Cont†bil */
DO:
    ASSIGN c-conta-aux = INPUT FRAME fpage0 c-conta-contabil NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
        ASSIGN c-conta-contabil:FORMAT IN FRAME fpage0 = "x(17)".
    ELSE
        ASSIGN c-conta-contabil:FORMAT IN FRAME fpage0 = c-formato-conta.

    RUN goToChContaContabil IN h-boad049 (INPUT i-ep-codigo-usuario,
                                          INPUT INPUT FRAME fpage0 c-conta-contabil).
    IF RETURN-VALUE <> "NOK":U THEN
        RUN getCharField IN h-boad049 (INPUT "titulo":U, OUTPUT c-desc-conta).
    ELSE
        ASSIGN c-desc-conta = "".

    DISP c-desc-conta
        WITH FRAME fpage0.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-conta-contabil wWindow
ON MOUSE-SELECT-DBLCLICK OF c-conta-contabil IN FRAME fpage0 /* Conta Cont†bil */
DO:
    APPLY "F5":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-nat-operacao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-nat-operacao wWindow
ON F5 OF c-nat-operacao IN FRAME fpage0 /* Natureza Operaá∆o */
DO:
    {method/zoomfields.i &ProgramZoom="inzoom/z04in245.w"
                         &FieldZoom1="nat-operacao"
                         &FieldScreen1="c-nat-operacao"
                         &Frame1="fPage0"
                         &FieldZoom2="denominacao"
                         &FieldScreen2="c-desc-natur"
                         &Frame2="fPage0"
                         &EnableImplant="NO"} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-nat-operacao wWindow
ON LEAVE OF c-nat-operacao IN FRAME fpage0 /* Natureza Operaá∆o */
DO:
    {method/ReferenceFields.i &HandleDBOLeave="h-boin245"
                              &KeyValue1="c-nat-operacao:SCREEN-VALUE IN FRAME fPage0"
                              &FieldName1="denominacao"
                              &FieldScreen1="c-desc-natur"
                              &Frame1="fPage0"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-nat-operacao wWindow
ON MOUSE-SELECT-DBLCLICK OF c-nat-operacao IN FRAME fpage0 /* Natureza Operaá∆o */
DO:
    APPLY "F5":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWindow 


/*:T--- L¢gica para inicializaá∆o do programam ---*/
{window/mainblock.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterInitializeInterface wWindow 
PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    c-conta-contabil:LOAD-MOUSE-POINTER("image/lupa.cur":U) IN FRAME fPage0.
    c-nat-operacao:LOAD-MOUSE-POINTER("image/lupa.cur":U)   IN FRAME fPage0.

    FIND FIRST param-global NO-LOCK NO-ERROR.
    ASSIGN c-formato-conta = param-global.formato-conta-contabil.

    RUN initializeDBOs.
    RUN mostraCampos.

    IF INPUT FRAME fpage0 c-conta-contabil = "":U THEN
        RUN sugereConta IN h-boes015 (INPUT p-chave-acesso, OUTPUT c-conta-contabil).

    DISP c-conta-contabil WITH FRAME fPage0.

    IF INPUT FRAME fpage0 c-conta-contabil <> "":U then do:
        IF ERROR-STATUS:ERROR THEN
            ASSIGN c-conta-aux = INPUT FRAME fpage0 c-conta-contabil NO-ERROR.
    
        IF ERROR-STATUS:ERROR THEN
            ASSIGN c-conta-contabil:FORMAT IN FRAME fpage0 = "x(17)":U.
        ELSE
            ASSIGN c-conta-contabil:FORMAT IN FRAME fpage0 = c-formato-conta.

        APPLY "LEAVE":U TO c-conta-contabil IN FRAME fpage0.
    END.
    
    APPLY "LEAVE":U TO c-nat-operacao   IN FRAME fPage0.

    RETURN "OK":U.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beforeDestroyInterface wWindow 
PROCEDURE beforeDestroyInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF VALID-HANDLE(h-boes015) THEN DO:
        DELETE PROCEDURE h-boes015.
        ASSIGN h-boes015 = ?.
    END.

    IF VALID-HANDLE(h-boad049) THEN DO:
        DELETE PROCEDURE h-boad049.
        ASSIGN h-boad049 = ?.
    END.

    IF VALID-HANDLE(h-boin245) THEN DO:
        DELETE PROCEDURE h-boin245.
        ASSIGN h-boin245 = ?.
    END.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE efetivaMovimento wWindow 
PROCEDURE efetivaMovimento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE tt-nfe016.

    RUN emptyRowErrors IN h-boes015 .
    RUN goToKey        IN h-boes015 (INPUT p-chave-acesso).
    IF RETURN-VALUE = "OK":U THEN DO:
        RUN getRecord IN h-boes015 (OUTPUT TABLE tt-nfe016).
        
        FIND FIRST tt-nfe016 NO-ERROR.
        ASSIGN tt-nfe016.serie-docto    = INPUT FRAME fPage0 c-serie-docto   
               tt-nfe016.nro-docto      = INPUT FRAME fPage0 c-nro-docto     
               tt-nfe016.nat-operacao   = INPUT FRAME fPage0 c-nat-operacao  
               tt-nfe016.conta-contabil = INPUT FRAME fPage0 c-conta-contabil.

        RUN setRecord    IN h-boes015 (INPUT TABLE tt-nfe016).
        RUN updateRecord IN h-boes015 .
    END.
    ELSE DO:
        CREATE tt-nfe016.
        ASSIGN tt-nfe016.serie-docto        = INPUT FRAME fPage0 c-serie-docto   
               tt-nfe016.nro-docto          = INPUT FRAME fPage0 c-nro-docto     
               tt-nfe016.nat-operacao       = INPUT FRAME fPage0 c-nat-operacao  
               tt-nfe016.conta-contabil     = INPUT FRAME fPage0 c-conta-contabil
               tt-nfe016.ch-acesso-comp-nfe = p-chave-acesso
               tt-nfe016.tipo               = 2.
                                              
        RUN retornaEmitente IN h-boes015 (INPUT  p-chave-acesso, OUTPUT tt-nfe016.cod-emitente).
        RUN setRecord       IN h-boes015 (INPUT TABLE tt-nfe016).
        RUN createRecord    IN h-boes015 .
    END.

    IF RETURN-VALUE = "NOK":U THEN DO:
        RUN getRowErrors IN h-boes015 (OUTPUT TABLE RowErrors).

        {method/showmessage.i1}

        /*--- Transfere temp-table RowErrors para a tela de mensagens de erros ---*/
        {method/showmessage.i2}

        RETURN "NOK":U.
    END.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeDBOs wWindow 
PROCEDURE initializeDBOs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    IF NOT VALID-HANDLE(h-boes015)
    OR h-boes015:TYPE <> "PROCEDURE":U
    OR h-boes015:FILE-NAME <> "esbo/boes015.p":U THEN
        RUN esbo/boes015.p PERSISTENT SET h-boes015.
    RUN openQueryStatic IN h-boes015 (INPUT "Main":U) NO-ERROR.

    IF NOT VALID-HANDLE(h-boad049) 
    OR h-boad049:TYPE      <> "PROCEDURE":U 
    OR h-boad049:FILE-NAME <> "adbo/boad049na.p":U THEN DO:
        RUN adbo/boad049na.p PERSISTENT SET h-boad049.
    END.
    RUN openQueryStatic IN h-boad049 (INPUT "Main":U) NO-ERROR.

    /** Natureza de Operaá∆o **/
    IF NOT VALID-HANDLE(h-boin245) 
    OR h-boin245:TYPE      <> "PROCEDURE":U 
    OR h-boin245:FILE-NAME <> "inbo/boin245na.p":U THEN DO:
        RUN inbo/boin245na.p PERSISTENT SET h-boin245.
    END.
    RUN openQueryStatic IN h-boin245 (INPUT "Main":U) NO-ERROR.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mostraCampos wWindow 
PROCEDURE mostraCampos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    EMPTY TEMP-TABLE tt-nfe016.

    RUN goToKey IN h-boes015 (INPUT p-chave-acesso).
    IF RETURN-VALUE = "OK":U THEN DO:
        RUN getRecord IN h-boes015 (OUTPUT TABLE tt-nfe016).

        FIND FIRST tt-nfe016 NO-ERROR.
        ASSIGN c-serie-docto    = tt-nfe016.serie-docto
               c-nro-docto      = tt-nfe016.nro-docto
               c-nat-operacao   = tt-nfe016.nat-operacao
               c-conta-contabil = tt-nfe016.conta-contabil.
    END.
    ELSE
        ASSIGN c-serie-docto    = ""
               c-nro-docto      = ""
               c-nat-operacao   = ""
               c-conta-contabil = "".

    DISP c-serie-docto   
         c-nro-docto     
         c-nat-operacao  
         c-conta-contabil
        WITH FRAME fPage0.

    ENABLE c-serie-docto   
           c-nro-docto     
           c-nat-operacao  
           c-conta-contabil
        WITH FRAME fPage0.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

