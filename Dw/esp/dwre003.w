&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-window 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i DWRE003 2.12.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE VARIABLE i-seq-ini      AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-seq-final    AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-cont         AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-dispositivo  AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-caminho      AS CHAR FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE c-arquivo      AS CHARACTER NO-UNDO.
DEFINE VARIABLE h-acomp        AS HANDLE              NO-UNDO.
DEFINE STREAM s-etiq.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE JanelaDetalhe
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 RECT-4 IMAGE-5 IMAGE-6 ~
i-qt-etiqueta tg-reimpressao bt-ok bt-cancelar bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS i-ultima-etiqueta i-qt-etiqueta ~
tg-reimpressao i-etiq-ini i-etiq-fim 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-window AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "Gerar" 
     SIZE 10 BY 1.

DEFINE VARIABLE i-etiq-fim AS INTEGER FORMAT ">>>>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE i-etiq-ini AS INTEGER FORMAT ">>>>>>>>>9" INITIAL 0 
     LABEL "Etiqueta" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE i-qt-etiqueta AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Quantidade Etiqueta Batismo" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE i-ultima-etiqueta AS INTEGER FORMAT ">>>>>>>>>9":U INITIAL 0 
     LABEL "Èltima Etiqueta Batismo Gerada" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 77.86 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 10.25.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 60 BY 3.75.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 60 BY 3.75.

DEFINE VARIABLE tg-reimpressao AS LOGICAL INITIAL no 
     LABEL "Reimprimir Etiqueta Batismo." 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     i-ultima-etiqueta AT ROW 3.29 COL 42.29 COLON-ALIGNED WIDGET-ID 2
     i-qt-etiqueta AT ROW 4.29 COL 42.29 COLON-ALIGNED WIDGET-ID 4
     tg-reimpressao AT ROW 7.63 COL 14 WIDGET-ID 16
     i-etiq-ini AT ROW 8.88 COL 23.86 COLON-ALIGNED WIDGET-ID 18
     i-etiq-fim AT ROW 8.88 COL 46.14 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     bt-ok AT ROW 12.21 COL 3
     bt-cancelar AT ROW 12.21 COL 14
     bt-ajuda AT ROW 12.21 COL 69
     "Impress∆o" VIEW-AS TEXT
          SIZE 10 BY .67 AT ROW 2.21 COL 13 WIDGET-ID 10
     "Reimpress∆o" VIEW-AS TEXT
          SIZE 11.86 BY .67 AT ROW 6.63 COL 13.14 WIDGET-ID 14
     RECT-1 AT ROW 12 COL 2
     RECT-2 AT ROW 1.25 COL 2 WIDGET-ID 6
     RECT-3 AT ROW 2.5 COL 10.14 WIDGET-ID 8
     RECT-4 AT ROW 6.92 COL 10.29 WIDGET-ID 12
     IMAGE-5 AT ROW 8.92 COL 40.14 WIDGET-ID 22
     IMAGE-6 AT ROW 8.92 COL 45.14 WIDGET-ID 24
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 12.58 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: JanelaDetalhe
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-window ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert Custom SmartWindow title>"
         HEIGHT             = 12.67
         WIDTH              = 80
         MAX-HEIGHT         = 28.79
         MAX-WIDTH          = 195.14
         VIRTUAL-HEIGHT     = 28.79
         VIRTUAL-WIDTH      = 195.14
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-window 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-window.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-window
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN i-etiq-fim IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN i-etiq-ini IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN i-ultima-etiqueta IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON END-ERROR OF w-window /* <insert Custom SmartWindow title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* <insert Custom SmartWindow title> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-window
ON CHOOSE OF bt-ajuda IN FRAME F-Main /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-window
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Cancelar */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME F-Main /* Gerar */
DO:
    IF tg-reimpressao:SCREEN-VALUE = "yes" THEN
        RUN pi-reimpressao.
    ELSE
        RUN pi-gera-etiqueta.

   /* APPLY "close":U TO THIS-PROCEDURE.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-reimpressao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-reimpressao w-window
ON VALUE-CHANGED OF tg-reimpressao IN FRAME F-Main /* Reimprimir Etiqueta Batismo. */
DO:
    
    IF tg-reimpressao:SCREEN-VALUE = "yes" THEN
        ASSIGN i-etiq-ini:SENSITIVE = YES
               i-etiq-fim:SENSITIVE = YES
               i-qt-etiqueta:SCREEN-VALUE = "0"
               i-qt-etiqueta:SENSITIVE = NO.
    ELSE
        ASSIGN i-etiq-ini:SCREEN-VALUE = "0"
               i-etiq-fim:SCREEN-VALUE = "0"
               i-etiq-ini:SENSITIVE = NO
               i-etiq-fim:SENSITIVE = NO
               i-qt-etiqueta:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-window 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-window  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-window  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-window  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
  THEN DELETE WIDGET w-window.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-window  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY i-ultima-etiqueta i-qt-etiqueta tg-reimpressao i-etiq-ini i-etiq-fim 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE RECT-1 RECT-2 RECT-3 RECT-4 IMAGE-5 IMAGE-6 i-qt-etiqueta 
         tg-reimpressao bt-ok bt-cancelar bt-ajuda 
      WITH FRAME F-Main IN WINDOW w-window.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW w-window.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-window 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-window 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-window 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}
  
  {utp/ut9000.i "DWRE003" "2.12.00.001"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  FIND dw-batismo NO-LOCK NO-ERROR.
    IF AVAIL dw-batismo THEN
        ASSIGN i-ultima-etiqueta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(dw-batismo.seq-etiq-batismo).
    ELSE
        ASSIGN i-ultima-etiqueta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(0).


  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gera-etiqueta w-window 
PROCEDURE pi-gera-etiqueta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN i-seq-ini   = 0
           i-seq-final = 0.

    IF i-qt-etiqueta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0" THEN DO:
        RUN utp/ut-msgs.p (INPUT "show":U, 
                           INPUT 17006, 
                           INPUT "Quantidade Inv†lida!" + "~~" +
                                 "Quantidade de Etiqueta Deve Ser Maior Que Zero!").
        RETURN 'ADM-ERROR':U.
    END.

    FIND dw-batismo EXCLUSIVE-LOCK NO-ERROR.
    
        IF AVAIL dw-batismo THEN DO:
            ASSIGN i-seq-ini = dw-batismo.seq-etiq-batismo + 1
                   i-seq-final = dw-batismo.seq-etiq-batismo + int(i-qt-etiqueta:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
        
            ASSIGN dw-batismo.seq-etiq-batismo = dw-batismo.seq-etiq-batismo + int(i-qt-etiqueta:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

            RUN pi-imprime-etiqueta.
        
        END.

        ELSE DO:
            ASSIGN i-seq-ini = 0
                   i-seq-final =  INT(i-qt-etiqueta:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

            CREATE dw-batismo.
            ASSIGN dw-batismo.seq-etiq-batismo = int(i-qt-etiqueta:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

            RUN pi-imprime-etiqueta.

        END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime-etiqueta w-window 
PROCEDURE pi-imprime-etiqueta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FIND imprsor_usuar NO-LOCK
       WHERE imprsor_usuar.cod_usuario = c-seg-usuario
         AND imprsor_usuar.log_imprsor_princ NO-ERROR.
    IF AVAIL imprsor_usuar THEN
        ASSIGN c-dispositivo = imprsor_usuar.nom_disposit_so.
    ELSE DO:
        RUN utp/ut-msgs.p (INPUT "show",
                           INPUT 17006,
                           INPUT "Usu†rio sem impressora definida!").
        RETURN NO-APPLY.
    END.

    ASSIGN c-arquivo = SESSION:TEMP-DIRECTORY + 
                       STRING(RANDOM(1,99999)) + 
                       STRING(TIME)   + 
                       STRING(YEAR( TODAY),"9999")  +
                       STRING(MONTH(TODAY),"99"  )  + 
                       STRING(DAY(  TODAY),"99"  )  + ".tmp".

    OS-DELETE VALUE(c-arquivo) NO-ERROR.

    RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
    {utp/ut-liter.i Etiqueta_Batismo *}
    RUN pi-inicializar IN h-acomp (INPUT "Iniciando...").
    RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

    OUTPUT STREAM s-etiq TO VALUE(c-arquivo)APPEND.
    
    DO i-cont = i-seq-ini TO i-seq-final:

        RUN pi-acompanhar IN h-acomp (INPUT "Etiqueta Batismo: " + STRING(i-cont)).

        PUT STREAM s-etiq UNFORMATTED
            "^XA~TA000~JSN^LT0^MMT^MNW^MTT^PON^PMN^LH0,0^JMA^PR4,4^MD0^JUS^LRN^CI0^XZ" SKIP
            "^XA^LL0472" SKIP
            "^PW709" SKIP
            "^BY4,3,204^FT45,259^BCN,,Y,N" SKIP
            "^FD>:B>" "B" + STRING(i-cont) "^FS" SKIP
            "^PQ1,0,1,Y^XZ" SKIP.
        
    END.

    OUTPUT STREAM s-etiq CLOSE.

    /*ASSIGN c-dispositivo = '"\\10.2.1.100\Zebra"'.*/

    OS-COMMAND SILENT TYPE VALUE( c-arquivo ) > value(c-dispositivo).

    OS-DELETE VALUE(c-arquivo) NO-ERROR.

    RUN pi-finalizar IN h-acomp.

    IF tg-reimpressao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO" THEN

        ASSIGN i-ultima-etiqueta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(i-seq-final)
               i-qt-etiqueta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0" .

   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-reimpressao w-window 
PROCEDURE pi-reimpressao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN i-seq-ini   = 0
           i-seq-final = 0.

    FIND dw-batismo NO-LOCK NO-ERROR.

    IF AVAIL dw-batismo AND 
      (INPUT FRAME {&FRAME-NAME} i-etiq-ini > dw-batismo.seq-etiq-batismo OR
       INPUT FRAME {&FRAME-NAME} i-etiq-fim > dw-batismo.seq-etiq-batismo) THEN DO:
        
        RUN utp/ut-msgs.p (INPUT "show":U, 
                           INPUT 17006, 
                           INPUT "Etiqueta Batismo Inv†lida!" + "~~" +
                                 "Sequencia de Etiqueta de Batismo Inexistente!").
        RETURN 'ADM-ERROR':U.
       
    END.

    IF i-etiq-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} > i-etiq-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} THEN DO:
        RUN utp/ut-msgs.p (INPUT "show":U, 
                           INPUT 17006, 
                           INPUT "Sequencia Inv†lida!" + "~~" +
                                 "Etiqueta Inicial N∆o Pode Ser Maior que Etiqueta Final!").
        RETURN 'ADM-ERROR':U.
    END.

    ASSIGN i-seq-ini   = int(i-etiq-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}) 
           i-seq-final = int(i-etiq-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

    RUN pi-imprime-etiqueta.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-window  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this JanelaDetalhe, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-window 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

