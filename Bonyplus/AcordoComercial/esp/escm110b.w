&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
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
{include/i-prgvrs.i ESCM110B 2.12.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAM p-cod-estab            LIKE estabelec.cod-estabel                NO-UNDO. 
DEFINE INPUT PARAM p-cod-emitente         LIKE emitente.cod-emitente                NO-UNDO. 
DEFINE INPUT PARAM p-cod_espec_docto      LIKE es-tit_ap.cod_espec_docto            NO-UNDO. 
DEFINE INPUT PARAM p-serie-docto          LIKE ems2custom.ext-docum-est.serie-docto            NO-UNDO. 
DEFINE INPUT PARAM p-nro-docto            LIKE ems2custom.ext-docum-est.nro-docto              NO-UNDO. 

/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE tt-es-tit_ap NO-UNDO LIKE es-tit_ap
    FIELD dat_emis_docto      LIKE tit_ap.dat_liquidac_tit_ap
    FIELD dat_liquidac_tit_ap LIKE tit_ap.dat_liquidac_tit_ap
    FIELD dat_prev_pagto      LIKE tit_ap.dat_prev_pagto
    FIELD dat_vencto_tit_ap   LIKE tit_ap.dat_vencto_tit_ap
    FIELD val_origin_tit_ap   LIKE tit_ap.val_origin_tit_ap.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE JanelaDetalhe
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-es-tit_ap

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-es-tit_ap

/* Definitions for BROWSE br-es-tit_ap                                  */
&Scoped-define FIELDS-IN-QUERY-br-es-tit_ap tt-es-tit_ap.nr-acordo-comerc tt-es-tit_ap.cdn_fornecedor tt-es-tit_ap.cod_espec_docto tt-es-tit_ap.cod_estab tt-es-tit_ap.cod_parcela tt-es-tit_ap.cod_ser_docto tt-es-tit_ap.cod_tit_ap tt-es-tit_ap.dat_emis_docto tt-es-tit_ap.dat_liquidac_tit_ap tt-es-tit_ap.dat_prev_pagto Dt tt-es-tit_ap.dat_vencto_tit_ap tt-es-tit_ap.val_origin_tit_ap   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-es-tit_ap   
&Scoped-define SELF-NAME br-es-tit_ap
&Scoped-define QUERY-STRING-br-es-tit_ap FOR EACH tt-es-tit_ap NO-LOCK
&Scoped-define OPEN-QUERY-br-es-tit_ap OPEN QUERY {&SELF-NAME} FOR EACH tt-es-tit_ap NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-es-tit_ap tt-es-tit_ap
&Scoped-define FIRST-TABLE-IN-QUERY-br-es-tit_ap tt-es-tit_ap


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-es-tit_ap}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 br-es-tit_ap bt-ok bt-cancelar ~
bt-ajuda 

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
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 108 BY 1.38
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-es-tit_ap FOR 
      tt-es-tit_ap SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-es-tit_ap
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-es-tit_ap w-window _FREEFORM
  QUERY br-es-tit_ap NO-LOCK DISPLAY
      tt-es-tit_ap.nr-acordo-comerc FORMAT "x(12)":U
      tt-es-tit_ap.cdn_fornecedor FORMAT ">>>,>>>,>>9":U
      tt-es-tit_ap.cod_espec_docto FORMAT "x(3)":U
      tt-es-tit_ap.cod_estab FORMAT "x(5)":U
      tt-es-tit_ap.cod_parcela FORMAT "x(2)":U
      tt-es-tit_ap.cod_ser_docto FORMAT "x(3)":U
      tt-es-tit_ap.cod_tit_ap FORMAT "x(10)":U
      tt-es-tit_ap.dat_emis_docto FORMAT "99/99/9999":U COLUMN-LABEL "Data  EmissÆo"   
      tt-es-tit_ap.dat_liquidac_tit_ap FORMAT "99/99/9999":U LABEL "Data Liquidacao"
      tt-es-tit_ap.dat_prev_pagto FORMAT "99/99/9999":U LABEL "Dt Prev Pagto"  
      tt-es-tit_ap.dat_vencto_tit_ap FORMAT "99/99/9999":U 
      tt-es-tit_ap.val_origin_tit_ap FORMAT "->>>,>>>,>>9.99"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 108 BY 10.5
         TITLE "T¡tulo … Pagar" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-es-tit_ap AT ROW 1.21 COL 2 WIDGET-ID 200
     bt-ok AT ROW 12.21 COL 3
     bt-cancelar AT ROW 12.21 COL 14
     bt-ajuda AT ROW 12.21 COL 98.72
     RECT-1 AT ROW 12 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 132.14 BY 12.58 WIDGET-ID 100.


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
         WIDTH              = 109.29
         MAX-HEIGHT         = 21.13
         MAX-WIDTH          = 132.29
         VIRTUAL-HEIGHT     = 21.13
         VIRTUAL-WIDTH      = 132.29
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
/* BROWSE-TAB br-es-tit_ap RECT-1 F-Main */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-es-tit_ap
/* Query rebuild information for BROWSE br-es-tit_ap
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-es-tit_ap NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is OPENED
*/  /* BROWSE br-es-tit_ap */
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
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-es-tit_ap
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
  ENABLE RECT-1 br-es-tit_ap bt-ok bt-cancelar bt-ajuda 
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
  
  {utp/ut9000.i "ESCM110B" "2.12.00.001"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  RUN pi-carrega-browser.

  {&OPEN-QUERY-br-es-tit_ap}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-browser w-window 
PROCEDURE pi-carrega-browser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
EMPTY TEMP-TABLE tt-es-tit_ap.

    FOR EACH es-tit_ap NO-LOCK
        WHERE es-tit_ap.cod_estab       = p-cod-estab      
          AND es-tit_ap.cdn_fornecedor  = p-cod-emitente   
          AND es-tit_ap.cod_espec_docto = p-cod_espec_docto
          AND es-tit_ap.cod_ser_docto   = p-serie-docto    
          AND es-tit_ap.cod_tit_ap      = p-nro-docto :
    
        FIND tt-es-tit_ap
            WHERE es-tit_ap.cod_estab       = p-cod-estab      
              AND es-tit_ap.cdn_fornecedor  = p-cod-emitente   
              AND es-tit_ap.cod_espec_docto = p-cod_espec_docto
              AND es-tit_ap.cod_ser_docto   = p-serie-docto    
              AND es-tit_ap.cod_tit_ap      = p-nro-docto 
              AND es-tit_ap.cod_parcela     = tt-es-tit_ap.cod_parcela NO-ERROR.
    
        IF NOT AVAIL tt-es-tit_ap THEN DO:
    
            CREATE tt-es-tit_ap.
            ASSIGN tt-es-tit_ap.cod_estab        = es-tit_ap.cod_estab       
                   tt-es-tit_ap.cdn_fornecedor   = es-tit_ap.cdn_fornecedor  
                   tt-es-tit_ap.cod_espec_docto  = es-tit_ap.cod_espec_docto 
                   tt-es-tit_ap.cod_ser_docto    = es-tit_ap.cod_ser_docto   
                   tt-es-tit_ap.cod_tit_ap       = es-tit_ap.cod_tit_ap      
                   tt-es-tit_ap.cod_parcela      = es-tit_ap.cod_parcela.     
                   
        END.
    
        ASSIGN tt-es-tit_ap.nr-acordo-comerc = es-tit_ap.nr-acordo-comerc.
    
        FIND tit_ap NO-LOCK
            WHERE tit_ap.cod_estab       = es-tit_ap.cod_estab      
              AND tit_ap.cdn_fornecedor  = es-tit_ap.cdn_fornecedor 
              AND tit_ap.cod_espec_docto = es-tit_ap.cod_espec_docto
              AND tit_ap.cod_ser_docto   = es-tit_ap.cod_ser_docto  
              AND tit_ap.cod_tit_ap      = es-tit_ap.cod_tit_ap     
              AND tit_ap.cod_parcela     = es-tit_ap.cod_parcela NO-ERROR.
    
        IF AVAIL tit_ap THEN
    
            ASSIGN tt-es-tit_ap.dat_emis_docto      = tit_ap.dat_liquidac_tit_ap 
                   tt-es-tit_ap.dat_liquidac_tit_ap = tit_ap.dat_liquidac_tit_ap 
                   tt-es-tit_ap.dat_prev_pagto      = tit_ap.dat_prev_pagto      
                   tt-es-tit_ap.dat_vencto_tit_ap   = tit_ap.dat_vencto_tit_ap   
                   tt-es-tit_ap.val_origin_tit_ap   = tit_ap.val_origin_tit_ap.
        ELSE
            ASSIGN tt-es-tit_ap.dat_emis_docto      = ? 
                   tt-es-tit_ap.dat_liquidac_tit_ap = ? 
                   tt-es-tit_ap.dat_prev_pagto      = ? 
                   tt-es-tit_ap.dat_vencto_tit_ap   = ? 
                   tt-es-tit_ap.val_origin_tit_ap   = 0.
                   
    END.


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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-es-tit_ap"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

