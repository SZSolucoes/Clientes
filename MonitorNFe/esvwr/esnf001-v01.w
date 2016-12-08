&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems2custom       PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESNF001-V01 2.06.00.001}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */
/*                                                                                */
/* OBS: Para os smartobjects o parametro m¢dulo dever  ser MUT                    */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i <programa> MUT}
&ENDIF

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/viewerd.w

/* global variable definitions */
def new global shared var gr-doc-espec as rowid no-undo.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var v-row-parent as rowid no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES nfe008
&Scoped-define FIRST-EXTERNAL-TABLE nfe008


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR nfe008.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS nfe008.nat-operacao nfe008.serie 
&Scoped-define ENABLED-TABLES nfe008
&Scoped-define FIRST-ENABLED-TABLE nfe008
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS nfe008.cod-estabel nfe008.cod-emitente ~
nfe008.nat-operacao nfe008.serie 
&Scoped-define DISPLAYED-TABLES nfe008
&Scoped-define FIRST-DISPLAYED-TABLE nfe008
&Scoped-Define DISPLAYED-OBJECTS c-desc-estab c-desc-emit c-desc-natur 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS nfe008.cod-estabel nfe008.cod-emitente 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
cod-emitente||y|ems2custom.nfe008.cod-emitente
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "cod-emitente"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE c-desc-emit AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 48 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-estab AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 48 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-natur AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 48 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 2.5.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 2.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     nfe008.cod-estabel AT ROW 1.25 COL 21 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     c-desc-estab AT ROW 1.25 COL 31.29 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     nfe008.cod-emitente AT ROW 2.25 COL 21 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     c-desc-emit AT ROW 2.25 COL 31.29 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     nfe008.nat-operacao AT ROW 4.13 COL 21 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     c-desc-natur AT ROW 4.13 COL 31.29 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     nfe008.serie AT ROW 5.13 COL 21 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 3.75 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ems2custom.nfe008
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 5.67
         WIDTH              = 88.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{include/c-viewer.i}
{utp/ut-glob.i}
{include/i_dbtype.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN c-desc-emit IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-desc-estab IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-desc-natur IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nfe008.cod-emitente IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN nfe008.cod-estabel IN FRAME f-main
   NO-ENABLE 1                                                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME nfe008.cod-emitente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL nfe008.cod-emitente V-table-Win
ON F5 OF nfe008.cod-emitente IN FRAME f-main /* Fornecedor */
DO:
    {include/zoomvar.i &prog-zoom=adzoom/z01ad098.w
                       &campo=nfe008.cod-emitente
                       &campozoom=cod-emitente
                       &campo2=c-desc-emit
                       &campozoom2=nome-abrev}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL nfe008.cod-emitente V-table-Win
ON LEAVE OF nfe008.cod-emitente IN FRAME f-main /* Fornecedor */
DO:
  FIND FIRST emitente
        WHERE emitente.cod-emitente = INT(nfe008.cod-emitente:SCREEN-VALUE IN FRAME f-main) NO-LOCK NO-ERROR .

    IF AVAIL emitente THEN
        ASSIGN c-desc-emit:SCREEN-VALUE IN FRAME f-main  = emitente.nome-abrev .

    ELSE
        ASSIGN c-desc-emit:SCREEN-VALUE IN FRAME f-main  = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL nfe008.cod-emitente V-table-Win
ON MOUSE-SELECT-DBLCLICK OF nfe008.cod-emitente IN FRAME f-main /* Fornecedor */
DO:
  APPLY 'f5' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME nfe008.cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL nfe008.cod-estabel V-table-Win
ON F5 OF nfe008.cod-estabel IN FRAME f-main /* Estabelecimento */
DO:
    {include/zoomvar.i &prog-zoom="adzoom/z01ad107.w"
                       &campo=nfe008.cod-estabel
                       &campozoom=cod-estabel
                       &campo2=c-desc-estab
                       &campozoom2=nome
                       &frame=f-main}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL nfe008.cod-estabel V-table-Win
ON LEAVE OF nfe008.cod-estabel IN FRAME f-main /* Estabelecimento */
DO:
    FIND FIRST estabelec
        WHERE estabelec.cod-estabel = nfe008.cod-estabel:SCREEN-VALUE IN FRAME f-main NO-LOCK NO-ERROR .

    IF AVAIL estabelec THEN
        ASSIGN c-desc-estab:SCREEN-VALUE IN FRAME f-main  = estabelec.nome .

    ELSE
        ASSIGN c-desc-estab:SCREEN-VALUE IN FRAME f-main  = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL nfe008.cod-estabel V-table-Win
ON MOUSE-SELECT-DBLCLICK OF nfe008.cod-estabel IN FRAME f-main /* Estabelecimento */
DO:
    APPLY 'f5' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME nfe008.nat-operacao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL nfe008.nat-operacao V-table-Win
ON F5 OF nfe008.nat-operacao IN FRAME f-main /* Natureza Opera‡Æo */
DO:
    
    {include/zoomvar.i &prog-zoom  = inzoom/z01in245.w
                      &campo      = nfe008.nat-operacao
                      &campozoom  = nat-operacao
                      &campo2     = c-desc-natur
                      &campozoom2 = denominacao}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL nfe008.nat-operacao V-table-Win
ON LEAVE OF nfe008.nat-operacao IN FRAME f-main /* Natureza Opera‡Æo */
DO:
  FIND FIRST natur-oper
        WHERE natur-oper.nat-operacao = nfe008.nat-operacao:SCREEN-VALUE IN FRAME f-main NO-LOCK NO-ERROR .

    IF AVAIL natur-oper THEN
        ASSIGN c-desc-natur:SCREEN-VALUE IN FRAME f-main  = natur-oper.denominaca .

    ELSE
        ASSIGN c-desc-natur:SCREEN-VALUE IN FRAME f-main  = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL nfe008.nat-operacao V-table-Win
ON MOUSE-SELECT-DBLCLICK OF nfe008.nat-operacao IN FRAME f-main /* Natureza Opera‡Æo */
DO:
  APPLY 'f5' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
nfe008.cod-estabel:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.  
nfe008.cod-emitente:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.  
nfe008.nat-operacao:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.  

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win  adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "nfe008"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "nfe008"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME f-main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
/*     {include/i-valid.i} */
if  not frame {&frame-name}:validate() then
      return 'ADM-ERROR':U.
    
    /*:T Ponha na pi-validate todas as valida‡äes */

    IF adm-new-record THEN DO:
        FIND FIRST nfe008
            WHERE nfe008.cod-estabel = INPUT FRAME {&FRAME-NAME} nfe008.cod-estabel
              AND nfe008.cod-emitente = INPUT FRAME {&FRAME-NAME} nfe008.cod-emitente NO-LOCK NO-ERROR.
        IF AVAIL nfe008 THEN DO:
            RUN utp/ut-msgs.p (INPUT "show":U, 
                               INPUT 1, 
                               INPUT "Fornecedor").     
            APPLY "entry" TO nfe008.cod-estabel IN FRAME {&FRAME-NAME}.
            RETURN "adm-error".        
        END.
    END.   

    FIND estabelec NO-LOCK
        WHERE estabelec.cod-estabel = nfe008.cod-estabel:SCREEN-VALUE IN FRAME f-main NO-ERROR.
    IF NOT AVAIL estabelec THEN do:
        RUN utp/ut-msgs.p (INPUT "show":U,
                           INPUT 56,
                           INPUT "Estabelecimento").
        APPLY "entry":U TO nfe008.cod-estabel IN FRAME {&FRAME-NAME}.
        RETURN "adm-error":U.
    END.

    FIND emitente NO-LOCK
        WHERE emitente.cod-emitente = int(nfe008.cod-emitente:SCREEN-VALUE IN FRAME f-main) NO-ERROR.
    IF NOT AVAIL emitente THEN do:
        RUN utp/ut-msgs.p (INPUT "show":U,
                           INPUT 56,
                           INPUT "Emitente").
        APPLY "entry":U TO nfe008.cod-emitente IN FRAME {&FRAME-NAME}.
        RETURN "adm-error":U.
    END.

    IF nfe008.nat-operacao:SCREEN-VALUE IN FRAME f-main <> "" THEN DO:
        FIND natur-oper NO-LOCK
            WHERE natur-oper.nat-operacao = nfe008.nat-operacao:SCREEN-VALUE IN FRAME f-main NO-ERROR.
        
        IF NOT AVAIL natur-oper THEN do:
            RUN utp/ut-msgs.p (INPUT "show":U,
                               INPUT 56,
                               INPUT "Natureza").
            APPLY "entry":U TO nfe008.nat-operacao IN FRAME {&FRAME-NAME}.
            RETURN "adm-error":U.
        END.
    END.
    /*:T NÆo gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assignïs nÆo feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    disable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

    FIND FIRST emitente
        WHERE emitente.cod-emitente = INT(nfe008.cod-emitente:SCREEN-VALUE IN FRAME f-main) NO-LOCK NO-ERROR .

    IF AVAIL emitente THEN
        ASSIGN c-desc-emit:SCREEN-VALUE IN FRAME f-main  = emitente.nome-emit .

    ELSE
        ASSIGN c-desc-emit:SCREEN-VALUE IN FRAME f-main  = "".

    FIND FIRST estabelec
        WHERE estabelec.cod-estabel = nfe008.cod-estabel:SCREEN-VALUE IN FRAME f-main NO-LOCK NO-ERROR .

    IF AVAIL estabelec THEN
        ASSIGN c-desc-estab:SCREEN-VALUE IN FRAME f-main  = estabelec.nome .

    ELSE
        ASSIGN c-desc-estab:SCREEN-VALUE IN FRAME f-main  = "".

    FIND FIRST natur-oper
        WHERE natur-oper.nat-operacao = nfe008.nat-operacao:SCREEN-VALUE IN FRAME f-main NO-LOCK NO-ERROR .

    IF AVAIL natur-oper THEN
        ASSIGN c-desc-natur:SCREEN-VALUE IN FRAME f-main  = natur-oper.denominacao .

    ELSE
        ASSIGN c-desc-natur:SCREEN-VALUE IN FRAME f-main  = "".

    FIND nfe008 NO-LOCK
        WHERE nfe008.cod-estabel = nfe008.cod-estabel:SCREEN-VALUE IN FRAME f-main
          AND nfe008.cod-emitente = int(nfe008.cod-emitente:SCREEN-VALUE IN FRAME f-main) NO-ERROR.

    IF AVAIL nfe008 THEN DO:

        ASSIGN gr-doc-espec = rowid(nfe008).

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    if adm-new-record = yes then
        enable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-parent V-table-Win 
PROCEDURE pi-atualiza-parent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define input parameter v-row-parent-externo as rowid no-undo.
    
    assign v-row-parent = v-row-parent-externo.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate V-table-Win 
PROCEDURE Pi-validate :
/*:T------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: NÆo fazer assign aqui. Nesta procedure
  devem ser colocadas apenas valida‡äes, pois neste ponto do programa o registro 
  ainda nÆo foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /*:T Valida‡Æo de dicion rio */
    
/*:T    Segue um exemplo de valida‡Æo de programa */
/*       find tabela where tabela.campo1 = c-variavel and               */
/*                         tabela.campo2 > i-variavel no-lock no-error. */
      
      /*:T Este include deve ser colocado sempre antes do ut-msgs.p */
/*       {include/i-vldprg.i}                                             */
/*       run utp/ut-msgs.p (input "show":U, input 7, input return-value). */
/*       return 'ADM-ERROR':U.                                            */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key V-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "cod-emitente" "nfe008" "cod-emitente"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "nfe008"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

