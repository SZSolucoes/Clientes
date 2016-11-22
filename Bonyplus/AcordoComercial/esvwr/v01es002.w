&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgadm            PROGRESS
          ems2custom       PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i V99XX999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/viewerd.w

/* global variable definitions */
DEFINE NEW GLOBAL SHARED VAR c-seg-usuario as char format "x(12)" no-undo.

/* Parameters Definitions ---                                           */
DEFINE BUFFER b-ext-emitente for ext-emitente.

/* Local Variable Definitions ---                                       */
def var v-row-parent as rowid no-undo.

DEFINE VARIABLE l-acordo-comer-anterior LIKE ext-emitente.acordo-comerc.
DEFINE VARIABLE i-cod-area              LIKE ext-emitente.cod-area.
DEFINE VARIABLE d-vl-max-acordo         LIKE ext-emitente.vl-max-acordo.
DEFINE VARIABLE d-percentual-maximo     LIKE ext-emitente.percentual-maximo.

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
&Scoped-define EXTERNAL-TABLES emitente
&Scoped-define FIRST-EXTERNAL-TABLE emitente


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR emitente.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ext-emitente.acordo-comerc ~
ext-emitente.cod-area ext-emitente.vl-max-acordo ~
ext-emitente.percentual-maximo 
&Scoped-define ENABLED-TABLES ext-emitente
&Scoped-define FIRST-ENABLED-TABLE ext-emitente
&Scoped-Define ENABLED-OBJECTS rt-mold RECT-10 
&Scoped-Define DISPLAYED-FIELDS ext-emitente.acordo-comerc ~
ext-emitente.cod-area ext-emitente.vl-max-acordo ~
ext-emitente.percentual-maximo ext-emitente.usuario ext-emitente.data-alter ~
ext-emitente.hora-alter 
&Scoped-define DISPLAYED-TABLES ext-emitente
&Scoped-define FIRST-DISPLAYED-TABLE ext-emitente
&Scoped-Define DISPLAYED-OBJECTS c-area-comercial 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS></FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = ':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE c-area-comercial AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 36.43 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74.29 BY 8.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 8.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     ext-emitente.acordo-comerc AT ROW 1.88 COL 6.57 WIDGET-ID 14
          LABEL "Recebe Acordo Comercial"
          VIEW-AS TOGGLE-BOX
          SIZE 28 BY .83
     ext-emitente.cod-area AT ROW 3.08 COL 22.29 COLON-ALIGNED WIDGET-ID 16
          LABEL "Area Comercial"
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     c-area-comercial AT ROW 3.08 COL 34.43 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     ext-emitente.vl-max-acordo AT ROW 4.08 COL 22.29 COLON-ALIGNED WIDGET-ID 20
          LABEL "Valor M ximo Acordo"
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     ext-emitente.percentual-maximo AT ROW 5.08 COL 22.29 COLON-ALIGNED WIDGET-ID 22
          LABEL "% M ximo"
          VIEW-AS FILL-IN 
          SIZE 6 BY .88
     ext-emitente.usuario AT ROW 6.04 COL 22.29 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     ext-emitente.data-alter AT ROW 7 COL 22.14 COLON-ALIGNED WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     ext-emitente.hora-alter AT ROW 8 COL 22.14 COLON-ALIGNED WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     rt-mold AT ROW 1 COL 1
     RECT-10 AT ROW 1.25 COL 3 WIDGET-ID 12
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: mgadm.emitente
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
         HEIGHT             = 8.96
         WIDTH              = 78.
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

/* SETTINGS FOR TOGGLE-BOX ext-emitente.acordo-comerc IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN c-area-comercial IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ext-emitente.cod-area IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ext-emitente.data-alter IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ext-emitente.hora-alter IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ext-emitente.percentual-maximo IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ext-emitente.usuario IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ext-emitente.vl-max-acordo IN FRAME f-main
   EXP-LABEL                                                            */
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

&Scoped-define SELF-NAME ext-emitente.acordo-comerc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ext-emitente.acordo-comerc V-table-Win
ON VALUE-CHANGED OF ext-emitente.acordo-comerc IN FRAME f-main /* Recebe Acordo Comercial */
DO:
    IF ext-emitente.acordo-comerc:CHECKED = YES THEN

        ENABLE ext-emitente.cod-area
               ext-emitente.vl-max-acordo
               ext-emitente.percentual-maximo WITH FRAME f-main.
         
    ELSE
        DISABLE ext-emitente.cod-area
                ext-emitente.vl-max-acordo
                ext-emitente.percentual-maximo WITH FRAME f-main.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ext-emitente.cod-area
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ext-emitente.cod-area V-table-Win
ON F5 OF ext-emitente.cod-area IN FRAME f-main /* Area Comercial */
DO:
  
    {include/zoomvar.i &prog-zoom="esp/escm101-z01.w"
                       &campo=ext-emitente.cod-area
                       &campozoom=cod-area
                       &campo2=c-area-comercial
                       &campozoom2=descricao
                       &frame=f-main}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ext-emitente.cod-area V-table-Win
ON LEAVE OF ext-emitente.cod-area IN FRAME f-main /* Area Comercial */
DO:
    
    FIND es-acordo-area NO-LOCK
        WHERE es-acordo-area.cod-area = INT(ext-emitente.cod-area:SCREEN-VALUE IN FRAME f-main) NO-ERROR.
    
    IF AVAIL es-acordo-area THEN
    
        ASSIGN c-area-comercial:SCREEN-VALUE = es-acordo-area.descricao.
    
    ELSE 
    
        ASSIGN c-area-comercial:SCREEN-VALUE = "".
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ext-emitente.cod-area V-table-Win
ON MOUSE-SELECT-DBLCLICK OF ext-emitente.cod-area IN FRAME f-main /* Area Comercial */
DO:
    APPLY 'F5' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
ext-emitente.cod-area:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.  

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
  {src/adm/template/row-list.i "emitente"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "emitente"}

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
    {include/i-valid.i}
    
    /*:T Ponha na pi-validate todas as valida‡äes */
    /*:T NÆo gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
 

    FIND FIRST ext-emitente 
       WHERE ext-emitente.cod-emitente = emitente.cod-emitente EXCLUSIVE-LOCK NO-ERROR.

    IF ext-emitente.cod-area:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "0" THEN DO:

        FIND es-acordo-area NO-LOCK
            WHERE es-acordo-area.cod-area = int(ext-emitente.cod-area:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-ERROR.
        
        IF NOT AVAIL es-acordo-area THEN DO:
            RUN utp/ut-msgs.p (INPUT "show":U, 
                           INPUT 56, 
                           INPUT "Area Comercial").     
            APPLY "entry" TO ext-emitente.cod-area IN FRAME {&FRAME-NAME}.
            RETURN 'ADM-ERROR'.
        
        END.
    END.
    
    ASSIGN l-acordo-comer-anterior  = ext-emitente.acordo-comerc
           i-cod-area               = ext-emitente.cod-area
           d-vl-max-acordo          = ext-emitente.vl-max-acordo
           d-percentual-maximo      = ext-emitente.percentual-maximo.
 

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.

    
    /*if not avail ext-emitente then do:
        create ext-emitente.
        assign ext-emitente.cod-emitente = emitente.cod-emitente.
    end.*/

   /* l-acordo-comer-anterior
    i-cod-area             
    d-vl-max-acordo        
    d-percentual-maximo  */  

    
    /*:T Todos os assignïs nÆo feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */

    IF l-acordo-comer-anterior <> ext-emitente.acordo-comerc:CHECKED THEN
        ASSIGN ext-emitente.usuario    = c-seg-usuario
               ext-emitente.data-alter = TODAY
               ext-emitente.hora-alter = STRING(TIME,"HH:MM:SS").
    ELSE
        IF i-cod-area <> int(ext-emitente.cod-area:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN
            ASSIGN ext-emitente.usuario    = c-seg-usuario
                   ext-emitente.data-alter = TODAY
                   ext-emitente.hora-alter = STRING(TIME,"HH:MM:SS").

        ELSE
            IF d-vl-max-acordo <> DEC(ext-emitente.vl-max-acordo:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN
                ASSIGN ext-emitente.usuario    = c-seg-usuario
                       ext-emitente.data-alter = TODAY
                       ext-emitente.hora-alter = STRING(TIME,"HH:MM:SS").
            ELSE
                IF d-percentual-maximo <> DEC(ext-emitente.percentual-maximo:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN
                    ASSIGN ext-emitente.usuario    = c-seg-usuario
                           ext-emitente.data-alter = TODAY
                           ext-emitente.hora-alter = STRING(TIME,"HH:MM:SS").

   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.

    if not avail ext-emitente then do:
        create ext-emitente.
        assign ext-emitente.cod-emitente = emitente.cod-emitente.
    end.


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
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  find ext-emitente of emitente no-lock no-error.


  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
  
  if not avail ext-emitente then do with frame {&frame-name} :
      assign ext-emitente.acordo-comerc:checked = no
             ext-emitente.cod-area:screen-value = ""
             ext-emitente.vl-max-acordo:screen-value = "0"
             ext-emitente.percentual-maximo:screen-value = "0"
             ext-emitente.usuario:SCREEN-VALUE  = ""
             ext-emitente.hora-alter:SCREEN-VALUE = "".
  end.

  IF AVAIL ext-emitente AND ext-emitente.cod-area <> 0 THEN DO:
      FIND es-acordo-area NO-LOCK
          WHERE es-acordo-area.cod-area = ext-emitente.cod-area NO-ERROR.
      IF AVAIL es-acordo-area THEN
          ASSIGN c-area-comercial:SCREEN-VALUE = es-acordo-area.descricao.
      ELSE
          ASSIGN c-area-comercial:SCREEN-VALUE = "".

  END.
             
  /* Code placed here will execute AFTER standard behavior.    */

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

  /* There are no foreign keys supplied by this SmartObject. */

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
  {src/adm/template/snd-list.i "emitente"}

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

