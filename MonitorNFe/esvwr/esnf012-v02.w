&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          movnfe           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/********************************************************************************
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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var v-row-parent as rowid no-undo.

define new global shared variable wh-esnf012-ch-acesso as character format "x(254)" no-undo.
define new global shared variable r-rowid as rowid no-undo.

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
&Scoped-define EXTERNAL-TABLES nfe003
&Scoped-define FIRST-EXTERNAL-TABLE nfe003


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR nfe003.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS nfe003.cod-emitente nfe003.serie-docto ~
nfe003.nro-docto nfe003.cod-estabel nfe003.dt-emissao nfe003.dt-transacao ~
nfe003.nat-oper-comp 
&Scoped-define ENABLED-TABLES nfe003
&Scoped-define FIRST-ENABLED-TABLE nfe003
&Scoped-Define ENABLED-OBJECTS RECT-16 RECT-17 
&Scoped-Define DISPLAYED-FIELDS nfe003.cod-emitente nfe003.serie-docto ~
nfe003.nro-docto nfe003.cod-estabel nfe003.dt-emissao nfe003.dt-transacao ~
nfe003.nat-oper-comp 
&Scoped-define DISPLAYED-TABLES nfe003
&Scoped-define FIRST-DISPLAYED-TABLE nfe003
&Scoped-Define DISPLAYED-OBJECTS c-cnpj c-nome-abrev c-serie c-nro-docto ~
c-cod-estabel c-nome-est c-dt-emissao c-dt-transacao c-nat-oper-comp ~
c-desc-nat-oper-comp 

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
<FOREIGN-KEYS>
cod-emitente||y|movnfe.nfe003.cod-emitente
ch-acesso-comp-nfe||y|movnfe.nfe003.ch-acesso-comp-nfe
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "cod-emitente,ch-acesso-comp-nfe"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE c-cnpj AS CHARACTER FORMAT "X(256)":U 
     LABEL "CNPJ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-estabel AS CHARACTER FORMAT "X(256)":U 
     LABEL "Estab" 
     VIEW-AS FILL-IN 
     SIZE 7.43 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-nat-oper-comp AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 16.72 BY .88 NO-UNDO.

DEFINE VARIABLE c-dt-emissao AS DATE FORMAT "99/99/9999":U 
     LABEL "Emiss�o" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE c-dt-transacao AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt Trans" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE c-nat-oper-comp AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nat Comp" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE c-nome-abrev AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 15.29 BY .88 NO-UNDO.

DEFINE VARIABLE c-nome-est AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19.57 BY .88 NO-UNDO.

DEFINE VARIABLE c-nro-docto AS CHARACTER FORMAT "X(256)":U 
     LABEL "Documento" 
     VIEW-AS FILL-IN 
     SIZE 18.29 BY .88 NO-UNDO.

DEFINE VARIABLE c-serie AS CHARACTER FORMAT "X(256)":U 
     LABEL "S�rie" 
     VIEW-AS FILL-IN 
     SIZE 9.29 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42 BY 8.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42 BY 8.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     c-cnpj AT ROW 2.25 COL 10.72 COLON-ALIGNED WIDGET-ID 50
     nfe003.cod-emitente AT ROW 2.29 COL 55.86 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .88
     c-nome-abrev AT ROW 2.29 COL 67.72 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     c-serie AT ROW 3.25 COL 10.72 COLON-ALIGNED WIDGET-ID 34
     nfe003.serie-docto AT ROW 3.29 COL 55.86 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 9.14 BY .88
     c-nro-docto AT ROW 4.25 COL 10.72 COLON-ALIGNED WIDGET-ID 36
     nfe003.nro-docto AT ROW 4.29 COL 55.86 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 17.14 BY .88
     c-cod-estabel AT ROW 5.25 COL 10.86 COLON-ALIGNED WIDGET-ID 38
     nfe003.cod-estabel AT ROW 5.29 COL 55.86 COLON-ALIGNED WIDGET-ID 4
          LABEL "Estab"
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     c-nome-est AT ROW 5.29 COL 63.43 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     c-dt-emissao AT ROW 6.25 COL 10.86 COLON-ALIGNED WIDGET-ID 42
     nfe003.dt-emissao AT ROW 6.29 COL 55.86 COLON-ALIGNED WIDGET-ID 6
          LABEL "Emiss�o"
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     c-dt-transacao AT ROW 7.25 COL 10.86 COLON-ALIGNED WIDGET-ID 44
     nfe003.dt-transacao AT ROW 7.29 COL 55.86 COLON-ALIGNED WIDGET-ID 8
          LABEL "Dt Trans"
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     c-nat-oper-comp AT ROW 8.25 COL 10.86 COLON-ALIGNED WIDGET-ID 46
     nfe003.nat-oper-comp AT ROW 8.29 COL 55.86 COLON-ALIGNED WIDGET-ID 10
          LABEL "Nat Comp"
          VIEW-AS FILL-IN 
          SIZE 10.14 BY .88
     c-desc-nat-oper-comp AT ROW 8.29 COL 66.29 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     "Dados Traduzidos" VIEW-AS TEXT
          SIZE 13 BY .67 AT ROW 1.25 COL 46.57 WIDGET-ID 18
     "Dados Originais" VIEW-AS TEXT
          SIZE 12.14 BY .67 AT ROW 1.25 COL 2.86 WIDGET-ID 22
     RECT-16 AT ROW 1.54 COL 44.57 WIDGET-ID 16
     RECT-17 AT ROW 1.54 COL 1 WIDGET-ID 20
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: movnfe.nfe003
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
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
         HEIGHT             = 8.79
         WIDTH              = 85.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{include/c-viewer.i}
{utp/ut-glob.i}

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

/* SETTINGS FOR FILL-IN c-cnpj IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-cod-estabel IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-desc-nat-oper-comp IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-dt-emissao IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-dt-transacao IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-nat-oper-comp IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-nome-abrev IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-nome-est IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-nro-docto IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-serie IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nfe003.cod-estabel IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN nfe003.dt-emissao IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN nfe003.dt-transacao IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN nfe003.nat-oper-comp IN FRAME f-main
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

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

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
  {src/adm/template/row-list.i "nfe003"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "nfe003"}

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
    
    /* Ponha na pi-validate todas as valida��es */
    /* N�o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /* Todos os assign�s n�o feitos pelo assign-record devem ser feitos aqui */  
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

RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

find emitente
    where emitente.cod-emitente = int(nfe003.cod-emitente:screen-value in frame {&frame-name}) no-lock no-error.
if avail emitente then
    assign c-nome-abrev:screen-value in frame {&frame-name} = string(emitente.nome-abrev).

find estabelec
    where estabelec.cod-estabel = nfe003.cod-estabel:screen-value in frame {&frame-name} no-lock no-error.
if avail estabelec then
    assign c-nome-est:screen-value in frame {&frame-name} = string(estabelec.nome).

find natur-oper
    where natur-oper.nat-operacao = nfe003.nat-oper-comp:screen-value in frame {&frame-name} no-lock no-error.
if avail natur-oper then
    assign c-desc-nat-oper-comp:screen-value in frame {&frame-name} = string(natur-oper.denominacao).

find nfe003
    where rowid(nfe003) = r-rowid.

if avail nfe003 then
    assign wh-esnf012-ch-acesso = nfe003.ch-acesso-comp-nfe.

find nfe003
    where nfe003.ch-acesso-comp-nfe = wh-esnf012-ch-acesso 
      and nfe003.idi-orig-trad      = 1 no-lock no-error.

if avail nfe003 then

    assign c-cnpj:screen-value in frame {&frame-name}      = string(nfe003.cnpj)
           c-serie:screen-value in frame {&frame-name}         = string(nfe003.serie-docto)
           c-nro-docto:screen-value in frame {&frame-name}     = string(nfe003.nro-docto)
           c-cod-estabel:screen-value in frame {&frame-name}   = string(nfe003.cod-estabel)
           c-dt-emissao:screen-value in frame {&frame-name}    = string(nfe003.dt-emissao)
           c-dt-transacao:screen-value in frame {&frame-name}  = string(nfe003.dt-transacao)
           c-nat-oper-comp:screen-value in frame {&frame-name} = string(nfe003.nat-oper-comp).

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
/*------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: N�o fazer assign aqui. Nesta procedure
  devem ser colocadas apenas valida��es, pois neste ponto do programa o registro 
  ainda n�o foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /* Valida��o de dicion�rio */
    
/*/*    Segue um exemplo de valida��o de programa */
 *     find tabela where tabela.campo1 = c-variavel and
 *                       tabela.campo2 > i-variavel no-lock no-error.
 *     
 *     /* Este include deve ser colocado sempre antes do ut-msgs.p */
 *     {include/i-vldprg.i}
 *     run utp/ut-msgs.p (input "show":U, input 7, input return-value).
 *     return 'ADM-ERROR':U.*/

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
  {src/adm/template/sndkycas.i "cod-emitente" "nfe003" "cod-emitente"}
  {src/adm/template/sndkycas.i "ch-acesso-comp-nfe" "nfe003" "ch-acesso-comp-nfe"}

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
  {src/adm/template/snd-list.i "nfe003"}

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

