&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          ems2custom       PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESCM106-B01 2.12.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/browserd.w

/* Parameters Definitions ---                                           */
 
/* Local Variable Definitions ---                                       */
def new global shared var  c-seg-usuario       as char format "x(16)" NO-UNDO.
DEFINE VARIABLE c-desc-sit-ficha AS CHARACTER   NO-UNDO.

def buffer b-es-acordo-area-ficha for es-acordo-area-ficha.

/* variÿveis de uso local */
def var v-row-table   as rowid   no-undo.
def var de-vficha1    as decimal no-undo. 
def var de-vficha2    as decimal no-undo. 
def var de-vficha3    as decimal no-undo.
def var i-cont        as integer.
def var de-vl-verba2  as decimal no-undo.
/* fim das variaveis utilizadas no estilo */

/*:T Variaveis usadas internamente pelo estilo, favor nao elimina-las     */

/*:T v ri veis de uso globla */
def  var v-row-parent    as rowid no-undo.

/*:T vari veis de uso local */
/* def var v-row-table  as rowid no-undo. */

/*:T fim das variaveis utilizadas no estilo */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE BrowserCadastro2
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES es-acordo-area
&Scoped-define FIRST-EXTERNAL-TABLE es-acordo-area


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es-acordo-area.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES es-acordo-area-ficha

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table es-acordo-area-ficha.num-ficha es-acordo-area-ficha.ano-ficha es-acordo-area-ficha.dt-emiss es-acordo-area-ficha.dt-validade-ini es-acordo-area-ficha.dt-validade-fim es-acordo-area-ficha.vl-verba es-acordo-area-ficha.vl-pago es-acordo-area-ficha.vl-recebido es-acordo-area-ficha.vl-utilizado fn-sit-ficha() @ c-desc-sit-ficha es-acordo-area-ficha.usuario   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table   
&Scoped-define SELF-NAME br-table
&Scoped-define QUERY-STRING-br-table FOR EACH es-acordo-area-ficha OF es-acordo-area NO-LOCK     ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br-table OPEN QUERY {&SELF-NAME} FOR EACH es-acordo-area-ficha OF es-acordo-area NO-LOCK     ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br-table es-acordo-area-ficha
&Scoped-define FIRST-TABLE-IN-QUERY-br-table es-acordo-area-ficha


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-table RECT-5 bt-incluir bt-modificar ~
bt-eliminar 
&Scoped-Define DISPLAYED-OBJECTS de-fich1 de-fich2 de-fich3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
cod-area||y|ems2custom.es-acordo-area-ficha.cod-area
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "cod-area"':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS>
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE>
<FILTER-ATTRIBUTES>
************************
* Initialize Filter Attributes */
RUN set-attribute-list IN THIS-PROCEDURE ('
  Filter-Value=':U).
/************************
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-sit-ficha B-table-Win 
FUNCTION fn-sit-ficha RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-eliminar 
     LABEL "&Eliminar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-incluir 
     LABEL "&Incluir" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-modificar 
     LABEL "&Modificar" 
     SIZE 10 BY 1.

DEFINE VARIABLE de-fich1 AS DECIMAL FORMAT "->,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0 
     LABEL "Valor Total de Fichas" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE de-fich2 AS DECIMAL FORMAT "->,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0 
     LABEL "Valor Total Recebido" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE de-fich3 AS DECIMAL FORMAT "->,>>>,>>>,>>>,>>>,>>9.99" INITIAL 0 
     LABEL "Valor Total Pago" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 87.57 BY 3.88.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      es-acordo-area-ficha SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table B-table-Win _FREEFORM
  QUERY br-table NO-LOCK DISPLAY
      es-acordo-area-ficha.num-ficha FORMAT ">>>,>>9":U
      es-acordo-area-ficha.ano-ficha FORMAT "9999":U
      es-acordo-area-ficha.dt-emiss FORMAT "99/99/9999":U
      es-acordo-area-ficha.dt-validade-ini FORMAT "99/99/9999":U WIDTH 12
      es-acordo-area-ficha.dt-validade-fim FORMAT "99/99/9999":U WIDTH 12
      es-acordo-area-ficha.vl-verba FORMAT ">>>,>>>,>>>,>>>,>>>,>>9.99":U WIDTH 12
      es-acordo-area-ficha.vl-pago FORMAT ">>>,>>>,>>>,>>>,>>>,>>9.99":U WIDTH 12
      es-acordo-area-ficha.vl-recebido FORMAT ">>>,>>>,>>>,>>>,>>>,>>9.99":U WIDTH 12
      es-acordo-area-ficha.vl-utilizado FORMAT ">>>,>>>,>>>,>>>,>>>,>>9.99":U WIDTH 12
      fn-sit-ficha() @ c-desc-sit-ficha COLUMN-LABEL "Sit.Ficha" FORMAT "x(16)" WIDTH 18
      es-acordo-area-ficha.usuario FORMAT "x(12)":U WIDTH 12
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 87 BY 7.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-table AT ROW 1 COL 1
     bt-incluir AT ROW 8.83 COL 1
     bt-modificar AT ROW 8.83 COL 11
     bt-eliminar AT ROW 8.83 COL 21
     de-fich1 AT ROW 11.04 COL 26 COLON-ALIGNED HELP
          "Valor da Verba liberada." WIDGET-ID 2
     de-fich2 AT ROW 12.04 COL 26 COLON-ALIGNED HELP
          "Valor da Verba liberada." WIDGET-ID 12
     de-fich3 AT ROW 13.08 COL 26 COLON-ALIGNED HELP
          "Valor da Verba liberada." WIDGET-ID 14
     "Total" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 10.17 COL 4 WIDGET-ID 10
     RECT-5 AT ROW 10.38 COL 1 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: BrowserCadastro2
   External Tables: ems2custom.es-acordo-area
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 13.42
         WIDTH              = 87.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{utp/ut-glob.i}
{src/adm/method/browser.i}
{include/c-brows3.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit L-To-R                            */
/* BROWSE-TAB br-table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN de-fich1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN de-fich2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN de-fich3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH es-acordo-area-ficha OF es-acordo-area NO-LOCK
    ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Query            is NOT OPENED
*/  /* BROWSE br-table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-table
&Scoped-define SELF-NAME br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON MOUSE-SELECT-DBLCLICK OF br-table IN FRAME F-Main
DO:
    RUN New-State("DblClick, SELF":U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON ROW-ENTRY OF br-table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON ROW-LEAVE OF br-table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON U1 OF br-table IN FRAME F-Main
DO:
  /* Calcula e exibe os campos de totais abaixo do browse */

  assign de-vficha1 = 0
         de-vficha2 = 0
         de-vficha3 = 0.

  find first param-inv 
       where param-inv.ep-codigo = i-ep-codigo-usuario no-lock no-error.
  
  

  /* desvincula a query do browse */
  ASSIGN br-table:REFRESHABLE IN FRAME {&frame-name}= no.

  GET FIRST br-table.
  DO WHILE AVAIL es-acordo-area-ficha:           /* tabela da query */

     ASSIGN de-vficha1 = de-vficha1 + es-acordo-area-ficha.vl-verba
            de-vficha2 = de-vficha2 + es-acordo-area-ficha.vl-recebido
            de-vficha3 = de-vficha3 + es-acordo-area-ficha.vl-pago.
                           
     GET NEXT br-table.
     
  END.
  
   /* vincula a query no browse e torna disponivel o registro corrente */
  ASSIGN br-table:REFRESHABLE IN FRAME {&frame-name} = yes.
  br-table:FETCH-SELECTED-ROW(1) in frame {&frame-name} NO-ERROR.
 
  assign de-fich1:screen-value in frame {&frame-name} = string(de-vficha1)
         de-fich2:screen-value in frame {&frame-name} = string(de-vficha2)
         de-fich3:screen-value in frame {&frame-name} = string(de-vficha3).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON VALUE-CHANGED OF br-table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  /* run new-state('New-Line|':U + string(rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}))). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-eliminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-eliminar B-table-Win
ON CHOOSE OF bt-eliminar IN FRAME F-Main /* Eliminar */
DO:
   RUN pi-eliminar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-incluir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-incluir B-table-Win
ON CHOOSE OF bt-incluir IN FRAME F-Main /* Incluir */
DO:
  RUN pi-Incmod ('incluir':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-modificar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modificar B-table-Win
ON CHOOSE OF bt-modificar IN FRAME F-Main /* Modificar */
DO:
  RUN pi-Incmod ('modificar':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win  adm/support/_adm-opn.p
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Opens different cases of the query based on attributes
               such as the 'Key-Name', or 'SortBy-Case'
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEF VAR Filter-Value AS CHAR NO-UNDO.

  /* Copy 'Filter-Attributes' into local variables. */
  RUN get-attribute ('Filter-Value':U).
  Filter-Value = RETURN-VALUE.

  /* No Foreign keys are accepted by this SmartObject. */

  {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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
  {src/adm/template/row-list.i "es-acordo-area"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es-acordo-area"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  if avail es-acordo-area-ficha then  do:
     apply 'U1' to br-table in frame {&FRAME-NAME}.
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available B-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  if available es-acordo-area-ficha then do:
     apply 'U1' to br-table in frame {&FRAME-NAME}.
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view B-table-Win 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  apply 'value-changed':U to {&browse-name} in frame {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key B-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "cod-area" "es-acordo-area-ficha" "cod-area"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "es-acordo-area"}
  {src/adm/template/snd-list.i "es-acordo-area-ficha"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
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
      {src/adm/template/bstates.i}
  END CASE.
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-sit-ficha B-table-Win 
FUNCTION fn-sit-ficha RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    CASE es-acordo-area-ficha.situacao:
    
        WHEN 1 THEN
            ASSIGN c-desc-sit-ficha = "Ficha sem Acordo".
        WHEN 2 THEN
            ASSIGN c-desc-sit-ficha = "Ficha com Acordo".
        WHEN 3 THEN
            ASSIGN c-desc-sit-ficha = "Ficha Finalizada".
    
    END CASE.

    RETURN c-desc-sit-ficha.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

