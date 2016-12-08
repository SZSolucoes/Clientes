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

define new global shared variable c-chave as character format "x(254)" no-undo.
def new global shared var r-rowid-linha as rowid no-undo.

def var c-ch-acesso as character format "x(254)" no-undo.
def var c-sequencia as character no-undo.
def buffer bf-nfe013 for nfe013.

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
&Scoped-define EXTERNAL-TABLES nfe013
&Scoped-define FIRST-EXTERNAL-TABLE nfe013


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR nfe013.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS nfe013.it-codigo nfe013.nat-operacao ~
nfe013.num-pedido nfe013.numero-ordem nfe013.qtd-interna nfe013.int-1 ~
nfe013.preco-unit nfe013.preco-total 
&Scoped-define ENABLED-TABLES nfe013
&Scoped-define FIRST-ENABLED-TABLE nfe013
&Scoped-Define ENABLED-OBJECTS RECT-16 RECT-30 
&Scoped-Define DISPLAYED-FIELDS nfe013.seq-item nfe013.it-codigo ~
nfe013.nat-operacao nfe013.num-pedido nfe013.numero-ordem ~
nfe013.log-fifo-oc nfe013.qtd-interna nfe013.int-1 nfe013.preco-unit ~
nfe013.un-interna nfe013.preco-total 
&Scoped-define DISPLAYED-TABLES nfe013
&Scoped-define FIRST-DISPLAYED-TABLE nfe013
&Scoped-Define DISPLAYED-OBJECTS c-seq-item c-it-codigo c-cfop c-desc-item ~
c-ped-compra c-desc-nat-oper c-ord-compra c-qtd-emit c-un-emit c-preco-unit ~
c-preco-total 

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
ch-acesso-comp-nfe|y|y|movnfe.nfe013.ch-acesso-comp-nfe
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "ch-acesso-comp-nfe",
     Keys-Supplied = "ch-acesso-comp-nfe"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE c-cfop AS CHARACTER FORMAT "X(256)":U 
     LABEL "CFOP" 
     VIEW-AS FILL-IN 
     SIZE 9.43 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-item AS CHARACTER FORMAT "X(256)":U 
     LABEL "Descri‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 32 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-nat-oper AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 21.57 BY .88 NO-UNDO.

DEFINE VARIABLE c-it-codigo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 23 BY .88 NO-UNDO.

DEFINE VARIABLE c-ord-compra AS CHARACTER FORMAT "X(256)":U 
     LABEL "Num. Ordem" 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88 NO-UNDO.

DEFINE VARIABLE c-ped-compra AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ped. Compra" 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88 NO-UNDO.

DEFINE VARIABLE c-preco-total AS CHARACTER FORMAT "X(256)":U 
     LABEL "Pre‡o Total" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE c-preco-unit AS CHARACTER FORMAT "X(256)":U 
     LABEL "Pre‡o Unit." 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE c-qtd-emit AS CHARACTER FORMAT "X(256)":U 
     LABEL "Qtd. Emitente" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE c-seq-item AS CHARACTER FORMAT "X(256)":U 
     LABEL "Seq" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE c-un-emit AS CHARACTER FORMAT "X(256)":U 
     LABEL "UN Emitente" 
     VIEW-AS FILL-IN 
     SIZE 7.43 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 45.72 BY 9.71.

DEFINE RECTANGLE RECT-30
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38 BY 9.71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     c-seq-item AT ROW 2 COL 12.57 COLON-ALIGNED WIDGET-ID 82
     nfe013.seq-item AT ROW 2 COL 51 COLON-ALIGNED WIDGET-ID 56
          VIEW-AS FILL-IN 
          SIZE 7 BY .88
     c-it-codigo AT ROW 3 COL 12.57 COLON-ALIGNED WIDGET-ID 84
     nfe013.it-codigo AT ROW 3 COL 51 COLON-ALIGNED WIDGET-ID 58
          VIEW-AS FILL-IN 
          SIZE 32 BY .88
     c-cfop AT ROW 4 COL 12.57 COLON-ALIGNED WIDGET-ID 86
     c-desc-item AT ROW 4 COL 51 COLON-ALIGNED WIDGET-ID 60
     c-ped-compra AT ROW 5 COL 12.57 COLON-ALIGNED WIDGET-ID 88
     nfe013.nat-operacao AT ROW 5 COL 51 COLON-ALIGNED WIDGET-ID 62
          LABEL "Nat. Opera‡Æo"
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     c-desc-nat-oper AT ROW 5 COL 61.43 COLON-ALIGNED NO-LABEL WIDGET-ID 80
     c-ord-compra AT ROW 6 COL 12.57 COLON-ALIGNED WIDGET-ID 90
     nfe013.num-pedido AT ROW 6 COL 51 COLON-ALIGNED WIDGET-ID 64
          LABEL "Ped. Compra"
          VIEW-AS FILL-IN 
          SIZE 13 BY .88
     c-qtd-emit AT ROW 7 COL 12.57 COLON-ALIGNED WIDGET-ID 92
     nfe013.numero-ordem AT ROW 7 COL 51 COLON-ALIGNED WIDGET-ID 66
          LABEL "Ord. Compra"
          VIEW-AS FILL-IN 
          SIZE 13 BY .88
     nfe013.log-fifo-oc AT ROW 7.04 COL 67 WIDGET-ID 76
          VIEW-AS TOGGLE-BOX
          SIZE 19 BY .83
     c-un-emit AT ROW 8 COL 12.57 COLON-ALIGNED WIDGET-ID 94
     nfe013.qtd-interna AT ROW 8 COL 51 COLON-ALIGNED WIDGET-ID 68
          VIEW-AS FILL-IN 
          SIZE 17 BY .88
     nfe013.int-1 AT ROW 8 COL 76 COLON-ALIGNED WIDGET-ID 78
          VIEW-AS FILL-IN 
          SIZE 7 BY .88
     c-preco-unit AT ROW 9 COL 12.57 COLON-ALIGNED WIDGET-ID 96
     nfe013.preco-unit AT ROW 9 COL 51 COLON-ALIGNED WIDGET-ID 74
          LABEL "Pre‡o Unit."
          VIEW-AS FILL-IN 
          SIZE 15 BY .88
     nfe013.un-interna AT ROW 9 COL 75 COLON-ALIGNED WIDGET-ID 70
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     c-preco-total AT ROW 10 COL 12.57 COLON-ALIGNED WIDGET-ID 98
     nfe013.preco-total AT ROW 10 COL 51 COLON-ALIGNED WIDGET-ID 72
          VIEW-AS FILL-IN 
          SIZE 15 BY .88
     "Dados Traduzidos" VIEW-AS TEXT
          SIZE 13 BY .67 AT ROW 1.25 COL 44 WIDGET-ID 24
     "Dados Originais" VIEW-AS TEXT
          SIZE 13 BY .67 AT ROW 1.25 COL 3.14 WIDGET-ID 22
     RECT-16 AT ROW 1.54 COL 41.29 WIDGET-ID 18
     RECT-30 AT ROW 1.54 COL 1 WIDGET-ID 20
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: movnfe.nfe013
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
         HEIGHT             = 10.25
         WIDTH              = 86.
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

/* SETTINGS FOR FILL-IN c-cfop IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-desc-item IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-desc-nat-oper IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-it-codigo IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-ord-compra IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-ped-compra IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-preco-total IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-preco-unit IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-qtd-emit IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-seq-item IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-un-emit IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX nfe013.log-fifo-oc IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nfe013.nat-operacao IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN nfe013.num-pedido IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN nfe013.numero-ordem IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN nfe013.preco-unit IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN nfe013.seq-item IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nfe013.un-interna IN FRAME f-main
   NO-ENABLE                                                            */
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
  DEF VAR key-value AS CHAR NO-UNDO.
  DEF VAR row-avail-enabled AS LOGICAL NO-UNDO.

  /* LOCK status on the find depends on FIELDS-ENABLED. */
  RUN get-attribute ('FIELDS-ENABLED':U).
  row-avail-enabled = (RETURN-VALUE eq 'yes':U).
  /* Look up the current key-value. */
  RUN get-attribute ('Key-Value':U).
  key-value = RETURN-VALUE.

  /* Find the current record using the current Key-Name. */
  RUN get-attribute ('Key-Name':U).
  CASE RETURN-VALUE:
    WHEN 'ch-acesso-comp-nfe':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = nfe013
           &WHERE = "WHERE nfe013.ch-acesso-comp-nfe eq key-value"
       }
  END CASE.

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
  {src/adm/template/row-list.i "nfe013"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "nfe013"}

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
    
    /* Ponha na pi-validate todas as valida‡äes */
    /* NÆo gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /* Todos os assignïs nÆo feitos pelo assign-record devem ser feitos aqui */  
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

find item
    where item.it-codigo = nfe013.it-codigo:screen-value in frame {&frame-name} no-lock no-error.
if avail item then
    assign c-desc-item:screen-value in frame {&frame-name} = item.desc-item.

find natur-oper
    where natur-oper.nat-operacao = nfe013.nat-operacao:screen-value in frame {&frame-name} no-lock no-error.
if avail natur-oper then
    assign c-desc-nat-oper:screen-value in frame {&frame-name} = natur-oper.denominacao.

find bf-nfe013
    where rowid(bf-nfe013) = r-rowid-linha no-lock no-error.

find nfe013
    where nfe013.ch-acesso-comp-nfe = bf-nfe013.ch-acesso-comp-nfe
      and nfe013.seq-item = bf-nfe013.seq-item
      and nfe013.idi-orig-trad = 1 no-lock no-error.

if avail nfe013 then
    case nfe013.idi-orig-trad:
        when 1 then do:
            assign c-seq-item:screen-value in frame {&frame-name}    = string(nfe013.seq-item)
                   c-it-codigo:screen-value in frame {&frame-name}   = nfe013.it-codigo
                   c-cfop:screen-value in frame {&frame-name}        = nfe013.cod-cfop
                   c-ped-compra:screen-value in frame {&frame-name}  = string(nfe013.num-pedido)
                   c-ord-compra:screen-value in frame {&frame-name}  = string(nfe013.numero-ordem)
                   c-qtd-emit:screen-value in frame {&frame-name}    = string(nfe013.qtd-comercial)
                   c-un-emit:screen-value in frame {&frame-name}     = nfe013.un-comercial
                   c-preco-unit:screen-value in frame {&frame-name}  = string(nfe013.preco-unit)
                   c-preco-total:screen-value in frame {&frame-name} = string(nfe013.preco-total).

        end.
    end case.

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
  Notes: NÆo fazer assign aqui. Nesta procedure
  devem ser colocadas apenas valida‡äes, pois neste ponto do programa o registro 
  ainda nÆo foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /* Valida‡Æo de dicion rio */
    
/*/*    Segue um exemplo de valida‡Æo de programa */
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
  {src/adm/template/sndkycas.i "ch-acesso-comp-nfe" "nfe013" "ch-acesso-comp-nfe"}

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
  {src/adm/template/snd-list.i "nfe013"}

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

