&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESOF002 2.12.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE tt-concilia-fiscal NO-UNDO 
    field cod-emitente like movto-estoq.cod-emitente
    field cod-estabel  like movto-estoq.cod-estabel
    field serie-docto  like movto-estoq.serie-docto
    field nro-docto    like movto-estoq.nro-docto
    field nat-operacao like movto-estoq.nat-operacao 
    field dt-trans     like movto-estoq.dt-trans
    field valor-ce-db as dec
    field valor-ce-cr as dec
    field valor-ft-cr as dec
    field valor-ft-db as dec
    field valor-of-cr as dec
    field valor-of-db as dec
    field valor-dif   as dec.

{cdp/cd0666.i} /* Temp-table de erros */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE c-nome-abrev AS CHARACTER FORMAT "(12)"  NO-UNDO.
DEFINE VARIABLE l-mostra-dif AS LOGICAL NO-UNDO.

DEFINE VARIABLE p-cod-emitente-ini      LIKE emitente.cod-emitente INITIAL 0                    NO-UNDO.
DEFINE VARIABLE p-cod-emitente-fim      LIKE emitente.cod-emitente INITIAL 999999999            NO-UNDO.
DEFINE VARIABLE p-cod-estabel-ini       LIKE docum-est.cod-estabel INITIAL ""                   NO-UNDO.
DEFINE VARIABLE p-cod-estabel-fim       LIKE docum-est.cod-estabel INITIAL "ZZZ":U              NO-UNDO.
DEFINE VARIABLE p-serie-ini             LIKE docum-est.serie-docto INITIAL ""                   NO-UNDO.
DEFINE VARIABLE p-serie-fim             LIKE docum-est.serie-docto INITIAL "ZZZZZZ":U           NO-UNDO.
DEFINE VARIABLE p-nro-docto-ini         LIKE docum-est.nro-docto   INITIAL ""                   NO-UNDO.
DEFINE VARIABLE p-nro-docto-fim         LIKE docum-est.nro-docto   INITIAL "ZZZZZZZZZZZZZZZZ":U NO-UNDO.
define VARIABLE p-dt-transacao-ini      LIKE docum-est.dt-trans    INITIAL 01/01/1800           NO-UNDO.
define VARIABLE p-dt-transacao-fim      LIKE docum-est.dt-trans    INITIAL 12/31/9999           NO-UNDO.
DEFINE VARIABLE pl-ok                   AS LOGICAL     NO-UNDO.
DEFINE VARIABLE da-movto                LIKE docum-est.dt-emissao NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-concilia

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-concilia-fiscal

/* Definitions for BROWSE br-concilia                                   */
&Scoped-define FIELDS-IN-QUERY-br-concilia tt-concilia-fiscal.cod-estabel tt-concilia-fiscal.cod-emitente fn-nome-abrev() @ c-nome-abrev tt-concilia-fiscal.nro-docto tt-concilia-fiscal.serie tt-concilia-fiscal.nat-operacao tt-concilia-fiscal.dt-trans tt-concilia-fiscal.valor-ce-db tt-concilia-fiscal.valor-ce-cr tt-concilia-fiscal.valor-ft-db tt-concilia-fiscal.valor-ft-cr tt-concilia-fiscal.valor-of-db tt-concilia-fiscal.valor-of-cr tt-concilia-fiscal.valor-dif   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-concilia   
&Scoped-define SELF-NAME br-concilia
&Scoped-define QUERY-STRING-br-concilia FOR EACH tt-concilia-fiscal where if l-mostra-dif then tt-concilia-fiscal.valor-dif > 0 else yes NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-concilia OPEN QUERY {&SELF-NAME} FOR EACH tt-concilia-fiscal where if l-mostra-dif then tt-concilia-fiscal.valor-dif > 0 else yes NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-concilia tt-concilia-fiscal
&Scoped-define FIRST-TABLE-IN-QUERY-br-concilia tt-concilia-fiscal


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-br-concilia}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btDif-2 rt-button btExcel br-concilia ~
btTotal btAtualiza btSelecao 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-nome-abrev w-livre 
FUNCTION fn-nome-abrev RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-livre AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-programa 
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat¢rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU m_Ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-livre MENUBAR
       SUB-MENU  mi-programa    LABEL "&Nome-do-Programa"
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btAtualiza 
     IMAGE-UP FILE "image/toolbar/im-relo.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-relo.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Atualiza Dados".

DEFINE BUTTON btDif-2 
     IMAGE-UP FILE "image/toolbar/im-npula.bmp":U
     IMAGE-INSENSITIVE FILE "image/im-npula.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Somente diferen‡as".

DEFINE BUTTON btExcel 
     IMAGE-UP FILE "image/toolbar/mip-csv.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-excel.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Gera Planilha".

DEFINE BUTTON btSelecao 
     IMAGE-UP FILE "image/toolbar/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-ran.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Sele‡Æo" 
     SIZE 5 BY 1.25 TOOLTIP "Sele‡Æo Documentos".

DEFINE BUTTON btTotal 
     IMAGE-UP FILE "image/toolbar/im-total.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-total.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Totais".

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 165 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-concilia FOR 
      tt-concilia-fiscal SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-concilia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-concilia w-livre _FREEFORM
  QUERY br-concilia NO-LOCK DISPLAY
      tt-concilia-fiscal.cod-estabel FORMAT "x(5)":U COLUMN-LABEL "Estab"
      tt-concilia-fiscal.cod-emitente FORMAT ">>>>>>>>9":U
      fn-nome-abrev() @ c-nome-abrev  FORMAT "x(12)":U COLUMN-LABEL "Cliente" WIDTH 15
      tt-concilia-fiscal.nro-docto FORMAT "x(16)":U
      tt-concilia-fiscal.serie FORMAT "x(5)":U
      tt-concilia-fiscal.nat-operacao 
      tt-concilia-fiscal.dt-trans FORMAT "99/99/9999":U
      tt-concilia-fiscal.valor-ce-db FORMAT ">>>,>>>,>>>,>>>,>>9.99":U COLUMN-LABEL "DB Estoque"   
      tt-concilia-fiscal.valor-ce-cr FORMAT ">>>,>>>,>>>,>>>,>>9.99":U COLUMN-LABEL "CR Estoque"
      tt-concilia-fiscal.valor-ft-db FORMAT ">>>,>>>,>>>,>>>,>>9.99":U COLUMN-LABEL "DB Faturamento"        
      tt-concilia-fiscal.valor-ft-cr FORMAT ">>>,>>>,>>>,>>>,>>9.99":U COLUMN-LABEL "CR Faturamento"
      tt-concilia-fiscal.valor-of-db FORMAT ">>>,>>>,>>>,>>>,>>9.99":U COLUMN-LABEL "Entradas OF"
      tt-concilia-fiscal.valor-of-cr FORMAT ">>>,>>>,>>>,>>>,>>9.99":U COLUMN-LABEL "Sa¡das OF"   
      tt-concilia-fiscal.valor-dif   FORMAT ">>>,>>>,>>>,>>>,>>9.99":U COLUMN-LABEL "Diferen‡a Di rio x OF"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 164 BY 26.25
         TITLE "Concilia‡Æo de Impostos" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     btDif-2 AT ROW 1.13 COL 28.86 WIDGET-ID 20
     btExcel AT ROW 1.13 COL 34 WIDGET-ID 16
     br-concilia AT ROW 3 COL 2 WIDGET-ID 200
     btTotal AT ROW 1.13 COL 23.72 WIDGET-ID 18
     btAtualiza AT ROW 1.13 COL 7.29 WIDGET-ID 8
     btSelecao AT ROW 1.13 COL 2.29 WIDGET-ID 4
     rt-button AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 214.57 BY 28.71 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-livre
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-livre ASSIGN
         HIDDEN             = YES
         TITLE              = "Template Livre <Insira complemento>"
         HEIGHT             = 28.5
         WIDTH              = 165.43
         MAX-HEIGHT         = 28.88
         MAX-WIDTH          = 215
         VIRTUAL-HEIGHT     = 28.88
         VIRTUAL-WIDTH      = 215
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-livre:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-livre 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-livre.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-livre
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
/* BROWSE-TAB br-concilia btExcel f-cad */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-concilia
/* Query rebuild information for BROWSE br-concilia
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-concilia-fiscal
where if l-mostra-dif then tt-concilia-fiscal.valor-dif > 0 else yes NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-concilia */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Template Livre <Insira complemento> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Template Livre <Insira complemento> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAtualiza
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAtualiza w-livre
ON CHOOSE OF btAtualiza IN FRAME f-cad
DO: 

    APPLY 'value-changed':U TO br-concilia IN FRAME {&FRAME-NAME}.

    RUN piLeituraDados.    

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDif-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDif-2 w-livre
ON CHOOSE OF btDif-2 IN FRAME f-cad
DO: 
    if l-mostra-dif then
        assign l-mostra-dif = no.    
    else
        assign l-mostra-dif = yes.

    {&open-query-br-concilia}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExcel w-livre
ON CHOOSE OF btExcel IN FRAME f-cad
DO: 

    RUN esp/esof002crp.p (INPUT TABLE tt-concilia-fiscal).   

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSelecao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSelecao w-livre
ON CHOOSE OF btSelecao IN FRAME f-cad /* Sele‡Æo */
DO:
        {&WINDOW-NAME}:SENSITIVE = FALSE.    
        RUN esp/esof002a.w (INPUT        1, /* NF-e */
                           INPUT-OUTPUT p-cod-emitente-ini,
                           INPUT-OUTPUT p-cod-emitente-fim,
                           INPUT-OUTPUT p-cod-estabel-ini,
                           INPUT-OUTPUT p-cod-estabel-fim,
                           INPUT-OUTPUT p-serie-ini,
                           INPUT-OUTPUT p-serie-fim,
                           INPUT-OUTPUT p-nro-docto-ini,
                           INPUT-OUTPUT p-nro-docto-fim,
                           INPUT-OUTPUT p-dt-transacao-ini,
                           INPUT-OUTPUT p-dt-transacao-fim,
                           INPUT-OUTPUT pl-ok).
        {&WINDOW-NAME}:SENSITIVE = TRUE.
    
        IF pl-ok THEN 
            RUN piLeituraDados.    
            
        APPLY 'value-changed':U TO br-concilia IN FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btTotal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btTotal w-livre
ON CHOOSE OF btTotal IN FRAME f-cad
DO: 
    {&WINDOW-NAME}:SENSITIVE = FALSE.    
    RUN esp/esof002b.w (INPUT TABLE tt-concilia-fiscal).
    {&WINDOW-NAME}:SENSITIVE = TRUE.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-livre
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-livre
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-livre
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-programa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-programa w-livre
ON MENU-DROP OF MENU mi-programa /* Nome-do-Programa */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-livre
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-livre
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-concilia
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-livre  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.17 , 149.43 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-livre  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-livre  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
  THEN DELETE WIDGET w-livre.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-livre  _DEFAULT-ENABLE
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
  ENABLE btDif-2 rt-button btExcel br-concilia btTotal btAtualiza btSelecao 
      WITH FRAME f-cad IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-livre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-livre 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-livre 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-livre 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN pi-before-initialize.

  {include/win-size.i}

  {utp/ut9000.i "ESOF002" "2.12.00.001"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  ASSIGN p-dt-transacao-ini = TODAY
         p-dt-transacao-fim = TODAY.

  RUN piLeituraDados.

  APPLY 'value-changed':U TO br-concilia IN FRAME {&FRAME-NAME}.

  /* Code placed here will execute AFTER standard behavior.    */

  RUN pi-after-initialize.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piLeituraDados w-livre 
PROCEDURE piLeituraDados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
EMPTY TEMP-TABLE tt-concilia-fiscal.

for each estabelec no-lock
   where estabelec.cod-estab >= p-cod-estabel-ini
     and estabelec.cod-estab <= p-cod-estabel-fim:
    for each sumar-ft no-lock
       where sumar-ft.cod-estabel   = estabelec.cod-estab
         AND sumar-ft.serie        >= p-serie-ini
         AND sumar-ft.serie        <= p-serie-fim
         AND sumar-ft.nr-nota-fis  >= p-nro-docto-ini
         AND sumar-ft.nr-nota-fis  <= p-nro-docto-fim
         AND sumar-ft.dt-movto     >= p-dt-transacao-ini
         AND sumar-ft.dt-movto     <= p-dt-transacao-fim ,
       first nota-fiscal no-lock
       where nota-fiscal.cod-estabel = sumar-ft.cod-estabel
         and nota-fiscal.serie       = sumar-ft.serie
         and nota-fiscal.nr-nota-fis = sumar-ft.nr-nota-fis:  
                          
        if sumar-ft.ct-conta <> "11305010018" and /* PIS */  
           sumar-ft.ct-conta <> "11305010019" and /* COFINS */
           sumar-ft.ct-conta <> "11305050003" and /* ICMS */
           sumar-ft.ct-conta <> "11305010020" and /* ICMS ST */
           sumar-ft.ct-conta <> "11305050007" and /* IPI */
           sumar-ft.ct-conta <> "11305010003" /* ISS */ then next.
         
        find first tt-concilia-fiscal
             where tt-concilia-fiscal.cod-emitente = nota-fiscal.cod-emitente
               and tt-concilia-fiscal.cod-estabel  = sumar-ft.cod-estabel
               and tt-concilia-fiscal.serie-docto  = sumar-ft.serie
               and tt-concilia-fiscal.nro-docto    = sumar-ft.nr-nota-fis
    /*           and tt-concilia-fiscal.nat-operacao = nota-fiscal.nat-operacao*/ no-error.
        if not avail tt-concilia-fiscal then do:
            CREATE tt-concilia-fiscal.
            BUFFER-COPY sumar-ft TO tt-concilia-fiscal.
            assign tt-concilia-fiscal.cod-emitente = nota-fiscal.cod-emitente
                   tt-concilia-fiscal.nat-operacao = nota-fiscal.nat-operacao
                   tt-concilia-fiscal.nro-docto    = sumar-ft.nr-nota-fis
                   tt-concilia-fiscal.serie-docto  = sumar-ft.serie
                   tt-concilia-fiscal.dt-trans     = sumar-ft.dt-movto.
        end.
        
        if sumar-ft.vl-contab > 0 then
            assign tt-concilia-fiscal.valor-ft-cr = tt-concilia-fiscal.valor-ft-cr + sumar-ft.vl-contab.
        else  
            assign tt-concilia-fiscal.valor-ft-db = tt-concilia-fiscal.valor-ft-db + sumar-ft.vl-contab.              
    END.
end.

do da-movto = p-dt-transacao-ini to p-dt-transacao-fim:
    FOR EACH movto-estoq 
       WHERE movto-estoq.dt-trans      = da-movto
         AND movto-estoq.cod-emitente >= p-cod-emitente-ini
         AND movto-estoq.cod-emitente <= p-cod-emitente-fim
         AND movto-estoq.cod-estabel  >= p-cod-estabel-ini
         AND movto-estoq.cod-estabel  <= p-cod-estabel-fim
         AND movto-estoq.serie-docto  >= p-serie-ini
         AND movto-estoq.serie-docto  <= p-serie-fim
         AND movto-estoq.nro-docto    >= p-nro-docto-ini
         AND movto-estoq.nro-docto    <= p-nro-docto-fim NO-LOCK:        
                 
        find first tt-concilia-fiscal
             where tt-concilia-fiscal.cod-emitente = movto-estoq.cod-emitente
               and tt-concilia-fiscal.cod-estabel  = movto-estoq.cod-estabel
               and tt-concilia-fiscal.serie-docto  = movto-estoq.serie-docto
               and tt-concilia-fiscal.nro-docto    = movto-estoq.nro-docto
    /*           and tt-concilia-fiscal.nat-operacao = movto-estoq.nat-operacao*/ no-error.
        if not avail tt-concilia-fiscal then do:
            CREATE tt-concilia-fiscal.
            BUFFER-COPY movto-estoq TO tt-concilia-fiscal.
        end.
        
        if  movto-estoq.tipo-trans = 1 then
            assign tt-concilia-fiscal.valor-ce-db = tt-concilia-fiscal.valor-ce-db
                                  + movto-estoq.valor-icm
                                  + movto-estoq.valor-ipi
                                  + movto-estoq.valor-iss
                                  + movto-estoq.valor-pis
                                  + movto-estoq.val-cofins. 
        else
            assign tt-concilia-fiscal.valor-ce-cr = tt-concilia-fiscal.valor-ce-cr                                   
                                  + movto-estoq.valor-icm
                                  + movto-estoq.valor-ipi
                                  + movto-estoq.valor-iss                              
                                  + movto-estoq.valor-pis
                                  + movto-estoq.val-cofins. 
    end.

    /*Entradas*/
    for each doc-fiscal no-lock
       where doc-fiscal.dt-docto      = da-movto
         AND doc-fiscal.cod-estabel  >= p-cod-estabel-ini
         AND doc-fiscal.cod-estabel  <= p-cod-estabel-fim
         AND doc-fiscal.serie        >= p-serie-ini
         AND doc-fiscal.serie        <= p-serie-fim
         AND doc-fiscal.nr-doc-fis   >= p-nro-docto-ini
         AND doc-fiscal.nr-doc-fis   <= p-nro-docto-fim
         AND (doc-fiscal.tipo-nat   = 1 OR 
             (doc-fiscal.tipo-nat   = 3 AND
              doc-fiscal.cod-cfop = "1933" OR
              doc-fiscal.cod-cfop = "2933"))         
         AND   doc-fiscal.ind-sit-doc = 1,
        EACH it-doc-fisc OF doc-fiscal:
        
        find first tt-concilia-fiscal
             where tt-concilia-fiscal.cod-emitente = doc-fiscal.cod-emitente
               and tt-concilia-fiscal.cod-estabel  = doc-fiscal.cod-estabel
               and tt-concilia-fiscal.serie-docto  = doc-fiscal.serie
               and tt-concilia-fiscal.nro-docto    = doc-fiscal.nr-doc-fis
    /*           and tt-concilia-fiscal.nat-operacao = doc-fiscal.nat-operacao*/ no-error.
        if not avail tt-concilia-fiscal then do:
            CREATE tt-concilia-fiscal.
            BUFFER-COPY doc-fiscal TO tt-concilia-fiscal.
            assign tt-concilia-fiscal.nro-docto    = doc-fiscal.nr-doc-fis
                   tt-concilia-fiscal.serie-docto  = doc-fiscal.serie
                   tt-concilia-fiscal.dt-trans     = doc-fiscal.dt-docto.
        end.
        
        assign tt-concilia-fiscal.valor-of-db = tt-concilia-fiscal.valor-of-db 
                                              + it-doc-fisc.val-pis
                                              + it-doc-fisc.val-cofins
                                              + it-doc-fisc.vl-iss-it 
                                              + it-doc-fisc.vl-icms-it 
                                              + it-doc-fisc.vl-ipi-it 
                                              + it-doc-fisc.vl-ipi-devol
                                              + it-doc-fisc.vl-icmsub-it.
    END.

    /*Sa¡das*/
    for each doc-fiscal no-lock
       where doc-fiscal.dt-docto      = da-movto
         AND doc-fiscal.cod-estabel  >= p-cod-estabel-ini
         AND doc-fiscal.cod-estabel  <= p-cod-estabel-fim
         AND doc-fiscal.serie        >= p-serie-ini
         AND doc-fiscal.serie        <= p-serie-fim
         AND doc-fiscal.nr-doc-fis  >= p-nro-docto-ini
         AND doc-fiscal.nr-doc-fis  <= p-nro-docto-fim
         AND ((doc-fiscal.tipo-nat    = 3 AND
               (doc-fiscal.cod-cfop = "5933" OR
                doc-fiscal.cod-cfop = "6933")) OR
           doc-fiscal.tipo-nat = 2 OR  
          (doc-fiscal.tipo-nat = 1 and doc-fiscal.ind-sit-doc = 2)),
        EACH it-doc-fisc OF doc-fiscal no-lock:
        
        find first tt-concilia-fiscal
             where tt-concilia-fiscal.cod-emitente = doc-fiscal.cod-emitente
               and tt-concilia-fiscal.cod-estabel  = doc-fiscal.cod-estabel
               and tt-concilia-fiscal.serie-docto  = doc-fiscal.serie
               and tt-concilia-fiscal.nro-docto    = doc-fiscal.nr-doc-fis
    /*           and tt-concilia-fiscal.nat-operacao = doc-fiscal.nat-operacao*/ no-error.
        if not avail tt-concilia-fiscal then do:
            CREATE tt-concilia-fiscal.
            BUFFER-COPY doc-fiscal TO tt-concilia-fiscal.
            assign tt-concilia-fiscal.cod-emitente = doc-fiscal.cod-emitente
                   tt-concilia-fiscal.nro-docto    = doc-fiscal.nr-doc-fis
                   tt-concilia-fiscal.serie-docto  = doc-fiscal.serie
                   tt-concilia-fiscal.dt-trans     = doc-fiscal.dt-docto.
        end.
        
        assign tt-concilia-fiscal.valor-of-cr = tt-concilia-fiscal.valor-of-cr
                                              + it-doc-fisc.val-pis
                                              + it-doc-fisc.val-cofins
                                              + it-doc-fisc.vl-iss-it 
                                              + it-doc-fisc.vl-icms-it 
                                              + it-doc-fisc.vl-ipi-it 
                                              + it-doc-fisc.vl-ipi-devol
                                              + it-doc-fisc.vl-icmsub-it.
    END.

END.    

for each tt-concilia-fiscal:

    assign tt-concilia-fiscal.valor-dif = ((tt-concilia-fiscal.valor-ft-cr - tt-concilia-fiscal.valor-ft-db) - tt-concilia-fiscal.valor-ce-db + tt-concilia-fiscal.valor-ce-cr).
    
    assign tt-concilia-fiscal.valor-dif = abs((tt-concilia-fiscal.valor-of-cr - tt-concilia-fiscal.valor-of-db) - tt-concilia-fiscal.valor-dif).    
end.

{&open-query-br-concilia}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-livre  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-concilia-fiscal"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-livre 
PROCEDURE state-changed :
/*:T -----------------------------------------------------------
  Purpose:     Manuseia trocas de estado dos SmartObjects
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-nome-abrev w-livre 
FUNCTION fn-nome-abrev RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND emitente
      WHERE emitente.cod-emitente = tt-concilia-fiscal.cod-emitente NO-LOCK NO-ERROR.
  IF AVAIL emitente THEN
      ASSIGN c-nome-abrev = emitente.nome-abrev.
  ELSE
      ASSIGN c-nome-abrev = "".


  RETURN c-nome-abrev.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

