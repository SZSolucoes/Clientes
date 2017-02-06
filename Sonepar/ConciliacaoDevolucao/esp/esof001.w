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
{include/i-prgvrs.i ESOF001 2.12.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE tt-doc-fiscal NO-UNDO LIKE doc-fiscal
    FIELD numero-dt-nota AS CHAR FORMAT "x(400)"
    FIELD valor-tot-doc-fiscal LIKE doc-fiscal.vl-mercad
    FIELD valor-tot-integrado LIKE doc-fiscal.vl-mercad
    FIELD valor-tot-diferenca AS DEC.

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

/* Parameters Definitions ---                                           */
DEFINE VARIABLE p-cod-estabel-ini   LIKE doc-fiscal.cod-estabel     NO-UNDO. 
DEFINE VARIABLE p-cod-estabel-fim   LIKE doc-fiscal.cod-estabel     NO-UNDO.
DEFINE VARIABLE p-serie-ini         LIKE doc-fiscal.serie           NO-UNDO.
DEFINE VARIABLE p-serie-fim         LIKE doc-fiscal.serie           NO-UNDO.
DEFINE VARIABLE p-nr-doc-fis-ini    LIKE doc-fiscal.nr-doc-fis      NO-UNDO.
DEFINE VARIABLE p-nr-doc-fis-fim    LIKE doc-fiscal.nr-doc-fis      NO-UNDO.
DEFINE VARIABLE p-cod-emitente-ini  LIKE doc-fiscal.cod-emitente    NO-UNDO.
DEFINE VARIABLE p-cod-emitente-fim  LIKE doc-fiscal.cod-emitente    NO-UNDO.
DEFINE VARIABLE p-nat-operacao-ini  LIKE doc-fiscal.nat-operacao    NO-UNDO. 
DEFINE VARIABLE p-nat-operacao-fim  LIKE doc-fiscal.nat-operacao    NO-UNDO.
DEFINE VARIABLE p-dt-transacao-ini  LIKE doc-fiscal.dt-impl         NO-UNDO. 
DEFINE VARIABLE p-dt-transacao-fim  LIKE doc-fiscal.dt-impl         NO-UNDO.

DEFINE VARIABLE tot AS DECIMAL     NO-UNDO.
                                                                          
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE c-tipo-nat              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE l-mostra-dif            AS LOGICAL NO-UNDO.
DEFINE VARIABLE da-movto                LIKE docum-est.dt-emissao NO-UNDO.

DEFINE NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR FORMAT "x(12)" NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME brTable1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-doc-fiscal

/* Definitions for BROWSE brTable1                                      */
&Scoped-define FIELDS-IN-QUERY-brTable1 tt-doc-fiscal.cod-estabel tt-doc-fiscal.dt-docto tt-doc-fiscal.nome-ab-emi tt-doc-fiscal.cod-emitente tt-doc-fiscal.nr-doc-fis tt-doc-fiscal.serie tt-doc-fiscal.nat-operacao tt-doc-fiscal.esp-docto fn-tipo-nat () @ c-tipo-nat tt-doc-fiscal.numero-dt-nota tt-doc-fiscal.valor-tot-doc-fiscal tt-doc-fiscal.valor-tot-integrado tt-doc-fiscal.valor-tot-diferenca   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brTable1   
&Scoped-define SELF-NAME brTable1
&Scoped-define QUERY-STRING-brTable1 FOR EACH tt-doc-fiscal NO-LOCK     WHERE IF l-mostra-dif THEN tt-doc-fiscal.valor-tot-diferenca > 0 ELSE YES                               BY tt-doc-fiscal.cod-estabel                                    BY tt-doc-fiscal.dt-emis-doc        INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brTable1 OPEN QUERY {&SELF-NAME} FOR EACH tt-doc-fiscal NO-LOCK     WHERE IF l-mostra-dif THEN tt-doc-fiscal.valor-tot-diferenca > 0 ELSE YES                               BY tt-doc-fiscal.cod-estabel                                    BY tt-doc-fiscal.dt-emis-doc        INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brTable1 tt-doc-fiscal
&Scoped-define FIRST-TABLE-IN-QUERY-brTable1 tt-doc-fiscal


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-brTable1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btDif-2 rt-button brTable1 btAtualiza ~
btExcel btSelecao btTotal 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-tipo-nat w-livre 
FUNCTION fn-tipo-nat RETURNS CHARACTER
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
     SIZE 5 BY 1.25 TOOLTIP "Atualiza Documentos".

DEFINE BUTTON btDif-2 
     IMAGE-UP FILE "image/toolbar/im-npula.bmp":U
     IMAGE-INSENSITIVE FILE "image/im-npula.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Somente diferenáas".

DEFINE BUTTON btExcel 
     IMAGE-UP FILE "image/toolbar/mip-csv.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-excel.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Gera Planilha".

DEFINE BUTTON btSelecao 
     IMAGE-UP FILE "image/toolbar/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-ran.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Seleá∆o" 
     SIZE 5 BY 1.25 TOOLTIP "Seleá∆o Documentos".

DEFINE BUTTON btTotal 
     IMAGE-UP FILE "image/toolbar/im-total.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-total.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Totais".

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 160 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brTable1 FOR 
      tt-doc-fiscal SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brTable1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brTable1 w-livre _FREEFORM
  QUERY brTable1 NO-LOCK DISPLAY
      tt-doc-fiscal.cod-estabel  WIDTH 5
      tt-doc-fiscal.dt-docto FORMAT "99/99/9999":U WIDTH 10
      tt-doc-fiscal.nome-ab-emi WIDTH 12
      tt-doc-fiscal.cod-emitente  WIDTH 5
      tt-doc-fiscal.nr-doc-fis WIDTH 10
      tt-doc-fiscal.serie WIDTH 5
      tt-doc-fiscal.nat-operacao WIDTH 8
      tt-doc-fiscal.esp-docto WIDTH 5
      fn-tipo-nat () @ c-tipo-nat FORMAT "x(30)":U WIDTH 11 COLUMN-LABEL "Tipo Natureza"
      tt-doc-fiscal.numero-dt-nota COLUMN-LABEL "Nota Fiscal/Data Emiss∆o" WIDTH 30
      tt-doc-fiscal.valor-tot-doc-fiscal COLUMN-LABEL "Valor Tot. Docto" WIDTH 12
      tt-doc-fiscal.valor-tot-integrado COLUMN-LABEL "Tot. Integ. Di†rio" WIDTH 12
      tt-doc-fiscal.valor-tot-diferenca COLUMN-LABEL "Dif. Devol x Di†rio" WIDTH 13
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SEPARATORS SIZE 158 BY 23.42
         FONT 1
         TITLE "Conciliaá∆o Devoluá∆o".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     btDif-2 AT ROW 1.13 COL 28.86 WIDGET-ID 20
     brTable1 AT ROW 2.58 COL 2 WIDGET-ID 200
     btAtualiza AT ROW 1.17 COL 7 WIDGET-ID 8
     btExcel AT ROW 1.13 COL 34 WIDGET-ID 22
     btSelecao AT ROW 1.13 COL 1.72 WIDGET-ID 4
     btTotal AT ROW 1.13 COL 23.72 WIDGET-ID 18
     rt-button AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 185.14 BY 26.54 WIDGET-ID 100.


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
         HEIGHT             = 25.08
         WIDTH              = 159.29
         MAX-HEIGHT         = 27.96
         MAX-WIDTH          = 195.14
         VIRTUAL-HEIGHT     = 27.96
         VIRTUAL-WIDTH      = 195.14
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
/* BROWSE-TAB brTable1 rt-button f-cad */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brTable1
/* Query rebuild information for BROWSE brTable1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-doc-fiscal NO-LOCK
    WHERE IF l-mostra-dif THEN tt-doc-fiscal.valor-tot-diferenca > 0 ELSE YES
                              BY tt-doc-fiscal.cod-estabel
                                   BY tt-doc-fiscal.dt-emis-doc
       INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Temp-Tables.tt-nfe003.cod-estabel|yes,Temp-Tables.tt-nfe003.cod-emitente|yes,Temp-Tables.tt-nfe003.serie-docto|yes,Temp-Tables.tt-nfe003.nro-docto|yes"
     _Query            is OPENED
*/  /* BROWSE brTable1 */
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


&Scoped-define BROWSE-NAME brTable1
&Scoped-define SELF-NAME brTable1
&Scoped-define SELF-NAME btAtualiza
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAtualiza w-livre
ON CHOOSE OF btAtualiza IN FRAME f-cad
DO: 

    RUN carregaBrowser.

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

    {&open-query-brTable1}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExcel w-livre
ON CHOOSE OF btExcel IN FRAME f-cad
DO: 
    RUN utp/ut-msgs.p ("show",
                       27100 ,
                       "Gerar Relat¢rio Excel?~~" +
                       "Ser† Gerado um Relat¢rio em Excel com as Informaá‰es do Browser, confirma?").
    IF RETURN-VALUE = "no" THEN
        RETURN NO-APPLY. 

    ELSE DO:
        RUN esp/esof001crp.p (INPUT TABLE tt-doc-fiscal). 
    END.

      

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSelecao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSelecao w-livre
ON CHOOSE OF btSelecao IN FRAME f-cad /* Seleá∆o */
DO:
   {&WINDOW-NAME}:SENSITIVE = FALSE.

    RUN esp/esof001a.w (INPUT-OUTPUT p-cod-estabel-ini,
                        INPUT-OUTPUT p-cod-estabel-fim,
                        INPUT-OUTPUT p-serie-ini,
                        INPUT-OUTPUT p-serie-fim,
                        INPUT-OUTPUT p-nr-doc-fis-ini,
                        INPUT-OUTPUT p-nr-doc-fis-fim,
                        INPUT-OUTPUT p-cod-emitente-ini, 
                        INPUT-OUTPUT p-cod-emitente-fim,
                        INPUT-OUTPUT p-nat-operacao-ini, 
                        INPUT-OUTPUT p-nat-operacao-fim,
                        INPUT-OUTPUT p-dt-transacao-ini,   
                        INPUT-OUTPUT p-dt-transacao-fim).

    {&WINDOW-NAME}:SENSITIVE = TRUE.

    RUN carregaBrowser.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btTotal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btTotal w-livre
ON CHOOSE OF btTotal IN FRAME f-cad
DO: 
    {&WINDOW-NAME}:SENSITIVE = FALSE.    
    RUN esp/esof001b.w (INPUT TABLE tt-doc-fiscal).
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
       RUN set-position IN h_p-exihel ( 1.17 , 143.43 ) NO-ERROR.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carregaBrowser w-livre 
PROCEDURE carregaBrowser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FOR EACH tt-doc-fiscal:
        DELETE tt-doc-fiscal.
    END. 

    FOR EACH doc-fiscal
        WHERE doc-fiscal.cod-estabel        >= p-cod-estabel-ini  
          AND doc-fiscal.cod-estabel        <= p-cod-estabel-fim  
          AND doc-fiscal.serie              >= p-serie-ini         
          AND doc-fiscal.serie              <= p-serie-fim         
          AND doc-fiscal.nr-doc-fis         >= p-nr-doc-fis-ini    
          AND doc-fiscal.nr-doc-fis         <= p-nr-doc-fis-fim    
          AND doc-fiscal.cod-emitente       >= p-cod-emitente-ini  
          AND doc-fiscal.cod-emitente       <= p-cod-emitente-fim  
          AND doc-fiscal.nat-operacao       >= p-nat-operacao-ini 
          AND doc-fiscal.nat-operacao       <= p-nat-operacao-fim 
          AND doc-fiscal.dt-docto           >= p-dt-transacao-ini  
          AND doc-fiscal.dt-docto           <= p-dt-transacao-fim
          AND doc-fiscal.esp-docto           = "NFD"   NO-LOCK:

        IF (doc-fiscal.nat-operacao    = "5209"
            OR doc-fiscal.nat-operacao = "6209"
            OR doc-fiscal.nat-operacao = "1209"
            OR doc-fiscal.nat-operacao = "2209" ) THEN
            NEXT.
        
        CREATE tt-doc-fiscal.
        BUFFER-COPY doc-fiscal TO tt-doc-fiscal.

    END.
  
    RUN verifica-nota-devolucao.
    
    RUN verifica-diario-estoque.
    
    {&open-query-brTable1}

    APPLY 'MOUSE-SELECT-CLICK' TO brTable1 IN FRAME f-cad.
    
    RETURN "OK":U.
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
  ENABLE btDif-2 rt-button brTable1 btAtualiza btExcel btSelecao btTotal 
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
  run pi-before-initialize.

  {include/win-size.i}

  {utp/ut9000.i "ESOF001" "2.12.00.001"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  ASSIGN p-cod-estabel-ini  = ""
         p-cod-estabel-fim  = "ZZZ"
         p-serie-ini        = ""
         p-serie-fim        = "ZZZZZ"
         p-nr-doc-fis-ini   = ""
         p-nr-doc-fis-fim   = "ZZZZZZZZZZZZZZZZ"
         p-cod-emitente-ini = 0
         p-cod-emitente-fim = 999999999
         p-nat-operacao-ini = ""
         p-nat-operacao-fim = "ZZZZZZ"
         p-dt-transacao-ini  = TODAY 
         p-dt-transacao-fim  = TODAY . 

  /* Code placed here will execute AFTER standard behavior.    */

  RUN pi-after-initialize.

  RUN carregaBrowser.

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
  {src/adm/template/snd-list.i "tt-doc-fiscal"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE verifica-diario-estoque w-livre 
PROCEDURE verifica-diario-estoque :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

EMPTY TEMP-TABLE tt-concilia-fiscal.

DO da-movto = p-dt-transacao-ini TO p-dt-transacao-fim:
    
    FOR EACH movto-estoq 
       WHERE movto-estoq.dt-trans      = da-movto
         AND movto-estoq.cod-emitente >= p-cod-emitente-ini
         AND movto-estoq.cod-emitente <= p-cod-emitente-fim
         AND movto-estoq.cod-estabel  >= p-cod-estabel-ini
         AND movto-estoq.cod-estabel  <= p-cod-estabel-fim
         AND movto-estoq.serie-docto  >= p-serie-ini
         AND movto-estoq.serie-docto  <= p-serie-fim
         AND movto-estoq.nro-docto    >= p-nr-doc-fis-ini
         AND movto-estoq.nro-docto    <= p-nr-doc-fis-fim NO-LOCK:  

        FIND FIRST tt-concilia-fiscal
             WHERE tt-concilia-fiscal.cod-emitente = movto-estoq.cod-emitente
               AND tt-concilia-fiscal.cod-estabel  = movto-estoq.cod-estabel
               AND tt-concilia-fiscal.serie-docto  = movto-estoq.serie-docto
               AND tt-concilia-fiscal.nro-docto    = movto-estoq.nro-docto
    /*           and tt-concilia-fiscal.nat-operacao = movto-estoq.nat-operacao*/ NO-ERROR.
        IF NOT AVAIL tt-concilia-fiscal THEN DO:
            CREATE tt-concilia-fiscal.
            BUFFER-COPY movto-estoq TO tt-concilia-fiscal.
        END.
      
        IF movto-estoq.tipo-trans = 1 THEN
            ASSIGN tt-concilia-fiscal.valor-ce-db = tt-concilia-fiscal.valor-ce-db
                                                    + (movto-estoq.valor-mat-m[1]
                                                    + movto-estoq.valor-ggf-m[1]
                                                    + movto-estoq.valor-mob-m[1]). 
        ELSE
            ASSIGN tt-concilia-fiscal.valor-ce-cr = tt-concilia-fiscal.valor-ce-cr + (movto-estoq.valor-ggf-m[1]
                                                                                   + movto-estoq.valor-mat-m[1]
                                                                                   + movto-estoq.valor-mob-m[1]
                                                                                   + movto-estoq.valor-icm
                                                                                   + movto-estoq.valor-ipi
                                                                                   + movto-estoq.valor-iss
                                                                                   + movto-estoq.valor-pis
                                                                                   + movto-estoq.val-cofins).
    END.
END.

FOR EACH tt-doc-fiscal :

    IF (tt-doc-fiscal.numero-dt-nota = ""
        OR tt-doc-fiscal.numero-dt-nota = ?) THEN
        DELETE tt-doc-fiscal.

    FOR EACH tt-concilia-fiscal
        WHERE tt-concilia-fiscal.cod-emitente = tt-doc-fiscal.cod-emitente
          AND tt-concilia-fiscal.cod-estabel  = tt-doc-fiscal.cod-estabel 
          AND tt-concilia-fiscal.serie-docto  = tt-doc-fiscal.serie 
          AND tt-concilia-fiscal.nro-docto    = tt-doc-fiscal.nr-doc-fis :

        IF tt-concilia-fiscal.valor-ce-db >= tt-concilia-fiscal.valor-ce-cr  THEN
            ASSIGN tt-doc-fiscal.valor-tot-integrado = tt-concilia-fiscal.valor-ce-db - tt-concilia-fiscal.valor-ce-cr.
        ELSE
            ASSIGN tt-doc-fiscal.valor-tot-integrado = tt-concilia-fiscal.valor-ce-cr - tt-concilia-fiscal.valor-ce-db .
    
        ASSIGN tt-doc-fiscal.valor-tot-diferenca = tt-doc-fiscal.valor-tot-doc-fiscal - tt-doc-fiscal.valor-tot-integrado .
                  
    END.

END.

{&open-query-brTable1}


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE verifica-nota-devolucao w-livre 
PROCEDURE verifica-nota-devolucao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FOR EACH tt-doc-fiscal:
        
        FOR EACH nota-fisc-adc NO-LOCK
            WHERE nota-fisc-adc.cod-estab        = tt-doc-fiscal.cod-estabel
              AND nota-fisc-adc.cod-serie        = tt-doc-fiscal.serie
              AND nota-fisc-adc.cod-nota-fisc    = tt-doc-fiscal.nr-doc-fis
              AND nota-fisc-adc.cdn-emitente     = tt-doc-fiscal.cod-emitente
              AND nota-fisc-adc.cod-natur-operac = tt-doc-fiscal.nat-operacao:

            ASSIGN tt-doc-fiscal.numero-dt-nota = tt-doc-fiscal.numero-dt-nota + STRING(nota-fisc-adc.cod-docto-referado) + STRING("-") + STRING(nota-fisc-adc.dat-docto-referado,"99/99/9999") + STRING("/") + " ".
            
        END.
        
        FOR EACH it-doc-fisc NO-LOCK OF tt-doc-fiscal:
                
             ASSIGN tt-doc-fiscal.valor-tot-doc-fiscal = tt-doc-fiscal.valor-tot-doc-fiscal + it-doc-fisc.vl-tot-item.
                
        END.
    
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-tipo-nat w-livre 
FUNCTION fn-tipo-nat RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  ASSIGN c-tipo-nat = "".

  ASSIGN c-tipo-nat   = {diinc/i07di037.i 04 tt-doc-fiscal.ind-ori-doc}.

  RETURN c-tipo-nat.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

