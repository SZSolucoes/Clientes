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
{include/i-prgvrs.i ESOF003 2.12.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE tt-doc-fiscal NO-UNDO LIKE doc-fiscal
    FIELD vl-tot-trib AS DEC
    FIELD cod-cfop-xml  LIKE doc-fiscal.cod-cfop
    FIELD vl-cont-doc-xml LIKE doc-fiscal.vl-cont-doc
    FIELD vl-tot-trib-xml AS DEC
    FIELD valor-tot-doc-fiscal LIKE doc-fiscal.vl-mercad
    FIELD valor-tot-integrado LIKE doc-fiscal.vl-mercad
    FIELD valor-tot-diferenca AS DEC
    FIELD cfop-correta AS LOGICAL.

DEFINE TEMP-TABLE tt-concilia-fiscal NO-UNDO 
    FIELD cod-emitente like movto-estoq.cod-emitente
    FIELD cod-estabel  like movto-estoq.cod-estabel
    FIELD serie-docto  like movto-estoq.serie-docto
    FIELD nro-docto    like movto-estoq.nro-docto
    FIELD nat-operacao like movto-estoq.nat-operacao 
    FIELD dt-trans     like movto-estoq.dt-trans
    FIELD valor-ce-db  AS DEC
    FIELD valor-ce-cr  AS DEC
    FIELD valor-ft-cr  AS DEC
    FIELD valor-ft-db  AS DEC
    FIELD valor-of-cr  AS DEC
    FIELD valor-of-db  AS DEC
    FIELD valor-dif    AS DEC.

DEFINE NEW SHARED TEMP-TABLE tt-data NO-UNDO
    FIELD data AS DATE.

/* Parameters Definitions ---                                           */
DEFINE VARIABLE p-cod-estabel       LIKE doc-fiscal.cod-estabel     NO-UNDO.
DEFINE VARIABLE p-dt-transacao-ini  LIKE doc-fiscal.dt-impl         NO-UNDO. 
DEFINE VARIABLE p-dt-transacao-fim  LIKE doc-fiscal.dt-impl         NO-UNDO.
DEFINE VARIABLE p-serie-ini         LIKE doc-fiscal.serie           NO-UNDO.
DEFINE VARIABLE p-serie-fim         LIKE doc-fiscal.serie           NO-UNDO.
DEFINE VARIABLE p-nr-doc-fis-ini    LIKE doc-fiscal.nr-doc-fis      NO-UNDO.
DEFINE VARIABLE p-nr-doc-fis-fim    LIKE doc-fiscal.nr-doc-fis      NO-UNDO.
DEFINE VARIABLE p-cod-emitente-ini  LIKE doc-fiscal.cod-emitente    NO-UNDO.
DEFINE VARIABLE p-cod-emitente-fim  LIKE doc-fiscal.cod-emitente    NO-UNDO.
DEFINE VARIABLE p-nota-sem-xml      AS LOGICAL                      NO-UNDO.

DEFINE VARIABLE tot           AS DECIMAL  NO-UNDO.
DEFINE VARIABLE da-cont       AS DATE     NO-UNDO.
DEFINE VARIABLE da-est-ini    AS DATE     NO-UNDO.
DEFINE VARIABLE da-est-fim    AS DATE     NO-UNDO.
DEF NEW SHARED VAR l-consid-cfop-serv AS LOGICAL            NO-UNDO.
                                                                          
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE c-tipo-nat              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE l-mostra-dif            AS LOGICAL NO-UNDO.
DEFINE VARIABLE da-movto                LIKE docum-est.dt-emissao NO-UNDO. 
DEFINE VARIABLE c-nome-abrev            AS CHARACTER   NO-UNDO.

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
&Scoped-define FIELDS-IN-QUERY-brTable1 tt-doc-fiscal.cod-estabel tt-doc-fiscal.cod-emitente fn-nome-abrev() @ c-nome-abrev tt-doc-fiscal.cgc tt-doc-fiscal.ins-estadual tt-doc-fiscal.esp-docto tt-doc-fiscal.nr-doc-fis tt-doc-fiscal.serie tt-doc-fiscal.dt-emis-doc tt-doc-fiscal.cod-cfop tt-doc-fiscal.cod-cfop-xml tt-doc-fiscal.vl-cont-doc tt-doc-fiscal.vl-cont-doc-xml tt-doc-fiscal.vl-tot-trib tt-doc-fiscal.vl-tot-trib-xml   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brTable1   
&Scoped-define SELF-NAME brTable1
&Scoped-define QUERY-STRING-brTable1 FOR EACH tt-doc-fiscal NO-LOCK     /*WHERE IF l-mostra-dif THEN tt-doc-fiscal.valor-tot-diferenca > 0 ELSE YES*/                               BY tt-doc-fiscal.cod-estabel                                    BY tt-doc-fiscal.dt-emis-doc        INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brTable1 OPEN QUERY {&SELF-NAME} FOR EACH tt-doc-fiscal NO-LOCK     /*WHERE IF l-mostra-dif THEN tt-doc-fiscal.valor-tot-diferenca > 0 ELSE YES*/                               BY tt-doc-fiscal.cod-estabel                                    BY tt-doc-fiscal.dt-emis-doc        INDEXED-REPOSITION.
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
     SIZE 5 BY 1.25 TOOLTIP "Atualiza Documentos".

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
     SIZE 159 BY 1.46
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
      tt-doc-fiscal.cod-estabel       WIDTH 5
      tt-doc-fiscal.cod-emitente      FORMAT ">>>>>>>>9":U WIDTH 5
      fn-nome-abrev() @ c-nome-abrev  FORMAT "x(12)":U COLUMN-LABEL "Cliente" WIDTH 15
      tt-doc-fiscal.cgc               COLUMN-LABEL "CNPJ" WIDTH 15
      tt-doc-fiscal.ins-estadual      WIDTH 15
      tt-doc-fiscal.esp-docto         WIDTH 5
      tt-doc-fiscal.nr-doc-fis        WIDTH 10
      tt-doc-fiscal.serie             WIDTH 5
      tt-doc-fiscal.dt-emis-doc       FORMAT "99/99/9999":U WIDTH 10
      tt-doc-fiscal.cod-cfop          WIDTH 5
      tt-doc-fiscal.cod-cfop-xml      COLUMN-LABEL "CFOP XML" WIDTH 8
      tt-doc-fiscal.vl-cont-doc       WIDTH 12
      tt-doc-fiscal.vl-cont-doc-xml   COLUMN-LABEL "Vl Cont bil XML" WIDTH 12
      tt-doc-fiscal.vl-tot-trib       COLUMN-LABEL "Val.Trib." WIDTH 12
      tt-doc-fiscal.vl-tot-trib-xml   COLUMN-LABEL "Val.Trib.XML " WIDTH 12
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SEPARATORS SIZE 158 BY 23.42
         FONT 1
         TITLE "Conferˆncia Livro Fiscal de Entrada".


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
         SIZE 160.57 BY 25.38 WIDGET-ID 100.


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
         HEIGHT             = 25.13
         WIDTH              = 159.72
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
    /*WHERE IF l-mostra-dif THEN tt-doc-fiscal.valor-tot-diferenca > 0 ELSE YES*/
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brTable1 w-livre
ON ROW-DISPLAY OF brTable1 IN FRAME f-cad /* Conferˆncia Livro Fiscal de Entrada */
DO:
  
    IF AVAIL tt-doc-fiscal THEN DO:
        IF (tt-doc-fiscal.cfop-correta = NO OR
            tt-doc-fiscal.vl-cont-doc <> tt-doc-fiscal.vl-cont-doc-xml OR
            tt-doc-fiscal.vl-tot-trib <> tt-doc-fiscal.vl-tot-trib-xml) THEN DO:

            ASSIGN tt-doc-fiscal.cod-estabel      :FGCOLOR IN BROWSE brTable1 = 12
                   tt-doc-fiscal.cod-emitente     :FGCOLOR IN BROWSE brTable1 = 12           
                   c-nome-abrev                   :FGCOLOR IN BROWSE brTable1 = 12        
                   tt-doc-fiscal.cgc              :FGCOLOR IN BROWSE brTable1 = 12
                   tt-doc-fiscal.ins-estadual     :FGCOLOR IN BROWSE brTable1 = 12     
                   tt-doc-fiscal.esp-docto        :FGCOLOR IN BROWSE brTable1 = 12
                   tt-doc-fiscal.nr-doc-fis       :FGCOLOR IN BROWSE brTable1 = 12     
                   tt-doc-fiscal.serie            :FGCOLOR IN BROWSE brTable1 = 12  
                   tt-doc-fiscal.dt-emis-doc      :FGCOLOR IN BROWSE brTable1 = 12  
                   tt-doc-fiscal.cod-cfop         :FGCOLOR IN BROWSE brTable1 = 12  
                   tt-doc-fiscal.cod-cfop-xml     :FGCOLOR IN BROWSE brTable1 = 12 
                   tt-doc-fiscal.vl-cont-doc      :FGCOLOR IN BROWSE brTable1 = 12
                   tt-doc-fiscal.vl-cont-doc-xml  :FGCOLOR IN BROWSE brTable1 = 12 
                   tt-doc-fiscal.vl-tot-trib      :FGCOLOR IN BROWSE brTable1 = 12 
                   tt-doc-fiscal.vl-tot-trib-xml  :FGCOLOR IN BROWSE brTable1 = 12.
                   
        END.
        ELSE DO:
            ASSIGN  tt-doc-fiscal.cod-estabel     :FGCOLOR IN BROWSE brTable1 = 2
                   tt-doc-fiscal.cod-emitente     :FGCOLOR IN BROWSE brTable1 = 2           
                   c-nome-abrev                   :FGCOLOR IN BROWSE brTable1 = 2        
                   tt-doc-fiscal.cgc              :FGCOLOR IN BROWSE brTable1 = 2
                   tt-doc-fiscal.ins-estadual     :FGCOLOR IN BROWSE brTable1 = 2     
                   tt-doc-fiscal.esp-docto        :FGCOLOR IN BROWSE brTable1 = 2
                   tt-doc-fiscal.nr-doc-fis       :FGCOLOR IN BROWSE brTable1 = 2     
                   tt-doc-fiscal.serie            :FGCOLOR IN BROWSE brTable1 = 2  
                   tt-doc-fiscal.dt-emis-doc      :FGCOLOR IN BROWSE brTable1 = 2  
                   tt-doc-fiscal.cod-cfop         :FGCOLOR IN BROWSE brTable1 = 2  
                   tt-doc-fiscal.cod-cfop-xml     :FGCOLOR IN BROWSE brTable1 = 2 
                   tt-doc-fiscal.vl-cont-doc      :FGCOLOR IN BROWSE brTable1 = 2
                   tt-doc-fiscal.vl-cont-doc-xml  :FGCOLOR IN BROWSE brTable1 = 2 
                   tt-doc-fiscal.vl-tot-trib      :FGCOLOR IN BROWSE brTable1 = 2 
                   tt-doc-fiscal.vl-tot-trib-xml  :FGCOLOR IN BROWSE brTable1 = 2.
        END.
    END.

    RETURN "OK":U.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
    /*if l-mostra-dif then
        assign l-mostra-dif = no.    
    else
        assign l-mostra-dif = yes.

    {&open-query-brTable1}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExcel w-livre
ON CHOOSE OF btExcel IN FRAME f-cad
DO: 
    /*RUN utp/ut-msgs.p ("show",
                       27100 ,
                       "Gerar Relat¢rio Excel?~~" +
                       "Ser  Gerado um Relat¢rio em Excel com as Informa‡äes do Browser, confirma?").
    IF RETURN-VALUE = "no" THEN
        RETURN NO-APPLY. 

    ELSE DO:
        RUN esp/esof001crp.p (INPUT TABLE tt-doc-fiscal). 
    END.*/

      

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSelecao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSelecao w-livre
ON CHOOSE OF btSelecao IN FRAME f-cad /* Sele‡Æo */
DO:
   {&WINDOW-NAME}:SENSITIVE = FALSE.

    RUN esp/esof003a.w (INPUT-OUTPUT p-cod-estabel,
                        INPUT-OUTPUT p-dt-transacao-ini,   
                        INPUT-OUTPUT p-dt-transacao-fim,
                        INPUT-OUTPUT p-serie-ini,       
                        INPUT-OUTPUT p-serie-fim,       
                        INPUT-OUTPUT p-nr-doc-fis-ini,  
                        INPUT-OUTPUT p-nr-doc-fis-fim,  
                        INPUT-OUTPUT p-cod-emitente-ini,
                        INPUT-OUTPUT p-cod-emitente-fim,
                        INPUT-OUTPUT p-nota-sem-xml).

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
    RUN esp/esof003b.w (INPUT TABLE tt-doc-fiscal).
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
       RUN set-position IN h_p-exihel ( 1.17 , 142.86 ) NO-ERROR.
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
    
    FOR EACH tt-data:
        DELETE tt-data.
    END.
    
    DO da-cont = p-dt-transacao-ini TO p-dt-transacao-fim:
        CREATE tt-data.
        ASSIGN tt-data.data = da-cont.
    END.

    FOR EACH tt-doc-fiscal:
        DELETE tt-doc-fiscal.
    END. 

    FOR EACH tt-data,
        EACH  doc-fiscal USE-INDEX  ch-registro 
        WHERE doc-fiscal.dt-docto        = tt-data.data
            AND (doc-fiscal.tipo-nat     = 1 OR
                 doc-fiscal.tipo-nat     = 3)
            AND  doc-fiscal.cod-estabel  = p-cod-estabel
            AND  doc-fiscal.serie        >= p-serie-ini
            AND  doc-fiscal.serie        <= p-serie-fim
            AND  doc-fiscal.nr-doc-fis   >= p-nr-doc-fis-ini
            AND  doc-fiscal.nr-doc-fis   <= p-nr-doc-fis-fim
            AND  doc-fiscal.cod-emitente >= p-cod-emitente-ini
            AND  doc-fiscal.cod-emitente <= p-cod-emitente-fim
            AND  doc-fiscal.ind-sit-doc = 1
        
        /** EPC ****************************/
       /* AND   NOT CAN-FIND(FIRST tt-doctos-excluidos
                           WHERE tt-doctos-excluidos.rw-reg = ROWID(doc-fiscal))*/
        /** EPC ****************************/
        NO-LOCK
        BREAK BY doc-fiscal.dt-docto
              BY doc-fiscal.cod-estabel
              BY doc-fiscal.serie
              BY doc-fiscal.nr-doc-fis
              BY doc-fiscal.cod-emitente
              BY doc-fiscal.tipo-nat    
              BY doc-fiscal.esp-docto
              BY doc-fiscal.cod-cfop:
        
        IF  doc-fiscal.tipo-nat = 3 AND NOT l-consid-cfop-serv THEN NEXT.
        
        IF  doc-fiscal.tipo-nat = 3 AND
                doc-fiscal.cod-cfop <> "1933" AND
                doc-fiscal.cod-cfop <> "2933" THEN NEXT.
        
            CREATE tt-doc-fiscal.
            BUFFER-COPY doc-fiscal TO tt-doc-fiscal.
            
            ASSIGN tt-doc-fiscal.vl-tot-trib  = doc-fiscal.vl-ipi + doc-fiscal.vl-pis + doc-fiscal.vl-icms.

            /*MESSAGE "Docto" tt-doc-fiscal.nr-doc-fis SKIP
                    "VlTribut" tt-doc-fiscal.vl-tot-trib SKIP
                    "val-base-contrib-social" tt-doc-fiscal.val-base-contrib-social  SKIP
                    "val-desp-outros" tt-doc-fiscal.val-desp-outros  SKIP
                    "val-despch" tt-doc-fiscal.val-despch  SKIP
                    "vl-bicms" tt-doc-fiscal.vl-bicms  SKIP
                    "vl-bipi" tt-doc-fiscal.vl-bipi  SKIP
                    "vl-biss" tt-doc-fiscal.vl-biss  SKIP
                    "vl-bsubs" tt-doc-fiscal.vl-bsubs  SKIP
                    "vl-cont-doc" tt-doc-fiscal.vl-cont-doc  SKIP
                    "vl-embalagem" tt-doc-fiscal.vl-embalagem  SKIP
                    "vl-finsocial" tt-doc-fiscal.vl-finsocial  SKIP
                    "vl-frete" tt-doc-fiscal.vl-frete  SKIP
                    "vl-icms" tt-doc-fiscal.vl-icms  SKIP
                    "vl-icms-com" tt-doc-fiscal.vl-icms-com  SKIP
                    "vl-icmsnt" tt-doc-fiscal.vl-icmsnt  SKIP
                    "vl-icmsou" tt-doc-fiscal.vl-icmsou  SKIP
                    "vl-icmsub" tt-doc-fiscal.vl-icmsub  SKIP
                    "vl-ipi" tt-doc-fiscal.vl-ipi  SKIP
                    "vl-ipi-devol" tt-doc-fiscal.vl-ipi-devol  SKIP
                    "vl-ipint" tt-doc-fiscal.vl-ipint  SKIP
                    "vl-ipiou" tt-doc-fiscal.vl-ipiou  SKIP
                    "vl-irf" tt-doc-fiscal.vl-irf  SKIP
                    "vl-iss" tt-doc-fiscal.vl-iss  SKIP
                    "vl-issnt" tt-doc-fiscal.vl-issnt  SKIP
                    "vl-issou" tt-doc-fiscal.vl-issou  SKIP
                    "vl-mercad" tt-doc-fiscal.vl-mercad  SKIP
                    "vl-pis" tt-doc-fiscal.vl-pis
                VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
        
        
    END.

    IF p-nota-sem-xml = NO THEN DO:
        FOR EACH tt-doc-fiscal:

            FIND FIRST doc-orig-nfe NO-LOCK
                WHERE doc-orig-nfe.idi-orig-trad = 2
                  AND doc-orig-nfe.cod-emitente  = tt-doc-fiscal.cod-emitente
                  AND doc-orig-nfe.cod-estabel   = tt-doc-fiscal.cod-estabel
                  AND doc-orig-nfe.serie         = tt-doc-fiscal.serie
                  AND doc-orig-nfe.nro-docto     = tt-doc-fiscal.nr-doc-fis
                  AND doc-orig-nfe.dt-emissao    = tt-doc-fiscal.dt-emis-doc
                  AND doc-orig-nfe.dt-transacao  = tt-doc-fiscal.dt-docto NO-ERROR.
                IF NOT AVAIL doc-orig-nfe THEN
                    DELETE tt-doc-fiscal.
        END.
    END.

    FOR EACH tt-doc-fiscal:
        
        FOR EACH doc-orig-nfe 
           WHERE doc-orig-nfe.idi-orig-trad = 2
             AND doc-orig-nfe.cod-emitente  = tt-doc-fiscal.cod-emitente
             AND doc-orig-nfe.cod-estabel   = tt-doc-fiscal.cod-estabel
             AND doc-orig-nfe.serie         = tt-doc-fiscal.serie
             AND doc-orig-nfe.nro-docto     = tt-doc-fiscal.nr-doc-fis
             AND doc-orig-nfe.dt-emissao    = tt-doc-fiscal.dt-emis-doc
             AND doc-orig-nfe.dt-transacao  = tt-doc-fiscal.dt-docto NO-LOCK:

            FIND FIRST item-doc-orig-nfe OF doc-orig-nfe NO-LOCK NO-ERROR.
            
            IF AVAIL item-doc-orig-nfe THEN DO:
            
                ASSIGN tt-doc-fiscal.cod-cfop-xml    = item-doc-orig-nfe.cod-cfop.
            END.

            ASSIGN tt-doc-fiscal.vl-cont-doc-xml = doc-orig-nfe.valor-total
                   tt-doc-fiscal.vl-tot-trib-xml = doc-orig-nfe.valor-ipi + doc-orig-nfe.valor-pis + doc-orig-nfe.valor-icms.

            /* esof004*/
            FIND esp-cfop-natur NO-LOCK
                WHERE esp-cfop-natur.cod-cfop       = tt-doc-fiscal.cod-cfop-xml
                  AND esp-cfop-natur.cod-cfop-relac = tt-doc-fiscal.cod-cfop NO-ERROR.

            IF AVAIL esp-cfop-natur THEN
                ASSIGN tt-doc-fiscal.cfop-correta = YES.
            ELSE
                ASSIGN tt-doc-fiscal.cfop-correta = NO.

        END.
    END.
    
    {&open-query-brTable1}

    /*APPLY 'MOUSE-SELECT-CLICK' TO brTable1 IN FRAME f-cad.*/
    
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

  {utp/ut9000.i "ESOF003" "2.12.00.001"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  ASSIGN p-cod-estabel       = "01"
         p-dt-transacao-ini  = TODAY 
         p-dt-transacao-fim  = TODAY 
         p-serie-ini         = ""
         p-serie-fim         = "ZZZZ"
         p-nr-doc-fis-ini    = ""
         p-nr-doc-fis-fim    = "ZZZZZZZZZZZZZZZZ"
         p-cod-emitente-ini  = 0
         p-cod-emitente-fim  = 999999999
         p-nota-sem-xml      = YES. 

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-nome-abrev w-livre 
FUNCTION fn-nome-abrev RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND emitente
      WHERE emitente.cod-emitente = tt-doc-fiscal.cod-emitente NO-LOCK NO-ERROR.
  IF AVAIL emitente THEN
      ASSIGN c-nome-abrev = emitente.nome-abrev.
  ELSE
      ASSIGN c-nome-abrev = "".


  RETURN c-nome-abrev.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

