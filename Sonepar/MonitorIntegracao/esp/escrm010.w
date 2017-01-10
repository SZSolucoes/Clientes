&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgesp            PROGRESS
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
{include/i-prgvrs.i ESCRM010 12.01.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

def var c-des-tipo as char no-undo.
def var c-status   as char no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-integra

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES crm-web-processados

/* Definitions for BROWSE br-integra                                    */
&Scoped-define FIELDS-IN-QUERY-br-integra crm-web-processados.nome-tabela ~
fn-status() @ c-status crm-web-processados.num-transacao ~
fn-tipo() @ c-des-tipo crm-web-processados.chave-tabela ~
crm-web-processados.dt-transacao crm-web-processados.hr-transacao ~
crm-web-processados.dt-integra crm-web-processados.hr-integra ~
crm-web-processados.log-erro 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-integra 
&Scoped-define QUERY-STRING-br-integra FOR EACH crm-web-processados ~
      WHERE (crm-web-processados.nome-tabela = input frame {&frame-name} cb-table ~
 OR input frame {&frame-name} cb-table = 'Todas') ~
 AND crm-web-processados.dt-transacao >= input frame {&frame-name} dt-Inicial ~
 AND crm-web-processados.dt-transacao <= input frame {&frame-name} dt-Final ~
 AND crm-web-processados.hr-transacao >= input frame {&frame-name} hr-inicial ~
 AND crm-web-processados.hr-transacao <= input frame {&frame-name} hr-final ~
 AND (input frame {&frame-name} cb-status = 0 ~
 OR (input frame {&frame-name} cb-status = 1 ~
 AND crm-web-processados.dt-integra <> ?) ~
 OR (input frame {&frame-name} cb-status = 2 ~
 AND crm-web-processados.log-erro = TRUE)) NO-LOCK ~
    BY crm-web-processados.dt-transacao DESCENDING ~
       BY crm-web-processados.hr-transacao DESCENDING INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-integra OPEN QUERY br-integra FOR EACH crm-web-processados ~
      WHERE (crm-web-processados.nome-tabela = input frame {&frame-name} cb-table ~
 OR input frame {&frame-name} cb-table = 'Todas') ~
 AND crm-web-processados.dt-transacao >= input frame {&frame-name} dt-Inicial ~
 AND crm-web-processados.dt-transacao <= input frame {&frame-name} dt-Final ~
 AND crm-web-processados.hr-transacao >= input frame {&frame-name} hr-inicial ~
 AND crm-web-processados.hr-transacao <= input frame {&frame-name} hr-final ~
 AND (input frame {&frame-name} cb-status = 0 ~
 OR (input frame {&frame-name} cb-status = 1 ~
 AND crm-web-processados.dt-integra <> ?) ~
 OR (input frame {&frame-name} cb-status = 2 ~
 AND crm-web-processados.log-erro = TRUE)) NO-LOCK ~
    BY crm-web-processados.dt-transacao DESCENDING ~
       BY crm-web-processados.hr-transacao DESCENDING INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-integra crm-web-processados
&Scoped-define FIRST-TABLE-IN-QUERY-br-integra crm-web-processados


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-br-integra}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bt-atualiz bt-reenviar bt-retorno bt-next ~
cb-table cb-status dt-Inicial dt-final bt-prev hr-Inicial hr-final ~
c-pesquisa rt-button RECT-1 IMAGE-1 br-integra IMAGE-2 IMAGE-3 IMAGE-4 
&Scoped-Define DISPLAYED-OBJECTS cb-table cb-status dt-Inicial dt-final ~
hr-Inicial hr-final c-pesquisa 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-status w-livre 
FUNCTION fn-status RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-tipo w-livre 
FUNCTION fn-tipo RETURNS CHARACTER
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
DEFINE BUTTON bt-atualiz 
     IMAGE-UP FILE "image/toolbar/im-chck1.bmp":U
     IMAGE-DOWN FILE "image/toolbar/ii-chck1.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-chck1.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "bt atualiz" 
     SIZE 4 BY 1.13.

DEFINE BUTTON bt-next AUTO-GO 
     IMAGE-UP FILE "image/toolbar/im-abx1.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-relo.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Pr¢ximo" 
     SIZE 5 BY 1.25 TOOLTIP "Pr¢ximo"
     FONT 4.

DEFINE BUTTON bt-prev AUTO-GO 
     IMAGE-UP FILE "image/toolbar/im-acm1.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-relo.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Anterior" 
     SIZE 5 BY 1.25 TOOLTIP "Anterior"
     FONT 4.

DEFINE BUTTON bt-reenviar 
     LABEL "Reenviar" 
     SIZE 15 BY 1.13.

DEFINE BUTTON bt-retorno 
     LABEL "Retorno" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE cb-status AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Status" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Todos",0,
                     "Integrado",1,
                     "Erro",2
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cb-table AS CHARACTER FORMAT "X(256)":U INITIAL "Todas" 
     LABEL "Tabela" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Todas","aval-cred-cli","aval-cred-ped","canal-venda","cond-pagto","emitente","estabelec","fam-comerc","familia","gr-cli","grup-estoque","icms-it-uf","item","item-estab","item-uf","loc-entr","micro-reg","natur-oper","nota-fiscal","ped-ent","ramo-atividade","regiao","repres","saldo-estoq","transporte" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE c-pesquisa AS CHARACTER FORMAT "X(40)":U 
     LABEL "Pesquisa Chave" 
     VIEW-AS FILL-IN 
     SIZE 25 BY .88 NO-UNDO.

DEFINE VARIABLE dt-final AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE dt-Inicial AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Transaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE hr-final AS CHARACTER FORMAT "99:99:99":U INITIAL "235959" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE hr-Inicial AS CHARACTER FORMAT "99:99:99":U INITIAL "000000" 
     LABEL "Hora Transaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FONT 1 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 154 BY 2.46.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 154 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-integra FOR 
      crm-web-processados SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-integra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-integra w-livre _STRUCTURED
  QUERY br-integra NO-LOCK DISPLAY
      crm-web-processados.nome-tabela COLUMN-LABEL "Tabela":C FORMAT "X(32)":U
            WIDTH 20
      fn-status() @ c-status COLUMN-LABEL "Status":C FORMAT "x(15)":U
      crm-web-processados.num-transacao FORMAT ">>>,>>>,>>>,>>>,>>>,>>9":U
      fn-tipo() @ c-des-tipo COLUMN-LABEL "Tipo Trans.":C FORMAT "x(15)":U
            WIDTH 12
      crm-web-processados.chave-tabela COLUMN-LABEL "Chave Tabela":C FORMAT "X(300)":U
            WIDTH 30
      crm-web-processados.dt-transacao COLUMN-LABEL "Dt. Trans":C FORMAT "99/99/9999":U
            WIDTH 11
      crm-web-processados.hr-transacao COLUMN-LABEL "Hr. Trans.":C FORMAT "99:99:99":U
            WIDTH 10
      crm-web-processados.dt-integra FORMAT "99/99/9999":U
      crm-web-processados.hr-integra FORMAT "99:99:99":U
      crm-web-processados.log-erro COLUMN-LABEL "Erro":C FORMAT "Sim/N∆o":U
            WIDTH 3.14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 153 BY 20.75
         TITLE "Fila Integraá∆o" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-atualiz AT ROW 3.63 COL 96.14 WIDGET-ID 68
     bt-reenviar AT ROW 26.08 COL 124 WIDGET-ID 72
     bt-retorno AT ROW 26.08 COL 139.72 WIDGET-ID 70
     bt-next AT ROW 3.67 COL 141.14 HELP
          "Pr¢ximo" WIDGET-ID 66
     cb-table AT ROW 2.75 COL 9 COLON-ALIGNED WIDGET-ID 6
     cb-status AT ROW 3.83 COL 9 COLON-ALIGNED WIDGET-ID 8
     dt-Inicial AT ROW 2.83 COL 60.43 COLON-ALIGNED WIDGET-ID 22
     dt-final AT ROW 2.83 COL 81.43 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     bt-prev AT ROW 3.67 COL 135.14 HELP
          "Anterior" WIDGET-ID 64
     hr-Inicial AT ROW 3.83 COL 60.43 COLON-ALIGNED WIDGET-ID 26
     hr-final AT ROW 3.83 COL 81.43 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     c-pesquisa AT ROW 2.75 COL 125.14 COLON-ALIGNED WIDGET-ID 60
     br-integra AT ROW 5.25 COL 1.57 WIDGET-ID 200
     rt-button AT ROW 1 COL 1
     RECT-1 AT ROW 2.58 COL 1 WIDGET-ID 4
     IMAGE-1 AT ROW 2.83 COL 75.43 WIDGET-ID 32
     IMAGE-2 AT ROW 2.83 COL 79.43 WIDGET-ID 34
     IMAGE-3 AT ROW 3.83 COL 75.43 WIDGET-ID 36
     IMAGE-4 AT ROW 3.83 COL 79.43 WIDGET-ID 38
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 154.29 BY 26.33 WIDGET-ID 100.


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
         HEIGHT             = 26.38
         WIDTH              = 154.29
         MAX-HEIGHT         = 26.75
         MAX-WIDTH          = 154.29
         VIRTUAL-HEIGHT     = 26.75
         VIRTUAL-WIDTH      = 154.29
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
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB br-integra IMAGE-1 f-cad */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-integra
/* Query rebuild information for BROWSE br-integra
     _TblList          = "mgesp.crm-web-processados"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "mgesp.crm-web-processados.dt-transacao|no,mgesp.crm-web-processados.hr-transacao|no"
     _Where[1]         = "(crm-web-processados.nome-tabela = input frame {&frame-name} cb-table
 OR input frame {&frame-name} cb-table = 'Todas')
 AND crm-web-processados.dt-transacao >= input frame {&frame-name} dt-Inicial
 AND crm-web-processados.dt-transacao <= input frame {&frame-name} dt-Final
 AND crm-web-processados.hr-transacao >= input frame {&frame-name} hr-inicial
 AND crm-web-processados.hr-transacao <= input frame {&frame-name} hr-final
 AND (input frame {&frame-name} cb-status = 0
 OR (input frame {&frame-name} cb-status = 1
 AND crm-web-processados.dt-integra <> ?)
 OR (input frame {&frame-name} cb-status = 2
 AND crm-web-processados.log-erro = TRUE))"
     _FldNameList[1]   > mgesp.crm-web-processados.nome-tabela
"crm-web-processados.nome-tabela" "Tabela" ? "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "C" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"fn-status() @ c-status" "Status" "x(15)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "C" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = mgesp.crm-web-processados.num-transacao
     _FldNameList[4]   > "_<CALC>"
"fn-tipo() @ c-des-tipo" "Tipo Trans." "x(15)" ? ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "C" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > mgesp.crm-web-processados.chave-tabela
"crm-web-processados.chave-tabela" ? ? "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "C" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > mgesp.crm-web-processados.dt-transacao
"crm-web-processados.dt-transacao" ? ? "date" ? ? ? ? ? ? no ? no no "11" yes no no "U" "" "C" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > mgesp.crm-web-processados.hr-transacao
"crm-web-processados.hr-transacao" ? ? "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "C" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   = mgesp.crm-web-processados.dt-integra
     _FldNameList[9]   = mgesp.crm-web-processados.hr-integra
     _FldNameList[10]   > mgesp.crm-web-processados.log-erro
"crm-web-processados.log-erro" "Erro" "Sim/N∆o" "logical" ? ? ? ? ? ? no ? no no "3.14" yes no no "U" "" "C" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE br-integra */
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


&Scoped-define BROWSE-NAME br-integra
&Scoped-define SELF-NAME br-integra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-integra w-livre
ON MOUSE-SELECT-DBLCLICK OF br-integra IN FRAME f-cad /* Fila Integraá∆o */
DO:

    if avail crm-web-processados then do:

        current-window:sensitive = no.

        run esp/escrm010a.w (input crm-web-processados.retorno-webservice).

        current-window:sensitive = yes.

    end.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-atualiz
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-atualiz w-livre
ON CHOOSE OF bt-atualiz IN FRAME f-cad /* bt atualiz */
DO:
  
    {&open-query-br-integra}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-next w-livre
ON CHOOSE OF bt-next IN FRAME f-cad /* Pr¢ximo */
DO:
  run pi-find (2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-prev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-prev w-livre
ON CHOOSE OF bt-prev IN FRAME f-cad /* Anterior */
DO:
  run pi-find(1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-reenviar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-reenviar w-livre
ON CHOOSE OF bt-reenviar IN FRAME f-cad /* Reenviar */
DO:

    if avail crm-web-processados then do:

        if crm-web-processados.nome-tabela = 'ped-venda' then do:
            run utp/ut-msgs.p (input 'show',
                               input 17006,
                               input 'Pedido de Venda n∆o pode ser reenviado!').

            return no-apply.
        end.
        else do:

            if not crm-web-processados.log-erro then do:
                run utp/ut-msgs.p (input 'show',
                                   input 17006,
                                   input 'Registro j† foi processado com sucesso!').
                return no-apply.
            end.
            else
                run esp/escrmapi001.p (rowid(crm-web-processados)).
        end.

    end.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-retorno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-retorno w-livre
ON CHOOSE OF bt-retorno IN FRAME f-cad /* Retorno */
DO:

    if avail crm-web-processados then do:

        current-window:sensitive = no.

        run esp/escrm010a.w (input crm-web-processados.retorno-webservice).

        current-window:sensitive = yes.

    end.
  
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
       RUN set-position IN h_p-exihel ( 1.17 , 138.57 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             c-pesquisa:HANDLE IN FRAME f-cad , 'AFTER':U ).
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
  DISPLAY cb-table cb-status dt-Inicial dt-final hr-Inicial hr-final c-pesquisa 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE bt-atualiz bt-reenviar bt-retorno bt-next cb-table cb-status 
         dt-Inicial dt-final bt-prev hr-Inicial hr-final c-pesquisa rt-button 
         RECT-1 IMAGE-1 br-integra IMAGE-2 IMAGE-3 IMAGE-4 
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

    {utp/ut9000.i "ESCRM010" "12.01.00.001"}

    assign dt-Inicial = today
           dt-final   = today.

    /* Dispatch standard ADM method.                             */
    run dispatch in this-procedure ( input 'initialize':U ) .

    /* Code placed here will execute AFTER standard behavior.    */

    run pi-after-initialize.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-find w-livre 
PROCEDURE pi-find :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    define input param p-direction as int no-undo.

    def var h-query  as handle no-undo.
    def var h-buffer as handle no-undo.
    
    br-integra:fetch-selected-row(1) in frame {&frame-name} no-error .
    
    assign h-query = br-integra:query in frame {&frame-name}
           h-buffer = h-query:get-buffer-handle(1)
           input frame {&frame-name} c-pesquisa.
    
    if p-direction = 2 then do: /*next*/
        do while h-query:get-next(no-lock):
            if index(h-buffer:buffer-field("chave-tabela"):buffer-value,c-pesquisa) > 0  then do:
                h-query:reposition-to-rowid(h-buffer:rowid).
                leave.
            end.
        end.
    end.
    if p-direction = 1 then do: /*prev*/
        do while h-query:get-prev(no-lock):
            if index(h-buffer:buffer-field("chave-tabela"):buffer-value,c-pesquisa) > 0  then do:
                h-query:reposition-to-rowid(h-buffer:rowid).
                leave.
            end.
        end.
    end.

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
  {src/adm/template/snd-list.i "crm-web-processados"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-status w-livre 
FUNCTION fn-status RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    def var c-aux as char no-undo.

    if avail crm-web-processados then do:
        assign c-aux = 'Pendente'.

        if crm-web-processados.dt-integra <> ? then do:

            if crm-web-processados.log-erro then
                assign c-aux = 'Erro'.
            else
                assign c-aux = 'Integrado'.
        end.
    end.

    return c-aux.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-tipo w-livre 
FUNCTION fn-tipo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    def var c-aux as char no-undo.

    /*1 - Inclus∆o, 2 - Alteraá∆o, 3 - Exclus∆o*/

    if avail crm-web-processados then do:
        case crm-web-processados.tipo-trans:
            when 1 then
                assign c-aux = 'Inclus∆o':U.
            when 2 then
                assign c-aux = 'Alteraá∆o':U.
            when 3 then
                assign c-aux = 'Exclus∆o':U.
        end case.
    end.

    return c-aux.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

