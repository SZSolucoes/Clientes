&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME wReport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wReport 
/*:T*******************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESNF005 2.00.00.000}

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        ESNF005
&GLOBAL-DEFINE Version        1.00.00.000
&GLOBAL-DEFINE VersionLayout  

&GLOBAL-DEFINE Folder         YES
&GLOBAL-DEFINE InitialPage    1
&GLOBAL-DEFINE FolderLabels   Sele‡Æo,Digita‡Æo,ImpressÆo

&GLOBAL-DEFINE PGLAY          NO
&GLOBAL-DEFINE PGSEL          YES
&GLOBAL-DEFINE PGCLA          NO
&GLOBAL-DEFINE PGPAR          NO
&GLOBAL-DEFINE PGDIG          YES
&GLOBAL-DEFINE PGIMP          YES
&GLOBAL-DEFINE PGLOG          NO

&GLOBAL-DEFINE RTF            NO

&GLOBAL-DEFINE page0Widgets   btOk ~
                              btCancel ~
                              btHelp2
&GLOBAL-DEFINE page1Widgets   
&GLOBAL-DEFINE page2Widgets   c-editor
&GLOBAL-DEFINE page3Widgets   
&GLOBAL-DEFINE page4Widgets   
&GLOBAL-DEFINE page5Widgets   brDigita ~
                              btAdd ~
                              btUpdate ~
                              btDelete 
&GLOBAL-DEFINE page6Widgets   rsDestino ~
                              btConfigImpr ~
                              btFile ~
                              rsExecution ~
                              tg-parametros
&GLOBAL-DEFINE page7Widgets   
&GLOBAL-DEFINE page8Widgets   

&GLOBAL-DEFINE page0Text      
&GLOBAL-DEFINE page1Text      
&GLOBAL-DEFINE page2Text      
&GLOBAL-DEFINE page3Text      
&GLOBAL-DEFINE page4Text      
&GLOBAL-DEFINE page5Text      
&GLOBAL-DEFINE page6Text      text-destino text-modo text-param
&GLOBAL-DEFINE page7Text      
&GLOBAL-DEFINE page8Text   

&GLOBAL-DEFINE page1Fields    
&GLOBAL-DEFINE page2Fields    i-cod-emitente-ini i-cod-emitente-fim ~
                              c-nome-abrev-ini   c-nome-abrev-fim
&GLOBAL-DEFINE page3Fields    
&GLOBAL-DEFINE page4Fields    
&GLOBAL-DEFINE page5Fields    
&GLOBAL-DEFINE page6Fields    cFile
&GLOBAL-DEFINE page7Fields    
&GLOBAL-DEFINE page8Fields    

/* Parameters Definitions ---                                           */

DEFINE TEMP-TABLE tt-param NO-UNDO
    FIELD destino          AS INTEGER
    FIELD arquivo          AS CHAR FORMAT "x(35)":U
    FIELD arquivo-excel    AS CHAR FORMAT "x(35)":U
    FIELD usuario          AS CHAR FORMAT "x(12)":U
    FIELD data-exec        AS DATE
    FIELD hora-exec        AS INTEGER
    FIELD classifica       AS INTEGER
    FIELD desc-classifica  AS CHAR FORMAT "x(40)":U
    FIELD modelo           AS CHAR FORMAT "x(35)":U
    FIELD l-habilitaRtf    AS LOG
    FIELD cod-emitente-ini LIKE emitente.cod-emitente
    FIELD cod-emitente-fim LIKE emitente.cod-emitente
    FIELD nome-abrev-ini   LIKE emitente.nome-abrev
    FIELD nome-abrev-fim   LIKE emitente.nome-abrev
    FIELD tg-parametros    AS LOG .

DEFINE TEMP-TABLE tt-digita NO-UNDO
    FIELD cod-emitente LIKE emitente.cod-emitente 
    FIELD nome-abrev   LIKE emitente.nome-abrev
    INDEX id cod-emitente.

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.

DEF VAR l-ok            AS LOGICAL                NO-UNDO.
DEF VAR c-arq-digita    AS CHAR                   NO-UNDO.
DEF VAR c-terminal      AS CHAR                   NO-UNDO.
DEF VAR cArqConv        AS CHAR                   NO-UNDO.
DEF VAR c-arq-aux       AS CHAR                   NO-UNDO.
DEF VAR h-boad098       AS HANDLE                 NO-UNDO.
DEF VAR c-nome-abrev  LIKE emitente.nome-abrev    NO-UNDO.

/*15/02/2005 - tech1007 - Variavel definida para tratar se o programa est  rodando no WebEnabler*/
DEFINE            SHARED VARIABLE hWenController AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE adm-broker-hdl AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0
&Scoped-define BROWSE-NAME brDigita

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-digita

/* Definitions for BROWSE brDigita                                      */
&Scoped-define FIELDS-IN-QUERY-brDigita tt-digita.cod-emitente tt-digita.nome-abrev   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brDigita tt-digita.cod-emitente   
&Scoped-define ENABLED-TABLES-IN-QUERY-brDigita tt-digita
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-brDigita tt-digita
&Scoped-define SELF-NAME brDigita
&Scoped-define QUERY-STRING-brDigita FOR EACH tt-digita
&Scoped-define OPEN-QUERY-brDigita OPEN QUERY brDigita FOR EACH tt-digita.
&Scoped-define TABLES-IN-QUERY-brDigita tt-digita
&Scoped-define FIRST-TABLE-IN-QUERY-brDigita tt-digita


/* Definitions for FRAME fPage5                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fPage5 ~
    ~{&OPEN-QUERY-brDigita}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rtToolBar btOK btCancel btHelp2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn_nome_abrev wReport 
FUNCTION fn_nome_abrev RETURNS CHARACTER
  ( p-cod-emitente AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wReport AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "&Fechar" 
     SIZE 10 BY 1.

DEFINE BUTTON btHelp2 
     LABEL "&Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON btOK 
     LABEL "&Executar" 
     SIZE 10 BY 1.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE c-editor AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 60 BY 3.5 NO-UNDO.

DEFINE VARIABLE c-nome-abrev-fim AS CHARACTER FORMAT "X(12)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE c-nome-abrev-ini AS CHARACTER FORMAT "X(12)" 
     LABEL "Nome Abreviado":R17 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE i-cod-emitente-fim AS INTEGER FORMAT ">>>>>>>>9" INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE i-cod-emitente-ini AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     LABEL "Emitente":R8 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
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

DEFINE BUTTON btAdd 
     LABEL "Inserir" 
     SIZE 10 BY 1
     FONT 1.

DEFINE BUTTON btDelete 
     LABEL "Retirar" 
     SIZE 10 BY 1
     FONT 1.

DEFINE BUTTON btUpdate 
     LABEL "Alterar" 
     SIZE 10 BY 1
     FONT 1.

DEFINE BUTTON btConfigImpr 
     IMAGE-UP FILE "image\im-cfprt":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON btFile 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE cFile AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.14 BY .63
     FONT 1 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execu‡Æo" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63
     FONT 1 NO-UNDO.

DEFINE VARIABLE text-param AS CHARACTER FORMAT "X(256)":U INITIAL "Parƒmetros de ImpressÆo" 
      VIEW-AS TEXT 
     SIZE 17.86 BY .67 NO-UNDO.

DEFINE VARIABLE rsDestino AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
"Arquivo", 2,
"Terminal", 3
     SIZE 44 BY 1.08
     FONT 1 NO-UNDO.

DEFINE VARIABLE rsDestiny AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
"Arquivo", 2,
"Terminal", 3
     SIZE 44 BY 1.08
     FONT 1 NO-UNDO.

DEFINE VARIABLE rsExecution AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "On-Line", 1,
"Batch", 2
     SIZE 27.86 BY .92
     FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-32
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.14 BY 2.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.14 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.14 BY 1.71.

DEFINE VARIABLE tg-parametros AS LOGICAL INITIAL yes 
     LABEL "Imprimir Parƒmetros" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .83 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brDigita FOR 
      tt-digita SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brDigita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brDigita wReport _FREEFORM
  QUERY brDigita DISPLAY
      tt-digita.cod-emitente COLUMN-LABEL "Emitente"
      tt-digita.nome-abrev
ENABLE
      tt-digita.cod-emitente
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 82 BY 8.75
         BGCOLOR 15 FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     btOK AT ROW 16.75 COL 2
     btCancel AT ROW 16.75 COL 13
     btHelp2 AT ROW 16.75 COL 80
     rtToolBar AT ROW 16.5 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 17
         FONT 1 WIDGET-ID 100.

DEFINE FRAME fPage5
     brDigita AT ROW 1.25 COL 1
     btAdd AT ROW 10 COL 1
     btUpdate AT ROW 10 COL 11
     btDelete AT ROW 10 COL 21
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.81
         SIZE 84.43 BY 10.15
         FONT 1 WIDGET-ID 100.

DEFINE FRAME fPage2
     c-editor AT ROW 1.5 COL 14 NO-LABEL WIDGET-ID 16
     i-cod-emitente-ini AT ROW 5.88 COL 15.28
     i-cod-emitente-fim AT ROW 5.88 COL 54.14 NO-LABEL
     c-nome-abrev-ini AT ROW 6.88 COL 20 COLON-ALIGNED HELP
          "Nome abreviado do cliente/fornecedor" WIDGET-ID 2
     c-nome-abrev-fim AT ROW 6.88 COL 52.14 COLON-ALIGNED HELP
          "Nome abreviado do cliente/fornecedor" NO-LABEL WIDGET-ID 4
     IMAGE-1 AT ROW 5.88 COL 36.29
     IMAGE-2 AT ROW 5.88 COL 51.14
     IMAGE-3 AT ROW 6.88 COL 36.29 WIDGET-ID 6
     IMAGE-4 AT ROW 6.88 COL 51.14 WIDGET-ID 8
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.81
         SIZE 84.43 BY 10.15
         FONT 1
         TITLE "Objetivo" WIDGET-ID 100.

DEFINE FRAME fPage6
     text-param AT ROW 7.25 COL 1.14 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     rsDestino AT ROW 2.38 COL 3.14 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     cFile AT ROW 3.63 COL 3.14 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     btFile AT ROW 3.5 COL 43 HELP
          "Escolha do nome do arquivo"
     btConfigImpr AT ROW 3.5 COL 43 HELP
          "Configura‡Æo da impressora"
     rsExecution AT ROW 5.88 COL 2.86 HELP
          "Modo de Execu‡Æo" NO-LABEL
     text-destino AT ROW 1.63 COL 1.86 COLON-ALIGNED NO-LABEL
     text-modo AT ROW 5.13 COL 1.14 COLON-ALIGNED NO-LABEL
     rsDestiny AT ROW 2.38 COL 3.14 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL WIDGET-ID 2 NO-TAB-STOP 
     tg-parametros AT ROW 8.08 COL 3 WIDGET-ID 10
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.38 COL 2
     RECT-32 AT ROW 7.5 COL 2 WIDGET-ID 6
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.81
         SIZE 84.43 BY 10.15
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wReport ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 17
         WIDTH              = 90
         MAX-HEIGHT         = 22
         MAX-WIDTH          = 114.14
         VIRTUAL-HEIGHT     = 22
         VIRTUAL-WIDTH      = 114.14
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wReport 
/* ************************* Included-Libraries *********************** */

{report/report.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wReport
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME fPage2:FRAME = FRAME fpage0:HANDLE
       FRAME fPage5:FRAME = FRAME fpage0:HANDLE
       FRAME fPage6:FRAME = FRAME fpage0:HANDLE.

/* SETTINGS FOR FRAME fpage0
   NOT-VISIBLE FRAME-NAME                                               */
/* SETTINGS FOR FRAME fPage2
                                                                        */
/* SETTINGS FOR EDITOR c-editor IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN i-cod-emitente-fim IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN i-cod-emitente-ini IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME fPage5
                                                                        */
/* BROWSE-TAB brDigita 1 fPage5 */
/* SETTINGS FOR FRAME fPage6
   Custom                                                               */
/* SETTINGS FOR RADIO-SET rsDestiny IN FRAME fPage6
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       rsDestiny:HIDDEN IN FRAME fPage6           = TRUE.

ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME fPage6     = 
                "Destino".

ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME fPage6     = 
                "Execu‡Æo".

/* SETTINGS FOR FILL-IN text-param IN FRAME fPage6
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wReport)
THEN wReport:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brDigita
/* Query rebuild information for BROWSE brDigita
     _START_FREEFORM
OPEN QUERY brDigita FOR EACH tt-digita.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brDigita */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fpage0
/* Query rebuild information for FRAME fpage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fpage0 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage2
/* Query rebuild information for FRAME fPage2
     _Query            is NOT OPENED
*/  /* FRAME fPage2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage6
/* Query rebuild information for FRAME fPage6
     _Query            is NOT OPENED
*/  /* FRAME fPage6 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wReport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wReport wReport
ON END-ERROR OF wReport
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wReport wReport
ON WINDOW-CLOSE OF wReport
DO:
  /* This event will close the window and terminate the procedure.  */
  {report/logfin.i}  
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brDigita
&Scoped-define FRAME-NAME fPage5
&Scoped-define SELF-NAME brDigita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brDigita wReport
ON DEL OF brDigita IN FRAME fPage5
DO:
   apply 'choose' to btDelete in frame fPage5.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brDigita wReport
ON END-ERROR OF brDigita IN FRAME fPage5
ANYWHERE 
DO:
    if  brDigita:new-row in frame fPage5 then do:
        if  avail tt-digita then
            delete tt-digita.
        if  brDigita:delete-current-row() in frame fPage5 then. 
    end.                                                               
    else do:
        get current brDigita.
        display tt-digita.cod-emitente 
                tt-digita.nome-abrev
            with browse brDigita. 
    end.
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brDigita wReport
ON ENTER OF brDigita IN FRAME fPage5
ANYWHERE
DO:
  apply 'tab' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brDigita wReport
ON INS OF brDigita IN FRAME fPage5
DO:
   apply 'choose' to btAdd in frame fPage5.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brDigita wReport
ON OFF-END OF brDigita IN FRAME fPage5
DO:
   apply 'entry' to btAdd in frame fPage5.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brDigita wReport
ON ROW-ENTRY OF brDigita IN FRAME fPage5
DO:
   /*:T trigger para inicializar campos da temp table de digita‡Æo */
   if  brDigita:new-row in frame fPage5 then do:
       assign tt-digita.cod-emitente:screen-value in browse brDigita = ""
              tt-digita.nome-abrev:screen-value   in browse brDigita = "".
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brDigita wReport
ON ROW-LEAVE OF brDigita IN FRAME fPage5
DO:
    /*:T  aqui que a grava‡Æo da linha da temp-table ‚ efetivada.
       Por‚m as valida‡äes dos registros devem ser feitas na procedure pi-executar,
       no local indicado pelo coment rio */
    
    IF brDigita:NEW-ROW IN FRAME fPage5 THEN DO:
        do transaction on error undo, return no-apply:

            CREATE tt-digita.
            ASSIGN INPUT BROWSE brDigita tt-digita.cod-emitente.
            ASSIGN tt-digita.nome-abrev = fn_nome_abrev(tt-digita.cod-emitente).
            
            DISP tt-digita.nome-abrev WITH BROWSE brDigita.
    
            brDigita:CREATE-RESULT-LIST-ENTRY() in frame fPage5.
        end.
    END.
    else do transaction on error undo, return no-apply:
        IF AVAIL tt-digita THEN DO:
            ASSIGN INPUT BROWSE brDigita tt-digita.cod-emitente.
            ASSIGN tt-digita.nome-abrev = fn_nome_abrev(tt-digita.cod-emitente).
            DISP tt-digita.nome-abrev WITH BROWSE brDigita.
        END.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAdd wReport
ON CHOOSE OF btAdd IN FRAME fPage5 /* Inserir */
DO:
    assign btUpdate:SENSITIVE in frame fPage5 = yes
           btDelete:SENSITIVE in frame fPage5 = yes.
    
    if num-results("brDigita":U) > 0 then
        brDigita:INSERT-ROW("after":U) in frame fPage5.
    else do transaction:
        create tt-digita.
        
        open query brDigita for each tt-digita.
        
        apply "entry":U to tt-digita.cod-emitente in browse brDigita. 
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define SELF-NAME btCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancel wReport
ON CHOOSE OF btCancel IN FRAME fpage0 /* Fechar */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage6
&Scoped-define SELF-NAME btConfigImpr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btConfigImpr wReport
ON CHOOSE OF btConfigImpr IN FRAME fPage6
DO:
   {report/rpimp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage5
&Scoped-define SELF-NAME btDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDelete wReport
ON CHOOSE OF btDelete IN FRAME fPage5 /* Retirar */
DO:
    if  brDigita:num-selected-rows > 0 then do on error undo, return no-apply:
        get current brDigita.
        delete tt-digita.
        if  brDigita:delete-current-row() in frame fPage5 then.
    end.
    
    if num-results("brDigita":U) = 0 then
        assign btUpdate:SENSITIVE in frame fPage5 = no
               btDelete:SENSITIVE in frame fPage5 = no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage6
&Scoped-define SELF-NAME btFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFile wReport
ON CHOOSE OF btFile IN FRAME fPage6
DO:
    /*{report/rparq.i}*/

    assign cArqConv = replace(input frame fPage6 cFile, "/":U, "~\":U).
    SYSTEM-DIALOG GET-FILE cArqConv
       FILTERS "*.xls":U "*.xls":U,
               "*.*":U "*.*":U
       ASK-OVERWRITE 
       DEFAULT-EXTENSION "xls":U
       INITIAL-DIR session:temp-directory
       SAVE-AS
       USE-FILENAME
       UPDATE l-ok.
    if  l-ok = yes then do:
        assign cFile = replace(cArqConv, "~\":U, "/":U).
        display cFile with frame fPage6.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define SELF-NAME btHelp2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btHelp2 wReport
ON CHOOSE OF btHelp2 IN FRAME fpage0 /* Ajuda */
DO:
    {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOK wReport
ON CHOOSE OF btOK IN FRAME fpage0 /* Executar */
DO:
   do  on error undo, return no-apply:
       run piExecute.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage5
&Scoped-define SELF-NAME btUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUpdate wReport
ON CHOOSE OF btUpdate IN FRAME fPage5 /* Alterar */
DO:
   apply 'entry' to tt-digita.cod-emitente in browse brDigita. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage6
&Scoped-define SELF-NAME rsDestino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsDestino wReport
ON VALUE-CHANGED OF rsDestino IN FRAME fPage6
DO:
    do  with frame fPage6:
        case self:screen-value:
            when "1":U then do:
                assign cFile:sensitive                    = no
                       cFile:visible                      = yes
                       cFile:SCREEN-VALUE IN FRAME fPage6 = "":U
                       btFile:visible                     = no
                       btConfigImpr:visible               = yes.
            end.
            when "2":U then do:
                assign cFile:sensitive                    = yes
                       cFile:visible                      = yes
                       cFile:SCREEN-VALUE IN FRAME fPage6 = SESSION:TEMP-DIRECTORY + c-programa-mg97 + ".xls":U
                       btFile:visible                     = yes
                       btConfigImpr:visible               = no.
            end.
            when "3":U then do:
                assign cFile:visible                      = no
                       cFile:sensitive                    = no
                       cFile:SCREEN-VALUE IN FRAME fPage6 = SESSION:TEMP-DIRECTORY + c-programa-mg97 + ".xls":U
                       btFile:visible                     = no
                       btConfigImpr:visible               = no.
            END.
        end case.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsDestiny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsDestiny wReport
ON VALUE-CHANGED OF rsDestiny IN FRAME fPage6
DO:
   /* do  with frame fPage6:
        case self:screen-value:
            when "1":U then do:
                assign cFile:sensitive                    = no
                       cFile:visible                      = yes
                       cFile:SCREEN-VALUE IN FRAME fPage6 = "":U
                       btFile:visible                     = no
                       btConfigImpr:visible               = yes.
            end.
            when "2":U then do:
                assign cFile:sensitive                    = yes
                       cFile:visible                      = yes
                       cFile:SCREEN-VALUE IN FRAME fPage6 = SESSION:TEMP-DIRECTORY + c-programa-mg97 + ".xls":U
                       btFile:visible                     = yes
                       btConfigImpr:visible               = no.
            end.
            when "3":U then do:
                assign cFile:visible                      = no
                       cFile:sensitive                    = no
                       cFile:SCREEN-VALUE IN FRAME fPage6 = "":U
                       btFile:visible                     = no
                       btConfigImpr:visible               = no.
            END.
        end case.
    end.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsExecution
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsExecution wReport
ON VALUE-CHANGED OF rsExecution IN FRAME fPage6
DO:
   {report/rprse.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wReport 


/*:T--- L¢gica para inicializa‡Æo do programam ---*/
{report/mainblock.i}

IF NOT VALID-HANDLE(h-boad098) 
OR h-boad098:TYPE <> "PROCEDURE":U
OR h-boad098:FILE-NAME <> "adbo/boad098na.p":U THEN DO:
    RUN adbo/boad098na.p PERSISTENT SET h-boad098.

    RUN openQueryStatic IN h-boad098 (INPUT "Main":U) NO-ERROR.
END.

ON 'F5':U OF tt-digita.cod-emitente IN BROWSE brDigita 
OR 'MOUSE-SELECT-DBLCLICK':U OF tt-digita.cod-emitente IN BROWSE brDigita 
DO:
    {include/zoomvar.i &prog-zoom=adzoom/z01ad098.w
                        &campo=tt-digita.cod-emitente
                        &campozoom=cod-emitente
                        &BROWSE=brDigita
                        &frame=fPage5} 
                        
    APPLY "LEAVE":U TO tt-digita.cod-emitente IN BROWSE brDigita.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterInitializeInterface wReport 
PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN c-editor:SCREEN-VALUE IN FRAME fPage2 = "Este programa tem como objetivo listar as inconsistˆncias do fornecedor (CD0401) e do seu relacionamento com os itens (CC0105)." +
                                                   "SerÆo considerados apenas os emitentes que emitem NF-e.".
           c-editor:READ-ONLY    IN FRAME fPage2 = YES.

    APPLY "VALUE-CHANGED":U TO rsDestino IN FRAME fPage6.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beforeDestroyInterface wReport 
PROCEDURE beforeDestroyInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF VALID-HANDLE(h-boad098) THEN DO:
        RUN destroy IN h-boad098.
        ASSIGN h-boad098 = ?.
    END.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piExecute wReport 
PROCEDURE piExecute :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    define var r-tt-digita as rowid no-undo.
    
    do on error undo, return error on stop  undo, return error:
        /*{report/rpexa.i}*/

        IF cFile:SCREEN-VALUE IN FRAME fPage6 = "" THEN
            ASSIGN cFile:SCREEN-VALUE IN FRAME fPage6 = SESSION:TEMP-DIRECTORY + c-programa-mg97 + ".xls":U.
    
        if  input frame fPage6 rsDestino   = 2 
        and input frame fPage6 rsExecution = 1 then do:
    
            ASSIGN c-arq-aux = input frame fPage6 cFile
                   c-arq-aux = replace(c-arq-aux, "/":U, "~\":U).
            if  r-index(c-arq-aux, "~\":U) > 0 then do:
                assign file-info:file-name = substring(c-arq-aux,1,r-index(c-arq-aux, "~\":U)).
                if  file-info:full-pathname = ? or not file-info:file-type matches "*D*":U then do:
                    run utp/ut-msgs.p (input "show":U, 
                                       input 5749, 
                                       input "").
                    apply 'entry':U to cFile in frame fPage6.
                    return error.
                end.
            end.
            
            assign file-info:file-name = c-arq-aux.
            if file-info:file-type matches "*D*":U then do:
                run utp/ut-msgs.p (input "show":U, 
                                   input 73, 
                                   input "").
                apply 'entry':U to cFile in frame fPage6.
                return error.
            end.
        end.    
        /*******/
    
        /*15/02/2005 - tech1007 - Teste alterado pois RTF nÆo ‚ mais op‡Æo de Destino*/
        if input frame fPage6 rsDestino = 2 and
           input frame fPage6 rsExecution = 1 then do:
            run utp/ut-vlarq.p (input input frame fPage6 cFile).
            
            if return-value = "NOK":U then do:
                run utp/ut-msgs.p (input "show":U, input 73, input "":U).
                apply "ENTRY":U to cFile in frame fPage6.
                return error.
            end.
        end.
        
        /*:T Coloque aqui as valida‡äes da p gina de Digita‡Æo, lembrando que elas devem
           apresentar uma mensagem de erro cadastrada, posicionar nesta p gina e colocar
           o focus no campo com problemas */
        /*browse brDigita:SET-REPOSITIONED-ROW (browse brDigita:DOWN, "ALWAYS":U).*/
        
        for each tt-digita no-lock:
            assign r-tt-digita = rowid(tt-digita).
            
            /*:T Valida‡Æo de duplicidade de registro na temp-table tt-digita */
            find first b-tt-digita 
                where b-tt-digita.cod-emitente = tt-digita.cod-emitente 
                  and rowid(b-tt-digita) <> rowid(tt-digita) 
                no-lock no-error.
            if  avail b-tt-digita then do:
                reposition brDigita to rowid rowid(b-tt-digita).
                
                run utp/ut-msgs.p (input "SHOW":U, input 108, input "":U).
                apply "ENTRY":U to tt-digita.cod-emitente in browse brDigita.
                
                return error.
            end.
            
            /*:T As demais valida‡äes devem ser feitas aqui */
            IF  tt-digita.cod-emitente >= 0 THEN DO:
                RUN goToKey IN h-boad098 (INPUT tt-digita.cod-emitente).
                IF RETURN-VALUE = "NOK":U THEN DO:
    
                    ASSIGN BROWSE brDigita:CURRENT-COLUMN = tt-digita.cod-emitente:HANDLE IN BROWSE brDigita.
                
                    REPOSITION brDigita TO ROWID r-tt-digita.
                   
                    RUN utp/ut-msgs.p (INPUT "SHOW":U, input 2, input "Emitente").
                    APPLY "ENTRY":U TO tt-digita.cod-emitente IN BROWSE brDigita.
                    
                    RETURN ERROR.
                END.
            END.
        end.
        
        /*:T Aqui sÆo gravados os campos da temp-table que ser  passada como parƒmetro
           para o programa RP.P */
        
        create tt-param.
        assign tt-param.usuario          = c-seg-usuario
               tt-param.destino          = input frame fPage6 rsDestino
               tt-param.data-exec        = today
               tt-param.hora-exec        = time
               tt-param.cod-emitente-ini = INPUT FRAME fPage2 i-cod-emitente-ini
               tt-param.cod-emitente-fim = INPUT FRAME fPage2 i-cod-emitente-fim
               tt-param.nome-abrev-ini   = INPUT FRAME fPage2 c-nome-abrev-ini  
               tt-param.nome-abrev-fim   = INPUT FRAME fPage2 c-nome-abrev-fim 
               tt-param.tg-parametros    = INPUT FRAME fPage6 tg-parametros .
        
        if tt-param.destino = 1 
        then 
            assign tt-param.arquivo = "":U.
        else if  tt-param.destino = 2 
            then assign tt-param.arquivo-excel = input frame fPage6 cFile.
                else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U
                            tt-param.arquivo-excel = session:temp-directory + c-programa-mg97 + ".xls":U.
        
        /*:T Coloque aqui a l¢gica de grava‡Æo dos demais campos que devem ser passados
           como parƒmetros para o programa RP.P, atrav‚s da temp-table tt-param */
        
        
        
        /*:T Executar do programa RP.P que ir  criar o relat¢rio */
        {report/rpexb.i}
        
        SESSION:SET-WAIT-STATE("GENERAL":U).
        
        {report/rprun.i esp/esnf005rp.p}
        
        {report/rpexc.i}
        
        SESSION:SET-WAIT-STATE("":U).
        
        {report/rptrm.i}
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn_nome_abrev wReport 
FUNCTION fn_nome_abrev RETURNS CHARACTER
  ( p-cod-emitente AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    {method/ReferenceFields.i &HandleDBOLeave="h-boad098"
                              &KeyValue1="tt-digita.cod-emitente:screen-value in BROWSE brDigita"
                              &FieldName1="nome-abrev"
                              &Variable1="c-nome-abrev"}
    
    RETURN c-nome-abrev.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

