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
{include/i-prgvrs.i ESCM110 2.12.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE tt-es-acordo-comerc NO-UNDO LIKE es-acordo-comerc
    FIELD dt-aprov LIKE es-acordo-pendencia.dt-aprov
    FIELD sit-receb AS LOGICAL FORMAT "Sim/N∆o"
    FIELD sit-financ AS CHAR
    FIELD cod-aprovador LIKE es-acordo-pendencia.cod-aprovador
    FIELD sit-acordo-app AS CHAR
    FIELD ind-situacao LIKE es-acordo-pendencia.ind-situacao.

/* Parameters Definitions ---                                           */
DEFINE VARIABLE p-cod-estab-ini       LIKE estabelec.cod-estabel                  NO-UNDO. 
DEFINE VARIABLE p-cod-estab-fim       LIKE estabelec.cod-estabel                  NO-UNDO.
DEFINE VARIABLE p-cod-emitente-ini    LIKE emitente.cod-emitente                  NO-UNDO.
DEFINE VARIABLE p-cod-emitente-fim    LIKE emitente.cod-emitente                  NO-UNDO.
DEFINE VARIABLE p-cod-area-ini        LIKE es-acordo-area.cod-area                NO-UNDO.
DEFINE VARIABLE p-cod-area-fim        LIKE es-acordo-area.cod-area                NO-UNDO.
DEFINE VARIABLE p-cod-repres-ini      LIKE es-acordo-comerc.cod-repres            NO-UNDO.
DEFINE VARIABLE p-cod-repres-fim      LIKE es-acordo-comerc.cod-repres            NO-UNDO.
DEFINE VARIABLE p-dt-criacao-ini      LIKE es-acordo-comerc.dt-criacao            NO-UNDO. 
DEFINE VARIABLE p-dt-criacao-fim      LIKE es-acordo-comerc.dt-criacao            NO-UNDO.
DEFINE VARIABLE p-ind-situacao        LIKE es-acordo-pendencia.ind-situacao       NO-UNDO. 
DEFINE VARIABLE p-sit-acordo-aceite   LIKE es-acordo-comerc.sit-acordo-aceite     NO-UNDO.
DEFINE VARIABLE p-sit-acordo-contabil LIKE es-acordo-comerc.sit-acordo-contabil   NO-UNDO. 
                                                                          
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE c-situacao              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-sit-acordo-contabil   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-sit-acordo-aceite     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-sit-acordo-app        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-nome-area             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE l-atualizada            AS LOGICAL     NO-UNDO.
DEFINE VARIABLE c-nr-acordo-comerc-glob AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-nome-abrev            LIKE emitente.nome-emit            NO-UNDO.

DEFINE NEW GLOBAL SHARED VAR c-seg-usuario as char format "x(12)" no-undo.

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
&Scoped-define INTERNAL-TABLES tt-es-acordo-comerc

/* Definitions for BROWSE brTable1                                      */
&Scoped-define FIELDS-IN-QUERY-brTable1 tt-es-acordo-comerc.cod-estabel tt-es-acordo-comerc.cod-emitente fn-nome-abrev () @ c-nome-abrev fn-nome-area () @ c-nome-area tt-es-acordo-comerc.cod-repres tt-es-acordo-comerc.nr-acordo-comerc tt-es-acordo-comerc.sit-acordo-app fn-sit-acordo-aceite () @ c-sit-acordo-aceite tt-es-acordo-comerc.usuar-lib-aceite tt-es-acordo-comerc.dt-lib-aceite tt-es-acordo-comerc.hr-lib-aceite fn-sit-acordo-contabil () @ c-sit-acordo-contabil tt-es-acordo-comerc.usuar-lib-contabil tt-es-acordo-comerc.dt-lib-contabil tt-es-acordo-comerc.hr-lib-contabil tt-es-acordo-comerc.dt-criacao tt-es-acordo-comerc.vl-invest tt-es-acordo-comerc.impresso tt-es-acordo-comerc.dt-aprov tt-es-acordo-comerc.sit-receb tt-es-acordo-comerc.sit-financ tt-es-acordo-comerc.cod-aprovador   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brTable1   
&Scoped-define SELF-NAME brTable1
&Scoped-define QUERY-STRING-brTable1 FOR EACH tt-es-acordo-comerc NO-LOCK                             WHERE (tt-es-acordo-comerc.ind-situacao = p-ind-situacao - 1 OR                                     p-ind-situacao = 4)                              BY tt-es-acordo-comerc.cod-emitente        INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brTable1 OPEN QUERY {&SELF-NAME} FOR EACH tt-es-acordo-comerc NO-LOCK                             WHERE (tt-es-acordo-comerc.ind-situacao = p-ind-situacao - 1 OR                                     p-ind-situacao = 4)                              BY tt-es-acordo-comerc.cod-emitente        INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brTable1 tt-es-acordo-comerc
&Scoped-define FIRST-TABLE-IN-QUERY-brTable1 tt-es-acordo-comerc


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-brTable1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btAtualiza rt-button bt-inf-duplicata ~
brTable1 bt-aceita-acordo bt-libera-contabil btSelecao 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-nome-area w-livre 
FUNCTION fn-nome-area RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-sit-acordo-aceite w-livre 
FUNCTION fn-sit-acordo-aceite RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-sit-acordo-app w-livre 
FUNCTION fn-sit-acordo-app RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-sit-acordo-contabil w-livre 
FUNCTION fn-sit-acordo-contabil RETURNS CHARACTER
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
DEFINE BUTTON bt-aceita-acordo 
     LABEL "Aceite Acordo" 
     SIZE 17 BY 1.13.

DEFINE BUTTON bt-inf-duplicata 
     IMAGE-UP FILE "image/toolbar/im-aval.bmp":U
     IMAGE-DOWN FILE "image/toolbar/ii-aval.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-aval.bmp":U
     LABEL "Button 1" 
     SIZE 6 BY 1.13 TOOLTIP "Informaá∆o das Duplicatas".

DEFINE BUTTON bt-libera-contabil 
     LABEL "Libera Cont†bil" 
     SIZE 17 BY 1.13.

DEFINE BUTTON btAtualiza 
     IMAGE-UP FILE "image/toolbar/im-relo.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-relo.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Atualiza Documentos".

DEFINE BUTTON btSelecao 
     IMAGE-UP FILE "image/toolbar/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-ran.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Seleá∆o" 
     SIZE 5 BY 1.25 TOOLTIP "Seleá∆o Documentos".

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 185 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brTable1 FOR 
      tt-es-acordo-comerc SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brTable1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brTable1 w-livre _FREEFORM
  QUERY brTable1 NO-LOCK DISPLAY
      tt-es-acordo-comerc.cod-estabel COLUMN-LABEL "Estab" FORMAT "x(5)":U WIDTH 5
      tt-es-acordo-comerc.cod-emitente COLUMN-LABEL "Emit" FORMAT ">>>>>>>>9":U WIDTH 5
      fn-nome-abrev () @ c-nome-abrev COLUMN-LABEL "Cliente" FORMAT "x(12)" WIDTH 12
      fn-nome-area () @ c-nome-area COLUMN-LABEL "Area" FORMAT "x(12)" WIDTH 12
      tt-es-acordo-comerc.cod-repres FORMAT ">>>>9":U WIDTH 8
      tt-es-acordo-comerc.nr-acordo-comerc FORMAT "x(12)":U WIDTH 12
      tt-es-acordo-comerc.sit-acordo-app COLUMN-LABEL "Lib.Acordo App" FORMAT "x(18)" WIDTH 18
      fn-sit-acordo-aceite () @ c-sit-acordo-aceite COLUMN-LABEL "Liber. Aceite" FORMAT "x(18)" WIDTH 18
      tt-es-acordo-comerc.usuar-lib-aceite WIDTH 12
      tt-es-acordo-comerc.dt-lib-aceite   WIDTH 10
      tt-es-acordo-comerc.hr-lib-aceite   WIDTH 10
      fn-sit-acordo-contabil () @ c-sit-acordo-contabil COLUMN-LABEL "Liber.Contabil" FORMAT "x(19)" WIDTH 18
      tt-es-acordo-comerc.usuar-lib-contabil WIDTH 12
      tt-es-acordo-comerc.dt-lib-contabil   WIDTH 10
      tt-es-acordo-comerc.hr-lib-contabil   WIDTH 10
      tt-es-acordo-comerc.dt-criacao FORMAT "99/99/9999":U WIDTH 10
      tt-es-acordo-comerc.vl-invest FORMAT ">>>>>>>,>>9.99":U WIDTH 15
      tt-es-acordo-comerc.impresso FORMAT "Sim/N∆o" WIDTH 10
      tt-es-acordo-comerc.dt-aprov FORMAT "99/99/999" LABEL "Dt.Aprov.Acord" WIDTH 15
      tt-es-acordo-comerc.sit-receb FORMAT "Sim/N∆o" LABEL "Entr.Receb" WIDTH 10
      tt-es-acordo-comerc.sit-financ  WIDTH 10
      tt-es-acordo-comerc.cod-aprovador WIDTH 12
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SEPARATORS SIZE 184 BY 23.42
         FONT 1
         TITLE "Monitor de Acordos Comerciais".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     btAtualiza AT ROW 1.17 COL 7 WIDGET-ID 8
     bt-inf-duplicata AT ROW 1.17 COL 162.57 WIDGET-ID 14
     brTable1 AT ROW 2.58 COL 2 WIDGET-ID 200
     bt-aceita-acordo AT ROW 26.13 COL 2.14 WIDGET-ID 10
     bt-libera-contabil AT ROW 26.13 COL 19.29 WIDGET-ID 12
     btSelecao AT ROW 1.13 COL 1.72 WIDGET-ID 4
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
         HEIGHT             = 26.38
         WIDTH              = 185.43
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
/* BROWSE-TAB brTable1 bt-inf-duplicata f-cad */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brTable1
/* Query rebuild information for BROWSE brTable1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-es-acordo-comerc NO-LOCK
                            WHERE (tt-es-acordo-comerc.ind-situacao = p-ind-situacao - 1 OR
                                    p-ind-situacao = 4)
                             BY tt-es-acordo-comerc.cod-emitente
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
ON MOUSE-SELECT-CLICK OF brTable1 IN FRAME f-cad /* Monitor de Acordos Comerciais */
DO:
    IF AVAIL tt-es-acordo-comerc THEN DO:

        IF tt-es-acordo-comerc.sit-acordo-aceite = 1 THEN DO:

            ASSIGN bt-aceita-acordo:LABEL = "Aceite Acordo".

            DISABLE bt-libera-contabil WITH FRAME f-cad.
            ENABLE bt-aceita-acordo WITH FRAME f-cad.

        END.

        IF tt-es-acordo-comerc.sit-acordo-aceite = 2 THEN DO:

            ASSIGN bt-aceita-acordo:LABEL = "Desfaz Aceite Acordo".

            ENABLE bt-libera-contabil WITH FRAME f-cad.
            ENABLE bt-aceita-acordo WITH FRAME f-cad.

        END.

        IF tt-es-acordo-comerc.sit-acordo-contabil = 1 THEN DO:

            ASSIGN bt-libera-contabil:LABEL = "Libera Cont†bil".

            ENABLE bt-libera-contabil WITH FRAME f-cad.
            ENABLE bt-aceita-acordo WITH FRAME f-cad.

        END.

        IF tt-es-acordo-comerc.sit-acordo-contabil = 2 THEN DO:

            ASSIGN bt-libera-contabil:LABEL = "Desfaz Cont†bil".

            ENABLE bt-libera-contabil WITH FRAME f-cad.
            DISABLE bt-aceita-acordo WITH FRAME f-cad.

        END.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brTable1 w-livre
ON ROW-DISPLAY OF brTable1 IN FRAME f-cad /* Monitor de Acordos Comerciais */
DO:
    IF AVAIL tt-es-acordo-comerc THEN DO:

        IF  tt-es-acordo-comerc.sit-acordo-app     <> ""
            AND tt-es-acordo-comerc.sit-acordo-app <> "Aprovado Acordo" THEN
            /* Vermelho*/
            ASSIGN tt-es-acordo-comerc.cod-estabel:BGCOLOR   IN BROWSE brTable1 = 12
                   tt-es-acordo-comerc.cod-emitente:BGCOLOR   IN BROWSE brTable1 = 12
                   c-nome-abrev:BGCOLOR   IN BROWSE brTable1 = 12
                   c-nome-area:BGCOLOR   IN BROWSE brTable1 = 12
                   tt-es-acordo-comerc.cod-repres:BGCOLOR   IN BROWSE brTable1 = 12
                   tt-es-acordo-comerc.nr-acordo-comerc:BGCOLOR   IN BROWSE brTable1 = 12
                   tt-es-acordo-comerc.sit-acordo-app:BGCOLOR   IN BROWSE brTable1 = 12
                   c-sit-acordo-aceite:BGCOLOR   IN BROWSE brTable1 = 12
                   tt-es-acordo-comerc.usuar-lib-aceite:BGCOLOR   IN BROWSE brTable1 = 12
                   tt-es-acordo-comerc.dt-lib-aceite:BGCOLOR   IN BROWSE brTable1 = 12
                   tt-es-acordo-comerc.hr-lib-aceite:BGCOLOR   IN BROWSE brTable1 = 12
                   c-sit-acordo-contabil:BGCOLOR   IN BROWSE brTable1 = 12
                   tt-es-acordo-comerc.usuar-lib-contabil:BGCOLOR   IN BROWSE brTable1 = 12
                   tt-es-acordo-comerc.dt-lib-contabil:BGCOLOR   IN BROWSE brTable1 = 12
                   tt-es-acordo-comerc.hr-lib-contabil:BGCOLOR   IN BROWSE brTable1 = 12
                   tt-es-acordo-comerc.dt-criacao:BGCOLOR   IN BROWSE brTable1 = 12
                   tt-es-acordo-comerc.vl-invest:BGCOLOR   IN BROWSE brTable1 = 12
                   tt-es-acordo-comerc.impresso:BGCOLOR   IN BROWSE brTable1 = 12
                   tt-es-acordo-comerc.dt-aprov:BGCOLOR   IN BROWSE brTable1 = 12
                   tt-es-acordo-comerc.sit-receb:BGCOLOR   IN BROWSE brTable1 = 12
                   tt-es-acordo-comerc.sit-financ:BGCOLOR   IN BROWSE brTable1 = 12
                   tt-es-acordo-comerc.cod-aprovador:BGCOLOR   IN BROWSE brTable1 = 12.

        ELSE DO:
            /* Amarelo*/
            IF  tt-es-acordo-comerc.sit-acordo-app            <> ""
                AND tt-es-acordo-comerc.sit-acordo-app         =  "Aprovado Acordo"
                AND (tt-es-acordo-comerc.sit-acordo-aceite     = 1 OR /* Pendente Aceite */
                     tt-es-acordo-comerc.sit-acordo-contabil   = 1) THEN /* Pendente Cont†bil*/


                ASSIGN tt-es-acordo-comerc.cod-estabel:BGCOLOR   IN BROWSE brTable1 = 14
                       tt-es-acordo-comerc.cod-emitente:BGCOLOR   IN BROWSE brTable1 = 14
                       c-nome-abrev:BGCOLOR   IN BROWSE brTable1 = 14
                       c-nome-area:BGCOLOR   IN BROWSE brTable1 = 14
                       tt-es-acordo-comerc.cod-repres:BGCOLOR   IN BROWSE brTable1 = 14
                       tt-es-acordo-comerc.nr-acordo-comerc:BGCOLOR   IN BROWSE brTable1 = 14
                       tt-es-acordo-comerc.sit-acordo-app:BGCOLOR   IN BROWSE brTable1 = 14
                       c-sit-acordo-aceite:BGCOLOR   IN BROWSE brTable1 = 14
                       tt-es-acordo-comerc.usuar-lib-aceite:BGCOLOR   IN BROWSE brTable1 = 14
                       tt-es-acordo-comerc.dt-lib-aceite:BGCOLOR   IN BROWSE brTable1 = 14
                       tt-es-acordo-comerc.hr-lib-aceite:BGCOLOR   IN BROWSE brTable1 = 14
                       c-sit-acordo-contabil:BGCOLOR   IN BROWSE brTable1 = 14
                       tt-es-acordo-comerc.usuar-lib-contabil:BGCOLOR   IN BROWSE brTable1 = 14
                       tt-es-acordo-comerc.dt-lib-contabil:BGCOLOR   IN BROWSE brTable1 = 14
                       tt-es-acordo-comerc.hr-lib-contabil:BGCOLOR   IN BROWSE brTable1 = 14
                       tt-es-acordo-comerc.dt-criacao:BGCOLOR   IN BROWSE brTable1 = 14
                       tt-es-acordo-comerc.vl-invest:BGCOLOR   IN BROWSE brTable1 = 14
                       tt-es-acordo-comerc.impresso:BGCOLOR   IN BROWSE brTable1 = 14
                       tt-es-acordo-comerc.dt-aprov:BGCOLOR   IN BROWSE brTable1 = 14
                       tt-es-acordo-comerc.sit-receb:BGCOLOR   IN BROWSE brTable1 = 14
                       tt-es-acordo-comerc.sit-financ:BGCOLOR   IN BROWSE brTable1 = 14
                       tt-es-acordo-comerc.cod-aprovador:BGCOLOR   IN BROWSE brTable1 = 14.

            ELSE DO:
                /* Verde*/
                IF  tt-es-acordo-comerc.sit-acordo-app          <> ""
                    AND tt-es-acordo-comerc.sit-acordo-app       =  "Aprovado Acordo"
                    AND tt-es-acordo-comerc.sit-acordo-aceite   = 2 /** Autorizado Aceite Aceite */
                    AND tt-es-acordo-comerc.sit-acordo-contabil = 2 THEN /* Autorizado Aceite Cont†bil*/

                ASSIGN tt-es-acordo-comerc.cod-estabel:BGCOLOR   IN BROWSE brTable1 = 10
                       tt-es-acordo-comerc.cod-emitente:BGCOLOR   IN BROWSE brTable1 = 10
                       c-nome-abrev:BGCOLOR   IN BROWSE brTable1 = 10
                       c-nome-area:BGCOLOR   IN BROWSE brTable1 = 10
                       tt-es-acordo-comerc.cod-repres:BGCOLOR   IN BROWSE brTable1 = 10
                       tt-es-acordo-comerc.nr-acordo-comerc:BGCOLOR   IN BROWSE brTable1 = 10
                       tt-es-acordo-comerc.sit-acordo-app:BGCOLOR   IN BROWSE brTable1 = 10
                       c-sit-acordo-aceite:BGCOLOR   IN BROWSE brTable1 = 10
                       tt-es-acordo-comerc.usuar-lib-aceite:BGCOLOR   IN BROWSE brTable1 = 10
                       tt-es-acordo-comerc.dt-lib-aceite:BGCOLOR   IN BROWSE brTable1 = 10
                       tt-es-acordo-comerc.hr-lib-aceite:BGCOLOR   IN BROWSE brTable1 = 10
                       c-sit-acordo-contabil:BGCOLOR   IN BROWSE brTable1 = 10
                       tt-es-acordo-comerc.usuar-lib-contabil:BGCOLOR   IN BROWSE brTable1 = 10
                       tt-es-acordo-comerc.dt-lib-contabil:BGCOLOR   IN BROWSE brTable1 = 10
                       tt-es-acordo-comerc.hr-lib-contabil:BGCOLOR   IN BROWSE brTable1 = 10
                       tt-es-acordo-comerc.dt-criacao:BGCOLOR   IN BROWSE brTable1 = 10
                       tt-es-acordo-comerc.vl-invest:BGCOLOR   IN BROWSE brTable1 = 10
                       tt-es-acordo-comerc.impresso:BGCOLOR   IN BROWSE brTable1 = 10
                       tt-es-acordo-comerc.dt-aprov:BGCOLOR   IN BROWSE brTable1 = 10
                       tt-es-acordo-comerc.sit-receb:BGCOLOR   IN BROWSE brTable1 = 10
                       tt-es-acordo-comerc.sit-financ:BGCOLOR   IN BROWSE brTable1 = 10
                       tt-es-acordo-comerc.cod-aprovador:BGCOLOR   IN BROWSE brTable1 = 10.

            END.
            
        END.
        
    END.
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-aceita-acordo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-aceita-acordo w-livre
ON CHOOSE OF bt-aceita-acordo IN FRAME f-cad /* Aceite Acordo */
DO:
   
    IF tt-es-acordo-comerc.sit-acordo-aceite = 1 THEN DO:
        
        RUN utp/ut-msgs.p ("show",
                           27100 ,
                           "Aceita o Acordo Comercial?~~" +
                           "O Acordo Comercial" + " " + STRING(tt-es-acordo-comerc.nr-acordo-comerc) + " " + "ser† aceito, confirma?").
        if return-value = "no" then
            return no-apply. 
        ELSE DO:

            FIND es-acordo-comerc EXCLUSIVE-LOCK
                WHERE es-acordo-comerc.nr-acordo-comerc = tt-es-acordo-comerc.nr-acordo-comerc NO-ERROR.
            
            IF AVAIL es-acordo-comerc THEN DO:
                ASSIGN es-acordo-comerc.sit-acordo-aceite = 2
                       es-acordo-comerc.usuar-lib-aceite  = c-seg-usuario
                       es-acordo-comerc.dt-lib-aceite     = TODAY
                       es-acordo-comerc.hr-lib-aceite     = STRING(TIME,"hh:mm:ss").

                RUN carregaBrowseAcordo.

                RETURN.

            END.

        END.

    END.

    IF tt-es-acordo-comerc.sit-acordo-aceite = 2
        AND tt-es-acordo-comerc.sit-acordo-contabil  = 2  THEN DO:

        RUN utp/ut-msgs.p ("show",
                           17006 ,
                           "Primeiro Desfazer Liberaá∆o Contabil").
        RETURN NO-APPLY.
        
    END.
    IF tt-es-acordo-comerc.sit-acordo-aceite = 2
        AND tt-es-acordo-comerc.sit-acordo-contabil  <> 2  THEN DO:

        RUN utp/ut-msgs.p ("show",
                           27100 ,
                           "Desfaz o Aceite do Acordo Comercial?~~" +
                           "O Acordo Comercial" + " " + STRING(tt-es-acordo-comerc.nr-acordo-comerc) + " " + "ser† desfeito o Aceite, confirma?").
        if return-value = "no" then
            return no-apply. 
        ELSE DO:

            FIND es-acordo-comerc EXCLUSIVE-LOCK
                WHERE es-acordo-comerc.nr-acordo-comerc = tt-es-acordo-comerc.nr-acordo-comerc NO-ERROR.
            
            IF AVAIL es-acordo-comerc THEN DO:
                ASSIGN es-acordo-comerc.sit-acordo-aceite = 1
                       es-acordo-comerc.usuar-lib-aceite  = ""
                       es-acordo-comerc.dt-lib-aceite     = ?
                       es-acordo-comerc.hr-lib-aceite     = "".

                RUN carregaBrowseAcordo.

            END.
        END.
    END.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-inf-duplicata
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-inf-duplicata w-livre
ON CHOOSE OF bt-inf-duplicata IN FRAME f-cad /* Button 1 */
DO:
    IF AVAIL tt-es-acordo-comerc THEN DO:

        FIND FIRST ems2custom.ext-docum-est NO-LOCK
            WHERE ext-docum-est.nr-acordo-comerc = tt-es-acordo-comerc.nr-acordo-comerc NO-ERROR.

        IF AVAIL ext-docum-est THEN DO:
            
            {&WINDOW-NAME}:SENSITIVE = FALSE.
            
            RUN esp/escm110b.w (INPUT tt-es-acordo-comerc.cod-estab,
                                INPUT ext-docum-est.cod-emitente,
                                INPUT "DP", 
                                INPUT ext-docum-est.serie-docto,
                                INPUT ext-docum-est.nro-docto).
            
            {&WINDOW-NAME}:SENSITIVE = TRUE.
        END.
            
        RUN carregaBrowseAcordo.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-libera-contabil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-libera-contabil w-livre
ON CHOOSE OF bt-libera-contabil IN FRAME f-cad /* Libera Cont†bil */
DO: 
    /* Verifica se o Acordo j† deu entrada no RE1001 */
    FIND FIRST es-tit_ap NO-LOCK
        WHERE es-tit_ap.cdn_fornecedor     = tt-es-acordo-comerc.cod-emitente
          AND es-tit_ap.nr-acordo-comerc   = tt-es-acordo-comerc.nr-acordo-comerc NO-ERROR.

    IF AVAIL es-tit_ap THEN DO:

        RUN utp/ut-msgs.p ("show",
                           17006 ,
                           "Permiss∆o Negada ~~ " + "Acordo Comercial j† entrou no recebimento o Documento " + " " + STRING(es-tit_ap.cod_tit_ap)
                            + " " + "SÇrie" + " " + STRING(es-tit_ap.cod_ser_docto) + " " + "Fornecedor" + " " + STRING(es-tit_ap.cdn_fornecedor) ).
        RETURN NO-APPLY.
    END.

    IF tt-es-acordo-comerc.sit-acordo-contabil = 1
        AND tt-es-acordo-comerc.sit-acordo-aceite = 2 THEN DO:
        
        RUN utp/ut-msgs.p ("show",
                           27100 ,
                           "Libera Contabil?~~" +
                           "O Acordo Comercial" + " " + STRING(tt-es-acordo-comerc.nr-acordo-comerc) + " " + "ser† feito o Libera Contabil, confirma?").
        if return-value = "no" then
            return no-apply. 
        ELSE DO:

            FIND es-acordo-comerc EXCLUSIVE-LOCK
                WHERE es-acordo-comerc.nr-acordo-comerc = tt-es-acordo-comerc.nr-acordo-comerc NO-ERROR.
            
            IF AVAIL es-acordo-comerc THEN DO:
                ASSIGN es-acordo-comerc.sit-acordo-contabil = 2
                       es-acordo-comerc.usuar-lib-contabil  = c-seg-usuario
                       es-acordo-comerc.dt-lib-contabil     = TODAY
                       es-acordo-comerc.hr-lib-contabil     = STRING(TIME,"hh:mm:ss").

                RUN carregaBrowseAcordo.

                RETURN.

            END.

        END.

    END.

    IF tt-es-acordo-comerc.sit-acordo-contabil = 1
        AND tt-es-acordo-comerc.sit-acordo-aceite = 1 THEN DO:

        RUN utp/ut-msgs.p ("show",
                           17006 ,
                           "Primeiro Fazer Aceite Acordo").
        RETURN NO-APPLY.
        
    END.
    IF tt-es-acordo-comerc.sit-acordo-contabil = 2
        AND tt-es-acordo-comerc.sit-acordo-aceite = 2 THEN DO:

        RUN utp/ut-msgs.p ("show",
                           27100 ,
                           "Desfaz o Libera Contabil?~~" +
                           "O Acordo Comercial" + " " + STRING(c-nr-acordo-comerc-glob) + " " + "ser† desfeito o Libera Contabil, confirma?").
        if return-value = "no" then
            return no-apply. 
        ELSE DO:

            FIND es-acordo-comerc EXCLUSIVE-LOCK
                WHERE es-acordo-comerc.nr-acordo-comerc = tt-es-acordo-comerc.nr-acordo-comerc NO-ERROR.
            
            IF AVAIL es-acordo-comerc THEN DO:
                ASSIGN es-acordo-comerc.sit-acordo-contabil = 1
                       es-acordo-comerc.usuar-lib-contabil  = ""
                       es-acordo-comerc.dt-lib-contabil     = ?
                       es-acordo-comerc.hr-lib-contabil     = "".

                RUN carregaBrowseAcordo.

            END.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAtualiza
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAtualiza w-livre
ON CHOOSE OF btAtualiza IN FRAME f-cad
DO: 

    RUN carregaBrowseAcordo.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSelecao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSelecao w-livre
ON CHOOSE OF btSelecao IN FRAME f-cad /* Seleá∆o */
DO:
   {&WINDOW-NAME}:SENSITIVE = FALSE.

    RUN esp/escm110a.w (INPUT-OUTPUT p-cod-estab-ini,
                        INPUT-OUTPUT p-cod-estab-fim,
                        INPUT-OUTPUT p-cod-emitente-ini, 
                        INPUT-OUTPUT p-cod-emitente-fim,
                        INPUT-OUTPUT p-cod-area-ini,
                        INPUT-OUTPUT p-cod-area-fim,
                        INPUT-OUTPUT p-cod-repres-ini,
                        INPUT-OUTPUT p-cod-repres-fim,
                        INPUT-OUTPUT p-dt-criacao-ini,   
                        INPUT-OUTPUT p-dt-criacao-fim,
                        INPUT-OUTPUT p-ind-situacao,       
                        INPUT-OUTPUT p-sit-acordo-aceite,  
                        INPUT-OUTPUT p-sit-acordo-contabil).

    {&WINDOW-NAME}:SENSITIVE = TRUE.

    RUN carregaBrowseAcordo.

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
       RUN set-position IN h_p-exihel ( 1.17 , 169.57 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             bt-inf-duplicata:HANDLE IN FRAME f-cad , 'AFTER':U ).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carregaBrowseAcordo w-livre 
PROCEDURE carregaBrowseAcordo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FOR EACH tt-es-acordo-comerc:
        DELETE tt-es-acordo-comerc.
    END. 

    FOR EACH es-acordo-comerc
        WHERE es-acordo-comerc.cod-estabel        >= p-cod-estab-ini   
          AND es-acordo-comerc.cod-estabel        <= p-cod-estab-fim   
          AND es-acordo-comerc.cod-area           >= p-cod-area-ini     
          AND es-acordo-comerc.cod-area           <= p-cod-area-fim     
          AND es-acordo-comerc.cod-repres         >= p-cod-repres-ini   
          AND es-acordo-comerc.cod-repres         <= p-cod-repres-fim   
          AND es-acordo-comerc.cod-emitente       >= p-cod-emitente-ini 
          AND es-acordo-comerc.cod-emitente       <= p-cod-emitente-fim 
          AND es-acordo-comerc.dt-criacao         >= p-dt-criacao-ini  
          AND es-acordo-comerc.dt-criacao         <= p-dt-criacao-fim
          AND (es-acordo-comerc.sit-acordo-aceite  = p-sit-acordo-aceite OR
               p-sit-acordo-aceite = 3)
          AND (es-acordo-comerc.sit-acordo-contabil  = p-sit-acordo-contabil OR
               p-sit-acordo-contabil = 3) NO-LOCK:
        
        CREATE tt-es-acordo-comerc.
        BUFFER-COPY es-acordo-comerc TO tt-es-acordo-comerc.

        /* Verifica se o Acordo j† deu entrada no RE1001 */
        FIND FIRST es-tit_ap NO-LOCK
            WHERE es-tit_ap.cdn_fornecedor     = es-acordo-comerc.cod-emitente
              AND es-tit_ap.nr-acordo-comerc   = es-acordo-comerc.nr-acordo-comerc NO-ERROR.
        
        IF AVAIL es-tit_ap THEN DO:

            ASSIGN tt-es-acordo-comerc.sit-receb = YES.

        END.

        FIND FIRST es-acordo-pendencia NO-LOCK
            WHERE es-acordo-pendencia.nr-acordo-comerc = tt-es-acordo-comerc.nr-acordo-comerc
              AND es-acordo-pendencia.ind-situacao = 2 NO-ERROR.

        IF AVAIL es-acordo-pendencia THEN
             ASSIGN tt-es-acordo-comerc.sit-acordo-app = "Reprovado Acordo"
                    tt-es-acordo-comerc.ind-situacao   = 2.

        ELSE DO:
            FIND FIRST es-acordo-pendencia NO-LOCK
                WHERE es-acordo-pendencia.nr-acordo-comerc = tt-es-acordo-comerc.nr-acordo-comerc
                  AND es-acordo-pendencia.ind-situacao = 0 NO-ERROR.

            IF AVAIL es-acordo-pendencia THEN
                 ASSIGN tt-es-acordo-comerc.sit-acordo-app = "Pendente Acordo"
                        tt-es-acordo-comerc.ind-situacao   = 0.

            ELSE DO:
                FIND FIRST es-acordo-pendencia NO-LOCK
                    WHERE es-acordo-pendencia.nr-acordo-comerc = tt-es-acordo-comerc.nr-acordo-comerc
                      AND es-acordo-pendencia.ind-situacao = 1 NO-ERROR.

                IF AVAIL es-acordo-pendencia THEN
                     ASSIGN tt-es-acordo-comerc.sit-acordo-app = "Aprovado Acordo"
                            tt-es-acordo-comerc.ind-situacao   = 1.
                
                ELSE
                    ASSIGN tt-es-acordo-comerc.sit-acordo-app = "".

            END.

        END.
    
    END.
   
   
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
  ENABLE btAtualiza rt-button bt-inf-duplicata brTable1 bt-aceita-acordo 
         bt-libera-contabil btSelecao 
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

  {utp/ut9000.i "ESCM110" "2.12.00.001"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  ASSIGN p-cod-estab-ini       = ""
         p-cod-estab-fim       = "ZZZ"
         p-cod-emitente-ini    = 0
         p-cod-emitente-fim    = 999999999
         p-cod-area-ini        = 0
         p-cod-area-fim        = 999999
         p-cod-repres-ini      = 0
         p-cod-repres-fim      = 99999
         p-dt-criacao-ini      = TODAY - 30
         p-dt-criacao-fim      = TODAY
         p-ind-situacao        = 4    /* 1 = Pendente, 2 = Aprovado, 3 = Reprovado, 4 = Todos*/
         p-sit-acordo-aceite   = 3    /* 1 = Pendente Aceite, 2 = Autorizado Aceite, 3 = Todos*/
         p-sit-acordo-contabil = 3 .  /* 1 = Pendente Contabil, 2 = Autorizado Contabil, 3 = Todos*/

  RUN carregaBrowseAcordo.

  /* Code placed here will execute AFTER standard behavior.    */

  RUN pi-after-initialize.

  RUN carregaBrowseAcordo.

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
  {src/adm/template/snd-list.i "tt-es-acordo-comerc"}

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
      WHERE emitente.cod-emitente = tt-es-acordo-comerc.cod-emitente NO-LOCK NO-ERROR.
  IF AVAIL emitente THEN
      ASSIGN c-nome-abrev = emitente.nome-abrev.
  ELSE 
      ASSIGN c-nome-abrev = "".

  RETURN c-nome-abrev.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-nome-area w-livre 
FUNCTION fn-nome-area RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    FIND es-acordo-area NO-LOCK
        WHERE es-acordo-area.cod-area = tt-es-acordo-comerc.cod-area NO-ERROR.

    IF AVAIL es-acordo-area THEN
        ASSIGN c-nome-area = es-acordo-area.descricao.
    ELSE
        ASSIGN c-nome-area = "".

  RETURN c-nome-area.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-sit-acordo-aceite w-livre 
FUNCTION fn-sit-acordo-aceite RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    CASE tt-es-acordo-comerc.sit-acordo-aceite:
        WHEN 1 THEN
            ASSIGN c-sit-acordo-aceite = "Pendente Aceite".
        WHEN 2 THEN
            ASSIGN c-sit-acordo-aceite = "Autorizado Aceite".
    END CASE.

    RETURN c-sit-acordo-aceite.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-sit-acordo-app w-livre 
FUNCTION fn-sit-acordo-app RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    ASSIGN c-sit-acordo-app = "".
    IF AVAIL tt-es-acordo-comerc THEN DO:

        FIND FIRST es-acordo-pendencia NO-LOCK
            WHERE es-acordo-pendencia.nr-acordo-comerc = tt-es-acordo-comerc.nr-acordo-comerc
              AND es-acordo-pendencia.ind-situacao = 2 NO-ERROR.

        IF AVAIL es-acordo-pendencia THEN
             ASSIGN c-sit-acordo-app = "Reprovado Acordo".

        ELSE DO:
            FIND FIRST es-acordo-pendencia NO-LOCK
                WHERE es-acordo-pendencia.nr-acordo-comerc = tt-es-acordo-comerc.nr-acordo-comerc
                  AND es-acordo-pendencia.ind-situacao = 0 NO-ERROR.

            IF AVAIL es-acordo-pendencia THEN
                 ASSIGN c-sit-acordo-app = "Pendente Acordo".

            ELSE DO:
                FIND FIRST es-acordo-pendencia NO-LOCK
                    WHERE es-acordo-pendencia.nr-acordo-comerc = tt-es-acordo-comerc.nr-acordo-comerc
                      AND es-acordo-pendencia.ind-situacao = 1 NO-ERROR.

                IF AVAIL es-acordo-pendencia THEN
                     ASSIGN c-sit-acordo-app = "Aprovado Acordo".
                
                ELSE
                    ASSIGN c-sit-acordo-app = "".

            END.

        END.
        
    END.
    
    RETURN c-sit-acordo-app.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-sit-acordo-contabil w-livre 
FUNCTION fn-sit-acordo-contabil RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    CASE tt-es-acordo-comerc.sit-acordo-contabil:
        WHEN 1 THEN
            ASSIGN c-sit-acordo-contabil = "Pendente Cont†bil".
        WHEN 2 THEN
            ASSIGN c-sit-acordo-contabil = "Autorizado Cont†bil".
    END CASE.

    RETURN c-sit-acordo-contabil.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

