&Scoped-define WINDOW-NAME wWindow
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESNF003D 2.00.00.000}  /*** 010009 ***/

/*:T*******************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

CREATE WIDGET-POOL.

{cdp/cdcfgdis.i}

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        ESNF003D
&GLOBAL-DEFINE Version        2.00.00.000

&GLOBAL-DEFINE WindowType     Detail

&GLOBAL-DEFINE page0Widgets   c-chave-acesso c-consulta ~
                              btHelp btOK btCancel 

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAM p-chave-acesso LIKE nfe003.ch-acesso-comp-nfe NO-UNDO. 

/* Local Variable Definitions ---                                       */



/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btOK btCancel btHelp RECT-1 rtToolBar 
&Scoped-Define DISPLAYED-OBJECTS c-chave-acesso c-consulta 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWindow AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "&Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON btHelp 
     LABEL "&Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON btOK 
     LABEL "&OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE c-chave-acesso AS CHARACTER FORMAT "x(60)" 
     LABEL "Chave Acesso" 
     VIEW-AS FILL-IN 
     SIZE 61.86 BY .88 NO-UNDO.

DEFINE VARIABLE c-consulta AS CHARACTER FORMAT "X(30)":U INITIAL "Consulta Chave Acesso" 
      VIEW-AS TEXT 
     SIZE 18 BY .67 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 2.54.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 78 BY 1.42
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     c-chave-acesso AT ROW 2.5 COL 14.14 COLON-ALIGNED HELP
          "Chave Acesso Completa Nota Fiscal" WIDGET-ID 4
     btOK AT ROW 4.83 COL 2
     btCancel AT ROW 4.83 COL 13
     btHelp AT ROW 4.83 COL 68
     c-consulta AT ROW 1.46 COL 4 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 1.71 COL 1
     rtToolBar AT ROW 4.58 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81.43 BY 7.25
         FONT 1.


/* *********************** Procedure Settings ************************ */

/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
 */

/* *************************  Create Window  ************************** */

IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWindow ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 5.17
         WIDTH              = 78.14
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 101
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 101
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

/* ************************* Included-Libraries *********************** */

{window/window.i}




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

/* SETTINGS FOR WINDOW wWindow
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME fpage0
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN c-chave-acesso IN FRAME fpage0
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-consulta IN FRAME fpage0
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWindow)
THEN wWindow:HIDDEN = yes.



/* Setting information for Queries and Browse Widgets fields            */

/* Query rebuild information for FRAME fpage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fpage0 */

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWindow
ON END-ERROR OF wWindow
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.


ON WINDOW-CLOSE OF wWindow
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.


&Scoped-define SELF-NAME btCancel
ON CHOOSE OF btCancel IN FRAME fpage0 /* Cancelar */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.


&Scoped-define SELF-NAME btHelp
ON CHOOSE OF btHelp IN FRAME fpage0 /* Ajuda */
DO:
    {include/ajuda.i}
END.


&Scoped-define SELF-NAME btOK
ON CHOOSE OF btOK IN FRAME fpage0 /* OK */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.


&UNDEFINE SELF-NAME



/*:T--- L¢gica para inicializa‡Æo do programam ---*/
{window/mainblock.i}


/* **********************  Internal Procedures  *********************** */

PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN c-chave-acesso                           = p-chave-acesso
           /*c-chave-acesso:FORMAT    IN FRAME fPage0 = "99.9999.99.999.999/9999-99-99-999-999.999.999-999.999.999-9"*/
           c-chave-acesso:READ-ONLY IN FRAME fPage0 = YES.

    /*
    OUTPUT TO "CLIPBOARD".
    PUT c-chave-acesso.
    OUTPUT CLOSE.*/

    DISP c-chave-acesso
        WITH FRAME fPage0.

    RETURN "OK":U.

END PROCEDURE.

