&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wZoom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wZoom 
/*:T*******************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i XX9999 9.99.99.999}

&IF "{&EMSFND_VERSION}" >= "1.00"
&THEN
&ENDIF

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program           <ProgramName>
&GLOBAL-DEFINE Version           <ProgramVersion>

&GLOBAL-DEFINE InitialPage       1
&GLOBAL-DEFINE FolderLabels      <Folder1 ,Folder 2 ,... , Folder8>

&GLOBAL-DEFINE Range             YES

&GLOBAL-DEFINE FieldsRangePage1  <Field1,Field2,...,FieldN>
&GLOBAL-DEFINE FieldsRangePage2  <Field1,Field2,...,FieldN>
&GLOBAL-DEFINE FieldsRangePage3  <Field1,Field2,...,FieldN>
&GLOBAL-DEFINE FieldsRangePage4  <Field1,Field2,...,FieldN>
&GLOBAL-DEFINE FieldsRangePage5  <Field1,Field2,...,FieldN>
&GLOBAL-DEFINE FieldsRangePage6  <Field1,Field2,...,FieldN>
&GLOBAL-DEFINE FieldsRangePage7  <Field1,Field2,...,FieldN>
&GLOBAL-DEFINE FieldsRangePage8  <Field1,Field2,...,FieldN>
&GLOBAL-DEFINE FieldsAnyKeyPage1 <YES,YES,...,YES>
&GLOBAL-DEFINE FieldsAnyKeyPage2 <YES,YES,...,YES>
&GLOBAL-DEFINE FieldsAnyKeyPage3 <YES,YES,...,YES>
&GLOBAL-DEFINE FieldsAnyKeyPage4 <YES,YES,...,YES>
&GLOBAL-DEFINE FieldsAnyKeyPage5 <YES,YES,...,YES>
&GLOBAL-DEFINE FieldsAnyKeyPage6 <YES,YES,...,YES>
&GLOBAL-DEFINE FieldsAnyKeyPage7 <YES,YES,...,YES>
&GLOBAL-DEFINE FieldsAnyKeyPage8 <YES,YES,...,YES>

&GLOBAL-DEFINE ttTable1          <Temp-Table Name>
&GLOBAL-DEFINE hDBOTable1        <Handle DBO Variable Name>
&GLOBAL-DEFINE DBOTable1         <DBOTable1 Table Name>

&GLOBAL-DEFINE ttTable2          <Temp-Table Name>
&GLOBAL-DEFINE hDBOTable2        <Handle DBO Variable Name>
&GLOBAL-DEFINE DBOTable2         <DBOTable2 Table Name>

&GLOBAL-DEFINE page1Browse      
&GLOBAL-DEFINE page2Browse      

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* Local Variable Definitions (DBOs Handles) --- */
DEFINE VARIABLE {&hDBOTable1} AS HANDLE NO-UNDO.
DEFINE VARIABLE {&hDBOTable2} AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Zoom

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0
&Scoped-define BROWSE-NAME brTable1

/* Definitions for FRAME fPage1                                         */

/* Definitions for FRAME fPage2                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rtToolBar btOK btCancel btHelp 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wZoom AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON btHelp 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON btOK 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 90 BY 1.42
     BGCOLOR 7 .

DEFINE BUTTON btImplant1 
     LABEL "Implantar" 
     SIZE 10 BY 1.

DEFINE BUTTON btImplant2 
     LABEL "Implantar" 
     SIZE 10 BY 1.


/* Browse definitions                                                   */
DEFINE BROWSE brTable1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brTable1 wZoom _STRUCTURED
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 82 BY 10.67
         FONT 2.

DEFINE BROWSE brTable2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brTable2 wZoom _STRUCTURED
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 82 BY 10.67
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     btOK AT ROW 16.71 COL 2
     btCancel AT ROW 16.71 COL 13
     btHelp AT ROW 16.71 COL 80
     rtToolBar AT ROW 16.5 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 16.98
         FONT 1.

DEFINE FRAME fPage1
     brTable1 AT ROW 2.33 COL 2
     btImplant1 AT ROW 13 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.5 ROW 2.45
         SIZE 84.43 BY 13.29
         FONT 1.

DEFINE FRAME fPage2
     brTable2 AT ROW 2.33 COL 2
     btImplant2 AT ROW 13 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.45
         SIZE 84.43 BY 13.29
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Zoom Template
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wZoom ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 17
         WIDTH              = 90
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 90
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 90
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


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wZoom
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME fPage1:FRAME = FRAME fpage0:HANDLE
       FRAME fPage2:FRAME = FRAME fpage0:HANDLE.

/* SETTINGS FOR FRAME fpage0
   FRAME-NAME                                                           */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME fPage1:MOVE-BEFORE-TAB-ITEM (FRAME fPage2:HANDLE)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FRAME fPage1
                                                                        */
/* BROWSE-TAB brTable1 1 fPage1 */
/* SETTINGS FOR FRAME fPage2
                                                                        */
/* BROWSE-TAB brTable2 1 fPage2 */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wZoom)
THEN wZoom:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fpage0
/* Query rebuild information for FRAME fpage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fpage0 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage1
/* Query rebuild information for FRAME fPage1
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fPage1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage2
/* Query rebuild information for FRAME fPage2
     _Query            is NOT OPENED
*/  /* FRAME fPage2 */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wZoom 
/* ************************* Included-Libraries *********************** */

{zoom/zoom.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wZoom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wZoom wZoom
ON END-ERROR OF wZoom
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wZoom wZoom
ON WINDOW-CLOSE OF wZoom
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancel wZoom
ON CHOOSE OF btCancel IN FRAME fpage0 /*:T Cancelar */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btHelp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btHelp wZoom
ON CHOOSE OF btHelp IN FRAME fpage0 /*:T Ajuda */
DO:
    {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME btImplant1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btImplant1 wZoom
ON CHOOSE OF btImplant1 IN FRAME fPage1 /*:T Implantar */
DO:
    {zoom/implant.i &ProgramImplant="<ProgramName>"
                    &PageNumber="1"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage2
&Scoped-define SELF-NAME btImplant2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btImplant2 wZoom
ON CHOOSE OF btImplant2 IN FRAME fPage2 /*:T Implantar */
DO:
    {zoom/implant.i &ProgramImplant="<ProgramName>"
                    &PageNumber="2"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define SELF-NAME btOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOK wZoom
ON CHOOSE OF btOK IN FRAME fpage0 /* OK */
DO:
    RUN returnValues IN THIS-PROCEDURE.
    
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brTable1
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wZoom 


/* ***************************  Main Block  *************************** */

/*:T--- L�gica para inicializa��o do programam ---*/
{zoom/mainblock.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeDBOs wZoom 
PROCEDURE initializeDBOs :
/*:T------------------------------------------------------------------------------
  Purpose:     Inicializa DBOs
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    /*:T--- Verifica se o DBO j� est� inicializado ---*/
    IF NOT VALID-HANDLE({&hDBOTable1}) OR
       {&hDBOTable1}:TYPE <> "PROCEDURE":U OR
       {&hDBOTable1}:FILE-NAME <> "<DBOProgram>":U THEN DO:
       
        {btb/btb008za.i1 <DBOProgram> YES}
        {btb/btb008za.i2 <DBOProgram> '' {&hDBOTable1}} 
    END.
    
    RUN setConstraint<Description> IN {&hDBOTable1} (<pamameters>) NO-ERROR.
    
    /*:T--- Verifica se o DBO j� est� inicializado ---*/
    IF NOT VALID-HANDLE({&hDBOTable2}) OR
       {&hDBOTable2}:TYPE <> "PROCEDURE":U OR
       {&hDBOTable2}:FILE-NAME <> "<DBOProgram>":U THEN DO:
       
        {btb/btb008za.i1 <DBOProgram> YES}
        {btb/btb008za.i2 <DBOProgram> '' {&hDBOTable2}} 
    END.
    
    RUN setConstraint<Description> IN {&hDBOTable2} (<pamameters>) NO-ERROR.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openQueries wZoom 
PROCEDURE openQueries :
/*:T------------------------------------------------------------------------------
  Purpose:     Atualiza browsers
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    {zoom/openqueries.i &Query="<QueryName>"
                        &PageNumber="1"}
    
    {zoom/openqueries.i &Query="<QueryName>"
                        &PageNumber="2"}
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE returnFieldsPage1 wZoom 
PROCEDURE returnFieldsPage1 :
/*:T------------------------------------------------------------------------------
  Purpose:     Retorna valores dos campos da p�gina 1
  Parameters:  recebe nome do campo
               retorna valor do campo
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE  INPUT PARAMETER pcField      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pcFieldValue AS CHARACTER NO-UNDO.
    
    IF AVAILABLE {&ttTable1} THEN DO:
        CASE pcField:
            WHEN "<FieldName>":U THEN
                ASSIGN pcFieldValue = STRING({&ttTable1}.<FieldName>).
        END CASE.
    END.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE returnFieldsPage2 wZoom 
PROCEDURE returnFieldsPage2 :
/*:T------------------------------------------------------------------------------
  Purpose:     Retorna valores dos campos da p�gina 2
  Parameters:  recebe nome do campo
               retorna valor do campo
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE  INPUT PARAMETER pcField      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pcFieldValue AS CHARACTER NO-UNDO.
    
    IF AVAILABLE {&ttTable2} THEN DO:
        CASE pcField:
            WHEN "<FieldName>":U THEN
                ASSIGN pcFieldValue = STRING({&ttTable2}.<FieldName>).
        END CASE.
    END.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setConstraints wZoom 
PROCEDURE setConstraints :
/*:T------------------------------------------------------------------------------
  Purpose:     Seta constraints e atualiza o browse, conforme n�mero da p�gina
               passado como par�metro
  Parameters:  recebe n�mero da p�gina
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE INPUT PARAMETER pPageNumber AS INTEGER NO-UNDO.
    
    /*:T--- Seta constraints conforme n�mero da p�gina ---*/
    CASE pPageNumber:
        WHEN 1 THEN
            /*:T--- Seta Constraints para o DBO Table1 ---*/
            RUN setConstraint<Description> IN {&hDBOTable1} (INPUT <Parameter>,
                                                              INPUT <Parameter>,
                                                              ...
                                                              INPUT <Parameter>).
        
        WHEN 2 THEN
            /*:T--- Seta Constraints para o DBO Table2 ---*/
            RUN setConstraint<Description> IN {&hDBOTable2} (INPUT <Parameter>,
                                                              INPUT <Parameter>,
                                                              ...
                                                              INPUT <Parameter>).
    END CASE.
    
    /*:T--- Seta vari�vel iConstraintPageNumber com o n�mero da p�gina atual 
          Esta vari�vel � utilizada no m�todo openQueries ---*/
    ASSIGN iConstraintPageNumber = pPageNumber.
    
    /*:T--- Atualiza browse ---*/
    RUN openQueries IN THIS-PROCEDURE.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


