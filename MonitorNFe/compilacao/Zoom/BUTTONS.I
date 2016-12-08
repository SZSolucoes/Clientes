&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
/* Procedure Description
"Method Library que cont‚m as l¢gicas dos botäes do ToolBar."
*/
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Method-Library 
/*--------------------------------------------------------------------------
    Library    : zoom/Buttons.i
    Purpose    : Method Library que cont‚m as l¢gicas dos botäes do ToolBar 

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Method-Library
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Method-Library ASSIGN
         HEIGHT             = 2
         WIDTH              = 39.86.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Method-Library 
/* ************************* Included-Libraries *********************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Method-Library 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-returnValues) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE returnValues Method-Library 
PROCEDURE returnValues :
/*------------------------------------------------------------------------------
  Purpose:     Executa m‚todo para retorna de campos conforme a p gina 
               selecionada
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE cFieldValueAux  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hFieldHandleAux AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iNumFieldAux    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iNumPageAux     AS INTEGER   NO-UNDO.

    IF  VALID-HANDLE(hWindowParent) THEN
        ASSIGN hWindowParent:SENSITIVE = YES.
    
    /*--- Retorna o n£mero da p gina corrente ---*/
    &IF "{&Folder}":U = "YES":U &THEN
        RUN getCurrentFolder IN hFolder (OUTPUT iNumPageAux).
    &ENDIF
    
    /*--- Verifica se deve ser retornado o rowid do registro corrente, 
          neste caso ser  retornado o rowid da p gina corrente ---*/
    IF cFieldNames = "ROWID":U THEN DO:
        &IF NUM-ENTRIES("{&FolderLabels}":U) = 8 &THEN
            IF iNumPageAux = 8 THEN DO:
                IF AVAILABLE {&ttTable8} THEN
                    RUN repositionRecord IN WIDGET-HANDLE(cFieldHandles) (INPUT {&ttTable8}.r-Rowid).
                
                RETURN "OK":U.
            END.
        &ENDIF
        
        &IF NUM-ENTRIES("{&FolderLabels}":U) >= 7 &THEN
            IF iNumPageAux = 7 THEN DO:
                IF AVAILABLE {&ttTable7} THEN
                    RUN repositionRecord IN WIDGET-HANDLE(cFieldHandles) (INPUT {&ttTable7}.r-Rowid).
                
                RETURN "OK":U.
            END.
        &ENDIF
        
        &IF NUM-ENTRIES("{&FolderLabels}":U) >= 6 &THEN
            IF iNumPageAux = 6 THEN DO:
                IF AVAILABLE {&ttTable6} THEN
                    RUN repositionRecord IN WIDGET-HANDLE(cFieldHandles) (INPUT {&ttTable6}.r-Rowid).
                
                RETURN "OK":U.
            END.
        &ENDIF
        
        &IF NUM-ENTRIES("{&FolderLabels}":U) >= 5 &THEN
            IF iNumPageAux = 5 THEN DO:
                IF AVAILABLE {&ttTable5} THEN
                    RUN repositionRecord IN WIDGET-HANDLE(cFieldHandles) (INPUT {&ttTable5}.r-Rowid).
                
                RETURN "OK":U.
            END.
        &ENDIF
        
        &IF NUM-ENTRIES("{&FolderLabels}":U) >= 4 &THEN
            IF iNumPageAux = 4 THEN DO:
                IF AVAILABLE {&ttTable4} THEN
                    RUN repositionRecord IN WIDGET-HANDLE(cFieldHandles) (INPUT {&ttTable4}.r-Rowid).
                
                RETURN "OK":U.
            END.
        &ENDIF
        
        &IF NUM-ENTRIES("{&FolderLabels}":U) >= 3 &THEN
            IF iNumPageAux = 3 THEN DO:
                IF AVAILABLE {&ttTable3} THEN
                    RUN repositionRecord IN WIDGET-HANDLE(cFieldHandles) (INPUT {&ttTable3}.r-Rowid).
                
                RETURN "OK":U.
            END.
        &ENDIF
        
        &IF NUM-ENTRIES("{&FolderLabels}":U) >= 2 &THEN
            IF iNumPageAux = 2 THEN DO:
                IF AVAILABLE {&ttTable2} THEN
                    RUN repositionRecord IN WIDGET-HANDLE(cFieldHandles) (INPUT {&ttTable2}.r-Rowid).
                
                RETURN "OK":U.
            END.
        &ENDIF
        
        &IF NUM-ENTRIES("{&FolderLabels}":U) >= 1 &THEN
            IF iNumPageAux = 1 THEN DO:
                IF AVAILABLE {&ttTable1} THEN
                    RUN repositionRecord IN WIDGET-HANDLE(cFieldHandles) (INPUT {&ttTable1}.r-Rowid).
                
                RETURN "OK":U.
            END.
        &ENDIF
    END.
    
    DO iNumFieldAux = 1 TO NUM-ENTRIES(cFieldNames):
        /*--- Executa m‚todo para retorna do valor do campo ---*/
        RUN VALUE("returnFieldsPage":U + STRING(iNumPageAux)) IN THIS-PROCEDURE ( INPUT ENTRY(iNumFieldAux, cFieldNames),
                                                                                 OUTPUT cFieldValueAux).
        
        /*--- Seta propriedade SCREEN-VALUE com os valores dos campos a 
              serem retornados ---*/
        ASSIGN hFieldHandleAux = WIDGET-HANDLE(ENTRY(iNumFieldAux, cFieldHandles))
               hFieldHandleAux:SCREEN-VALUE = cFieldValueAux.
    END.
    
    /*--- Aplica ENTRY para o primeiro campo a ser retornado ---*/
    ASSIGN hFieldHandleAux = WIDGET-HANDLE(ENTRY(1, cFieldHandles)).
    APPLY "ENTRY":U TO hFieldHandleAux.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

