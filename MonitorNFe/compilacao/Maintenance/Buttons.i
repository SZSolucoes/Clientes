&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
/* Procedure Description
"Method Library que contÇm as l¢gicas dos bot‰es do ToolBar."
*/
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Method-Library 
/*--------------------------------------------------------------------------
    Library    : maintenance/Buttons.i
    Purpose    : Method Library que contÇm as l¢gicas dos bot‰es do ToolBar

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

DEFINE VARIABLE offQuery AS LOGICAL NO-UNDO.

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
         WIDTH              = 40.
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

&IF DEFINED(EXCLUDE-addRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addRecord Method-Library 
PROCEDURE addRecord :
/*------------------------------------------------------------------------------
  Purpose:     Inclui novo registro
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE cButtonsStateAux AS CHARACTER NO-UNDO.
    
    /*--- Seta cursor do mouse para espera ---*/
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    IF AVAILABLE {&ttTable} THEN 
         assign epc-rowid = {&ttTable}.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customizaá∆o (before/after) ---*/
    {method/custom.i &Event="BEFORE-ADD"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable}"
                     &RowidTable="epc-rowid"}
    
    /*--- Seta vari†vel cAction ---*/
    ASSIGN cAction = "ADD":U.
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        IF AVAILABLE {&ttTable} THEN DO:
            /*--- Limpa temp-table ttBuffer ---*/
            FOR EACH ttBuffer:
                DELETE ttBuffer.
            END.
            
            /*--- Atualiza temp-table ttBuffer ---*/
            CREATE ttBuffer.
            BUFFER-COPY {&ttTable} TO ttBuffer.
        END.
    &ENDIF
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Cria novo registro para temp-table {&ttTable} ---*/
        FOR EACH {&ttTable}:
            DELETE {&ttTable}.
        END.
        
        CREATE {&ttTable}.
    &ELSE
        /*--- Cria novo registro para temp-table {&ttTable} no DBO ---*/
        RUN newRecord IN {&hDBOTable}.
    &ENDIF
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- N∆o h† a necessidade de retornar registro da temp-table {&ttTable},
              pois para o BO 1.1 o registro Ç criado pelo pr¢prio thinTemplate ---*/
    &ELSE
        /*--- Retorna registro da temp-table {&ttTable} do DBO ---*/
        RUN getRecord IN {&hDBOTable} (OUTPUT TABLE {&ttTable}).
    &ENDIF
    
    /*--- Atualize dados em tela ---*/
    RUN displayFields IN THIS-PROCEDURE.

    DO WITH FRAME fPage0:
        /*--- Seta vari†vel cButtonsState com o estado atual dos bot‰es 
              de navegaá∆o ---*/
        ASSIGN
            
            cButtonsState = "":U
            
            &IF "{&First}":U = "YES":U &THEN
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btFirst:SENSITIVE)
            &ENDIF

            &IF "{&Prev}":U = "YES":U &THEN
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btPrev:SENSITIVE)
            &ENDIF

            &IF "{&Next}":U = "YES":U &THEN
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btNext:SENSITIVE)
            &ENDIF

            &IF "{&Last}":U = "YES":U &THEN
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btLast:SENSITIVE)
            &ENDIF

            &IF "{&GoTo}":U = "YES":U &THEN
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btgoTo:SENSITIVE)
            &ENDIF

            &IF "{&Search}":U = "YES":U &THEN
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btSearch:SENSITIVE)
            &ENDIF

            &IF "{&Add}":U = "YES":U &THEN
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btAdd:SENSITIVE)
            &ENDIF

            &IF "{&Copy}":U = "YES":U &THEN
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btCopy:SENSITIVE)
            &ENDIF

            &IF "{&Update}":U = "YES":U &THEN
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btUpdate:SENSITIVE)
            &ENDIF

            &IF "{&Delete}":U = "YES":U &THEN
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btDelete:SENSITIVE)
            &ENDIF 
            
            /* tech14187 - 10/04/2007 - 1468007 - n∆o Ç necess†rio guardar o estado anterior destes bot‰es. */
/*             /*tech1139 - 05/12/2006 - FO 1387.046 */                                                         */
/*             &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN                                                  */
/*                 cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btQueryJoins:SENSITIVE)   */
/*             &ENDIF                                                                                           */
/*                                                                                                              */
/*             &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN                                                */
/*                 cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btReportsJoins:SENSITIVE) */
/*             &ENDIF                                                                                           */
/*             /*tech14207*/                                                                                    */
/*                                                                                                              */
/*             /*tech1139 - bot∆o Exit */                                                                       */
/*             cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btExit:SENSITIVE)             */
/*                                                                                                              */
/*             /*tech1139 - bot∆o Help */                                                                       */
/*             cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btHelp:SENSITIVE)             */
/*                                                                                                              */
/*             /*tech1139 - 05/12/2006 - FO 1387.046 */                                                         */
            .
        
        /*--- Seta vari†vel cButtonsStateAux para ser passada como parÉmetro para o 
              mÇtodo controlToolBar ---*/
        ASSIGN
            
            cButtonsStateAux = "":U
            
            &IF "{&First}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Prev}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Next}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Last}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&GoTo}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Search}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Add}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Copy}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Update}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Delete}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Undo}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Cancel}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Save}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            /*tech1139 - 05/12/2006 - FO 1387.046 */
            &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U            
            &ENDIF
            
            &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U                        
            &ENDIF
            
            /*tech1139 - bot∆o Exit */
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U                        

           /*tech1139 - bot∆o Help */
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U                                     

            /*tech1139 - 05/12/2006 - FO 1387.046 */
            .

    END.
    

    /*--- Seta estado dos bot‰es/menu ---*/
    RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
    
    /*--- Seta estado das frames ---*/
    RUN enableFields IN THIS-PROCEDURE.

    IF AVAILABLE {&ttTable} THEN 
         assign epc-rowid = {&ttTable}.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.    

    /*--- Executa programas de customizaá∆o (before/after) ---*/
    {method/custom.i &Event="AFTER-ADD"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable}"
                     &RowidTable="epc-rowid"}
    
    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-cancelRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancelRecord Method-Library 
PROCEDURE cancelRecord :
/*------------------------------------------------------------------------------
  Purpose:     Cancela alteraá‰es feitas para o registro corrente
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE cButtonsStateAux AS CHARACTER NO-UNDO.
    
    /*--- Seta cursor do mouse para espera ---*/
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    IF AVAILABLE {&ttTable} THEN 
         assign epc-rowid = {&ttTable}.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customizaá∆o (before/after) ---*/
    {method/custom.i &Event="BEFORE-CANCEL"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable}"
                     &RowidTable="epc-rowid"}
    
    /*--- Elimina janela de mensagens de erro ---*/
    {method/showmessage.i3}
    
    DO WITH FRAME fPage0:
        /*--- Seta vari†vel cButtonsStateAux para ser passada como parÉmetro para o 
              mÇtodo controlToolBar ---*/
        ASSIGN
            
            cButtonsStateAux = "":U
            
            &IF "{&First}":U = "YES":U &THEN
                &IF "{&ButtonEntry}":U = "":U &THEN
                    &SCOPED-DEFINE ButtonEntry 1
                &ELSE
                    &SCOPED-DEFINE ButtonEntry {&ButtonEntry} + 1
                &ENDIF
                
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + ENTRY({&ButtonEntry}, cButtonsState)
            &ENDIF
            
            &IF "{&Prev}":U = "YES":U &THEN
                &IF "{&ButtonEntry}":U = "":U &THEN
                    &SCOPED-DEFINE ButtonEntry 1
                &ELSE
                    &SCOPED-DEFINE ButtonEntry {&ButtonEntry} + 1
                &ENDIF
                
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + ENTRY({&ButtonEntry}, cButtonsState)
            &ENDIF
            
            &IF "{&Next}":U = "YES":U &THEN
                &IF "{&ButtonEntry}":U = "":U &THEN
                    &SCOPED-DEFINE ButtonEntry 1
                &ELSE
                    &SCOPED-DEFINE ButtonEntry {&ButtonEntry} + 1
                &ENDIF
                
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + ENTRY({&ButtonEntry}, cButtonsState)
            &ENDIF
            
            &IF "{&Last}":U = "YES":U &THEN
                &IF "{&ButtonEntry}":U = "":U &THEN
                    &SCOPED-DEFINE ButtonEntry 1
                &ELSE
                    &SCOPED-DEFINE ButtonEntry {&ButtonEntry} + 1
                &ENDIF
                
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + ENTRY({&ButtonEntry}, cButtonsState)
            &ENDIF
            
            &IF "{&GoTo}":U = "YES":U &THEN
                &IF "{&ButtonEntry}":U = "":U &THEN
                    &SCOPED-DEFINE ButtonEntry 1
                &ELSE
                    &SCOPED-DEFINE ButtonEntry {&ButtonEntry} + 1
                &ENDIF
                
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + ENTRY({&ButtonEntry}, cButtonsState)
            &ENDIF
            
            &IF "{&Search}":U = "YES":U &THEN
                &IF "{&ButtonEntry}":U = "":U &THEN
                    &SCOPED-DEFINE ButtonEntry 1
                &ELSE
                    &SCOPED-DEFINE ButtonEntry {&ButtonEntry} + 1
                &ENDIF
                
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + ENTRY({&ButtonEntry}, cButtonsState)
            &ENDIF
            
            &IF "{&Add}":U = "YES":U &THEN
                &IF "{&ButtonEntry}":U = "":U &THEN
                    &SCOPED-DEFINE ButtonEntry 1
                &ELSE
                    &SCOPED-DEFINE ButtonEntry {&ButtonEntry} + 1
                &ENDIF
                
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + ENTRY({&ButtonEntry}, cButtonsState)
            &ENDIF
            
            &IF "{&Copy}":U = "YES":U &THEN
                &IF "{&ButtonEntry}":U = "":U &THEN
                    &SCOPED-DEFINE ButtonEntry 1
                &ELSE
                    &SCOPED-DEFINE ButtonEntry {&ButtonEntry} + 1
                &ENDIF
                
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + ENTRY({&ButtonEntry}, cButtonsState)
            &ENDIF
            
            &IF "{&Update}":U = "YES":U &THEN
                &IF "{&ButtonEntry}":U = "":U &THEN
                    &SCOPED-DEFINE ButtonEntry 1
                &ELSE
                    &SCOPED-DEFINE ButtonEntry {&ButtonEntry} + 1
                &ENDIF
                
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + ENTRY({&ButtonEntry}, cButtonsState)
            &ENDIF
            
            &IF "{&Delete}":U = "YES":U &THEN
                &IF "{&ButtonEntry}":U = "":U &THEN
                    &SCOPED-DEFINE ButtonEntry 1
                &ELSE
                    &SCOPED-DEFINE ButtonEntry {&ButtonEntry} + 1
                &ENDIF
                
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + ENTRY({&ButtonEntry}, cButtonsState)
            &ENDIF
            
            &IF "{&Undo}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Cancel}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Save}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            
            /*tech1139 - 05/12/2006 - FO 1387.046 */ 
            /* tech14187 - 10/04/2007 - 1468007*/
            &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
                        
            /*bot∆o Exit */
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U

            /*bot∆o Help */
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U

            /*tech1139 - 05/12/2006 - FO 1387.046 */
            .
    END.
    
    /*--- Seta estado dos bot‰es/menu ---*/
    RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).

    /*--- Seta estado das frames ---*/
    RUN disableFields IN THIS-PROCEDURE.
    
    /*--- Limpa temp-table {&ttTable} ---*/
    FOR EACH {&ttTable}:
        DELETE {&ttTable}.
    END.
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Atualiza temp-table {&ttTable} com base na temp-table ttBuffer ---*/
        FIND FIRST ttBuffer NO-ERROR.
        IF AVAILABLE ttBuffer THEN DO:
            CREATE {&ttTable}.
            BUFFER-COPY ttBuffer TO {&ttTable}.
        END.
    &ELSE
        /*--- Atualiza temp-table {&ttTable} com base no registro corrento do DBO ---*/
        
        /*--- Limpa temp-table RowErrors do DBO ---*/
        RUN emptyRowErrors IN {&hDBOTable}.
        
        /*--- Posiciona DBO no registro corrente ---*/
        RUN bringCurrent IN {&hDBOTable}.
        
        /*--- Retorna registro da temp-table {&ttTable} do DBO ---*/
        RUN getRecord IN {&hDBOTable} (OUTPUT TABLE {&ttTable}).
    &ENDIF
    
    ASSIGN cAction = "":U.
    
    /*--- Atualize dados em tela ---*/
    RUN displayFields IN THIS-PROCEDURE.
    
    IF AVAILABLE {&ttTable} THEN 
         assign epc-rowid = {&ttTable}.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customizaá∆o (before/after) ---*/
    {method/custom.i &Event="AFTER-CANCEL"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable}"
                     &RowidTable="epc-rowid"}
    
    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-copyRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyRecord Method-Library 
PROCEDURE copyRecord :
/*------------------------------------------------------------------------------
  Purpose:     Copia registro atual
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE cButtonsStateAux AS CHARACTER NO-UNDO.
    
    /*--- Seta cursor do mouse para espera ---*/
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    IF AVAILABLE {&ttTable} THEN 
         assign epc-rowid = {&ttTable}.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customizaá∆o (before/after) ---*/
    {method/custom.i &Event="BEFORE-COPY"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable}"
                     &RowidTable="epc-rowid"}
    
    /*--- Seta vari†vel cAction ---*/
    ASSIGN cAction = "COPY":U.
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        IF AVAILABLE {&ttTable} THEN DO:
            /*--- Limpa temp-table ttBuffer ---*/
            FOR EACH ttBuffer:
                DELETE ttBuffer.
            END.
            
            /*--- Atualiza temp-table ttBuffer ---*/
            CREATE ttBuffer.
            BUFFER-COPY {&ttTable} TO ttBuffer.
        END.
    &ENDIF
    
    DO WITH FRAME fPage0:
        /*--- Seta vari†vel cButtonsState com o estado atual dos bot‰es 
              de navegaá∆o ---*/
        ASSIGN
            
            cButtonsState = "":U
            
            &IF "{&First}":U = "YES":U &THEN
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btFirst:SENSITIVE)
            &ENDIF
            
            &IF "{&Prev}":U = "YES":U &THEN
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btPrev:SENSITIVE)
            &ENDIF
            
            &IF "{&Next}":U = "YES":U &THEN
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btNext:SENSITIVE)
            &ENDIF
            
            &IF "{&Last}":U = "YES":U &THEN
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btLast:SENSITIVE)
            &ENDIF
            
            &IF "{&GoTo}":U = "YES":U &THEN
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btgoTo:SENSITIVE)
            &ENDIF
            
            &IF "{&Search}":U = "YES":U &THEN
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btSearch:SENSITIVE)
            &ENDIF
            
            &IF "{&Add}":U = "YES":U &THEN
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btAdd:SENSITIVE)
            &ENDIF
            
            &IF "{&Copy}":U = "YES":U &THEN
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btCopy:SENSITIVE)
            &ENDIF
            
            &IF "{&Update}":U = "YES":U &THEN
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btUpdate:SENSITIVE)
            &ENDIF
            
            &IF "{&Delete}":U = "YES":U &THEN
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btDelete:SENSITIVE)
            &ENDIF
            

            /* tech14187 - 10/04/2007 - 1468007 - n∆o Ç necess†rio guardar o estado anterior destes bot‰es. */
/*             /*tech1139 - 05/12/2006 - FO 1387.046 */                                                         */
/*             &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN /*alterado*/                                     */
/*                 cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btQueryJoins:SENSITIVE)   */
/*             &ENDIF                                                                                           */
/*                                                                                                              */
/*             &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN                                                */
/*                 cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btReportsJoins:SENSITIVE) */
/*             &ENDIF                                                                                           */
/*                                                                                                              */
/*             /*tech1139 - bot∆o Exit */                                                                       */
/*             cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btExit:SENSITIVE)             */
/*                                                                                                              */
/*             /*tech1139 - bot∆o Help */                                                                       */
/*             cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btHelp:SENSITIVE)             */
/*                                                                                                              */
/*             /*tech1139 - 05/12/2006 - FO 1387.046 */                                                         */
            .
        
        /*--- Seta vari†vel cButtonsStateAux para ser passada como parÉmetro para o 
              mÇtodo controlToolBar ---*/
        ASSIGN
            
            cButtonsStateAux = "":U
            
            &IF "{&First}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Prev}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Next}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Last}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&GoTo}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Search}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Add}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Copy}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Update}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Delete}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Undo}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Cancel}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Save}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            /*tech1139 - 05/12/2006 - FO 1387.046 */
            &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN /*alterado*/
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            /*tech1139 - bot∆o Exit */
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                
            /*tech1139 - bot∆o Help */
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U

            /*tech1139 - 05/12/2006 - FO 1387.046 */
            .
    END.
    
    /*--- Seta estado dos bot‰es/menu ---*/
    RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
    
    /*--- Seta estado das frames ---*/
    RUN enableFields IN THIS-PROCEDURE.
    
    IF AVAILABLE {&ttTable} THEN 
         assign epc-rowid = {&ttTable}.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customizaá∆o (before/after) ---*/
    {method/custom.i &Event="AFTER-COPY"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable}"
                     &RowidTable="epc-rowid"}
    
    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-deleteRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteRecord Method-Library 
PROCEDURE deleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     Elimina registro corrente
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE cButtonsStateAux AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturnAux       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE rCurrentAux      AS ROWID     NO-UNDO.
    
    /*--- Seta cursor do mouse para espera ---*/
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    IF AVAILABLE {&ttTable} THEN 
         assign epc-rowid = {&ttTable}.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customizaá∆o (before/after) ---*/
    {method/custom.i &Event="BEFORE-DELETE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable}"
                     &RowidTable="epc-rowid"}
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- N∆o h† a necessidade de limpar a temp-table RowErrors, pois para
              o BO 1.1 a temp-table Ç limpa pelo pr¢prio mÇtodo de eliminaá∆o ---*/
    &ELSE
        /*--- Limpa temp-table RowErrors no DBO ---*/
        RUN emptyRowErrors IN {&hDBOTable}.
    &ENDIF
    
    /*--- Elimina janela de mensagens de erro ---*/
    {method/showmessage.i3}
    
    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    
    /*--- Exibe mensagem de confirmaá∆o de eliminaá∆o ---*/
    RUN utp/ut-msgs.p (INPUT "SHOW":U, 
                       INPUT 550, 
                       INPUT "":U).
    
    /*--- Seta cursor do mouse para espera ---*/
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    IF RETURN-VALUE = "YES":U THEN DO:
        /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
        &IF DEFINED(DBOVersion) <> 0 &THEN
            /*--- Seta vari†vel rCurrentAux com o valor da vari†vel rCurrent ---*/
            ASSIGN rCurrentAux = rCurrent.
            
            /*--- Elimina registro passado como parÉmetro para o BO 1.1 ---*/
            RUN validateDelete IN {&hDBOTable} (INPUT-OUTPUT rCurrentAux,
                                                OUTPUT TABLE RowErrors).
            
            /*--- Transfere dados da tabela {&ttTable} para a temp-table de 
                  comunicaá∆o, somente internamento no BO 1.1 ---*/
            RUN setCurrent IN {&hDBOTable}.
            
            /*--- Seta vari†vel cReturnAux com o valor OK para indicar que a 
                  eliminaá∆o foi feita com sucesso ou NOK para indicar o contr†rio ---*/
            IF NOT CAN-FIND(FIRST RowErrors) AND rCurrentAux <> ? THEN
                ASSIGN cReturnAux = "OK":U.
            ELSE
                ASSIGN cReturnAux = "NOK":U.
        &ELSE
            /*--- Elimina registro corrento do DBO ---*/
            RUN deleteRecord IN {&hDBOTable}.
            
            /*--- Seta vari†vel cReturnAux com o conte£do do RETURN-VALUE ---*/
            ASSIGN cReturnAux = RETURN-VALUE.
        &ENDIF
        
        /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
        &IF DEFINED(DBOVersion) <> 0 &THEN
            /*--- Retorna registro da temp-table {&ttTable} do BO 1.1 ---*/
            RUN getCurrent IN {&hDBOTable} (OUTPUT TABLE {&ttTable}).
        &ELSE
            /*--- Retorna registro da temp-table {&ttTable} do DBO ---*/
            RUN getRecord IN {&hDBOTable} (OUTPUT TABLE {&ttTable}).
        &ENDIF
        
        /*--- Atualize dados em tela ---*/
        RUN displayFields IN THIS-PROCEDURE.
        
        /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
        &IF DEFINED(DBOVersion) <> 0 &THEN
            /*--- Quando for utilizado BO 1.1 n∆o ser† feito o tratamento dos 
                  erros, estes somente s∆o exibidos em tela 
                  PorÇm ser† verificado se o BO 1.1 est† posicionado em algum 
                  registro e caso n∆o esteja Ç considerado que a query est†
                  vazia ---*/
            
            IF cReturnAux = "NOK":U THEN DO:
                IF rCurrentAux = ? THEN DO:
                    /*--- Seta estado dos bot‰es/menus ---*/
                    DO WITH FRAME fPage0:
                        ASSIGN
                            cButtonsStateAux = "":U
                            
                            &IF "{&First}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Prev}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Next}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Last}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&GoTo}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Search}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Add}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            &ENDIF
                            
                            &IF "{&Copy}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Update}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Delete}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Undo}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Cancel}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Save}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            /*tech1139 - 05/12/2006 - FO 1387.046 */
                            &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN /*alterado*/
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            &ENDIF
                            
                            &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            &ENDIF
                            
                            /*tech1139 - bot∆o Exit */ 
                            /* tech14187 - 10/04/2007 - 1468007 */
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U

                            /*tech1139 - bot∆o Help */
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                
                            /*tech1139 - 05/12/2006 - FO 1387.046 */
                            .
                        
                    END.
                    
                    /*--- Seta estado dos bot‰es/menu ---*/
                    RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
                    
                    /*--- Seta cursor do mouse para normal ---*/
                    SESSION:SET-WAIT-STATE("":U).
                    
                    RETURN "OK":U.
                END.
                
                /*--- Inicializa janela de mensagens de erro ---*/
                {method/showmessage.i1}
                
                /*--- Seta cursor do mouse para normal ---*/
                SESSION:SET-WAIT-STATE("":U).
                
                /*--- Transfere temp-table RowErrors para a janela de mensagens de erro ---*/
                {method/showmessage.i2}
                /****
                 TECH14187 - 1538059
                 Problema da ShowMessage Modal
                 ****/
                &IF "{&Modal}":U = "":U &THEN
                    WAIT-FOR CLOSE OF hShowMsg. /*Alterado por tech30713 - 07/09/2006 - Ao eliminar um regostro n∆o estava 
                                                 aparecendo a mensagem de erro. FO:1375930 */
                &ENDIF

                /*FO 1336930 - tech1007 - Alteracao para eliminar a janela da ut-msgs - 15/08/2006 */
                /*C¢digo destinado a tirar a ShowMessage da mem¢ria */
                {method/showmessage.i3}
                /*FO 1336930 - tech1007 - Alteracao para eliminar a janela da ut-msgs - 15/08/2006 */
                
                RETURN "NOK":U.
            END.
        &ELSE
            IF cReturnAux = "NOK":U THEN DO:
                /*--- Retorna temp-table RowErrors do DBO ---*/
                RUN getRowErrors IN {&hDBOTable} (OUTPUT TABLE RowErrors).               
                
                /*--- Descriá∆o dos erros:
                      3: Tabela {&TableLabel} n∆o dispon°vel
                      6: Query est† fechada
                      8: Query est† vazia 
                      10: Query est† posicionada no £ltimo registro ---*/
                
                IF CAN-FIND(FIRST RowErrors 
                            WHERE (RowErrors.ErrorType = "INTERNAL":U) AND
                                  (RowErrors.ErrorNumber = 8)) THEN DO:
                    /*--- Seta estado dos bot‰es/menus ---*/
                    DO WITH FRAME fPage0:
                        ASSIGN
                            cButtonsStateAux = "":U
                            
                            &IF "{&First}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Prev}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Next}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Last}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&GoTo}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Search}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Add}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            &ENDIF
                            
                            &IF "{&Copy}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Update}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Delete}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Undo}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Cancel}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Save}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            /*tech1139 - 05/12/2006 - FO 1387.046 */
                            &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            &ENDIF
                            
                            &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            &ENDIF
                            
                            /* tech14187 - 10/04/2007 - 1468007 */
                            /*tech1139 - bot∆o Exit */
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U

                            /*tech1139 - bot∆o Help */
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            
                            /*tech1139 - 05/12/2006 - FO 1387.046 */
                            .
                        
                    END.
                    
                    /*--- Seta estado dos bot‰es/menu ---*/
                    RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
                    
                    /*--- Seta cursor do mouse para normal ---*/
                    SESSION:SET-WAIT-STATE("":U).
                    
                    RETURN "OK":U.
                END.
                
                IF CAN-FIND(FIRST RowErrors 
                            WHERE (RowErrors.ErrorType = "INTERNAL":U) AND
                                  (RowErrors.ErrorNumber = 10)) THEN DO:
                    /*--- Seta estado dos bot‰es/menus ---*/
                    DO WITH FRAME fPage0:
                        ASSIGN
                            cButtonsStateAux = "":U
                            
                            &IF "{&First}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            &ENDIF
                            
                            &IF "{&Prev}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            &ENDIF
                            
                            &IF "{&Next}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Last}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&GoTo}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            &ENDIF
                            
                            &IF "{&Search}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            &ENDIF
                            
                            &IF "{&Add}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            &ENDIF
                            
                            &IF "{&Copy}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            &ENDIF
                            
                            &IF "{&Update}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            &ENDIF
                            
                            &IF "{&Delete}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            &ENDIF
                            
                            &IF "{&Undo}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Cancel}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Save}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            /*tech1139 - 05/12/2006 - FO 1387.046 */
                            &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            &ENDIF
                            
                            &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            &ENDIF
                            
                            /*tech1139 - bot∆o Exit */
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U

                            /*tech1139 - bot∆o Help */
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            
                            /*tech1139 - 05/12/2006 - FO 1387.046 */                
                            .
                    END.
                    
                    /*--- Seta estado dos bot‰es/menu ---*/
                    RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
                    
                    /*--- Seta cursor do mouse para normal ---*/
                    SESSION:SET-WAIT-STATE("":U).
                    
                    RETURN "OK":U.
                END.
                
                IF CAN-FIND(FIRST RowErrors 
                            WHERE (RowErrors.ErrorType = "INTERNAL":U) AND
                                  (RowErrors.ErrorNumber = 3 OR
                                   RowErrors.ErrorNumber = 6)) THEN DO:
                    /*--- Seta estado dos bot‰es/menus ---*/
                    DO WITH FRAME fPage0:
                        ASSIGN
                            cButtonsStateAux = "":U
                            
                            &IF "{&First}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Prev}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Next}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Last}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&GoTo}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Search}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Add}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            &ENDIF
                            
                            &IF "{&Copy}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Update}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Delete}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Undo}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Cancel}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            &IF "{&Save}":U = "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ENDIF
                            
                            /*tech1139 - 05/12/2006 - FO 1387.046 */
                            &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            &ENDIF
                            
                            &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            &ENDIF
                            
                            /* tech14187 - 10/04/2007 - 1468007*/
                            /*tech1139 - bot∆o Exit */
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                                                      
                            /*tech1139 - bot∆o Help */
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            
                            /*tech1139 - 05/12/2006 - FO 1387.046 */
                            .
                    END.
                    
                    /*--- Seta estado dos bot‰es/menu ---*/
                    RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
                    
                    /*--- Seta cursor do mouse para normal ---*/
                    SESSION:SET-WAIT-STATE("":U).
                    
                    RETURN "OK":U.
                END.
                
                /*--- Inicializa janela de mensagens de erro ---*/
                {method/showmessage.i1}

                /*--- Seta cursor do mouse para normal ---*/
                SESSION:SET-WAIT-STATE("":U).                

                /*--- Transfere temp-table RowErrors para a janela de mensagens de erro ---*/
                {method/showmessage.i2}
                /****
                 TECH14187 - 1538059
                 Problema da ShowMessage Modal
                 ****/
                &IF "{&Modal}":U = "":U &THEN
                    WAIT-FOR CLOSE OF hShowMsg. /*Alterado por tech30713 - 07/09/2006 - Ao eliminar um regostro n∆o estava 
                                                 aparecendo a mensagem de erro. FO:1375930 */
                &ENDIF
                
                
                /*FO 1336930 - tech1007 - Alteracao para eliminar a janela da ut-msgs - 15/08/2006 */
                /*C¢digo destinado a tirar a ShowMessage da mem¢ria */
                {method/showmessage.i3}
                /*FO 1336930 - tech1007 - Alteracao para eliminar a janela da ut-msgs - 15/08/2006 */

                RETURN "NOK":U.
            END.
        &ENDIF
        
    END.
    
    /*--- Seta cursor do mouse para espera ---*/
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    IF AVAILABLE {&ttTable} THEN 
         assign epc-rowid = {&ttTable}.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customizaá∆o (before/after) ---*/
    {method/custom.i &Event="AFTER-DELETE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable}"
                     &RowidTable="epc-rowid"}
    
    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFirst) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getFirst Method-Library 
PROCEDURE getFirst :
/*------------------------------------------------------------------------------
  Purpose:     Posiciona no primeiro registro
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE cButtonsStateAux AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturnAux       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE rCurrentAux      AS ROWID     NO-UNDO.
   

    /*--- Seta cursor do mouse para espera ---*/
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- N∆o h† a necessidade de limpar a temp-table RowErrors ---*/
    &ELSE
        /*--- Limpa temp-table RowErrors no DBO ---*/
        RUN emptyRowErrors IN {&hDBOTable}.
    &ENDIF
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Posiciona BO 1.1 no primeiro registro ---*/
        RUN findFirst IN {&hDBOTable}.
    &ELSE
        /*--- Posiciona DBO no primeiro registro ---*/
        RUN getFirst IN {&hDBOTable}.
    &ENDIF
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Quando for utilizado BO 1.1 n∆o ser† feito o tratamento dos erros 
              PorÇm ser† verificado se o BO 1.1 est† posicionado em algum 
              registro e caso n∆o esteja Ç considerado que a query est†
              vazia ---*/
        RUN getRowid IN {&hDBOTable} (OUTPUT rCurrentAux).
        IF rCurrentAux = ? THEN
            ASSIGN cReturnAux = "NOK":U.
    &ELSE
        /*--- Seta vari†vel cReturnAux com o conte£do do RETURN-VALUE ---*/
        ASSIGN cReturnAux = RETURN-VALUE.
    &ENDIF
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Retorna registro da temp-table {&ttTable} do BO 1.1 ---*/
        RUN getCurrent IN {&hDBOTable} (OUTPUT TABLE {&ttTable}).
    &ELSE
        /*--- Retorna registro da temp-table {&ttTable} do DBO ---*/
        RUN getRecord IN {&hDBOTable} (OUTPUT TABLE {&ttTable}).
    &ENDIF
    
    /*--- Atualize dados em tela ---*/
    RUN displayFields IN THIS-PROCEDURE.
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Quando for utilizado BO 1.1 n∆o ser† feito o tratamento dos 
              erros, estes somente s∆o exibidos em tela 
              PorÇm ser† verificado se o BO 1.1 est† posicionado em algum 
              registro e caso n∆o esteja Ç considerado que a query est†
              vazia ---*/
        
        IF cReturnAux = "NOK":U THEN DO:
            /*--- Seta estado dos bot‰es/menus ---*/
            DO WITH FRAME fPage0:
                ASSIGN
                    cButtonsStateAux = "":U
                    
                    &IF "{&First}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                    &ENDIF
                    
                    &IF "{&Prev}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                    &ENDIF
                    
                    &IF "{&Next}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                    &ENDIF
                    
                    &IF "{&Last}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                    &ENDIF
                    
                    &IF "{&GoTo}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                    &ENDIF
                    
                    &IF "{&Search}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                    &ENDIF
                    
                    &IF "{&Add}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    &ENDIF
                    
                    &IF "{&Copy}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                    &ENDIF
                    
                    &IF "{&Update}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                    &ENDIF
                    
                    &IF "{&Delete}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                    &ENDIF
                    
                    &IF "{&Undo}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                    &ENDIF
                    
                    &IF "{&Cancel}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                    &ENDIF
                    
                    &IF "{&Save}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                    &ENDIF
                    
                    /*tech1139 - 05/12/2006 - FO 1387.046 */
                    &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN /*alterado*/
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    &ENDIF
                    
                    &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    &ENDIF
                                        
                    /* tech14187 - 10/04/2007 - 1468007*/
                    /*tech1139 - bot∆o Exit */
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U

                    /*tech1139 - bot∆o Help */
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    
                    /*tech1139 - 05/12/2006 - FO 1387.046 */
                    .
                
            END.
            
            /*--- Seta estado dos bot‰es/menu ---*/
            RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
            
            /*--- Seta cursor do mouse para normal ---*/
            SESSION:SET-WAIT-STATE("":U).

            /*--- Alterado por Valdir (tech264), quando o objeto com focus ficar desabilitado transfere o focus para a FRAME ---*/
            /*--- Transfere o Focus para a Frame ---*/
            RUN transfereFocoFrame IN THIS-PROCEDURE.
            
            RETURN "NOK":U.
        END.
    &ELSE
        IF cReturnAux = "NOK":U THEN DO:
            /*--- Retorna temp-table RowErrors do DBO ---*/
            RUN getRowErrors IN {&hDBOTable} (OUTPUT TABLE RowErrors).
            
            /*--- Descriá∆o dos erros:
                  3: Tabela {&TableLabel} n∆o dispon°vel
                  6: Query est† fechada
                  8: Query est† vazia ---*/
            IF CAN-FIND(FIRST RowErrors 
                        WHERE (RowErrors.ErrorType = "INTERNAL":U) AND
                              (RowErrors.ErrorNumber = 3 OR
                               RowErrors.ErrorNumber = 6 OR
                               RowErrors.ErrorNumber = 8 )) THEN DO:
                /*--- Seta estado dos bot‰es/menus ---*/
                DO WITH FRAME fPage0:
                    ASSIGN
                        cButtonsStateAux = "":U
                        
                        &IF "{&First}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Prev}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Next}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Last}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&GoTo}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Search}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Add}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&Copy}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Update}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Delete}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Undo}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Cancel}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Save}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN /*alterado*/
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF

                        /* tech14187 - 10/04/2007 - 1468007*/
                        /*tech1139 - bot∆o Exit */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U

                        /*tech1139 - bot∆o Help */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        /*tech1139 - 05/12/2006 - FO 1387.046 */          
                        .

                END.
                
                /*--- Seta estado dos bot‰es/menu ---*/
                RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
                
                /*--- Seta cursor do mouse para normal ---*/
                SESSION:SET-WAIT-STATE("":U).
                
                /*--- Alterado por Valdir (tech264), quando o objeto com focus ficar desabilitado transfere o focus para a FRAME ---*/
                /*--- Transfere o Focus para a Frame ---*/
                RUN transfereFocoFrame IN THIS-PROCEDURE.

                RETURN "NOK":U.
            END.
            
            /*--- Seta cursor do mouse para normal ---*/
            SESSION:SET-WAIT-STATE("":U).
            
            RETURN "NOK":U.
        END.
    &ENDIF
    
    /*--- Seta estado dos bot‰es/menus ---*/
    DO WITH FRAME fPage0:

        ASSIGN
            cButtonsStateAux = "":U
            
            &IF "{&First}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Prev}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Next}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Last}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&GoTo}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Search}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Add}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Copy}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Update}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Delete}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Undo}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Cancel}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Save}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            /*tech1139 - 05/12/2006 - FO 1387.046 */
            &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF

            &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            /*tech1139 - bot∆o Exit */                                      
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            /*tech1139 - bot∆o Help */
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
            /*tech1139 - 05/12/2006 - FO 1387.046 */
            .
    END.
    
    /*--- Seta estado dos bot‰es/menu ---*/
    RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
    
    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    
    /*--- Alterado por Valdir (tech264), quando o objeto com focus ficar desabilitado transfere o focus para a FRAME ---*/
    /*--- Transfere o Focus para a Frame ---*/
    
    /*alteracao feita por anderson em 09/06/2001 para verificar
     se o botoes de navegacao estao definidos no programa*/
    &IF "{&FIRST}":U = "YES":U &THEN
        IF btFirst:SENSITIVE = NO THEN
            RUN transfereFocoFrame IN THIS-PROCEDURE.
    &ENDIF 
         
   RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLast) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getLast Method-Library 
PROCEDURE getLast :
/*------------------------------------------------------------------------------
  Purpose:     Posiciona no £ltimo registro
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE cButtonsStateAux AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturnAux       AS CHARACTER NO-UNDO.
    
    /*--- Seta cursor do mouse para espera ---*/
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- N∆o h† a necessidade de limpar a temp-table RowErrors ---*/
    &ELSE
        /*--- Limpa temp-table RowErrors no DBO ---*/
        RUN emptyRowErrors IN {&hDBOTable}.
    &ENDIF
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Posiciona BO 1.1 no £ltimo registro ---*/
        RUN findLast IN {&hDBOTable}.
    &ELSE
        /*--- Posiciona DBO no £ltimo registro ---*/
        RUN getLast IN {&hDBOTable}.
    &ENDIF
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Quando for utilizado BO 1.1 n∆o ser† feito o tratamento dos erros ---*/
    &ELSE
        /*--- Seta vari†vel cReturnAux com o conte£do do RETURN-VALUE ---*/
        ASSIGN cReturnAux = RETURN-VALUE.
    &ENDIF
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Retorna registro da temp-table {&ttTable} do BO 1.1 ---*/
        RUN getCurrent IN {&hDBOTable} (OUTPUT TABLE {&ttTable}).
    &ELSE
        /*--- Retorna registro da temp-table {&ttTable} do DBO ---*/
        RUN getRecord IN {&hDBOTable} (OUTPUT TABLE {&ttTable}).
    &ENDIF
    
    /*--- Atualize dados em tela ---*/
    RUN displayFields IN THIS-PROCEDURE.
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Quando for utilizado BO 1.1 n∆o ser† feito o tratamento dos erros ---*/
    &ELSE
        IF cReturnAux = "NOK":U THEN DO:
            /*--- Retorna temp-table RowErrors do DBO ---*/
            RUN getRowErrors IN {&hDBOTable} (OUTPUT TABLE RowErrors).
            
            /*--- Descriá∆o dos erros:
                  3: Tabela {&TableLabel} n∆o dispon°vel
                  6: Query est† fechada
                  8: Query est† vazia ---*/
            IF CAN-FIND(FIRST RowErrors 
                        WHERE (RowErrors.ErrorType = "INTERNAL":U) AND
                              (RowErrors.ErrorNumber = 3 OR
                               RowErrors.ErrorNumber = 6 OR
                               RowErrors.ErrorNumber = 8 )) THEN DO:
                /*--- Seta estado dos bot‰es/menus ---*/
                DO WITH FRAME fPage0:
                    ASSIGN
                        cButtonsStateAux = "":U
                        
                        &IF "{&First}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Prev}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Next}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Last}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&GoTo}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Search}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Add}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&Copy}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Update}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Delete}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Undo}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Cancel}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Save}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN /*alterado*/
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        /* tech14187 - 10/04/2007 - 1468007 */
                        /*tech1139 - bot∆o Exit */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        /*tech1139 - bot∆o Help */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        .
                END.
                
                /*--- Seta estado dos bot‰es/menu ---*/
                RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
                
                /*--- Seta cursor do mouse para normal ---*/
                SESSION:SET-WAIT-STATE("":U).
              
                /*--- Alterado por Valdir (tech264), quando o objeto com focus ficar desabilitado transfere o focus para a FRAME ---*/
                /*--- Transfere o Focus para a Frame ---*/
                RUN transfereFocoFrame IN THIS-PROCEDURE.
              
                RETURN "NOK":U.
            END.
            
            /*--- Seta cursor do mouse para normal ---*/
            SESSION:SET-WAIT-STATE("":U).
            
            RETURN "NOK":U.
        END.
    &ENDIF
    
    /*--- Seta estado dos bot‰es/menus ---*/
    DO WITH FRAME fPage0:
        ASSIGN
            cButtonsStateAux = "":U
            
            &IF "{&First}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Prev}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Next}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Last}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&GoTo}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Search}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Add}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Copy}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Update}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Delete}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Undo}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Cancel}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Save}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            /*tech1139 - 05/12/2006 - FO 1387.046 */
            &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN /*alterado*/
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF

            /*tech1139 - bot∆o Exit */            
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U

            /*tech1139 - bot∆o Help */
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            
           /*tech1139 - 05/12/2006 - FO 1387.046 */
           .
    END.
    
    /*--- Seta estado dos bot‰es/menu ---*/
    RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
    
    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).

    /*--- Alterado por Valdir (tech264), quando o objeto com focus ficar desabilitado transfere o focus para a FRAME ---*/
    /*--- Transfere o Focus para a Frame ---*/
    
    /*alteracao feita por anderson em 09/06/2001 para verificar
     se o botoes de navegacao estao definidos no programa*/
    &IF "{&LAST}":U = "YES":U &THEN
        IF btLast:SENSITIVE = NO THEN
            RUN transfereFocoFrame IN THIS-PROCEDURE.
    &ENDIF
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getNext) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getNext Method-Library 
PROCEDURE getNext :
/*------------------------------------------------------------------------------
  Purpose:     Posiciona no pr¢ximo registro
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE cButtonsStateAux AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturnAux       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE rCurrentAux      AS ROWID     NO-UNDO.
    
    /*--- Seta cursor do mouse para espera ---*/
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- N∆o h† a necessidade de limpar a temp-table RowErrors ---*/
    &ELSE
        /*--- Limpa temp-table RowErrors no DBO ---*/
        RUN emptyRowErrors IN {&hDBOTable}.
    &ENDIF
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Posiciona BO 1.1 no pr¢ximo registro ---*/
        RUN findNext IN {&hDBOTable} (OUTPUT cReturnAux).
        
        /*--- Caso o retorno seja diferente de "" a vari†vel cReturnAux Ç setada
              com o valor NOK sen∆o com o valor OK ---*/
        IF cReturnAux <> "":U THEN
            ASSIGN cReturnAux = "NOK":U.
        ELSE
            ASSIGN cReturnAux = "OK":U.
    &ELSE
        /*--- Posiciona DBO no pr¢ximo registro ---*/
        RUN getNext IN {&hDBOTable}.
    &ENDIF
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Quando for utilizado BO 1.1 n∆o ser† feito o tratamento dos erros ---*/
    &ELSE
        /*--- Seta vari†vel cReturnAux com o conte£do do RETURN-VALUE ---*/
        ASSIGN cReturnAux = RETURN-VALUE.
    &ENDIF
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Retorna registro da temp-table {&ttTable} do BO 1.1 ---*/
        RUN getCurrent IN {&hDBOTable} (OUTPUT TABLE {&ttTable}).
    &ELSE
        /*--- Retorna registro da temp-table {&ttTable} do DBO ---*/
        RUN getRecord IN {&hDBOTable} (OUTPUT TABLE {&ttTable}).
    &ENDIF
    
    /*--- Atualize dados em tela ---*/
    RUN displayFields IN THIS-PROCEDURE.
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Quando for utilizado BO 1.1 n∆o ser† feito o tratamento dos erros 
              Mas caso a vari†vel cReturnAux tenha o valor NOK, ser† considerado 
              que o BO 1.1 est† posicionado no £ltimo registro ---*/
        IF cReturnAux = "NOK":U THEN DO:
            /*--- Seta vari†vel rCurrentAux com o valor da vari†vel rCurrent ---*/
            ASSIGN rCurrentAux = rCurrent.
            
            /*--- Ao tentar-se navegar ap¢s o £ltimo registro, o BO 1.1 perde o
                  posicionamento
                  Sendo assim, ser† posicionado com base no rowid corrente ---*/
            RUN findRowid IN {&hDBOTable} (INPUT rCurrentAux).
            
            /*--- Seta estado dos bot‰es/menus ---*/
            DO WITH FRAME fPage0:
                ASSIGN
                    cButtonsStateAux = "":U
                    
                    &IF "{&First}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    &ENDIF
                    
                    &IF "{&Prev}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    &ENDIF
                    
                    &IF "{&Next}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                    &ENDIF
                    
                    &IF "{&Last}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                    &ENDIF
                    
                    &IF "{&GoTo}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    &ENDIF
                    
                    &IF "{&Search}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    &ENDIF
                    
                    &IF "{&Add}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    &ENDIF
                    
                    &IF "{&Copy}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    &ENDIF
                    
                    &IF "{&Update}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    &ENDIF
                    
                    &IF "{&Delete}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    &ENDIF
                    
                    &IF "{&Undo}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                    &ENDIF
                    
                    &IF "{&Cancel}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                    &ENDIF
                    
                    &IF "{&Save}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                    &ENDIF
                    
                    /*tech1139 - 05/12/2006 - FO 1387.046 */
                    &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN /*alterado*/
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    &ENDIF
                    
                    &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    &ENDIF
                    
                    /*tech1139 - bot∆o Exit */                    
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U

                    /*tech1139 - bot∆o Help */
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U

                    /*tech1139 - 05/12/2006 - FO 1387.046 */
                    .
            END.
            
            /*--- Seta estado dos bot‰es/menu ---*/
            RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
            
            /*--- Seta cursor do mouse para normal ---*/
            SESSION:SET-WAIT-STATE("":U).
            
            /*--- Alterado por Valdir (tech264), quando o objeto com focus ficar desabilitado transfere o focus para a FRAME ---*/
            /*--- Transfere o Focus para a Frame ---*/
            RUN transfereFocoFrame IN THIS-PROCEDURE.

            RETURN "NOK":U.
        END.
    &ELSE
        IF cReturnAux = "NOK":U THEN DO:
            /*--- Retorna temp-table RowErrors do DBO ---*/
            RUN getRowErrors IN {&hDBOTable} (OUTPUT TABLE RowErrors).
            
            /*--- Descriá∆o dos erros:
                  3: Tabela {&TableLabel} n∆o dispon°vel
                  6: Query est† fechada
                  8: Query est† vazia 
                  10: Query est† posicionada no £ltimo registro ---*/
            
            IF CAN-FIND(FIRST RowErrors 
                        WHERE (RowErrors.ErrorType = "INTERNAL":U) AND
                              (RowErrors.ErrorNumber = 10)) THEN DO:
                /*--- Seta estado dos bot‰es/menus ---*/
                DO WITH FRAME fPage0:
                    ASSIGN
                        cButtonsStateAux = "":U
                        
                        &IF "{&First}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&Prev}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&Next}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Last}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&GoTo}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&Search}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&Add}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&Copy}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&Update}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&Delete}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&Undo}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Cancel}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Save}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN /*alterado*/
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        /*tech1139 - bot∆o Exit */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U

                        /*tech1139 - bot∆o Help */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                                                  
                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        .
                END.
                
                /*--- Seta estado dos bot‰es/menu ---*/
                RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
                
                /*--- Seta cursor do mouse para normal ---*/
                SESSION:SET-WAIT-STATE("":U).
                
                /*--- Alterado por Valdir (tech264), quando o objeto com focus ficar desabilitado transfere o focus para a FRAME ---*/
                /*--- Transfere o Focus para a Frame ---*/
                RUN transfereFocoFrame IN THIS-PROCEDURE.

                RETURN "NOK":U.
            END.
            
            IF CAN-FIND(FIRST RowErrors 
                        WHERE (RowErrors.ErrorType = "INTERNAL":U) AND
                              (RowErrors.ErrorNumber = 3 OR
                               RowErrors.ErrorNumber = 8 OR
                               RowErrors.ErrorNumber = 6)) THEN DO:
                /*--- Seta estado dos bot‰es/menus ---*/
                DO WITH FRAME fPage0:
                    ASSIGN
                        cButtonsStateAux = "":U
                        
                        &IF "{&First}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Prev}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Next}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Last}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&GoTo}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Search}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Add}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&Copy}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Update}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Delete}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Undo}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Cancel}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Save}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN /*alterado*/
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        /* tech14187 - 10/04/2007 - 1468007 */
                        /*tech1139 - bot∆o Exit */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U

                        /*tech1139 - bot∆o Help */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        .
                END.
                
                /*--- Seta estado dos bot‰es/menu ---*/
                RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
                
                /*--- Seta cursor do mouse para normal ---*/
                SESSION:SET-WAIT-STATE("":U).
                
                /*--- Alterado por Valdir (tech264), quando o objeto com focus ficar desabilitado transfere o focus para a FRAME ---*/
                /*--- Transfere o Focus para a Frame ---*/
                RUN transfereFocoFrame IN THIS-PROCEDURE.

                RETURN "NOK":U.
            END.
            
            /*--- Seta cursor do mouse para normal ---*/
            SESSION:SET-WAIT-STATE("":U).
            
            RETURN "NOK":U.
        END.
    &ENDIF
    
    /*--- Seta estado dos bot‰es/menus ---*/
    DO WITH FRAME fPage0:
        ASSIGN
            cButtonsStateAux = "":U
            
            &IF "{&First}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Prev}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Next}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Last}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&GoTo}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Search}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Add}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Copy}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Update}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Delete}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Undo}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Cancel}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Save}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            /*tech1139 - 05/12/2006 - FO 1387.046 */
            &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN /*alterado*/
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN
                 cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U 
            &ENDIF
            
            /*tech1139 - bot∆o Exit */
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U 

            /*tech1139 - bot∆o Help */
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U 

            /*tech1139 - 05/12/2006 - FO 1387.046 */
            .
    END.
    
    /*--- Seta estado dos bot‰es/menu ---*/
    RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
    
    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).

    /*--- Alterado por Valdir (tech264), quando o objeto com focus ficar desabilitado transfere o focus para a FRAME ---*/
    /*--- Transfere o Focus para a Frame ---*/
    
    /*alteracao feita por anderson em 09/06/2001 para verificar
     se o botoes de navegacao estao definidos no programa*/
    &IF "{&NEXT}":U = "YES":U &THEN
        IF btNext:SENSITIVE = NO THEN
            RUN transfereFocoFrame IN THIS-PROCEDURE.
    &ENDIF
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPrev) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPrev Method-Library 
PROCEDURE getPrev :
/*------------------------------------------------------------------------------
  Purpose:     Posiciona no registro anterior
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE cButtonsStateAux AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturnAux       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE rCurrentAux      AS ROWID     NO-UNDO.
    
    /*--- Seta cursor do mouse para espera ---*/
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- N∆o h† a necessidade de limpar a temp-table RowErrors ---*/
    &ELSE
        /*--- Limpa temp-table RowErrors no DBO ---*/
        RUN emptyRowErrors IN {&hDBOTable}.
    &ENDIF
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Posiciona BO 1.1 no registro anterior ---*/
        RUN findPrev IN {&hDBOTable} (OUTPUT cReturnAux).
        
        /*--- Caso o retorno seja diferente de "" a vari†vel cReturnAux Ç setada
              com o valor NOK ---*/
        IF cReturnAux <> "":U THEN
            ASSIGN cReturnAux = "NOK":U.
        ELSE
            ASSIGN cReturnAux = "OK":U.
    &ELSE
        /*--- Posiciona DBO no registro anterior ---*/
        RUN getPrev IN {&hDBOTable}.
    &ENDIF
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Quando for utilizado BO 1.1 n∆o ser† feito o tratamento dos erros ---*/
    &ELSE
        /*--- Seta vari†vel cReturnAux com o conte£do do RETURN-VALUE ---*/
        ASSIGN cReturnAux = RETURN-VALUE.
    &ENDIF
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Retorna registro da temp-table {&ttTable} do BO 1.1 ---*/
        RUN getCurrent IN {&hDBOTable} (OUTPUT TABLE {&ttTable}).
    &ELSE
        /*--- Retorna registro da temp-table {&ttTable} do DBO ---*/
        RUN getRecord IN {&hDBOTable} (OUTPUT TABLE {&ttTable}).
    &ENDIF
    
    /*--- Atualize dados em tela ---*/
    RUN displayFields IN THIS-PROCEDURE.
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Quando for utilizado BO 1.1 n∆o ser† feito o tratamento dos erros 
              Mas caso a vari†vel cReturnAux tenha o valor NOK, ser† considerado 
              que o BO 1.1 est† posicionado no £ltimo registro ---*/
        IF cReturnAux = "NOK":U THEN DO:
            /*--- Seta vari†vel rCurrentAux com o valor da vari†vel rCurrent ---*/
            ASSIGN rCurrentAux = rCurrent.
            
            /*--- Ao tentar-se navegar ap¢s o £ltimo registro, o BO 1.1 perde o
                  posicionamento
                  Sendo assim, ser† posicionado com base no rowid corrente ---*/
            RUN findRowid IN {&hDBOTable} (INPUT rCurrentAux).
            
            /*--- Seta estado dos bot‰es/menus ---*/
            DO WITH FRAME fPage0:
                ASSIGN
                    cButtonsStateAux = "":U
                    
                    &IF "{&First}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                    &ENDIF
                    
                    &IF "{&Prev}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                    &ENDIF
                    
                    &IF "{&Next}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    &ENDIF
                    
                    &IF "{&Last}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    &ENDIF
                    
                    &IF "{&GoTo}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    &ENDIF
                    
                    &IF "{&Search}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    &ENDIF
                    
                    &IF "{&Add}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    &ENDIF
                    
                    &IF "{&Copy}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    &ENDIF
                    
                    &IF "{&Update}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    &ENDIF
                    
                    &IF "{&Delete}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    &ENDIF
                    
                    &IF "{&Undo}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                    &ENDIF
                    
                    &IF "{&Cancel}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                    &ENDIF
                    
                    &IF "{&Save}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                    &ENDIF
                    
                    /*tech1139 - 05/12/2006 - FO 1387.046 */
                    &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN /*alterado*/
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    &ENDIF
                    
                    &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    &ENDIF

                    /*tech1139 - bot∆o Help */
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                    /*tech1139 - bot∆o Exit */
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    
                    /*tech1139 - 05/12/2006 - FO 1387.046 */
                    .
            END.
            
            /*--- Seta estado dos bot‰es/menu ---*/
            RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
            
            /*--- Seta cursor do mouse para normal ---*/
            SESSION:SET-WAIT-STATE("":U).
            
            /*--- Alterado por Valdir (tech264), quando o objeto com focus ficar desabilitado transfere o focus para a FRAME ---*/
            /*--- Transfere o Focus para a Frame ---*/
            RUN transfereFocoFrame IN THIS-PROCEDURE.

            RETURN "NOK":U.
        END.
    &ELSE
        IF cReturnAux = "NOK":U THEN DO:
            /*--- Retorna temp-table RowErrors do DBO ---*/
            RUN getRowErrors IN {&hDBOTable} (OUTPUT TABLE RowErrors).
            
            /*--- Descriá∆o dos erros:
                  3: Tabela {&TableLabel} n∆o dispon°vel
                  5: Funcao nao disponivel para banco de dados diferente de Progress
                  6: Query est† fechada
                  8: Query est† vazia 
                  9: Query est† posicionada no primeiro registro ---*/
            
            IF CAN-FIND(FIRST RowErrors 
                        WHERE (RowErrors.ErrorType = "INTERNAL":U) AND
                              (RowErrors.ErrorNumber = 9)) THEN DO:
                /*--- Seta estado dos bot‰es/menus ---*/
                DO WITH FRAME fPage0:
                    ASSIGN
                        cButtonsStateAux = "":U
                        
                        &IF "{&First}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Prev}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Next}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&Last}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&GoTo}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&Search}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&Add}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&Copy}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&Update}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&Delete}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&Undo}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Cancel}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Save}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN /*alterado*/
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        /*tech1139 - bot∆o Exit */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        /*tech1139 - bot∆o Help */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U

                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        .
                END.
                
                /*--- Seta estado dos bot‰es/menu ---*/
                RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
                
                /*--- Seta cursor do mouse para normal ---*/
                SESSION:SET-WAIT-STATE("":U).
                
                /*--- Alterado por Valdir (tech264), quando o objeto com focus ficar desabilitado transfere o focus para a FRAME ---*/
                /*--- Transfere o Focus para a Frame ---*/
                RUN transfereFocoFrame IN THIS-PROCEDURE.

                RETURN "NOK":U.
            END.
            
            IF CAN-FIND(FIRST RowErrors 
                        WHERE (RowErrors.ErrorType = "INTERNAL":U) AND
                              (RowErrors.ErrorNumber = 3 OR
                               RowErrors.ErrorNumber = 8 OR
                               RowErrors.ErrorNumber = 6)) THEN DO:
                /*--- Seta estado dos bot‰es/menus ---*/
                DO WITH FRAME fPage0:
                    ASSIGN
                        cButtonsStateAux = "":U
                        
                        &IF "{&First}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Prev}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Next}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Last}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&GoTo}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Search}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Add}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&Copy}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Update}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Delete}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Undo}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Cancel}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Save}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN /*alterado*/
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        /* tech14187 - 10/04/2007 - 1468007 */
                        /*tech1139 - bot∆o Help */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                                                   
                        /*tech1139 - bot∆o Exit */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        .

                END.
                
                /*--- Seta estado dos bot‰es/menu ---*/
                RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
                
                /*--- Seta cursor do mouse para normal ---*/
                SESSION:SET-WAIT-STATE("":U).
                
                /*--- Alterado por Valdir (tech264), quando o objeto com focus ficar desabilitado transfere o focus para a FRAME ---*/
                /*--- Transfere o Focus para a Frame ---*/
                RUN transfereFocoFrame IN THIS-PROCEDURE.

                RETURN "NOK":U.
            END.

            IF CAN-FIND(FIRST RowErrors 
                        WHERE RowErrors.ErrorType = "INTERNAL":U AND
                              RowErrors.ErrorNumber = 5) THEN DO:
                /*--- Seta estado dos bot‰es/menus ---*/
                DO WITH FRAME fPage0:
                    ASSIGN
                        cButtonsStateAux = "":U
                        
                        &IF "{&First}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        /* Desabilitar botao de Prev em banco diferente de Progress */
                        &IF "{&Prev}":U = "YES":U &THEN
                            &IF "{&ems_dbtype}":U = "ORACLE":U &THEN
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                            &ELSE
                                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                            &ENDIF
                        &ENDIF
                        
                        &IF "{&Next}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&Last}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&GoTo}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&Search}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&Add}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&Copy}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&Update}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&Delete}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&Undo}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Cancel}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        &IF "{&Save}":U = "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                        &ENDIF
                        
                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN /*alterado*/
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        &ENDIF
                        
                        /*tech1139 - bot∆o Exit */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U

                        /*tech1139 - bot∆o Help */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        .
                END.
                
                /*--- Seta estado dos bot‰es/menu ---*/
                RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
                
                /*--- Seta cursor do mouse para normal ---*/
                SESSION:SET-WAIT-STATE("":U).
                
                /*--- Alterado por Valdir (tech264), quando o objeto com focus ficar desabilitado transfere o focus para a FRAME ---*/
                /*--- Transfere o Focus para a Frame ---*/
                RUN transfereFocoFrame IN THIS-PROCEDURE.

                RETURN "NOK":U.
            END.
            
            /*--- Seta cursor do mouse para normal ---*/
            SESSION:SET-WAIT-STATE("":U).
            
            RETURN "NOK":U.
        END.
    &ENDIF
    
    /*--- Seta estado dos bot‰es/menus ---*/
    DO WITH FRAME fPage0:
        ASSIGN
            cButtonsStateAux = "":U
            
            &IF "{&First}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Prev}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Next}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Last}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&GoTo}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Search}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Add}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Copy}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Update}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Delete}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Undo}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Cancel}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Save}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            /*tech1139 - 05/12/2006 - FO 1387.046 */
            &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN /*ALTERADO*/
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            /*tech1139 - bot∆o Exit */
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                
            /*tech1139 - bot∆o Help */
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                                      
            /*tech1139 - 05/12/2006 - FO 1387.046 */

        .
    END.
    
    /*--- Seta estado dos bot‰es/menu ---*/
    RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
    
    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    
    /*--- Alterado por Valdir (tech264), quando o objeto com focus ficar desabilitado transfere o focus para a FRAME ---*/
    /*--- Transfere o Focus para a Frame ---*/
    
    /*alteracao feita por anderson em 09/06/2001 para verificar
     se o botoes de navegacao estao definidos no programa*/
    &IF "{&PREV}":U = "YES":U &THEN
        IF btPrev:SENSITIVE = NO THEN
            RUN transfereFocoFrame IN THIS-PROCEDURE.
    &ENDIF

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-saveRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveRecord Method-Library 
PROCEDURE saveRecord :
/*------------------------------------------------------------------------------
  Purpose:     Salva modificaá‰es efetuadas
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cButtonsStateAux AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturnAux       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE rCurrentAux      AS ROWID     NO-UNDO.

    /* Adicionado por Anderson(tech540) para validaá∆o de campos date e char devido limitaá∆o banco SQL 
    em 29/01/2003*/
    &IF "{&ems_dbtype}":U = "MSS":U &THEN
        {utp/ut-verdatamss.i}
    &ENDIF
    /*fim alteraá∆o Anderson 29/01/2003*/

    /*--- Seta cursor do mouse para espera ---*/
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    IF AVAILABLE {&ttTable} THEN 
         assign epc-rowid = {&ttTable}.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customizaá∆o (before/after) ---*/
    {method/custom.i &Event="BEFORE-ASSIGN"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable}"
                     &RowidTable="epc-rowid"}
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- N∆o h† a necessidade de limpar a temp-table RowErrors, pois para
              o BO 1.1 a temp-table Ç limpa pelo pr¢prio mÇtodo de eliminaá∆o ---*/
    &ELSE
        /*--- Limpa temp-table RowErrors no DBO ---*/
        RUN emptyRowErrors IN {&hDBOTable}.
    &ENDIF
    
    /*--- Salva os campos da temp-table {&ttTable} ---*/
    RUN saveFields IN THIS-PROCEDURE.
    IF RETURN-VALUE = "NOK":U THEN
       RETURN "NOK":U.

    
    /*--- Executar mÇtodo validateRecord, caso exista ---*/
    IF THIS-PROCEDURE:GET-SIGNATURE("validateRecord":U) <> "":U THEN DO:
        RUN validateRecord IN THIS-PROCEDURE NO-ERROR.
        IF RETURN-VALUE = "NOK":U THEN
            RETURN "NOK":U.
    END.
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- N∆o h† a necessidade de transferir a temp-table {&ttTable} para o
              BO 1.1, pois o mÇtodo validateCreate e validateUpdate j† o fazem ---*/
    &ELSE
        /*--- Transfere temp-table {&ttTable} para o DBO ---*/
        RUN setRecord IN {&hDBOTable} (INPUT TABLE {&ttTable}).
    &ENDIF
    
    CASE cAction:
        /*--- Verifica se est† sendo feita uma inclus∆o/c¢pia ---*/
        WHEN "ADD":U OR WHEN "COPY":U THEN
            /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
            &IF DEFINED(DBOVersion) <> 0 &THEN
                DO:
                    /*--- Cria um novo registro no BO 1.1 ---*/
                    RUN validateCreate IN {&hDBOTable} (INPUT TABLE {&ttTable},
                                                        OUTPUT TABLE RowErrors,
                                                        OUTPUT rCurrentAux).
                    
                    /*--- Caso tenha ocorrido erros a vari†vel cReturnAux Ç setada 
                          com o valor NOK sen∆o com o valor OK ---*/
                    IF CAN-FIND(FIRST RowErrors) THEN
                        ASSIGN cReturnAux = "NOK":U.
                    ELSE DO:
                        /*--- Reabre queries dos DBOs, a fim incluir o novo registro
                              no result-list da query ---*/
                        RUN initializeDBOs IN THIS-PROCEDURE.
                        
                        /*--- Posiciona o programa DBO no registro recÇm criado ---*/
                        RUN findRowid IN {&hDBOTable} (INPUT rCurrentAux).
                        
                        ASSIGN cReturnAux = "OK":U.
                    END.
                END.
            &ELSE
                /*--- Cria um novo registro ---*/
                RUN createRecord IN {&hDBOTable}.
            &ENDIF
        
        /*--- Verifica se est† sendo feita uma alteraá∆o ---*/
        WHEN "UPDATE":U THEN
            /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
            &IF DEFINED(DBOVersion) <> 0 &THEN
                DO:
                    /*--- Altera o registro corrente do BO 1.1 ---*/
                    RUN validateUpdate IN {&hDBOTable} (INPUT TABLE {&ttTable},
                                                        INPUT {&ttTable}.r-Rowid,
                                                        OUTPUT TABLE RowErrors).
                    
                    /*--- Caso tenha ocorrido erros a vari†vel cReturnAux Ç setada 
                          com o valor NOK sen∆o com o valor OK ---*/
                    IF CAN-FIND(FIRST RowErrors) THEN
                        ASSIGN cReturnAux = "NOK":U.
                    ELSE
                        ASSIGN cReturnAux = "OK":U.
                END.
            &ELSE
                /*--- Altera o registro corrente do DBO ---*/
                RUN updateRecord IN {&hDBOTable}.
            &ENDIF
    END CASE.
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- N∆o h† a necessidade de fazer o tratamento do RETURN-VALUE para
              BO 1.1 e, ainda, a vari†vel cReturnAux Ç setada anteriormente ---*/
    &ELSE
        /*--- Atualiza vari†vel cReturnAux com o conte£do do RETURN-VALUE ---*/
        ASSIGN cReturnAux = RETURN-VALUE.
    &ENDIF
    
    /*--- Verifica ocorrància de erros durante a gravaá∆o ---*/
    IF cReturnAux = "NOK":U THEN DO:
        /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
        &IF DEFINED(DBOVersion) <> 0 &THEN
            /*--- N∆o h† a necessidade de retornar a temp-table RowErrors para
                  o BO 1.1, pois isto j† foi feito anteriormente ---*/
        &ELSE
            /*--- Retorna temp-table RowErrors do DBO ---*/
            RUN getRowErrors IN {&hDBOTable} (OUTPUT TABLE RowErrors).
        &ENDIF
        
        /*--- Inicializa tela de mensagens de erros ---*/
        {method/showmessage.i1}
        
        /*--- Seta cursor do mouse para normal ---*/
        SESSION:SET-WAIT-STATE("":U).
        
        /*Logica inserida por Anderson para quando houver erro 3 ou 34
        n∆o se mostrar nenhum outro erro de validaá∆o dos boÔs*/
        IF CAN-FIND(RowErrors WHERE RowErrors.ErrorNumber = 3 AND
                                     RowErrors.Errortype = "INTERNAL":U AND
                                     RowErrors.ErrorSubType = "ERROR":U) OR
            CAN-FIND(RowErrors WHERE RowErrors.ErrorNumber = 34 AND
                                     RowErrors.Errortype = "INTERNAL":U AND
                                     RowErrors.ErrorSubType = "ERROR":U) THEN DO:

            FOR EACH RowErrors WHERE RowErrors.ErrorNumber <> 3 AND 
                                     RowErrors.ErrorNumber <> 34 NO-LOCK:
                DELETE rowErrors.                                
            END.

        END.
        /*fim da alteraá∆o Anderson*/

        
        /*--- Transfere temp-table RowErrors para a tela de mensagens de erros ---*/
        {method/showmessage.i2}

        /****
         TECH14187 - 1538059
         Problema da ShowMessage Modal
         ****/
        &IF "{&Modal}":U = "":U &THEN
            WAIT-FOR CLOSE OF hShowMsg. 
        &ENDIF

         /*Alterado por Anderson para tratar o erro de registro eliminado por outro
         usu†rio*/
         IF CAN-FIND(RowErrors WHERE RowErrors.ErrorNumber = 3 AND
                                     RowErrors.Errortype = "INTERNAL":U AND
                                     RowErrors.ErrorSubType = "ERROR":U) OR
            CAN-FIND(RowErrors WHERE RowErrors.ErrorNumber = 34 AND
                                     RowErrors.Errortype = "INTERNAL":U AND
                                     RowErrors.ErrorSubType = "ERROR":U) THEN DO:
                /*Cancela a alteraá∆o do registro pois o mesmo n∆o esta disponivel*/
                RUN cancelRecord IN THIS-PROCEDURE.
                
                /*---tente reposicionar o registro pai se nao consguir reabre a query ---*/
                RUN initializeDBOs IN THIS-PROCEDURE.
                
                /*--- Posiciona no primeiro registro da query ---*/
                RUN getFirst IN THIS-PROCEDURE.
         END.
         /*Fim da alteraá∆o Anderson*/
    END.
    ELSE
        /*--- Destr¢i tela de mensagens de erros ---*/
        {method/showmessage.i3}

    IF cReturnAux = "OK":U THEN DO:
        /*--- Retorna rowid do registro corrente do DBO ---*/
        RUN getRowid IN {&hDBOTable} (OUTPUT rCurrentAux).
        
        /*--- Atualiza vari†vel rCurrent, campo {&ttTable}.r-Rowid e vari†vel
              global {&rTable} com o valor do rowid do registro recÇm criado ---*/
        ASSIGN rCurrent          = rCurrentAux
               {&ttTable}.r-Rowid = rCurrentAux
               {&rTable}         = rCurrentAux.
        
    IF AVAILABLE {&ttTable} THEN 
         assign epc-rowid = {&ttTable}.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

        /*--- Executa programas de customizaá∆o (before/after) ---*/
        {method/custom.i &Event="AFTER-ASSIGN"
                         &Object="CONTAINER"
                         &ObjectHandle="THIS-PROCEDURE:HANDLE"
                         &FrameHandle="FRAME fPage0:HANDLE"
                         &Table="{&DBOTable}"
                         &RowidTable="epc-rowid"}
        
        /*--- Continua em modo de inclus∆o no caso da vari†vel cAction = "ADD" ---*/
        IF cAction = "ADD":U THEN DO:
            ASSIGN cAction = "":U.
            
            /*--- Seleciona a primeira p†gina caso ocorra sucesso na gravaá∆o ---*/
            &IF "{&Folder}":U = "YES":U &THEN
                RUN setFolder IN hFolder (INPUT 1).
            &ENDIF
            
            /*--- Verifica se est† sendo utilizada inclus∆o M£ltipla ou Simples ---*/
            IF lMultipleAdd THEN DO:
                /*--- Executa evento de inclus∆o ---*/
                &IF "{&Add}":U = "YES":U &THEN
                    APPLY "CHOOSE":U TO btAdd IN FRAME fPage0.
                &ENDIF
                
                /*--- Seta vari†vel cButtonsState com o estado atual dos bot‰es 
                      de navegaá∆o ---*/
                ASSIGN
                    
                    cButtonsState = "":U
                    
                    &IF "{&First}":U = "YES":U &THEN
                        cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + "YES":U
                    &ENDIF
                    /* Desabilitar botao de Prev em banco diferente de Progress */
                    &IF "{&Prev}":U = "YES":U &THEN
                        &IF "{&ems_dbtype}":U = "ORACLE":U &THEN
                            cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + "NO":U
                        &ELSE
                            cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + "YES":U
                        &ENDIF
                    &ENDIF
                    
                    &IF "{&Next}":U = "YES":U &THEN
                        cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + "YES":U
                    &ENDIF
                    
                    &IF "{&Last}":U = "YES":U &THEN
                        cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + "YES":U
                    &ENDIF
                    
                    &IF "{&GoTo}":U = "YES":U &THEN
                        cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + "YES":U
                    &ENDIF
                    
                    &IF "{&Search}":U = "YES":U &THEN
                        cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + "YES":U
                    &ENDIF
                    
                    &IF "{&Add}":U = "YES":U &THEN
                        cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + "YES":U
                    &ENDIF
                    
                    &IF "{&Copy}":U = "YES":U &THEN
                        cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + "YES":U
                    &ENDIF
                    
                    &IF "{&Update}":U = "YES":U &THEN
                        cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + "YES":U
                    &ENDIF
                    
                    &IF "{&Delete}":U = "YES":U &THEN
                        cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + "YES":U
                    &ENDIF
                    
                       /* tech14187 - 10/04/2007 - 1468007 - n∆o Ç necess†rio guardar o estado anterior destes bot‰es. */
/*                     /*tech14207 - 20/12/2003 - FO 1387.046*/                                */
/*                     &IF "{&ExcludeBtQueryJoins}":U = "YES":U &THEN                          */
/*                         cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + "YES":U */
/*                     &ENDIF                                                                  */
/*                                                                                             */
/*                     &IF "{&ExcludeBtReportsJoins}":U = "YES":U &THEN                        */
/*                         cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + "YES":U */
/*                     &ENDIF                                                                  */
/*                                                                                             */
/*                     cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + "YES":U     */
/*                                                                                             */
/*                     cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + "YES":U     */
/*                                                                                             */
/*                     /*tech14207 - 20/12/2003 - FO 1387.046*/                                */
                    .
            END.
            ELSE DO:
                /*--- Seta vari†vel cButtonsStateAux para ser passada como parÉmetro para o 
                      mÇtodo controlToolBar ---*/
                ASSIGN
                    
                    cButtonsStateAux = cButtonsState
                    
                    &IF "{&Undo}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                    &ENDIF
                    
                    &IF "{&Cancel}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                    &ENDIF
                    
                    &IF "{&Save}":U = "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                    &ENDIF
                    
                    /*tech14207 - 20/12/2003 - FO 1387.046*/                    
                     &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN 
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    &ENDIF
                    
                    &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    &ENDIF
                    
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U

                    /*tech14207 - 20/12/2003 - FO 1387.046*/                    
                    .
                
                /*--- Seta estado dos bot‰es/menu ---*/
                RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
                
                /*--- Seta estado das frames ---*/
                RUN disableFields IN THIS-PROCEDURE.
            END.
        END.
        ELSE DO:
            ASSIGN cAction = "":U.
            
            /*--- Seta vari†vel cButtonsStateAux para ser passada como parÉmetro para o 
                  mÇtodo controlToolBar ---*/
            ASSIGN
                
                cButtonsStateAux = cButtonsState
                
                &IF "{&Undo}":U = "YES":U &THEN
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                &ENDIF
                
                &IF "{&Cancel}":U = "YES":U &THEN
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                &ENDIF
                
                &IF "{&Save}":U = "YES":U &THEN
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
                &ENDIF
                
                /*tech14207 - 20/12/2003 - FO 1387.046*/
                &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                &ENDIF
                
                &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                &ENDIF
                
                /*tech1139 - bot∆o Exit */
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    
                /*tech1139 - bot∆o Help */
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                
                /*tech14207 - 20/12/2003 - FO 1387.046*/
                .
            
            /*--- Seta estado dos bot‰es/menu ---*/
            RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
            
            /*--- Seta estado das frames ---*/
            RUN disableFields IN THIS-PROCEDURE.
        END.
        
        /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
        &IF DEFINED(DBOVersion) <> 0 &THEN
            /*--- N∆o h† a necessidade de retornar a temp-table RowErrors para
                  o BO 1.1, pois isto j† foi feito anteriormente ---*/
        &ELSE
            /*--- Retorna temp-table RowErrors do DBO ---*/
            RUN getRowErrors IN {&hDBOTable} (OUTPUT TABLE RowErrors).
        &ENDIF

        /* Caso esteja definido o preprocessor newRecordOffQuery no DBO,
           ent„o n„o mostra erros de reposicionamento */ 
        RUN NewRecordOffQuery IN {&hDBOTable} (OUTPUT offQuery) NO-ERROR.
        IF ERROR-STATUS:ERROR = NO AND offQuery = YES THEN DO:
            IF CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorNumber <> 3 AND
                                              RowErrors.ErrorNumber <> 8) and
                                              RowErrors.ErrorSubType = "ERROR":U THEN DO: 
            /*--- Inicializa tela de mensagens de erros ---*/
                {method/showmessage.i1}
               
                /*--- Seta cursor do mouse para normal ---*/
                SESSION:SET-WAIT-STATE("":U).
            
            /*--- Transfere temp-table RowErrors para a tela de mensagens de erros ---*/
                {method/showmessage.i2 &Modal="YES"}
            END.
        END.
        ELSE DO:
            IF CAN-FIND(FIRST RowErrors) THEN DO: 
            /*--- Inicializa tela de mensagens de erros ---*/
                {method/showmessage.i1}
               
                /*--- Seta cursor do mouse para normal ---*/
                SESSION:SET-WAIT-STATE("":U).
            
            /*--- Transfere temp-table RowErrors para a tela de mensagens de erros ---*/
                {method/showmessage.i2 &Modal="YES"}
            END.
        END.
    END.
    
    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    
    /*--- Alterado por Valdir (tech264), quando o objeto com focus ficar desabilitado transfere o focus para a FRAME ---*/
    /*--- Transfere o Focus para a Frame ---*/
    /*alteracao feita por anderson em 09/06/2001 para verificar
     se o botoes de navegacao estao definidos no programa*/
    &IF "{&SAVE}":U = "YES":U &THEN
        IF btSave:SENSITIVE = NO THEN
            RUN transfereFocoFrame IN THIS-PROCEDURE.
    &ENDIF

    IF CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorSubType = "ERROR":U 
                OR RowErrors.ErrorSubType = "":U) THEN
        RETURN "NOK":U.
    ELSE
        RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-showQueryJoins) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showQueryJoins Method-Library 
PROCEDURE showQueryJoins :
/*------------------------------------------------------------------------------
  Purpose:     Executa janela de Consultas Relacionadas
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
/* alterado por Valdir (tech264) novo mÇtodo de teste do valid-handle */    
    /*--- Inicializa janela de Consultas Relacionadas ---*/
    IF NOT VALID-HANDLE(hQueryJoins) OR
       hQueryJoins:TYPE <> "PROCEDURE":U OR
       hQueryJoins:FILE-NAME <> "utp/ut-cons.w" THEN
        RUN utp/ut-cons.w PERSISTENT SET hQueryJoins (INPUT c-programa-mg97).
    
    IF VALID-HANDLE(hQueryJoins) AND
       hQueryJoins:TYPE = "PROCEDURE":U AND
       hQUeryJoins:FILE-NAME = "utp/ut-cons.w" THEN
        RUN dispatch IN hQueryJoins (INPUT "INITIALIZE":U).
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-showReportsJoins) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showReportsJoins Method-Library 
PROCEDURE showReportsJoins :
/*------------------------------------------------------------------------------
  Purpose:     Executa janela de Relat¢rios Relacionados
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
/* alterado por Valdir (tech264) novo mÇtodo de teste do valid-handle */    
    /*--- Inicializa janela de Relat¢rios Relacionados ---*/
    IF NOT VALID-HANDLE(hReportsJoins) OR
       hReportsJoins:TYPE <> "PROCEDURE":U OR
       hReportsJoins:FILE-NAME <> "utp/ut-relat.w":U THEN
        RUN utp/ut-relat.w PERSISTENT SET hReportsJoins (INPUT c-programa-mg97).
    
    IF VALID-HANDLE(hReportsJoins) AND
       hReportsJoins:TYPE = "PROCEDURE":U AND
       hReportsJoins:FILE-NAME = "utp/ut-relat.w":U THEN
        RUN dispatch IN hReportsJoins (INPUT "INITIALIZE":U).
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-transfereFocoFrame) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE transfereFocoFrame Method-Library 
PROCEDURE transfereFocoFrame :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    APPLY "entry":U TO FRAME fPage0.
    
    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-undoRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE undoRecord Method-Library 
PROCEDURE undoRecord :
/*------------------------------------------------------------------------------
  Purpose:     Desfaz alteraá‰es feitas para o registro corrente
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    /*--- Seta cursor do mouse para espera ---*/
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    IF AVAILABLE {&ttTable} THEN 
         assign epc-rowid = {&ttTable}.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customizaá∆o (before/after) ---*/
    {method/custom.i &Event="BEFORE-UNDO"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable}"
                     &RowidTable="epc-rowid"}
    
    /*--- Limpa temp-table {&ttTable} ---*/
    FOR EACH {&ttTable}:
        DELETE {&ttTable}.
    END.
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Atualiza temp-table {&ttTable} com base na temp-table ttBuffer ---*/
        FIND FIRST ttBuffer NO-ERROR.
        IF AVAILABLE ttBuffer THEN DO:
            CREATE {&ttTable}.
            BUFFER-COPY ttBuffer TO {&ttTable}.
        END.
    &ELSE
        /*--- Atualiza temp-table {&ttTable} com base no registro corrento do DBO ---*/
        
        /*--- Limpa temp-table RowErrors do DBO ---*/
        RUN emptyRowErrors IN {&hDBOTable}.
        
        /*Correcao do comportamento do evento do botao undorecord no thim maintenence 
          para que o mesmo, na inclusao limpe os campos 
          e na altercao ou copia traga os valores do registro corrente do BO */
        /* Alteracao feita por Anderson (tech485) em 19/04/2001
           Alterado por Paulo H. Lazzarotti em 08/05/2002 */
        IF cAction = "COPY":U OR cAction = "UPDATE":U THEN
            /*--- Posiciona DBO no registro corrente ---*/
            RUN bringCurrent IN {&hDBOTable}.

        /*--- Retorna registro da temp-table {&ttTable} do DBO ---*/
        RUN getRecord IN {&hDBOTable} (OUTPUT TABLE {&ttTable}).
    
    &ENDIF
        
    /*--- Atualize dados em tela ---*/
    RUN displayFields IN THIS-PROCEDURE.
    
    IF AVAILABLE {&ttTable} THEN 
         assign epc-rowid = {&ttTable}.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customizaá∆o (before/after) ---*/
    {method/custom.i &Event="AFTER-UNDO"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable}"
                     &RowidTable="epc-rowid"}
    
    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-updateRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateRecord Method-Library 
PROCEDURE updateRecord :
/*------------------------------------------------------------------------------
  Purpose:     Inclui novo registro
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE cButtonsStateAux AS CHARACTER NO-UNDO.
    
    /*--- Seta cursor do mouse para espera ---*/
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    /*****************
      In°cio modificaá‰es referentes a FO 1189107, sobre a criaá∆o de eventos de EPC para interface de 
      ThinTemplates para o evento de UpdateRecord, similar aos que j† existem para AddRecord. 
      Respons†vel: tech14187 - Data: 09/08/2005 - Task: 2526
      OS: 6722 - Ativid: 139698 - Tarefa: 167095
     *****************/

    IF AVAILABLE {&ttTable} THEN 
         assign epc-rowid = {&ttTable}.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.    

    /*--- Executa programas de customizaá∆o (before/after) ---*/
    {method/custom.i &Event="BEFORE-UPDATE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable}"
                     &RowidTable="epc-rowid"}
    
    /*****************
      Fim das modificaá‰es referentes a FO 1189107, sobre a criaá∆o de eventos de EPC para interface de 
      ThinTemplates para o evento de UpdateRecord, similar aos que j† existem para AddRecord. 
      Respons†vel: tech14187 - Data: 09/08/2005 - Task: 2526
      OS: 6722 - Ativid: 139698 - Tarefa: 167095
     *****************/


    /*--- Seta vari†vel cAction ---*/
    ASSIGN cAction = "UPDATE":U.
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        IF AVAILABLE {&ttTable} THEN DO:
            /*--- Limpa temp-table ttBuffer ---*/
            FOR EACH ttBuffer:
                DELETE ttBuffer.
            END.
            
            /*--- Atualiza temp-table ttBuffer ---*/
            CREATE ttBuffer.
            BUFFER-COPY {&ttTable} TO ttBuffer.
        END.
    &ENDIF
    
    DO WITH FRAME fPage0:
        /*--- Seta vari†vel cButtonsState com o estado atual dos bot‰es 
              de navegaá∆o ---*/
        ASSIGN
            
            cButtonsState = "":U
            
            &IF "{&First}":U = "YES":U &THEN
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btFirst:SENSITIVE)
            &ENDIF
            
            &IF "{&Prev}":U = "YES":U &THEN
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btPrev:SENSITIVE)
            &ENDIF
            
            &IF "{&Next}":U = "YES":U &THEN
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btNext:SENSITIVE)
            &ENDIF
            
            &IF "{&Last}":U = "YES":U &THEN
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btLast:SENSITIVE)
            &ENDIF
            
            &IF "{&GoTo}":U = "YES":U &THEN
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btgoTo:SENSITIVE)
            &ENDIF
            
            &IF "{&Search}":U = "YES":U &THEN
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btSearch:SENSITIVE)
            &ENDIF
            
            &IF "{&Add}":U = "YES":U &THEN
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btAdd:SENSITIVE)
            &ENDIF
            
            &IF "{&Copy}":U = "YES":U &THEN
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btCopy:SENSITIVE)
            &ENDIF
            
            &IF "{&Update}":U = "YES":U &THEN
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btUpdate:SENSITIVE)
            &ENDIF
            
            &IF "{&Delete}":U = "YES":U &THEN
                cButtonsState = cButtonsState + MIN(cButtonsState, ",":U) + STRING(btDelete:SENSITIVE)
            &ENDIF
            
            .
        
        /*--- Seta vari†vel cButtonsStateAux para ser passada como parÉmetro para o 
              mÇtodo controlToolBar ---*/
        ASSIGN
            
            cButtonsStateAux = "":U
            
            &IF "{&First}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Prev}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Next}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Last}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&GoTo}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Search}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Add}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Copy}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Update}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Delete}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&Undo}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Cancel}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            &IF "{&Save}":U = "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
            &ENDIF
            
            /*tech1139 - 05/12/2006 - FO 1387.046 */
            &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN /*alterado*/
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            &ENDIF
            
            /*tech1139 - bot∆o Exit */
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U            

            /*tech1139 - bot∆o Help */
            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "NO":U
            
            /*tech1139 - 05/12/2006 - FO 1387.046 */
            .
    END.
    
    /*--- Seta estado dos bot‰es/menu ---*/
    RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
    
    /*--- Seta estado das frames ---*/
    RUN enableFields IN THIS-PROCEDURE.
    
    /*****************
      In°cio modificaá‰es referentes a FO 1189107, sobre a criaá∆o de eventos de EPC para interface de 
      ThinTemplates para o evento de UpdateRecord, similar aos que j† existem para AddRecord. 
      Respons†vel: tech14187 - Data: 09/08/2005 - Task: 2526
      OS: 6722 - Ativid: 139698 - Tarefa: 167095
     *****************/

    IF AVAILABLE {&ttTable} THEN 
         assign epc-rowid = {&ttTable}.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.    

    /*--- Executa programas de customizaá∆o (before/after) ---*/
    {method/custom.i &Event="AFTER-UPDATE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable}"
                     &RowidTable="epc-rowid"}
    
    /*****************
      Fim das modificaá‰es referentes a FO 1189107, sobre a criaá∆o de eventos de EPC para interface de 
      ThinTemplates para o evento de UpdateRecord, similar aos que j† existem para AddRecord. 
      Respons†vel: tech14187 - Data: 09/08/2005 - Task: 2526
      OS: 6722 - Ativid: 139698 - Tarefa: 167095
     *****************/

    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

