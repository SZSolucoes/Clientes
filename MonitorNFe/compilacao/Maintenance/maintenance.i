&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
/* Procedure Description
"Method Library principal para Maintenance Template, que cont‚m defini‡äes e chamadas a outras Method Libraries."
*/
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Method-Library 
/*--------------------------------------------------------------------------
    Library    : maintenance/Maintenance.i
    Purpose    : Method Library principal para Maintenance Template, que 
                 cont‚m defini‡äes e chamadas a outras Method Libraries 

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* Global Variable Definitions ---                                        */
DEFINE NEW GLOBAL SHARED VARIABLE hWindowStyles AS HANDLE NO-UNDO.

&IF "{&DBOTable}":U <> "":U &THEN
    &GLOBAL-DEFINE rTable gr-{&DBOTable}
    
    DEFINE NEW GLOBAL SHARED VARIABLE gr-{&DBOTable} AS ROWID NO-UNDO.
&ENDIF

/* Local Temp-Table Definitions ---                                       */
/*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
&IF DEFINED(DBOVersion) <> 0 &THEN
    DEFINE TEMP-TABLE ttBuffer NO-UNDO LIKE {&ttTable}.
    
    DEFINE TEMP-TABLE RowErrors NO-UNDO
        FIELD ErrorSequence    AS INTEGER
        FIELD ErrorNumber      AS INTEGER
        FIELD ErrorDescription AS CHARACTER
        FIELD ErrorParameters  AS CHARACTER
        FIELD ErrorType        AS CHARACTER
        FIELD ErrorHelp        AS CHARACTER
        FIELD ErrorSubType     AS CHARACTER.
&ELSE
    {method/dbotterr.i}
&ENDIF

/*Alterado 08/11/2006 - tech1007 - FO 1301831 - Altera‡Æo para exibir o botÆo de LAST em programas Thin
/** Farley - Projeto SQL - 12/08/2003 **/
&IF "{&EMS_DBTYPE}":U = "MSS":U &THEN
    define variable iBotaoLast as integer no-undo.
    define variable hBotaoLast as handle  no-undo.
    define variable hFieldGroup as handle no-undo.
    define variable iAchaBtLast as integer no-undo.
    &GLOBAL-DEFINE LAST NO
    assign hBotaoLast = frame {&frame-name}:first-child.
    Repeat While Valid-handle(hBotaoLast):
        If iAchaBtLast = 10 Then leave. /* O objeto do botÆo Last sempre estar  presente nos 10 primeiros Handles da Frames fPage0 */
        If hBotaoLast:type = "field-group" Then Assign hFieldGroup = hBotaoLast.
        If hBotaoLast:type <> "Expression" Then Do:
            If hBotaoLast:type = "button" Then do:
                If hBotaoLast:name = "btLast" Then do:
                    assign hBotaoLast:sensitive = no
                           hBotaoLast:visible   = no
                           hBotaoLast:hidden    = yes.
                    leave.
                End.
            End.
        End.
        
        If Can-query(hBotaoLast,"first-child") Then Assign hBotaoLast = hBotaoLast:First-child.
        Else Assign hBotaoLast = hBotaoLast:next-sibling No-error.
        If Valid-handle(hBotaoLast) = No Then do:
            Assign hBotaoLast = hFieldGroup:next-sibling No-error.
        End.
        assign iAchaBtLast = iAchaBtLast + 1.
    End.
               
    assign hBotaoLast = menu mbMain:first-child
           iAchaBtLast = 0.
    Repeat while valid-handle(hBotaoLast):
        If iAchaBtLast = 10 Then leave.
        If can-query(hBotaoLast,"name") Then do:
            If hBotaoLast:name = "miLast" Then do:
                assign hBotaoLast:sensitive = no.
                leave.
            End.
        End.
        If can-query(hBotaoLast,"first-child") Then
            assign hBotaoLast = hBotaoLast:first-child.
        else assign hBotaoLast = hBotaoLast:next-sibling.
        assign iAchaBtLast = iAchaBtLast + 1.
    End.
&ENDIF
/** FIM - Farley **/
Fim altera‡Æo 08/11/2006*/

/* Local Variable Definitions ---                                         */
DEFINE VARIABLE cAction           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cButtonsState     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cButtonsStateAux  AS CHARACTER NO-UNDO.
DEFINE VARIABLE deColPanel        AS DECIMAL   NO-UNDO
    DECIMALS 2 INITIAL 31.
DEFINE VARIABLE deColPanelNav     AS DECIMAL   NO-UNDO
    DECIMALS 2 INITIAL 1.60.
DEFINE VARIABLE hFolder           AS HANDLE    NO-UNDO.
DEFINE VARIABLE hProgramZoom      AS HANDLE    NO-UNDO.
DEFINE VARIABLE hQueryJoins       AS HANDLE    NO-UNDO.
DEFINE VARIABLE hReportsJoins     AS HANDLE    NO-UNDO.
DEFINE VARIABLE hShowMsg          AS HANDLE    NO-UNDO.
DEFINE VARIABLE lMultipleAdd      AS LOGICAL   NO-UNDO
    INITIAL YES.
DEFINE VARIABLE lCustomExecuted   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lOverrideExecuted AS LOGICAL   NO-UNDO.
DEFINE VARIABLE rCurrent          AS ROWID     NO-UNDO.

DEFINE VARIABLE c-nom-prog-dpc-mg97  AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-nom-prog-appc-mg97 AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-nom-prog-upc-mg97  AS CHARACTER NO-UNDO.

DEFINE VARIABLE epc-rowid            AS ROWID     NO-UNDO.

&IF "{&mguni_version}" >= "2.06b" OR "{&aplica_facelift}" = "YES" &THEN
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
    define new global shared variable h-facelift as handle no-undo.
    if not valid-handle(h-facelift) then run btb/btb901zo.p persistent set h-facelift.
&ENDIF

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
         HEIGHT             = 10.17
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Method-Library 
/* ************************* Included-Libraries *********************** */

{utp/ut-glob.i}
{include/i-sysvar.i}
{btb/btb008za.i0}
{maintenance/buttons.i}
/*** Alterado por Farley - em 23/07/2003 ***/
{include/i_fnctrad.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Method-Library 


/* ***************************  Main Block  *************************** */
    &IF "{&mguni_version}" >= "2.06b" OR "{&aplica_facelift}" = "YES" &THEN
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
    run pi_aplica_facelift in this-procedure.
    &ENDIF

/*******
    Altera‡Æo 07/12/2005 - tech14187 - Altera‡Æo para evitar estouro de segmento
******/
    RUN piTraduz IN THIS-PROCEDURE.


/*Alterado por Valdir (tech264) para fazer tratamento da trigger de help*/
ON HELP OF wMaintenance DO:
    apply "choose":U to btHelp in frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-applyReturn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE applyReturn Method-Library 
PROCEDURE applyReturn :
/*------------------------------------------------------------------------------
  Purpose:     L¢gica para trigger de RETURN
  Parameters:  
  Notes:       Torna-se necess rio o comando CASE pois a trigger de RETURN,
               aparentemente, executa autom ticamente a op‡Æo NO-APPLY
------------------------------------------------------------------------------*/
    CASE SELF:TYPE:
        /*--- Evento RETURN padrÆo para widgets do tipo editor ---*/
        WHEN "EDITOR":U THEN
            SELF:INSERT-STRING(CHR(10)).
        
        /*--- Evento RETURN padrÆo para widgets do tipo button ---*/
        WHEN "BUTTON":U THEN
            APPLY "CHOOSE":U TO SELF.
        
        /*--- Evento RETURN padrÆo para outros widgets ---*/
        OTHERWISE
            &IF "{&Save}":U = "YES":U &THEN
                APPLY "CHOOSE":U TO btSave IN FRAME fPage0.
            &ELSE
                APPLY "RETURN":U TO SELF.
            &ENDIF
    END CASE.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-changePage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changePage Method-Library 
PROCEDURE changePage :
/*------------------------------------------------------------------------------
  Purpose:     M‚todo executado pelo programa de Folder, quando h  troca de 
               p gina
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    {method/override.i &Position="Before"
                       &Procedure="changePage"}
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.

    IF AVAILABLE {&ttTable} THEN 
         assign epc-rowid = {&ttTable}.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.    

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="BEFORE-CHANGE-PAGE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable}"
                     &RowidTable="epc-rowid"}
    
    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="AFTER-CHANGE-PAGE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable}"
                     &RowidTable="epc-rowid"}
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    {method/override.i &Position="After"
                       &Procedure="changePage"}
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-controlToolBar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE controlToolBar Method-Library 
PROCEDURE controlToolBar :
/*------------------------------------------------------------------------------
  Purpose:     Controla estado dos botäes do retƒngulo rtToolBar
  Parameters:  recebe estado dos botäes
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pButtonsState AS CHARACTER NO-UNDO.
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    {method/override.i &Position="Before"
                       &Procedure="controlToolBar"}
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    IF AVAILABLE {&ttTable} THEN 
         assign epc-rowid = {&ttTable}.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="BEFORE-CONTROL-TOOL-BAR"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable}"
                     &RowidTable="epc-rowid"}

    DO WITH FRAME fPage0:

        ASSIGN
            
            &IF "{&First}":U = "YES":U &THEN
                &IF "{&ButtonEntry}":U = "":U &THEN
                    &SCOPED-DEFINE ButtonEntry 1
                &ELSE
                    &SCOPED-DEFINE ButtonEntry {&ButtonEntry} + 1
                &ENDIF
                
                btFirst:SENSITIVE                          = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U
                MENU-ITEM miFirst:SENSITIVE IN MENU mbMain = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U
            &ENDIF
            
            &IF "{&Prev}":U = "YES":U &THEN
                &IF "{&ButtonEntry}":U = "":U &THEN
                    &SCOPED-DEFINE ButtonEntry 1
                &ELSE
                    &SCOPED-DEFINE ButtonEntry {&ButtonEntry} + 1
                &ENDIF
                
                btPrev:SENSITIVE                          = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U
                MENU-ITEM miPrev:SENSITIVE IN MENU mbMain = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U
            &ENDIF
            
            &IF "{&Next}":U = "YES":U &THEN
                &IF "{&ButtonEntry}":U = "":U &THEN
                    &SCOPED-DEFINE ButtonEntry 1
                &ELSE
                    &SCOPED-DEFINE ButtonEntry {&ButtonEntry} + 1
                &ENDIF
                
                btNext:SENSITIVE                          = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U
                MENU-ITEM miNext:SENSITIVE IN MENU mbMain = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U
            &ENDIF
            
            &IF "{&Last}":U = "YES":U &THEN
                &IF "{&ButtonEntry}":U = "":U &THEN
                    &SCOPED-DEFINE ButtonEntry 1
                &ELSE
                    &SCOPED-DEFINE ButtonEntry {&ButtonEntry} + 1
                &ENDIF
                
                btLast:SENSITIVE                          = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U
                MENU-ITEM miLast:SENSITIVE IN MENU mbMain = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U
            &ENDIF
            
            &IF "{&GoTo}":U = "YES":U &THEN
                &IF "{&ButtonEntry}":U = "":U &THEN
                    &SCOPED-DEFINE ButtonEntry 1
                &ELSE
                    &SCOPED-DEFINE ButtonEntry {&ButtonEntry} + 1
                &ENDIF
                
                btGoTo:SENSITIVE                          = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U
                MENU-ITEM miGoTo:SENSITIVE IN MENU mbMain = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U
            &ENDIF
            
            &IF "{&Search}":U = "YES":U &THEN
                &IF "{&ButtonEntry}":U = "":U &THEN
                    &SCOPED-DEFINE ButtonEntry 1
                &ELSE
                    &SCOPED-DEFINE ButtonEntry {&ButtonEntry} + 1
                &ENDIF
                
                btSearch:SENSITIVE                          = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U
                MENU-ITEM miSearch:SENSITIVE IN MENU mbMain = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U
            &ENDIF
            
            &IF "{&Add}":U = "YES":U &THEN
                &IF "{&ButtonEntry}":U = "":U &THEN
                    &SCOPED-DEFINE ButtonEntry 1
                &ELSE
                    &SCOPED-DEFINE ButtonEntry {&ButtonEntry} + 1
                &ENDIF
                
                btAdd:SENSITIVE                          = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U
                MENU-ITEM miAdd:SENSITIVE IN MENU mbMain = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U
            &ENDIF
            
            &IF "{&Copy}":U = "YES":U &THEN
                &IF "{&ButtonEntry}":U = "":U &THEN
                    &SCOPED-DEFINE ButtonEntry 1
                &ELSE
                    &SCOPED-DEFINE ButtonEntry {&ButtonEntry} + 1
                &ENDIF
                
                btCopy:SENSITIVE                          = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U
                MENU-ITEM miCopy:SENSITIVE IN MENU mbMain = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U
            &ENDIF
            
            &IF "{&Update}":U = "YES":U &THEN
                &IF "{&ButtonEntry}":U = "":U &THEN
                    &SCOPED-DEFINE ButtonEntry 1
                &ELSE
                    &SCOPED-DEFINE ButtonEntry {&ButtonEntry} + 1
                &ENDIF
                
                btUpdate:SENSITIVE                          = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U
                MENU-ITEM miUpdate:SENSITIVE IN MENU mbMain = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U
            &ENDIF
            
            &IF "{&Delete}":U = "YES":U &THEN
                &IF "{&ButtonEntry}":U = "":U &THEN
                    &SCOPED-DEFINE ButtonEntry 1
                &ELSE
                    &SCOPED-DEFINE ButtonEntry {&ButtonEntry} + 1
                &ENDIF
                
                btDelete:SENSITIVE                          = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U
                MENU-ITEM miDelete:SENSITIVE IN MENU mbMain = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U
            &ENDIF
            
            &IF "{&Undo}":U = "YES":U &THEN
                &IF "{&ButtonEntry}":U = "":U &THEN
                    &SCOPED-DEFINE ButtonEntry 1
                &ELSE
                    &SCOPED-DEFINE ButtonEntry {&ButtonEntry} + 1
                &ENDIF
                
                btUndo:SENSITIVE                          = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U
                MENU-ITEM miUndo:SENSITIVE IN MENU mbMain = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U
            &ENDIF
            
            &IF "{&Cancel}":U = "YES":U &THEN
                &IF "{&ButtonEntry}":U = "":U &THEN
                    &SCOPED-DEFINE ButtonEntry 1
                &ELSE
                    &SCOPED-DEFINE ButtonEntry {&ButtonEntry} + 1
                &ENDIF
                
                btCancel:SENSITIVE                          = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U
                MENU-ITEM miCancel:SENSITIVE IN MENU mbMain = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U
            &ENDIF
            
            &IF "{&Save}":U = "YES":U &THEN
                &IF "{&ButtonEntry}":U = "":U &THEN
                    &SCOPED-DEFINE ButtonEntry 1
                &ELSE
                    &SCOPED-DEFINE ButtonEntry {&ButtonEntry} + 1
                &ENDIF
                
                btSave:SENSITIVE                          = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U
                MENU-ITEM miSave:SENSITIVE IN MENU mbMain = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U
            &ENDIF

            /*tech1139 - 05/12/2006 - FO 1387.046 */
            &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN
                &IF "{&ButtonEntry}":U = "":U &THEN
                    &SCOPED-DEFINE ButtonEntry 1
                &ELSE
                    &SCOPED-DEFINE ButtonEntry {&ButtonEntry} + 1
                &ENDIF
                
                btQueryJoins:SENSITIVE                          = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U
                MENU-ITEM miQueryJoins:SENSITIVE IN MENU mbMain = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U            
            &ENDIF
            
            &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN
                &IF "{&ButtonEntry}":U = "":U &THEN
                    &SCOPED-DEFINE ButtonEntry 1
                &ELSE
                    &SCOPED-DEFINE ButtonEntry {&ButtonEntry} + 1
                &ENDIF
                
                btReportsJoins:SENSITIVE                          = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U
                MENU-ITEM miReportsJoins:SENSITIVE IN MENU mbMain = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U                        
            &ENDIF

            &IF "{&ButtonEntry}":U = "":U &THEN
                &SCOPED-DEFINE ButtonEntry 1
            &ELSE
                &SCOPED-DEFINE ButtonEntry {&ButtonEntry} + 1
            &ENDIF
                
            /*Erro de entry*/
            btExit:SENSITIVE                          = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U
            MENU-ITEM miExit:SENSITIVE IN MENU mbMain = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U      
            
                    
            &IF "{&ButtonEntry}":U = "":U &THEN
                &SCOPED-DEFINE ButtonEntry 1
            &ELSE
                &SCOPED-DEFINE ButtonEntry {&ButtonEntry} + 1
            &ENDIF
                
            btHelp:SENSITIVE                              = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U
            MENU-ITEM miContents:SENSITIVE IN MENU mbMain = ENTRY({&ButtonEntry}, pButtonsState) = "YES":U                        
            /*tech1139 - 05/12/2006 - FO 1387.046 */
            .
    END.
    
    IF AVAILABLE {&ttTable} THEN 
         assign epc-rowid = {&ttTable}.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="AFTER-CONTROL-TOOL-BAR"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable}"
                     &RowidTable="epc-rowid"}
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    {method/override.i &Position="After"
                       &Procedure="controlToolBar"}
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-destroyInterface) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE destroyInterface Method-Library 
PROCEDURE destroyInterface :
/*------------------------------------------------------------------------------
  Purpose:     Destr¢i programa
  Parameters:  
  Notes:       Destr¢i programas de: Folder, Consultas e Relat¢rios Relacionados
------------------------------------------------------------------------------*/
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    {method/override.i &Position="Before"
                       &Procedure="destroyInterface"}
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    IF AVAILABLE {&ttTable} THEN 
         assign epc-rowid = {&ttTable}.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="BEFORE-DESTROY-INTERFACE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable}"
                     &RowidTable="epc-rowid"}
    
    /*Inserida a chamada da include i-logfin1.i
    devido aos thintemplates nÆo gerarem log de 
    execucao de programas*/
    {include/i-logfin1.i}

    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Destr¢i BO 1.1 ---*/
        &IF "{&hDBOTable}":U <> "":U &THEN
            IF VALID-HANDLE({&hDBOTable}) THEN
                DELETE PROCEDURE {&hDBOTable}.
        &ENDIF
    &ELSE
        /*--- Destr¢i DBO ---*/
        &IF "{&hDBOTable}":U <> "":U &THEN
            IF VALID-HANDLE({&hDBOTable}) THEN
                RUN destroy IN {&hDBOTable}.
        &ENDIF
    &ENDIF
    
    /*--- Destr¢i programa de folder ---*/
    &IF "{&Folder}":U = "YES":U &THEN
        IF VALID-HANDLE(hFolder) THEN
            DELETE PROCEDURE hFolder.
    &ENDIF
    
    /*--- Destr¢i programa de Consultas Relacionadas ---*/
    IF VALID-HANDLE(hQueryJoins) THEN
        DELETE PROCEDURE hQueryJoins.
    
    /*--- Destr¢i programa de Relat¢rios Relacionados ---*/
    IF VALID-HANDLE(hReportsJoins) THEN
        DELETE PROCEDURE hReportsJoins.
    
    IF AVAILABLE {&ttTable} THEN 
         assign epc-rowid = {&ttTable}.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="AFTER-DESTROY-INTERFACE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable}"
                     &RowidTable="epc-rowid"}
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    {method/override.i &Position="After"
                       &Procedure="destroyInterface"}
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    /*--- Destr¢i os Servidores RPC inicializados pelos DBOs ---*/
    {btb/btb008za.i3}
        
    /*Alteracao para deletar da mem¢ria o WindowStyles e o btb008za.p*/
    IF VALID-HANDLE(h-servid-rpc) THEN
    DO:
       DELETE PROCEDURE h-servid-rpc.
       ASSIGN h-servid-rpc = ?. /*Garantir que a vari vel nÆo vai mais apontar para nenhum handle de outro objeto - este problema apareceu na v9.1B com Windows2000*/
    END.

    /*--- Destr¢i janela associada ao programa ---*/
    IF VALID-HANDLE(wMaintenance) THEN
        DELETE WIDGET wMaintenance.
    
    /*--- Destr¢i programa ---*/
    IF THIS-PROCEDURE:PERSISTENT THEN
        DELETE PROCEDURE THIS-PROCEDURE.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-disableFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disableFields Method-Library 
PROCEDURE disableFields :
/*------------------------------------------------------------------------------
  Purpose:     Desabilita os campos da temp-table {&tt-table} 
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    {method/override.i &Position="Before"
                       &Procedure="disableFields"}
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    IF AVAILABLE {&ttTable} THEN 
         assign epc-rowid = {&ttTable}.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="BEFORE-DISABLE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable}"
                     &RowidTable="epc-rowid"}
    
    /*--- Desabilita campos contidos no {&page0KeyFields} ---*/
    &IF "{&page0KeyFields}":U <> "":U &THEN
        DO WITH FRAME fPage0:
            DISABLE {&page0KeyFields}.
        END.
    &ENDIF
    
    /*--- Desabilita campos contidos no {&page0Fields} ---*/
    &IF "{&page0Fields}":U <> "":U &THEN
        DO WITH FRAME fPage0:
            DISABLE {&page0Fields}.
        END.
    &ENDIF
    
    /*--- Desabilita campos contidos no {&page1Fields} ---*/
    &IF "{&page1Fields}":U <> "":U &THEN
        DO WITH FRAME fPage1:
            DISABLE {&page1Fields}.
        END.
    &ENDIF
    
    /*--- Desabilita campos contidos no {&page2Fields} ---*/
    &IF "{&page2Fields}":U <> "":U &THEN
        DO WITH FRAME fPage2:
            DISABLE {&page2Fields}.
        END.
    &ENDIF
    
    /*--- Desabilita campos contidos no {&page3Fields} ---*/
    &IF "{&page3Fields}":U <> "":U &THEN
        DO WITH FRAME fPage3:
            DISABLE {&page3Fields}.
        END.
    &ENDIF
    
    /*--- Desabilita campos contidos no {&page4Fields} ---*/
    &IF "{&page4Fields}":U <> "":U &THEN
        DO WITH FRAME fPage4:
            DISABLE {&page4Fields}.
        END.
    &ENDIF
    
    /*--- Desabilita campos contidos no {&page5Fields} ---*/
    &IF "{&page5Fields}":U <> "":U &THEN
        DO WITH FRAME fPage5:
            DISABLE {&page5Fields}.
        END.
    &ENDIF
    
    /*--- Desabilita campos contidos no {&page6Fields} ---*/
    &IF "{&page6Fields}":U <> "":U &THEN
        DO WITH FRAME fPage6:
            DISABLE {&page6Fields}.
        END.
    &ENDIF
    
    /*--- Desabilita campos contidos no {&page7Fields} ---*/
    &IF "{&page7Fields}":U <> "":U &THEN
        DO WITH FRAME fPage7:
            DISABLE {&page7Fields}.
        END.
    &ENDIF
    
    /*--- Desabilita campos contidos no {&page8Fields} ---*/
    &IF "{&page8Fields}":U <> "":U &THEN
        DO WITH FRAME fPage8:
            DISABLE {&page8Fields}.
        END.
    &ENDIF
    
    IF AVAILABLE {&ttTable} THEN 
         assign epc-rowid = {&ttTable}.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="AFTER-DISABLE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable}"
                     &RowidTable="epc-rowid"}
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    {method/override.i &Position="After"
                       &Procedure="disableFields"}
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    &IF "{&mguni_version}" >= "2.06b" OR "{&aplica_facelift}" = "YES" &THEN
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
    IF VALID-HANDLE(h-faceLift) THEN DO:
        &IF "{&page0Fields}":U <> "":U OR
            "{&page0KeyFields}":U <> "":U &THEN
               RUN pi_change_state_color IN h-facelift (INPUT FRAME fPage0:HANDLE, NO).
        &ENDIF
        /*--- Desabilita campos contidos no {&page1Fields} ---*/
        &IF "{&page1Fields}":U <> "":U &THEN
               RUN pi_change_state_color IN h-facelift (INPUT FRAME fPage1:HANDLE, NO).
        &ENDIF
        
        /*--- Desabilita campos contidos no {&page2Fields} ---*/
        &IF "{&page2Fields}":U <> "":U &THEN
               RUN pi_change_state_color IN h-facelift (INPUT FRAME fPage2:HANDLE, NO).
        &ENDIF
        
        /*--- Desabilita campos contidos no {&page3Fields} ---*/
        &IF "{&page3Fields}":U <> "":U &THEN
               RUN pi_change_state_color IN h-facelift (INPUT FRAME fPage3:HANDLE, NO).
        &ENDIF
        
        /*--- Desabilita campos contidos no {&page4Fields} ---*/
        &IF "{&page4Fields}":U <> "":U &THEN
               RUN pi_change_state_color IN h-facelift (INPUT FRAME fPage4:HANDLE, NO).
        &ENDIF
        
        /*--- Desabilita campos contidos no {&page5Fields} ---*/
        &IF "{&page5Fields}":U <> "":U &THEN
               RUN pi_change_state_color IN h-facelift (INPUT FRAME fPage5:HANDLE, NO).
        &ENDIF
        
        /*--- Desabilita campos contidos no {&page6Fields} ---*/
        &IF "{&page6Fields}":U <> "":U &THEN
               RUN pi_change_state_color IN h-facelift (INPUT FRAME fPage6:HANDLE, NO).
        &ENDIF
        
        /*--- Desabilita campos contidos no {&page7Fields} ---*/
        &IF "{&page7Fields}":U <> "":U &THEN
               RUN pi_change_state_color IN h-facelift (INPUT FRAME fPage7:HANDLE, NO).
        &ENDIF
        
        /*--- Desabilita campos contidos no {&page8Fields} ---*/
        &IF "{&page8Fields}":U <> "":U &THEN
               RUN pi_change_state_color IN h-facelift (INPUT FRAME fPage8:HANDLE, NO).
        &ENDIF    
    END.
    &ENDIF
    /*********fim altera‡Æo*************/

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-dispatch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dispatch Method-Library 
PROCEDURE dispatch :
/*------------------------------------------------------------------------------
  Purpose:     Manter compatibilidade com SmartObjects
  Parameters:  recebe m‚todo a ser executado
  Notes:       Somente haver  tratamento para o m‚todo initialize, quando a 
               execu‡Æo deste m‚todo for solicitada ser  executado o m‚todo
               initializeInterface
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pMethod AS CHARACTER NO-UNDO.
    
    IF pMethod = "INITIALIZE":U THEN
        RUN initializeInterface IN THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-displayFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayFields Method-Library 
PROCEDURE displayFields :
/*------------------------------------------------------------------------------
  Purpose:     Exibe os campos da temp-table {&tt-table} em tela
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    /*--- Posiciona temp-table {&ttTable} no primeiro registro ---*/
    FIND FIRST {&ttTable} NO-ERROR.
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    {method/override.i &Position="Before"
                       &Procedure="displayFields"}
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    IF AVAILABLE {&ttTable} THEN 
         assign epc-rowid = {&ttTable}.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="BEFORE-DISPLAY"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable}"
                     &RowidTable="epc-rowid"}
    
    IF AVAILABLE {&ttTable} THEN DO:
        /*--- Atualiza vari veis rCurrent e {&rTable} com o valor do 
              rowid corrente ---*/
        ASSIGN rCurrent  = {&ttTable}.r-Rowid
               {&rTable} = rCurrent.
        
        /*--- Exibe campos contidos no {&page0KeyFields} em tela ---*/
        &IF "{&page0KeyFields}":U <> "":U &THEN
            DO WITH FRAME fPage0:
                DISPLAY {&page0KeyFields}.
            END.
        &ENDIF
        
        /*--- Exibe campos contidos no {&page0Fields} em tela ---*/
        &IF "{&page0Fields}":U <> "":U &THEN
            DO WITH FRAME fPage0:
                DISPLAY {&page0Fields}.
            END.
        &ENDIF
        
        /*--- Exibe campos contidos no {&page1Fields} em tela ---*/
        &IF "{&page1Fields}":U <> "":U &THEN
            DO WITH FRAME fPage1:
                DISPLAY {&page1Fields}.
            END.
        &ENDIF
        
        /*--- Exibe campos contidos no {&page2Fields} em tela ---*/
        &IF "{&page2Fields}":U <> "":U &THEN
            DO WITH FRAME fPage2:
                DISPLAY {&page2Fields}.
            END.
        &ENDIF
        
        /*--- Exibe campos contidos no {&page3Fields} em tela ---*/
        &IF "{&page3Fields}":U <> "":U &THEN
            DO WITH FRAME fPage3:
                DISPLAY {&page3Fields}.
            END.
        &ENDIF
        
        /*--- Exibe campos contidos no {&page4Fields} em tela ---*/
        &IF "{&page4Fields}":U <> "":U &THEN
            DO WITH FRAME fPage4:
                DISPLAY {&page4Fields}.
            END.
        &ENDIF
        
        /*--- Exibe campos contidos no {&page5Fields} em tela ---*/
        &IF "{&page5Fields}":U <> "":U &THEN
            DO WITH FRAME fPage5:
                DISPLAY {&page5Fields}.
            END.
        &ENDIF
        
        /*--- Exibe campos contidos no {&page6Fields} em tela ---*/
        &IF "{&page6Fields}":U <> "":U &THEN
            DO WITH FRAME fPage6:
                DISPLAY {&page6Fields}.
            END.
        &ENDIF
        
        /*--- Exibe campos contidos no {&page7Fields} em tela ---*/
        &IF "{&page7Fields}":U <> "":U &THEN
            DO WITH FRAME fPage7:
                DISPLAY {&page7Fields}.
            END.
        &ENDIF
        
        /*--- Exibe campos contidos no {&page8Fields} em tela ---*/
        &IF "{&page8Fields}":U <> "":U &THEN
            DO WITH FRAME fPage8:
                DISPLAY {&page8Fields}.
            END.
        &ENDIF
    END.
    ELSE DO:
        /*--- Atualiza vari veis rCurrent e {&rTable} com o valor do 
              rowid corrente ---*/
        ASSIGN rCurrent    = ?
               {&rTable} = rCurrent.
        
        CREATE {&ttTable}.
        
        /*--- Exibe campos contidos no {&page0KeyFields} em tela ---*/
        &IF "{&page0KeyFields}":U <> "":U &THEN
            DO WITH FRAME fPage0:
                DISPLAY {&page0KeyFields}.
            END.
        &ENDIF
        
        /*--- Exibe campos contidos no {&page0Fields} em tela ---*/
        &IF "{&page0Fields}":U <> "":U &THEN
            DO WITH FRAME fPage0:
                DISPLAY {&page0Fields}.
            END.
        &ENDIF
        
        /*--- Exibe campos contidos no {&page1Fields} em tela ---*/
        &IF "{&page1Fields}":U <> "":U &THEN
            DO WITH FRAME fPage1:
                DISPLAY {&page1Fields}.
            END.
        &ENDIF
        
        /*--- Exibe campos contidos no {&page2Fields} em tela ---*/
        &IF "{&page2Fields}":U <> "":U &THEN
            DO WITH FRAME fPage2:
                DISPLAY {&page2Fields}.
            END.
        &ENDIF
        
        /*--- Exibe campos contidos no {&page3Fields} em tela ---*/
        &IF "{&page3Fields}":U <> "":U &THEN
            DO WITH FRAME fPage3:
                DISPLAY {&page3Fields}.
            END.
        &ENDIF
        
        /*--- Exibe campos contidos no {&page4Fields} em tela ---*/
        &IF "{&page4Fields}":U <> "":U &THEN
            DO WITH FRAME fPage4:
                DISPLAY {&page4Fields}.
            END.
        &ENDIF
        
        /*--- Exibe campos contidos no {&page5Fields} em tela ---*/
        &IF "{&page5Fields}":U <> "":U &THEN
            DO WITH FRAME fPage5:
                DISPLAY {&page5Fields}.
            END.
        &ENDIF
        
        /*--- Exibe campos contidos no {&page6Fields} em tela ---*/
        &IF "{&page6Fields}":U <> "":U &THEN
            DO WITH FRAME fPage6:
                DISPLAY {&page6Fields}.
            END.
        &ENDIF
        
        /*--- Exibe campos contidos no {&page7Fields} em tela ---*/
        &IF "{&page7Fields}":U <> "":U &THEN
            DO WITH FRAME fPage7:
                DISPLAY {&page7Fields}.
            END.
        &ENDIF
        
        /*--- Exibe campos contidos no {&page8Fields} em tela ---*/
        &IF "{&page8Fields}":U <> "":U &THEN
            DO WITH FRAME fPage8:
                DISPLAY {&page8Fields}.
            END.
        &ENDIF
        
        DELETE {&ttTable}.
    END.
    
    IF AVAILABLE {&ttTable} THEN 
         assign epc-rowid = {&ttTable}.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="AFTER-DISPLAY"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable}"
                     &RowidTable="epc-rowid"}
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    {method/override.i &Position="After"
                       &Procedure="displayFields"}
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-enableFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableFields Method-Library 
PROCEDURE enableFields :
/*------------------------------------------------------------------------------
  Purpose:     Habilita os campos da temp-table {&tt-table} 
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE hFieldEntryAux    AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lExecutedEntryAux AS LOGICAL NO-UNDO.
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    {method/override.i &Position="Before"
                       &Procedure="enableFields"}
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    IF AVAILABLE {&ttTable} THEN 
         assign epc-rowid = {&ttTable}.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="BEFORE-ENABLE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable}"
                     &RowidTable="epc-rowid"}
    
    /*--- Habilita campos contidos no {&page0KeyFields} ---*/
    &IF "{&page0KeyFields}":U <> "":U &THEN
        IF cAction = "ADD":U OR cAction = "COPY":U THEN DO WITH FRAME fPage0:
            ENABLE {&page0KeyFields}.
        END.
    &ENDIF
    
    /*--- Habilita campos contidos no {&page0Fields} ---*/
    &IF "{&page0Fields}":U <> "":U &THEN
        DO WITH FRAME fPage0:
            ENABLE {&page0Fields}.
        END.
    &ENDIF
    
    /*--- Habilita campos contidos no {&page1Fields} ---*/
    &IF "{&page1Fields}":U <> "":U &THEN
        DO WITH FRAME fPage1:
            ENABLE {&page1Fields}.
        END.
    &ENDIF
    
    /*--- Habilita campos contidos no {&page2Fields} ---*/
    &IF "{&page2Fields}":U <> "":U &THEN
        DO WITH FRAME fPage2:
            ENABLE {&page2Fields}.
        END.
    &ENDIF
    
    /*--- Habilita campos contidos no {&page3Fields} ---*/
    &IF "{&page3Fields}":U <> "":U &THEN
        DO WITH FRAME fPage3:
            ENABLE {&page3Fields}.
        END.
    &ENDIF
    
    /*--- Habilita campos contidos no {&page4Fields} ---*/
    &IF "{&page4Fields}":U <> "":U &THEN
        DO WITH FRAME fPage4:
            ENABLE {&page4Fields}.
        END.
    &ENDIF
    
    /*--- Habilita campos contidos no {&page5Fields} ---*/
    &IF "{&page5Fields}":U <> "":U &THEN
        DO WITH FRAME fPage5:
            ENABLE {&page5Fields}.
        END.
    &ENDIF
    
    /*--- Habilita campos contidos no {&page6Fields} ---*/
    &IF "{&page6Fields}":U <> "":U &THEN
        DO WITH FRAME fPage6:
            ENABLE {&page6Fields}.
        END.
    &ENDIF
    
    /*--- Habilita campos contidos no {&page7Fields} ---*/
    &IF "{&page7Fields}":U <> "":U &THEN
        DO WITH FRAME fPage7:
            ENABLE {&page7Fields}.
        END.
    &ENDIF
    
    /*--- Habilita campos contidos no {&page8Fields} ---*/
    &IF "{&page8Fields}":U <> "":U &THEN
        DO WITH FRAME fPage8:
            ENABLE {&page8Fields}.
        END.
    &ENDIF
    
    &IF "{&page0KeyFields}":U <> "":U &THEN
        /*--- Executar o ENTRY para o primeiro campo ---*/
        ASSIGN hFieldEntryAux = FRAME fPage0:FIRST-CHILD.
        DO WHILE VALID-HANDLE(hFieldEntryAux):
            IF (hFieldEntryAux:TYPE = "FILL-IN":U OR
                hFieldEntryAux:TYPE = "COMBO-BOX":U OR
                hFieldEntryAux:TYPE = "TOGGLE-BOX":U OR
                hFieldEntryAux:TYPE = "RADIO-SET":U) AND
               (hFieldEntryAux:FRAME = FRAME fPage0:HANDLE) THEN
                IF hFieldEntryAux:SENSITIVE = YES THEN DO:
                    /*--- Corrigir BUG PROGRESS, pois o ENTRY nÆo funciona enquanto 
                          o SET-WAIT-STATE est  em espera ---*/
                    SESSION:SET-WAIT-STATE("":U).
                    
                    APPLY "ENTRY":U TO hFieldEntryAux.
                    
                    ASSIGN lExecutedEntryAux = YES.
                    
                    /*--- Corrigir BUG PROGRESS, pois o ENTRY nÆo funciona enquanto 
                          o SET-WAIT-STATE est  em espera ---*/
                    SESSION:SET-WAIT-STATE("GENERAL":U).
                    
                    LEAVE.
                END.
            
            IF hFieldEntryAux:TYPE = "FIELD-GROUP":U THEN
                ASSIGN hFieldEntryAux = hFieldEntryAux:FIRST-CHILD.
            ELSE
                ASSIGN hFieldEntryAux = hFieldEntryAux:NEXT-SIBLING.
        END.
    &ENDIF
    
    &IF "{&page0Fields}":U <> "":U &THEN
        /*--- Executar o ENTRY para o primeiro campo ---*/
        IF NOT lExecutedEntryAux THEN DO:
            ASSIGN hFieldEntryAux = FRAME fPage0:FIRST-CHILD.
            
            DO WHILE VALID-HANDLE(hFieldEntryAux):
                IF (hFieldEntryAux:TYPE = "FILL-IN":U OR
                    hFieldEntryAux:TYPE = "COMBO-BOX":U OR
                    hFieldEntryAux:TYPE = "TOGGLE-BOX":U OR
                    hFieldEntryAux:TYPE = "RADIO-SET":U) AND
                   (hFieldEntryAux:FRAME = FRAME fPage0:HANDLE) THEN
                    IF hFieldEntryAux:SENSITIVE = YES THEN DO:
                        /*--- Corrigir BUG PROGRESS, pois o ENTRY nÆo funciona 
                              enquanto o SET-WAIT-STATE est  em espera ---*/
                        SESSION:SET-WAIT-STATE("":U).
                        APPLY "ENTRY":U TO hFieldEntryAux.
                        
                        ASSIGN lExecutedEntryAux = YES.
                        
                        /*--- Corrigir BUG PROGRESS, pois o ENTRY nÆo funciona enquanto 
                              o SET-WAIT-STATE est  em espera ---*/
                        SESSION:SET-WAIT-STATE("GENERAL":U).
                        
                        LEAVE.
                    END.
                
                IF hFieldEntryAux:TYPE = "FIELD-GROUP":U THEN
                    ASSIGN hFieldEntryAux = hFieldEntryAux:FIRST-CHILD.
                ELSE
                    ASSIGN hFieldEntryAux = hFieldEntryAux:NEXT-SIBLING.
            END.
        END.
    &ENDIF
    
    IF AVAILABLE {&ttTable} THEN 
         assign epc-rowid = {&ttTable}.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="AFTER-ENABLE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable}"
                     &RowidTable="epc-rowid"}
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    {method/override.i &Position="After"
                       &Procedure="enableFields"}
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    &IF "{&mguni_version}" >= "2.06b" OR "{&aplica_facelift}" = "YES" &THEN
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
    IF VALID-HANDLE(h-faceLift) THEN DO:
        &IF "{&page0Fields}":U <> "":U OR
            "{&page0KeyFields}":U <> "":U &THEN
               RUN pi_change_state_color IN h-facelift (INPUT FRAME fPage0:HANDLE, YES).
        &ENDIF
        /*--- Desabilita campos contidos no {&page1Fields} ---*/
        &IF "{&page1Fields}":U <> "":U &THEN
               RUN pi_change_state_color IN h-facelift (INPUT FRAME fPage1:HANDLE, YES).
        &ENDIF
        
        /*--- Desabilita campos contidos no {&page2Fields} ---*/
        &IF "{&page2Fields}":U <> "":U &THEN
               RUN pi_change_state_color IN h-facelift (INPUT FRAME fPage2:HANDLE, YES).
        &ENDIF
        
        /*--- Desabilita campos contidos no {&page3Fields} ---*/
        &IF "{&page3Fields}":U <> "":U &THEN
               RUN pi_change_state_color IN h-facelift (INPUT FRAME fPage3:HANDLE, YES).
        &ENDIF
        
        /*--- Desabilita campos contidos no {&page4Fields} ---*/
        &IF "{&page4Fields}":U <> "":U &THEN
               RUN pi_change_state_color IN h-facelift (INPUT FRAME fPage4:HANDLE, YES).
        &ENDIF
        
        /*--- Desabilita campos contidos no {&page5Fields} ---*/
        &IF "{&page5Fields}":U <> "":U &THEN
               RUN pi_change_state_color IN h-facelift (INPUT FRAME fPage5:HANDLE, YES).
        &ENDIF
        
        /*--- Desabilita campos contidos no {&page6Fields} ---*/
        &IF "{&page6Fields}":U <> "":U &THEN
               RUN pi_change_state_color IN h-facelift (INPUT FRAME fPage6:HANDLE, YES).
        &ENDIF
        
        /*--- Desabilita campos contidos no {&page7Fields} ---*/
        &IF "{&page7Fields}":U <> "":U &THEN
               RUN pi_change_state_color IN h-facelift (INPUT FRAME fPage7:HANDLE, YES).
        &ENDIF
        
        /*--- Desabilita campos contidos no {&page8Fields} ---*/
        &IF "{&page8Fields}":U <> "":U &THEN
               RUN pi_change_state_color IN h-facelift (INPUT FRAME fPage8:HANDLE, YES).
        &ENDIF    
    END.
    &ENDIF
    /*********fim altera‡Æo*************/

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initializeInterface) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeInterface Method-Library 
PROCEDURE initializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     Inicialize programa
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

    /*Alterado por Anderson (tech485) para o template mostrar o nome da empresa*/

    def var c_cod_empres_usuar as char no-undo.
    def var c_nom_razao_social as char no-undo.

    /*fim alteracao Anderson*/

    /*--- Inicializa‡Æo de OCXs ---*/
    IF THIS-PROCEDURE:GET-SIGNATURE("control_load":U) <> "":U THEN DO:
        RUN control_load IN THIS-PROCEDURE NO-ERROR.
        VIEW FRAME fPage0 IN WINDOW wMaintenance.
    END.
    
    /*--- Executa valida‡äes de inicializa‡Æo ---*/
    ASSIGN c-programa-mg97 = CAPS("{&Program}":U)
           c-versao-mg97   = "{&Version}":U.

    {utp/ut-vfsec.i}

    /*Inserida a chamada da include i-logini.i
    devido aos thintemplates nÆo gerarem log de 
    execucao de programas*/
    {include/i-logini.i}
    
    
    /*Alterado por Anderson (tech485) para o template mostrar o nome da empresa*/
    /*{utp/ut9000.i "{&Program}" "{&Version}"} *//*esta foi comentada por estar causando erro nos thintemplates*/
    /*rodar pi-rsocial persistent para verifica»’o empresa usuario*/
    /* alterado por Valdir (tech264) novo m‚todo de teste do valid-handle */
   
    if not valid-handle(h-rsocial) or
       h-rsocial:TYPE <> "PROCEDURE":U or
       h-rsocial:FILE-NAME <> "utp/ut-rsocial.p":U then do:
        if l-achou-prog then
            run utp/ut-rsocial.p persistent set h-rsocial.
    end.
    if l-achou-prog then
        run pi-rsocial in h-rsocial (output c_cod_empres_usuar, output c_nom_razao_social).
    
    find prog_dtsul no-lock
        where prog_dtsul.cod_prog_dtsul = c-programa-mg97 no-error.
    if  avail prog_dtsul then do:
        assign c-titulo-prog-mg97    = prog_dtsul.des_prog_dtsul
               c-nom-prog-upc-mg97   = prog_dtsul.nom_prog_upc
               c-nom-prog-appc-mg97  = prog_dtsul.nom_prog_appc
               c-nom-prog-dpc-mg97   = prog_dtsul.nom_prog_dpc
               i-num-topico-hlp-mg97 = prog_dtsul.num_topico.
       &IF DEFINED(TransformacaoWindow) <> 0 &THEN
          if session:window-system <> "TTY":U then 
          &IF '{&emsfnd_version}' >= '1.00'  &THEN
             assign i-template          = prog_dtsul.idi_template.
          &ELSE
             /*assign i-template          = prog_dtsul.ind_template.*/
          &ENDIF
       &ENDIF

        
        find procedimento no-lock
            where procedimento.cod_proced = prog_dtsul.cod_proced no-error.
        if  avail procedimento then do:
            find modul_dtsul no-lock
                where modul_dtsul.cod_modul_dtsul = procedimento.cod_modul_dtsul no-error. 
            if  avail modul_dtsul then do:
                assign c-modulo-mg97         = caps(modul_dtsul.nom_modul_dtsul_menu)
                       c-cod-mod-mg97        = caps(modul_dtsul.cod_modul_dtsul)
                       c-nom-manual-hlp-mg97 = "dochlp~/":U + string(modul_dtsul.num_manual_documen, "999999":U) + ".hlp":U.
            end.
        end.
    end.                                                      
    else do:
        assign c-titulo-prog-mg97    = caps(c-programa-mg97)
               c-nom-prog-upc-mg97   = ""
               c-nom-prog-appc-mg97  = ""
               i-num-topico-hlp-mg97 = 0
               c-nom-manual-hlp-mg97 = "dochlp~/000000.hlp":U.
    end.                 
    /* Tradu‡Æo T¡tulo dos Programas */

    /* FO 1354855  - 11/08/2006 - tech1139 */
    run utp/ut-liter.p (input replace(c-titulo-prog-mg97, " ", "_") ,
                        input "*":U,
                        input "":U). 
    /* FO 1354855  - 11/08/2006 - tech1139 */

    Assign c-titulo-prog-mg97 = Return-value.
     
    &IF  "{&WINDOW-NAME}":U <> "" AND "{&WINDOW-NAME}":U <> "CURRENT-WINDOW":U &THEN 
         assign {&WINDOW-NAME}:title = if l-achou-prog then
                                       c-titulo-prog-mg97
                                     + " - ":U 
                                     + c-programa-mg97 
                                     + " - ":U 
                                     + c-versao-mg97  
                                     + " - ":U 
                                     + c_cod_empres_usuar
                                     + " - ":U 
                                     + c_nom_razao_social
                                     else 
                                       c-titulo-prog-mg97
                                     + " - ":U 
                                     + c-programa-mg97 
                                     + " - ":U 
                                     + c-versao-mg97.
    &ELSE
         assign frame {&FRAME-NAME}:title = if l-achou-prog then
                                              c-titulo-prog-mg97  
                                            + " - ":U 
                                            + c-programa-mg97 
                                            + " - ":U 
                                            + c-versao-mg97
                                            + " - ":U 
                                            + c_cod_empres_usuar
                                            + " - ":U 
                                            + c_nom_razao_social
                                            else
                                              c-titulo-prog-mg97  
                                            + " - ":U 
                                            + c-programa-mg97 
                                            + " - ":U 
                                            + c-versao-mg97.

    &ENDIF
    

    /*fim alteracao Anderson(tech485)*/
    
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    {method/override.i &Position="Before"
                       &Procedure="initializeInterface"}
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN DO:
        /*--- Destr¢i programa ---*/
        RUN destroyInterface IN THIS-PROCEDURE.
        
        RETURN "NOK":U.
    END.
    
    IF AVAILABLE {&ttTable} THEN 
         assign epc-rowid = {&ttTable}.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="BEFORE-INITIALIZE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable}"
                     &RowidTable="epc-rowid"}
    
    DO WITH FRAME fPage0:
        
        /*--- Habilitar/Desabilitar botäes/menus ---*/ 
        ASSIGN
            &IF "{&First}":U = "YES":U &THEN
                btFirst:SENSITIVE                          = NO
                MENU-ITEM miFirst:SENSITIVE IN MENU mbMain = NO
            &ENDIF
            
            &IF "{&Prev}":U = "YES":U &THEN
                btPrev:SENSITIVE                          = NO
                MENU-ITEM miPrev:SENSITIVE IN MENU mbMain = NO
            &ENDIF
            
            &IF "{&Next}":U = "YES":U &THEN
                btNext:SENSITIVE                          = NO
                MENU-ITEM miNext:SENSITIVE IN MENU mbMain = NO
            &ENDIF
            
            &IF "{&Last}":U = "YES":U &THEN
                btLast:SENSITIVE                          = NO
                MENU-ITEM miLast:SENSITIVE IN MENU mbMain = NO
            &ENDIF
            
            &IF "{&GoTo}":U = "YES":U &THEN
                btGoTo:SENSITIVE                          = NO
                MENU-ITEM miGoTo:SENSITIVE IN MENU mbMain = NO
            &ENDIF
            
            &IF "{&Search}":U = "YES":U &THEN
                btSearch:SENSITIVE                          = NO
                MENU-ITEM miSearch:SENSITIVE IN MENU mbMain = NO
            &ENDIF
            
            &IF "{&Add}":U = "YES":U &THEN
                btAdd:SENSITIVE                          = NO
                MENU-ITEM miAdd:SENSITIVE IN MENU mbMain = NO
            &ENDIF
            
            &IF "{&Copy}":U = "YES":U &THEN
                btCopy:SENSITIVE                          = NO
                MENU-ITEM miCopy:SENSITIVE IN MENU mbMain = NO
            &ENDIF
            
            &IF "{&Update}":U = "YES":U &THEN
                btUpdate:SENSITIVE                          = NO
                MENU-ITEM miUpdate:SENSITIVE IN MENU mbMain = NO
            &ENDIF
            
            &IF "{&Delete}":U = "YES":U &THEN
                btDelete:SENSITIVE                          = NO
                MENU-ITEM miDelete:SENSITIVE IN MENU mbMain = NO
            &ENDIF
            
            &IF "{&Undo}":U = "YES":U &THEN
                btUndo:SENSITIVE                          = NO
                MENU-ITEM miUndo:SENSITIVE IN MENU mbMain = NO
            &ENDIF
            
            &IF "{&Cancel}":U = "YES":U &THEN
                btCancel:SENSITIVE                          = NO
                MENU-ITEM miCancel:SENSITIVE IN MENU mbMain = NO
            &ENDIF
            
            &IF "{&Save}":U = "YES":U &THEN
                btSave:SENSITIVE                          = NO
                MENU-ITEM miSave:SENSITIVE IN MENU mbMain = NO
            &ENDIF
            
            /*tech1139 - 05/12/2006 - FO 1387.046 */
            /*tech1139 - 09/02/2007 - FO 1453.143 
            &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN
                btQueryJoins:SENSITIVE                            = NO
                MENU-ITEM miQueryJoins:SENSITIVE IN MENU mbMain   = NO
            &ENDIF
            
            &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN
                btReportsJoins:SENSITIVE                          = NO
                MENU-ITEM miReportsJoins:SENSITIVE IN MENU mbMain = NO
            &ENDIF
            FIM - tech1139 - 09/02/2007 - FO 1453.143 */
            
            btExit:SENSITIVE                                  = YES
            MENU-ITEM miExit:SENSITIVE IN MENU mbMain         = YES
            
            
            btHelp:SENSITIVE                                  = YES
            MENU-ITEM miContents:SENSITIVE IN MENU mbMain     = YES
            /*tech1139 - 05/12/2006 - FO 1387.046 */

            .
        /*--- Posicionar widgets ---*/
        ASSIGN 
            rtToolBar:WIDTH  = FRAME fPage0:WIDTH
            rtToolBar:HEIGHT = 1.46
            rtToolBar:COL    = 1
            rtToolBar:ROW    = 1
            
            &IF "{&First}":U = "YES":U &THEN
                btFirst:ROW    = 1.12
                btFirst:COL    = deColPanelNav
                &IF "{&mguni_version}" >= "2.06b" OR "{&aplica_facelift}" = "YES" &THEN
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
                btFirst:HEIGHT = 1.13
                &ELSE
                btFirst:HEIGHT = 1.25
                &ENDIF
                btFirst:WIDTH  = 4
                deColPanelNav  = deColPanelNav + 4
            &ENDIF
            
            &IF "{&Prev}":U = "YES":U &THEN
                btPrev:ROW    = 1.12
                btPrev:COL    = deColPanelNav
                &IF "{&mguni_version}" >= "2.06b" OR "{&aplica_facelift}" = "YES" &THEN
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
                btPrev:HEIGHT = 1.13
                &ELSE
                btPrev:HEIGHT = 1.25
                &ENDIF
                btPrev:WIDTH  = 4
                deColPanelNav = deColPanelNav + 4
            &ENDIF
            
            &IF "{&Next}":U = "YES":U &THEN
                btNext:ROW    = 1.12
                btNext:COL    = deColPanelNav
                &IF "{&mguni_version}" >= "2.06b" OR "{&aplica_facelift}" = "YES" &THEN
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
                btNext:HEIGHT = 1.13
                &ELSE
                btNext:HEIGHT = 1.25
                &ENDIF
                btNext:WIDTH  = 4
                deColPanelNav = deColPanelNav + 4 
            &ENDIF
            
            &IF "{&Last}":U = "YES":U &THEN
                btLast:ROW    = 1.12
                btLast:COL    = deColPanelNav
                &IF "{&mguni_version}" >= "2.06b" OR "{&aplica_facelift}" = "YES" &THEN
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
                btLast:HEIGHT = 1.13
                &ELSE
                btLast:HEIGHT = 1.25
                &ENDIF
                btLast:WIDTH  = 4
                deColPanelNav = deColPanelNav + 4 
            &ENDIF
            
            &IF "{&GoTo}":U = "YES":U &THEN
                btGoTo:ROW    = 1.12
                btGoTo:COL    = deColPanelNav
                &IF "{&mguni_version}" >= "2.06b" OR "{&aplica_facelift}" = "YES" &THEN
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
                btGoTo:HEIGHT = 1.13
                &ELSE
                btGoTo:HEIGHT = 1.25
                &ENDIF
                btGoTo:WIDTH  = 4
                deColPanelNav = deColPanelNav + 4 
            &ENDIF
            
            &IF "{&Search}":U = "YES":U &THEN
                btSearch:ROW    = 1.12
                btSearch:COL    = deColPanelNav
                &IF "{&mguni_version}" >= "2.06b" OR "{&aplica_facelift}" = "YES" &THEN
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
                btSearch:HEIGHT = 1.13
                &ELSE
                btSearch:HEIGHT = 1.25
                &ENDIF
                btSearch:WIDTH  = 4
                deColPanelNav   = deColPanelNav + 4
            &ENDIF
            
            &IF "{&Add}":U = "YES":U &THEN
                btAdd:ROW    = 1.12
                btAdd:COL    = deColPanel
                &IF "{&mguni_version}" >= "2.06b" OR "{&aplica_facelift}" = "YES" &THEN
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
                btAdd:HEIGHT = 1.13
                &ELSE
                btAdd:HEIGHT = 1.25
                &ENDIF
                btAdd:WIDTH  = 4
                deColPanel   = deColPanel + 4
            &ENDIF
            
            &IF "{&Copy}":U = "YES":U &THEN
                btCopy:ROW    = 1.12
                btCopy:COL    = deColPanel
                &IF "{&mguni_version}" >= "2.06b" OR "{&aplica_facelift}" = "YES" &THEN
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
                btCopy:HEIGHT = 1.13
                &ELSE
                btCopy:HEIGHT = 1.25
                &ENDIF
                btCopy:WIDTH  = 4
                deColPanel    = deColPanel + 4
            &ENDIF
            
            &IF "{&Update}":U = "YES":U &THEN
                btUpdate:ROW    = 1.12
                btUpdate:COL    = deColPanel
                &IF "{&mguni_version}" >= "2.06b" OR "{&aplica_facelift}" = "YES" &THEN
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
                btUpdate:HEIGHT = 1.13
                &ELSE
                btUpdate:HEIGHT = 1.25
                &ENDIF
                btUpdate:WIDTH  = 4
                deColPanel      = deColPanel + 4
            &ENDIF
            
            &IF "{&Delete}":U = "YES":U &THEN
                btDelete:ROW    = 1.12
                btDelete:COL    = deColPanel
                &IF "{&mguni_version}" >= "2.06b" OR "{&aplica_facelift}" = "YES" &THEN
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
                btDelete:HEIGHT = 1.13
                &ELSE
                btDelete:HEIGHT = 1.25
                &ENDIF
                btDelete:WIDTH  = 4
                deColPanel      = deColPanel + 4
            &ENDIF
            
            &IF "{&Undo}":U = "YES":U &THEN
                btUndo:ROW    = 1.12
                btUndo:COL    = deColPanel
                &IF "{&mguni_version}" >= "2.06b" OR "{&aplica_facelift}" = "YES" &THEN
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
                btUndo:HEIGHT = 1.13
                &ELSE
                btUndo:HEIGHT = 1.25
                &ENDIF
                btUndo:WIDTH  = 4
                deColPanel    = deColPanel + 4
            &ENDIF
            
            &IF "{&Cancel}":U = "YES":U &THEN
                btCancel:ROW    = 1.12
                btCancel:COL    = deColPanel
                &IF "{&mguni_version}" >= "2.06b" OR "{&aplica_facelift}" = "YES" &THEN
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
                btCancel:HEIGHT = 1.13
                &ELSE
                btCancel:HEIGHT = 1.25
                &ENDIF
                btCancel:WIDTH  = 4
                deColPanel      = deColPanel + 4
            &ENDIF
            
            &IF "{&Save}":U = "YES":U &THEN
                btSave:ROW    = 1.12
                btSave:COL    = deColPanel
                &IF "{&mguni_version}" >= "2.06b" OR "{&aplica_facelift}" = "YES" &THEN
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
                btSave:HEIGHT = 1.13
                &ELSE
                btSave:HEIGHT = 1.25
                &ENDIF
                btSave:WIDTH  = 4
                deColPanel    = deColPanel + 4
            &ENDIF
            

            &IF "{&Help}":U = "YES":U &THEN /*tech30713 - 08/12/2006 - FO 1387046 */
                btHelp:ROW            = 1.12
                btHelp:COL            = FRAME fPage0:WIDTH - 3.35
                &IF "{&mguni_version}" >= "2.06b" OR "{&aplica_facelift}" = "YES" &THEN
    /**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
                btHelp:HEIGHT         = 1.13
                &ELSE
                btHelp:HEIGHT         = 1.25
                &ENDIF
                btHelp:WIDTH          = 4
            &ENDIF
            &IF "{&Exit}":U = "YES":U &THEN /*tech30713 - 08/12/2006 - FO 1387046 */
                btExit:ROW            = 1.12 
                btExit:COL            = btHelp:COL  - 4 
                &IF "{&mguni_version}" >= "2.06b" OR "{&aplica_facelift}" = "YES" &THEN
    /**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
                btExit:HEIGHT         = 1.13
                &ELSE
                btExit:HEIGHT         = 1.25
                &ENDIF
                btExit:WIDTH          = 4
    
                deColPanel   = btExit:COL - 4
            &ENDIF
            
            /*tech30713 - 08/12/2006 - FO 1387046 */
            /*tech1139 - 09/02/2007 - FO 1453.143 
            &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN
                btReportsJoins:ROW    = 1.12 
                btReportsJoins:COL    = deColPanel
                &IF "{&mguni_version}" >= "2.06b" OR "{&aplica_facelift}" = "YES" &THEN
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
                btReportsJoins:HEIGHT = 1.13
                &ELSE
                btReportsJoins:HEIGHT = 1.25
                &ENDIF
                btReportsJoins:WIDTH  = 4
                deColPanel            = deColPanel - 4
            &ENDIF

            
            &IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN
                btQueryJoins:ROW      = 1.12 
                btQueryJoins:COL      = deColPanel
                &IF "{&mguni_version}" >= "2.06b" OR "{&aplica_facelift}" = "YES" &THEN
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
                btQueryJoins:HEIGHT   = 1.13
                &ELSE
                btQueryJoins:HEIGHT   = 1.25
                &ENDIF
                btQueryJoins:WIDTH    = 4
            &ENDIF
            FIM - tech1139 - 09/02/2007 - FO 1453.143 */

            .
        /*tech30713 - 08/12/2006 - FO 1387046 */
    END.
    
    /*--- Executa programa de folder ---*/
    &IF "{&Folder}":U = "YES":U &THEN
        RUN utp/thinfolder.w PERSISTENT SET hFolder.
        
        RUN setProgramParent IN hFolder (INPUT THIS-PROCEDURE).
        
        RUN setOCX IN hFolder (INPUT THIS-PROCEDURE:GET-SIGNATURE("control_load":U) <> "":U) NO-ERROR.
        
        RUN initializeFolders IN hFolder (INPUT FRAME fPage0:HANDLE,
                                          INPUT &IF NUM-ENTRIES("{&FolderLabels}":U) >= 1 &THEN
                                                    STRING(FRAME fPage1:HANDLE)
                                                &ENDIF

                                                &IF NUM-ENTRIES("{&FolderLabels}":U) >= 2 &THEN
                                                        + ",":U + STRING(FRAME fPage2:HANDLE)
                                                &ENDIF

                                                &IF NUM-ENTRIES("{&FolderLabels}":U) >= 3 &THEN
                                                    + ",":U + STRING(FRAME fPage3:HANDLE)
                                                &ENDIF

                                                &IF NUM-ENTRIES("{&FolderLabels}":U) >= 4 &THEN
                                                    + ",":U + STRING(FRAME fPage4:HANDLE)
                                                &ENDIF

                                                &IF NUM-ENTRIES("{&FolderLabels}":U) >= 5 &THEN
                                                    + ",":U + STRING(FRAME fPage5:HANDLE)
                                                &ENDIF                     
 
                                                &IF NUM-ENTRIES("{&FolderLabels}":U) >= 6 &THEN
                                                    + ",":U + STRING(FRAME fPage6:HANDLE)
                                                &ENDIF                     
 
                                                &IF NUM-ENTRIES("{&FolderLabels}":U) >= 7 &THEN
                                                    + ",":U + STRING(FRAME fPage7:HANDLE)
                                                &ENDIF                     
 
                                                &IF NUM-ENTRIES("{&FolderLabels}":U) = 8 &THEN
                                                    + ",":U + STRING(FRAME fPage8:HANDLE)
                                                &ENDIF,
                                          INPUT "{&FolderLabels}":U,
                                          INPUT NUM-ENTRIES("{&FolderLabels}":U),
                                          INPUT ?,
                                          INPUT ?,
                                          INPUT ?,
                                          INPUT ?).
    &ENDIF
    
    /*--- Executa programa de estilo de janelas ---*/
    IF  VALID-HANDLE(hWindowStyles) = NO OR
        hWindowStyles:TYPE <> "PROCEDURE":U OR
        (hWindowStyles:FILE-NAME <> "utp/WindowStyles.p":U AND
        hWindowStyles:FILE-NAME <> "utp/WindowStyles.r":U) THEN
        RUN utp/windowstyles.p PERSISTENT SET hWindowStyles.
    
    /*TECH14187 - FO1468831 - Permite maximizar se o pr‚-processador estiver definido como YES*/
    &IF "{&EnableMaxButton}" NE "YES" &THEN
        /*--- Seta estilo de janela para thinMaintenance ---*/
        RUN disableMax IN hWindowStyles (INPUT wMaintenance:hWnd).
    &ENDIF
    /*FIM TECH14187 FO1468831*/
    
    /*Colocado esta l¢gica para caso o desenvolvedor passar o handle da bo j  
     estartado o mesmo nÆo mostrar erros de execu‡äes antigas sa inicializa‡Æo 
     do programa Feita por Anderson tech485 28/08/2202*/
    &IF DEFINED(DBOVersion) = 0 &THEN
        &IF "{&hDBOTable}":U <> "":U &THEN
            IF VALID-HANDLE({&hDBOTable}) THEN
                RUN emptyrowerrors IN {&hDBOTable} NO-ERROR.
        &ENDIF
    &ENDIF
    /*fim altera‡Æo Anderson tech485 28/08/2202*/

    /*--- Inicializa DBOs ---*/
    RUN initializeDBOs IN THIS-PROCEDURE.
    
    /*--- Verifica se a inicializa‡Æo do DBO foi feita com sucesso ---*/
    &IF "{&hDBOTable}":U <> "":U &THEN
        IF NOT VALID-HANDLE({&hDBOTable}) THEN DO:
            /*--- Exibir mensagem de finaliza‡Æo do programa ---*/
            RUN utp/ut-msgs.p (INPUT "SHOW":U, 
                               INPUT 18881, 
                               INPUT CAPS("{&Program}":U) + "~~":U + CAPS("{&Version}":U)).
            
            /*--- Destr¢i programa ---*/
            RUN destroyInterface IN THIS-PROCEDURE.
            
            RETURN "NOK":U.
        END.
        &IF DEFINED(DBOVersion) = 0 &THEN
        ELSE DO:
            /*Foi inclu¡da esta l¢gica para verifica‡Æo de erros relacionados com 
            a permissÆo de execu‡Æo do programa de dbo Altera‡Æo em 17/06/2002 por Anderson(tech540)*/
            RUN getrowerrors IN {&hDBOTable} (OUTPUT TABLE rowerrors).
            IF CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorNumber <> 3 AND
                                              RowErrors.ErrorNumber <> 8 AND
                                              RowErrors.ErrorNumber <> 10 AND
                                              RowErrors.ErrorSubType = "ERROR":U) THEN DO:
               {method/showmessage.i1}
               {method/showmessage.i2}

               /****
                TECH14187 - 1538059
                Problema da ShowMessage Modal
                ****/
               &IF "{&Modal}":U = "":U &THEN
                   WAIT-FOR CLOSE OF hShowMsg.
               &ENDIF

               {method/showmessage.i3}
                RUN destroyInterface IN THIS-PROCEDURE.
                RETURN "NOK":U.
            END.
        END.
        &ENDIF
    &ENDIF
    
    /*--- Reposicionamento autom tico atrav‚s de vari vel global ---*/
    IF {&rTable} <> ? THEN DO:
        /*--- Reposiciona DBO atrav‚s de vari vel global ---*/
        RUN repositionRecord IN THIS-PROCEDURE (INPUT {&rTable}).
        
        &IF "{&First}":U = "YES":U &THEN        
            IF RETURN-VALUE = "NOK":U THEN
                APPLY "CHOOSE":U TO btFirst IN FRAME fPage0.
        &ENDIF
    END.
    
    &IF "{&First}":U = "YES":U &THEN
        ELSE DO:
            APPLY "CHOOSE":U TO btFirst IN FRAME fPage0.
        END.
    &ELSE
        ELSE DO:
            RUN displayFields IN THIS-PROCEDURE.
        END.
    &ENDIF
    
    &IF "{&Folder}":U = "YES":U &THEN
        RUN setFolder IN hFolder (INPUT {&InitialPage}).
    &ENDIF
    
    IF AVAILABLE {&ttTable} THEN 
         assign epc-rowid = {&ttTable}.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="AFTER-INITIALIZE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable}"
                     &RowidTable="epc-rowid"}
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    {method/override.i &Position="After"
                       &Procedure="initializeInterface"}
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN DO:
        /*--- Destr¢i programa ---*/
        RUN destroyInterface IN THIS-PROCEDURE.
        
        RETURN "NOK":U.
    END.

    /*** Alterado por Farley - em 23/07/2003 ***/
    &IF "{&FNC_MULTI_IDIOMA}":U = "YES":U &THEN
        /*** Alterado por Paulo H. Lazzarotti em 09/07/2003
         Esta valida‡Æo ‚ provis¢ria, at‚ que todos os clientes
         tenham este utilit rio em seus ambientes ****/
        IF SEARCH("utp/ut-trcampos.r":U) <> ? OR
           SEARCH("utp/ut-trcampos.p":U) <> ? THEN
            /*ALtera‡Æo Anderson  tech540 em 12/05/2003 para o projeto de 
            tradu‡Æo onde este ir  traduzir os valores (labels) dos objetos em telas*/
            RUN utp/ut-trcampos.p.
            /*fim altera‡Æo Anderson 12/05/2003*/
    &ENDIF
    
    /*--- Visualiza janela ---*/
    VIEW wMaintenance.
    APPLY "ENTRY":U TO FRAME fPage0.
    APPLY "ENTRY":U TO wMaintenance.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-piTraduz) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piTraduz Method-Library 
PROCEDURE piTraduz :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*******
    Altera‡Æo 07/12/2005 - tech14187 - Altera‡Æo para evitar estouro de segmento
******/

/*Alteracao 27/07/2005 - tech1007 - Alteracao para fazer a traducao dos templates*/
&IF "{&First}":U = "YES":U &THEN
    RUN utp/ut-liter.p (INPUT "Primeira ocorrˆncia", "*", "R").
    btFirst:TOOLTIP in frame {&frame-name} = RETURN-VALUE.
    btFirst:HELP IN FRAME {&FRAME-NAME} = RETURN-VALUE.
&ENDIF
&IF "{&Prev}":U = "YES":U &THEN
    RUN utp/ut-liter.p (INPUT "Ocorrˆncia anterior", "*", "R").    
    btPrev:TOOLTIP in frame {&frame-name} = RETURN-VALUE.
    btPrev:HELP IN FRAME {&FRAME-NAME} = RETURN-VALUE.
&ENDIF        
&IF "{&Next}":U = "YES":U &THEN
    RUN utp/ut-liter.p (INPUT "Pr¢xima ocorrˆncia", "*", "R").    
    btNext:TOOLTIP in frame {&frame-name} = RETURN-VALUE.
    btNext:HELP IN FRAME {&FRAME-NAME} = RETURN-VALUE.
&ENDIF
&IF "{&Last}":U = "YES":U &THEN
    RUN utp/ut-liter.p (INPUT "éltima ocorrˆncia", "*", "R").    
    btLast:TOOLTIP in frame {&frame-name} = RETURN-VALUE.
    btLast:HELP IN FRAME {&FRAME-NAME} = RETURN-VALUE.
&ENDIF
&IF "{&GoTo}":U = "YES":U &THEN
    RUN utp/ut-liter.p (INPUT "V  para", "*", "R").    
    btGoTo:TOOLTIP in frame {&frame-name} = RETURN-VALUE.
    btGoTo:HELP IN FRAME {&FRAME-NAME} = RETURN-VALUE.
&ENDIF
&IF "{&Search}":U = "YES":U &THEN
    RUN utp/ut-liter.p (INPUT "Pesquisa", "*", "R").    
    btSearch:TOOLTIP in frame {&frame-name} = RETURN-VALUE.
    btSearch:HELP IN FRAME {&FRAME-NAME} = RETURN-VALUE.
&ENDIF
&IF "{&Add}":U = "YES":U &THEN
    RUN utp/ut-liter.p (INPUT "Inclui nova ocorrˆncia", "*", "R").    
    btAdd:TOOLTIP in frame {&frame-name} = RETURN-VALUE.
    btAdd:HELP IN FRAME {&FRAME-NAME} = RETURN-VALUE.
&ENDIF
&IF "{&Copy}":U = "YES":U &THEN
    RUN utp/ut-liter.p (INPUT "Cria uma c¢pia da ocorrˆncia corrente", "*", "R").    
    btCopy:TOOLTIP in frame {&frame-name} = RETURN-VALUE.
    btCopy:HELP IN FRAME {&FRAME-NAME} = RETURN-VALUE.
&ENDIF     
&IF "{&Update}":U = "YES":U &THEN
    RUN utp/ut-liter.p (INPUT "Altera ocorrˆncia corrente", "*", "R").    
    btUpdate:TOOLTIP in frame {&frame-name} = RETURN-VALUE.
    btUpdate:HELP IN FRAME {&FRAME-NAME} = RETURN-VALUE.
&ENDIF
&IF "{&Delete}":U = "YES":U &THEN
    RUN utp/ut-liter.p (INPUT "Elimina ocorrˆncia corrente", "*", "R").    
    btDelete:TOOLTIP in frame {&frame-name} = RETURN-VALUE.
    btDelete:HELP IN FRAME {&FRAME-NAME} = RETURN-VALUE.
&ENDIF      
&IF "{&Undo}":U = "YES":U &THEN
    RUN utp/ut-liter.p (INPUT "Desfaz altera‡äes", "*", "R").    
    btUndo:TOOLTIP in frame {&frame-name} = RETURN-VALUE.
    btUndo:HELP IN FRAME {&FRAME-NAME} = RETURN-VALUE.
&ENDIF                       
&IF "{&Cancel}":U = "YES":U &THEN
    RUN utp/ut-liter.p (INPUT "Cancela altera‡äes", "*", "R").    
    btCancel:TOOLTIP in frame {&frame-name} = RETURN-VALUE.
    btCancel:HELP IN FRAME {&FRAME-NAME} = RETURN-VALUE.
&ENDIF
&IF "{&Save}":U = "YES":U &THEN
    RUN utp/ut-liter.p (INPUT "Confirma altera‡äes", "*", "R").    
    btSave:TOOLTIP in frame {&frame-name} = RETURN-VALUE.
    btSave:HELP IN FRAME {&FRAME-NAME} = RETURN-VALUE.
&ENDIF

/*tech1139 - 09/02/2007 - FO 1453.143 */
&IF "{&ExcludeBtQueryJoins}":U <> "YES":U &THEN
    RUN utp/ut-liter.p (INPUT "Consultas relacionadas", "*", "R").    
    btQueryJoins:TOOLTIP in frame {&frame-name} = RETURN-VALUE.
    btQueryJoins:HELP IN FRAME {&FRAME-NAME} = RETURN-VALUE.
&ENDIF
&IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN                
    RUN utp/ut-liter.p (INPUT "Relat¢rios relacionados", "*", "R").    
    btReportsJoins:TOOLTIP in frame {&frame-name} = RETURN-VALUE.
    btReportsJoins:HELP IN FRAME {&FRAME-NAME} = RETURN-VALUE.
&ENDIF
/*tech1139 - 09/02/2007 - FO 1453.143 */


RUN utp/ut-liter.p (INPUT "Sair", "*", "R").    
btExit:TOOLTIP in frame {&frame-name} = RETURN-VALUE.
btExit:HELP IN FRAME {&FRAME-NAME} = RETURN-VALUE.
    
RUN utp/ut-liter.p (INPUT "Ajuda", "*", "R").    
btHelp:TOOLTIP in frame {&frame-name} = RETURN-VALUE.
btHelp:HELP IN FRAME {&FRAME-NAME} = RETURN-VALUE.
/*Fim alteracao 27/07/2005*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi_aplica_facelift) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_aplica_facelift Method-Library 
PROCEDURE pi_aplica_facelift :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
    define variable wg     as widget-handle no-undo.
       
&IF "{&mguni_version}" >= "2.06b" OR "{&aplica_facelift}" = "YES" &THEN
    &IF "{&page0Fields}":U <> "":U OR
        "{&page0KeyFields}":U <> "":U &THEN
        run pi_aplica_facelift_thin in h-facelift ( input frame fPage0:handle ).
    &ENDIF
    &IF "{&page1Fields}":U <> "":U &THEN
        run pi_aplica_facelift_thin in h-facelift ( input frame fPage1:handle ).
    &ENDIF
    &IF "{&page2Fields}":U <> "":U &THEN
        run pi_aplica_facelift_thin in h-facelift ( input frame fPage2:handle ).
    &ENDIF
    &IF "{&page3Fields}":U <> "":U &THEN
        run pi_aplica_facelift_thin in h-facelift ( input frame fPage3:handle ).
    &ENDIF
    &IF "{&page4Fields}":U <> "":U &THEN
        run pi_aplica_facelift_thin in h-facelift ( input frame fPage4:handle ).
    &ENDIF
    &IF "{&page5Fields}":U <> "":U &THEN
        run pi_aplica_facelift_thin in h-facelift ( input frame fPage5:handle ).
    &ENDIF
    &IF "{&page6Fields}":U <> "":U &THEN
        run pi_aplica_facelift_thin in h-facelift ( input frame fPage6:handle ).
    &ENDIF
    &IF "{&page7Fields}":U <> "":U &THEN
        run pi_aplica_facelift_thin in h-facelift ( input frame fPage7:handle ).
    &ENDIF
    &IF "{&page8Fields}":U <> "":U &THEN
        run pi_aplica_facelift_thin in h-facelift ( input frame fPage8:handle ).
    &ENDIF
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-repositionRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repositionRecord Method-Library 
PROCEDURE repositionRecord :
/*------------------------------------------------------------------------------
  Purpose:     Reposiciona DBO atrav‚s de um rowid
  Parameters:  recebe rowid
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pRowid AS ROWID NO-UNDO.
    
    DEFINE VARIABLE cReturnAux  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE rCurrentAux AS ROWID     NO-UNDO.
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Posiciona query do BO 1.1 atrav‚s de rowid passado como parƒmetro ---*/
        RUN findRowid IN {&hDBOTable} (INPUT pRowid).
        
        /*--- Seta vari vel rCurrentDBO com o valor do rowid corrento do BO 1.1
              Caso a vari vel tenha o valor ? indica que o reposicionamento nÆo foi
              realizado com sucesso ---*/
        RUN getRowid IN {&hDBOTable} (OUTPUT rCurrentAux).
        IF rCurrentAux = ? THEN
            ASSIGN cReturnAux = "NOK":U.
        ELSE
            ASSIGN cReturnAux = "OK":U.
    &ELSE
        /*--- Reposiciona query do DBO atrav‚s de rowid passado como parƒmetro ---*/
        RUN repositionRecord IN {&hDBOTable} (INPUT pRowid).
    &ENDIF
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Quando for utilizado BO 1.1 nÆo ser  feito o tratamento dos erros ---*/
    &ELSE
        /*--- Seta vari vel cReturnAux com o conte£do do RETURN-VALUE ---*/
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
        /*--- Quando for utilizado BO 1.1 nÆo ser  feito o tratamento dos erros 
              Mas caso a vari vel cReturnAux tenha o valor NOK, ser  considerado 
              que o BO 1.1 nÆo est  posicionado em registro algum ---*/
        IF cReturnAux = "NOK":U THEN DO:
            /*--- Alterado por Valdir (tech264) para permitir customiza‡äes nos botäes ---*/
            /*--- Seta estado dos botäes/menus ---*/
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

                    &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN /*alterado*/
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                    &ENDIF

                    /* tech14187 - 10/04/2007 - 1468007 */
                    /*tech1139 - botÆo Exit */
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U

                    /*tech1139 - botÆo Help */
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U

                    /*tech1139 - 05/12/2006 - FO 1387.046 */
                    .
            END.
            
            /*--- Seta estado dos botäes/menu ---*/
            RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
            
            /*--- Seta cursor do mouse para normal ---*/
            SESSION:SET-WAIT-STATE("":U).
            
            RETURN "NOK":U.
        END.
    &ELSE
        IF cReturnAux = "NOK":U THEN DO:
            /*--- Retorna temp-table RowErrors do DBO ---*/
            RUN getRowErrors IN {&hDBOTable} (OUTPUT TABLE RowErrors).
            
            /*--- Descri‡Æo dos erros:
                   6: Query est  fechada 
                  11: NÆo foi poss¡vel reposicionar a query ---*/
            
            IF CAN-FIND(FIRST RowErrors 
                        WHERE (RowErrors.ErrorType = "INTERNAL":U) AND
                              (RowErrors.ErrorNumber = 6)) THEN DO:

                /*--- Alterado por Valdir (tech264) para permitir customiza‡äes nos botäes ---*/
                /*--- Seta estado dos botäes/menus ---*/
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
                        
                        &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN /*alterado*/
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U            
                        &ENDIF
                        
                            
                        /* tech14187 - 10/04/2007 - 1468007 */
                        /*tech1139 - botÆo Exit */ 
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        /*tech1139 - botÆo Help */ 
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        .
                    
                    /*--- Seta estado dos botäes/menu ---*/
                    RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
                    
                    /*--- Seta cursor do mouse para normal ---*/
                    SESSION:SET-WAIT-STATE("":U).
                    
                    RETURN "NOK":U.
                END.
            END.
            
            IF CAN-FIND(FIRST RowErrors 
                        WHERE (RowErrors.ErrorType = "INTERNAL":U) AND
                              (RowErrors.ErrorNumber = 11)) THEN DO:

                /*--- Alterado por Valdir (tech264) para permitir customiza‡äes nos botäes ---*/
                /*--- Seta estado dos botäes/menus ---*/
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
                        
                        &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN /*alterado*/
                            cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U            
                        &ENDIF
                                                
                        /* tech14187 - 10/04/2007 - 1468007 */
                        /*tech1139 - botÆo Exit */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U            
                        
                        /*tech1139 - botÆo Help */
                        cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                        
                        /*tech1139 - 05/12/2006 - FO 1387.046 */
                        .
                    
                    /*--- Seta estado dos botäes/menu ---*/
                    RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
                                        
                    /*--- Seta cursor do mouse para normal ---*/
                    SESSION:SET-WAIT-STATE("":U).
                    
                    RETURN "NOK":U.
                END.
            END.
            
            /*--- Seta cursor do mouse para normal ---*/
            SESSION:SET-WAIT-STATE("":U).
            
            RETURN "NOK":U.
        END.
    &ENDIF
    
    /*--- Alterado por Valdir (tech264) para permitir customiza‡äes nos botäes ---*/
    /*--- Seta estado dos botäes/menus ---*/
    DO WITH FRAME fPage0:

          ASSIGN
                cButtonsStateAux = "":U
                &IF "{&First}":U = "YES":U &THEN
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                &ENDIF
                /* Desabilitar botÆo de Prev em banco diferente de Progress */
                &IF "{&Prev}":U = "YES":U &THEN
                    /*Alteracao 03/10/2006 - tech1007 - Alteracao para desabilitar os botäes de forma correta para bancos Oracle*/
                    /*O teste foi feito desta forma para desabilitar o botÆo prev apenas para versäes >= 9.1B. A partir da 9.1C
                      este botÆo nÆo precisa ser desabilitado */
                    &IF "{&ems_dbtype}":U <> "PROGRESS":U AND (PROVERSION < "9.1C":U AND 
                        INTEGER(ENTRY(1,PROVERSION,".")) <= 9) &THEN
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
                
                &IF "{&ExcludeBtReportsJoins}":U <> "YES":U &THEN /*alterado*/
                    cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U            
                &ENDIF
                
                /*tech1139 - botÆo Exit */
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U            

                /*tech1139 - botÆo Help */
                cButtonsStateAux = cButtonsStateAux + MIN(cButtonsStateAux, ",":U) + "YES":U
                /*tech1139 - 05/12/2006 - FO 1387.046 */
                .

    END.

    /*--- Seta estado dos botäes/menu ---*/
    RUN controlToolBar IN THIS-PROCEDURE (INPUT cButtonsStateAux).
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-saveFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveFields Method-Library 
PROCEDURE saveFields :
/*------------------------------------------------------------------------------
  Purpose:     Salva campos da temp-table {&ttTable}
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
   
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    {method/override.i &Position="Before"
                       &Procedure="saveFields"}
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    IF AVAILABLE {&ttTable} THEN 
         assign epc-rowid = {&ttTable}.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="BEFORE-SAVE-FIELDS"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable}"
                     &RowidTable="epc-rowid"}
    
    /*--- Salva campos contidos no {&page0KeyFields} ---*/
    &IF "{&page0KeyFields}":U <> "":U &THEN
        DO WITH FRAME fPage0:
            ASSIGN {&page0KeyFields}.
        END.
    &ENDIF
    
    /*--- Salva campos contidos no {&page0Fields} ---*/
    &IF "{&page0Fields}":U <> "":U &THEN
        DO WITH FRAME fPage0:
            ASSIGN {&page0Fields}.
        END.
    &ENDIF
    
    /*--- Salva campos contidos no {&page1Fields} ---*/
    &IF "{&page1Fields}":U <> "":U &THEN
        DO WITH FRAME fPage1:
            ASSIGN {&page1Fields}.
        END.
    &ENDIF
    
    /*--- Salva campos contidos no {&page2Fields} ---*/
    &IF "{&page2Fields}":U <> "":U &THEN
        DO WITH FRAME fPage2:
            ASSIGN {&page2Fields}.
        END.
    &ENDIF
    
    /*--- Salva campos contidos no {&page3Fields} ---*/
    &IF "{&page3Fields}":U <> "":U &THEN
        DO WITH FRAME fPage3:
            ASSIGN {&page3Fields}.
        END.
    &ENDIF
    
    /*--- Salva campos contidos no {&page4Fields} ---*/
    &IF "{&page4Fields}":U <> "":U &THEN
        DO WITH FRAME fPage4:
            ASSIGN {&page4Fields}.
        END.
    &ENDIF
    
    /*--- Salva campos contidos no {&page5Fields} ---*/
    &IF "{&page5Fields}":U <> "":U &THEN
        DO WITH FRAME fPage5:
            ASSIGN {&page5Fields}.
        END.
    &ENDIF
    
    /*--- Salva campos contidos no {&page6Fields} ---*/
    &IF "{&page6Fields}":U <> "":U &THEN
        DO WITH FRAME fPage6:
            ASSIGN {&page6Fields}.
        END.
    &ENDIF
    
    /*--- Salva campos contidos no {&page7Fields} ---*/
    &IF "{&page7Fields}":U <> "":U &THEN
        DO WITH FRAME fPage7:
            ASSIGN {&page7Fields}.
        END.
    &ENDIF
    
    /*--- Salva campos contidos no {&page8Fields} ---*/
    &IF "{&page8Fields}":U <> "":U &THEN
        DO WITH FRAME fPage8:
            ASSIGN {&page8Fields}.
        END.
    &ENDIF
    
    IF AVAILABLE {&ttTable} THEN 
         assign epc-rowid = {&ttTable}.r-Rowid. 
    ELSE 
         assign epc-rowid = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="AFTER-SAVE-FIELDS"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable}"
                     &RowidTable="epc-rowid"}
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    {method/override.i &Position="After"
                       &Procedure="saveFields"}
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-translate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE translate Method-Library 
PROCEDURE translate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
         RUN translateMenu IN THIS-PROCEDURE (INPUT {&WINDOW-NAME}:MENU-BAR).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-translateMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE translateMenu Method-Library 
PROCEDURE translateMenu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{include/i-trdmn.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

