&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
/* Procedure Description
"Method Library principal para Maintenance Template, que cont‚m defini‡äes e chamadas a outras Method Libraries."
*/
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Method-Library 
/*--------------------------------------------------------------------------
    Library    : Report/Report.i
    Purpose    : Method Library principal para Report Template, que 
                 cont‚m defini‡äes e chamadas a outras Method Libraries 

    Authors    : Fabiano Espindola

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* Global Variable Definitions ---                                        */
DEFINE NEW GLOBAL SHARED VARIABLE hWindowStyles AS HANDLE NO-UNDO.

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

/* Local Variable Definitions ---                                         */
DEFINE VARIABLE cAction              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cButtonsState        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE deColPanel           AS DECIMAL     NO-UNDO
    DECIMALS 2 INITIAL 31.
DEFINE VARIABLE hFolder              AS HANDLE      NO-UNDO.
DEFINE VARIABLE hProgramZoom         AS HANDLE      NO-UNDO.
DEFINE VARIABLE hQueryJoins          AS HANDLE      NO-UNDO.
DEFINE VARIABLE hReportsJoins        AS HANDLE      NO-UNDO.
DEFINE VARIABLE hShowMsg             AS HANDLE      NO-UNDO.
DEFINE VARIABLE lMultipleAdd         AS LOGICAL     NO-UNDO
    INITIAL YES.
DEFINE VARIABLE lCustomExecuted      AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lOverrideExecuted    AS LOGICAL     NO-UNDO.
DEFINE VARIABLE rCurrent             AS ROWID       NO-UNDO.

DEFINE VARIABLE c-nom-prog-dpc-mg97  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-nom-prog-appc-mg97 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-nom-prog-upc-mg97  AS CHARACTER   NO-UNDO.

DEFINE VARIABLE wh-pesquisa          AS HANDLE      NO-UNDO.
DEFINE VARIABLE c-arq-old            AS CHAR        NO-UNDO.
DEFINE VARIABLE c-arq-old-batch      AS CHAR        NO-UNDO.

DEFINE VARIABLE c-imp-old            AS CHAR        NO-UNDO.

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
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Method-Library 
/* ************************* Included-Libraries *********************** */

{utp/ut-glob.i}
{include/i-sysvar.i}
{btb/btb008za.i0}
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
            APPLY "RETURN":U TO SELF.
    END CASE.
    
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
    
    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="BEFORE-DESTROY-INTERFACE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="'?'"
                     &RowidTable="'?'"}
    
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
    
    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="AFTER-DESTROY-INTERFACE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="'?'"
                     &RowidTable="'?'"}
    
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
    IF VALID-HANDLE(wReport) THEN
        DELETE WIDGET wReport.
    
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
    
    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="BEFORE-DISABLE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="'?'"
                     &RowidTable="'?'"}
    
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
    
    /*--- Desabilita widgets contidos no {&page1Widgets} ---*/
    &IF "{&page1Widgets}":U <> "":U &THEN
        DO WITH FRAME fPage1:
            DISABLE {&page1Widgets}.
        END.
    &ENDIF
    
    /*--- Desabilita widgets contidos no {&page2Widgets} ---*/
    &IF "{&page2Widgets}":U <> "":U &THEN
        DO WITH FRAME fPage2:
            DISABLE {&page2Widgets}.
        END.
    &ENDIF

    /*--- Desabilita widgets contidos no {&page3Widgets} ---*/
    &IF "{&page3Widgets}":U <> "":U &THEN
        DO WITH FRAME fPage3:
            DISABLE {&page3Widgets}.
        END.
    &ENDIF

    /*--- Desabilita widgets contidos no {&page4Widgets} ---*/
    &IF "{&page4Widgets}":U <> "":U &THEN
        DO WITH FRAME fPage4:
            DISABLE {&page4Widgets}.
        END.
    &ENDIF

    /*--- Desabilita widgets contidos no {&page5Widgets} ---*/
    &IF "{&page5Widgets}":U <> "":U &THEN
        DO WITH FRAME fPage5:
            DISABLE {&page5Widgets}.
        END.
    &ENDIF

    /*--- Desabilita widgets contidos no {&page6Widgets} ---*/
    &IF "{&page6Widgets}":U <> "":U &THEN
        DO WITH FRAME fPage6:
            DISABLE {&page6Widgets}.
        END.
    &ENDIF

    /*--- Desabilita widgets contidos no {&page7Widgets} ---*/
    &IF "{&page7Widgets}":U <> "":U &THEN
        DO WITH FRAME fPage7:
            DISABLE {&page7Widgets}.
        END.
    &ENDIF

    /*--- Desabilita widgets contidos no {&page8Widgets} ---*/
    &IF "{&page8Widgets}":U <> "":U &THEN
        DO WITH FRAME fPage8:
            DISABLE {&page8Widgets}.
        END.
    &ENDIF

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="AFTER-DISABLE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="'?'"
                     &RowidTable="'?'"}
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    {method/override.i &Position="After"
                       &Procedure="disableFields"}
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
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

&IF DEFINED(EXCLUDE-displayText) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayText Method-Library 
PROCEDURE displayText :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    &IF "{&page1Text}":U <> "":U &THEN
        DISPLAY {&page1Text} WITH FRAME fPage1.
    &ENDIF
    &IF "{&page2Text}":U <> "":U &THEN
        DISPLAY {&page2Text} WITH FRAME fPage2.
    &ENDIF
    &IF "{&page3Text}":U <> "":U &THEN
        DISPLAY {&page3Text} WITH FRAME fPage3.
    &ENDIF
    &IF "{&page4Text}":U <> "":U &THEN
        DISPLAY {&page4Text} WITH FRAME fPage4.
    &ENDIF
    &IF "{&page5Text}":U <> "":U &THEN
        DISPLAY {&page5Text} WITH FRAME fPage5.
    &ENDIF
    &IF "{&page6Text}":U <> "":U &THEN
        DISPLAY {&page6Text} WITH FRAME fPage6.
    &ENDIF
    &IF "{&page7Text}":U <> "":U &THEN
        DISPLAY {&page7Text} WITH FRAME fPage7.
    &ENDIF
    &IF "{&page8Text}":U <> "":U &THEN
        DISPLAY {&page8Text} WITH FRAME fPage8.
    &ENDIF

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
    
    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="BEFORE-ENABLE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="'?'"
                     &RowidTable="'?'"}
    
    /*--- Habilita campos contidos no {&page1Fields} ---*/
    &IF "{&page1Fields}":U <> "":U &THEN
        ENABLE {&page1Fields} WITH FRAME fPage1.
        DISPLAY {&page1Fields} WITH FRAME fPage1.
    &ENDIF
    
    /*--- Habilita campos contidos no {&page2Fields} ---*/
    &IF "{&page2Fields}":U <> "":U &THEN
        ENABLE {&page2Fields} WITH FRAME fPage2.
        DISPLAY {&page2Fields} WITH FRAME fPage2.
    &ENDIF
    
    /*--- Habilita campos contidos no {&page3Fields} ---*/
    &IF "{&page3Fields}":U <> "":U &THEN
        ENABLE {&page3Fields} WITH FRAME fPage3.
        DISPLAY {&page3Fields} WITH FRAME fPage3.
    &ENDIF
    
    /*--- Habilita campos contidos no {&page4Fields} ---*/
    &IF "{&page4Fields}":U <> "":U &THEN
        ENABLE {&page4Fields} WITH FRAME fPage4.
        DISPLAY {&page4Fields} WITH FRAME fPage4.
    &ENDIF
    
    /*--- Habilita campos contidos no {&page5Fields} ---*/
    &IF "{&page5Fields}":U <> "":U &THEN
        ENABLE {&page5Fields} WITH FRAME fPage5.
        DISPLAY {&page5Fields} WITH FRAME fPage5.
    &ENDIF
    
    /*--- Habilita campos contidos no {&page6Fields} ---*/
    &IF "{&page6Fields}":U <> "":U &THEN
        ENABLE {&page6Fields} WITH FRAME fPage6.
        DISPLAY {&page6Fields} WITH FRAME fPage6.
    &ENDIF
    
    /*--- Habilita campos contidos no {&page7Fields} ---*/
    &IF "{&page7Fields}":U <> "":U &THEN
        ENABLE {&page7Fields} WITH FRAME fPage7.
        DISPLAY {&page7Fields} WITH FRAME fPage7.
    &ENDIF
    
    /*--- Habilita campos contidos no {&page8Fields} ---*/
    &IF "{&page8Fields}":U <> "":U &THEN
        ENABLE {&page8Fields} WITH FRAME fPage8.
        DISPLAY {&page8Fields} WITH FRAME fPage8.
    &ENDIF

    /*--- Habilita widgets contidos no {&page0Widgets} ---*/
    &IF "{&page0Widgets}":U <> "":U &THEN
        ENABLE {&page0Widgets} WITH FRAME fPage0.
        DISPLAY {&page0Widgets} WITH FRAME fPage0.
    &ENDIF
    
    /*--- Habilita widgets contidos no {&page1Widgets} ---*/
    &IF "{&page1Widgets}":U <> "":U &THEN
        ENABLE {&page1Widgets} WITH FRAME fPage1. 
        DISPLAY {&page1Widgets} WITH FRAME fPage1.
    &ENDIF
    
    /*--- Habilita widgets contidos no {&page2Widgets} ---*/
    &IF "{&page2Widgets}":U <> "":U &THEN
        ENABLE {&page2Widgets} WITH FRAME fPage2.
        DISPLAY {&page2Widgets} WITH FRAME fPage2.
    &ENDIF

    /*--- Habilita widgets contidos no {&page3Widgets} ---*/
    &IF "{&page3Widgets}":U <> "":U &THEN
        ENABLE {&page3Widgets} WITH FRAME fPage3.
        DISPLAY {&page3Widgets} WITH FRAME fPage3.
    &ENDIF

    /*--- Habilita widgets contidos no {&page4Widgets} ---*/
    &IF "{&page4Widgets}":U <> "":U &THEN
        ENABLE {&page4Widgets} WITH FRAME fPage4.
        DISPLAY {&page4Widgets} WITH FRAME fPage4.
    &ENDIF

    /*--- Habilita widgets contidos no {&page5Widgets} ---*/
    &IF "{&page5Widgets}":U <> "":U &THEN
        ENABLE {&page5Widgets} WITH FRAME fPage5.
        DISPLAY {&page5Widgets} WITH FRAME fPage5.
    &ENDIF

    /*--- Habilita widgets contidos no {&page6Widgets} ---*/
    &IF "{&page6Widgets}":U <> "":U &THEN
        ENABLE {&page6Widgets} WITH FRAME fPage6.
        DISPLAY {&page6Widgets} WITH FRAME fPage6.
        if rsDestiny:screen-value in frame fPage6 = "3" then 
            assign cFile:hidden in frame fPage6 = yes
                   btFile:hidden in frame fPage6 = yes
                   btConfigImpr:hidden in frame fPage6 = yes.  
    &ENDIF

    /*--- Habilita widgets contidos no {&page7Widgets} ---*/
    &IF "{&page7Widgets}":U <> "":U &THEN
        ENABLE {&page7Widgets} WITH FRAME fPage7.
        DISPLAY {&page7Widgets} WITH FRAME fPage7.
        if rsDestiny:screen-value in frame fPage7 = "3" then 
            assign cDestinyFile:hidden in frame fPage7 = yes
                   btDestinyFile:hidden in frame fPage7 = yes
                   btConfigImprDest:hidden in frame fPage7 = yes.
    &ENDIF

    /*--- Habilita widgets contidos no {&page8Widgets} ---*/
    &IF "{&page8Widgets}":U <> "":U &THEN 
        ENABLE {&page8Widgets} WITH FRAME fPage8.
        DISPLAY {&page8Widgets} WITH FRAME fPage8.
    &ENDIF

    &IF "{&page1Fields}":U <> "":U &THEN
        /*--- Executar o ENTRY para o primeiro campo ---*/
        ASSIGN hFieldEntryAux = FRAME fPage1:FIRST-CHILD.
        
        DO WHILE VALID-HANDLE(hFieldEntryAux):
            IF (hFieldEntryAux:TYPE = "FILL-IN":U OR
                hFieldEntryAux:TYPE = "COMBO-BOX":U OR
                hFieldEntryAux:TYPE = "RADIO-SET":U) AND
               (hFieldEntryAux:FRAME = FRAME fPage1:HANDLE) THEN
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
    
    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="AFTER-ENABLE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="'?'"
                     &RowidTable="'?'"}
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    {method/override.i &Position="After"
                       &Procedure="enableFields"}
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
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
        VIEW FRAME fPage0 IN WINDOW wReport.
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
    
    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="BEFORE-INITIALIZE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="'?'"
                     &RowidTable="'?'"}
    
    DO WITH FRAME fPage0:
    
    /*--- Executa programa de folder ---*/
    &IF "{&Folder}":U = "YES":U &THEN
        RUN utp/thinfolder.w PERSISTENT SET hFolder.
        
        RUN setOCX IN hFolder (INPUT THIS-PROCEDURE:GET-SIGNATURE("control_load":U) <> "":U) NO-ERROR.
        
        RUN initializeFolders IN hFolder (INPUT FRAME fPage0:HANDLE,
                                          INPUT &IF "{&PGLAY}":U = "YES":U &THEN
                                                    STRING(FRAME fPage1:HANDLE)
                                                &ENDIF
                                                
                                                &IF "{&PGSEL}":U = "YES":U &THEN
                                                    &IF "{&PGLAY}":U = "YES":U &THEN
                                                    + ",":U + 
                                                    &ENDIF
                                                    STRING(FRAME fPage2:HANDLE)
                                                &ENDIF
                                                
                                                &IF "{&PGCLA}":U = "YES":U &THEN
                                                    &IF "{&PGLAY}":U = "YES":U OR
                                                        "{&PGSEL}":U = "YES":U &THEN
                                                    + ",":U +
                                                    &ENDIF
                                                    STRING(FRAME fPage3:HANDLE)
                                                &ENDIF
                                                
                                                &IF "{&PGPAR}":U = "YES":U &THEN
                                                    &if "{&PGLAY}":U = "YES":U OR
                                                        "{&PGSEL}":U = "YES":U OR
                                                        "{&PGCLA}":U = "YES":U &THEN
                                                    + ",":U +
                                                    &ENDIF
                                                    STRING(FRAME fPage4:HANDLE)
                                                &ENDIF
                                                
                                                &IF "{&PGDIG}":U = "YES":U &THEN
                                                    &if "{&PGLAY}":U = "YES":U OR
                                                        "{&PGSEL}":U = "YES":U OR
                                                        "{&PGCLA}":U = "YES":U OR
                                                        "{&PGPAR}":U = "YES":U &THEN
                                                    + ",":U +
                                                    &ENDIF
                                                    STRING(FRAME fPage5:HANDLE)
                                                &ENDIF
                                                
                                                &IF "{&PGIMP}":U = "YES":U &THEN
                                                    &if "{&PGLAY}":U = "YES":U OR
                                                        "{&PGSEL}":U = "YES":U OR
                                                        "{&PGCLA}":U = "YES":U OR
                                                        "{&PGPAR}":U = "YES":U OR
                                                        "{&PGDIG}":U = "YES":U &THEN
                                                    + ",":U +
                                                    &ENDIF
                                                    STRING(FRAME fPage6:HANDLE)
                                                &ENDIF

                                                &IF "{&PGLOG}":U = "YES":U &THEN
                                                    
                                                    &if "{&PGLAY}":U = "YES":U OR
                                                        "{&PGSEL}":U = "YES":U OR
                                                        "{&PGCLA}":U = "YES":U OR
                                                        "{&PGPAR}":U = "YES":U OR
                                                        "{&PGDIG}":U = "YES":U OR
                                                        "{&PGIMP}":U = "YES":U &THEN
                                                    + ",":U +
                                                    &ENDIF
                                                    STRING(FRAME fPage7:HANDLE)
                                                &ENDIF,
                                          INPUT "{&FolderLabels}":U,
                                          INPUT NUM-ENTRIES("{&FolderLabels}":U),
                                          INPUT ?,
                                          INPUT ?,
                                          INPUT ?,
                                          INPUT FRAME fPage0:HEIGHT - 
                                                &IF "{&PGLAY}":U = "YES":U &THEN 
                                                    FRAME fPage1:ROW 
                                                &ELSEIF "{&PGSEL}":U = "YES":U &THEN 
                                                        FRAME fPage2:ROW 
                                                    &ELSEIF "{&PGCLA}":U = "YES":U &THEN 
                                                            FRAME fPage3:ROW 
                                                        &ELSEIF "{&PGPAR}":U = "YES":U &THEN 
                                                                FRAME fPage4:ROW 
                                                            &ELSEIF "{&PGDIG}":U = "YES":U &THEN 
                                                                    FRAME fPage5:ROW 
                                                                &ELSEIF "{&PGIMP}":U = "YES":U &THEN 
                                                                        FRAME fPage6:ROW 
                                                                    &ELSEIF "{&PGLOG}":U = "YES":U &THEN 
                                                                            FRAME fPage7:ROW 
                                                                        &ENDIF 
                                                                        - 0.75 ).


    &ENDIF
    END.
    
    /*--- Executa programa de estilo de janelas ---*/
    IF  VALID-HANDLE(hWindowStyles) = NO OR
        hWindowStyles:TYPE <> "PROCEDURE":U OR
        (hWindowStyles:FILE-NAME <> "utp/WindowStyles.p":U AND
        hWindowStyles:FILE-NAME <> "utp/WindowStyles.r":U) THEN
        RUN utp/windowstyles.p PERSISTENT SET hWindowStyles.
    
    /*--- Seta estilo de janela para thinMaintenance ---*/
    RUN disableMax IN hWindowStyles (INPUT wReport:hWnd).
    
    /*--- Inicializa DBOs ---*/
    /* RUN initializeDBOs IN THIS-PROCEDURE. */
    
    /*--- Habilita Widgets e Campos ---*/
    RUN enableFields IN THIS-PROCEDURE.
    
    /*--- Mostra textos nas frames ---*/
    RUN displayText IN THIS-PROCEDURE.
    
    /*--- Verifica se a inicializa‡Æo do DBO foi feita com sucesso ---*/
/*    &IF "{&hDBOTable}":U <> "":U &THEN
 *         IF NOT VALID-HANDLE({&hDBOTable}) THEN DO:
 *             /*--- Exibir mensagem de finaliza‡Æo do programa ---*/
 *             RUN utp/ut-msgs.p (INPUT "SHOW":U, 
 *                                INPUT 18881, 
 *                                INPUT CAPS("{&Program}":U) + "~~" + CAPS("{&Version}":U)).
 *             
 *             /*--- Destr¢i programa ---*/
 *             RUN destroyInterface IN THIS-PROCEDURE.
 *             
 *             RETURN "NOK":U.
 *         END.
 *     &ENDIF*/
    
    &IF "{&Folder}":U = "YES":U &THEN
        RUN setFolder IN hFolder (INPUT {&InitialPage}).
    &ENDIF
    
    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="AFTER-INITIALIZE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="'?'"
                     &RowidTable="'?'"}
    
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
    VIEW wReport.
    APPLY "ENTRY":U TO FRAME fPage0.
    APPLY "ENTRY":U TO wReport.
    
    RETURN "OK":U.
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

&IF "{&mguni_version}" >= "2.06b" OR "{&aplica_facelift}" = "YES" &THEN
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
    &IF "{&page0Fields}":U <> "":U OR 
        "{&page0Widgets}":U <> "":U &THEN
    run pi_aplica_facelift_thin in h-facelift ( input frame fPage0:handle ).
    &ENDIF
    &IF "{&page1Fields}":U <> "":U OR 
        "{&page1Widgets}":U <> "":U &THEN
    run pi_aplica_facelift_thin in h-facelift ( input frame fPage1:handle ).
    &ENDIF
    &IF "{&page2Fields}":U <> "":U OR 
        "{&page2Widgets}":U <> "":U &THEN
    run pi_aplica_facelift_thin in h-facelift ( input frame fPage2:handle ).
    &ENDIF
    &IF "{&page3Fields}":U <> "":U OR 
        "{&page3Widgets}":U <> "":U &THEN
    run pi_aplica_facelift_thin in h-facelift ( input frame fPage3:handle ).
    &ENDIF
    &IF "{&page4Fields}":U <> "":U OR 
        "{&page4Widgets}":U <> "":U &THEN
    run pi_aplica_facelift_thin in h-facelift ( input frame fPage4:handle ).
    &ENDIF
    &IF "{&page5Fields}":U <> "":U OR 
        "{&page5Widgets}":U <> "":U &THEN
    run pi_aplica_facelift_thin in h-facelift ( input frame fPage5:handle ).
    &ENDIF
    &IF "{&page6Fields}":U <> "":U OR 
        "{&page6Widgets}":U <> "":U &THEN
    run pi_aplica_facelift_thin in h-facelift ( input frame fPage6:handle ).
    &ENDIF
    &IF "{&page7Fields}":U <> "":U OR 
        "{&page7Widgets}":U <> "":U &THEN
    run pi_aplica_facelift_thin in h-facelift ( input frame fPage7:handle ).
    &ENDIF
    &IF "{&page8Fields}":U <> "":U OR 
        "{&page8Widgets}":U <> "":U &THEN
    run pi_aplica_facelift_thin in h-facelift ( input frame fPage8:handle ).
    &ENDIF
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
