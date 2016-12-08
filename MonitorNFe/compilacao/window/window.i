&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
/* Procedure Description
"Method Library principal para Maintenance Template, que cont‚m defini‡äes e chamadas a outras Method Libraries."
*/
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Method-Library 
/*--------------------------------------------------------------------------
    Library    : window/Window.i
    Purpose    : Method Library principal para Window Template, que 
                 cont‚m defini‡äes e chamadas a outras Method Libraries 

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* Global Variable Definitions ---                                        */
DEFINE NEW GLOBAL SHARED VARIABLE hWindowStyles AS HANDLE NO-UNDO.

&IF "{&mguni_version}" >= "2.06b" OR "{&aplica_facelift}" = "YES" &THEN
    /**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
    define new global shared variable h-facelift as handle no-undo.
    if not valid-handle(h-facelift) then run btb/btb901zo.p persistent set h-facelift.
&ENDIF

/*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
&IF DEFINED(DBOVersion) <> 0 &THEN
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
DEFINE VARIABLE hFolder           AS HANDLE  NO-UNDO.
DEFINE VARIABLE hProgramZoom      AS HANDLE  NO-UNDO.
DEFINE VARIABLE hQueryJoins       AS HANDLE  NO-UNDO.
DEFINE VARIABLE hReportsJoins     AS HANDLE  NO-UNDO.
DEFINE VARIABLE hShowMsg          AS HANDLE  NO-UNDO.
DEFINE VARIABLE hWindowParent     AS HANDLE  NO-UNDO.
DEFINE VARIABLE lCustomExecuted   AS LOGICAL NO-UNDO.
DEFINE VARIABLE lOverrideExecuted AS LOGICAL NO-UNDO.

DEFINE VARIABLE c-nom-prog-dpc-mg97  AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-nom-prog-appc-mg97 AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-nom-prog-upc-mg97  AS CHARACTER NO-UNDO.

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
/*** Alterado por Farley - em 23/07/2003 ***/
{include/i_fnctrad.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Method-Library 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
    
    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="BEFORE-CHANGE-PAGE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="?"
                     &RowidTable="?"}
    
    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="AFTER-CHANGE-PAGE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="?"
                     &RowidTable="?"}
    
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

&IF DEFINED(EXCLUDE-destroyInterface) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE destroyInterface Method-Library 
PROCEDURE destroyInterface :
/*------------------------------------------------------------------------------
  Purpose:     Destr¢i programa
  Parameters:  
  Notes:       Destr¢i programa de Folder
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
                     &Table="?"
                     &RowidTable="?"}
    
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
                     &Table="?"
                     &RowidTable="?"}
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    {method/override.i &Position="After"
                       &Procedure="destroyInterface"}
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    &IF "{&WindowType}":U = "Detail":U &THEN
        /*--- Retira estilo de janela (modal) para thinMaintenace ---*/
        IF VALID-HANDLE(hWindowParent) THEN DO:
            ASSIGN hWindowParent:SENSITIVE = YES.
        END.
    &ENDIF
    
    /*--- Destr¢i janela associada ao programa ---*/
    IF VALID-HANDLE(wWindow) THEN
        DELETE WIDGET wWindow.
    
    /*--- Destr¢i programa ---*/
    IF THIS-PROCEDURE:PERSISTENT THEN
        DELETE PROCEDURE THIS-PROCEDURE.
    
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

&IF DEFINED(EXCLUDE-displayWidgets) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayWidgets Method-Library 
PROCEDURE displayWidgets :
/*------------------------------------------------------------------------------
  Purpose:     Exibe os widgets em tela
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE hFieldEntryAux    AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lExecutedEntryAux AS LOGICAL NO-UNDO.
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    {method/override.i &Position="Before"
                       &Procedure="displayWidgets"}
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="BEFORE-DISPLAY"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="?"
                     &RowidTable="?"}
    
    /*--- Exibe widgets contidos em {page0Widgets} ---*/
    &IF "{&page0Widgets}":U <> "":U &THEN
        DISPLAY {&page0Widgets} WITH FRAME fPage0.
    &ENDIF
    
    /*--- Exibe widgets contidos em {page1Widgets} ---*/
    &IF "{&page1Widgets}":U <> "":U &THEN
        DISPLAY {&page1Widgets} WITH FRAME fPage1.
    &ENDIF
    
    /*--- Exibe widgets contidos em {page2Widgets} ---*/
    &IF "{&page2Widgets}":U <> "":U &THEN
        DISPLAY {&page2Widgets} WITH FRAME fPage2.
    &ENDIF
    
    /*--- Exibe widgets contidos em {page3Widgets} ---*/
    &IF "{&page3Widgets}":U <> "":U &THEN
        DISPLAY {&page3Widgets} WITH FRAME fPage3.
    &ENDIF
    
    /*--- Exibe widgets contidos em {page4Widgets} ---*/
    &IF "{&page4Widgets}":U <> "":U &THEN
        DISPLAY {&page4Widgets} WITH FRAME fPage4.
    &ENDIF
    
    /*--- Exibe widgets contidos em {page5Widgets} ---*/
    &IF "{&page5Widgets}":U <> "":U &THEN
        DISPLAY {&page5Widgets} WITH FRAME fPage5.
    &ENDIF
    
    /*--- Exibe widgets contidos em {page6Widgets} ---*/
    &IF "{&page6Widgets}":U <> "":U &THEN
        DISPLAY {&page6Widgets} WITH FRAME fPage6.
    &ENDIF
    
    /*--- Exibe widgets contidos em {page7Widgets} ---*/
    &IF "{&page7Widgets}":U <> "":U &THEN
        DISPLAY {&page7Widgets} WITH FRAME fPage7.
    &ENDIF
    
    /*--- Exibe widgets contidos em {page8Widgets} ---*/
    &IF "{&page8Widgets}":U <> "":U &THEN
        DISPLAY {&page8Widgets} WITH FRAME fPage8.
    &ENDIF
    
    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="AFTER-DISPLAY"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="?"
                     &RowidTable="?"}
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    {method/override.i &Position="After"
                       &Procedure="displayWidgets"}
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-enableWidgets) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableWidgets Method-Library 
PROCEDURE enableWidgets :
/*------------------------------------------------------------------------------
  Purpose:     Habilita os widgets em tela
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE hFieldEntryAux    AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lExecutedEntryAux AS LOGICAL NO-UNDO.
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    {method/override.i &Position="Before"
                       &Procedure="enableWidgets"}
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="BEFORE-ENABLE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="?"
                     &RowidTable="?"}
    
    /*--- Habilita widgets contidos em {page0Widgets} ---*/
    &IF "{&page0Widgets}":U <> "":U &THEN
        ENABLE {&page0Widgets} WITH FRAME fPage0.
    &ENDIF
    
    /*--- Habilita widgets contidos em {page1Widgets} ---*/
    &IF "{&page1Widgets}":U <> "":U &THEN
        ENABLE {&page1Widgets} WITH FRAME fPage1.
    &ENDIF
    
    /*--- Habilita widgets contidos em {page2Widgets} ---*/
    &IF "{&page2Widgets}":U <> "":U &THEN
        ENABLE {&page2Widgets} WITH FRAME fPage2.
    &ENDIF
    
    /*--- Habilita widgets contidos em {page3Widgets} ---*/
    &IF "{&page3Widgets}":U <> "":U &THEN
        ENABLE {&page3Widgets} WITH FRAME fPage3.
    &ENDIF
    
    /*--- Habilita widgets contidos em {page4Widgets} ---*/
    &IF "{&page4Widgets}":U <> "":U &THEN
        ENABLE {&page4Widgets} WITH FRAME fPage4.
    &ENDIF
    
    /*--- Habilita widgets contidos em {page5Widgets} ---*/
    &IF "{&page5Widgets}":U <> "":U &THEN
        ENABLE {&page5Widgets} WITH FRAME fPage5.
    &ENDIF
    
    /*--- Habilita widgets contidos em {page6Widgets} ---*/
    &IF "{&page6Widgets}":U <> "":U &THEN
        ENABLE {&page6Widgets} WITH FRAME fPage6.
    &ENDIF
    
    /*--- Habilita widgets contidos em {page7Widgets} ---*/
    &IF "{&page7Widgets}":U <> "":U &THEN
        ENABLE {&page7Widgets} WITH FRAME fPage7.
    &ENDIF
    
    /*--- Habilita widgets contidos em {page8Widgets} ---*/
    &IF "{&page8Widgets}":U <> "":U &THEN
        ENABLE {&page8Widgets} WITH FRAME fPage8.
    &ENDIF
    
    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="AFTER-ENABLE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="?"
                     &RowidTable="?"}
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    {method/override.i &Position="After"
                       &Procedure="enableWidgets"}
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
        VIEW FRAME fPage0 IN WINDOW wWindow.
    END.
    
    /*--- Executa valida‡äes de inicializa‡Æo ---*/
    ASSIGN c-programa-mg97 = CAPS("{&Program}":U)
           c-versao-mg97   = "{&Version}":U.
    {utp/ut-vfsec.i}
    

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
    /* TECH14187 - FO 1514824 - Erro na tradu‡Æo do titulo das ThinWindow */
    run utp/ut-liter.p (input replace(c-titulo-prog-mg97, " ", "_") ,
                        input "*":U,
                        input "":U). 

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
                     &Table="?"
                     &RowidTable="?"}
    
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
                                    &IF "{&WindowType}":U = "Master":U &THEN
                                          INPUT ?).
                                    &ELSEIF "{&WindowType}":U = "Detail":U &THEN
                                          /*--- Est  sendo utilizada a f¢rmula abaixo porquˆ 
                                                a barra de botäes fica na parte inferior da frame ---*/
                                          INPUT FRAME fPage0:HEIGHT - FRAME fPage1:ROW - 0.75).
                                    &ENDIF
    &ENDIF
    
    /*--- Executa programa de estilo de janelas ---*/
    IF  VALID-HANDLE(hWindowStyles) = NO OR
        hWindowStyles:TYPE <> "PROCEDURE":U OR
        (hWindowStyles:FILE-NAME <> "utp/WindowStyles.p":U AND
        hWindowStyles:FILE-NAME <> "utp/WindowStyles.r":U) THEN
        RUN utp/windowstyles.p PERSISTENT SET hWindowStyles.
    
    /*--- Exibe Widgets em tela ---*/
    RUN displayWidgets IN THIS-PROCEDURE.
    
    /*--- Habilita Widgets em tela ---*/
    RUN enableWidgets IN THIS-PROCEDURE.

    /*--- Paulo - FO 679.985 
          Este tratamento deve ser feito depois de inicializar algum 
          objeto na window, para que o handle da window seja v lido ---*/
    &IF "{&WindowType}":U = "Master":U &THEN
        /*--- Seta estilo de janela para thinWindow ---*/
        RUN disableMax IN hWindowStyles (INPUT wWindow:hWnd).
    &ELSEIF "{&WindowType}":U = "Detail":U &THEN
        /*--- Seta estilo de janela para thinWindow ---*/
        RUN deleteMinMax IN hWindowStyles (INPUT wWindow:hWnd).
    &ENDIF    
    
    &IF "{&Folder}":U = "YES":U &THEN
        RUN setFolder IN hFolder (INPUT {&InitialPage}).
    &ENDIF
    
    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="AFTER-INITIALIZE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="?"
                     &RowidTable="?"}
    
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
    VIEW wWindow.
    
    &IF "{&WindowType}":U = "Detail":U &THEN
        /*--- Seta estilo de janela modal para thinMaintenance ---*/
        /* tech1139 - 02/11/2005 - FO 1214.774 */

/*         ASSIGN hWindowParent           = SESSION:FIRST-CHILD        */
/*                hWindowParent           = hWindowParent:NEXT-SIBLING */
/*                hWindowParent:SENSITIVE = NO.                        */    
/*
        ASSIGN i = LENGTH(PROGRAM-NAME(2)) - 23.
        ASSIGN c-programa = SUBSTRING(PROGRAM-NAME(2),24,i).
        /* tech1139 - para separar o nome do programa da string USER-INTERFACE-TRIGGER no program-name(2) */
*/    

        /*Alteracao 24/01/2006 - tech14207 - Alterado para desabilitar o programa pai*/
        /*O c¢digo foi alterado para permitir que o mesmo programa fosse executado mais de uma vez corretamente*/
        
        &if integer(entry(1,proversion,".")) >= 9 &then
            DEFINE VAR h-source AS HANDLE     NO-UNDO.
            DEFINE VARIABLE h-this AS HANDLE     NO-UNDO. /*26/09/2006 - tech30713 - FO: 1309021*/
    
            ASSIGN h-source = SOURCE-PROCEDURE.
            ASSIGN h-this = THIS-PROCEDURE.
    
            IF (THIS-PROCEDURE <> SOURCE-PROCEDURE) THEN DO:
                ASSIGN h-source = SOURCE-PROCEDURE.
                IF VALID-HANDLE(h-source) AND  VALID-HANDLE(h-source:CURRENT-WINDOW) THEN DO:
                    ASSIGN hWindowParent           = h-source:CURRENT-WINDOW
                           hWindowParent:SENSITIVE = NO.  /* FO 1367.797 - tech1139 - 08/09/2006 */
                END.
            END.
        &ELSE 
            /*Alteracao 15/12/2005 - tech1007 - Alterado para desabilitar o programa pai*/
            DEFINE VAR h-prog AS HANDLE     NO-UNDO.
            DEFINE VARIABLE c-programa AS CHARACTER  NO-UNDO.

            IF NUM-ENTRIES(PROGRAM-NAME(2)," ") > 1 THEN DO:
                ASSIGN c-programa = ENTRY(NUM-ENTRIES(PROGRAM-NAME(2)," "), PROGRAM-NAME(2)," ").
            END.
            ELSE DO:
                ASSIGN c-programa = ENTRY(1,PROGRAM-NAME(2)," ").
            END.                   
            IF INDEX(PROGRAM-NAME(1), c-programa) = 0 THEN DO:
                ASSIGN h-prog = SESSION:FIRST-PROCEDURE.
                DO WHILE h-prog <> ?:                   
                    IF h-prog:FILE-NAME = c-programa THEN DO:
                       LEAVE.
                    END.  
                    ASSIGN h-prog = h-prog:NEXT-SIBLING.
                END.               
                IF VALID-HANDLE(h-prog) THEN DO:
                    ASSIGN hWindowParent = h-prog:CURRENT-WINDOW.

                    IF VALID-HANDLE(hWindowParent) THEN
                       ASSIGN hWindowParent:SENSITIVE = NO. /* FO 1367.797 - tech1139 - 08/09/2006 */

                END.
            END.
            /*Fim Alteracao 15/12/2005*/
        &ENDIF                                          
        /*Fim da altera‡Æo 24/01/2006 - tech14207*/
    &ENDIF
    
    APPLY "ENTRY":U TO FRAME fPage0.
    APPLY "ENTRY":U TO wWindow.
    
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
    
    /*--- Inicializa janela de Consultas Relacionadas ---*/
    IF NOT VALID-HANDLE(hQueryJoins) THEN
        RUN utp/ut-cons.w PERSISTENT SET hQueryJoins (INPUT c-programa-mg97).
    
    IF VALID-HANDLE(hQueryJoins) THEN
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
    
    /*--- Inicializa janela de Relat¢rios Relacionados ---*/
    IF NOT VALID-HANDLE(hReportsJoins) THEN
        RUN utp/ut-relat.w PERSISTENT SET hReportsJoins (INPUT c-programa-mg97).
    
    IF VALID-HANDLE(hReportsJoins) THEN
        RUN dispatch IN hReportsJoins (INPUT "INITIALIZE":U).
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/*Alterado 07/11/2006 - tech1007 - FO 1410116 - Cria‡Æo das procedures respons veis pela tradu‡Æo do template*/
&IF DEFINED(EXCLUDE-translate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE translate Method-Library 
PROCEDURE translate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &IF "{&WindowType}" = "Master" &THEN
        RUN translateMenu IN THIS-PROCEDURE (INPUT {&WINDOW-NAME}:MENU-BAR).
    &ENDIF

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

    &IF "{&WindowType}" = "Master" &THEN
        {include/i-trdmn.i}
    &ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
/*Fim altera‡Æo 07/11/2006*/
