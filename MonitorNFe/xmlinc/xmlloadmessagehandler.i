&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : XMLLoadMessageHandler.i
    Purpose     : Carregar o programa xmlutp/message_handler.p na mem¢ria,
                  caso ainda n∆o esteja carregado, e faz a inicializaá∆o 
                  do contexto para envio de uma nova mensagem XML.

    Syntax      :
    
    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .i file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Verifica se include com definiá∆o de vari†veis j† est† definido --- */
&IF NOT "{&XMLLoadMessageHandlerDef}" = "TRUE" &THEN
    /* Definiá∆o de vari†veis locais e globais ------------------------ */
    {xmlinc/xmlloadmessagehandlerdef.i}
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
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* Inicializa vari†vel de status de execuá∆o --- */
ASSIGN cGlobalMHReturnValue = "OK":U.


/* Verifica se xmlutp/message_handler.p j† est† carregado em mem¢ria --- */
IF NOT VALID-HANDLE(hGlobalMessageHandler)     OR
   hGlobalMessageHandler:TYPE <> "PROCEDURE":U OR 
   hGlobalMessageHandler:FILE-NAME <> "xmlutp/message_handler.p":U THEN DO:  
    /* Verifica existància do xmlutp/message_handler.p antes de carreg†-lo em mem¢ria --- */
    IF SEARCH("xmlutp/message_handler.r":U) <> ? OR SEARCH("xmlutp/message_handler.p":U) <> ? THEN DO:
        /* Carrega xmlutp/message_handler em mem¢ria, e inicializa contexto de envio de uma 
             nova mensagem XML --- */
        RUN xmlutp/message_handler.p PERSISTENT SET hGlobalMessageHandler (INPUT "{&XmlProdName}":U,
                                                                           INPUT "{&XmlProdVersion}":U,
                                                                           INPUT "{&XmlApp}":U).
        
        /* Verifica se a inicializaá∆o foi realizada com sucesso --- */
        IF RETURN-VALUE <> "OK":U THEN DO:
            /* Armazena status de execuá∆o */
            ASSIGN cGlobalMHReturnValue = RETURN-VALUE.
            
            /* Remove xmlutp/message_handle.p da mem¢ria --- */
            DELETE PROCEDURE hGlobalMessageHandler NO-ERROR.
        END.
    END.
    ELSE 
        /* Armazena status de execuá∆o --- */
        ASSIGN cGlobalMHReturnValue = "Arquivo xmlutp/message_handler.p n∆o foi encontrado".
END.
ELSE 
    /* Inicializa contexto de envio de uma nova mensagem XML --- */
    RUN initMessage IN hGlobalMessageHandler (INPUT "{&XmlProdName}":U, 
                                              INPUT "{&XmlProdVersion}":U, 
                                              INPUT "{&XmlApp}":U).


/* Repassa vari†veis utilizadas internamente para os par∆metros --- */
ASSIGN {&MessageHandler} = hGlobalMessageHandler
       {&MHReturnValue}  = cGlobalMHReturnValue.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
