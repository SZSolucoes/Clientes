&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : xmlLoadMessageHandlerRecSen.i
    Purpose     : Carrega o MessageHandler.p no adapter de recebimento tratando possiveis erros e gerando mensagem de retorno

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .i file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

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


/* ASSOCIA PROCESSAMENTO CORRETO */
ASSIGN cGlobalMHReturnValue = "OK".

/* VERIFICA SE O PROGRAMA J¡ ESTA PERSISTENTE */
IF NOT VALID-HANDLE(hGlobalMessageHandler)     OR
   hGlobalMessageHandler:TYPE <> "PROCEDURE":U OR 
   hGlobalMessageHandler:FILE-NAME <> "xmlutp/message_handler.p":U 
THEN DO: 
    /* VERIFICA SE O ARQUIVO QUE SER¡ POSTO EM PERSISTENCIA EXISTE */
    IF SEARCH("xmlutp/message_handler.r") <> ? OR SEARCH("xmlutp/message_handler.p") <> ?
    THEN DO:
        /* DEIXA EM PERSISTENCIA O MessageHandler */
        RUN xmlutp/message_handler.p PERSISTENT SET hGlobalMessageHandler (
                                                                           &IF DEFINED(vXmlProdName) <> 0 &THEN
                                                                                INPUT {&vXmlProdName},
                                                                           &ELSE
                                                                                INPUT "{&XmlProdName}",
                                                                           &ENDIF
                                                                           
                                                                           &IF DEFINED(vXmlProdVersion) <> 0 &THEN
                                                                                INPUT {&vXmlProdVersion},
                                                                           &ELSE     
                                                                                INPUT "{&XmlProdVersion}",
                                                                           &ENDIF
                                                                           
                                                                           &IF DEFINED(vXmlApp) <> 0 &THEN
                                                                                INPUT {&vXmlApp}
                                                                           &ELSE
                                                                                INPUT "{&XmlApp}"
                                                                           &ENDIF
                                                                          ).
        /* VERIFICA SE AO CARREGAR OCORREU ALGUM ERRO */
        IF RETURN-VALUE <> "OK" 
        THEN DO:
            /* ASSOCIA O ERRO A VARIAVEL */
            ASSIGN cGlobalMHReturnValue = RETURN-VALUE.
            DELETE PROCEDURE hGlobalMessageHandler NO-ERROR.
        END.
    END.
    ELSE 
        /* ASSOCIA O ERRO A VARIAVEL */
        ASSIGN cGlobalMHReturnValue = "Arquivo xmlutp/message_handler.p n∆o foi encontrado".
END.
ELSE
    /* REINICIALIZA O MessageHandler */
    RUN initMessage IN hGlobalMessageHandler (
                                              &IF DEFINED(vXmlProdName) <> 0 &THEN
                                                   INPUT {&vXmlProdName},
                                              &ELSE
                                                   INPUT "{&XmlProdName}",
                                              &ENDIF
                                              
                                              &IF DEFINED(vXmlProdVersion) <> 0 &THEN
                                                   INPUT {&vXmlProdVersion},
                                              &ELSE     
                                                   INPUT "{&XmlProdVersion}",
                                              &ENDIF
                                              
                                              &IF DEFINED(vXmlApp) <> 0 &THEN
                                                   INPUT {&vXmlApp}
                                              &ELSE
                                                   INPUT "{&XmlApp}"
                                              &ENDIF
                                             ).
  
&IF DEFINED(MhReturnError) <> 0 &THEN
    /* VERIFICA SE O MessageHandler N√O FOI EXECUTADO CORRETAMENTE */
    IF cGlobalMHReturnValue <> "OK" 
    THEN DO:
        /* **** GERA XML DE ERRO PARA SER RETORNADO AO CONTROL BROKER **** */
        /* CRIA X-DOCUMENTs E X-NODEREFs PARA CRIAR XML DE ERRO */
        CREATE X-DOCUMENT hGlobalMHReturnError.
        CREATE X-NODEREF hMHNode1.
        CREATE X-NODEREF hMHNode2.
        CREATE X-NODEREF hMHNode3.
        CREATE X-NODEREF hMHNode4.
        CREATE X-NODEREF hMHNode5.
        CREATE X-NODEREF hMHNode6.
        
        /* CRIA NODE PRINCIPAL DATASUL */
        hGlobalMHReturnError:CREATE-NODE(hMHNode1, "DatasulMessage", "ELEMENT").    
        /* INFORMA O ENCODING DO XML */
        hGlobalMHReturnError:ENCODING = "UTF-8". 
        
        /* CRIA NODE SECUNDARIO PREAMBLE */
        hGlobalMHReturnError:CREATE-NODE(hMHNode2, "Preamble", "ELEMENT").    
        
        /* ADICIONA NODE ACIMA NO XML PRINCIPAL */
        hGlobalMHReturnError:APPEND-CHILD(hMHNode1).
        /* ADICIONA NODE ACIMA CRIADO, AO XML PRINCIPAL */
        hMHNode1:APPEND-CHILD(hMHNode2).
        
        /* CRIA NODE standardName */
        hGlobalMHReturnError:CREATE-NODE(hMHNode3, "standardName", "ELEMENT").    
        hGlobalMHReturnError:CREATE-NODE(hMHNode4, "", "TEXT").    
        hMHNode4:NODE-VALUE = "Datasul".
        
        /* ADICIONA NODE ACIMA CRIADO, AO XML PRINCIPAL */
        hMHNode2:APPEND-CHILD(hMHNode3).
        hMHNode3:APPEND-CHILD(hMHNode4).
        
        /* CRIA NODE standardName */
        hGlobalMHReturnError:CREATE-NODE(hMHNode3, "standardVersion", "ELEMENT").    
        hGlobalMHReturnError:CREATE-NODE(hMHNode4, "", "TEXT").    
        hMHNode4:NODE-VALUE = "200_205".
          
        /* ADICIONA NODE ACIMA CRIADO, AO XML PRINCIPAL */
        hMHNode2:APPEND-CHILD(hMHNode3).
        hMHNode3:APPEND-CHILD(hMHNode4).
        
        /* CRIA NODE ReturnContent */
        hGlobalMHReturnError:CREATE-NODE(hMHNode2, "ReturnContent", "ELEMENT").    
        /* ADICIONA NODE ACIMA CRIADO, AO XML PRINCIPAL */
        hMHNode1:APPEND-CHILD(hMHNode2).
        
        /* CRIA NODE IsMessageProcessed */
        hGlobalMHReturnError:CREATE-NODE(hMHNode3, "IsMessageProcessed", "ELEMENT").    
        hGlobalMHReturnError:CREATE-NODE(hMHNode4, "", "TEXT").    
        hMHNode4:NODE-VALUE = "no".
          
        /* ADICIONA NODE ACIMA CRIADO, AO XML PRINCIPAL */
        hMHNode2:APPEND-CHILD(hMHNode3).
        hMHNode3:APPEND-CHILD(hMHNode4).
        
        /* CRIA NODE ListOfError */
        hGlobalMHReturnError:CREATE-NODE(hMHNode3, "ListOfError", "ELEMENT").    
        /* ADICIONA NODE ACIMA CRIADO, AO XML PRINCIPAL */
        hMHNode2:APPEND-CHILD(hMHNode3).
        
        /* CRIA NODE Error */
        hGlobalMHReturnError:CREATE-NODE(hMHNode4, "Error", "ELEMENT").    
        hMHNode3:APPEND-CHILD(hMHNode4).
        
        /* CRIA NODE Id */
        hGlobalMHReturnError:CREATE-NODE(hMHNode5, "Id", "ELEMENT").    
        hGlobalMHReturnError:CREATE-NODE(hMHNode6, "", "TEXT").    
        /* ADICIONA NODE ACIMA CRIADO, AO XML PRINCIPAL */
        hMHNode6:NODE-VALUE = "1".
    
        /* ADICIONA NODE ACIMA CRIADO, AO XML PRINCIPAL */
        hMHNode4:APPEND-CHILD(hMHNode5).
        hMHNode5:APPEND-CHILD(hMHNode6).
    
        /* CRIA NODE Type */
        hGlobalMHReturnError:CREATE-NODE(hMHNode5, "Type", "ELEMENT").    
        hGlobalMHReturnError:CREATE-NODE(hMHNode6, "", "TEXT").    
        /* ADICIONA NODE ACIMA CRIADO, AO XML PRINCIPAL */
        hMHNode6:NODE-VALUE = "environment_error".
    
        /* ADICIONA NODE ACIMA CRIADO, AO XML PRINCIPAL */
        hMHNode4:APPEND-CHILD(hMHNode5).
        hMHNode5:APPEND-CHILD(hMHNode6).
    
        /* CRIA NODE Desc */
        hGlobalMHReturnError:CREATE-NODE(hMHNode5, "Desc", "ELEMENT").    
        hGlobalMHReturnError:CREATE-NODE(hMHNode6, "", "TEXT").    
        /* ADICIONA NODE ACIMA CRIADO, AO XML PRINCIPAL */
        hMHNode6:NODE-VALUE = cGlobalMHReturnValue.
    
        /* ADICIONA NODE ACIMA CRIADO, AO XML PRINCIPAL */
        hMHNode4:APPEND-CHILD(hMHNode5).
        hMHNode5:APPEND-CHILD(hMHNode6).
        
        /* ELIMINA OBJETOS N√O MAIS NECESS¡RIOS */
        DELETE OBJECT hMHNode1 NO-ERROR.
        DELETE OBJECT hMHNode2 NO-ERROR.
        DELETE OBJECT hMHNode3 NO-ERROR.
        DELETE OBJECT hMHNode4 NO-ERROR.
        DELETE OBJECT hMHNode5 NO-ERROR.
        DELETE OBJECT hMHNode6 NO-ERROR.
    
        /* ASSOCIA VALORES OBTIDO INTERNAMENTE AS VARIAVIES EXTERNAS */
        ASSIGN {&MhReturnError} = hGlobalMHReturnError.
    END.
&ENDIF

/* ASSOCIA VALORES OBTIDO INTERNAMENTE AS VARIAVIES EXTERNAS */
ASSIGN {&MessageHandler} = hGlobalMessageHandler
       {&MHReturnValue}  = cGlobalMHReturnValue.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


