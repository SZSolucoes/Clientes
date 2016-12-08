&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : xmlLoadGenXml.i
    Purpose     : Verfica se a APIXML já foi carregado anteriormente

    Syntax      :

    Description : 

    Author(s)   : Christopher de Siqueira (tech14145)
    Created     : 30/12/2002 15:01
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

/* ASSOCIA VARIAVEL Á PROCESSAMENTO CORRETO */
ASSIGN cGlobalGXReturnValue = "OK".

/* VERIFICA SE A API XML JÁ ESTA NA MEMORIA */
IF NOT VALID-HANDLE(hGlobalGenXml)             OR 
   hGlobalGenXml:TYPE <> "PROCEDURE":U         OR 
   hGlobalGenXml:FILE-NAME <> "xmlutp/ut-genxml.p":U 
THEN DO:
    /* VERIFICA SE O ARQUIVO QUE SERÁ POSTO EM PERSISTENCIA EXISTE */
    IF SEARCH("xmlutp/ut-genxml.r") <> ? OR SEARCH("xmlutp/ut-genxml.p") <> ?
    THEN 
        /* COLOCA EM PERSISTENCIA O ut-genxml */
        RUN xmlutp/ut-genxml.p PERSISTENT SET hGlobalGenXml.
    ELSE 
        /* ASSOCIA O ERRO A VARIAVEL */
        ASSIGN cGlobalGXReturnValue = "Arquivo xmlutp/ut-genxml.p não foi encontrado".
END.
ELSE 
    /* RESET AS VARIAVEIS LOCAIS */
    RUN reset IN hGlobalGenXml.

/* VERIFICA SE O MessageHandler NÃO FOI EXECUTADO CORRETAMENTE */
IF cGlobalGXReturnValue <> "OK" 
THEN DO:
    /* **** GERA XML DE ERRO PARA SER RETORNADO AO CONTROL BROKER **** */
    /* CRIA X-DOCUMENTs E X-NODEREFs PARA CRIAR XML DE ERRO */
    CREATE X-DOCUMENT hGlobalGXReturnError.
    CREATE X-NODEREF hGXNode1.
    CREATE X-NODEREF hGXNode2.
    CREATE X-NODEREF hGXNode3.
    CREATE X-NODEREF hGXNode4.
    CREATE X-NODEREF hGXNode5.
    CREATE X-NODEREF hGXNode6.
    
    /* CRIA NODE PRINCIPAL DATASUL */
    hGlobalGXReturnError:CREATE-NODE(hGXNode1, "DatasulMessage", "ELEMENT").    
    /* INFORMA O ENCODING DO XML */
    hGlobalGXReturnError:ENCODING = "UTF-8". 
    
    /* CRIA NODE SECUNDARIO PREAMBLE */
    hGlobalGXReturnError:CREATE-NODE(hGXNode2, "Preamble", "ELEMENT").    
    
    /* ADICIONA NODE ACIMA NO XML PRINCIPAL */
    hGlobalGXReturnError:APPEND-CHILD(hGXNode1).
    /* ADICIONA NODE ACIMA CRIADO, AO XML PRINCIPAL */
    hGXNode1:APPEND-CHILD(hGXNode2).
    
    /* CRIA NODE standardName */
    hGlobalGXReturnError:CREATE-NODE(hGXNode3, "standardName", "ELEMENT").    
    hGlobalGXReturnError:CREATE-NODE(hGXNode4, "", "TEXT").    
    hGXNode4:NODE-VALUE = "Datasul".
    
    /* ADICIONA NODE ACIMA CRIADO, AO XML PRINCIPAL */
    hGXNode2:APPEND-CHILD(hGXNode3).
    hGXNode3:APPEND-CHILD(hGXNode4).
    
    /* CRIA NODE standardName */
    hGlobalGXReturnError:CREATE-NODE(hGXNode3, "standardVersion", "ELEMENT").    
    hGlobalGXReturnError:CREATE-NODE(hGXNode4, "", "TEXT").    
    hGXNode4:NODE-VALUE = "200_205".
      
    /* ADICIONA NODE ACIMA CRIADO, AO XML PRINCIPAL */
    hGXNode2:APPEND-CHILD(hGXNode3).
    hGXNode3:APPEND-CHILD(hGXNode4).
    
    /* CRIA NODE ReturnContent */
    hGlobalGXReturnError:CREATE-NODE(hGXNode2, "ReturnContent", "ELEMENT").    
    /* ADICIONA NODE ACIMA CRIADO, AO XML PRINCIPAL */
    hGXNode1:APPEND-CHILD(hGXNode2).
    
    /* CRIA NODE IsMessageProcessed */
    hGlobalGXReturnError:CREATE-NODE(hGXNode3, "IsMessageProcessed", "ELEMENT").    
    hGlobalGXReturnError:CREATE-NODE(hGXNode4, "", "TEXT").    
    hGXNode4:NODE-VALUE = "no".
      
    /* ADICIONA NODE ACIMA CRIADO, AO XML PRINCIPAL */
    hGXNode2:APPEND-CHILD(hGXNode3).
    hGXNode3:APPEND-CHILD(hGXNode4).
    
    /* CRIA NODE ListOfError */
    hGlobalGXReturnError:CREATE-NODE(hGXNode3, "ListOfError", "ELEMENT").    
    /* ADICIONA NODE ACIMA CRIADO, AO XML PRINCIPAL */
    hGXNode2:APPEND-CHILD(hGXNode3).
    
    /* CRIA NODE Error */
    hGlobalGXReturnError:CREATE-NODE(hGXNode4, "Error", "ELEMENT").    
    hGXNode3:APPEND-CHILD(hGXNode4).
    
    /* CRIA NODE Id */
    hGlobalGXReturnError:CREATE-NODE(hGXNode5, "Id", "ELEMENT").    
    hGlobalGXReturnError:CREATE-NODE(hGXNode6, "", "TEXT").    
    /* ADICIONA NODE ACIMA CRIADO, AO XML PRINCIPAL */
    hGXNode6:NODE-VALUE = "1".

    /* ADICIONA NODE ACIMA CRIADO, AO XML PRINCIPAL */
    hGXNode4:APPEND-CHILD(hGXNode5).
    hGXNode5:APPEND-CHILD(hGXNode6).

    /* CRIA NODE Type */
    hGlobalGXReturnError:CREATE-NODE(hGXNode5, "Type", "ELEMENT").    
    hGlobalGXReturnError:CREATE-NODE(hGXNode6, "", "TEXT").    
    /* ADICIONA NODE ACIMA CRIADO, AO XML PRINCIPAL */
    hGXNode6:NODE-VALUE = "environment_error".

    /* ADICIONA NODE ACIMA CRIADO, AO XML PRINCIPAL */
    hGXNode4:APPEND-CHILD(hGXNode5).
    hGXNode5:APPEND-CHILD(hGXNode6).

    /* CRIA NODE Desc */
    hGlobalGXReturnError:CREATE-NODE(hGXNode5, "Desc", "ELEMENT").    
    hGlobalGXReturnError:CREATE-NODE(hGXNode6, "", "TEXT").    
    /* ADICIONA NODE ACIMA CRIADO, AO XML PRINCIPAL */
    hGXNode6:NODE-VALUE = cGlobalGXReturnValue.

    /* ADICIONA NODE ACIMA CRIADO, AO XML PRINCIPAL */
    hGXNode4:APPEND-CHILD(hGXNode5).
    hGXNode5:APPEND-CHILD(hGXNode6).
    
    /* ELIMINA OBJETOS NÃO MAIS NECESSÁRIOS */
    DELETE OBJECT hGXNode1 NO-ERROR.
    DELETE OBJECT hGXNode2 NO-ERROR.
    DELETE OBJECT hGXNode3 NO-ERROR.
    DELETE OBJECT hGXNode4 NO-ERROR.
    DELETE OBJECT hGXNode5 NO-ERROR.
    DELETE OBJECT hGXNode6 NO-ERROR.

    /* ASSOCIA VALORES OBTIDO INTERNAMENTE AS VARIAVIES EXTERNAS */
    ASSIGN {&GXReturnError} = hGlobalGXReturnError.
END.

/* ASSOCIA HANDLE OBTIDA Á VARIAVEL UTILIZADA EXTERNAMENTE */
ASSIGN {&GenXml}        = hGlobalGenXml
       {&GXReturnValue} = cGlobalGXReturnValue.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


