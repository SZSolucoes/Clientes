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

/* VARIAVEL GLOBAL */
DEF NEW GLOBAL SHARED VAR hGlobalGenXml        AS HANDLE NO-UNDO.
DEF                   VAR cGlobalGXReturnValue AS CHAR   NO-UNDO.

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

/* ASSOCIA HANDLE OBTIDA Á VARIAVEL UTILIZADA EXTERNAMENTE */
ASSIGN {&GenXml}        = hGlobalGenXml
       {&GXReturnValue} = cGlobalGXReturnValue.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


