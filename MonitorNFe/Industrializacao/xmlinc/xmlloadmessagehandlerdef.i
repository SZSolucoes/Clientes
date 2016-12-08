&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : XMLLoadMessageHandlerDef.i
    Purpose     : Defini‡Æo de vari veis utilizadas para carregar o 
                  programa xmlutp/message_handler.p na mem¢ria.

    Syntax      :
    
    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .i file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Verifica se include j  est  definido --- */
&IF NOT "{&XMLLoadMessageHandlerDef}" = "TRUE" &THEN
    /* Preprocessar para indicar que include j  est  definido --- */
    &GLOBAL-DEFINE XMLLoadMessageHandlerDef TRUE
    
    
    /* Defini‡Æo de vari veis globais --------------------------------- */
    DEFINE NEW GLOBAL SHARED VARIABLE hGlobalMessageHandler AS HANDLE NO-UNDO.
    
    /* Defini‡Æo de vari veis locais ----------------------------------*/
    DEFINE VARIABLE cGlobalMHReturnValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hGlobalMHReturnError AS HANDLE    NO-UNDO.
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
