&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : xmlLoadMessageHandlerRecDef.i
    Purpose     : Define variaveis do xmlLoadMessageHandlerRec.i

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .i file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* VARIAVEIS GLOBAIS */
DEF NEW GLOBAL SHARED VAR hGlobalMessageHandler  AS HANDLE NO-UNDO.
DEF                   VAR hGlobalMHReturnError   AS HANDLE NO-UNDO.
DEF                   VAR cGlobalMHReturnValue   AS CHAR   NO-UNDO.

DEF                   VAR hMHNode1               AS HANDLE NO-UNDO.
DEF                   VAR hMHNode2               AS HANDLE NO-UNDO.
DEF                   VAR hMHNode3               AS HANDLE NO-UNDO.
DEF                   VAR hMHNode4               AS HANDLE NO-UNDO.
DEF                   VAR hMHNode5               AS HANDLE NO-UNDO.
DEF                   VAR hMHNode6               AS HANDLE NO-UNDO.

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


