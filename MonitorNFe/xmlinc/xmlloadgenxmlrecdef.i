&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : xmlLoadGenXml.i
    Purpose     : Verfica se a APIXML j� foi carregado anteriormente

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
DEF NEW GLOBAL SHARED VAR hGlobalGenXml          AS HANDLE NO-UNDO.
DEF                   VAR hGlobalGXReturnError   AS HANDLE NO-UNDO.
DEF                   VAR cGlobalGXReturnValue   AS CHAR   NO-UNDO.

DEF                   VAR hGXNode1               AS HANDLE NO-UNDO.
DEF                   VAR hGXNode2               AS HANDLE NO-UNDO.
DEF                   VAR hGXNode3               AS HANDLE NO-UNDO.
DEF                   VAR hGXNode4               AS HANDLE NO-UNDO.
DEF                   VAR hGXNode5               AS HANDLE NO-UNDO.
DEF                   VAR hGXNode6               AS HANDLE NO-UNDO.

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


