&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : xmlgettranssubsinfo.i
    Purpose     : DEFINE A PROCEDURE getTransSubsInfo

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getTransSubsInfo Include 
PROCEDURE getTransSubsInfo :
/*------------------------------------------------------------------------------
  Purpose: OBTEM OS VALORES DOS PRÉ-PROCESSADORES DO ADAPTER DE RECEBIMENTO 
  Parameters: cXmlTranName, cXmlMinTranVersion, cXmlMaxTranVersion, cXmlListOfEvents
  Notes:       
------------------------------------------------------------------------------*/

    /* PARAMETROS DE SAÍDA */
    DEF OUTPUT PARAM cXmlTranName       AS CHAR NO-UNDO.
    DEF OUTPUT PARAM cXmlMinTranVersion AS CHAR NO-UNDO.
    DEF OUTPUT PARAM cXmlMaxTranVersion AS CHAR NO-UNDO.
    DEF OUTPUT PARAM cXmlListOfEvents   AS CHAR NO-UNDO.
    DEF OUTPUT PARAM cXmlListOfAction   AS CHAR NO-UNDO.

    /* ASSOCIA AOS PARAMETROS DE RETORNO OS VALORES DOS PRÉ-PROCESSADORES */
    ASSIGN 
        &IF DEFINED(vXmlTranName) <> 0 &THEN
            cXmlTranName       = {&vXmlTranName}
        &ELSE
            cXmlTranName       = "{&XmlTranName}"
        &ENDIF
        
        &IF DEFINED(vXmlMinTranVersion) <> 0 &THEN
            cXmlMinTranVersion = {&vXmlMinTranVersion}
        &ELSE
            cXmlMinTranVersion = "{&XmlMinTranVersion}"
        &ENDIF
        
        &IF DEFINED(vXmlMaxTranVersion) <> 0 &THEN
            cXmlMaxTranVersion = {&vXmlMaxTranVersion}
        &ELSE
            cXmlMaxTranVersion = "{&XmlMaxTranVersion}"
        &ENDIF
        
        &IF DEFINED(vXmlListOfEvents) <> 0 &THEN
            cXmlListOfEvents   = {&vXmlListOfEvents}
        &ELSE
            cXmlListOfEvents   = "{&XmlListOfEvents}"
        &ENDIF
        
        &IF DEFINED(vXmlListOfAction) <> 0 &THEN
            cXmlListOfAction   = {&vXmlListOfAction}
        &ELSE
            cXmlListOfAction   = "{&XmlListOfAction}"
        &ENDIF
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

