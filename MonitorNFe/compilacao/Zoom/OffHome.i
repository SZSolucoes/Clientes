&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
/* Procedure Description
"L�gica para a trigger OFF-END de browse."
*/
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*--------------------------------------------------------------------------
    File       : zoom/OffHome.i
    Purpose    : L�gica para a trigger OFF-HOME de browse

    Parameters : 
        &PageNumber : n�mero da p�gina onde est� o browse, a ser utilizado
                      para definir o nome de alguns widgets tais como
                      brTable{&PageNumber}

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

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
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
 



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/*--- Executa procedure applyPageDown para disparar o evento de OFF-HOME ---*/
RUN applyOffHome IN THIS-PROCEDURE (INPUT {&PageNumber}).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


