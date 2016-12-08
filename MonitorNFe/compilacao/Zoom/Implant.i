&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
/* Procedure Description
"Chama programa de inclusÆo de filhos, … partir de programas do tipo MasterDetail."
*/
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*--------------------------------------------------------------------------
    File       : zoom/Implant.i
    Purpose    : Chama programa de implanta‡Æo, … partir de programas do 
                 tipo Zoom

    Parameters : 
        &ProgramImplant : nome do programa de filhos
        &PageNumber     : n£mero da p gina onde est  o browse, a ser 
                          utilizado para definir o nome de alguns widgets 
                          tais como brTable{&PageNumber}

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
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
 



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/*Lógica incluída por Anderson tech540 para desabilitar a janela de zoom quando 
o mesmo chamar o programa de cadastro para inplantar o novo registro 23/07/2001*/
/*Desabilita janela de zoom até o programa de cadastro ser fechado*/
{&WINDOW-NAME}:SENSITIVE = NO.

/*--- Executa programa de implanta‡Æo ---*/
RUN {&ProgramImplant} PERSISTENT SET hProgramImplant.

/* Alterado por Valdir (tech264) para novo m‚todo de teste do valid-handle,
   onde agora sÆo testados tamb‚m os atributos TYPE e FILE-NAME do handle.  */
   
IF VALID-HANDLE(hProgramImplant)             AND
   hProgramImplant:TYPE      = "PROCEDURE":U AND
   hProgramImplant:FILE-NAME = "{&ProgramImplant}":U THEN
    /*--- Inicializa programa de implanta‡Æo ---*/
    RUN initializeInterface IN hProgramImplant.

IF VALID-HANDLE(hProgramImplant)             AND
   hProgramImplant:TYPE      = "PROCEDURE":U AND
   hProgramImplant:FILE-NAME = "{&ProgramImplant}":U THEN DO:
    WAIT-FOR CLOSE OF hProgramImplant.
    
    /*--- Posiciona no £ltimo registro utilizado no programa de implanta‡Æo ---*/
    IF {&rTable{&PageNumber}} <> ? THEN DO:
        ASSIGN iRepositionPageNumber = {&PageNumber}
               rRepositionTable      = {&rTable{&PageNumber}}.
        
        /*--- Atualiza browse da p gina {&PageNumber} ---*/
        RUN openQueries IN THIS-PROCEDURE.
    END.
END.

/*Lógica incluída por Anderson tech540 para desabilitar a janela de zoom quando 
o mesmo chamar o programa de cadastro para inplantar o novo registro 23/07/2001*/
/*habilita janela de zoom apos o programa de cadastro ser fechado*/
{&WINDOW-NAME}:SENSITIVE = YES.

/*Entra com o focus no zoom*/
APPLY "entry":U TO {&WINDOW-NAME}.
/*fim alteração Anderson tech485 23/07/2001 */


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


