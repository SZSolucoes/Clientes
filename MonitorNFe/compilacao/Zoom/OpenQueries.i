&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
/* Procedure Description
"Atualiza browsers em janelas de Zoom ."
*/
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*--------------------------------------------------------------------------
    File       : zoom/OpenQueries.i
    Purpose    : Atualiza browsers em janelas de Zoom 

    Parameters : 
        &Query        : nome da query, a ser utilizado para passagem de 
                        parƒmetro para o m‚todo openQueryStatic
        &PageNumber   : n£mero da p gina onde est  o browse, a ser utilizado
                        para definir o nome de alguns widgets tais como
                        brTable{&PageNumber}

        Somente para BO 1.1:
        
        &QueryNumber  : n£mero da query, a ser utilizado para passagem de 
                        parƒmetro para o m‚todo openQuery

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

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
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
 



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

IF ((iConstraintPageNumber = 0 OR iConstraintPageNumber = {&PageNumber}) AND iRepositionPageNumber = 0) OR
   ((iRepositionPageNumber = 0 OR iRepositionPageNumber = {&PageNumber}) AND iConstraintPageNumber = 0) THEN DO:

    IF AVAILABLE {&ttTable{&PageNumber}} THEN 
        ASSIGN epc-rowid{&PageNumber} = {&ttTable{&PageNumber}}.r-Rowid.
    ELSE 
        ASSIGN epc-rowid{&PageNumber} = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="BEFORE-OPEN-QUERY"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable{&PageNumber}}"
                     &RowidTable="epc-rowid{&PageNumber}"}
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Abre query do DBO ---*/
        RUN openQuery IN {&hDBOTable{&PageNumber}} (INPUT {&QueryNumber}).
        
        /*--- Posiciona DBO no primeiro registro, caso necess rio ---*/
        IF iRepositionPageNumber = 0
           THEN RUN findFirst IN {&hDBOTable{&PageNumber}}.
        
        /*--- Retorna faixa de registro do DBO ---*/
        RUN serverSendRows IN {&hDBOTable{&PageNumber}} (INPUT ?,
                                                         INPUT IF iRepositionPageNumber = {&PageNumber}
                                                                 THEN STRING(rRepositionTable)
                                                                 ELSE ?,
                                                         INPUT NO,
                                                         INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                               &ELSE 40 &endif,
                                                         OUTPUT iRowsReturned,
                                                         OUTPUT TABLE {&ttTable{&PageNumber}}).
    &ELSE
        /*--- Abre query do DBO ---*/
        RUN openQueryStatic IN {&hDBOTable{&PageNumber}} (INPUT "{&Query}":U).
        
        /*--- Retorna faixa de registro do DBO ---*/
        RUN getBatchRecords IN {&hDBOTable{&PageNumber}} (INPUT IF iRepositionPageNumber = {&PageNumber}
                                                                  THEN rRepositionTable
                                                                  ELSE ?,
                                                          INPUT NO,
                                                          INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                                &ELSE 40 &endif,
                                                          OUTPUT iRowsReturned,
                                                          OUTPUT TABLE {&ttTable{&PageNumber}}).
    &ENDIF
    
    /*--- Abre a query associada ao browse da p gina {&PageNumber} ---*/
    {&OPEN-QUERY-{&page{&PageNumber}browse}}
    APPLY "VALUE-CHANGED":U TO BROWSE {&page{&PageNumber}browse}.
    
    /*--- Seta vari vel iConstraintPageNumber com o valor 0 (zero), desta forma
          ‚ impedido que outra p gina seja reposicionada ---*/
    ASSIGN iConstraintPageNumber = 0.
    
    /*--- Seta vari vel iRepositionPageNumber com o valor 0 (zero), desta forma
          ‚ impedido que outra p gina seja reposicionada ---*/
    ASSIGN iRepositionPageNumber = 0.

    IF AVAILABLE {&ttTable{&PageNumber}} THEN 
        ASSIGN epc-rowid{&PageNumber} = {&ttTable{&PageNumber}}.r-Rowid.
    ELSE 
        ASSIGN epc-rowid{&PageNumber} = ?.
    
    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    {method/custom.i &Event="AFTER-OPEN-QUERY"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="{&DBOTable{&PageNumber}}"
                     &RowidTable="epc-rowid{&PageNumber}"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


