&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Method-Library 
/*-------------------------------------------------------------------------
    Library     : query.i  
    Purpose     : Basic ADM methods for query objects

    Syntax      : {src/adm/method/query.i}

    Description :

    Author(s)   :
    Created     :
    Notes       :
    HISTORY: 

--------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&IF DEFINED (adm-query) = 0 &THEN
&GLOBAL adm-query yes

&GLOBAL adm-open-query yes

/* Dialog program to run to set runtime attributes - if not defined in master */
&IF DEFINED(adm-attribute-dlg) = 0 &THEN
&SCOP adm-attribute-dlg adm/support/queryd.w
&ENDIF

/************ Customizacao Datasul S.A. - John Cleber Jaraceski
              Ao utilizar Dataserver Oracle ou SQL-Server, deve-se trocar o uso de QUERY por FIND
              **********************************/
&IF "{&EMS_DBTYPE}":U = "PROGRESS":U &THEN
    &IF DEFINED(CHANGE-QUERY-TO-FIND) <> 0 &THEN
        &UNDEFINE CHANGE-QUERY-TO-FIND
    &ENDIF
&ENDIF

&IF "{&CHANGE-QUERY-TO-FIND}":U EQ "TRUE":U &THEN
    &IF DEFINED(WHERE-FIRST-TABLE-IN-QUERY-{&QUERY-NAME}) EQ 0 &THEN
        &GLOBAL-DEFINE WHERE-FIRST-TABLE-IN-QUERY-{&QUERY-NAME} TRUE
    &ENDIF
    
    &IF DEFINED(JOIN-SECOND-TABLE-IN-QUERY-{&QUERY-NAME}) EQ 0 &THEN
        &GLOBAL-DEFINE JOIN-SECOND-TABLE-IN-QUERY-{&QUERY-NAME} TRUE
    &ENDIF
    
    &IF DEFINED(WHERE-SECOND-TABLE-IN-QUERY-{&QUERY-NAME}) EQ 0 &THEN
        &GLOBAL-DEFINE WHERE-SECOND-TABLE-IN-QUERY-{&QUERY-NAME} TRUE
    &ENDIF
    
    &IF DEFINED(JOIN-THIRD-TABLE-IN-QUERY-{&QUERY-NAME}) EQ 0 &THEN
        &GLOBAL-DEFINE JOIN-THIRD-TABLE-IN-QUERY-{&QUERY-NAME} TRUE
    &ENDIF
    
    &IF DEFINED(WHERE-THIRD-TABLE-IN-QUERY-{&QUERY-NAME}) EQ 0 &THEN
        &GLOBAL-DEFINE WHERE-THIRD-TABLE-IN-QUERY-{&QUERY-NAME} TRUE
    &ENDIF    
&ENDIF
/***********************************************/

/* +++ This is the list of attributes whose values are to be returned
   by get-attribute-list, that is, those whose values are part of the
   definition of the object instance and should be passed to init-object
   by the UIB-generated code in adm-create-objects. */
&IF DEFINED(adm-attribute-list) = 0 &THEN
&SCOP adm-attribute-list Key-Name,SortBy-Case
&ENDIF

  DEFINE VARIABLE adm-first-rowid AS ROWID NO-UNDO /* INIT ? */.
  DEFINE VARIABLE adm-last-rowid  AS ROWID NO-UNDO /* INIT ? */.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Method-Library
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Method-Library ASSIGN
         HEIGHT             = 6.88
         WIDTH              = 66.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Method-Library 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}
{src/adm/method/record.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Method-Library 


/* ***************************  Main Block  *************************** */

ASSIGN adm-first-rowid = ?
       adm-last-rowid  = ?.   /* INIT doesn't work for ROWIDs */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-adm-get-first) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-get-first Method-Library 
PROCEDURE adm-get-first :
/* -----------------------------------------------------------
  Purpose:     Gets the first record in the default query.
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/

  &IF DEFINED(FIRST-TABLE-IN-QUERY-{&QUERY-NAME}) NE 0 &THEN
    IF NOT adm-query-opened THEN RETURN.

/************ Customizacao Datasul S.A. - John Cleber Jaraceski
              Ao utilizar Dataserver Oracle ou SQL-Server, deve-se trocar o uso de QUERY por FIND
              **********************************/
    &IF "{&CHANGE-QUERY-TO-FIND}":U EQ "TRUE":U &THEN
/* tech1139 - 27/07/2005 - FO 1186.555 */        
        FIND FIRST {&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} NO-LOCK
            &IF "{&WHERE-FIRST-TABLE-IN-QUERY-{&QUERY-NAME}}":U NE "":U &THEN
                WHERE {&WHERE-FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} NO-ERROR.
            &ELSE
                NO-ERROR.
            &ENDIF
/* tech1139 - 27/07/2005 - FO 1186.555 */
        &IF DEFINED(SECOND-TABLE-IN-QUERY-{&QUERY-NAME}) NE 0 &THEN
            FIND FIRST {&SECOND-TABLE-IN-QUERY-{&QUERY-NAME}} NO-LOCK 
                WHERE {&JOIN-SECOND-TABLE-IN-QUERY-{&QUERY-NAME}} AND
                      {&WHERE-SECOND-TABLE-IN-QUERY-{&QUERY-NAME}}
                NO-ERROR.

            DO WHILE AVAILABLE {&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} AND
                     NOT AVAILABLE {&SECOND-TABLE-IN-QUERY-{&QUERY-NAME}}: /* inner-join */
                FIND NEXT {&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} NO-LOCK 
                    WHERE {&WHERE-FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} NO-ERROR.

                FIND FIRST {&SECOND-TABLE-IN-QUERY-{&QUERY-NAME}} NO-LOCK 
                    WHERE {&JOIN-SECOND-TABLE-IN-QUERY-{&QUERY-NAME}} AND
                          {&WHERE-SECOND-TABLE-IN-QUERY-{&QUERY-NAME}}
                    NO-ERROR.
            END.

            IF NOT AVAILABLE {&SECOND-TABLE-IN-QUERY-{&QUERY-NAME}} THEN
                RELEASE {&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} NO-ERROR. /* inner-join obriga existência da primeira e segunda tabela */
        &ENDIF
    &ELSE
        GET FIRST {&QUERY-NAME}.
    &ENDIF
/***********************************************/

    IF AVAILABLE {&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} THEN
    DO:
        IF NUM-ENTRIES("{&TABLES-IN-QUERY-{&QUERY-NAME}}":U," ":U) = 1 THEN
            ASSIGN adm-first-rowid = 
                ROWID ({&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}}).
        RUN new-state ('first-record,SELF':U).
    END.

    RUN dispatch IN THIS-PROCEDURE ('row-changed':U) .

  &ELSEIF DEFINED(FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}) NE 0 &THEN
    DEFINE VARIABLE browser-handle AS HANDLE  NO-UNDO.
    DEFINE VARIABLE q-stat         AS LOGICAL NO-UNDO.

    IF NOT adm-query-opened THEN RETURN.
    DO WITH FRAME {&FRAME-NAME}:
      IF num-results("{&BROWSE-NAME}":U) = 0 THEN  /* Browse is empty */
        RUN new-state ('no-record-available,SELF':U).
      ELSE DO:
        browser-handle = {&BROWSE-NAME}:HANDLE.
        APPLY "HOME":U TO BROWSE {&BROWSE-NAME}.
        q-stat = browser-handle:SELECT-FOCUSED-ROW(). 
        IF NUM-ENTRIES("{&TABLES-IN-QUERY-{&BROWSE-NAME}}":U," ":U) = 1 THEN
            ASSIGN adm-first-rowid = 
                ROWID ({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
        IF num-results("{&BROWSE-NAME}":U) = 1 THEN  /* Just one row */
            RUN new-state ('only-record,SELF':U).
        ELSE RUN new-state ('first-record,SELF':U).
      END.
    END.
  &ENDIF
    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-adm-get-last) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-get-last Method-Library 
PROCEDURE adm-get-last :
/* -----------------------------------------------------------
  Purpose:     gets the last record in the default query.
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/

  &IF DEFINED(FIRST-TABLE-IN-QUERY-{&QUERY-NAME}) NE 0 &THEN
    IF NOT adm-query-opened THEN RETURN.

/************ Customizacao Datasul S.A. - John Cleber Jaraceski
              Ao utilizar Dataserver Oracle ou SQL-Server, deve-se trocar o uso de QUERY por FIND
              **********************************/
    &IF "{&CHANGE-QUERY-TO-FIND}":U EQ "TRUE":U &THEN
/* tech1139 - 27/07/2005 - FO 1186.555 */
        FIND LAST {&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} NO-LOCK
            &IF "{&WHERE-FIRST-TABLE-IN-QUERY-{&QUERY-NAME}}":U NE "":U &THEN
                WHERE {&WHERE-FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} NO-ERROR.
            &ELSE
                NO-ERROR.
            &ENDIF
/* tech1139 - 27/07/2005 - FO 1186.555 */
        &IF DEFINED(SECOND-TABLE-IN-QUERY-{&QUERY-NAME}) NE 0 &THEN
            FIND LAST {&SECOND-TABLE-IN-QUERY-{&QUERY-NAME}} NO-LOCK 
                WHERE {&JOIN-SECOND-TABLE-IN-QUERY-{&QUERY-NAME}} AND
                      {&WHERE-SECOND-TABLE-IN-QUERY-{&QUERY-NAME}}
                NO-ERROR.

            DO WHILE AVAILABLE {&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} AND
                     NOT AVAILABLE {&SECOND-TABLE-IN-QUERY-{&QUERY-NAME}}: /* inner-join */
                FIND PREV {&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} NO-LOCK 
                    WHERE {&WHERE-FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} NO-ERROR.

                FIND LAST {&SECOND-TABLE-IN-QUERY-{&QUERY-NAME}} NO-LOCK 
                    WHERE {&JOIN-SECOND-TABLE-IN-QUERY-{&QUERY-NAME}} AND
                          {&WHERE-SECOND-TABLE-IN-QUERY-{&QUERY-NAME}}
                    NO-ERROR.
            END.

            IF NOT AVAILABLE {&SECOND-TABLE-IN-QUERY-{&QUERY-NAME}} THEN
                RELEASE {&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} NO-ERROR. /* inner-join obriga existência da primeira e segunda tabela */
        &ENDIF
    &ELSE
        GET LAST {&QUERY-NAME}.
    &ENDIF
/***********************************************/

    IF AVAILABLE {&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} THEN 
    DO:
        IF NUM-ENTRIES("{&TABLES-IN-QUERY-{&QUERY-NAME}}":U," ":U) = 1 THEN
            ASSIGN adm-last-rowid = 
                ROWID ({&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}}).
        IF adm-last-rowid NE ? AND adm-first-rowid = adm-last-rowid THEN
            RUN new-state ('only-record,SELF':U).  /* Just one rec in dataset */
        ELSE RUN new-state ('last-record,SELF':U).
    END.
    RUN dispatch IN THIS-PROCEDURE ('row-changed':U).

  &ELSEIF DEFINED(FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}) NE 0 &THEN
    DEFINE VARIABLE browser-handle AS HANDLE  NO-UNDO.
    DEFINE VARIABLE q-stat         AS LOGICAL NO-UNDO.

    IF NOT adm-query-opened THEN RETURN.
    DO WITH FRAME {&FRAME-NAME}:
      IF num-results("{&BROWSE-NAME}":U) = 0 THEN  /* Browse is empty */
        RUN new-state ('no-record-available,SELF':U).
      ELSE DO:
        browser-handle = {&BROWSE-NAME}:HANDLE.
        APPLY "END":U TO BROWSE {&BROWSE-NAME}.
        q-stat = browser-handle:SELECT-FOCUSED-ROW(). 
        IF NUM-ENTRIES("{&TABLES-IN-QUERY-{&BROWSE-NAME}}":U," ":U) = 1 THEN
            ASSIGN adm-last-rowid = 
                ROWID ({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
        RUN new-state ('last-record,SELF':U).
      END.
    END.
  &ENDIF
    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-adm-get-next) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-get-next Method-Library 
PROCEDURE adm-get-next :
/* -----------------------------------------------------------
  Purpose:     Gets the next record in the default query.
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/

  &IF DEFINED(FIRST-TABLE-IN-QUERY-{&QUERY-NAME}) NE 0 &THEN
    IF NOT adm-query-opened THEN RETURN.

/************ Customizacao Datasul S.A. - John Cleber Jaraceski
              Ao utilizar Dataserver Oracle ou SQL-Server, deve-se trocar o uso de QUERY por FIND
              **********************************/
    &IF "{&CHANGE-QUERY-TO-FIND}":U EQ "TRUE":U &THEN
        IF NOT tt-uib-reposition-find THEN DO:
            &IF DEFINED(SECOND-TABLE-IN-QUERY-{&QUERY-NAME}) NE 0 &THEN
                FIND NEXT {&SECOND-TABLE-IN-QUERY-{&QUERY-NAME}} NO-LOCK 
                    WHERE {&JOIN-SECOND-TABLE-IN-QUERY-{&QUERY-NAME}} AND
                          {&WHERE-SECOND-TABLE-IN-QUERY-{&QUERY-NAME}}
                    NO-ERROR.

                DO WHILE AVAILABLE {&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} AND
                         NOT AVAILABLE {&SECOND-TABLE-IN-QUERY-{&QUERY-NAME}}: /* inner-join */
                    FIND NEXT {&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} NO-LOCK 
                        WHERE {&WHERE-FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} NO-ERROR.

                    FIND FIRST {&SECOND-TABLE-IN-QUERY-{&QUERY-NAME}} NO-LOCK 
                        WHERE {&JOIN-SECOND-TABLE-IN-QUERY-{&QUERY-NAME}} AND
                              {&WHERE-SECOND-TABLE-IN-QUERY-{&QUERY-NAME}}
                        NO-ERROR.
                END.

                IF NOT AVAILABLE {&SECOND-TABLE-IN-QUERY-{&QUERY-NAME}} THEN
                    RELEASE {&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} NO-ERROR. /* inner-join obriga existência da primeira e segunda tabela */
            &ELSE
/* tech1139 - 27/07/2005 - FO 1186.555 */
                FIND NEXT {&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} NO-LOCK
                &IF "{&WHERE-FIRST-TABLE-IN-QUERY-{&QUERY-NAME}}":U NE "":U &THEN
                    WHERE {&WHERE-FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} NO-ERROR.
                &ELSE
                    NO-ERROR.
                &ENDIF
/* tech1139 - 27/07/2005 - FO 1186.555 */                
            &ENDIF
        END.
        ELSE DO:
            &IF DEFINED(SECOND-TABLE-IN-QUERY-{&QUERY-NAME}) NE 0 &THEN
                FIND FIRST {&SECOND-TABLE-IN-QUERY-{&QUERY-NAME}} NO-LOCK 
                    WHERE {&JOIN-SECOND-TABLE-IN-QUERY-{&QUERY-NAME}} AND
                          {&WHERE-SECOND-TABLE-IN-QUERY-{&QUERY-NAME}}
                    NO-ERROR.

                DO WHILE AVAILABLE {&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} AND
                         NOT AVAILABLE {&SECOND-TABLE-IN-QUERY-{&QUERY-NAME}}: /* inner-join */
                    FIND NEXT {&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} NO-LOCK 
                        WHERE {&WHERE-FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} NO-ERROR.

                    FIND FIRST {&SECOND-TABLE-IN-QUERY-{&QUERY-NAME}} NO-LOCK 
                        WHERE {&JOIN-SECOND-TABLE-IN-QUERY-{&QUERY-NAME}} AND
                              {&WHERE-SECOND-TABLE-IN-QUERY-{&QUERY-NAME}}
                        NO-ERROR.
                END.

                IF NOT AVAILABLE {&SECOND-TABLE-IN-QUERY-{&QUERY-NAME}} THEN
                    RELEASE {&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} NO-ERROR. /* inner-join obriga existência da primeira e segunda tabela */
            &ENDIF
        END.

        ASSIGN tt-uib-reposition-find = FALSE.
    &ELSE
        GET NEXT {&QUERY-NAME}.
    &ENDIF
/***********************************************/

/************ Customizacao Datasul S.A. - John Cleber Jaraceski - 29/10/1999
              Ao utilizar Dataserver Oracle OU SQL SERVER, deve-se verificar se o pr¢ximo registro
              posicionado pela query est  realmente dispon¡vel
              **********************************/
    &IF "{&ems_dbtype}":U = "ORACLE":U &THEN
        DEFINE VARIABLE rCurrentQueryRowid AS ROWID NO-UNDO.

        IF AVAIL({&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}}) THEN 
            ASSIGN rCurrentQueryRowid = ROWID({&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}}).
        ELSE
            ASSIGN rCurrentQueryRowid = ?.

        IF AVAIL({&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}}) AND         
           CAN-FIND(FIRST {&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} WHERE
                        ROWID({&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}}) = rCurrentQueryRowid) THEN DO:
    &ELSE
        IF AVAIL({&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}}) THEN
        DO:    
    &ENDIF
/***********************************************/

/************ Customizacao Datasul S.A. - John Cleber Jaraceski - 29/10/1999
              Implementar novas caracteristicas para o panel p-navega.w 
              devido a deficiencia dos Templates PROGRESS com DATABASE DIFERENTE DE PROGRESS
              **********************************/
        &IF "{&ems_dbtype}":U = "ORACLE":U &THEN
            IF tt-uib-reposition-query THEN
                ASSIGN tt-uib-reposition-query = NO
                       /*Alterado 02/10/2006 - tech1007 - Alterado para habilitar os botoes de navegacao de forma correta quando for banco Oracle*/
                       &IF PROVERSION < "9.1C":U AND INTEGER(ENTRY(1,PROVERSION,".")) <= 9 &THEN
                       adm-first-rowid         = ROWID({&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}})
                       &ENDIF
                       /*Fim Alteracao 02/10/2006*/
                .
        &ENDIF
/***********************************************/

        IF adm-last-rowid = ROWID({&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}})
             THEN RUN new-state ('last-record,SELF':U).
        ELSE RUN new-state ('not-first-or-last,SELF':U).

        RUN dispatch IN THIS-PROCEDURE ('row-changed':U).
    END.
    ELSE 
        RUN dispatch IN THIS-PROCEDURE ('get-last':U).  

  &ELSEIF DEFINED(FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}) NE 0 &THEN
    DEFINE VARIABLE browser-handle AS HANDLE  NO-UNDO.
    DEFINE VARIABLE q-stat         AS LOGICAL NO-UNDO.

    IF NOT adm-query-opened THEN RETURN.
    DO WITH FRAME {&FRAME-NAME}:
      IF num-results("{&BROWSE-NAME}":U) = 0 THEN  /* Browse is empty */
        RUN new-state ('no-record-available,SELF':U).
      ELSE DO:
        browser-handle = {&BROWSE-NAME}:HANDLE.
        q-stat = browser-handle:SELECT-NEXT-ROW(). 
        IF adm-last-rowid = ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}})
             THEN RUN new-state ('last-record,SELF':U).
        ELSE RUN new-state ('not-first-or-last,SELF':U).
        RUN dispatch IN THIS-PROCEDURE ('row-changed':U).
      END.
    END.
  &ENDIF
    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-adm-get-prev) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-get-prev Method-Library 
PROCEDURE adm-get-prev :
/* -----------------------------------------------------------
  Purpose:     Gets the previous record in the default query.
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/

  &IF DEFINED(FIRST-TABLE-IN-QUERY-{&QUERY-NAME}) NE 0 &THEN
    IF NOT adm-query-opened THEN RETURN.

/************ Customizacao Datasul S.A. - John Cleber Jaraceski
              Ao utilizar Dataserver Oracle ou SQL-Server, deve-se trocar o uso de QUERY por FIND
              **********************************/
    &IF "{&CHANGE-QUERY-TO-FIND}":U EQ "TRUE":U &THEN
        &IF DEFINED(SECOND-TABLE-IN-QUERY-{&QUERY-NAME}) NE 0 &THEN
            FIND PREV {&SECOND-TABLE-IN-QUERY-{&QUERY-NAME}} NO-LOCK 
                WHERE {&JOIN-SECOND-TABLE-IN-QUERY-{&QUERY-NAME}} AND
                      {&WHERE-SECOND-TABLE-IN-QUERY-{&QUERY-NAME}}
                NO-ERROR.

            DO WHILE AVAILABLE {&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} AND
                     NOT AVAILABLE {&SECOND-TABLE-IN-QUERY-{&QUERY-NAME}}: /* inner-join */
                FIND PREV {&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} NO-LOCK 
                    WHERE {&WHERE-FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} NO-ERROR.

                FIND LAST {&SECOND-TABLE-IN-QUERY-{&QUERY-NAME}} NO-LOCK 
                    WHERE {&JOIN-SECOND-TABLE-IN-QUERY-{&QUERY-NAME}} AND
                          {&WHERE-SECOND-TABLE-IN-QUERY-{&QUERY-NAME}}
                    NO-ERROR.
            END.

            IF NOT AVAILABLE {&SECOND-TABLE-IN-QUERY-{&QUERY-NAME}} THEN
                RELEASE {&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} NO-ERROR. /* inner-join obriga existência da primeira e segunda tabela */
        &ELSE
/* tech1139 - 27/07/2005 - FO 1186.555 */
            FIND PREV {&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} NO-LOCK
            &IF "{&WHERE-FIRST-TABLE-IN-QUERY-{&QUERY-NAME}}":U NE "":U &THEN
                WHERE {&WHERE-FIRST-TABLE-IN-QUERY-{&QUERY-NAME}} NO-ERROR.
            &ELSE
                NO-ERROR.
            &ENDIF
/* tech1139 - 27/07/2005 - FO 1186.555 */
        &ENDIF    
    &ELSE
        GET PREV {&QUERY-NAME}. 
    &ENDIF
/***********************************************/

    IF AVAIL({&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}}) THEN
    DO:
        IF adm-first-rowid = ROWID({&FIRST-TABLE-IN-QUERY-{&QUERY-NAME}})
            THEN RUN new-state ('first-record,SELF':U).
        ELSE RUN new-state ('not-first-or-last,SELF':U).
        RUN dispatch IN THIS-PROCEDURE ('row-changed':U).
    END.
    ELSE 
        RUN dispatch IN THIS-PROCEDURE ('get-first':U).

  &ELSEIF DEFINED(FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}) NE 0 &THEN
    DEFINE VARIABLE browser-handle AS HANDLE  NO-UNDO.
    DEFINE VARIABLE q-stat         AS LOGICAL NO-UNDO.

    IF NOT adm-query-opened THEN RETURN.
    DO WITH FRAME {&FRAME-NAME}:
      IF num-results("{&BROWSE-NAME}":U) = 0 THEN  /* Browse is empty */
        RUN new-state ('no-record-available,SELF':U).
      ELSE DO:
        browser-handle = {&BROWSE-NAME}:HANDLE.
        q-stat = browser-handle:SELECT-PREV-ROW(). 
        IF adm-first-rowid = ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}})
             THEN RUN new-state ('first-record,SELF':U).
        ELSE RUN new-state ('not-first-or-last,SELF':U).
        RUN dispatch IN THIS-PROCEDURE ('row-changed':U).
      END.
    END.
  &ENDIF
    RETURN.

END PROCEDURE.

&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-New-First-Record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE New-First-Record Method-Library 
PROCEDURE New-First-Record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER rRowid AS ROWID NO-UNDO.

    ASSIGN adm-first-rowid = rRowid.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

