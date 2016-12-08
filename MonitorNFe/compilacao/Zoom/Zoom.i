&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
/* Procedure Description
"Method Library principal para Zoom Template, que contÇm definiá‰es e chamadas a outras Method Libraries."
*/
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Method-Library 
/*--------------------------------------------------------------------------
    Library    : zoom/Zoom.i
    Purpose    : Method Library principal para Zoom Template, que contÇm 
                 definiá‰es e chamadas a outras Method Libraries 

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* Global Variable Definitions ---                                        */
DEFINE NEW GLOBAL SHARED VARIABLE hWindowStyles AS HANDLE NO-UNDO.

&IF "{&DBOTable1}":U <> "":U &THEN
    &GLOBAL-DEFINE rTable1 gr-{&DBOTable1}
    
    DEFINE NEW GLOBAL SHARED VARIABLE gr-{&DBOTable1} AS ROWID NO-UNDO.
&ENDIF

/* Global Variable Definitions ---                                        */
&IF "{&DBOTable2}":U <> "":U &THEN
    &GLOBAL-DEFINE rTable2 gr-{&DBOTable2}
    
    DEFINE NEW GLOBAL SHARED VARIABLE gr-{&DBOTable2} AS ROWID NO-UNDO.
&ENDIF

/* Global Variable Definitions ---                                        */
&IF "{&DBOTable3}":U <> "":U &THEN
    &GLOBAL-DEFINE rTable3 gr-{&DBOTable3}
    
    DEFINE NEW GLOBAL SHARED VARIABLE gr-{&DBOTable3} AS ROWID NO-UNDO.
&ENDIF

/* Global Variable Definitions ---                                        */
&IF "{&DBOTable4}":U <> "":U &THEN
    &GLOBAL-DEFINE rTable4 gr-{&DBOTable4}
    
    DEFINE NEW GLOBAL SHARED VARIABLE gr-{&DBOTable4} AS ROWID NO-UNDO.
&ENDIF

/* Global Variable Definitions ---                                        */
&IF "{&DBOTable5}":U <> "":U &THEN
    &GLOBAL-DEFINE rTable5 gr-{&DBOTable5}
    
    DEFINE NEW GLOBAL SHARED VARIABLE gr-{&DBOTable5} AS ROWID NO-UNDO.
&ENDIF

/* Global Variable Definitions ---                                        */
&IF "{&DBOTable6}":U <> "":U &THEN
    &GLOBAL-DEFINE rTable6 gr-{&DBOTable6}
    
    DEFINE NEW GLOBAL SHARED VARIABLE gr-{&DBOTable6} AS ROWID NO-UNDO.
&ENDIF

/* Global Variable Definitions ---                                        */
&IF "{&DBOTable7}":U <> "":U &THEN
    &GLOBAL-DEFINE rTable7 gr-{&DBOTable7}
    
    DEFINE NEW GLOBAL SHARED VARIABLE gr-{&DBOTable7} AS ROWID NO-UNDO.
&ENDIF

/* Global Variable Definitions ---                                        */
&IF "{&DBOTable8}":U <> "":U &THEN
    &GLOBAL-DEFINE rTable8 gr-{&DBOTable8}
    
    DEFINE NEW GLOBAL SHARED VARIABLE gr-{&DBOTable8} AS ROWID NO-UNDO.
&ENDIF

/* Definir utilizaÁ„o de folder */
&IF DEFINED(folder) = 0 &THEN
    &GLOBAL-DEFINE Folder      YES
&ENDIF

/* Local Temp-Table Definitions ---                                       */
/*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
&IF DEFINED(DBOVersion) <> 0 &THEN
    DEFINE TEMP-TABLE RowErrors NO-UNDO
        FIELD ErrorSequence    AS INTEGER
        FIELD ErrorNumber      AS INTEGER
        FIELD ErrorDescription AS CHARACTER
        FIELD ErrorParameters  AS CHARACTER
        FIELD ErrorType        AS CHARACTER
        FIELD ErrorHelp        AS CHARACTER
        FIELD ErrorSubType     AS CHARACTER.
&ELSE
    {method/dbotterr.i}
&ENDIF

/*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
/*&IF DEFINED(DBOVersion) <> 0 &THEN*/
    &IF "{&page1browse}":U <> "":U &THEN
        DEFINE TEMP-TABLE ttTable1Aux NO-UNDO LIKE {&ttTable1}.
    &ENDIF
    
    &IF "{&page2browse}":U <> "":U &THEN
        DEFINE TEMP-TABLE ttTable2Aux NO-UNDO LIKE {&ttTable2}.
    &ENDIF
    
    &IF "{&page3browse}":U <> "":U &THEN
        DEFINE TEMP-TABLE ttTable3Aux NO-UNDO LIKE {&ttTable3}.
    &ENDIF
    
    &IF "{&page4browse}":U <> "":U &THEN
        DEFINE TEMP-TABLE ttTable4Aux NO-UNDO LIKE {&ttTable4}.
    &ENDIF
    
    &IF "{&page5browse}":U <> "":U &THEN
        DEFINE TEMP-TABLE ttTable5Aux NO-UNDO LIKE {&ttTable5}.
    &ENDIF
    
    &IF "{&page6browse}":U <> "":U &THEN
        DEFINE TEMP-TABLE ttTable6Aux NO-UNDO LIKE {&ttTable6}.
    &ENDIF
    
    &IF "{&page7browse}":U <> "":U &THEN
        DEFINE TEMP-TABLE ttTable7Aux NO-UNDO LIKE {&ttTable7}.
    &ENDIF
    
    &IF "{&page8browse}":U <> "":U &THEN
        DEFINE TEMP-TABLE ttTable8Aux NO-UNDO LIKE {&ttTable8}.
    &ENDIF
/*&ELSE*/
    {method/dbottraw.i}
/*&ENDIF*/

/* Local Variable Definitions ---                                         */
DEFINE VARIABLE cEventBrowse          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldNames           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldHandles         AS CHARACTER NO-UNDO.
DEFINE VARIABLE hFolder               AS HANDLE    NO-UNDO.
DEFINE VARIABLE hProgramImplant       AS HANDLE    NO-UNDO.
DEFINE VARIABLE hRange                AS HANDLE    NO-UNDO.
DEFINE VARIABLE hShowMsg              AS HANDLE    NO-UNDO.
DEFINE VARIABLE hWindowParent         AS HANDLE    NO-UNDO.
DEFINE VARIABLE iConstraintPageNumber AS INTEGER   NO-UNDO.
DEFINE VARIABLE iRepositionPageNumber AS INTEGER   NO-UNDO.
DEFINE VARIABLE iRowsReturned         AS INTEGER   NO-UNDO.
DEFINE VARIABLE lCustomExecuted       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lOverrideExecuted     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE rRepositionTable      AS ROWID     NO-UNDO.
DEFINE VARIABLE hFirstField           AS handle     NO-UNDO.


DEFINE VARIABLE c-nom-prog-dpc-mg97  AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-nom-prog-appc-mg97 AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-nom-prog-upc-mg97  AS CHARACTER NO-UNDO.

DEFINE VARIABLE epc-rowid1           AS ROWID     NO-UNDO.
DEFINE VARIABLE epc-rowid2           AS ROWID     NO-UNDO.
DEFINE VARIABLE epc-rowid3           AS ROWID     NO-UNDO.
DEFINE VARIABLE epc-rowid4           AS ROWID     NO-UNDO.
DEFINE VARIABLE epc-rowid5           AS ROWID     NO-UNDO.
DEFINE VARIABLE epc-rowid6           AS ROWID     NO-UNDO.
DEFINE VARIABLE epc-rowid7           AS ROWID     NO-UNDO.
DEFINE VARIABLE epc-rowid8           AS ROWID     NO-UNDO.

&IF "{&mguni_version}" >= "2.06b" OR "{&aplica_facelift}" = "YES" &THEN
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
    define new global shared variable h-facelift as handle no-undo.
    if not valid-handle(h-facelift) then run btb/btb901zo.p persistent set h-facelift.
&ENDIF

/*--- Vari†veis utilizadas nos eventos de CURSOR-DOWN, END, OFF-END e PAGE-DOWN ---*/
&IF "{&page1browse}":U <> "":U &THEN
    DEFINE VARIABLE rCurrent1 AS ROWID NO-UNDO.
&ENDIF

&IF "{&page2browse}":U <> "":U &THEN
    DEFINE VARIABLE rCurrent2 AS ROWID NO-UNDO.
&ENDIF

&IF "{&page3browse}":U <> "":U &THEN
    DEFINE VARIABLE rCurrent3 AS ROWID NO-UNDO.
&ENDIF

&IF "{&page4browse}":U <> "":U &THEN
    DEFINE VARIABLE rCurrent4 AS ROWID NO-UNDO.
&ENDIF

&IF "{&page5browse}":U <> "":U &THEN
    DEFINE VARIABLE rCurrent5 AS ROWID NO-UNDO.
&ENDIF

&IF "{&page6browse}":U <> "":U &THEN
    DEFINE VARIABLE rCurrent6 AS ROWID NO-UNDO.
&ENDIF

&IF "{&page7browse}":U <> "":U &THEN
    DEFINE VARIABLE rCurrent7 AS ROWID NO-UNDO.
&ENDIF

&IF "{&page8browse}":U <> "":U &THEN
    DEFINE VARIABLE rCurrent8 AS ROWID NO-UNDO.
&ENDIF

/* Function Definitions ---                                               */
&IF "{&Range}":U = "YES":U &THEN
    FUNCTION fnIniRangeChar RETURN CHAR    (INPUT pPageNumber AS INTEGER) IN hRange.
    FUNCTION fnEndRangeChar RETURN CHAR    (INPUT pPageNumber AS INTEGER) IN hRange.
    FUNCTION fnIniRangeInt  RETURN INTEGER (INPUT pPageNumber AS INTEGER) IN hRange.
    FUNCTION fnEndRangeInt  RETURN INTEGER (INPUT pPageNumber AS INTEGER) IN hRange.
    FUNCTION fnIniRangeDate RETURN DATE    (INPUT pPageNumber AS INTEGER) IN hRange.
    FUNCTION fnEndRangeDate RETURN DATE    (INPUT pPageNumber AS INTEGER) IN hRange.
    FUNCTION fnIniRangeDec  RETURN DECIMAL (INPUT pPageNumber AS INTEGER) IN hRange.
    FUNCTION fnEndRangeDec  RETURN DECIMAL (INPUT pPageNumber AS INTEGER) IN hRange.

    FUNCTION fnIniRangeCharPage RETURN CHAR    (INPUT pPageNumber AS INTEGER, INPUT pFieldNum AS INTEGER) IN hRange.
    FUNCTION fnEndRangeCharPage RETURN CHAR    (INPUT pPageNumber AS INTEGER, INPUT pFieldNum AS INTEGER) IN hRange.
    FUNCTION fnIniRangeIntPage  RETURN INTEGER (INPUT pPageNumber AS INTEGER, INPUT pFieldNum AS INTEGER) IN hRange.
    FUNCTION fnEndRangeIntPage  RETURN INTEGER (INPUT pPageNumber AS INTEGER, INPUT pFieldNum AS INTEGER) IN hRange.
    FUNCTION fnIniRangeDatePage RETURN DATE    (INPUT pPageNumber AS INTEGER, INPUT pFieldNum AS INTEGER) IN hRange.
    FUNCTION fnEndRangeDatePage RETURN DATE    (INPUT pPageNumber AS INTEGER, INPUT pFieldNum AS INTEGER) IN hRange.
    FUNCTION fnIniRangeDecPage  RETURN DECIMAL (INPUT pPageNumber AS INTEGER, INPUT pFieldNum AS INTEGER) IN hRange.
    FUNCTION fnEndRangeDecPage  RETURN DECIMAL (INPUT pPageNumber AS INTEGER, INPUT pFieldNum AS INTEGER) IN hRange.

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
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Method-Library 
/* ************************* Included-Libraries *********************** */

{utp/ut-glob.i}
{include/i-sysvar.i}
{btb/btb008za.i0}
{zoom/buttons.i}
/*** Alterado por Farley - em 23/07/2003 ***/
{include/i_fnctrad.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Method-Library 


/* ***************************  Main Block  *************************** */

&IF "{&mguni_version}" >= "2.06b" OR "{&aplica_facelift}" = "YES" &THEN
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
run pi_aplica_facelift in this-procedure.
&ENDIF

/*Alterado por Valdir (tech264) para fazer tratamento da trigger de help*/
ON HELP OF wZoom DO:
    apply "choose":U to btHelp in frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

PROCEDURE GetSystemTime EXTERNAL "KERNEL32.DLL":U PERSISTENT:
    DEFINE OUTPUT PARAMETER lpSystemTime AS MEMPTR NO-UNDO.
END.

&IF DEFINED(EXCLUDE-applyCursorDown) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE applyCursorDown Method-Library 
PROCEDURE applyCursorDown :
/*------------------------------------------------------------------------------
  Purpose:     L¢gica para trigger de CURSOR-DOWN
  Parameters:  recebe o n£mero da p†gina
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pPageNumber AS INTEGER NO-UNDO.
    
    /*--- Seta vari†vel cEventBrowse com o valor CURSOR-DOWN ---*/
    ASSIGN cEventBrowse = "CURSOR-DOWN":U.
    
    CASE pPageNumber:
        &IF "{&page1browse}":U <> "":U &THEN
            WHEN 1 THEN
                /*--- Seta vari†vel rCurrent1 com o valor do rowid corrente ---*/
                ASSIGN rCurrent1 = IF AVAILABLE {&ttTable1}
                                       THEN ROWID({&ttTable1})
                                       ELSE ?.
        &ENDIF
        
        &IF "{&page2browse}":U <> "":U &THEN
            WHEN 2 THEN
                /*--- Seta vari†vel rCurrent2 com o valor do rowid corrente ---*/
                ASSIGN rCurrent2 = IF AVAILABLE {&ttTable2}
                                       THEN ROWID({&ttTable2})
                                       ELSE ?.
        &ENDIF
        
        &IF "{&page3browse}":U <> "":U &THEN
            WHEN 3 THEN
                /*--- Seta vari†vel rCurrent3 com o valor do rowid corrente ---*/
                ASSIGN rCurrent3 = IF AVAILABLE {&ttTable3}
                                       THEN ROWID({&ttTable3})
                                       ELSE ?.
        &ENDIF
        
        &IF "{&page4browse}":U <> "":U &THEN
            WHEN 4 THEN
                /*--- Seta vari†vel rCurrent4 com o valor do rowid corrente ---*/
                ASSIGN rCurrent4 = IF AVAILABLE {&ttTable4}
                                       THEN ROWID({&ttTable4})
                                       ELSE ?.
        &ENDIF
        
        &IF "{&page5browse}":U <> "":U &THEN
            WHEN 5 THEN
                /*--- Seta vari†vel rCurrent5 com o valor do rowid corrente ---*/
                ASSIGN rCurrent5 = IF AVAILABLE {&ttTable5}
                                       THEN ROWID({&ttTable5})
                                       ELSE ?.
        &ENDIF
        
        &IF "{&page6browse}":U <> "":U &THEN
            WHEN 6 THEN
                /*--- Seta vari†vel rCurrent6 com o valor do rowid corrente ---*/
                ASSIGN rCurrent6 = IF AVAILABLE {&ttTable6}
                                       THEN ROWID({&ttTable6})
                                       ELSE ?.
        &ENDIF
        
        &IF "{&page7browse}":U <> "":U &THEN
            WHEN 7 THEN
                /*--- Seta vari†vel rCurrent7 com o valor do rowid corrente ---*/
                ASSIGN rCurrent7 = IF AVAILABLE {&ttTable7}
                                       THEN ROWID({&ttTable7})
                                       ELSE ?.
        &ENDIF
        
        &IF "{&page8browse}":U <> "":U &THEN
            WHEN 8 THEN
                /*--- Seta vari†vel rCurrent8 com o valor do rowid corrente ---*/
                ASSIGN rCurrent8 = IF AVAILABLE {&ttTable8}
                                       THEN ROWID({&ttTable8})
                                       ELSE ?.
        &ENDIF
    END CASE.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-applyCursorUp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE applyCursorUp Method-Library 
PROCEDURE applyCursorUp :
/*------------------------------------------------------------------------------
  Purpose:     L¢gica para trigger de CURSOR-UP
  Parameters:  recebe o n£mero da p†gina
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pPageNumber AS INTEGER NO-UNDO.
    
    /*--- Seta vari†vel cEventBrowse com o valor CURSOR-UP ---*/
    ASSIGN cEventBrowse = "CURSOR-UP":U.
    
    CASE pPageNumber:
        &IF "{&page1browse}":U <> "":U &THEN
            WHEN 1 THEN
                /*--- Seta vari†vel rCurrent1 com o valor do rowid corrente ---*/
                ASSIGN rCurrent1 = IF AVAILABLE {&ttTable1}
                                       THEN ROWID({&ttTable1})
                                       ELSE ?.
        &ENDIF
        
        &IF "{&page2browse}":U <> "":U &THEN
            WHEN 2 THEN
                /*--- Seta vari†vel rCurrent2 com o valor do rowid corrente ---*/
                ASSIGN rCurrent2 = IF AVAILABLE {&ttTable2}
                                       THEN ROWID({&ttTable2})
                                       ELSE ?.
        &ENDIF
        
        &IF "{&page3browse}":U <> "":U &THEN
            WHEN 3 THEN
                /*--- Seta vari†vel rCurrent3 com o valor do rowid corrente ---*/
                ASSIGN rCurrent3 = IF AVAILABLE {&ttTable3}
                                       THEN ROWID({&ttTable3})
                                       ELSE ?.
        &ENDIF
        
        &IF "{&page4browse}":U <> "":U &THEN
            WHEN 4 THEN
                /*--- Seta vari†vel rCurrent4 com o valor do rowid corrente ---*/
                ASSIGN rCurrent4 = IF AVAILABLE {&ttTable4}
                                       THEN ROWID({&ttTable4})
                                       ELSE ?.
        &ENDIF
        
        &IF "{&page5browse}":U <> "":U &THEN
            WHEN 5 THEN
                /*--- Seta vari†vel rCurrent5 com o valor do rowid corrente ---*/
                ASSIGN rCurrent5 = IF AVAILABLE {&ttTable5}
                                       THEN ROWID({&ttTable5})
                                       ELSE ?.
        &ENDIF
        
        &IF "{&page6browse}":U <> "":U &THEN
            WHEN 6 THEN
                /*--- Seta vari†vel rCurrent6 com o valor do rowid corrente ---*/
                ASSIGN rCurrent6 = IF AVAILABLE {&ttTable6}
                                       THEN ROWID({&ttTable6})
                                       ELSE ?.
        &ENDIF
        
        &IF "{&page7browse}":U <> "":U &THEN
            WHEN 7 THEN
                /*--- Seta vari†vel rCurrent7 com o valor do rowid corrente ---*/
                ASSIGN rCurrent7 = IF AVAILABLE {&ttTable7}
                                       THEN ROWID({&ttTable7})
                                       ELSE ?.
        &ENDIF
        
        &IF "{&page8browse}":U <> "":U &THEN
            WHEN 8 THEN
                /*--- Seta vari†vel rCurrent8 com o valor do rowid corrente ---*/
                ASSIGN rCurrent8 = IF AVAILABLE {&ttTable8}
                                       THEN ROWID({&ttTable8})
                                       ELSE ?.
        &ENDIF
    END CASE.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-applyValueChanged) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE applyValueChanged Method-Library 
PROCEDURE applyValueChanged :
/*------------------------------------------------------------------------------
  Purpose:     L¢gica para trigger de VALUE-CHANGED
  Parameters:  recebe o n£mero da p†gina
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pPageNumber AS INTEGER NO-UNDO.
    
    CASE pPageNumber:
        &IF "{&page1browse}":U <> "":U &THEN
            WHEN 1 THEN DO:

                IF AVAILABLE {&ttTable1} THEN 
                    ASSIGN epc-rowid1 = {&ttTable1}.r-Rowid.
                ELSE 
                    ASSIGN epc-rowid1 = ?.

                /*--- Executa programas de customizaá∆o (before/after) ---*/
                {method/custom.i &Event="BEFORE-VALUE-CHANGED"
                                 &Object="CONTAINER"
                                 &ObjectHandle="THIS-PROCEDURE:HANDLE"
                                 &FrameHandle="FRAME fPage0:HANDLE"
                                 &Table="{&DBOTable1}"
                                 &RowidTable="epc-rowid1"}
                
                /*--- Executa programas de customizaá∆o (before/after) ---*/
                {method/custom.i &Event="AFTER-VALUE-CHANGED"
                                 &Object="CONTAINER"
                                 &ObjectHandle="THIS-PROCEDURE:HANDLE"
                                 &FrameHandle="FRAME fPage0:HANDLE"
                                 &Table="{&DBOTable1}"
                                 &RowidTable="epc-rowid1"}
            END.
        &ENDIF
        
        &IF "{&page2browse}":U <> "":U &THEN
            WHEN 2 THEN DO:

                IF AVAILABLE {&ttTable2} THEN 
                    ASSIGN epc-rowid2 = {&ttTable2}.r-Rowid.
                ELSE 
                    ASSIGN epc-rowid2 = ?.

                /*--- Executa programas de customizaá∆o (before/after) ---*/
                {method/custom.i &Event="BEFORE-VALUE-CHANGED"
                                 &Object="CONTAINER"
                                 &ObjectHandle="THIS-PROCEDURE:HANDLE"
                                 &FrameHandle="FRAME fPage0:HANDLE"
                                 &Table="{&DBOTable2}"
                                 &RowidTable="epc-rowid2"}
                
                /*--- Executa programas de customizaá∆o (before/after) ---*/
                {method/custom.i &Event="AFTER-VALUE-CHANGED"
                                 &Object="CONTAINER"
                                 &ObjectHandle="THIS-PROCEDURE:HANDLE"
                                 &FrameHandle="FRAME fPage0:HANDLE"
                                 &Table="{&DBOTable2}"
                                 &RowidTable="epc-rowid2"}
            END.
        &ENDIF
        
        &IF "{&page3browse}":U <> "":U &THEN
            WHEN 3 THEN DO:

                IF AVAILABLE {&ttTable3} THEN 
                    ASSIGN epc-rowid3 = {&ttTable3}.r-Rowid.
                ELSE 
                    ASSIGN epc-rowid3 = ?.

                /*--- Executa programas de customizaá∆o (before/after) ---*/
                {method/custom.i &Event="BEFORE-VALUE-CHANGED"
                                 &Object="CONTAINER"
                                 &ObjectHandle="THIS-PROCEDURE:HANDLE"
                                 &FrameHandle="FRAME fPage0:HANDLE"
                                 &Table="{&DBOTable3}"
                                 &RowidTable="epc-rowid3"}
                
                /*--- Executa programas de customizaá∆o (before/after) ---*/
                {method/custom.i &Event="AFTER-VALUE-CHANGED"
                                 &Object="CONTAINER"
                                 &ObjectHandle="THIS-PROCEDURE:HANDLE"
                                 &FrameHandle="FRAME fPage0:HANDLE"
                                 &Table="{&DBOTable3}"
                                 &RowidTable="epc-rowid3"}
            END.
        &ENDIF
        
        &IF "{&page4browse}":U <> "":U &THEN
            WHEN 4 THEN DO:

                IF AVAILABLE {&ttTable4} THEN 
                    ASSIGN epc-rowid4 = {&ttTable4}.r-Rowid.
                ELSE 
                    ASSIGN epc-rowid4 = ?.

                /*--- Executa programas de customizaá∆o (before/after) ---*/
                {method/custom.i &Event="BEFORE-VALUE-CHANGED"
                                 &Object="CONTAINER"
                                 &ObjectHandle="THIS-PROCEDURE:HANDLE"
                                 &FrameHandle="FRAME fPage0:HANDLE"
                                 &Table="{&DBOTable4}"
                                 &RowidTable="epc-rowid4"}
                
                /*--- Executa programas de customizaá∆o (before/after) ---*/
                {method/custom.i &Event="AFTER-VALUE-CHANGED"
                                 &Object="CONTAINER"
                                 &ObjectHandle="THIS-PROCEDURE:HANDLE"
                                 &FrameHandle="FRAME fPage0:HANDLE"
                                 &Table="{&DBOTable4}"
                                 &RowidTable="epc-rowid4"}
            END.
        &ENDIF
        
        &IF "{&page5browse}":U <> "":U &THEN
            WHEN 5 THEN DO:

                IF AVAILABLE {&ttTable5} THEN 
                    ASSIGN epc-rowid5 = {&ttTable5}.r-Rowid.
                ELSE 
                    ASSIGN epc-rowid5 = ?.

                /*--- Executa programas de customizaá∆o (before/after) ---*/
                {method/custom.i &Event="BEFORE-VALUE-CHANGED"
                                 &Object="CONTAINER"
                                 &ObjectHandle="THIS-PROCEDURE:HANDLE"
                                 &FrameHandle="FRAME fPage0:HANDLE"
                                 &Table="{&DBOTable5}"
                                 &RowidTable="epc-rowid5"}
                
                /*--- Executa programas de customizaá∆o (before/after) ---*/
                {method/custom.i &Event="AFTER-VALUE-CHANGED"
                                 &Object="CONTAINER"
                                 &ObjectHandle="THIS-PROCEDURE:HANDLE"
                                 &FrameHandle="FRAME fPage0:HANDLE"
                                 &Table="{&DBOTable5}"
                                 &RowidTable="epc-rowid5"}
            END.
        &ENDIF
        
        &IF "{&page6browse}":U <> "":U &THEN
            WHEN 6 THEN DO:

                IF AVAILABLE {&ttTable6} THEN 
                    ASSIGN epc-rowid6 = {&ttTable6}.r-Rowid.
                ELSE 
                    ASSIGN epc-rowid6 = ?.

                /*--- Executa programas de customizaá∆o (before/after) ---*/
                {method/custom.i &Event="BEFORE-VALUE-CHANGED"
                                 &Object="CONTAINER"
                                 &ObjectHandle="THIS-PROCEDURE:HANDLE"
                                 &FrameHandle="FRAME fPage0:HANDLE"
                                 &Table="{&DBOTable6}"
                                 &RowidTable="epc-rowid6"}
                
                /*--- Executa programas de customizaá∆o (before/after) ---*/
                {method/custom.i &Event="AFTER-VALUE-CHANGED"
                                 &Object="CONTAINER"
                                 &ObjectHandle="THIS-PROCEDURE:HANDLE"
                                 &FrameHandle="FRAME fPage0:HANDLE"
                                 &Table="{&DBOTable6}"
                                 &RowidTable="epc-rowid6"}
            END.
        &ENDIF
        
        &IF "{&page7browse}":U <> "":U &THEN
            WHEN 7 THEN DO:

                IF AVAILABLE {&ttTable7} THEN 
                    ASSIGN epc-rowid7 = {&ttTable7}.r-Rowid.
                ELSE 
                    ASSIGN epc-rowid7 = ?.

                /*--- Executa programas de customizaá∆o (before/after) ---*/
                {method/custom.i &Event="BEFORE-VALUE-CHANGED"
                                 &Object="CONTAINER"
                                 &ObjectHandle="THIS-PROCEDURE:HANDLE"
                                 &FrameHandle="FRAME fPage0:HANDLE"
                                 &Table="{&DBOTable7}"
                                 &RowidTable="epc-rowid7"}
                
                /*--- Executa programas de customizaá∆o (before/after) ---*/
                {method/custom.i &Event="AFTER-VALUE-CHANGED"
                                 &Object="CONTAINER"
                                 &ObjectHandle="THIS-PROCEDURE:HANDLE"
                                 &FrameHandle="FRAME fPage0:HANDLE"
                                 &Table="{&DBOTable7}"
                                 &RowidTable="epc-rowid7"}
            END.
        &ENDIF
        
        &IF "{&page8browse}":U <> "":U &THEN
            WHEN 8 THEN DO:

                IF AVAILABLE {&ttTable8} THEN 
                    ASSIGN epc-rowid8 = {&ttTable8}.r-Rowid.
                ELSE 
                    ASSIGN epc-rowid8 = ?.

                /*--- Executa programas de customizaá∆o (before/after) ---*/
                {method/custom.i &Event="BEFORE-VALUE-CHANGED"
                                 &Object="CONTAINER"
                                 &ObjectHandle="THIS-PROCEDURE:HANDLE"
                                 &FrameHandle="FRAME fPage0:HANDLE"
                                 &Table="{&DBOTable8}"
                                 &RowidTable="epc-rowid8"}
                
                /*--- Executa programas de customizaá∆o (before/after) ---*/
                {method/custom.i &Event="AFTER-VALUE-CHANGED"
                                 &Object="CONTAINER"
                                 &ObjectHandle="THIS-PROCEDURE:HANDLE"
                                 &FrameHandle="FRAME fPage0:HANDLE"
                                 &Table="{&DBOTable8}"
                                 &RowidTable="epc-rowid8"}
            END.
        &ENDIF
    END CASE.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-applyDblClick) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE applyDblClick Method-Library 
PROCEDURE applyDblClick :
/*------------------------------------------------------------------------------
  Purpose:     L¢gica para trigger de DBL-CLICK
  Parameters:  recebe o n£mero da p†gina
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pPageNumber AS INTEGER NO-UNDO.
    
    /*nao foi possivel efetuar um apply choose to btok por  
     que o programa estava perdendo o foco qdo chave estrangeira*/
    
    RUN returnValues IN THIS-PROCEDURE.
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    
    RETURN RETURN-VALUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-applyEnd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE applyEnd Method-Library 
PROCEDURE applyEnd :
/*------------------------------------------------------------------------------
  Purpose:     L¢gica para trigger de END
  Parameters:  recebe o n£mero da p†gina
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pPageNumber AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE rLastCreated AS ROWID NO-UNDO.
    DEFINE VARIABLE rLast     AS ROWID NO-UNDO.
    
    /*--- Testa o pre-processador RowNumDefinedN para todas as p†ginas para
          definir iLastSeq que Ç utilizada no c†lculo do valor do rowNum    ---*/
    &IF  "{&ROW-NUM-DEFINED1}":U = "YES":U  or
         "{&ROW-NUM-DEFINED2}":U = "YES":U  or
         "{&ROW-NUM-DEFINED3}":U = "YES":U  or
         "{&ROW-NUM-DEFINED4}":U = "YES":U  or
         "{&ROW-NUM-DEFINED5}":U = "YES":U  or
         "{&ROW-NUM-DEFINED6}":U = "YES":U  or
         "{&ROW-NUM-DEFINED7}":U = "YES":U  or
         "{&ROW-NUM-DEFINED8}":U = "YES":U  &THEN
        DEFINE VARIABLE iLastSeq AS INTEGER NO-UNDO INIT 0.
    &ENDIF
    
    CASE pPageNumber:
        &IF "{&page1browse}":U <> "":U &THEN
            WHEN 1 THEN DO:
                /*--- Seta vari†vel rLast com o valor do £ltimo rowid da 
                      tabela {&ttTable1} existente no browse ---*/
                GET LAST {&page1browse} NO-LOCK.
                IF AVAILABLE {&ttTable1} THEN
                    ASSIGN rLast = {&ttTable1}.r-Rowid.
                ELSE
                    ASSIGN rLast = ?.
                
                /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                &IF DEFINED(DBOVersion) <> 0 &THEN
                    /*--- Retorna todos os registros do DBO filho, na temp-table ttTable1Aux ---*/
                    RUN serverSendRows IN {&hDBOTable1} (INPUT ?,
                                                       INPUT STRING(rLast),
                                                       INPUT IF rLast = ? THEN NO ELSE YES,
                                                       INPUT ?,
                                                       OUTPUT iRowsReturned,
                                                       OUTPUT TABLE ttTable1Aux).
                &ELSE
                    /*--- Retorna todos os registros do DBO filho, na temp-table ttTable1Aux ---*/
                    RUN getBatchRecords IN {&hDBOTable1} (INPUT rLast,
                                                        INPUT IF rLast = ? THEN NO ELSE YES,
                                                        INPUT ?,
                                                        OUTPUT iRowsReturned,
                                                        OUTPUT TABLE ttTable1Aux).
                &ENDIF
                
                /*--- Cancela trigger caso n∆o existam mais registros no DBO filho ---*/
                IF CAN-FIND(FIRST ttTable1Aux) THEN DO:
                    /*--- Transfere dados da temp-table ttTable1Aux para a temp-table {&ttTable1} ---*/
                    &IF  "{&ROW-NUM-DEFINED1}":U = "YES":U  &THEN
                        ASSIGN iLastSeq = 0.
                        FOR LAST {&ttTable1} BY {&ttTable1}.RowNum:
                            /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                            ASSIGN iLastSeq = {&ttTable1}.RowNum.
                        END.
                    &ENDIF
                    FOR EACH ttTable1Aux:
                        CREATE {&ttTable1}.

                        &IF  "{&ROW-NUM-DEFINED1}":U = "YES":U  &THEN
                            ASSIGN {&ttTable1}.RowNum = {&ttTable1}.RowNum + iLastSeq
                                           iLastSeq = {&ttTable1}.RowNum. 
                            /*--- Acrescentado incremento do iLastSeq para que na pr¢xima 
                                    iteraá∆o do For Each n∆o repita o registro. ---*/    
                            BUFFER-COPY ttTable1Aux except RowNum TO {&ttTable1}.
                        &ELSE
                            BUFFER-COPY ttTable1Aux TO {&ttTable1}.
                        &ENDIF
                        ASSIGN rLastCreated = ROWID({&ttTable1}).
                    END.
                
                    /*--- Abre o browse filho, para atualizaá∆o dos dados ---*/
                    {&OPEN-QUERY-{&page1browse}}
                    
                    /*--- Seta view-port do browse filho para reposicionamento ---*/
                    {&page1browse}:SET-REPOSITIONED-ROW({&page1browse}:DOWN IN FRAME fPage1) IN FRAME fPage1.
                    
                    /*--- Reposiciona browse filho no £ltimo registro ---*/
                    REPOSITION {&page1browse} TO ROWID rLastCreated NO-ERROR.
                    GET NEXT {&page1browse} NO-LOCK.
                    
                    /*--- Posiciona browse filho no pr¢ximo registro ---*/
                    {&page1browse}:SELECT-FOCUSED-ROW().
                END. /*--- CAN-FIND FIRST {&ttTable1} ---*/
            END.
        &ENDIF
        
        &IF "{&page2browse}":U <> "":U &THEN
            WHEN 2 THEN DO:
                /*--- Seta vari†vel rLast com o valor do £ltimo rowid da 
                      tabela {&ttTable2} existente no browse ---*/
                GET LAST {&page2browse} NO-LOCK.
                IF AVAILABLE {&ttTable2} THEN
                    ASSIGN rLast = {&ttTable2}.r-Rowid.
                ELSE
                    ASSIGN rLast = ?.
                
                /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                &IF DEFINED(DBOVersion) <> 0 &THEN
                    /*--- Retorna todos os registros do DBO filho, na temp-table ttTable2Aux ---*/
                    RUN serverSendRows IN {&hDBOTable2} (INPUT ?,
                                                       INPUT STRING(rLast),
                                                       INPUT IF rLast = ? THEN NO ELSE YES,
                                                       INPUT ?,
                                                       OUTPUT iRowsReturned,
                                                       OUTPUT TABLE ttTable2Aux).
                &ELSE
                    /*--- Retorna todos os registros do DBO filho, na temp-table ttTable2Aux ---*/
                    RUN getBatchRecords IN {&hDBOTable2} (INPUT rLast,
                                                        INPUT IF rLast = ? THEN NO ELSE YES,
                                                        INPUT ?,
                                                        OUTPUT iRowsReturned,
                                                        OUTPUT TABLE ttTable2Aux).
                &ENDIF
                
                /*--- Cancela trigger caso n∆o existam mais registros no DBO filho ---*/
                IF CAN-FIND(FIRST ttTable2Aux) THEN DO:
                    /*--- Transfere dados da temp-table ttTable2Aux para a temp-table {&ttTable2} ---*/
                    &IF  "{&ROW-NUM-DEFINED2}":U = "YES":U  &THEN
                        ASSIGN iLastSeq = 0.
                        FOR LAST {&ttTable2} BY {&ttTable2}.RowNum:
                            /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                            ASSIGN iLastSeq = {&ttTable2}.RowNum.
                        END.
                    &ENDIF
                    FOR EACH ttTable2Aux:
                        CREATE {&ttTable2}.

                        &IF  "{&ROW-NUM-DEFINED2}":U = "YES":U  &THEN
                            ASSIGN {&ttTable2}.RowNum = {&ttTable2}.RowNum + iLastSeq
                                           iLastSeq = {&ttTable2}.RowNum. 
                            /*--- Acrescentado incremento do iLastSeq para que na pr¢xima 
                                    iteraá∆o do For Each n∆o repita o registro. ---*/    
                            BUFFER-COPY ttTable2Aux except RowNum TO {&ttTable2}.
                        &ELSE
                            BUFFER-COPY ttTable2Aux TO {&ttTable2}.
                        &ENDIF
                        ASSIGN rLastCreated = ROWID({&ttTable2}).
                    END.
                
                    /*--- Abre o browse filho, para atualizaá∆o dos dados ---*/
                    {&OPEN-QUERY-{&page2browse}}
                    
                    /*--- Seta view-port do browse filho para reposicionamento ---*/
                    {&page2browse}:SET-REPOSITIONED-ROW({&page2browse}:DOWN IN FRAME fPage2) IN FRAME fPage2.
                    
                    /*--- Reposiciona browse filho no £ltimo registro ---*/
                    REPOSITION {&page2browse} TO ROWID rLastCreated NO-ERROR.
                    GET NEXT {&page2browse} NO-LOCK.
                    
                    /*--- Posiciona browse filho no pr¢ximo registro ---*/
                    {&page2browse}:SELECT-FOCUSED-ROW().
                END. /*--- CAN-FIND FIRST {&ttTable2} ---*/
            END.
        &ENDIF
        
        &IF "{&page3browse}":U <> "":U &THEN
            WHEN 3 THEN DO:
                /*--- Seta vari†vel rLast com o valor do £ltimo rowid da 
                      tabela {&ttTable3} existente no browse ---*/
                GET LAST {&page3browse} NO-LOCK.
                IF AVAILABLE {&ttTable3} THEN
                    ASSIGN rLast = {&ttTable3}.r-Rowid.
                ELSE
                    ASSIGN rLast = ?.
                
                /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                &IF DEFINED(DBOVersion) <> 0 &THEN
                    /*--- Retorna todos os registros do DBO filho, na temp-table ttTable3Aux ---*/
                    RUN serverSendRows IN {&hDBOTable3} (INPUT ?,
                                                       INPUT STRING(rLast),
                                                       INPUT IF rLast = ? THEN NO ELSE YES,
                                                       INPUT ?,
                                                       OUTPUT iRowsReturned,
                                                       OUTPUT TABLE ttTable3Aux).
                &ELSE
                    /*--- Retorna todos os registros do DBO filho, na temp-table ttTable3Aux ---*/
                    RUN getBatchRecords IN {&hDBOTable3} (INPUT rLast,
                                                        INPUT IF rLast = ? THEN NO ELSE YES,
                                                        INPUT ?,
                                                        OUTPUT iRowsReturned,
                                                        OUTPUT TABLE ttTable3Aux).
                &ENDIF
                
                /*--- Cancela trigger caso n∆o existam mais registros no DBO filho ---*/
                IF CAN-FIND(FIRST ttTable3Aux) THEN DO:
                    /*--- Transfere dados da temp-table ttTable3Aux para a temp-table {&ttTable3} ---*/
                    &IF  "{&ROW-NUM-DEFINED3}":U = "YES":U  &THEN
                        ASSIGN iLastSeq = 0.
                        FOR LAST {&ttTable3} BY {&ttTable3}.RowNum:
                            /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                            ASSIGN iLastSeq = {&ttTable3}.RowNum.
                        END.
                    &ENDIF
                    FOR EACH ttTable3Aux:
                        CREATE {&ttTable3}.

                        &IF  "{&ROW-NUM-DEFINED3}":U = "YES":U  &THEN
                            ASSIGN {&ttTable3}.RowNum = {&ttTable3}.RowNum + iLastSeq
                                           iLastSeq = {&ttTable3}.RowNum. 
                            /*--- Acrescentado incremento do iLastSeq para que na pr¢xima 
                                    iteraá∆o do For Each n∆o repita o registro. ---*/    
                            BUFFER-COPY ttTable3Aux except RowNum TO {&ttTable3}.
                        &ELSE
                            BUFFER-COPY ttTable3Aux TO {&ttTable3}.
                        &ENDIF
                        ASSIGN rLastCreated = ROWID({&ttTable3}).
                    END.
                
                    /*--- Abre o browse filho, para atualizaá∆o dos dados ---*/
                    {&OPEN-QUERY-{&page3browse}}
                    
                    /*--- Seta view-port do browse filho para reposicionamento ---*/
                    {&page3browse}:SET-REPOSITIONED-ROW({&page3browse}:DOWN IN FRAME fPage3) IN FRAME fPage3.
                    
                    /*--- Reposiciona browse filho no £ltimo registro ---*/
                    REPOSITION {&page3browse} TO ROWID rLastCreated NO-ERROR.
                    GET NEXT {&page3browse} NO-LOCK.
                    
                    /*--- Posiciona browse filho no pr¢ximo registro ---*/
                    {&page3browse}:SELECT-FOCUSED-ROW().
                END. /*--- CAN-FIND FIRST {&ttTable3} ---*/
            END.
        &ENDIF
        
        &IF "{&page4browse}":U <> "":U &THEN
            WHEN 4 THEN DO:
                /*--- Seta vari†vel rLast com o valor do £ltimo rowid da 
                      tabela {&ttTable4} existente no browse ---*/
                GET LAST {&page4browse} NO-LOCK.
                IF AVAILABLE {&ttTable4} THEN
                    ASSIGN rLast = {&ttTable4}.r-Rowid.
                ELSE
                    ASSIGN rLast = ?.
                
                /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                &IF DEFINED(DBOVersion) <> 0 &THEN
                    /*--- Retorna todos os registros do DBO filho, na temp-table ttTable4Aux ---*/
                    RUN serverSendRows IN {&hDBOTable4} (INPUT ?,
                                                       INPUT STRING(rLast),
                                                       INPUT IF rLast = ? THEN NO ELSE YES,
                                                       INPUT ?,
                                                       OUTPUT iRowsReturned,
                                                       OUTPUT TABLE ttTable4Aux).
                &ELSE
                    /*--- Retorna todos os registros do DBO filho, na temp-table ttTable4Aux ---*/
                    RUN getBatchRecords IN {&hDBOTable4} (INPUT rLast,
                                                        INPUT IF rLast = ? THEN NO ELSE YES,
                                                        INPUT ?,
                                                        OUTPUT iRowsReturned,
                                                        OUTPUT TABLE ttTable4Aux).
                &ENDIF
                
                /*--- Cancela trigger caso n∆o existam mais registros no DBO filho ---*/
                IF CAN-FIND(FIRST ttTable4Aux) THEN DO:
                    /*--- Transfere dados da temp-table ttTable4Aux para a temp-table {&ttTable4} ---*/
                    &IF  "{&ROW-NUM-DEFINED4}":U = "YES":U  &THEN
                        ASSIGN iLastSeq = 0.
                        FOR LAST {&ttTable4} BY {&ttTable4}.RowNum:
                            /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                            ASSIGN iLastSeq = {&ttTable4}.RowNum.
                        END.
                    &ENDIF
                    FOR EACH ttTable4Aux:
                        CREATE {&ttTable4}.

                        &IF  "{&ROW-NUM-DEFINED4}":U = "YES":U  &THEN
                            ASSIGN {&ttTable4}.RowNum = {&ttTable4}.RowNum + iLastSeq
                                           iLastSeq = {&ttTable4}.RowNum. 
                            /*--- Acrescentado incremento do iLastSeq para que na pr¢xima 
                                    iteraá∆o do For Each n∆o repita o registro. ---*/    
                            BUFFER-COPY ttTable4Aux except RowNum TO {&ttTable4}.
                        &ELSE
                            BUFFER-COPY ttTable4Aux TO {&ttTable4}.
                        &ENDIF
                        ASSIGN rLastCreated = ROWID({&ttTable4}).
                    END.
                
                    /*--- Abre o browse filho, para atualizaá∆o dos dados ---*/
                    {&OPEN-QUERY-{&page4browse}}
                    
                    /*--- Seta view-port do browse filho para reposicionamento ---*/
                    {&page4browse}:SET-REPOSITIONED-ROW({&page4browse}:DOWN IN FRAME fPage4) IN FRAME fPage4.
                    
                    /*--- Reposiciona browse filho no £ltimo registro ---*/
                    REPOSITION {&page4browse} TO ROWID rLastCreated NO-ERROR.
                    GET NEXT {&page4browse} NO-LOCK.
                    
                    /*--- Posiciona browse filho no pr¢ximo registro ---*/
                    {&page4browse}:SELECT-FOCUSED-ROW().
                END. /*--- CAN-FIND FIRST {&ttTable4} ---*/
            END.
        &ENDIF
        
        &IF "{&page5browse}":U <> "":U &THEN
            WHEN 5 THEN DO:
                /*--- Seta vari†vel rLast com o valor do £ltimo rowid da 
                      tabela {&ttTable5} existente no browse ---*/
                GET LAST {&page5browse} NO-LOCK.
                IF AVAILABLE {&ttTable5} THEN
                    ASSIGN rLast = {&ttTable5}.r-Rowid.
                ELSE
                    ASSIGN rLast = ?.
                
                /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                &IF DEFINED(DBOVersion) <> 0 &THEN
                    /*--- Retorna todos os registros do DBO filho, na temp-table ttTable5Aux ---*/
                    RUN serverSendRows IN {&hDBOTable5} (INPUT ?,
                                                       INPUT STRING(rLast),
                                                       INPUT IF rLast = ? THEN NO ELSE YES,
                                                       INPUT ?,
                                                       OUTPUT iRowsReturned,
                                                       OUTPUT TABLE ttTable5Aux).
                &ELSE
                    /*--- Retorna todos os registros do DBO filho, na temp-table ttTable5Aux ---*/
                    RUN getBatchRecords IN {&hDBOTable5} (INPUT rLast,
                                                        INPUT IF rLast = ? THEN NO ELSE YES,
                                                        INPUT ?,
                                                        OUTPUT iRowsReturned,
                                                        OUTPUT TABLE ttTable5Aux).
                &ENDIF
                
                /*--- Cancela trigger caso n∆o existam mais registros no DBO filho ---*/
                IF CAN-FIND(FIRST ttTable5Aux) THEN DO:
                    /*--- Transfere dados da temp-table ttTable5Aux para a temp-table {&ttTable5} ---*/
                    &IF  "{&ROW-NUM-DEFINED5}":U = "YES":U  &THEN
                        ASSIGN iLastSeq = 0.
                        FOR LAST {&ttTable5} BY {&ttTable5}.RowNum:
                            /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                            ASSIGN iLastSeq = {&ttTable5}.RowNum.
                        END.
                    &ENDIF
                    FOR EACH ttTable5Aux:
                        CREATE {&ttTable5}.

                        &IF  "{&ROW-NUM-DEFINED5}":U = "YES":U  &THEN
                            ASSIGN {&ttTable5}.RowNum = {&ttTable5}.RowNum + iLastSeq
                                           iLastSeq = {&ttTable5}.RowNum. 
                            /*--- Acrescentado incremento do iLastSeq para que na pr¢xima 
                                    iteraá∆o do For Each n∆o repita o registro. ---*/    
                            BUFFER-COPY ttTable5Aux except RowNum TO {&ttTable5}.
                        &ELSE
                            BUFFER-COPY ttTable5Aux TO {&ttTable5}.
                        &ENDIF
                        ASSIGN rLastCreated = ROWID({&ttTable5}).
                    END.
                
                    /*--- Abre o browse filho, para atualizaá∆o dos dados ---*/
                    {&OPEN-QUERY-{&page5browse}}
                    
                    /*--- Seta view-port do browse filho para reposicionamento ---*/
                    {&page5browse}:SET-REPOSITIONED-ROW({&page5browse}:DOWN IN FRAME fPage5) IN FRAME fPage5.
                    
                    /*--- Reposiciona browse filho no £ltimo registro ---*/
                    REPOSITION {&page5browse} TO ROWID rLastCreated NO-ERROR.
                    GET NEXT {&page5browse} NO-LOCK.
                    
                    /*--- Posiciona browse filho no pr¢ximo registro ---*/
                    {&page5browse}:SELECT-FOCUSED-ROW().
                END. /*--- CAN-FIND FIRST {&ttTable5} ---*/
            END.
        &ENDIF
        
        &IF "{&page6browse}":U <> "":U &THEN
            WHEN 6 THEN DO:
                /*--- Seta vari†vel rLast com o valor do £ltimo rowid da 
                      tabela {&ttTable6} existente no browse ---*/
                GET LAST {&page6browse} NO-LOCK.
                IF AVAILABLE {&ttTable6} THEN
                    ASSIGN rLast = {&ttTable6}.r-Rowid.
                ELSE
                    ASSIGN rLast = ?.
                
                /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                &IF DEFINED(DBOVersion) <> 0 &THEN
                    /*--- Retorna todos os registros do DBO filho, na temp-table ttTable6Aux ---*/
                    RUN serverSendRows IN {&hDBOTable6} (INPUT ?,
                                                       INPUT STRING(rLast),
                                                       INPUT IF rLast = ? THEN NO ELSE YES,
                                                       INPUT ?,
                                                       OUTPUT iRowsReturned,
                                                       OUTPUT TABLE ttTable6Aux).
                &ELSE
                    /*--- Retorna todos os registros do DBO filho, na temp-table ttTable6Aux ---*/
                    RUN getBatchRecords IN {&hDBOTable6} (INPUT rLast,
                                                        INPUT IF rLast = ? THEN NO ELSE YES,
                                                        INPUT ?,
                                                        OUTPUT iRowsReturned,
                                                        OUTPUT TABLE ttTable6Aux).
                &ENDIF
                
                /*--- Cancela trigger caso n∆o existam mais registros no DBO filho ---*/
                IF CAN-FIND(FIRST ttTable6Aux) THEN DO:
                    /*--- Transfere dados da temp-table ttTable6Aux para a temp-table {&ttTable6} ---*/
                    &IF  "{&ROW-NUM-DEFINED6}":U = "YES":U  &THEN
                        ASSIGN iLastSeq = 0.
                        FOR LAST {&ttTable6} BY {&ttTable6}.RowNum:
                            /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                            ASSIGN iLastSeq = {&ttTable6}.RowNum.
                        END.
                    &ENDIF
                    FOR EACH ttTable6Aux:
                        CREATE {&ttTable6}.

                        &IF  "{&ROW-NUM-DEFINED6}":U = "YES":U  &THEN
                            ASSIGN {&ttTable6}.RowNum = {&ttTable6}.RowNum + iLastSeq
                                           iLastSeq = {&ttTable6}.RowNum. 
                            /*--- Acrescentado incremento do iLastSeq para que na pr¢xima 
                                    iteraá∆o do For Each n∆o repita o registro. ---*/    
                            BUFFER-COPY ttTable6Aux except RowNum TO {&ttTable6}.
                        &ELSE
                            BUFFER-COPY ttTable6Aux TO {&ttTable6}.
                        &ENDIF
                        ASSIGN rLastCreated = ROWID({&ttTable6}).
                    END.
                
                    /*--- Abre o browse filho, para atualizaá∆o dos dados ---*/
                    {&OPEN-QUERY-{&page6browse}}
                    
                    /*--- Seta view-port do browse filho para reposicionamento ---*/
                    {&page6browse}:SET-REPOSITIONED-ROW({&page6browse}:DOWN IN FRAME fPage6) IN FRAME fPage6.
                    
                    /*--- Reposiciona browse filho no £ltimo registro ---*/
                    REPOSITION {&page6browse} TO ROWID rLastCreated NO-ERROR.
                    GET NEXT {&page6browse} NO-LOCK.
                    
                    /*--- Posiciona browse filho no pr¢ximo registro ---*/
                    {&page6browse}:SELECT-FOCUSED-ROW().
                END. /*--- CAN-FIND FIRST {&ttTable6} ---*/
            END.
        &ENDIF
        
        &IF "{&page7browse}":U <> "":U &THEN
            WHEN 7 THEN DO:
                /*--- Seta vari†vel rLast com o valor do £ltimo rowid da 
                      tabela {&ttTable7} existente no browse ---*/
                GET LAST {&page7browse} NO-LOCK.
                IF AVAILABLE {&ttTable7} THEN
                    ASSIGN rLast = {&ttTable7}.r-Rowid.
                ELSE
                    ASSIGN rLast = ?.
                
                /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                &IF DEFINED(DBOVersion) <> 0 &THEN
                    /*--- Retorna todos os registros do DBO filho, na temp-table ttTable7Aux ---*/
                    RUN serverSendRows IN {&hDBOTable7} (INPUT ?,
                                                       INPUT STRING(rLast),
                                                       INPUT IF rLast = ? THEN NO ELSE YES,
                                                       INPUT ?,
                                                       OUTPUT iRowsReturned,
                                                       OUTPUT TABLE ttTable7Aux).
                &ELSE
                    /*--- Retorna todos os registros do DBO filho, na temp-table ttTable7Aux ---*/
                    RUN getBatchRecords IN {&hDBOTable7} (INPUT rLast,
                                                        INPUT IF rLast = ? THEN NO ELSE YES,
                                                        INPUT ?,
                                                        OUTPUT iRowsReturned,
                                                        OUTPUT TABLE ttTable7Aux).
                &ENDIF
                
                /*--- Cancela trigger caso n∆o existam mais registros no DBO filho ---*/
                IF CAN-FIND(FIRST ttTable7Aux) THEN DO:
                    /*--- Transfere dados da temp-table ttTable7Aux para a temp-table {&ttTable7} ---*/
                    &IF  "{&ROW-NUM-DEFINED7}":U = "YES":U  &THEN
                        ASSIGN iLastSeq = 0.
                        FOR LAST {&ttTable7} BY {&ttTable7}.RowNum:
                            /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                            ASSIGN iLastSeq = {&ttTable7}.RowNum.
                        END.
                    &ENDIF
                    FOR EACH ttTable7Aux:
                        CREATE {&ttTable7}.

                        &IF  "{&ROW-NUM-DEFINED7}":U = "YES":U  &THEN
                            ASSIGN {&ttTable7}.RowNum = {&ttTable7}.RowNum + iLastSeq
                                           iLastSeq = {&ttTable7}.RowNum. 
                            /*--- Acrescentado incremento do iLastSeq para que na pr¢xima 
                                    iteraá∆o do For Each n∆o repita o registro. ---*/    
                            BUFFER-COPY ttTable7Aux except RowNum TO {&ttTable7}.
                        &ELSE
                            BUFFER-COPY ttTable7Aux TO {&ttTable7}.
                        &ENDIF
                        ASSIGN rLastCreated = ROWID({&ttTable7}).
                    END.
                
                    /*--- Abre o browse filho, para atualizaá∆o dos dados ---*/
                    {&OPEN-QUERY-{&page7browse}}
                    
                    /*--- Seta view-port do browse filho para reposicionamento ---*/
                    {&page7browse}:SET-REPOSITIONED-ROW({&page7browse}:DOWN IN FRAME fPage7) IN FRAME fPage7.
                    
                    /*--- Reposiciona browse filho no £ltimo registro ---*/
                    REPOSITION {&page7browse} TO ROWID rLastCreated NO-ERROR.
                    GET NEXT {&page7browse} NO-LOCK.
                    
                    /*--- Posiciona browse filho no pr¢ximo registro ---*/
                    {&page7browse}:SELECT-FOCUSED-ROW().
                END. /*--- CAN-FIND FIRST {&ttTable7} ---*/
            END.
        &ENDIF
        
        &IF "{&page8browse}":U <> "":U &THEN
            WHEN 8 THEN DO:
                /*--- Seta vari†vel rLast com o valor do £ltimo rowid da 
                      tabela {&ttTable8} existente no browse ---*/
                GET LAST {&page8browse} NO-LOCK.
                IF AVAILABLE {&ttTable8} THEN
                    ASSIGN rLast = {&ttTable8}.r-Rowid.
                ELSE
                    ASSIGN rLast = ?.
                
                /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                &IF DEFINED(DBOVersion) <> 0 &THEN
                    /*--- Retorna todos os registros do DBO filho, na temp-table ttTable8Aux ---*/
                    RUN serverSendRows IN {&hDBOTable8} (INPUT ?,
                                                       INPUT STRING(rLast),
                                                       INPUT IF rLast = ? THEN NO ELSE YES,
                                                       INPUT ?,
                                                       OUTPUT iRowsReturned,
                                                       OUTPUT TABLE ttTable8Aux).
                &ELSE
                    /*--- Retorna todos os registros do DBO filho, na temp-table ttTable8Aux ---*/
                    RUN getBatchRecords IN {&hDBOTable8} (INPUT rLast,
                                                        INPUT IF rLast = ? THEN NO ELSE YES,
                                                        INPUT ?,
                                                        OUTPUT iRowsReturned,
                                                        OUTPUT TABLE ttTable8Aux).
                &ENDIF
                
                /*--- Cancela trigger caso n∆o existam mais registros no DBO filho ---*/
                IF CAN-FIND(FIRST ttTable8Aux) THEN DO:
                    /*--- Transfere dados da temp-table ttTable8Aux para a temp-table {&ttTable8} ---*/
                    &IF  "{&ROW-NUM-DEFINED8}":U = "YES":U  &THEN
                        ASSIGN iLastSeq = 0.
                        FOR LAST {&ttTable8} BY {&ttTable8}.RowNum:
                            /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                            ASSIGN iLastSeq = {&ttTable8}.RowNum.
                        END.
                    &ENDIF
                    FOR EACH ttTable8Aux:
                        CREATE {&ttTable8}.

                        &IF  "{&ROW-NUM-DEFINED8}":U = "YES":U  &THEN
                            ASSIGN {&ttTable8}.RowNum = {&ttTable8}.RowNum + iLastSeq
                                           iLastSeq = {&ttTable8}.RowNum. 
                            /*--- Acrescentado incremento do iLastSeq para que na pr¢xima 
                                    iteraá∆o do For Each n∆o repita o registro. ---*/    
                            BUFFER-COPY ttTable8Aux except RowNum TO {&ttTable8}.
                        &ELSE
                            BUFFER-COPY ttTable8Aux TO {&ttTable8}.
                        &ENDIF
                        ASSIGN rLastCreated = ROWID({&ttTable8}).
                    END.
                
                    /*--- Abre o browse filho, para atualizaá∆o dos dados ---*/
                    {&OPEN-QUERY-{&page8browse}}
                    
                    /*--- Seta view-port do browse filho para reposicionamento ---*/
                    {&page8browse}:SET-REPOSITIONED-ROW({&page8browse}:DOWN IN FRAME fPage8) IN FRAME fPage8.
                    
                    /*--- Reposiciona browse filho no £ltimo registro ---*/
                    REPOSITION {&page8browse} TO ROWID rLastCreated NO-ERROR.
                    GET NEXT {&page8browse} NO-LOCK.
                    
                    /*--- Posiciona browse filho no pr¢ximo registro ---*/
                    {&page8browse}:SELECT-FOCUSED-ROW().
                END. /*--- CAN-FIND FIRST {&ttTable8} ---*/
            END.
        &ENDIF
    END CASE.
    
    /*--- Seta vari†vel cEventBrowse com o valor "" ---*/
    ASSIGN cEventBrowse = "":U.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-applyHome) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE applyHome Method-Library 
PROCEDURE applyHome :
/*------------------------------------------------------------------------------
  Purpose:     L¢gica para trigger de HOME
  Parameters:  recebe o n£mero da p†gina
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pPageNumber AS INTEGER NO-UNDO.
    
    /*INSERIDO TESTE DO PRE-PROCESSADOR &DBOVersion PARA QUE O C‡DIGO ABAIXO SEJA EXECUTADO*/
    /*SOMENTE QUANDO O DBO FOR 2.O*/
    /*ALTERAÄ«O FEITA POR ANDERSON (TECH485) EM 04/04/2001 PARA EVITAR QUE QUANDO SE NAVEGUE PARA*/
    /*CIMA EM UM BROWSE O MESMO N«O DE O ERRO DE REGISTRO DUPLICADO*/
    /*TESTE PRE-PROCESSADOR(VERS«O DO BO)*/
    &IF DEFINED(DBOVersion) = 0 &THEN    

        DEFINE VARIABLE rLastCreated AS ROWID NO-UNDO.
        DEFINE VARIABLE rFirst    AS ROWID NO-UNDO.
        
        /*--- Testa o pre-processador RowNumDefinedN para todas as p†ginas para
              definir iFirstSeq que Ç utilizada no c†lculo do valor do rowNum    ---*/
        &IF  "{&ROW-NUM-DEFINED1}":U = "YES":U  or
             "{&ROW-NUM-DEFINED2}":U = "YES":U  or
             "{&ROW-NUM-DEFINED3}":U = "YES":U  or
             "{&ROW-NUM-DEFINED4}":U = "YES":U  or
             "{&ROW-NUM-DEFINED5}":U = "YES":U  or
             "{&ROW-NUM-DEFINED6}":U = "YES":U  or
             "{&ROW-NUM-DEFINED7}":U = "YES":U  or
             "{&ROW-NUM-DEFINED8}":U = "YES":U  &THEN
            DEFINE VARIABLE iFirstSeq AS INTEGER NO-UNDO INIT 0.
        &ENDIF

        CASE pPageNumber:
    
            &IF "{&page1browse}":U <> "":U &THEN
                WHEN 1 THEN DO:
                    /*--- Seta vari†vel rFirst com o valor do primeiro rowid 
                          da tabela {&ttTable1} existente no browse               ---*/
                    GET FIRST {&page1browse} NO-LOCK.
                    IF AVAILABLE {&ttTable1} THEN DO:
                        ASSIGN rFirst = {&ttTable1}.r-Rowid.
                            
                        /*--- Seta ponteiro do browse no registro correto ---*/
                        BROWSE {&page1browse}:SELECT-FOCUSED-ROW().
                    END.
                    ELSE
                        ASSIGN rFirst = ?.
                        
                    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                    &IF DEFINED(DBOVersion) <> 0 &THEN
                        /*--- Retorna registros do DBO filho, na temp-table ttTable1Aux ---*/
                        RUN serverSendRows IN {&hDBOTable1} (INPUT ?,
                                                           INPUT STRING(rFirst),
                                                           INPUT IF rFirst = ? THEN NO ELSE YES,
                                                           INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                                 &ELSE 40 &endif,
                                                           OUTPUT iRowsReturned,
                                                           OUTPUT TABLE ttTable1Aux).
                    &ELSE
                        /*--- Retorna todos os registros do DBO filho, na temp-table ttTable1Aux ---*/
                        RUN getBatchRecordsPrev IN {&hDBOTable1} (INPUT rFirst,
                                                                INPUT IF rFirst = ? THEN NO ELSE YES,
                                                                INPUT ?,
                                                                OUTPUT iRowsReturned,
                                                                OUTPUT TABLE ttTable1Aux).
                    &ENDIF
                        
                    /*--- Cancela trigger caso n∆o existam mais registros no DBO filho ---*/
                    IF CAN-FIND(FIRST ttTable1Aux) THEN DO:
    
                        /*--- Transfere dados da temp-table ttTable1Aux para a temp-table {&ttTable1} ---*/
                        &IF  "{&ROW-NUM-DEFINED1}":U = "YES":U  &THEN
                            ASSIGN iFirstSeq = 0.
                            FOR LAST {&ttTable1} BY {&ttTable1}.RowNum:
                                /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                                ASSIGN iFirstSeq = {&ttTable1}.RowNum.
                            END.
                        &ENDIF
                        FOR EACH ttTable1Aux:
                            CREATE {&ttTable1}.
                            &IF  "{&ROW-NUM-DEFINED1}":U = "YES":U  &THEN
                                ASSIGN {&ttTable1}.RowNum = {&ttTable1}.RowNum + iFirstSeq
                                               iFirstSeq = {&ttTable1}.RowNum. 
                                /*--- Acrescentado incremento do iFirstSeq para que na pr¢xima 
                                        iteraá∆o do For Each n∆o repita o registro. ---*/    
                                BUFFER-COPY ttTable1Aux except RowNum TO {&ttTable1}.
                            &ELSE
                                BUFFER-COPY ttTable1Aux TO {&ttTable1}.
                            &ENDIF
                            ASSIGN rLastCreated = ROWID({&ttTable1}).
                        END.
                        
                        /*--- Abre o browse filho, para atualizaá∆o dos dados ---*/
                        {&OPEN-QUERY-{&page1browse}}
                        
                        /*--- Seta view-port do browse filho para reposicionamento ---*/
                        {&page1browse}:SET-REPOSITIONED-ROW({&page1browse}:DOWN IN FRAME fPage1) IN FRAME fPage1.
                        
                        /*--- Reposiciona browse filho no £ltimo registro ---*/
                        REPOSITION {&page1browse} TO ROWID rLastCreated NO-ERROR.
                        GET NEXT {&page1browse} NO-LOCK.
                        
                        /*--- Posiciona browse filho no pr¢ximo registro ---*/
                        {&page1browse}:SELECT-FOCUSED-ROW().
                    END. /*--- END DO CAN-FIND {&ttTable1} ---*/
                END.
            &ENDIF
            
            &IF "{&page2browse}":U <> "":U &THEN
                WHEN 2 THEN DO:
                    /*--- Seta vari†vel rFirst com o valor do primeiro rowid 
                          da tabela {&ttTable2} existente no browse               ---*/
                    GET FIRST {&page2browse} NO-LOCK.
                    IF AVAILABLE {&ttTable2} THEN DO:
                        ASSIGN rFirst = {&ttTable2}.r-Rowid.
                            
                        /*--- Seta ponteiro do browse no registro correto ---*/
                        BROWSE {&page2browse}:SELECT-FOCUSED-ROW().
                    END.
                    ELSE
                        ASSIGN rFirst = ?.
                        
                    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                    &IF DEFINED(DBOVersion) <> 0 &THEN
                        /*--- Retorna registros do DBO filho, na temp-table ttTable2Aux ---*/
                        RUN serverSendRows IN {&hDBOTable2} (INPUT ?,
                                                           INPUT STRING(rFirst),
                                                           INPUT IF rFirst = ? THEN NO ELSE YES,
                                                           INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                                 &ELSE 40 &endif,
                                                           OUTPUT iRowsReturned,
                                                           OUTPUT TABLE ttTable2Aux).
                    &ELSE
                        /*--- Retorna todos os registros do DBO filho, na temp-table ttTable2Aux ---*/
                        RUN getBatchRecordsPrev IN {&hDBOTable2} (INPUT rFirst,
                                                                INPUT IF rFirst = ? THEN NO ELSE YES,
                                                                INPUT ?,
                                                                OUTPUT iRowsReturned,
                                                                OUTPUT TABLE ttTable2Aux).
                    &ENDIF
                        
                    /*--- Cancela trigger caso n∆o existam mais registros no DBO filho ---*/
                    IF CAN-FIND(FIRST ttTable2Aux) THEN DO:
    
                        /*--- Transfere dados da temp-table ttTable2Aux para a temp-table {&ttTable2} ---*/
                        &IF  "{&ROW-NUM-DEFINED2}":U = "YES":U  &THEN
                            ASSIGN iFirstSeq = 0.
                            FOR LAST {&ttTable2} BY {&ttTable2}.RowNum:
                                /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                                ASSIGN iFirstSeq = {&ttTable2}.RowNum.
                            END.
                        &ENDIF
                        FOR EACH ttTable2Aux:
                            CREATE {&ttTable2}.
                            &IF  "{&ROW-NUM-DEFINED2}":U = "YES":U  &THEN
                                ASSIGN {&ttTable2}.RowNum = {&ttTable2}.RowNum + iFirstSeq
                                               iFirstSeq = {&ttTable2}.RowNum. 
                                /*--- Acrescentado incremento do iFirstSeq para que na pr¢xima 
                                        iteraá∆o do For Each n∆o repita o registro. ---*/    
                                BUFFER-COPY ttTable2Aux except RowNum TO {&ttTable2}.
                            &ELSE
                                BUFFER-COPY ttTable2Aux TO {&ttTable2}.
                            &ENDIF
                            ASSIGN rLastCreated = ROWID({&ttTable2}).
                        END.
                        
                        /*--- Abre o browse filho, para atualizaá∆o dos dados ---*/
                        {&OPEN-QUERY-{&page2browse}}
                        
                        /*--- Seta view-port do browse filho para reposicionamento ---*/
                        {&page2browse}:SET-REPOSITIONED-ROW({&page2browse}:DOWN IN FRAME fPage2) IN FRAME fPage2.
                        
                        /*--- Reposiciona browse filho no £ltimo registro ---*/
                        REPOSITION {&page2browse} TO ROWID rLastCreated NO-ERROR.
                        GET NEXT {&page2browse} NO-LOCK.
                        
                        /*--- Posiciona browse filho no pr¢ximo registro ---*/
                        {&page2browse}:SELECT-FOCUSED-ROW().
                    END. /*--- END DO CAN-FIND {&ttTable2} ---*/
                END.
            &ENDIF
            
            &IF "{&page3browse}":U <> "":U &THEN
                WHEN 3 THEN DO:
                    /*--- Seta vari†vel rFirst com o valor do primeiro rowid 
                          da tabela {&ttTable3} existente no browse               ---*/
                    GET FIRST {&page3browse} NO-LOCK.
                    IF AVAILABLE {&ttTable3} THEN DO:
                        ASSIGN rFirst = {&ttTable3}.r-Rowid.
                            
                        /*--- Seta ponteiro do browse no registro correto ---*/
                        BROWSE {&page3browse}:SELECT-FOCUSED-ROW().
                    END.
                    ELSE
                        ASSIGN rFirst = ?.
                        
                    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                    &IF DEFINED(DBOVersion) <> 0 &THEN
                        /*--- Retorna registros do DBO filho, na temp-table ttTable3Aux ---*/
                        RUN serverSendRows IN {&hDBOTable3} (INPUT ?,
                                                           INPUT STRING(rFirst),
                                                           INPUT IF rFirst = ? THEN NO ELSE YES,
                                                           INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                                 &ELSE 40 &endif,
                                                           OUTPUT iRowsReturned,
                                                           OUTPUT TABLE ttTable3Aux).
                    &ELSE
                        /*--- Retorna todos os registros do DBO filho, na temp-table ttTable3Aux ---*/
                        RUN getBatchRecordsPrev IN {&hDBOTable3} (INPUT rFirst,
                                                                INPUT IF rFirst = ? THEN NO ELSE YES,
                                                                INPUT ?,
                                                                OUTPUT iRowsReturned,
                                                                OUTPUT TABLE ttTable3Aux).
                    &ENDIF
                        
                    /*--- Cancela trigger caso n∆o existam mais registros no DBO filho ---*/
                    IF CAN-FIND(FIRST ttTable3Aux) THEN DO:
    
                        /*--- Transfere dados da temp-table ttTable3Aux para a temp-table {&ttTable3} ---*/
                        &IF  "{&ROW-NUM-DEFINED3}":U = "YES":U  &THEN
                            ASSIGN iFirstSeq = 0.
                            FOR LAST {&ttTable3} BY {&ttTable3}.RowNum:
                                /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                                ASSIGN iFirstSeq = {&ttTable3}.RowNum.
                            END.
                        &ENDIF
                        FOR EACH ttTable3Aux:
                            CREATE {&ttTable3}.
                            &IF  "{&ROW-NUM-DEFINED3}":U = "YES":U  &THEN
                                ASSIGN {&ttTable3}.RowNum = {&ttTable3}.RowNum + iFirstSeq
                                               iFirstSeq = {&ttTable3}.RowNum. 
                                /*--- Acrescentado incremento do iFirstSeq para que na pr¢xima 
                                        iteraá∆o do For Each n∆o repita o registro. ---*/    
                                BUFFER-COPY ttTable3Aux except RowNum TO {&ttTable3}.
                            &ELSE
                                BUFFER-COPY ttTable3Aux TO {&ttTable3}.
                            &ENDIF
                            ASSIGN rLastCreated = ROWID({&ttTable3}).
                        END.
                        
                        /*--- Abre o browse filho, para atualizaá∆o dos dados ---*/
                        {&OPEN-QUERY-{&page3browse}}
                        
                        /*--- Seta view-port do browse filho para reposicionamento ---*/
                        {&page3browse}:SET-REPOSITIONED-ROW({&page3browse}:DOWN IN FRAME fPage3) IN FRAME fPage3.
                        
                        /*--- Reposiciona browse filho no £ltimo registro ---*/
                        REPOSITION {&page3browse} TO ROWID rLastCreated NO-ERROR.
                        GET NEXT {&page3browse} NO-LOCK.
                        
                        /*--- Posiciona browse filho no pr¢ximo registro ---*/
                        {&page3browse}:SELECT-FOCUSED-ROW().
                    END. /*--- END DO CAN-FIND {&ttTable3} ---*/
                END.
            &ENDIF

            
            &IF "{&page4browse}":U <> "":U &THEN
                WHEN 4 THEN DO:
                    /*--- Seta vari†vel rFirst com o valor do primeiro rowid 
                          da tabela {&ttTable4} existente no browse               ---*/
                    GET FIRST {&page4browse} NO-LOCK.
                    IF AVAILABLE {&ttTable4} THEN DO:
                        ASSIGN rFirst = {&ttTable4}.r-Rowid.
                            
                        /*--- Seta ponteiro do browse no registro correto ---*/
                        BROWSE {&page4browse}:SELECT-FOCUSED-ROW().
                    END.
                    ELSE
                        ASSIGN rFirst = ?.
                        
                    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                    &IF DEFINED(DBOVersion) <> 0 &THEN
                        /*--- Retorna registros do DBO filho, na temp-table ttTable4Aux ---*/
                        RUN serverSendRows IN {&hDBOTable4} (INPUT ?,
                                                           INPUT STRING(rFirst),
                                                           INPUT IF rFirst = ? THEN NO ELSE YES,
                                                           INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                                 &ELSE 40 &endif,
                                                           OUTPUT iRowsReturned,
                                                           OUTPUT TABLE ttTable4Aux).
                    &ELSE
                        /*--- Retorna todos os registros do DBO filho, na temp-table ttTable4Aux ---*/
                        RUN getBatchRecordsPrev IN {&hDBOTable4} (INPUT rFirst,
                                                                INPUT IF rFirst = ? THEN NO ELSE YES,
                                                                INPUT ?,
                                                                OUTPUT iRowsReturned,
                                                                OUTPUT TABLE ttTable4Aux).
                    &ENDIF
                        
                    /*--- Cancela trigger caso n∆o existam mais registros no DBO filho ---*/
                    IF CAN-FIND(FIRST ttTable4Aux) THEN DO:
    
                        /*--- Transfere dados da temp-table ttTable4Aux para a temp-table {&ttTable4} ---*/
                        &IF  "{&ROW-NUM-DEFINED4}":U = "YES":U  &THEN
                            ASSIGN iFirstSeq = 0.
                            FOR LAST {&ttTable4} BY {&ttTable4}.RowNum:
                                /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                                ASSIGN iFirstSeq = {&ttTable4}.RowNum.
                            END.
                        &ENDIF
                        FOR EACH ttTable4Aux:
                            CREATE {&ttTable4}.
                            &IF  "{&ROW-NUM-DEFINED4}":U = "YES":U  &THEN
                                ASSIGN {&ttTable4}.RowNum = {&ttTable4}.RowNum + iFirstSeq
                                               iFirstSeq = {&ttTable4}.RowNum. 
                                /*--- Acrescentado incremento do iFirstSeq para que na pr¢xima 
                                        iteraá∆o do For Each n∆o repita o registro. ---*/    
                                BUFFER-COPY ttTable4Aux except RowNum TO {&ttTable4}.
                            &ELSE
                                BUFFER-COPY ttTable4Aux TO {&ttTable4}.
                            &ENDIF
                            ASSIGN rLastCreated = ROWID({&ttTable4}).
                        END.
                        
                        /*--- Abre o browse filho, para atualizaá∆o dos dados ---*/
                        {&OPEN-QUERY-{&page4browse}}
                        
                        /*--- Seta view-port do browse filho para reposicionamento ---*/
                        {&page4browse}:SET-REPOSITIONED-ROW({&page4browse}:DOWN IN FRAME fPage4) IN FRAME fPage4.
                        
                        /*--- Reposiciona browse filho no £ltimo registro ---*/
                        REPOSITION {&page4browse} TO ROWID rLastCreated NO-ERROR.
                        GET NEXT {&page4browse} NO-LOCK.
                        
                        /*--- Posiciona browse filho no pr¢ximo registro ---*/
                        {&page4browse}:SELECT-FOCUSED-ROW().
                    END. /*--- END DO CAN-FIND {&ttTable4} ---*/
                END.
            &ENDIF

            
            &IF "{&page5browse}":U <> "":U &THEN
                WHEN 5 THEN DO:
                    /*--- Seta vari†vel rFirst com o valor do primeiro rowid 
                          da tabela {&ttTable5} existente no browse               ---*/
                    GET FIRST {&page5browse} NO-LOCK.
                    IF AVAILABLE {&ttTable5} THEN DO:
                        ASSIGN rFirst = {&ttTable5}.r-Rowid.
                            
                        /*--- Seta ponteiro do browse no registro correto ---*/
                        BROWSE {&page5browse}:SELECT-FOCUSED-ROW().
                    END.
                    ELSE
                        ASSIGN rFirst = ?.
                        
                    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                    &IF DEFINED(DBOVersion) <> 0 &THEN
                        /*--- Retorna registros do DBO filho, na temp-table ttTable5Aux ---*/
                        RUN serverSendRows IN {&hDBOTable5} (INPUT ?,
                                                           INPUT STRING(rFirst),
                                                           INPUT IF rFirst = ? THEN NO ELSE YES,
                                                           INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                                 &ELSE 40 &endif,
                                                           OUTPUT iRowsReturned,
                                                           OUTPUT TABLE ttTable5Aux).
                    &ELSE
                        /*--- Retorna todos os registros do DBO filho, na temp-table ttTable5Aux ---*/
                        RUN getBatchRecordsPrev IN {&hDBOTable5} (INPUT rFirst,
                                                                INPUT IF rFirst = ? THEN NO ELSE YES,
                                                                INPUT ?,
                                                                OUTPUT iRowsReturned,
                                                                OUTPUT TABLE ttTable5Aux).
                    &ENDIF
                        
                    /*--- Cancela trigger caso n∆o existam mais registros no DBO filho ---*/
                    IF CAN-FIND(FIRST ttTable5Aux) THEN DO:
    
                        /*--- Transfere dados da temp-table ttTable5Aux para a temp-table {&ttTable5} ---*/
                        &IF  "{&ROW-NUM-DEFINED5}":U = "YES":U  &THEN
                            ASSIGN iFirstSeq = 0.
                            FOR LAST {&ttTable5} BY {&ttTable5}.RowNum:
                                /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                                ASSIGN iFirstSeq = {&ttTable5}.RowNum.
                            END.
                        &ENDIF
                        FOR EACH ttTable5Aux:
                            CREATE {&ttTable5}.
                            &IF  "{&ROW-NUM-DEFINED5}":U = "YES":U  &THEN
                                ASSIGN {&ttTable5}.RowNum = {&ttTable5}.RowNum + iFirstSeq
                                               iFirstSeq = {&ttTable5}.RowNum. 
                                /*--- Acrescentado incremento do iFirstSeq para que na pr¢xima 
                                        iteraá∆o do For Each n∆o repita o registro. ---*/    
                                BUFFER-COPY ttTable5Aux except RowNum TO {&ttTable5}.
                            &ELSE
                                BUFFER-COPY ttTable5Aux TO {&ttTable5}.
                            &ENDIF
                            ASSIGN rLastCreated = ROWID({&ttTable5}).
                        END.
                        
                        /*--- Abre o browse filho, para atualizaá∆o dos dados ---*/
                        {&OPEN-QUERY-{&page5browse}}
                        
                        /*--- Seta view-port do browse filho para reposicionamento ---*/
                        {&page5browse}:SET-REPOSITIONED-ROW({&page5browse}:DOWN IN FRAME fPage5) IN FRAME fPage5.
                        
                        /*--- Reposiciona browse filho no £ltimo registro ---*/
                        REPOSITION {&page5browse} TO ROWID rLastCreated NO-ERROR.
                        GET NEXT {&page5browse} NO-LOCK.
                        
                        /*--- Posiciona browse filho no pr¢ximo registro ---*/
                        {&page5browse}:SELECT-FOCUSED-ROW().
                    END. /*--- END DO CAN-FIND {&ttTable5} ---*/
                END.
            &ENDIF

            
            &IF "{&page6browse}":U <> "":U &THEN
                WHEN 6 THEN DO:
                    /*--- Seta vari†vel rFirst com o valor do primeiro rowid 
                          da tabela {&ttTable6} existente no browse               ---*/
                    GET FIRST {&page6browse} NO-LOCK.
                    IF AVAILABLE {&ttTable6} THEN DO:
                        ASSIGN rFirst = {&ttTable6}.r-Rowid.
                            
                        /*--- Seta ponteiro do browse no registro correto ---*/
                        BROWSE {&page6browse}:SELECT-FOCUSED-ROW().
                    END.
                    ELSE
                        ASSIGN rFirst = ?.
                        
                    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                    &IF DEFINED(DBOVersion) <> 0 &THEN
                        /*--- Retorna registros do DBO filho, na temp-table ttTable6Aux ---*/
                        RUN serverSendRows IN {&hDBOTable6} (INPUT ?,
                                                           INPUT STRING(rFirst),
                                                           INPUT IF rFirst = ? THEN NO ELSE YES,
                                                           INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                                 &ELSE 40 &endif,
                                                           OUTPUT iRowsReturned,
                                                           OUTPUT TABLE ttTable6Aux).
                    &ELSE
                        /*--- Retorna todos os registros do DBO filho, na temp-table ttTable6Aux ---*/
                        RUN getBatchRecordsPrev IN {&hDBOTable6} (INPUT rFirst,
                                                                INPUT IF rFirst = ? THEN NO ELSE YES,
                                                                INPUT ?,
                                                                OUTPUT iRowsReturned,
                                                                OUTPUT TABLE ttTable6Aux).
                    &ENDIF
                        
                    /*--- Cancela trigger caso n∆o existam mais registros no DBO filho ---*/
                    IF CAN-FIND(FIRST ttTable6Aux) THEN DO:
    
                        /*--- Transfere dados da temp-table ttTable6Aux para a temp-table {&ttTable6} ---*/
                        &IF  "{&ROW-NUM-DEFINED6}":U = "YES":U  &THEN
                            ASSIGN iFirstSeq = 0.
                            FOR LAST {&ttTable6} BY {&ttTable6}.RowNum:
                                /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                                ASSIGN iFirstSeq = {&ttTable6}.RowNum.
                            END.
                        &ENDIF
                        FOR EACH ttTable6Aux:
                            CREATE {&ttTable6}.
                            &IF  "{&ROW-NUM-DEFINED6}":U = "YES":U  &THEN
                                ASSIGN {&ttTable6}.RowNum = {&ttTable6}.RowNum + iFirstSeq
                                               iFirstSeq = {&ttTable6}.RowNum. 
                                /*--- Acrescentado incremento do iFirstSeq para que na pr¢xima 
                                        iteraá∆o do For Each n∆o repita o registro. ---*/    
                                BUFFER-COPY ttTable6Aux except RowNum TO {&ttTable6}.
                            &ELSE
                                BUFFER-COPY ttTable6Aux TO {&ttTable6}.
                            &ENDIF
                            ASSIGN rLastCreated = ROWID({&ttTable6}).
                        END.
                        
                        /*--- Abre o browse filho, para atualizaá∆o dos dados ---*/
                        {&OPEN-QUERY-{&page6browse}}
                        
                        /*--- Seta view-port do browse filho para reposicionamento ---*/
                        {&page6browse}:SET-REPOSITIONED-ROW({&page6browse}:DOWN IN FRAME fPage6) IN FRAME fPage6.
                        
                        /*--- Reposiciona browse filho no £ltimo registro ---*/
                        REPOSITION {&page6browse} TO ROWID rLastCreated NO-ERROR.
                        GET NEXT {&page6browse} NO-LOCK.
                        
                        /*--- Posiciona browse filho no pr¢ximo registro ---*/
                        {&page6browse}:SELECT-FOCUSED-ROW().
                    END. /*--- END DO CAN-FIND {&ttTable6} ---*/
                END.
            &ENDIF

            
            &IF "{&page7browse}":U <> "":U &THEN
                WHEN 7 THEN DO:
                    /*--- Seta vari†vel rFirst com o valor do primeiro rowid 
                          da tabela {&ttTable7} existente no browse               ---*/
                    GET FIRST {&page7browse} NO-LOCK.
                    IF AVAILABLE {&ttTable7} THEN DO:
                        ASSIGN rFirst = {&ttTable7}.r-Rowid.
                            
                        /*--- Seta ponteiro do browse no registro correto ---*/
                        BROWSE {&page7browse}:SELECT-FOCUSED-ROW().
                    END.
                    ELSE
                        ASSIGN rFirst = ?.
                        
                    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                    &IF DEFINED(DBOVersion) <> 0 &THEN
                        /*--- Retorna registros do DBO filho, na temp-table ttTable7Aux ---*/
                        RUN serverSendRows IN {&hDBOTable7} (INPUT ?,
                                                           INPUT STRING(rFirst),
                                                           INPUT IF rFirst = ? THEN NO ELSE YES,
                                                           INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                                 &ELSE 40 &endif,
                                                           OUTPUT iRowsReturned,
                                                           OUTPUT TABLE ttTable7Aux).
                    &ELSE
                        /*--- Retorna todos os registros do DBO filho, na temp-table ttTable7Aux ---*/
                        RUN getBatchRecordsPrev IN {&hDBOTable7} (INPUT rFirst,
                                                                INPUT IF rFirst = ? THEN NO ELSE YES,
                                                                INPUT ?,
                                                                OUTPUT iRowsReturned,
                                                                OUTPUT TABLE ttTable7Aux).
                    &ENDIF
                        
                    /*--- Cancela trigger caso n∆o existam mais registros no DBO filho ---*/
                    IF CAN-FIND(FIRST ttTable7Aux) THEN DO:
    
                        /*--- Transfere dados da temp-table ttTable7Aux para a temp-table {&ttTable7} ---*/
                        &IF  "{&ROW-NUM-DEFINED7}":U = "YES":U  &THEN
                            ASSIGN iFirstSeq = 0.
                            FOR LAST {&ttTable7} BY {&ttTable7}.RowNum:
                                /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                                ASSIGN iFirstSeq = {&ttTable7}.RowNum.
                            END.
                        &ENDIF
                        FOR EACH ttTable7Aux:
                            CREATE {&ttTable7}.
                            &IF  "{&ROW-NUM-DEFINED7}":U = "YES":U  &THEN
                                ASSIGN {&ttTable7}.RowNum = {&ttTable7}.RowNum + iFirstSeq
                                               iFirstSeq = {&ttTable7}.RowNum. 
                                /*--- Acrescentado incremento do iFirstSeq para que na pr¢xima 
                                        iteraá∆o do For Each n∆o repita o registro. ---*/    
                                BUFFER-COPY ttTable7Aux except RowNum TO {&ttTable7}.
                            &ELSE
                                BUFFER-COPY ttTable7Aux TO {&ttTable7}.
                            &ENDIF
                            ASSIGN rLastCreated = ROWID({&ttTable7}).
                        END.
                        
                        /*--- Abre o browse filho, para atualizaá∆o dos dados ---*/
                        {&OPEN-QUERY-{&page7browse}}
                        
                        /*--- Seta view-port do browse filho para reposicionamento ---*/
                        {&page7browse}:SET-REPOSITIONED-ROW({&page7browse}:DOWN IN FRAME fPage7) IN FRAME fPage7.
                        
                        /*--- Reposiciona browse filho no £ltimo registro ---*/
                        REPOSITION {&page7browse} TO ROWID rLastCreated NO-ERROR.
                        GET NEXT {&page7browse} NO-LOCK.
                        
                        /*--- Posiciona browse filho no pr¢ximo registro ---*/
                        {&page7browse}:SELECT-FOCUSED-ROW().
                    END. /*--- END DO CAN-FIND {&ttTable7} ---*/
                END.
            &ENDIF

            
            &IF "{&page8browse}":U <> "":U &THEN
                WHEN 8 THEN DO:
                    /*--- Seta vari†vel rFirst com o valor do primeiro rowid 
                          da tabela {&ttTable8} existente no browse               ---*/
                    GET FIRST {&page8browse} NO-LOCK.
                    IF AVAILABLE {&ttTable8} THEN DO:
                        ASSIGN rFirst = {&ttTable8}.r-Rowid.
                            
                        /*--- Seta ponteiro do browse no registro correto ---*/
                        BROWSE {&page8browse}:SELECT-FOCUSED-ROW().
                    END.
                    ELSE
                        ASSIGN rFirst = ?.
                        
                    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                    &IF DEFINED(DBOVersion) <> 0 &THEN
                        /*--- Retorna registros do DBO filho, na temp-table ttTable8Aux ---*/
                        RUN serverSendRows IN {&hDBOTable8} (INPUT ?,
                                                           INPUT STRING(rFirst),
                                                           INPUT IF rFirst = ? THEN NO ELSE YES,
                                                           INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                                 &ELSE 40 &endif,
                                                           OUTPUT iRowsReturned,
                                                           OUTPUT TABLE ttTable8Aux).
                    &ELSE
                        /*--- Retorna todos os registros do DBO filho, na temp-table ttTable8Aux ---*/
                        RUN getBatchRecordsPrev IN {&hDBOTable8} (INPUT rFirst,
                                                                INPUT IF rFirst = ? THEN NO ELSE YES,
                                                                INPUT ?,
                                                                OUTPUT iRowsReturned,
                                                                OUTPUT TABLE ttTable8Aux).
                    &ENDIF
                        
                    /*--- Cancela trigger caso n∆o existam mais registros no DBO filho ---*/
                    IF CAN-FIND(FIRST ttTable8Aux) THEN DO:
    
                        /*--- Transfere dados da temp-table ttTable8Aux para a temp-table {&ttTable8} ---*/
                        &IF  "{&ROW-NUM-DEFINED8}":U = "YES":U  &THEN
                            ASSIGN iFirstSeq = 0.
                            FOR LAST {&ttTable8} BY {&ttTable8}.RowNum:
                                /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                                ASSIGN iFirstSeq = {&ttTable8}.RowNum.
                            END.
                        &ENDIF
                        FOR EACH ttTable8Aux:
                            CREATE {&ttTable8}.
                            &IF  "{&ROW-NUM-DEFINED8}":U = "YES":U  &THEN
                                ASSIGN {&ttTable8}.RowNum = {&ttTable8}.RowNum + iFirstSeq
                                               iFirstSeq = {&ttTable8}.RowNum. 
                                /*--- Acrescentado incremento do iFirstSeq para que na pr¢xima 
                                        iteraá∆o do For Each n∆o repita o registro. ---*/    
                                BUFFER-COPY ttTable8Aux except RowNum TO {&ttTable8}.
                            &ELSE
                                BUFFER-COPY ttTable8Aux TO {&ttTable8}.
                            &ENDIF
                            ASSIGN rLastCreated = ROWID({&ttTable8}).
                        END.
                        
                        /*--- Abre o browse filho, para atualizaá∆o dos dados ---*/
                        {&OPEN-QUERY-{&page8browse}}
                        
                        /*--- Seta view-port do browse filho para reposicionamento ---*/
                        {&page8browse}:SET-REPOSITIONED-ROW({&page8browse}:DOWN IN FRAME fPage8) IN FRAME fPage8.
                        
                        /*--- Reposiciona browse filho no £ltimo registro ---*/
                        REPOSITION {&page8browse} TO ROWID rLastCreated NO-ERROR.
                        GET NEXT {&page8browse} NO-LOCK.
                        
                        /*--- Posiciona browse filho no pr¢ximo registro ---*/
                        {&page8browse}:SELECT-FOCUSED-ROW().
                    END. /*--- END DO CAN-FIND {&ttTable8} ---*/
                END.
            &ENDIF
    
        END CASE.
    
    
    &ENDIF /*FIM DO TESTE DO PREPROCESSADOR &DBOVERSION(VERS«O DO DBO)*/
    /*FIM ALTERAÄ«O ANDERSON (TECH485) 04/04/2001*/
    
    
    /*--- Seta vari†vel cEventBrowse com o valor "" ---*/
    ASSIGN cEventBrowse = "":U.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-applyOffEnd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE applyOffEnd Method-Library 
PROCEDURE applyOffEnd :
/*------------------------------------------------------------------------------
  Purpose:     L¢gica para trigger de OFF-END
  Parameters:  recebe o n£mero da p†gina
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pPageNumber AS INTEGER NO-UNDO.
    
    
    DEFINE VARIABLE rLast AS ROWID NO-UNDO.
    DEFINE VARIABLE rReposition AS ROWID NO-UNDO.
    
    /*--- Testa o pre-processador RowNumDefinedN para todas as p†ginas para
          definir iLastSeq que Ç utilizada no c†lculo do valor do rowNum    ---*/
    &IF  "{&ROW-NUM-DEFINED1}":U = "YES":U  or
         "{&ROW-NUM-DEFINED2}":U = "YES":U  or
         "{&ROW-NUM-DEFINED3}":U = "YES":U  or
         "{&ROW-NUM-DEFINED4}":U = "YES":U  or
         "{&ROW-NUM-DEFINED5}":U = "YES":U  or
         "{&ROW-NUM-DEFINED6}":U = "YES":U  or
         "{&ROW-NUM-DEFINED7}":U = "YES":U  or
         "{&ROW-NUM-DEFINED8}":U = "YES":U  &THEN
        DEFINE VARIABLE iLastSeq AS INTEGER NO-UNDO INIT 0.
    &ENDIF

    /*--- Tratar evento de SCROLL-NOTIFY do Browse ---*/
    IF cEventBrowse = "":U THEN
        ASSIGN cEventBrowse = "SCROLL-NOTIFY":U.
    
    CASE pPageNumber:
        &IF "{&page1browse}":U <> "":U &THEN
            WHEN 1 THEN DO WITH FRAME fPage1:
                /*--- Seta vari†vel rLast com o valor do £ltimo rowid 
                      da tabela {&ttTable1} existente no browse            ---*/
                GET LAST {&page1browse} NO-LOCK.
                IF AVAILABLE {&ttTable1} THEN
                    ASSIGN rLast = {&ttTable1}.r-Rowid
                           rReposition = rowid({&ttTable1}).
                ELSE
                    ASSIGN rLast = ?
                           rReposition = ?.
                
                IF  {&page1browse}:MULTIPLE AND rReposition <> ? THEN
                    ASSIGN rCurrent1 = rReposition.
                
                /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                &IF DEFINED(DBOVersion) <> 0 &THEN
                    /*--- Retorna registros do DBO filho, na temp-table ttTable1Aux ---*/
                    RUN serverSendRows IN {&hDBOTable1} (INPUT ?,
                                                       INPUT STRING(rLast),
                                                       INPUT IF rLast = ? THEN NO ELSE YES,
                                                       INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                             &ELSE 40 &endif,
                                                       OUTPUT iRowsReturned,
                                                       OUTPUT TABLE ttTable1Aux).
                &ELSE
                    /*--- Retorna registros do DBO filho, na temp-table ttTable1Aux ---*/
                    RUN getBatchRecords IN {&hDBOTable1} (INPUT rLast,
                                                        INPUT IF rLast = ? THEN NO ELSE YES,
                                                        INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                              &ELSE 40 &endif,
                                                        OUTPUT iRowsReturned,
                                                        OUTPUT TABLE ttTable1Aux).
                &ENDIF
                
                /*--- Posiciona no £ltimo registro caso n∆o existam mais registros no DBO filho ---*/
                IF CAN-FIND(FIRST ttTable1Aux) THEN DO:
                    /*--- Transfere dados da temp-table ttTable1Aux 
                          para a temp-table {&ttTable1}   ---*/
                    &IF  "{&ROW-NUM-DEFINED1}":U = "YES":U  &THEN
                        ASSIGN iLastSeq = 0.
                        FOR LAST {&ttTable1} BY {&ttTable1}.RowNum:
                            /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                            ASSIGN iLastSeq = {&ttTable1}.RowNum.
                        END.
                    &ENDIF
                    FOR EACH ttTable1Aux:
                        CREATE {&ttTable1}.

                        &IF  "{&ROW-NUM-DEFINED1}":U = "YES":U  &THEN
                            ASSIGN {&ttTable1}.RowNum = {&ttTable1}.RowNum + iLastSeq
                                           iLastSeq = {&ttTable1}.RowNum. 
                            /*--- Acrescentado incremento do iLastSeq para que na pr¢xima 
                                    iteraá∆o do For Each n∆o repita o registro. ---*/    
                            BUFFER-COPY ttTable1Aux except RowNum TO {&ttTable1}.
                        &ELSE
                            BUFFER-COPY ttTable1Aux TO {&ttTable1}.
                        &ENDIF
                    END.

                    /*--- Abre o browse filho, para atualizaªío dos dados ---*/
                    {&OPEN-QUERY-{&page1browse}}
                END.
                
                /*--- Posiciona browse, no pr¢ximo registro ---*/
                CASE cEventBrowse:
                    WHEN "PAGE-DOWN":U THEN DO:
                        /*--- Seta view-port do browse para reposicionamento ---*/
                        {&page1browse}:SET-REPOSITIONED-ROW(1).
                        
                        IF  rCurrent1 <> ? THEN DO:
                            /*--- Reposiciona browse no registro atual ---*/
                            REPOSITION {&page1browse} TO ROWID rCurrent1 NO-ERROR.
                            GET NEXT {&page1browse} NO-LOCK.
                            
                            /*--- Seleciona o £ltimo registro do view-port do browse ---*/
                            {&page1browse}:SELECT-ROW({&page1browse}:NUM-ITERATIONS IN FRAME fPage1).
                        END.
                    END.
                    OTHERWISE DO:
                        /*--- Seta view-port do browse para reposicionamento ---*/
                        {&page1browse}:SET-REPOSITIONED-ROW({&page1browse}:DOWN IN FRAME fPage1 - 1) IN FRAME fPage1. 
                        
                        IF  rReposition <> ? THEN DO:
                            /*--- Reposiciona browse no registro atual ---*/
                            REPOSITION {&page1browse} TO ROWID rReposition NO-ERROR.
                            GET NEXT {&page1browse} NO-LOCK.
                            
                            /*--- Posiciona browse no pr¢ximo registro ---*/
                            {&page1browse}:SELECT-FOCUSED-ROW().
                            {&page1browse}:SELECT-NEXT-ROW().
                        END.
                    END.
                END CASE.
            END.
        &ENDIF
        
        &IF "{&page2browse}":U <> "":U &THEN
            WHEN 2 THEN DO WITH FRAME fPage2:
                /*--- Seta vari†vel rLast com o valor do £ltimo rowid 
                      da tabela {&ttTable2} existente no browse            ---*/
                GET LAST {&page2browse} NO-LOCK.
                IF AVAILABLE {&ttTable2} THEN
                    ASSIGN rLast       = {&ttTable2}.r-Rowid
                           rReposition = rowid({&ttTable2}).
                ELSE
                    ASSIGN rLast       = ?
                           rReposition = ?.
                
                IF  {&page2browse}:MULTIPLE AND rReposition <> ? THEN
                    ASSIGN rCurrent2 = rReposition.
                
                /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                &IF DEFINED(DBOVersion) <> 0 &THEN
                    /*--- Retorna registros do DBO filho, na temp-table ttTable2Aux ---*/
                    RUN serverSendRows IN {&hDBOTable2} (INPUT ?,
                                                       INPUT STRING(rLast),
                                                       INPUT IF rLast = ? THEN NO ELSE YES,
                                                       INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                             &ELSE 40 &endif,
                                                       OUTPUT iRowsReturned,
                                                       OUTPUT TABLE ttTable2Aux).
                &ELSE
                    /*--- Retorna registros do DBO filho, na temp-table ttTable2Aux ---*/
                    RUN getBatchRecords IN {&hDBOTable2} (INPUT rLast,
                                                        INPUT IF rLast = ? THEN NO ELSE YES,
                                                        INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                              &ELSE 40 &endif,
                                                        OUTPUT iRowsReturned,
                                                        OUTPUT TABLE ttTable2Aux).
                &ENDIF
                
                /*--- Posiciona no £ltimo registro caso n∆o existam mais registros no DBO filho ---*/
                IF CAN-FIND(FIRST ttTable2Aux) THEN DO:
                    /*--- Transfere dados da temp-table ttTable2Aux 
                          para a temp-table {&ttTable2}   ---*/
                    &IF  "{&ROW-NUM-DEFINED2}":U = "YES":U  &THEN
                        ASSIGN iLastSeq = 0.
                        FOR LAST {&ttTable2} BY {&ttTable2}.RowNum:
                            /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                            ASSIGN iLastSeq = {&ttTable2}.RowNum.
                        END.
                    &ENDIF
                    FOR EACH ttTable2Aux:
                        CREATE {&ttTable2}.

                        &IF  "{&ROW-NUM-DEFINED2}":U = "YES":U  &THEN
                            ASSIGN {&ttTable2}.RowNum = {&ttTable2}.RowNum + iLastSeq
                                           iLastSeq = {&ttTable2}.RowNum. 
                            /*--- Acrescentado incremento do iLastSeq para que na pr¢xima 
                                    iteraá∆o do For Each n∆o repita o registro. ---*/    
                            BUFFER-COPY ttTable2Aux except RowNum TO {&ttTable2}.
                        &ELSE
                            BUFFER-COPY ttTable2Aux TO {&ttTable2}.
                        &ENDIF
                    END.

                    /*--- Abre o browse filho, para atualizaªío dos dados ---*/
                    {&OPEN-QUERY-{&page2browse}}
                END.
                
                /*--- Posiciona browse, no pr¢ximo registro ---*/
                CASE cEventBrowse:
                    WHEN "PAGE-DOWN":U THEN DO:
                        /*--- Seta view-port do browse para reposicionamento ---*/
                        {&page2browse}:SET-REPOSITIONED-ROW(1).
                        
                        IF  rCurrent2 <> ? THEN DO:
                            /*--- Reposiciona browse no registro atual ---*/
                            REPOSITION {&page2browse} TO ROWID rCurrent2 NO-ERROR.
                            GET NEXT {&page2browse} NO-LOCK.
                            
                            /*--- Seleciona o £ltimo registro do view-port do browse ---*/
                            {&page2browse}:SELECT-ROW({&page2browse}:NUM-ITERATIONS IN FRAME fPage2).
                        END.
                    END.
                    OTHERWISE DO:
                        /*--- Seta view-port do browse para reposicionamento ---*/
                        {&page2browse}:SET-REPOSITIONED-ROW({&page2browse}:DOWN IN FRAME fPage2 - 1) IN FRAME fPage2.
                        
                        IF  rReposition <> ? THEN DO:
                            /*--- Reposiciona browse no registro atual ---*/
                            REPOSITION {&page2browse} TO ROWID rReposition NO-ERROR.
                            GET NEXT {&page2browse} NO-LOCK.
                            
                            /*--- Posiciona browse no pr¢ximo registro ---*/
                            {&page2browse}:SELECT-FOCUSED-ROW().
                            {&page2browse}:SELECT-NEXT-ROW().
                        END.
                    END.
                END CASE.
            END.
        &ENDIF
        
        &IF "{&page3browse}":U <> "":U &THEN
            WHEN 3 THEN DO WITH FRAME fPage3:
                /*--- Seta vari†vel rLast com o valor do £ltimo rowid 
                      da tabela {&ttTable3} existente no browse            ---*/
                GET LAST {&page3browse} NO-LOCK.
                IF AVAILABLE {&ttTable3} THEN
                    ASSIGN rLast       = {&ttTable3}.r-Rowid
                           rReposition = rowid({&ttTable3}).
                ELSE
                    ASSIGN rLast       = ?
                           rReposition = ?.
                
                IF  {&page3browse}:MULTIPLE AND rReposition <> ? THEN
                    ASSIGN rCurrent3 = rReposition.
                
                /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                &IF DEFINED(DBOVersion) <> 0 &THEN
                    /*--- Retorna registros do DBO filho, na temp-table ttTable3Aux ---*/
                    RUN serverSendRows IN {&hDBOTable3} (INPUT ?,
                                                       INPUT STRING(rLast),
                                                       INPUT IF rLast = ? THEN NO ELSE YES,
                                                       INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                             &ELSE 40 &endif,
                                                       OUTPUT iRowsReturned,
                                                       OUTPUT TABLE ttTable3Aux).
                &ELSE
                    /*--- Retorna registros do DBO filho, na temp-table ttTable3Aux ---*/
                    RUN getBatchRecords IN {&hDBOTable3} (INPUT rLast,
                                                        INPUT IF rLast = ? THEN NO ELSE YES,
                                                        INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                              &ELSE 40 &endif,
                                                        OUTPUT iRowsReturned,
                                                        OUTPUT TABLE ttTable3Aux).
                &ENDIF
                
                /*--- Posiciona no £ltimo registro caso n∆o existam mais registros no DBO filho ---*/
                IF CAN-FIND(FIRST ttTable3Aux) THEN DO:
                    /*--- Transfere dados da temp-table ttTable3Aux 
                          para a temp-table {&ttTable3}   ---*/
                    &IF  "{&ROW-NUM-DEFINED3}":U = "YES":U  &THEN
                        ASSIGN iLastSeq = 0.
                        FOR LAST {&ttTable3} BY {&ttTable3}.RowNum:
                            /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                            ASSIGN iLastSeq = {&ttTable3}.RowNum.
                        END.
                    &ENDIF
                    FOR EACH ttTable3Aux:
                        CREATE {&ttTable3}.

                        &IF  "{&ROW-NUM-DEFINED3}":U = "YES":U  &THEN
                            ASSIGN {&ttTable3}.RowNum = {&ttTable3}.RowNum + iLastSeq
                                           iLastSeq = {&ttTable3}.RowNum. 
                            /*--- Acrescentado incremento do iLastSeq para que na pr¢xima 
                                    iteraá∆o do For Each n∆o repita o registro. ---*/    
                            BUFFER-COPY ttTable3Aux except RowNum TO {&ttTable3}.
                        &ELSE
                            BUFFER-COPY ttTable3Aux TO {&ttTable3}.
                        &ENDIF
                    END.

                    /*--- Abre o browse filho, para atualizaªío dos dados ---*/
                    {&OPEN-QUERY-{&page3browse}}
                END.
                
                /*--- Posiciona browse, no pr¢ximo registro ---*/
                CASE cEventBrowse:
                    WHEN "PAGE-DOWN":U THEN DO:
                        /*--- Seta view-port do browse para reposicionamento ---*/
                        {&page3browse}:SET-REPOSITIONED-ROW(1).
                        
                        IF  rCurrent3 <> ? THEN DO:
                            /*--- Reposiciona browse no registro atual ---*/
                            REPOSITION {&page3browse} TO ROWID rCurrent3 NO-ERROR.
                            GET NEXT {&page3browse} NO-LOCK.
                            
                            /*--- Seleciona o £ltimo registro do view-port do browse ---*/
                            {&page3browse}:SELECT-ROW({&page3browse}:NUM-ITERATIONS IN FRAME fPage3).
                        END.
                    END.
                    OTHERWISE DO:
                        /*--- Seta view-port do browse para reposicionamento ---*/
                        {&page3browse}:SET-REPOSITIONED-ROW({&page3browse}:DOWN IN FRAME fPage3 - 1) IN FRAME fPage3.
                        
                        IF  rReposition <> ? THEN DO:
                            /*--- Reposiciona browse no registro atual ---*/
                            REPOSITION {&page3browse} TO ROWID rReposition NO-ERROR.
                            GET NEXT {&page3browse} NO-LOCK.
                            
                            /*--- Posiciona browse no pr¢ximo registro ---*/
                            {&page3browse}:SELECT-FOCUSED-ROW().
                            {&page3browse}:SELECT-NEXT-ROW().
                        END.
                    END.
                END CASE.
            END.
        &ENDIF
        
        &IF "{&page4browse}":U <> "":U &THEN
            WHEN 4 THEN DO WITH FRAME fPage4:
                /*--- Seta vari†vel rLast com o valor do £ltimo rowid 
                      da tabela {&ttTable4} existente no browse            ---*/
                GET LAST {&page4browse} NO-LOCK.
                IF AVAILABLE {&ttTable4} THEN
                    ASSIGN rLast       = {&ttTable4}.r-Rowid
                           rReposition = rowid({&ttTable4}).
                ELSE
                    ASSIGN rLast       = ? 
                           rReposition = ?.
                
                IF  {&page4browse}:MULTIPLE AND rReposition <> ? THEN
                    ASSIGN rCurrent4 = rReposition.
                
                /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                &IF DEFINED(DBOVersion) <> 0 &THEN
                    /*--- Retorna registros do DBO filho, na temp-table ttTable4Aux ---*/
                    RUN serverSendRows IN {&hDBOTable4} (INPUT ?,
                                                       INPUT STRING(rLast),
                                                       INPUT IF rLast = ? THEN NO ELSE YES,
                                                       INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                             &ELSE 40 &endif,
                                                       OUTPUT iRowsReturned,
                                                       OUTPUT TABLE ttTable4Aux).
                &ELSE
                    /*--- Retorna registros do DBO filho, na temp-table ttTable4Aux ---*/
                    RUN getBatchRecords IN {&hDBOTable4} (INPUT rLast,
                                                        INPUT IF rLast = ? THEN NO ELSE YES,
                                                        INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                              &ELSE 40 &endif,
                                                        OUTPUT iRowsReturned,
                                                        OUTPUT TABLE ttTable4Aux).
                &ENDIF
                
                /*--- Posiciona no £ltimo registro caso n∆o existam mais registros no DBO filho ---*/
                IF CAN-FIND(FIRST ttTable4Aux) THEN DO:
                    /*--- Transfere dados da temp-table ttTable4Aux 
                          para a temp-table {&ttTable4}   ---*/
                    &IF  "{&ROW-NUM-DEFINED4}":U = "YES":U  &THEN
                        ASSIGN iLastSeq = 0.
                        FOR LAST {&ttTable4} BY {&ttTable4}.RowNum:
                            /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                            ASSIGN iLastSeq = {&ttTable4}.RowNum.
                        END.
                    &ENDIF
                    FOR EACH ttTable4Aux:
                        CREATE {&ttTable4}.

                        &IF  "{&ROW-NUM-DEFINED4}":U = "YES":U  &THEN
                            ASSIGN {&ttTable4}.RowNum = {&ttTable4}.RowNum + iLastSeq
                                           iLastSeq = {&ttTable4}.RowNum. 
                            /*--- Acrescentado incremento do iLastSeq para que na pr¢xima 
                                    iteraá∆o do For Each n∆o repita o registro. ---*/    
                            BUFFER-COPY ttTable4Aux except RowNum TO {&ttTable4}.
                        &ELSE
                            BUFFER-COPY ttTable4Aux TO {&ttTable4}.
                        &ENDIF
                    END.

                    /*--- Abre o browse filho, para atualizaªío dos dados ---*/
                    {&OPEN-QUERY-{&page4browse}}
                END.
                
                /*--- Posiciona browse, no pr¢ximo registro ---*/
                CASE cEventBrowse:
                    WHEN "PAGE-DOWN":U THEN DO:
                        /*--- Seta view-port do browse para reposicionamento ---*/
                        {&page4browse}:SET-REPOSITIONED-ROW(1).
                        
                        IF  rCurrent4 <> ? THEN DO:
                            /*--- Reposiciona browse no registro atual ---*/
                            REPOSITION {&page4browse} TO ROWID rCurrent4 NO-ERROR.
                            GET NEXT {&page4browse} NO-LOCK.
                            
                            /*--- Seleciona o £ltimo registro do view-port do browse ---*/
                            {&page4browse}:SELECT-ROW({&page4browse}:NUM-ITERATIONS IN FRAME fPage4).
                        END.
                    END.
                    OTHERWISE DO:
                        /*--- Seta view-port do browse para reposicionamento ---*/
                        {&page4browse}:SET-REPOSITIONED-ROW({&page4browse}:DOWN IN FRAME fPage4 - 1) IN FRAME fPage4.
                        
                        IF  rReposition <> ? THEN DO:
                            /*--- Reposiciona browse no registro atual ---*/
                            REPOSITION {&page4browse} TO ROWID rReposition NO-ERROR.
                            GET NEXT {&page4browse} NO-LOCK.
                            
                            /*--- Posiciona browse no pr¢ximo registro ---*/
                            {&page4browse}:SELECT-FOCUSED-ROW().
                            {&page4browse}:SELECT-NEXT-ROW().
                        END.
                    END.
                END CASE.
            END.
        &ENDIF
        
        &IF "{&page5browse}":U <> "":U &THEN
            WHEN 5 THEN DO WITH FRAME fPage5:
                /*--- Seta vari†vel rLast com o valor do £ltimo rowid 
                      da tabela {&ttTable5} existente no browse            ---*/
                GET LAST {&page5browse} NO-LOCK.
                IF AVAILABLE {&ttTable5} THEN
                    ASSIGN rLast       = {&ttTable5}.r-Rowid
                           rReposition = rowid({&ttTable5}).
                ELSE
                    ASSIGN rLast       = ?
                           rReposition = ?.
                
                IF  {&page5browse}:MULTIPLE AND rReposition <> ? THEN
                    ASSIGN rCurrent5 = rReposition.
                
                /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                &IF DEFINED(DBOVersion) <> 0 &THEN
                    /*--- Retorna registros do DBO filho, na temp-table ttTable5Aux ---*/
                    RUN serverSendRows IN {&hDBOTable5} (INPUT ?,
                                                       INPUT STRING(rLast),
                                                       INPUT IF rLast = ? THEN NO ELSE YES,
                                                       INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                             &ELSE 40 &endif,
                                                       OUTPUT iRowsReturned,
                                                       OUTPUT TABLE ttTable5Aux).
                &ELSE
                    /*--- Retorna registros do DBO filho, na temp-table ttTable5Aux ---*/
                    RUN getBatchRecords IN {&hDBOTable5} (INPUT rLast,
                                                        INPUT IF rLast = ? THEN NO ELSE YES,
                                                        INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                              &ELSE 40 &endif,
                                                        OUTPUT iRowsReturned,
                                                        OUTPUT TABLE ttTable5Aux).
                &ENDIF
                
                /*--- Posiciona no £ltimo registro caso n∆o existam mais registros no DBO filho ---*/
                IF CAN-FIND(FIRST ttTable5Aux) THEN DO:
                    /*--- Transfere dados da temp-table ttTable5Aux 
                          para a temp-table {&ttTable5}   ---*/
                    &IF  "{&ROW-NUM-DEFINED5}":U = "YES":U  &THEN
                        ASSIGN iLastSeq = 0.
                        FOR LAST {&ttTable5} BY {&ttTable5}.RowNum:
                            /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                            ASSIGN iLastSeq = {&ttTable5}.RowNum.
                        END.
                    &ENDIF
                    FOR EACH ttTable5Aux:
                        CREATE {&ttTable5}.

                        &IF  "{&ROW-NUM-DEFINED5}":U = "YES":U  &THEN
                            ASSIGN {&ttTable5}.RowNum = {&ttTable5}.RowNum + iLastSeq
                                           iLastSeq = {&ttTable5}.RowNum. 
                            /*--- Acrescentado incremento do iLastSeq para que na pr¢xima 
                                    iteraá∆o do For Each n∆o repita o registro. ---*/    
                            BUFFER-COPY ttTable5Aux except RowNum TO {&ttTable5}.
                        &ELSE
                            BUFFER-COPY ttTable5Aux TO {&ttTable5}.
                        &ENDIF
                    END.

                    /*--- Abre o browse filho, para atualizaªío dos dados ---*/
                    {&OPEN-QUERY-{&page5browse}}
                END.
                
                /*--- Posiciona browse, no pr¢ximo registro ---*/
                CASE cEventBrowse:
                    WHEN "PAGE-DOWN":U THEN DO:
                        /*--- Seta view-port do browse para reposicionamento ---*/
                        {&page5browse}:SET-REPOSITIONED-ROW(1).
                        
                        IF  rCurrent5 <> ? THEN DO:
                            /*--- Reposiciona browse no registro atual ---*/
                            REPOSITION {&page5browse} TO ROWID rCurrent5 NO-ERROR.
                            GET NEXT {&page5browse} NO-LOCK.
                            
                            /*--- Seleciona o £ltimo registro do view-port do browse ---*/
                            {&page5browse}:SELECT-ROW({&page5browse}:NUM-ITERATIONS IN FRAME fPage5).
                        END.
                    END.
                    OTHERWISE DO:
                        /*--- Seta view-port do browse para reposicionamento ---*/
                        {&page5browse}:SET-REPOSITIONED-ROW({&page5browse}:DOWN IN FRAME fPage5 - 1) IN FRAME fPage5.
                        
                        IF  rReposition <> ? THEN DO:
                            /*--- Reposiciona browse no registro atual ---*/
                            REPOSITION {&page5browse} TO ROWID rReposition NO-ERROR.
                            GET NEXT {&page5browse} NO-LOCK.
                            
                            /*--- Posiciona browse no pr¢ximo registro ---*/
                            {&page5browse}:SELECT-FOCUSED-ROW().
                            {&page5browse}:SELECT-NEXT-ROW().
                        END.
                    END.
                END CASE.
            END.
        &ENDIF
        
        &IF "{&page6browse}":U <> "":U &THEN
            WHEN 6 THEN DO WITH FRAME fPage6:
                /*--- Seta vari†vel rLast com o valor do £ltimo rowid 
                      da tabela {&ttTable6} existente no browse            ---*/
                GET LAST {&page6browse} NO-LOCK.
                IF AVAILABLE {&ttTable6} THEN
                    ASSIGN rLast       = {&ttTable6}.r-Rowid
                           rReposition = rowid({&ttTable6}).
                ELSE
                    ASSIGN rLast       = ?
                           rReposition = ?.
                
                IF  {&page6browse}:MULTIPLE AND rReposition <> ? THEN
                    ASSIGN rCurrent6 = rReposition.
                
                /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                &IF DEFINED(DBOVersion) <> 0 &THEN
                    /*--- Retorna registros do DBO filho, na temp-table ttTable6Aux ---*/
                    RUN serverSendRows IN {&hDBOTable6} (INPUT ?,
                                                       INPUT STRING(rLast),
                                                       INPUT IF rLast = ? THEN NO ELSE YES,
                                                       INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                             &ELSE 40 &endif,
                                                       OUTPUT iRowsReturned,
                                                       OUTPUT TABLE ttTable6Aux).
                &ELSE
                    /*--- Retorna registros do DBO filho, na temp-table ttTable6Aux ---*/
                    RUN getBatchRecords IN {&hDBOTable6} (INPUT rLast,
                                                        INPUT IF rLast = ? THEN NO ELSE YES,
                                                        INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                              &ELSE 40 &endif,
                                                        OUTPUT iRowsReturned,
                                                        OUTPUT TABLE ttTable6Aux).
                &ENDIF
                
                /*--- Posiciona no £ltimo registro caso n∆o existam mais registros no DBO filho ---*/
                IF CAN-FIND(FIRST ttTable6Aux) THEN DO:
                    /*--- Transfere dados da temp-table ttTable6Aux 
                          para a temp-table {&ttTable6}   ---*/
                    &IF  "{&ROW-NUM-DEFINED6}":U = "YES":U  &THEN
                        ASSIGN iLastSeq = 0.
                        FOR LAST {&ttTable6} BY {&ttTable6}.RowNum:
                            /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                            ASSIGN iLastSeq = {&ttTable6}.RowNum.
                        END.
                    &ENDIF
                    FOR EACH ttTable6Aux:
                        CREATE {&ttTable6}.

                        &IF  "{&ROW-NUM-DEFINED6}":U = "YES":U  &THEN
                            ASSIGN {&ttTable6}.RowNum = {&ttTable6}.RowNum + iLastSeq
                                           iLastSeq = {&ttTable6}.RowNum. 
                            /*--- Acrescentado incremento do iLastSeq para que na pr¢xima 
                                    iteraá∆o do For Each n∆o repita o registro. ---*/    
                            BUFFER-COPY ttTable6Aux except RowNum TO {&ttTable6}.
                        &ELSE
                            BUFFER-COPY ttTable6Aux TO {&ttTable6}.
                        &ENDIF
                    END.

                    /*--- Abre o browse filho, para atualizaªío dos dados ---*/
                    {&OPEN-QUERY-{&page6browse}}
                END.
                
                /*--- Posiciona browse, no pr¢ximo registro ---*/
                CASE cEventBrowse:
                    WHEN "PAGE-DOWN":U THEN DO:
                        /*--- Seta view-port do browse para reposicionamento ---*/
                        {&page6browse}:SET-REPOSITIONED-ROW(1).
                        
                        IF  rCurrent6 <> ? THEN DO:
                            /*--- Reposiciona browse no registro atual ---*/
                            REPOSITION {&page6browse} TO ROWID rCurrent6 NO-ERROR.
                            GET NEXT {&page6browse} NO-LOCK.
                            
                            /*--- Seleciona o £ltimo registro do view-port do browse ---*/
                            {&page6browse}:SELECT-ROW({&page6browse}:NUM-ITERATIONS IN FRAME fPage6).
                        END.
                    END.
                    OTHERWISE DO:
                        /*--- Seta view-port do browse para reposicionamento ---*/
                        {&page6browse}:SET-REPOSITIONED-ROW({&page6browse}:DOWN IN FRAME fPage6 - 1) IN FRAME fPage6.
                        
                        IF  rReposition <> ? THEN DO:
                            /*--- Reposiciona browse no registro atual ---*/
                            REPOSITION {&page6browse} TO ROWID rReposition NO-ERROR.
                            GET NEXT {&page6browse} NO-LOCK.
                            
                            /*--- Posiciona browse no pr¢ximo registro ---*/
                            {&page6browse}:SELECT-FOCUSED-ROW().
                            {&page6browse}:SELECT-NEXT-ROW().
                        END.
                    END.
                END CASE.
            END.
        &ENDIF
        
        &IF "{&page7browse}":U <> "":U &THEN
            WHEN 7 THEN DO WITH FRAME fPage7:
                /*--- Seta vari†vel rLast com o valor do £ltimo rowid 
                      da tabela {&ttTable7} existente no browse            ---*/
                GET LAST {&page7browse} NO-LOCK.
                IF AVAILABLE {&ttTable7} THEN
                    ASSIGN rLast       = {&ttTable7}.r-Rowid
                           rReposition = rowid({&ttTable7}).
                ELSE
                    ASSIGN rLast       = ?
                           rReposition = ?.
                
                IF  {&page7browse}:MULTIPLE AND rReposition <> ? THEN
                    ASSIGN rCurrent7 = rReposition.
                
                /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                &IF DEFINED(DBOVersion) <> 0 &THEN
                    /*--- Retorna registros do DBO filho, na temp-table ttTable7Aux ---*/
                    RUN serverSendRows IN {&hDBOTable7} (INPUT ?,
                                                       INPUT STRING(rLast),
                                                       INPUT IF rLast = ? THEN NO ELSE YES,
                                                       INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                             &ELSE 40 &endif,
                                                       OUTPUT iRowsReturned,
                                                       OUTPUT TABLE ttTable7Aux).
                &ELSE
                    /*--- Retorna registros do DBO filho, na temp-table ttTable7Aux ---*/
                    RUN getBatchRecords IN {&hDBOTable7} (INPUT rLast,
                                                        INPUT IF rLast = ? THEN NO ELSE YES,
                                                        INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                              &ELSE 40 &endif,
                                                        OUTPUT iRowsReturned,
                                                        OUTPUT TABLE ttTable7Aux).
                &ENDIF
                
                /*--- Posiciona no £ltimo registro caso n∆o existam mais registros no DBO filho ---*/
                IF CAN-FIND(FIRST ttTable7Aux) THEN DO:
                    /*--- Transfere dados da temp-table ttTable7Aux 
                          para a temp-table {&ttTable7}   ---*/
                    &IF  "{&ROW-NUM-DEFINED7}":U = "YES":U  &THEN
                        ASSIGN iLastSeq = 0.
                        FOR LAST {&ttTable7} BY {&ttTable7}.RowNum:
                            /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                            ASSIGN iLastSeq = {&ttTable7}.RowNum.
                        END.
                    &ENDIF
                    FOR EACH ttTable7Aux:
                        CREATE {&ttTable7}.

                        &IF  "{&ROW-NUM-DEFINED7}":U = "YES":U  &THEN
                            ASSIGN {&ttTable7}.RowNum = {&ttTable7}.RowNum + iLastSeq
                                           iLastSeq = {&ttTable7}.RowNum. 
                            /*--- Acrescentado incremento do iLastSeq para que na pr¢xima 
                                    iteraá∆o do For Each n∆o repita o registro. ---*/    
                            BUFFER-COPY ttTable7Aux except RowNum TO {&ttTable7}.
                        &ELSE
                            BUFFER-COPY ttTable7Aux TO {&ttTable7}.
                        &ENDIF
                    END.

                    /*--- Abre o browse filho, para atualizaªío dos dados ---*/
                    {&OPEN-QUERY-{&page7browse}}
                END.
                
                /*--- Posiciona browse, no pr¢ximo registro ---*/
                CASE cEventBrowse:
                    WHEN "PAGE-DOWN":U THEN DO:
                        /*--- Seta view-port do browse para reposicionamento ---*/
                        {&page7browse}:SET-REPOSITIONED-ROW(1).
                        
                        IF  rCurrent7 <> ? THEN DO:
                            /*--- Reposiciona browse no registro atual ---*/
                            REPOSITION {&page7browse} TO ROWID rCurrent7 NO-ERROR.
                            GET NEXT {&page7browse} NO-LOCK.
                            
                            /*--- Seleciona o £ltimo registro do view-port do browse ---*/
                            {&page7browse}:SELECT-ROW({&page7browse}:NUM-ITERATIONS IN FRAME fPage7).
                        END.
                    END.
                    OTHERWISE DO:
                        /*--- Seta view-port do browse para reposicionamento ---*/
                        {&page7browse}:SET-REPOSITIONED-ROW({&page7browse}:DOWN IN FRAME fPage7 - 1) IN FRAME fPage7.
                        
                        IF  rReposition <> ? THEN DO:
                            /*--- Reposiciona browse no registro atual ---*/
                            REPOSITION {&page7browse} TO ROWID rReposition NO-ERROR.
                            GET NEXT {&page7browse} NO-LOCK.
                            
                            /*--- Posiciona browse no pr¢ximo registro ---*/
                            {&page7browse}:SELECT-FOCUSED-ROW().
                            {&page7browse}:SELECT-NEXT-ROW().
                        END.
                    END.
                END CASE.
            END.
        &ENDIF
        
        &IF "{&page8browse}":U <> "":U &THEN
            WHEN 8 THEN DO WITH FRAME fPage8:
                /*--- Seta vari†vel rLast com o valor do £ltimo rowid 
                      da tabela {&ttTable8} existente no browse            ---*/
                GET LAST {&page8browse} NO-LOCK.
                IF AVAILABLE {&ttTable8} THEN
                    ASSIGN rLast       = {&ttTable8}.r-Rowid
                           rReposition = rowid({&ttTable8}).
                ELSE
                    ASSIGN rLast       = ?
                           rReposition = ?.
                
                IF  {&page8browse}:MULTIPLE AND rReposition <> ? THEN
                    ASSIGN rCurrent8 = rReposition.
                
                /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                &IF DEFINED(DBOVersion) <> 0 &THEN
                    /*--- Retorna registros do DBO filho, na temp-table ttTable8Aux ---*/
                    RUN serverSendRows IN {&hDBOTable8} (INPUT ?,
                                                       INPUT STRING(rLast),
                                                       INPUT IF rLast = ? THEN NO ELSE YES,
                                                       INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                             &ELSE 40 &endif,
                                                       OUTPUT iRowsReturned,
                                                       OUTPUT TABLE ttTable8Aux).
                &ELSE
                    /*--- Retorna registros do DBO filho, na temp-table ttTable8Aux ---*/
                    RUN getBatchRecords IN {&hDBOTable8} (INPUT rLast,
                                                        INPUT IF rLast = ? THEN NO ELSE YES,
                                                        INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                              &ELSE 40 &endif,
                                                        OUTPUT iRowsReturned,
                                                        OUTPUT TABLE ttTable8Aux).
                &ENDIF
                
                /*--- Posiciona no £ltimo registro caso n∆o existam mais registros no DBO filho ---*/
                IF CAN-FIND(FIRST ttTable8Aux) THEN DO:
                    /*--- Transfere dados da temp-table ttTable8Aux 
                          para a temp-table {&ttTable8}   ---*/
                    &IF  "{&ROW-NUM-DEFINED8}":U = "YES":U  &THEN
                        ASSIGN iLastSeq = 0.
                        FOR LAST {&ttTable8} BY {&ttTable8}.RowNum:
                            /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                            ASSIGN iLastSeq = {&ttTable8}.RowNum.
                        END.
                    &ENDIF
                    FOR EACH ttTable8Aux:
                        CREATE {&ttTable8}.

                        &IF  "{&ROW-NUM-DEFINED8}":U = "YES":U  &THEN
                            ASSIGN {&ttTable8}.RowNum = {&ttTable8}.RowNum + iLastSeq
                                           iLastSeq = {&ttTable8}.RowNum. 
                            /*--- Acrescentado incremento do iLastSeq para que na pr¢xima 
                                    iteraá∆o do For Each n∆o repita o registro. ---*/    
                            BUFFER-COPY ttTable8Aux except RowNum TO {&ttTable8}.
                        &ELSE
                            BUFFER-COPY ttTable8Aux TO {&ttTable8}.
                        &ENDIF
                    END.

                    /*--- Abre o browse filho, para atualizaªío dos dados ---*/
                    {&OPEN-QUERY-{&page8browse}}
                END.
                                
                /*--- Posiciona browse, no pr¢ximo registro ---*/
                CASE cEventBrowse:
                    WHEN "PAGE-DOWN":U THEN DO:
                        /*--- Seta view-port do browse para reposicionamento ---*/
                        {&page8browse}:SET-REPOSITIONED-ROW(1).
                        
                        IF  rCurrent8 <> ? THEN DO:
                            /*--- Reposiciona browse no registro atual ---*/
                            REPOSITION {&page8browse} TO ROWID rCurrent8 NO-ERROR.
                            GET NEXT {&page8browse} NO-LOCK.
                            
                            /*--- Seleciona o £ltimo registro do view-port do browse ---*/
                            {&page8browse}:SELECT-ROW({&page8browse}:NUM-ITERATIONS IN FRAME fPage8).
                        END.
                    END.
                    OTHERWISE DO:
                        /*--- Seta view-port do browse para reposicionamento ---*/
                        {&page8browse}:SET-REPOSITIONED-ROW({&page8browse}:DOWN IN FRAME fPage8 - 1) IN FRAME fPage8.
                        
                        IF  rReposition <> ? THEN DO:
                            /*--- Reposiciona browse no registro atual ---*/
                            REPOSITION {&page8browse} TO ROWID rReposition NO-ERROR.
                            GET NEXT {&page8browse} NO-LOCK.
                            
                            /*--- Posiciona browse no pr¢ximo registro ---*/
                            {&page8browse}:SELECT-FOCUSED-ROW().
                            {&page8browse}:SELECT-NEXT-ROW().
                        END.
                    END.
                END CASE.
            END.
        &ENDIF
    END CASE.
    
    
    /*--- Seta vari†vel cEventBrowse com o valor "" ---*/
    ASSIGN cEventBrowse = "":U.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


&IF DEFINED(EXCLUDE-applyOffHome) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE applyOffHome Method-Library 
PROCEDURE applyOffHome :
/*------------------------------------------------------------------------------
  Purpose:     L¢gica para trigger de OFF-HOME
  Parameters:  recebe o n£mero da p†gina
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pPageNumber AS INTEGER NO-UNDO.
    
    
    /*INSERIDO TESTE DO PRE-PROCESSADOR &DBOVersion PARA QUE O C‡DIGO ABAIXO SEJA EXECUTADO*/
    /*SOMENTE QUANDO O DBO FOR 2.O*/
    /*ALTERAÄ«O FEITA POR ANDERSON (TECH485) EM 04/04/2001 PARA EVITAR QUE QUANDO SE NAVEGUE PARA*/
    /*CIMA EM UM BROWSE O MESMO N«O DE O ERRO DE REGISTRO DUPLICADO*/
    /*TESTE PRE-PROCESSADOR(VERS«O DO BO)*/
    &IF DEFINED(DBOVersion) = 0 &THEN    
    
        DEFINE VARIABLE rFirst AS ROWID NO-UNDO.
        DEFINE VARIABLE rReposition AS ROWID NO-UNDO.
    
        /*--- Testa o pre-processador RowNumDefinedN para todas as p†ginas para
          definir iLastSeq que Ç utilizada no c†lculo do valor do rowNum    ---*/
        &IF  "{&ROW-NUM-DEFINED1}":U = "YES":U  or
             "{&ROW-NUM-DEFINED2}":U = "YES":u  or
             "{&ROW-NUM-DEFINED3}":U = "YES":u  or
             "{&ROW-NUM-DEFINED4}":U = "YES":u  or
             "{&ROW-NUM-DEFINED5}":U = "YES":u  or
             "{&ROW-NUM-DEFINED6}":U = "YES":u  or
             "{&ROW-NUM-DEFINED7}":U = "YES":u  or
             "{&ROW-NUM-DEFINED8}":U = "YES":u  &THEN
             DEFINE VARIABLE iFirstSeq AS INTEGER NO-UNDO INIT 0.
        &ENDIF
    
        /*--- Tratar evento de SCROLL-NOTIFY do Browse ---*/
        IF cEventBrowse = "":U THEN
            ASSIGN cEventBrowse = "SCROLL-NOTIFY":U.


        
        CASE pPageNumber:
            &IF "{&page1browse}":U <> "":U &THEN
                WHEN 1 THEN DO WITH FRAME fPage1:
                    /*--- Seta vari†vel rFirst com o valor do primeiro rowid da 
                          tabela {&ttTable1} existente no browse ---*/
                    GET FIRST {&page1browse} NO-LOCK.
                    IF AVAILABLE {&ttTable1} THEN
                        ASSIGN rFirst = {&ttTable1}.r-Rowid
                               rReposition = ROWID({&ttTable1}).
                    ELSE
                        ASSIGN rFirst = ?
                               rReposition = ?.
                
                    IF  {&page1browse}:MULTIPLE AND rReposition <> ? THEN
                        ASSIGN rCurrent1 = rReposition.
            
                    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                    &IF DEFINED(DBOVersion) <> 0 &THEN
                        /*--- Retorna registros do DBO filho, na temp-table ttTable1Aux ---*/
                        RUN serverSendRows IN {&hDBOTable1} (INPUT ?,
                                                           INPUT STRING(rFirst),
                                                           INPUT IF rFirst = ? THEN NO ELSE YES,
                                                           INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                                 &ELSE 40 &endif,
                                                           OUTPUT iRowsReturned,
                                                           OUTPUT TABLE ttTable1Aux).
                    &ELSE
                        /*--- Retorna registros do DBO filho, na temp-table ttTable1Aux ---*/
                        RUN getBatchRecordsPrev IN {&hDBOTable1} (INPUT rFirst,
                                                                INPUT IF rFirst = ? THEN NO ELSE YES,
                                                                INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                                      &ELSE 40 &endif,
                                                                OUTPUT iRowsReturned,
                                                                OUTPUT TABLE ttTable1Aux).
                    &ENDIF
                    
                    IF  CAN-FIND(FIRST {&ttTable1}) THEN DO:
                        /*--- Transfere dados da temp-table ttTable1Aux para a temp-table {&ttTable1} ---*/
                        &IF  "{&ROW-NUM-DEFINED1}":U = "YES":U  &THEN
                             ASSIGN iFirstSeq = 0.
                            FOR LAST {&ttTable1} BY {&ttTable1}.RowNum:
                                /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                                ASSIGN iFirstSeq = {&ttTable1}.RowNum.
                            END.
                        &ENDIF
                        FOR EACH ttTable1Aux:
                            CREATE {&ttTable1}.
                            &IF  "{&ROW-NUM-DEFINED1}":U = "YES":U  &THEN
                                ASSIGN {&ttTable1}.RowNum = {&ttTable1}.RowNum + iFirstSeq
                                               iFirstSeq = {&ttTable1}.RowNum. 
                                /*--- Acrescentado incremento do iLastSeq para que na pr¢xima 
                                        iteraá∆o do For Each n∆o repita o registro. ---*/    
                                BUFFER-COPY ttTable1Aux except RowNum TO {&ttTable1}.
                            &ELSE
                                BUFFER-COPY ttTable1Aux TO {&ttTable1}.
                            &ENDIF
                        END.
                    
                        /*--- Abre o browse filho, para atualizaá∆o dos dados ---*/
                        {&OPEN-QUERY-{&page1browse}}
                    END.    /*--- CAN FIND FIRST {&ttTable1} ---*/
                
                    /*--- Posiciona browse, no pr¢ximo registro ---*/
                    CASE cEventBrowse:
                        WHEN "PAGE-UP":U THEN DO:
                            /*--- Seta view-port do browse para reposicionamento ---*/
                            {&page1browse}:SET-REPOSITIONED-ROW({&page1browse}:DOWN IN FRAME fPage1).
                        
                            IF  rCurrent1 <> ? THEN DO:
                                /*--- Reposiciona browse no registro atual ---*/
                                REPOSITION {&page1browse} TO ROWID rCurrent1 NO-ERROR.
                                GET NEXT {&page1browse} NO-LOCK.
                            
                                /*--- Seleciona o £ltimo registro do view-port do browse ---*/
                                {&page1browse}:SELECT-ROW(1).
                            END.
                        END.
                        OTHERWISE DO:
                            /*--- Seta view-port do browse para reposicionamento ---*/
                            {&page1browse}:SET-REPOSITIONED-ROW(1) IN FRAME fPage1.
                        
                            IF  rReposition <> ? THEN DO:
                                /*--- Reposiciona browse no registro atual ---*/
                                REPOSITION {&page1browse} TO ROWID rReposition NO-ERROR.
                                GET NEXT {&page1browse} NO-LOCK.
                            
                                /*--- Posiciona browse no pr¢ximo registro ---*/
                                {&page1browse}:SELECT-FOCUSED-ROW().
                                {&page1browse}:SELECT-PREV-ROW().
                            END.
                        END.
                    END CASE.
                END.
            &ENDIF
        
            &IF "{&page2browse}":U <> "":U &THEN
                WHEN 2 THEN DO WITH FRAME fPage2:
                    /*--- Seta vari†vel rFirst com o valor do primeiro rowid da 
                          tabela {&ttTable2} existente no browse ---*/
                    GET FIRST {&page2browse} NO-LOCK.
                    IF AVAILABLE {&ttTable2} THEN
                        ASSIGN rFirst = {&ttTable2}.r-Rowid
                               rReposition = ROWID({&ttTable2}).
                    ELSE
                        ASSIGN rFirst = ?
                               rReposition = ?.
                    
                    IF  {&page2browse}:MULTIPLE AND rReposition <> ? THEN
                        ASSIGN rCurrent2 = rReposition.
                
                    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                    &IF DEFINED(DBOVersion) <> 0 &THEN
                        /*--- Retorna registros do DBO filho, na temp-table ttTable2Aux ---*/
                        RUN serverSendRows IN {&hDBOTable2} (INPUT ?,
                                                           INPUT STRING(rFirst),
                                                           INPUT IF rFirst = ? THEN NO ELSE YES,
                                                           INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                                 &ELSE 40 &endif,
                                                           OUTPUT iRowsReturned,
                                                           OUTPUT TABLE ttTable2Aux).
                    &ELSE
                        /*--- Retorna registros do DBO filho, na temp-table ttTable2Aux ---*/
                        RUN getBatchRecordsPrev IN {&hDBOTable2} (INPUT rFirst,
                                                                INPUT IF rFirst = ? THEN NO ELSE YES,
                                                                INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                                      &ELSE 40 &endif,
                                                                OUTPUT iRowsReturned,
                                                                OUTPUT TABLE ttTable2Aux).
                    &ENDIF
                        
                    IF  CAN-FIND(FIRST {&ttTable2}) THEN DO:
                        /*--- Transfere dados da temp-table ttTable2Aux para a temp-table {&ttTable2} ---*/
                        &IF  "{&ROW-NUM-DEFINED2}":U = "YES":U  &THEN
                            ASSIGN iFirstSeq = 0.
                            FOR LAST {&ttTable2} BY {&ttTable2}.RowNum:
                                /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                                ASSIGN iFirstSeq = {&ttTable2}.RowNum.
                            END.
                        &ENDIF
                        FOR EACH ttTable2Aux:
                            CREATE {&ttTable2}.
                            &IF  "{&ROW-NUM-DEFINED2}":U = "YES":U  &THEN
                                ASSIGN {&ttTable2}.RowNum = {&ttTable2}.RowNum + iFirstSeq
                                               iFirstSeq = {&ttTable2}.RowNum. 
                                /*--- Acrescentado incremento do iLastSeq para que na pr¢xima 
                                        iteraá∆o do For Each n∆o repita o registro. ---*/    
                                BUFFER-COPY ttTable2Aux except RowNum TO {&ttTable2}.
                            &ELSE
                                BUFFER-COPY ttTable2Aux TO {&ttTable2}.
                            &ENDIF
                        END.
                        
                        /*--- Abre o browse filho, para atualizaá∆o dos dados ---*/
                        {&OPEN-QUERY-{&page2browse}}
                    END. /*--- CAN FIND FIRST {&ttTable2} ---*/
                    
                    /*--- Posiciona browse, no pr¢ximo registro ---*/
                    CASE cEventBrowse:
                        WHEN "PAGE-UP":U THEN DO:
                            /*--- Seta view-port do browse para reposicionamento ---*/
                            {&page2browse}:SET-REPOSITIONED-ROW({&page2browse}:DOWN IN FRAME fPage2).
                            
                            IF  rCurrent2 <> ? THEN DO:
                                /*--- Reposiciona browse no registro atual ---*/
                                REPOSITION {&page2browse} TO ROWID rCurrent2 NO-ERROR.
                                GET NEXT {&page2browse} NO-LOCK.
                                
                                /*--- Seleciona o £ltimo registro do view-port do browse ---*/
                                {&page2browse}:SELECT-ROW(1).
                            END.
                        END.
                        OTHERWISE DO:
                            /*--- Seta view-port do browse para reposicionamento ---*/
                            {&page2browse}:SET-REPOSITIONED-ROW(1) IN FRAME fPage2.
                            
                            IF  rReposition <> ? THEN DO:
                                /*--- Reposiciona browse no registro atual ---*/
                                REPOSITION {&page2browse} TO ROWID rReposition NO-ERROR.
                                GET NEXT {&page2browse} NO-LOCK.
                                
                                /*--- Posiciona browse no pr¢ximo registro ---*/
                                {&page2browse}:SELECT-PREV-ROW().
                                {&page2browse}:SELECT-PREV-ROW().
                            END.
                        END.
                    END CASE.
                END.
            &ENDIF
        
            &IF "{&page3browse}":U <> "":U &THEN
                WHEN 3 THEN DO WITH FRAME fPage3:
                    /*--- Seta vari†vel rFirst com o valor do primeiro rowid da 
                          tabela {&ttTable3} existente no browse ---*/
                    GET FIRST {&page3browse} NO-LOCK.
                    IF AVAILABLE {&ttTable3} THEN
                        ASSIGN rFirst = {&ttTable3}.r-Rowid
                               rReposition = ROWID({&ttTable3}).
                    ELSE
                        ASSIGN rFirst = ?
                               rReposition = ?.
                    
                    IF  {&page3browse}:MULTIPLE AND rReposition <> ? THEN
                        ASSIGN rCurrent3 = rReposition.
                
                    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                    &IF DEFINED(DBOVersion) <> 0 &THEN
                        /*--- Retorna registros do DBO filho, na temp-table ttTable3Aux ---*/
                        RUN serverSendRows IN {&hDBOTable3} (INPUT ?,
                                                           INPUT STRING(rFirst),
                                                           INPUT IF rFirst = ? THEN NO ELSE YES,
                                                           INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                                 &ELSE 40 &endif,
                                                           OUTPUT iRowsReturned,
                                                           OUTPUT TABLE ttTable3Aux).
                    &ELSE
                        /*--- Retorna registros do DBO filho, na temp-table ttTable3Aux ---*/
                        RUN getBatchRecordsPrev IN {&hDBOTable3} (INPUT rFirst,
                                                                INPUT IF rFirst = ? THEN NO ELSE YES,
                                                                INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                                      &ELSE 40 &endif,
                                                                OUTPUT iRowsReturned,
                                                                OUTPUT TABLE ttTable3Aux).
                    &ENDIF
                        
                    IF  CAN-FIND(FIRST {&ttTable3}) THEN DO:
                        /*--- Transfere dados da temp-table ttTable3Aux para a temp-table {&ttTable3} ---*/
                        &IF  "{&ROW-NUM-DEFINED3}":U = "YES":U  &THEN
                            ASSIGN iFirstSeq = 0.
                            FOR LAST {&ttTable3} BY {&ttTable3}.RowNum:
                                /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                                ASSIGN iFirstSeq = {&ttTable3}.RowNum.
                            END.
                        &ENDIF
                        FOR EACH ttTable3Aux:
                            CREATE {&ttTable3}.
                            &IF  "{&ROW-NUM-DEFINED3}":U = "YES":U  &THEN
                                ASSIGN {&ttTable3}.RowNum = {&ttTable3}.RowNum + iFirstSeq
                                               iFirstSeq = {&ttTable3}.RowNum. 
                                /*--- Acrescentado incremento do iLastSeq para que na pr¢xima 
                                        iteraá∆o do For Each n∆o repita o registro. ---*/    
                                BUFFER-COPY ttTable3Aux except RowNum TO {&ttTable3}.
                            &ELSE
                                BUFFER-COPY ttTable3Aux TO {&ttTable3}.
                            &ENDIF
                        END.
                        
                        /*--- Abre o browse filho, para atualizaá∆o dos dados ---*/
                        {&OPEN-QUERY-{&page3browse}}
                    END. /*--- CAN FIND FIRST {&ttTable3} ---*/
                    
                    /*--- Posiciona browse, no pr¢ximo registro ---*/
                    CASE cEventBrowse:
                        WHEN "PAGE-UP":U THEN DO:
                            /*--- Seta view-port do browse para reposicionamento ---*/
                            {&page3browse}:SET-REPOSITIONED-ROW({&page3browse}:DOWN IN FRAME fPage3).
                            
                            IF  rCurrent3 <> ? THEN DO:
                                /*--- Reposiciona browse no registro atual ---*/
                                REPOSITION {&page3browse} TO ROWID rCurrent3 NO-ERROR.
                                GET NEXT {&page3browse} NO-LOCK.
                                
                                /*--- Seleciona o £ltimo registro do view-port do browse ---*/
                                {&page3browse}:SELECT-ROW(1).
                            END.
                        END.
                        OTHERWISE DO:
                            /*--- Seta view-port do browse para reposicionamento ---*/
                            {&page3browse}:SET-REPOSITIONED-ROW(1) IN FRAME fPage3.
                            
                            IF  rReposition <> ? THEN DO:
                                /*--- Reposiciona browse no registro atual ---*/
                                REPOSITION {&page3browse} TO ROWID rReposition NO-ERROR.
                                GET NEXT {&page3browse} NO-LOCK.
                                
                                /*--- Posiciona browse no pr¢ximo registro ---*/
                                {&page3browse}:SELECT-PREV-ROW().
                                {&page3browse}:SELECT-PREV-ROW().
                            END.
                        END.
                    END CASE.
                END.
            &ENDIF
        
            &IF "{&page4browse}":U <> "":U &THEN
                WHEN 4 THEN DO WITH FRAME fPage4:
                    /*--- Seta vari†vel rFirst com o valor do primeiro rowid da 
                          tabela {&ttTable4} existente no browse ---*/
                    GET FIRST {&page4browse} NO-LOCK.
                    IF AVAILABLE {&ttTable4} THEN
                        ASSIGN rFirst = {&ttTable4}.r-Rowid
                               rReposition = ROWID({&ttTable4}).
                    ELSE
                        ASSIGN rFirst = ?
                               rReposition = ?.
                    
                    IF  {&page4browse}:MULTIPLE AND rReposition <> ? THEN
                        ASSIGN rCurrent4 = rReposition.
                
                    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                    &IF DEFINED(DBOVersion) <> 0 &THEN
                        /*--- Retorna registros do DBO filho, na temp-table ttTable4Aux ---*/
                        RUN serverSendRows IN {&hDBOTable4} (INPUT ?,
                                                           INPUT STRING(rFirst),
                                                           INPUT IF rFirst = ? THEN NO ELSE YES,
                                                           INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                                 &ELSE 40 &endif,
                                                           OUTPUT iRowsReturned,
                                                           OUTPUT TABLE ttTable4Aux).
                    &ELSE
                        /*--- Retorna registros do DBO filho, na temp-table ttTable4Aux ---*/
                        RUN getBatchRecordsPrev IN {&hDBOTable4} (INPUT rFirst,
                                                                INPUT IF rFirst = ? THEN NO ELSE YES,
                                                                INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                                      &ELSE 40 &endif,
                                                                OUTPUT iRowsReturned,
                                                                OUTPUT TABLE ttTable4Aux).
                    &ENDIF
                        
                    IF  CAN-FIND(FIRST {&ttTable4}) THEN DO:
                        /*--- Transfere dados da temp-table ttTable4Aux para a temp-table {&ttTable4} ---*/
                        &IF  "{&ROW-NUM-DEFINED4}":U = "YES":U  &THEN
                            ASSIGN iFirstSeq = 0.
                            FOR LAST {&ttTable4} BY {&ttTable4}.RowNum:
                                /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                                ASSIGN iFirstSeq = {&ttTable4}.RowNum.
                            END.
                        &ENDIF
                        FOR EACH ttTable4Aux:
                            CREATE {&ttTable4}.
                            &IF  "{&ROW-NUM-DEFINED4}":U = "YES":U  &THEN
                                ASSIGN {&ttTable4}.RowNum = {&ttTable4}.RowNum + iFirstSeq
                                               iFirstSeq = {&ttTable4}.RowNum. 
                                /*--- Acrescentado incremento do iLastSeq para que na pr¢xima 
                                        iteraá∆o do For Each n∆o repita o registro. ---*/    
                                BUFFER-COPY ttTable4Aux except RowNum TO {&ttTable4}.
                            &ELSE
                                BUFFER-COPY ttTable4Aux TO {&ttTable4}.
                            &ENDIF
                        END.
                        
                        /*--- Abre o browse filho, para atualizaá∆o dos dados ---*/
                        {&OPEN-QUERY-{&page4browse}}
                    END. /*--- CAN FIND FIRST {&ttTable4} ---*/
                    
                    /*--- Posiciona browse, no pr¢ximo registro ---*/
                    CASE cEventBrowse:
                        WHEN "PAGE-UP":U THEN DO:
                            /*--- Seta view-port do browse para reposicionamento ---*/
                            {&page4browse}:SET-REPOSITIONED-ROW({&page4browse}:DOWN IN FRAME fPage4).
                            
                            IF  rCurrent4 <> ? THEN DO:
                                /*--- Reposiciona browse no registro atual ---*/
                                REPOSITION {&page4browse} TO ROWID rCurrent4 NO-ERROR.
                                GET NEXT {&page4browse} NO-LOCK.
                                
                                /*--- Seleciona o £ltimo registro do view-port do browse ---*/
                                {&page4browse}:SELECT-ROW(1).
                            END.
                        END.
                        OTHERWISE DO:
                            /*--- Seta view-port do browse para reposicionamento ---*/
                            {&page4browse}:SET-REPOSITIONED-ROW(1) IN FRAME fPage4.
                            
                            IF  rReposition <> ? THEN DO:
                                /*--- Reposiciona browse no registro atual ---*/
                                REPOSITION {&page4browse} TO ROWID rReposition NO-ERROR.
                                GET NEXT {&page4browse} NO-LOCK.
                                
                                /*--- Posiciona browse no pr¢ximo registro ---*/
                                {&page4browse}:SELECT-PREV-ROW().
                                {&page4browse}:SELECT-PREV-ROW().
                            END.
                        END.
                    END CASE.
                END.
            &ENDIF
        
            &IF "{&page5browse}":U <> "":U &THEN
                WHEN 5 THEN DO WITH FRAME fPage5:
                    /*--- Seta vari†vel rFirst com o valor do primeiro rowid da 
                          tabela {&ttTable5} existente no browse ---*/
                    GET FIRST {&page5browse} NO-LOCK.
                    IF AVAILABLE {&ttTable5} THEN
                        ASSIGN rFirst = {&ttTable5}.r-Rowid
                               rReposition = ROWID({&ttTable5}).
                    ELSE
                        ASSIGN rFirst = ?
                               rReposition = ?.
                    
                    IF  {&page5browse}:MULTIPLE AND rReposition <> ? THEN
                        ASSIGN rCurrent5 = rReposition.
                
                    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                    &IF DEFINED(DBOVersion) <> 0 &THEN
                        /*--- Retorna registros do DBO filho, na temp-table ttTable5Aux ---*/
                        RUN serverSendRows IN {&hDBOTable5} (INPUT ?,
                                                           INPUT STRING(rFirst),
                                                           INPUT IF rFirst = ? THEN NO ELSE YES,
                                                           INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                                 &ELSE 40 &endif,
                                                           OUTPUT iRowsReturned,
                                                           OUTPUT TABLE ttTable5Aux).
                    &ELSE
                        /*--- Retorna registros do DBO filho, na temp-table ttTable5Aux ---*/
                        RUN getBatchRecordsPrev IN {&hDBOTable5} (INPUT rFirst,
                                                                INPUT IF rFirst = ? THEN NO ELSE YES,
                                                                INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                                      &ELSE 40 &endif,
                                                                OUTPUT iRowsReturned,
                                                                OUTPUT TABLE ttTable5Aux).
                    &ENDIF
                        
                    IF  CAN-FIND(FIRST {&ttTable5}) THEN DO:
                        /*--- Transfere dados da temp-table ttTable5Aux para a temp-table {&ttTable5} ---*/
                        &IF  "{&ROW-NUM-DEFINED5}":U = "YES":U  &THEN
                            ASSIGN iFirstSeq = 0.
                            FOR LAST {&ttTable5} BY {&ttTable5}.RowNum:
                                /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                                ASSIGN iFirstSeq = {&ttTable5}.RowNum.
                            END.
                        &ENDIF
                        FOR EACH ttTable5Aux:
                            CREATE {&ttTable5}.
                            &IF  "{&ROW-NUM-DEFINED5}":U = "YES":U  &THEN
                                ASSIGN {&ttTable5}.RowNum = {&ttTable5}.RowNum + iFirstSeq
                                               iFirstSeq = {&ttTable5}.RowNum. 
                                /*--- Acrescentado incremento do iLastSeq para que na pr¢xima 
                                        iteraá∆o do For Each n∆o repita o registro. ---*/    
                                BUFFER-COPY ttTable5Aux except RowNum TO {&ttTable5}.
                            &ELSE
                                BUFFER-COPY ttTable5Aux TO {&ttTable5}.
                            &ENDIF
                        END.
                        
                        /*--- Abre o browse filho, para atualizaá∆o dos dados ---*/
                        {&OPEN-QUERY-{&page5browse}}
                    END. /*--- CAN FIND FIRST {&ttTable5} ---*/
                    
                    /*--- Posiciona browse, no pr¢ximo registro ---*/
                    CASE cEventBrowse:
                        WHEN "PAGE-UP":U THEN DO:
                            /*--- Seta view-port do browse para reposicionamento ---*/
                            {&page5browse}:SET-REPOSITIONED-ROW({&page5browse}:DOWN IN FRAME fPage5).
                            
                            IF  rCurrent5 <> ? THEN DO:
                                /*--- Reposiciona browse no registro atual ---*/
                                REPOSITION {&page5browse} TO ROWID rCurrent5 NO-ERROR.
                                GET NEXT {&page5browse} NO-LOCK.
                                
                                /*--- Seleciona o £ltimo registro do view-port do browse ---*/
                                {&page5browse}:SELECT-ROW(1).
                            END.
                        END.
                        OTHERWISE DO:
                            /*--- Seta view-port do browse para reposicionamento ---*/
                            {&page5browse}:SET-REPOSITIONED-ROW(1) IN FRAME fPage5.
                            
                            IF  rReposition <> ? THEN DO:
                                /*--- Reposiciona browse no registro atual ---*/
                                REPOSITION {&page5browse} TO ROWID rReposition NO-ERROR.
                                GET NEXT {&page5browse} NO-LOCK.
                                
                                /*--- Posiciona browse no pr¢ximo registro ---*/
                                {&page5browse}:SELECT-PREV-ROW().
                                {&page5browse}:SELECT-PREV-ROW().
                            END.
                        END.
                    END CASE.
                END.
            &ENDIF
        
            &IF "{&page6browse}":U <> "":U &THEN
                WHEN 6 THEN DO WITH FRAME fPage6:
                    /*--- Seta vari†vel rFirst com o valor do primeiro rowid da 
                          tabela {&ttTable6} existente no browse ---*/
                    GET FIRST {&page6browse} NO-LOCK.
                    IF AVAILABLE {&ttTable6} THEN
                        ASSIGN rFirst = {&ttTable6}.r-Rowid
                               rReposition = ROWID({&ttTable6}).
                    ELSE
                        ASSIGN rFirst = ?
                               rReposition = ?.
                    
                    IF  {&page6browse}:MULTIPLE AND rReposition <> ? THEN
                        ASSIGN rCurrent6 = rReposition.
                
                    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                    &IF DEFINED(DBOVersion) <> 0 &THEN
                        /*--- Retorna registros do DBO filho, na temp-table ttTable6Aux ---*/
                        RUN serverSendRows IN {&hDBOTable6} (INPUT ?,
                                                           INPUT STRING(rFirst),
                                                           INPUT IF rFirst = ? THEN NO ELSE YES,
                                                           INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                                 &ELSE 40 &endif,
                                                           OUTPUT iRowsReturned,
                                                           OUTPUT TABLE ttTable6Aux).
                    &ELSE
                        /*--- Retorna registros do DBO filho, na temp-table ttTable6Aux ---*/
                        RUN getBatchRecordsPrev IN {&hDBOTable6} (INPUT rFirst,
                                                                INPUT IF rFirst = ? THEN NO ELSE YES,
                                                                INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                                      &ELSE 40 &endif,
                                                                OUTPUT iRowsReturned,
                                                                OUTPUT TABLE ttTable6Aux).
                    &ENDIF
                        
                    IF  CAN-FIND(FIRST {&ttTable6}) THEN DO:
                        /*--- Transfere dados da temp-table ttTable6Aux para a temp-table {&ttTable6} ---*/
                        &IF  "{&ROW-NUM-DEFINED6}":U = "YES":U  &THEN
                            ASSIGN iFirstSeq = 0.
                            FOR LAST {&ttTable6} BY {&ttTable6}.RowNum:
                                /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                                ASSIGN iFirstSeq = {&ttTable6}.RowNum.
                            END.
                        &ENDIF
                        FOR EACH ttTable6Aux:
                            CREATE {&ttTable6}.
                            &IF  "{&ROW-NUM-DEFINED6}":U = "YES":U  &THEN
                                ASSIGN {&ttTable6}.RowNum = {&ttTable6}.RowNum + iFirstSeq
                                               iFirstSeq = {&ttTable6}.RowNum. 
                                /*--- Acrescentado incremento do iLastSeq para que na pr¢xima 
                                        iteraá∆o do For Each n∆o repita o registro. ---*/    
                                BUFFER-COPY ttTable6Aux except RowNum TO {&ttTable6}.
                            &ELSE
                                BUFFER-COPY ttTable6Aux TO {&ttTable6}.
                            &ENDIF
                        END.
                        
                        /*--- Abre o browse filho, para atualizaá∆o dos dados ---*/
                        {&OPEN-QUERY-{&page6browse}}
                    END. /*--- CAN FIND FIRST {&ttTable6} ---*/
                    
                    /*--- Posiciona browse, no pr¢ximo registro ---*/
                    CASE cEventBrowse:
                        WHEN "PAGE-UP":U THEN DO:
                            /*--- Seta view-port do browse para reposicionamento ---*/
                            {&page6browse}:SET-REPOSITIONED-ROW({&page6browse}:DOWN IN FRAME fPage6).
                            
                            IF  rCurrent6 <> ? THEN DO:
                                /*--- Reposiciona browse no registro atual ---*/
                                REPOSITION {&page6browse} TO ROWID rCurrent6 NO-ERROR.
                                GET NEXT {&page6browse} NO-LOCK.
                                
                                /*--- Seleciona o £ltimo registro do view-port do browse ---*/
                                {&page6browse}:SELECT-ROW(1).
                            END.
                        END.
                        OTHERWISE DO:
                            /*--- Seta view-port do browse para reposicionamento ---*/
                            {&page6browse}:SET-REPOSITIONED-ROW(1) IN FRAME fPage6.
                            
                            IF  rReposition <> ? THEN DO:
                                /*--- Reposiciona browse no registro atual ---*/
                                REPOSITION {&page6browse} TO ROWID rReposition NO-ERROR.
                                GET NEXT {&page6browse} NO-LOCK.
                                
                                /*--- Posiciona browse no pr¢ximo registro ---*/
                                {&page6browse}:SELECT-PREV-ROW().
                                {&page6browse}:SELECT-PREV-ROW().
                            END.
                        END.
                    END CASE.
                END.
            &ENDIF
        
            &IF "{&page7browse}":U <> "":U &THEN
                WHEN 7 THEN DO WITH FRAME fPage7:
                    /*--- Seta vari†vel rFirst com o valor do primeiro rowid da 
                          tabela {&ttTable7} existente no browse ---*/
                    GET FIRST {&page7browse} NO-LOCK.
                    IF AVAILABLE {&ttTable7} THEN
                        ASSIGN rFirst = {&ttTable7}.r-Rowid
                               rReposition = ROWID({&ttTable7}).
                    ELSE
                        ASSIGN rFirst = ?
                               rReposition = ?.
                    
                    IF  {&page7browse}:MULTIPLE AND rReposition <> ? THEN
                        ASSIGN rCurrent7 = rReposition.
                
                    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                    &IF DEFINED(DBOVersion) <> 0 &THEN
                        /*--- Retorna registros do DBO filho, na temp-table ttTable7Aux ---*/
                        RUN serverSendRows IN {&hDBOTable7} (INPUT ?,
                                                           INPUT STRING(rFirst),
                                                           INPUT IF rFirst = ? THEN NO ELSE YES,
                                                           INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                                 &ELSE 40 &endif,
                                                           OUTPUT iRowsReturned,
                                                           OUTPUT TABLE ttTable7Aux).
                    &ELSE
                        /*--- Retorna registros do DBO filho, na temp-table ttTable7Aux ---*/
                        RUN getBatchRecordsPrev IN {&hDBOTable7} (INPUT rFirst,
                                                                INPUT IF rFirst = ? THEN NO ELSE YES,
                                                                INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                                      &ELSE 40 &endif,
                                                                OUTPUT iRowsReturned,
                                                                OUTPUT TABLE ttTable7Aux).
                    &ENDIF
                        
                    IF  CAN-FIND(FIRST {&ttTable7}) THEN DO:
                        /*--- Transfere dados da temp-table ttTable7Aux para a temp-table {&ttTable7} ---*/
                        &IF  "{&ROW-NUM-DEFINED7}":U = "YES":U  &THEN
                            ASSIGN iFirstSeq = 0.
                            FOR LAST {&ttTable7} BY {&ttTable7}.RowNum:
                                /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                                ASSIGN iFirstSeq = {&ttTable7}.RowNum.
                            END.
                        &ENDIF
                        FOR EACH ttTable7Aux:
                            CREATE {&ttTable7}.
                            &IF  "{&ROW-NUM-DEFINED7}":U = "YES":U  &THEN
                                ASSIGN {&ttTable7}.RowNum = {&ttTable7}.RowNum + iFirstSeq
                                               iFirstSeq = {&ttTable7}.RowNum. 
                                /*--- Acrescentado incremento do iLastSeq para que na pr¢xima 
                                        iteraá∆o do For Each n∆o repita o registro. ---*/    
                                BUFFER-COPY ttTable7Aux except RowNum TO {&ttTable7}.
                            &ELSE
                                BUFFER-COPY ttTable7Aux TO {&ttTable7}.
                            &ENDIF
                        END.
                        
                        /*--- Abre o browse filho, para atualizaá∆o dos dados ---*/
                        {&OPEN-QUERY-{&page7browse}}
                    END. /*--- CAN FIND FIRST {&ttTable7} ---*/
                    
                    /*--- Posiciona browse, no pr¢ximo registro ---*/
                    CASE cEventBrowse:
                        WHEN "PAGE-UP":U THEN DO:
                            /*--- Seta view-port do browse para reposicionamento ---*/
                            {&page7browse}:SET-REPOSITIONED-ROW({&page7browse}:DOWN IN FRAME fPage7).
                            
                            IF  rCurrent7 <> ? THEN DO:
                                /*--- Reposiciona browse no registro atual ---*/
                                REPOSITION {&page7browse} TO ROWID rCurrent7 NO-ERROR.
                                GET NEXT {&page7browse} NO-LOCK.
                                
                                /*--- Seleciona o £ltimo registro do view-port do browse ---*/
                                {&page7browse}:SELECT-ROW(1).
                            END.
                        END.
                        OTHERWISE DO:
                            /*--- Seta view-port do browse para reposicionamento ---*/
                            {&page7browse}:SET-REPOSITIONED-ROW(1) IN FRAME fPage7.
                            
                            IF  rReposition <> ? THEN DO:
                                /*--- Reposiciona browse no registro atual ---*/
                                REPOSITION {&page7browse} TO ROWID rReposition NO-ERROR.
                                GET NEXT {&page7browse} NO-LOCK.
                                
                                /*--- Posiciona browse no pr¢ximo registro ---*/
                                {&page7browse}:SELECT-PREV-ROW().
                                {&page7browse}:SELECT-PREV-ROW().
                            END.
                        END.
                    END CASE.
                END.
            &ENDIF
        
            &IF "{&page8browse}":U <> "":U &THEN
                WHEN 8 THEN DO WITH FRAME fPage8:
                    /*--- Seta vari†vel rFirst com o valor do primeiro rowid da 
                          tabela {&ttTable8} existente no browse ---*/
                    GET FIRST {&page8browse} NO-LOCK.
                    IF AVAILABLE {&ttTable8} THEN
                        ASSIGN rFirst = {&ttTable8}.r-Rowid
                               rReposition = ROWID({&ttTable8}).
                    ELSE
                        ASSIGN rFirst = ?
                               rReposition = ?.
                    
                    IF  {&page8browse}:MULTIPLE AND rReposition <> ? THEN
                        ASSIGN rCurrent8 = rReposition.
                
                    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                    &IF DEFINED(DBOVersion) <> 0 &THEN
                        /*--- Retorna registros do DBO filho, na temp-table ttTable8Aux ---*/
                        RUN serverSendRows IN {&hDBOTable8} (INPUT ?,
                                                           INPUT STRING(rFirst),
                                                           INPUT IF rFirst = ? THEN NO ELSE YES,
                                                           INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                                 &ELSE 40 &endif,
                                                           OUTPUT iRowsReturned,
                                                           OUTPUT TABLE ttTable8Aux).
                    &ELSE
                        /*--- Retorna registros do DBO filho, na temp-table ttTable8Aux ---*/
                        RUN getBatchRecordsPrev IN {&hDBOTable8} (INPUT rFirst,
                                                                INPUT IF rFirst = ? THEN NO ELSE YES,
                                                                INPUT &IF defined(numRowsReturned) &then {&numRowsReturned}
                                                                      &ELSE 40 &endif,
                                                                OUTPUT iRowsReturned,
                                                                OUTPUT TABLE ttTable8Aux).
                    &ENDIF
                        
                    IF  CAN-FIND(FIRST {&ttTable8}) THEN DO:
                        /*--- Transfere dados da temp-table ttTable8Aux para a temp-table {&ttTable8} ---*/
                        &IF  "{&ROW-NUM-DEFINED8}":U = "YES":U  &THEN
                            ASSIGN iFirstSeq = 0.
                            FOR LAST {&ttTable8} BY {&ttTable8}.RowNum:
                                /*--- Pega £ltimo RowNum para continuar sequància. ---*/
                                ASSIGN iFirstSeq = {&ttTable8}.RowNum.
                            END.
                        &ENDIF
                        FOR EACH ttTable8Aux:
                            CREATE {&ttTable8}.
                            &IF  "{&ROW-NUM-DEFINED8}":U = "YES":U  &THEN
                                ASSIGN {&ttTable8}.RowNum = {&ttTable8}.RowNum + iFirstSeq
                                               iFirstSeq = {&ttTable8}.RowNum. 
                                /*--- Acrescentado incremento do iLastSeq para que na pr¢xima 
                                        iteraá∆o do For Each n∆o repita o registro. ---*/    
                                BUFFER-COPY ttTable8Aux except RowNum TO {&ttTable8}.
                            &ELSE
                                BUFFER-COPY ttTable8Aux TO {&ttTable8}.
                            &ENDIF
                        END.
                        
                        /*--- Abre o browse filho, para atualizaá∆o dos dados ---*/
                        {&OPEN-QUERY-{&page8browse}}
                    END. /*--- CAN FIND FIRST {&ttTable8} ---*/
                    
                    /*--- Posiciona browse, no pr¢ximo registro ---*/
                    CASE cEventBrowse:
                        WHEN "PAGE-UP":U THEN DO:
                            /*--- Seta view-port do browse para reposicionamento ---*/
                            {&page8browse}:SET-REPOSITIONED-ROW({&page8browse}:DOWN IN FRAME fPage8).
                            
                            IF  rCurrent8 <> ? THEN DO:
                                /*--- Reposiciona browse no registro atual ---*/
                                REPOSITION {&page8browse} TO ROWID rCurrent8 NO-ERROR.
                                GET NEXT {&page8browse} NO-LOCK.
                                
                                /*--- Seleciona o £ltimo registro do view-port do browse ---*/
                                {&page8browse}:SELECT-ROW(1).
                            END.
                        END.
                        OTHERWISE DO:
                            /*--- Seta view-port do browse para reposicionamento ---*/
                            {&page8browse}:SET-REPOSITIONED-ROW(1) IN FRAME fPage8.
                            
                            IF  rReposition <> ? THEN DO:
                                /*--- Reposiciona browse no registro atual ---*/
                                REPOSITION {&page8browse} TO ROWID rReposition NO-ERROR.
                                GET NEXT {&page8browse} NO-LOCK.
                                
                                /*--- Posiciona browse no pr¢ximo registro ---*/
                                {&page8browse}:SELECT-PREV-ROW().
                                {&page8browse}:SELECT-PREV-ROW().
                            END.
                        END.
                    END CASE.
                END.
            &ENDIF
        END CASE.
    
    &ENDIF
    /*FIM ALTERAÄ«O ANDERSON (TECH485) 04/04/2001*/
   
   
    /*--- Seta vari†vel cEventBrowse com o valor "" ---*/
    ASSIGN cEventBrowse = "":U.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-applyPageDown) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE applyPageDown Method-Library 
PROCEDURE applyPageDown :
/*------------------------------------------------------------------------------
  Purpose:     L¢gica para trigger de PAGE-DOWN
  Parameters:  recebe o n£mero da p†gina
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pPageNumber AS INTEGER NO-UNDO.
    
    /*--- Seta vari†vel cEventBrowse com o valor PAGE-DOWN ---*/
    ASSIGN cEventBrowse = "PAGE-DOWN":U.
    
    CASE pPageNumber:
        &IF "{&page1browse}":U <> "":U &THEN
            WHEN 1 THEN DO:
                /*--- Seta vari†vel rCurrent1 com o valor do rowid corrente ---*/
                ASSIGN rCurrent1 = IF AVAILABLE {&ttTable1}
                                       THEN ROWID({&ttTable1})
                                       ELSE ?.
            END.
        &ENDIF
        
        &IF "{&page2browse}":U <> "":U &THEN
            WHEN 2 THEN DO:
                /*--- Seta vari†vel rCurrent2 com o valor do rowid corrente ---*/
                ASSIGN rCurrent2 = IF AVAILABLE {&ttTable2}
                                       THEN ROWID({&ttTable2})
                                       ELSE ?.
            END.
        &ENDIF
        
        &IF "{&page3browse}":U <> "":U &THEN
            WHEN 3 THEN DO:
                /*--- Seta vari†vel rCurrent3 com o valor do rowid corrente ---*/
                ASSIGN rCurrent3 = IF AVAILABLE {&ttTable3}
                                       THEN ROWID({&ttTable3})
                                       ELSE ?.
            END.
        &ENDIF
        
        &IF "{&page4browse}":U <> "":U &THEN
            WHEN 4 THEN DO:
                /*--- Seta vari†vel rCurrent4 com o valor do rowid corrente ---*/
                ASSIGN rCurrent4 = IF AVAILABLE {&ttTable4}
                                       THEN ROWID({&ttTable4})
                                       ELSE ?.
            END.
        &ENDIF
        
        &IF "{&page5browse}":U <> "":U &THEN
            WHEN 5 THEN DO:
                /*--- Seta vari†vel rCurrent5 com o valor do rowid corrente ---*/
                ASSIGN rCurrent5 = IF AVAILABLE {&ttTable5}
                                       THEN ROWID({&ttTable5})
                                       ELSE ?.
            END.
        &ENDIF
        
        &IF "{&page6browse}":U <> "":U &THEN
            WHEN 6 THEN DO:
                /*--- Seta vari†vel rCurrent6 com o valor do rowid corrente ---*/
                ASSIGN rCurrent6 = IF AVAILABLE {&ttTable6}
                                       THEN ROWID({&ttTable6})
                                       ELSE ?.
            END.
        &ENDIF
        
        &IF "{&page7browse}":U <> "":U &THEN
            WHEN 7 THEN DO:
                /*--- Seta vari†vel rCurrent7 com o valor do rowid corrente ---*/
                ASSIGN rCurrent7 = IF AVAILABLE {&ttTable7}
                                       THEN ROWID({&ttTable7})
                                       ELSE ?.
            END.
        &ENDIF
        
        &IF "{&page8browse}":U <> "":U &THEN
            WHEN 8 THEN DO:
                /*--- Seta vari†vel rCurrent8 com o valor do rowid corrente ---*/
                ASSIGN rCurrent8 = IF AVAILABLE {&ttTable8}
                                       THEN ROWID({&ttTable8})
                                       ELSE ?.
            END.
        &ENDIF
    END CASE.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-applyPageUp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE applyPageUp Method-Library 
PROCEDURE applyPageUp :
/*------------------------------------------------------------------------------
  Purpose:     L¢gica para trigger de PAGE-UP
  Parameters:  recebe o n£mero da p†gina
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pPageNumber AS INTEGER NO-UNDO.
    
    /*--- Seta vari†vel cEventBrowse com o valor PAGE-UP ---*/
    ASSIGN cEventBrowse = "PAGE-UP":U.
    
    CASE pPageNumber:
        &IF "{&page1browse}":U <> "":U &THEN
            WHEN 1 THEN DO:
                /*--- Seta vari†vel rCurrent1 com o valor do rowid corrente ---*/
                ASSIGN rCurrent1 = IF AVAILABLE {&ttTable1}
                                       THEN ROWID({&ttTable1})
                                       ELSE ?.
            END.
        &ENDIF
        
        &IF "{&page2browse}":U <> "":U &THEN
            WHEN 2 THEN DO:
                /*--- Seta vari†vel rCurrent2 com o valor do rowid corrente ---*/
                ASSIGN rCurrent2 = IF AVAILABLE {&ttTable2}
                                       THEN ROWID({&ttTable2})
                                       ELSE ?.
            END.
        &ENDIF
        
        &IF "{&page3browse}":U <> "":U &THEN
            WHEN 3 THEN DO:
                /*--- Seta vari†vel rCurrent3 com o valor do rowid corrente ---*/
                ASSIGN rCurrent3 = IF AVAILABLE {&ttTable3}
                                       THEN ROWID({&ttTable3})
                                       ELSE ?.
            END.
        &ENDIF
        
        &IF "{&page4browse}":U <> "":U &THEN
            WHEN 4 THEN DO:
                /*--- Seta vari†vel rCurrent4 com o valor do rowid corrente ---*/
                ASSIGN rCurrent4 = IF AVAILABLE {&ttTable4}
                                       THEN ROWID({&ttTable4})
                                       ELSE ?.
            END.
        &ENDIF
        
        &IF "{&page5browse}":U <> "":U &THEN
            WHEN 5 THEN DO:
                /*--- Seta vari†vel rCurrent5 com o valor do rowid corrente ---*/
                ASSIGN rCurrent5 = IF AVAILABLE {&ttTable5}
                                       THEN ROWID({&ttTable5})
                                       ELSE ?.
            END.
        &ENDIF
        
        &IF "{&page6browse}":U <> "":U &THEN
            WHEN 6 THEN DO:
                /*--- Seta vari†vel rCurrent6 com o valor do rowid corrente ---*/
                ASSIGN rCurrent6 = IF AVAILABLE {&ttTable6}
                                       THEN ROWID({&ttTable6})
                                       ELSE ?.
            END.
        &ENDIF
        
        &IF "{&page7browse}":U <> "":U &THEN
            WHEN 7 THEN DO:
                /*--- Seta vari†vel rCurrent7 com o valor do rowid corrente ---*/
                ASSIGN rCurrent7 = IF AVAILABLE {&ttTable7}
                                       THEN ROWID({&ttTable7})
                                       ELSE ?.
            END.
        &ENDIF
        
        &IF "{&page8browse}":U <> "":U &THEN
            WHEN 8 THEN DO:
                /*--- Seta vari†vel rCurrent8 com o valor do rowid corrente ---*/
                ASSIGN rCurrent8 = IF AVAILABLE {&ttTable8}
                                       THEN ROWID({&ttTable8})
                                       ELSE ?.
            END.
        &ENDIF
    END CASE.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-destroyInterface) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE destroyInterface Method-Library 
PROCEDURE destroyInterface :
/*------------------------------------------------------------------------------
  Purpose:     Destr¢i programa
  Parameters:  
  Notes:       Destr¢i programas de: Folder e DBOs
------------------------------------------------------------------------------*/
    
    /*--- Executa mÇtodos sobrepostos (before/after) ---*/
    {method/override.i &Position="Before"
                       &Procedure="destroyInterface"}
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    /*--- Executa programas de customizaá∆o (before/after) ---*/
    {method/custom.i &Event="BEFORE-DESTROY-INTERFACE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="?"
                     &RowidTable="?"}
                     
    /*Inserida a chamada da include i-logfin1.i
    devido aos thintemplates n∆o gerarem log de 
    execucao de programas*/
    {include/i-logfin1.i}
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Destr¢i BO 1.1 ---*/
        &IF "{&hDBOTable1}":U <> "":U &THEN
            IF VALID-HANDLE({&hDBOTable1}) THEN
                DELETE PROCEDURE {&hDBOTable1}.
        &ENDIF
    &ELSE
        /*--- Destr¢i DBO Table1 ---*/
        &IF "{&hDBOTable1}":U <> "":U &THEN
            IF VALID-HANDLE({&hDBOTable1}) THEN
                RUN destroy IN {&hDBOTable1}.
        &ENDIF
    &ENDIF
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Destr¢i BO 1.1 ---*/
        &IF "{&hDBOTable2}":U <> "":U &THEN
            IF VALID-HANDLE({&hDBOTable2}) THEN
                DELETE PROCEDURE {&hDBOTable2}.
        &ENDIF
    &ELSE
        /*--- Destr¢i DBO Table2 ---*/
        &IF "{&hDBOTable2}":U <> "":U &THEN
            IF VALID-HANDLE({&hDBOTable2}) THEN
                RUN destroy IN {&hDBOTable2}.
        &ENDIF
    &ENDIF
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Destr¢i BO 1.1 ---*/
        &IF "{&hDBOTable3}":U <> "":U &THEN
            IF VALID-HANDLE({&hDBOTable3}) THEN
                DELETE PROCEDURE {&hDBOTable3}.
        &ENDIF
    &ELSE
        /*--- Destr¢i DBO Table3 ---*/
        &IF "{&hDBOTable3}":U <> "":U &THEN
            IF VALID-HANDLE({&hDBOTable3}) THEN
                RUN destroy IN {&hDBOTable3}.
        &ENDIF
    &ENDIF
        
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Destr¢i BO 1.1 ---*/
        &IF "{&hDBOTable4}":U <> "":U &THEN
            IF VALID-HANDLE({&hDBOTable4}) THEN
                DELETE PROCEDURE {&hDBOTable4}.
        &ENDIF
    &ELSE
        /*--- Destr¢i DBO Table4 ---*/
        &IF "{&hDBOTable4}":U <> "":U &THEN
            IF VALID-HANDLE({&hDBOTable4}) THEN
                RUN destroy IN {&hDBOTable4}.
        &ENDIF
    &ENDIF
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Destr¢i BO 1.1 ---*/
        &IF "{&hDBOTable5}":U <> "":U &THEN
            IF VALID-HANDLE({&hDBOTable5}) THEN
                DELETE PROCEDURE {&hDBOTable5}.
        &ENDIF
    &ELSE
        /*--- Destr¢i DBO Table5 ---*/
        &IF "{&hDBOTable5}":U <> "":U &THEN
            IF VALID-HANDLE({&hDBOTable5}) THEN
                RUN destroy IN {&hDBOTable5}.
        &ENDIF
    &ENDIF
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Destr¢i BO 1.1 ---*/
        &IF "{&hDBOTable6}":U <> "":U &THEN
            IF VALID-HANDLE({&hDBOTable6}) THEN
                DELETE PROCEDURE {&hDBOTable6}.
        &ENDIF
    &ELSE
        /*--- Destr¢i DBO Table6 ---*/
        &IF "{&hDBOTable6}":U <> "":U &THEN
            IF VALID-HANDLE({&hDBOTable6}) THEN
                RUN destroy IN {&hDBOTable6}.
        &ENDIF
    &ENDIF
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Destr¢i BO 1.1 ---*/
        &IF "{&hDBOTable7}":U <> "":U &THEN
            IF VALID-HANDLE({&hDBOTable7}) THEN
                DELETE PROCEDURE {&hDBOTable7}.
        &ENDIF
    &ELSE
        /*--- Destr¢i DBO Table7 ---*/
        &IF "{&hDBOTable7}":U <> "":U &THEN
            IF VALID-HANDLE({&hDBOTable7}) THEN
                RUN destroy IN {&hDBOTable7}.
        &ENDIF
    &ENDIF
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    &IF DEFINED(DBOVersion) <> 0 &THEN
        /*--- Destr¢i BO 1.1 ---*/
        &IF "{&hDBOTable8}":U <> "":U &THEN
            IF VALID-HANDLE({&hDBOTable8}) THEN
                DELETE PROCEDURE {&hDBOTable8}.
        &ENDIF
    &ELSE
        /*--- Destr¢i DBO Table8 ---*/
        &IF "{&hDBOTable8}":U <> "":U &THEN
            IF VALID-HANDLE({&hDBOTable8}) THEN
                RUN destroy IN {&hDBOTable8}.
        &ENDIF
    &ENDIF
    
    /*--- Destr¢i programa de range ---*/
    &IF "{&Range}":U = "YES":U &THEN
        IF VALID-HANDLE(hRange) THEN
            DELETE PROCEDURE hRange.
    &ENDIF
    
    /*--- Destr¢i programa de folder ---*/
    &IF "{&Folder}":U = "YES":U &THEN
        IF VALID-HANDLE(hFolder) THEN
            DELETE PROCEDURE hFolder.
    &ENDIF
    
    /*--- Executa programas de customizaá∆o (before/after) ---*/
    {method/custom.i &Event="AFTER-DESTROY-INTERFACE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="?"
                     &RowidTable="?"}
    
    /*--- Executa mÇtodos sobrepostos (before/after) ---*/
    {method/override.i &Position="After"
                       &Procedure="destroyInterface"}
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    /*--- Retira estilo de janela (modal) para thinMaintenace ---*/
    IF VALID-HANDLE(hWindowParent) THEN
        ASSIGN hWindowParent:SENSITIVE = YES.
    
    /*--- Destr¢i os Servidores RPC inicializados pelos DBOs ---*/
    {btb/btb008za.i3}

    /*Alteracao para deletar da mem¢ria o WindowStyles e o btb008za.p*/
    IF VALID-HANDLE(h-servid-rpc) THEN
    DO:
       DELETE PROCEDURE h-servid-rpc.
       ASSIGN h-servid-rpc = ?. /*Garantir que a vari†vel n∆o vai mais apontar para nenhum handle de outro objeto - este problema apareceu na v9.1B com Windows2000*/
    END.

    /*--- Destr¢i janela associada ao programa ---*/
    IF VALID-HANDLE(wZoom) THEN
        DELETE WIDGET wZoom.
    
    /*--- Destr¢i programa ---*/
    IF THIS-PROCEDURE:PERSISTENT THEN
        DELETE PROCEDURE THIS-PROCEDURE.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-dispatch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dispatch Method-Library 
PROCEDURE dispatch :
/*------------------------------------------------------------------------------
  Purpose:     Manter compatibilidade com SmartObjects
  Parameters:  recebe mÇtodo a ser executado
  Notes:       Somente haver† tratamento para o mÇtodo initialize, quando a 
               execuá∆o deste mÇtodo for solicitada ser† executado o mÇtodo
               initializeInterface
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pMethod AS CHARACTER NO-UNDO.
    
    IF pMethod = "INITIALIZE":U THEN
        RUN initializeInterface IN THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-enableImplant) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableImplant Method-Library 
PROCEDURE enableImplant :
/*------------------------------------------------------------------------------
  Purpose:     Habilita bot‰es de implantar
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    /*--- Habilita bot∆o btImplant1 ---*/
    &IF "{&page1Browse}":U <> "":U &THEN
        DO WITH FRAME fPage1:
            ENABLE btImplant1.
        END.
    &ENDIF
    
    /*--- Habilita bot∆o btImplant2 ---*/
    &IF "{&page2Browse}":U <> "":U &THEN
        DO WITH FRAME fPage2:
            ENABLE btImplant2.
        END.
    &ENDIF
    
    /*--- Habilita bot∆o btImplant3 ---*/
    &IF "{&page3Browse}":U <> "":U &THEN
        DO WITH FRAME fPage3:
            ENABLE btImplant3.
        END.
    &ENDIF
    
    /*--- Habilita bot∆o btImplant4 ---*/
    &IF "{&page4Browse}":U <> "":U &THEN
        DO WITH FRAME fPage4:
            ENABLE btImplant4.
        END.
    &ENDIF
    
    /*--- Habilita bot∆o btImplant5 ---*/
    &IF "{&page5Browse}":U <> "":U &THEN
        DO WITH FRAME fPage5:
            ENABLE btImplant5.
        END.
    &ENDIF
    
    /*--- Habilita bot∆o btImplant6 ---*/
    &IF "{&page6Browse}":U <> "":U &THEN
        DO WITH FRAME fPage6:
            ENABLE btImplant6.
        END.
    &ENDIF
    
    /*--- Habilita bot∆o btImplant7 ---*/
    &IF "{&page7Browse}":U <> "":U &THEN
        DO WITH FRAME fPage7:
            ENABLE btImplant7.
        END.
    &ENDIF
    
    /*--- Habilita bot∆o btImplant8 ---*/
    &IF "{&page8Browse}":U <> "":U &THEN
        DO WITH FRAME fPage8:
            ENABLE btImplant8.
        END.
    &ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initializeInterface) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeInterface Method-Library 
PROCEDURE initializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     Inicialize programa
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    /*Alterado por Anderson (tech485) para o template mostrar o nome da empresa*/

    def var c_cod_empres_usuar as char no-undo.
    def var c_nom_razao_social as char no-undo.

    /*fim alteracao Anderson*/


    /*--- Inicializaá∆o de OCXs ---*/
    IF THIS-PROCEDURE:GET-SIGNATURE("control_load":U) <> "":U THEN DO:
        RUN control_load IN THIS-PROCEDURE NO-ERROR.
        VIEW FRAME fPage0 IN WINDOW wZoom.
    END.
    
    /*--- Executa validaá‰es de inicializaá∆o ---*/
    ASSIGN c-programa-mg97 = CAPS("{&Program}":U)
           c-versao-mg97   = "{&Version}":U.
    {utp/ut-vfsec.i}

    /*Inserida a chamada da include i-logini.i
    devido aos thintemplates n∆o gerarem log de 
    execucao de programas*/
    {include/i-logini.i}    

    /*Alterado por Anderson (tech485) para o template mostrar o nome da empresa*/
    /*{utp/ut9000.i "{&Program}" "{&Version}"} *//*esta foi comentada por estar causando erro nos thintemplates*/
    /*rodar pi-rsocial persistent para verificaªío empresa usuario*/
   
    /* Alterado por Valdir (tech264) para novo mÇtodo de teste do handle,
       testando tambÇm os atributos TYPE e FILE-NAME do handle */
       
    if not valid-handle(h-rsocial) or
       h-rsocial:TYPE <> "PROCEDURE":U or
       h-rsocial:FILE-NAME <> "utp/ut-rsocial.p":U then do:
        if l-achou-prog then
            run utp/ut-rsocial.p persistent set h-rsocial.
    end.
    if l-achou-prog then
        run pi-rsocial in h-rsocial (output c_cod_empres_usuar, output c_nom_razao_social).
    
    find prog_dtsul no-lock
        where prog_dtsul.cod_prog_dtsul = c-programa-mg97 no-error.
    if  avail prog_dtsul then do:
        assign c-titulo-prog-mg97    = prog_dtsul.des_prog_dtsul
               c-nom-prog-upc-mg97   = prog_dtsul.nom_prog_upc
               c-nom-prog-appc-mg97  = prog_dtsul.nom_prog_appc
               c-nom-prog-dpc-mg97   = prog_dtsul.nom_prog_dpc
               i-num-topico-hlp-mg97 = prog_dtsul.num_topico.
       &IF DEFINED(TransformacaoWindow) <> 0 &THEN
          if session:window-system <> "TTY":U then 
          &IF '{&emsfnd_version}' >= '1.00'  &THEN
             assign i-template          = prog_dtsul.idi_template.
          &ELSE
             /*assign i-template          = prog_dtsul.ind_template.*/
          &ENDIF
       &ENDIF

        
        find procedimento no-lock
            where procedimento.cod_proced = prog_dtsul.cod_proced no-error.
        if  avail procedimento then do:
            find modul_dtsul no-lock
                where modul_dtsul.cod_modul_dtsul = procedimento.cod_modul_dtsul no-error. 
            if  avail modul_dtsul then do:
                assign c-modulo-mg97         = caps(modul_dtsul.nom_modul_dtsul_menu)
                       c-cod-mod-mg97        = caps(modul_dtsul.cod_modul_dtsul)
                       c-nom-manual-hlp-mg97 = "dochlp~/":U + string(modul_dtsul.num_manual_documen, "999999":U) + ".hlp":U.
            end.
        end.
    end.                                                      
    else do:
        assign c-titulo-prog-mg97    = caps(c-programa-mg97)
               c-nom-prog-upc-mg97   = ""
               c-nom-prog-appc-mg97  = ""
               i-num-topico-hlp-mg97 = 0
               c-nom-manual-hlp-mg97 = "dochlp~/000000.hlp":U.
    end.                 

    /* Traduá∆o T°tulo dos Programas */
    /* FO 1354855  - 11/08/2006 - tech1139 */
    run utp/ut-liter.p (input replace(c-titulo-prog-mg97, " ", "_") ,
                        input "*":U,
                        input "":U). 
    /* FO 1354855  - 11/08/2006 - tech1139 */

    Assign c-titulo-prog-mg97 = Return-value.
     
    &IF  "{&WINDOW-NAME}":U <> "" AND "{&WINDOW-NAME}":U <> "CURRENT-WINDOW":U &THEN 
         assign {&WINDOW-NAME}:title = if l-achou-prog then
                                       c-titulo-prog-mg97
                                     + " - ":U 
                                     + c-programa-mg97 
                                     + " - ":U 
                                     + c-versao-mg97  
                                     + " - ":U 
                                     + c_cod_empres_usuar
                                     + " - ":U 
                                     + c_nom_razao_social
                                     else 
                                       c-titulo-prog-mg97
                                     + " - ":U 
                                     + c-programa-mg97 
                                     + " - ":U 
                                     + c-versao-mg97.
    &ELSE
         assign frame {&FRAME-NAME}:title = if l-achou-prog then
                                              c-titulo-prog-mg97  
                                            + " - ":U 
                                            + c-programa-mg97 
                                            + " - ":U 
                                            + c-versao-mg97
                                            + " - ":U 
                                            + c_cod_empres_usuar
                                            + " - ":U 
                                            + c_nom_razao_social
                                            else
                                              c-titulo-prog-mg97  
                                            + " - ":U 
                                            + c-programa-mg97 
                                            + " - ":U 
                                            + c-versao-mg97.

    &ENDIF
    

    /*fim alteracao Anderson(tech485)*/



    /*--- Executa mÇtodos sobrepostos (before/after) ---*/
    {method/override.i &Position="Before"
                       &Procedure="initializeInterface"}
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN DO:
        /*--- Destr¢i programa ---*/
        RUN destroyInterface IN THIS-PROCEDURE.
        
        RETURN "NOK":U.
    END.
    
    /*--- Executa programas de customizaá∆o (before/after) ---*/
    {method/custom.i &Event="BEFORE-INITIALIZE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="?"
                     &RowidTable="?"}
    
    DO WITH FRAME fPage0:
        
        /*--- Habilitar/Desabilitar bot‰es ---*/
        ASSIGN
            btOK:SENSITIVE     = YES
            btCancel:SENSITIVE = YES
            btHelp:SENSITIVE   = YES.
        
        /*--- Definir tamanhos e posicionar widgets ---*/
        ASSIGN 
            rtToolBar:WIDTH  = FRAME fPage0:WIDTH
            rtToolBar:HEIGHT = 1.46
            rtToolBar:COL    = 1
            rtToolBar:ROW    = FRAME fPage0:HEIGHT - 0.63
            btOK:WIDTH       = 10
            btOK:HEIGHT      = 1
            btOK:COL         = 2
            btOK:ROW         = rtToolBar:ROW + 0.23
            btCancel:WIDTH   = 10
            btCancel:HEIGHT  = 1
            btCancel:COL     = 13
            btCancel:ROW     = rtToolBar:ROW + 0.23
            btHelp:WIDTH     = 10
            btHelp:HEIGHT    = 1
            btHelp:COL       = 80
            btHelp:ROW       = rtToolBar:ROW + 0.23.
    END.
    
    /*--- Executa programa de range ---*/
    &IF "{&Range}":U = "YES":U &THEN
        RUN utp/makerange.p PERSISTENT SET hRange.
    &ENDIF
    
    /*--- Executa programa de folder ---*/
    &IF "{&Folder}":U = "YES":U &THEN
        RUN utp/thinfolder.w PERSISTENT SET hFolder.
        
        RUN setProgramParent IN hFolder (INPUT THIS-PROCEDURE).
        
        RUN setOCX IN hFolder (INPUT THIS-PROCEDURE:GET-SIGNATURE("control_load":U) <> "":U) NO-ERROR.
        
        RUN initializeFolders IN hFolder (INPUT FRAME fPage0:HANDLE,
                                          INPUT &IF NUM-ENTRIES("{&FolderLabels}":U) >= 1 &THEN
                                                    STRING(FRAME fPage1:HANDLE)
                                                &ENDIF

                                                &IF NUM-ENTRIES("{&FolderLabels}":U) >= 2 &THEN
                                                        + ",":U + STRING(FRAME fPage2:HANDLE)
                                                &ENDIF

                                                &IF NUM-ENTRIES("{&FolderLabels}":U) >= 3 &THEN
                                                    + ",":U + STRING(FRAME fPage3:HANDLE)
                                                &ENDIF

                                                &IF NUM-ENTRIES("{&FolderLabels}":U) >= 4 &THEN
                                                    + ",":U + STRING(FRAME fPage4:HANDLE)
                                                &ENDIF

                                                &IF NUM-ENTRIES("{&FolderLabels}":U) >= 5 &THEN
                                                    + ",":U + STRING(FRAME fPage5:HANDLE)
                                                &ENDIF                     
 
                                                &IF NUM-ENTRIES("{&FolderLabels}":U) >= 6 &THEN
                                                    + ",":U + STRING(FRAME fPage6:HANDLE)
                                                &ENDIF                     
 
                                                &IF NUM-ENTRIES("{&FolderLabels}":U) >= 7 &THEN
                                                    + ",":U + STRING(FRAME fPage7:HANDLE)
                                                &ENDIF                     
 
                                                &IF NUM-ENTRIES("{&FolderLabels}":U) = 8 &THEN
                                                    + ",":U + STRING(FRAME fPage8:HANDLE)
                                                &ENDIF,
                                          INPUT "{&FolderLabels}":U,
                                          INPUT NUM-ENTRIES("{&FolderLabels}":U),
                                          INPUT ?,
                                          INPUT ?,
                                          INPUT ?,
                                          
                                          /*--- Est† sendo utilizada a f¢rmula abaixo porquà 
                                                a barra de bot‰es fica na parte inferior da frame ---*/
                                          INPUT FRAME fPage0:HEIGHT - FRAME fPage1:ROW - 0.75 ).

        &IF "{&Range}":U = "YES":U &THEN
            RUN setVariable in hFolder (input hRange).
        &ENDIF

    &ENDIF
    
    /*--- Habilitar browse contido no {&page1Browse} ---*/
    &IF "{&page1Browse}":U <> "":U &THEN
        DO WITH FRAME fPage1:
            ENABLE {&page1Browse}.
        END.
    &ENDIF
    
    /*--- Habilitar browse contido no {&page2Browse} ---*/
    &IF "{&page2Browse}":U <> "":U &THEN
        DO WITH FRAME fPage2:
            ENABLE {&page2Browse}.
        END.
    &ENDIF
    
    /*--- Habilitar browse contido no {&page3Browse} ---*/
    &IF "{&page3Browse}":U <> "":U &THEN
        DO WITH FRAME fPage3:
            ENABLE {&page3Browse}.
        END.
    &ENDIF
    
    /*--- Habilitar browse contido no {&page4Browse} ---*/
    &IF "{&page4Browse}":U <> "":U &THEN
        DO WITH FRAME fPage4:
            ENABLE {&page4Browse}.
        END.
    &ENDIF
    
    /*--- Habilitar browse contido no {&page5Browse} ---*/
    &IF "{&page5Browse}":U <> "":U &THEN
        DO WITH FRAME fPage5:
            ENABLE {&page5Browse}.
        END.
    &ENDIF
    
    /*--- Habilitar browse contido no {&page6Browse} ---*/
    &IF "{&page6Browse}":U <> "":U &THEN
        DO WITH FRAME fPage6:
            ENABLE {&page6Browse}.
        END.
    &ENDIF
    
    /*--- Habilitar browse contido no {&page7Browse} ---*/
    &IF "{&page7Browse}":U <> "":U &THEN
        DO WITH FRAME fPage7:
            ENABLE {&page7Browse}.
        END.
    &ENDIF
    
    /*--- Habilitar browse contido no {&page8Browse} ---*/
    &IF "{&page8Browse}":U <> "":U &THEN
        DO WITH FRAME fPage8:
            ENABLE {&page8Browse}.
        END.
    &ENDIF
    
    /*--- Executa programa de estilo de janelas ---*/
    IF  VALID-HANDLE(hWindowStyles) = NO OR
        hWindowStyles:TYPE <> "PROCEDURE":U OR
        (hWindowStyles:FILE-NAME <> "utp/WindowStyles.p":U AND
        hWindowStyles:FILE-NAME <> "utp/WindowStyles.r":U) THEN
        RUN utp/windowstyles.p PERSISTENT SET hWindowStyles.
    
    /*--- Seta estilo de janela para thinMaintenance ---*/
    RUN deleteMinMax IN hWindowStyles (INPUT wZoom:hWnd).

    /*Colocado esta l¢gica para caso o desenvolvedor passar o handle da bo j† 
     estartado o mesmo n∆o mostrar erros de execuá‰es antigas sa inicializaá∆o 
     do programa Feita por Anderson tech485 28/08/2202*/
    &IF DEFINED(DBOVersion) = 0 &THEN
        &IF "{&hDBOTable1}":U <> "":U &THEN
             IF VALID-HANDLE({&hDBOTable1}) THEN
                 RUN emptyrowerrors IN {&hDBOTable1} NO-ERROR.
        &ENDIF
        &IF "{&hDBOTable2}":U <> "":U &THEN
             IF VALID-HANDLE({&hDBOTable2}) THEN
                 RUN emptyrowerrors IN {&hDBOTable2} NO-ERROR.
        &ENDIF
        &IF "{&hDBOTable3}":U <> "":U &THEN
             IF VALID-HANDLE({&hDBOTable3}) THEN 
                 RUN emptyrowerrors IN {&hDBOTable3} NO-ERROR.
        &ENDIF
        &IF "{&hDBOTable4}":U <> "":U &THEN
             IF VALID-HANDLE({&hDBOTable4}) THEN
                 RUN emptyrowerrors IN {&hDBOTable4} NO-ERROR.
        &ENDIF
        &IF "{&hDBOTable5}":U <> "":U &THEN
             IF VALID-HANDLE({&hDBOTable5}) THEN
                 RUN emptyrowerrors IN {&hDBOTable5} NO-ERROR.
        &ENDIF
        &IF "{&hDBOTable6}":U <> "":U &THEN
             IF VALID-HANDLE({&hDBOTable6}) THEN
                 RUN emptyrowerrors IN {&hDBOTable6} NO-ERROR.
        &ENDIF
        &IF "{&hDBOTable7}":U <> "":U &THEN
             IF VALID-HANDLE({&hDBOTable7}) THEN
                 RUN emptyrowerrors IN {&hDBOTable7} NO-ERROR.
        &ENDIF
        &IF "{&hDBOTable8}":U <> "":U &THEN
             IF VALID-HANDLE({&hDBOTable8}) THEN 
                 RUN emptyrowerrors IN {&hDBOTable8} NO-ERROR.
        &ENDIF
    &ENDIF
    /*fim alteraá∆o Anderson tech485 28/08/2202*/
    
    /*--- Inicializa DBOs ---*/
    if session:set-wait-state("general":U) then.
    RUN initializeDBOs IN THIS-PROCEDURE.
    if session:set-wait-state("") then.
    
    /*--- Verifica se a inicializaá∆o dos DBOs foi feita com sucesso ---*/
    IF NO
        &IF "{&hDBOTable1}":U <> "":U &THEN
            OR NOT VALID-HANDLE({&hDBOTable1}) 
        &ENDIF
            
        &IF "{&hDBOTable2}":U <> "":U &THEN
            OR NOT VALID-HANDLE({&hDBOTable2})
        &ENDIF
        
        &IF "{&hDBOTable3}":U <> "":U &THEN
            OR NOT VALID-HANDLE({&hDBOTable3})
        &ENDIF
        
        &IF "{&hDBOTable4}":U <> "":U &THEN
            OR NOT VALID-HANDLE({&hDBOTable4})
        &ENDIF
        
        &IF "{&hDBOTable5}":U <> "":U &THEN
            OR NOT VALID-HANDLE({&hDBOTable5})
        &ENDIF
        
        &IF "{&hDBOTable6}":U <> "":U &THEN
            OR NOT VALID-HANDLE({&hDBOTable6})
        &ENDIF
        
        &IF "{&hDBOTable7}":U <> "":U &THEN
            OR NOT VALID-HANDLE({&hDBOTable7})
        &ENDIF
        
        &IF "{&hDBOTable8}":U <> "":U &THEN
            OR NOT VALID-HANDLE({&hDBOTable8})
        &ENDIF
        
    THEN DO:
        /*--- Exibir mensagem de finalizaá∆o do programa ---*/
        RUN utp/ut-msgs.p (INPUT "SHOW":U, 
                           INPUT 18881, 
                           INPUT CAPS("{&Program}":U) + "~~":U + CAPS("{&Version}":U)).
        
        /*--- Destr¢i programa ---*/
        RUN destroyInterface IN THIS-PROCEDURE.
        
        RETURN "NOK":U.
    END.
    &IF DEFINED(DBOVersion) = 0 &THEN
    ELSE DO:
        /*Foi inclu°da esta l¢gica para verificaá∆o de erros relacionados com 
         a permiss∆o de execuá∆o do programa de dbo Alteraá∆o em 17/06/2002 por Anderson(tech540)*/
         &IF "{&hDBOTable1}":U <> "":U &THEN
              RUN getrowerrors IN {&hDBOTable1} (OUTPUT TABLE rowerrors).
                IF CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorNumber <> 3 AND
                                                  RowErrors.ErrorNumber <> 8 AND
                                                  RowErrors.ErrorNumber <> 10 AND
                                                  RowErrors.ErrorSubType = "ERROR":U) THEN DO:
                  {method/showmessage.i1}
                  {method/showmessage.i2}
                   /****
                    TECH14187 - 1538059
                    Problema da ShowMessage Modal
                    ****/
                   &IF "{&Modal}":U = "":U &THEN
                        WAIT-FOR CLOSE OF hShowMsg.
                   &ENDIF
                  {method/showmessage.i3}
                  RUN destroyInterface IN THIS-PROCEDURE.
                  RETURN "NOK":U.
              END.
         &ENDIF
         
         &IF "{&hDBOTable2}":U <> "":U &THEN
              RUN getrowerrors IN {&hDBOTable2} (OUTPUT TABLE rowerrors).
              IF CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorNumber <> 3 AND
                                                RowErrors.ErrorNumber <> 8 AND
                                                RowErrors.ErrorNumber <> 10 AND
                                                RowErrors.ErrorSubType = "ERROR":U) THEN DO:
                  {method/showmessage.i1}
                  {method/showmessage.i2}
                   /****
                    TECH14187 - 1538059
                    Problema da ShowMessage Modal
                    ****/
                   &IF "{&Modal}":U = "":U &THEN
                        WAIT-FOR CLOSE OF hShowMsg.
                   &ENDIF
                  {method/showmessage.i3}
                  RUN destroyInterface IN THIS-PROCEDURE.
                  RETURN "NOK":U.
              END.
         &ENDIF
         
         &IF "{&hDBOTable3}":U <> "":U &THEN
              RUN getrowerrors IN {&hDBOTable3} (OUTPUT TABLE rowerrors).
              IF CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorNumber <> 3 AND
                                                RowErrors.ErrorNumber <> 8 AND
                                                RowErrors.ErrorNumber <> 10 AND
                                                RowErrors.ErrorSubType = "ERROR":U) THEN DO:
                  {method/showmessage.i1}
                  {method/showmessage.i2}
                   /****
                    TECH14187 - 1538059
                    Problema da ShowMessage Modal
                    ****/
                   &IF "{&Modal}":U = "":U &THEN
                        WAIT-FOR CLOSE OF hShowMsg.
                   &ENDIF
                  {method/showmessage.i3}
                  RUN destroyInterface IN THIS-PROCEDURE.
                  RETURN "NOK":U.
              END.
         &ENDIF
          
         &IF "{&hDBOTable4}":U <> "":U &THEN
              RUN getrowerrors IN {&hDBOTable4} (OUTPUT TABLE rowerrors).
                IF CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorNumber <> 3 AND
                                                  RowErrors.ErrorNumber <> 8 AND
                                                  RowErrors.ErrorNumber <> 10 AND
                                                  RowErrors.ErrorSubType = "ERROR":U) THEN DO:
                  {method/showmessage.i1}
                  {method/showmessage.i2}
                   /****
                    TECH14187 - 1538059
                    Problema da ShowMessage Modal
                    ****/
                   &IF "{&Modal}":U = "":U &THEN
                        WAIT-FOR CLOSE OF hShowMsg.
                   &ENDIF
                  {method/showmessage.i3}
                  RUN destroyInterface IN THIS-PROCEDURE.
                  RETURN "NOK":U.
              END.
         &ENDIF
          
         &IF "{&hDBOTable5}":U <> "":U &THEN
              RUN getrowerrors IN {&hDBOTable5} (OUTPUT TABLE rowerrors).
              IF CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorNumber <> 3 AND
                                                RowErrors.ErrorNumber <> 8 AND
                                                RowErrors.ErrorNumber <> 10 AND
                                                RowErrors.ErrorSubType = "ERROR":U) THEN DO:
                  {method/showmessage.i1}
                  {method/showmessage.i2}
                   /****
                    TECH14187 - 1538059
                    Problema da ShowMessage Modal
                    ****/
                   &IF "{&Modal}":U = "":U &THEN
                        WAIT-FOR CLOSE OF hShowMsg.
                   &ENDIF
                  {method/showmessage.i3}
                  RUN destroyInterface IN THIS-PROCEDURE.
                  RETURN "NOK":U.
              END.
         &ENDIF
            
         &IF "{&hDBOTable6}":U <> "":U &THEN
              RUN getrowerrors IN {&hDBOTable6} (OUTPUT TABLE rowerrors).
              IF CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorNumber <> 3 AND
                                                RowErrors.ErrorNumber <> 8 AND
                                                RowErrors.ErrorNumber <> 10 AND
                                                RowErrors.ErrorSubType = "ERROR":U) THEN DO:
                  {method/showmessage.i1}
                  {method/showmessage.i2}
                   /****
                    TECH14187 - 1538059
                    Problema da ShowMessage Modal
                    ****/
                   &IF "{&Modal}":U = "":U &THEN
                        WAIT-FOR CLOSE OF hShowMsg.
                   &ENDIF
                  {method/showmessage.i3}
                  RUN destroyInterface IN THIS-PROCEDURE.
                  RETURN "NOK":U.
              END.
         &ENDIF
          
         &IF "{&hDBOTable7}":U <> "":U &THEN
              RUN getrowerrors IN {&hDBOTable7} (OUTPUT TABLE rowerrors).
              IF CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorNumber <> 3 AND
                                                RowErrors.ErrorNumber <> 8 AND
                                                RowErrors.ErrorNumber <> 10 AND
                                                RowErrors.ErrorSubType = "ERROR":U) THEN DO:
                  {method/showmessage.i1}
                  {method/showmessage.i2}
                   /****
                    TECH14187 - 1538059
                    Problema da ShowMessage Modal
                    ****/
                   &IF "{&Modal}":U = "":U &THEN
                        WAIT-FOR CLOSE OF hShowMsg.
                   &ENDIF
                  {method/showmessage.i3}
                  RUN destroyInterface IN THIS-PROCEDURE.
                  RETURN "NOK":U.
              END.
         &ENDIF
          
         &IF "{&hDBOTable8}":U <> "":U &THEN
              RUN getrowerrors IN {&hDBOTable8} (OUTPUT TABLE rowerrors).
              IF CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorNumber <> 3 AND
                                                RowErrors.ErrorNumber <> 8 AND
                                                RowErrors.ErrorNumber <> 10 AND
                                                RowErrors.ErrorSubType = "ERROR":U) THEN DO:
                  {method/showmessage.i1}
                  {method/showmessage.i2}
                   /****
                    TECH14187 - 1538059
                    Problema da ShowMessage Modal
                    ****/
                   &IF "{&Modal}":U = "":U &THEN
                        WAIT-FOR CLOSE OF hShowMsg.
                   &ENDIF
                  {method/showmessage.i3}
                  RUN destroyInterface IN THIS-PROCEDURE.
                  RETURN "NOK":U.
              END.
         &ENDIF
    END.
    &ENDIF

    /*--- Atualiza browsers ---*/
    RUN openQueries IN THIS-PROCEDURE.
    
    &IF "{&Range}":U = "YES":U &THEN
        &IF "{&FieldNames}":U <> "" &THEN
            RUN initializeRange IN hRange (INPUT THIS-PROCEDURE:HANDLE,
                                   INPUT &IF NUM-ENTRIES("{&FolderLabels}":U) >= 1 &THEN
                                                STRING(FRAME fPage1:HANDLE)
                                            &ENDIF

                                            &IF NUM-ENTRIES("{&FolderLabels}":U) >= 2 &THEN
                                                    + ",":U + STRING(FRAME fPage2:HANDLE)
                                            &ENDIF

                                            &IF NUM-ENTRIES("{&FolderLabels}":U) >= 3 &THEN
                                                + ",":U + STRING(FRAME fPage3:HANDLE)
                                            &ENDIF

                                            &IF NUM-ENTRIES("{&FolderLabels}":U) >= 4 &THEN
                                                + ",":U + STRING(FRAME fPage4:HANDLE)
                                            &ENDIF

                                            &IF NUM-ENTRIES("{&FolderLabels}":U) >= 5 &THEN
                                                + ",":U + STRING(FRAME fPage5:HANDLE)
                                            &ENDIF                     
 
                                            &IF NUM-ENTRIES("{&FolderLabels}":U) >= 6 &THEN
                                                + ",":U + STRING(FRAME fPage6:HANDLE)
                                            &ENDIF                     
 
                                            &IF NUM-ENTRIES("{&FolderLabels}":U) >= 7 &THEN
                                                + ",":U + STRING(FRAME fPage7:HANDLE)
                                            &ENDIF                     
 
                                            &IF NUM-ENTRIES("{&FolderLabels}":U) = 8 &THEN
                                                + ",":U + STRING(FRAME fPage8:HANDLE)
                                            &ENDIF,
                                   INPUT "{&FieldNames}":U,
                                   INPUT NUM-ENTRIES("{&FolderLabels}":U),
                                   OUTPUT hFirstField).
        &ENDIF
        &IF "{&Range}":U = "YES":U &THEN
        IF VALID-HANDLE(hRange) THEN 
            run setahandlefolder in hRange (input hFolder).
        &ENDIF
        &IF "{&FieldsRangePage1}":U <> "" and
            "{&FieldNames}":U   = "" &THEN
            RUN initializeMultipleRange IN hRange (INPUT THIS-PROCEDURE:HANDLE,
                                                   INPUT STRING(FRAME fPage1:HANDLE),
                                                   INPUT "{&FieldsRangePage1}":U,
                                                   INPUT "{&FieldsAnyKeyPage1}",
                                                   OUTPUT hFirstField).
        &ENDIF
        &IF "{&FieldsRangePage2}":U <> "" and
            "{&FieldNames}":U   = "" &THEN
            RUN initializeMultipleRange IN hRange (INPUT THIS-PROCEDURE:HANDLE,
                                                   INPUT STRING(FRAME fPage2:HANDLE),
                                                   INPUT "{&FieldsRangePage2}":U,
                                                   INPUT "{&FieldsAnyKeyPage2}",
                                                   OUTPUT hFirstField).
        &ENDIF
        &IF "{&FieldsRangePage3}":U <> "" and
            "{&FieldNames}":U   = "" &THEN
            RUN initializeMultipleRange IN hRange (INPUT THIS-PROCEDURE:HANDLE,
                                                   INPUT STRING(FRAME fPage3:HANDLE),
                                                   INPUT "{&FieldsRangePage3}":U,
                                                   INPUT "{&FieldsAnyKeyPage3}":U,
                                                   OUTPUT hFirstField).
        &ENDIF
        &if "{&FieldsRangePage4}":U <> "" and
            "{&FieldNames}":U   = "" &THEN
            RUN initializeMultipleRange IN hRange (INPUT THIS-PROCEDURE:HANDLE,
                                                   INPUT STRING(FRAME fPage4:HANDLE),
                                                   INPUT "{&FieldsRangePage4}":U,
                                                   INPUT "{&FieldsAnyKeyPage4}":U,
                                                   OUTPUT hFirstField).
        &ENDIF
        &IF "{&FieldsRangePage5}":U <> "" and
            "{&FieldNames}":U   = "" &THEN
            RUN initializeMultipleRange IN hRange (INPUT THIS-PROCEDURE:HANDLE,
                                                   INPUT STRING(FRAME fPage5:HANDLE),
                                                   INPUT "{&FieldsRangePage5}":U,
                                                   INPUT "{&FieldsAnyKeyPage5}":U,
                                                   OUTPUT hFirstField).
        &ENDIF
        &IF "{&FieldsRangePage6}":U <> "" and
            "{&FieldNames}":U   = "" &THEN
            RUN initializeMultipleRange IN hRange (INPUT THIS-PROCEDURE:HANDLE,
                                                   INPUT STRING(FRAME fPage6:HANDLE),
                                                   INPUT "{&FieldsRangePage6}":U,
                                                   INPUT "{&FieldsAnyKeyPage6}":U,
                                                   OUTPUT hFirstField).
        &ENDIF
        &IF "{&FieldsRangePage7}":U <> "" and
            "{&FieldNames}":U   = "" &THEN
            RUN initializeMultipleRange IN hRange (INPUT THIS-PROCEDURE:HANDLE,
                                                   INPUT STRING(FRAME fPage7:HANDLE),
                                                   INPUT "{&FieldsRangePage7}":U,
                                                   INPUT "{&FieldsAnyKeyPage7}":U,
                                                   OUTPUT hFirstField).
        &ENDIF
        &IF "{&FieldsRangePage8}":U <> "" and
            "{&FieldNames}":U   = "" &THEN
            RUN initializeMultipleRange IN hRange (INPUT THIS-PROCEDURE:HANDLE,
                                                   INPUT STRING(FRAME fPage8:HANDLE),
                                                   INPUT "{&FieldsRangePage8}":U,
                                                   INPUT "{&FieldsAnyKeyPage8}":U,
                                                   OUTPUT hFirstField).
        &ENDIF
    &ENDIF
    
    &IF "{&Folder}":U = "YES":U &THEN
        RUN setFolder IN hFolder (INPUT {&InitialPage}).
    &ENDIF
    
    /*--- Executa programas de customizaá∆o (before/after) ---*/
    {method/custom.i &Event="AFTER-INITIALIZE"
                     &Object="CONTAINER"
                     &ObjectHandle="THIS-PROCEDURE:HANDLE"
                     &FrameHandle="FRAME fPage0:HANDLE"
                     &Table="?"
                     &RowidTable="?"}
    
    /*--- Executa mÇtodos sobrepostos (before/after) ---*/
    {method/override.i &Position="After"
                       &Procedure="initializeInterface"}
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN DO:
        /*--- Destr¢i programa ---*/
        RUN destroyInterface IN THIS-PROCEDURE.
        
        RETURN "NOK":U.
    END.

    /*** Alterado por Farley - em 23/07/2003 ***/
    &IF "{&FNC_MULTI_IDIOMA}":U = "YES":U &THEN
        /*** Alterado por Paulo H. Lazzarotti em 09/07/2003
             Esta validaá∆o Ç provis¢ria, atÇ que todos os clientes
             tenham este utilit†rio em seus ambientes ****/
        IF SEARCH("utp/ut-trcampos.r":U) <> ? OR 
           SEARCH("utp/ut-trcampos.p":U) <> ? THEN
            /*ALteraá∆o Anderson  tech540 em 12/05/2003 para o projeto de 
            traduá∆o onde este ir† traduzir os valores (labels) dos objetos em telas*/
            RUN utp/ut-trcampos.p.
            /*fim alteraá∆o Anderson 12/05/2003*/
    &ENDIF
    
    /*--- Visualiza janela ---*/
    VIEW wZoom.

    /*Alteracao 24/01/2006 - tech14207 - Alterado para desabilitar o programa pai*/
    /*O c¢digo foi alterado para permitir que o mesmo programa fosse executado mais de uma vez corretamente*/
    &if integer(entry(1,proversion,".")) >= 9 &then
        DEFINE VAR h-source AS HANDLE     NO-UNDO.
        DEFINE VARIABLE h-this AS HANDLE     NO-UNDO.

        ASSIGN h-source = SOURCE-PROCEDURE.
        ASSIGN h-this = THIS-PROCEDURE.
        
        IF (THIS-PROCEDURE <> SOURCE-PROCEDURE) THEN DO:
            ASSIGN h-source = SOURCE-PROCEDURE.
            /*26/09/2006 - tech30713 - FO: 1309021*/
            IF VALID-HANDLE(h-source) AND  VALID-HANDLE(h-source:CURRENT-WINDOW) THEN DO:
                ASSIGN hWindowParent           = h-source:CURRENT-WINDOW
                       hWindowParent:SENSITIVE = NO.
            END.
        END.
    &ELSE
        DEFINE VAR h-prog AS HANDLE     NO-UNDO.
        DEFINE VARIABLE c-programa AS CHARACTER  NO-UNDO.

        IF NUM-ENTRIES(PROGRAM-NAME(2)," ") > 1 THEN DO:
            ASSIGN c-programa = ENTRY(NUM-ENTRIES(PROGRAM-NAME(2)," "), PROGRAM-NAME(2)," ").
        END.
        ELSE DO:
            ASSIGN c-programa = ENTRY(1,PROGRAM-NAME(2)," ").
        END.                   
        IF INDEX(PROGRAM-NAME(1), c-programa) = 0 THEN DO:
            ASSIGN h-prog = SESSION:FIRST-PROCEDURE.
            DO WHILE h-prog <> ?:                   
                IF h-prog:FILE-NAME = c-programa THEN DO:
                   LEAVE.
                END.  
                ASSIGN h-prog = h-prog:NEXT-SIBLING.
            END.               
            IF VALID-HANDLE(h-prog) THEN DO:
                ASSIGN hWindowParent           = h-prog:CURRENT-WINDOW
                       hWindowParent:SENSITIVE = NO.
            END.
        END.
    &ENDIF                                          
    /*Fim da alteraá∆o 24/01/2006 - tech14207*/

    
    IF  VALID-HANDLE(hFirstField) THEN
        APPLY "ENTRY":U TO hFirstField.

    /*Alteracao 24/01/2006 - tech14207 - Alterado para desabilitar o programa pai*/
    /*O c¢digo foi alterado para permitir que o mesmo programa fosse executado mais de uma vez corretamente*/    
    /*--- Seta estilo de janela modal para thinMaintenance ---*/
/*     ASSIGN hWindowParent           = SESSION:FIRST-CHILD        */
/*            hWindowParent           = hWindowParent:NEXT-SIBLING */
/*            hWindowParent:SENSITIVE = NO.                        */
    /*Fim da alteraá∆o 24/01/2006 - tech14207*/
    

    /*Alteraá∆o 09/03/2007 - tech1007/tech30713 - FO 1465451 - Alterado para setar o foco corretamente quando for Progress 9.1B*/
    &IF LENGTH(PROVERSION) = 4 &THEN
    &SCOPED-DEFINE ProVers 00 
    &ELSE
    &SCOPED-DEFINE ProVers 0
    &ENDIF
    &if ('{&ProVers}':U + PROVERSION) <> '009.1B':U &then
        APPLY "ENTRY":U TO FRAME fPage0.
    &endif.    

    APPLY "ENTRY":U TO wZoom.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-repositionRecordInPage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repositionRecordInPage Method-Library 
PROCEDURE repositionRecordInPage :
/*------------------------------------------------------------------------------
  Purpose:     Reposiciona DBO table atravÇs de um rowid
  Parameters:  recebe rowid
               recebe n£mero da p†gina
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pRowid      AS ROWID   NO-UNDO.
    DEFINE INPUT PARAMETER pPageNumber AS INTEGER NO-UNDO.
    
    /*--- Seta vari†vel iRepositionPageNumber com o n£mero da p†gina na qual
          o browse filho ser† reposicionado ---*/
    /*--- Seta vari†vel rReposition com o rowid a ser reposicionado no 
          browse filho ---*/
    ASSIGN iRepositionPageNumber = pPageNumber
           rRepositionTable      = pRowid.
    
    /*--- Atualiza browse mas somente da p†gina iRepositionPageNumber ---*/
    RUN openQueries IN THIS-PROCEDURE.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFieldNamesHandles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setFieldNamesHandles Method-Library 
PROCEDURE setFieldNamesHandles :
/*------------------------------------------------------------------------------
  Purpose:     Seta nome dos campos a serem retornados
  Parameters:  recebe nome dos campos
               recebe handle dos campos
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pFieldNames   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pFieldHandles AS CHARACTER NO-UNDO.
    
    /*--- Atualiza vari†vel cFieldNames com o nome dos campos a serem 
          retornados ---*/
    ASSIGN cFieldNames   = pFieldNames
           cFieldHandles = pFieldHandles.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-changePage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changePage Method-Library 
PROCEDURE changePage :
/*------------------------------------------------------------------------------
  Purpose:     MÇtodo executado pelo programa de Folder, quando h† troca de 
               p†gina
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
def var pageN as integer no-undo.
    
    /*--- Executa mÇtodos sobrepostos (before/after) ---*/
    {method/override.i &Position="Before"
                       &Procedure="changePage"}
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.

    RUN GetCurrentFolder IN hFolder (OUTPUT PageN).
    
    Case pageN:
    &IF "{&page1Browse}":U <> "" &THEN
    when 1 then do:
        IF AVAILABLE {&ttTable1} THEN 
            ASSIGN epc-rowid1 = {&ttTable1}.r-Rowid.
        ELSE 
            ASSIGN epc-rowid1 = ?.
    
        /*--- Executa programas de customizaá∆o (before/after) ---*/
        {method/custom.i &Event="BEFORE-CHANGE-PAGE"
                         &Object="CONTAINER"
                         &ObjectHandle="THIS-PROCEDURE:HANDLE"
                         &FrameHandle="FRAME fPage0:HANDLE"
                         &Table="{&ttTable1}"
                         &RowidTable="epc-rowid1"}
        
        /*--- Executa programas de customizaá∆o (before/after) ---*/
        {method/custom.i &Event="AFTER-CHANGE-PAGE"
                         &Object="CONTAINER"
                         &ObjectHandle="THIS-PROCEDURE:HANDLE"
                         &FrameHandle="FRAME fPage0:HANDLE"
                         &Table="{&ttTable1}"
                         &RowidTable="epc-rowid1"}
    end.
    &ENDIF
    &IF "{&page2Browse}":U <> "" &THEN
    when 2 then do:
        IF AVAILABLE {&ttTable2} THEN 
            ASSIGN epc-rowid2 = {&ttTable2}.r-Rowid.
        ELSE 
            ASSIGN epc-rowid2 = ?.
    
        /*--- Executa programas de customizaá∆o (before/after) ---*/
        {method/custom.i &Event="BEFORE-CHANGE-PAGE"
                         &Object="CONTAINER"
                         &ObjectHandle="THIS-PROCEDURE:HANDLE"
                         &FrameHandle="FRAME fPage0:HANDLE"
                         &Table="{&ttTable2}"
                         &RowidTable="epc-rowid2"}
        
        /*--- Executa programas de customizaá∆o (before/after) ---*/
        {method/custom.i &Event="AFTER-CHANGE-PAGE"
                         &Object="CONTAINER"
                         &ObjectHandle="THIS-PROCEDURE:HANDLE"
                         &FrameHandle="FRAME fPage0:HANDLE"
                         &Table="{&ttTable2}"
                         &RowidTable="epc-rowid2"}
    end.    
    &ENDIF
    &IF "{&page3Browse}":U <> "" &THEN
    when 3 then do:
        IF AVAILABLE {&ttTable3} THEN 
            ASSIGN epc-rowid3 = {&ttTable3}.r-Rowid.
        ELSE 
            ASSIGN epc-rowid3 = ?.
    
        /*--- Executa programas de customizaá∆o (before/after) ---*/
        {method/custom.i &Event="BEFORE-CHANGE-PAGE"
                         &Object="CONTAINER"
                         &ObjectHandle="THIS-PROCEDURE:HANDLE"
                         &FrameHandle="FRAME fPage0:HANDLE"
                         &Table="{&ttTable3}"
                         &RowidTable="epc-rowid3"}
        
        /*--- Executa programas de customizaá∆o (before/after) ---*/
        {method/custom.i &Event="AFTER-CHANGE-PAGE"
                         &Object="CONTAINER"
                         &ObjectHandle="THIS-PROCEDURE:HANDLE"
                         &FrameHandle="FRAME fPage0:HANDLE"
                         &Table="{&ttTable3}"
                         &RowidTable="epc-rowid3"}
    end.    
    &ENDIF
    &IF "{&page4Browse}":U <> "" &THEN
    when 4 then do:
        IF AVAILABLE {&ttTable4} THEN 
            ASSIGN epc-rowid4 = {&ttTable4}.r-Rowid.
        ELSE 
            ASSIGN epc-rowid4 = ?.
    
        /*--- Executa programas de customizaá∆o (before/after) ---*/
        {method/custom.i &Event="BEFORE-CHANGE-PAGE"
                         &Object="CONTAINER"
                         &ObjectHandle="THIS-PROCEDURE:HANDLE"
                         &FrameHandle="FRAME fPage0:HANDLE"
                         &Table="{&ttTable4}"
                         &RowidTable="epc-rowid4"}
        
        /*--- Executa programas de customizaá∆o (before/after) ---*/
        {method/custom.i &Event="AFTER-CHANGE-PAGE"
                         &Object="CONTAINER"
                         &ObjectHandle="THIS-PROCEDURE:HANDLE"
                         &FrameHandle="FRAME fPage0:HANDLE"
                         &Table="{&ttTable4}"
                         &RowidTable="epc-rowid4"}
    end.    
    &ENDIF
    &IF "{&page5Browse}":U <> "" &THEN
    when 5 then do:
        IF AVAILABLE {&ttTable5} THEN 
            ASSIGN epc-rowid5 = {&ttTable5}.r-Rowid.
        ELSE 
            ASSIGN epc-rowid5 = ?.
    
        /*--- Executa programas de customizaá∆o (before/after) ---*/
        {method/custom.i &Event="BEFORE-CHANGE-PAGE"
                         &Object="CONTAINER"
                         &ObjectHandle="THIS-PROCEDURE:HANDLE"
                         &FrameHandle="FRAME fPage0:HANDLE"
                         &Table="{&ttTable5}"
                         &RowidTable="epc-rowid5"}
        
        /*--- Executa programas de customizaá∆o (before/after) ---*/
        {method/custom.i &Event="AFTER-CHANGE-PAGE"
                         &Object="CONTAINER"
                         &ObjectHandle="THIS-PROCEDURE:HANDLE"
                         &FrameHandle="FRAME fPage0:HANDLE"
                         &Table="{&ttTable5}"
                         &RowidTable="epc-rowid5"}
    end.    
    &ENDIF
    &IF "{&page6Browse}":U <> "" &THEN
    when 6 then do:
        IF AVAILABLE {&ttTable6} THEN 
            ASSIGN epc-rowid6 = {&ttTable6}.r-Rowid.
        ELSE 
            ASSIGN epc-rowid6 = ?.
    
        /*--- Executa programas de customizaá∆o (before/after) ---*/
        {method/custom.i &Event="BEFORE-CHANGE-PAGE"
                         &Object="CONTAINER"
                         &ObjectHandle="THIS-PROCEDURE:HANDLE"
                         &FrameHandle="FRAME fPage0:HANDLE"
                         &Table="{&ttTable6}"
                         &RowidTable="epc-rowid6"}
        
        /*--- Executa programas de customizaá∆o (before/after) ---*/
        {method/custom.i &Event="AFTER-CHANGE-PAGE"
                         &Object="CONTAINER"
                         &ObjectHandle="THIS-PROCEDURE:HANDLE"
                         &FrameHandle="FRAME fPage0:HANDLE"
                         &Table="{&ttTable6}"
                         &RowidTable="epc-rowid6"}
    end.    
    &ENDIF
    &IF "{&page7Browse}":U <> "" &THEN
    when 7 then do:
        IF AVAILABLE {&ttTable7} THEN 
            ASSIGN epc-rowid7 = {&ttTable7}.r-Rowid.
        ELSE 
            ASSIGN epc-rowid7 = ?.
    
        /*--- Executa programas de customizaá∆o (before/after) ---*/
        {method/custom.i &Event="BEFORE-CHANGE-PAGE"
                         &Object="CONTAINER"
                         &ObjectHandle="THIS-PROCEDURE:HANDLE"
                         &FrameHandle="FRAME fPage0:HANDLE"
                         &Table="{&ttTable7}"
                         &RowidTable="epc-rowid7"}
        
        /*--- Executa programas de customizaá∆o (before/after) ---*/
        {method/custom.i &Event="AFTER-CHANGE-PAGE"
                         &Object="CONTAINER"
                         &ObjectHandle="THIS-PROCEDURE:HANDLE"
                         &FrameHandle="FRAME fPage0:HANDLE"
                         &Table="{&ttTable7}"
                         &RowidTable="epc-rowid7"}
    end.    
    &ENDIF
    &IF "{&page8Browse}":U <> "" &THEN
    when 7 then do:
        IF AVAILABLE {&ttTable8} THEN 
            ASSIGN epc-rowid8 = {&ttTable8}.r-Rowid.
        ELSE 
            ASSIGN epc-rowid8 = ?.
    
        /*--- Executa programas de customizaá∆o (before/after) ---*/
        {method/custom.i &Event="BEFORE-CHANGE-PAGE"
                         &Object="CONTAINER"
                         &ObjectHandle="THIS-PROCEDURE:HANDLE"
                         &FrameHandle="FRAME fPage0:HANDLE"
                         &Table="{&ttTable8}"
                         &RowidTable="epc-rowid8"}
        
        /*--- Executa programas de customizaá∆o (before/after) ---*/
        {method/custom.i &Event="AFTER-CHANGE-PAGE"
                         &Object="CONTAINER"
                         &ObjectHandle="THIS-PROCEDURE:HANDLE"
                         &FrameHandle="FRAME fPage0:HANDLE"
                         &Table="{&ttTable8}"
                         &RowidTable="epc-rowid8"}
    end.    
    &ENDIF
    end case.

    /*--- Executa mÇtodos sobrepostos (before/after) ---*/
    {method/override.i &Position="After"
                       &Procedure="changePage"}
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/*Alteracao 27/07/2005 - tech1007 - Alterado para fazer traducao de itens de tela*/
&IF DEFINED(EXCLUDE-translate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE translate Method-Library 
PROCEDURE translate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*Os bot‰es desta tela n∆o tem prÇ-processadores, ent∆o eles podem ser exclu°dos
     por isso, para traduzir o label deles estamos buscando os que estiverem na tela
     e traduzindo o label*/
  DEFINE VARIABLE h_Frame AS HANDLE     NO-UNDO.

  ASSIGN h_Frame = FRAME fPage0:FIRST-CHILD. /* pegando o Field-Group */
  ASSIGN h_Frame = h_Frame:FIRST-CHILD.       /* pegando o 1o. Campo */
  DO WHILE h_Frame <> ? :
       if h_Frame:type <> "field-group" then do:  
          IF h_Frame:TYPE = "button" THEN
          DO :
             RUN utp/ut-liter.p (INPUT h_Frame:LABEL, "*", "C").
             ASSIGN h_Frame:LABEL = RETURN-VALUE
                    h_Frame:TOOLTIP = RETURN-VALUE.
          END.
          ASSIGN h_Frame = h_Frame:NEXT-SIBLING.
       end. 
       else do:
         assign h_frame = h_frame:first-child.
       end.    
  END.

  RUN utp/ut-liter.p (INPUT "Implantar", "*", "C").
  &IF "{&page1Browse}":U <> "":U &THEN
     ASSIGN btImplant1:LABEL IN FRAME fPage1   = RETURN-VALUE
            btImplant1:TOOLTIP IN FRAME fPage1 = RETURN-VALUE.
  &ENDIF
  &IF "{&page2Browse}":U <> "":U &THEN
     ASSIGN btImplant2:LABEL IN FRAME fPage2   = RETURN-VALUE
            btImplant2:TOOLTIP IN FRAME fPage2 = RETURN-VALUE.
  &ENDIF
  &IF "{&page3Browse}":U <> "":U &THEN
     ASSIGN btImplant3:LABEL IN FRAME fPage3   = RETURN-VALUE
            btImplant3:TOOLTIP IN FRAME fPage3 = RETURN-VALUE.
  &ENDIF
  &IF "{&page4Browse}":U <> "":U &THEN
     ASSIGN btImplant4:LABEL IN FRAME fPage4   = RETURN-VALUE
            btImplant4:TOOLTIP IN FRAME fPage4 = RETURN-VALUE.
  &ENDIF
  &IF "{&page5Browse}":U <> "":U &THEN
     ASSIGN btImplant5:LABEL IN FRAME fPage5   = RETURN-VALUE
            btImplant5:TOOLTIP IN FRAME fPage5 = RETURN-VALUE.
  &ENDIF
  &IF "{&page6Browse}":U <> "":U &THEN
     ASSIGN btImplant6:LABEL IN FRAME fPage6   = RETURN-VALUE
            btImplant6:TOOLTIP IN FRAME fPage6 = RETURN-VALUE.
  &ENDIF
  &IF "{&page7Browse}":U <> "":U &THEN
     ASSIGN btImplant7:LABEL IN FRAME fPage7   = RETURN-VALUE
            btImplant7:TOOLTIP IN FRAME fPage7 = RETURN-VALUE.
  &ENDIF
  &IF "{&page8Browse}":U <> "":U &THEN
     ASSIGN btImplant8:LABEL IN FRAME fPage8   = RETURN-VALUE
            btImplant8:TOOLTIP IN FRAME fPage8 = RETURN-VALUE.
  &ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_aplica_facelift Method-Library 
PROCEDURE pi_aplica_facelift :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
&IF "{&mguni_version}" >= "2.06b" OR "{&aplica_facelift}" = "YES" &THEN
/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
    run pi_aplica_facelift_thin in h-facelift ( input frame fPage0:handle ).
    &IF "{&page1browse}":U <> "":U &THEN
    run pi_aplica_facelift_thin in h-facelift ( input frame fPage1:handle ).
    &ENDIF
    &IF "{&page2browse}":U <> "":U &THEN
    run pi_aplica_facelift_thin in h-facelift ( input frame fPage2:handle ).
    &ENDIF
    &IF "{&page3browse}":U <> "":U &THEN
    run pi_aplica_facelift_thin in h-facelift ( input frame fPage3:handle ).
    &ENDIF
    &IF "{&page4browse}":U <> "":U &THEN
    run pi_aplica_facelift_thin in h-facelift ( input frame fPage4:handle ).
    &ENDIF
    &IF "{&page5browse}":U <> "":U &THEN
    run pi_aplica_facelift_thin in h-facelift ( input frame fPage5:handle ).
    &ENDIF
    &IF "{&page6browse}":U <> "":U &THEN
    run pi_aplica_facelift_thin in h-facelift ( input frame fPage6:handle ).
    &ENDIF
    &IF "{&page7browse}":U <> "":U &THEN
    run pi_aplica_facelift_thin in h-facelift ( input frame fPage7:handle ).
    &ENDIF
    &IF "{&page8browse}":U <> "":U &THEN
    run pi_aplica_facelift_thin in h-facelift ( input frame fPage8:handle ).
    &ENDIF
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
/*Fim Alteracao 27/07/2005*/
