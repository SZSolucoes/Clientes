&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*:T*******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESNF007 2.00.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE RowErrors NO-UNDO
    FIELD ErrorSequence    AS INTEGER
    FIELD ErrorNumber      AS INTEGER
    FIELD ErrorDescription AS CHARACTER
    FIELD ErrorParameters  AS CHARACTER
    FIELD ErrorType        AS CHARACTER
    FIELD ErrorHelp        AS CHARACTER
    FIELD ErrorSubType     AS CHARACTER. 

DEFINE VARIABLE hshowmsg AS HANDLE      NO-UNDO.
DEFINE VARIABLE nr-seq AS INTEGER     NO-UNDO.

DEFINE VARIABLE c-chave AS CHARACTER   NO-UNDO.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE NEW GLOBAL SHARED VARIABLE gr-docum-est-esp AS ROWID NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE h-objeto         AS HANDLE NO-UNDO.
DEFINE VARIABLE h-boin090 AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-buttom c-sefaz bt-ok bt-cancela bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS c-sefaz 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "&Ajuda" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE c-sefaz AS CHARACTER FORMAT "X(256)":U 
     LABEL "C¢digo SEFAZ" 
     VIEW-AS FILL-IN 
     SIZE 48.57 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 68 BY 1.42
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     c-sefaz AT ROW 1.75 COL 15.43 COLON-ALIGNED WIDGET-ID 2
     bt-ok AT ROW 3.92 COL 3 WIDGET-ID 4
     bt-cancela AT ROW 3.92 COL 13.86
     bt-ajuda AT ROW 3.92 COL 59
     rt-buttom AT ROW 3.67 COL 2
     SPACE(0.85) SKIP(0.15)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "V†lida C¢digo SEFAZ" WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/d-dialog.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME L-To-R                                                    */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* V†lida C¢digo SEFAZ */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
   

  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda D-Dialog
ON CHOOSE OF bt-ajuda IN FRAME D-Dialog /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok D-Dialog
ON CHOOSE OF bt-ok IN FRAME D-Dialog /* OK */
DO:
    DEF VAR c-site        AS CHAR                NO-UNDO.
    DEF VAR c-prog        AS CHAR                NO-UNDO.
    DEF VAR h-prog        AS HANDLE              NO-UNDO.
    DEFINE VARIABLE l-achou AS LOGICAL     NO-UNDO.

    EMPTY TEMP-TABLE RowErrors.

    ASSIGN c-chave = INPUT FRAME {&FRAME-NAME} c-sefaz.

    OUTPUT TO "CLIPBOARD".
    PUT c-chave.
    OUTPUT CLOSE.

    /*FIND FIRST docum-est 
        WHERE SUBSTR(docum-est.char-1,93,60) = INPUT FRAME {&FRAME-NAME} c-sefaz NO-LOCK NO-ERROR.*/

    ASSIGN l-achou = NO .
    FIND FIRST nfe003 
         WHERE nfe003.ch-acesso-comp-nfe = INPUT FRAME {&FRAME-NAME} c-sefaz 
           AND nfe003.idi-orig-trad = 2 NO-LOCK NO-ERROR.

    IF AVAIL nfe003 THEN 
        FIND FIRST docum-est 
            WHERE docum-est.serie-docto  = nfe003.serie-docto
              AND docum-est.nro-docto    = nfe003.nro-docto 
              AND docum-est.cod-emitente = nfe003.cod-emitente NO-LOCK NO-ERROR .    

    IF AVAIL docum-est THEN DO:
        
        ASSIGN gr-docum-est-esp = ROWID(docum-est)
               l-achou = YES .

                                   c-prog = "C:\Program Files\Internet Explorer\iexplore.exe".
        IF SEARCH(c-prog) = ? THEN c-prog = "C:\Arquivos de programas\Internet Explorer\iexplore.exe".
        
        c-site = "https://www.nfp.fazenda.sp.gov.br/areapublica/consultanfe.aspx".
        
        RUN utp/ut-utils.p PERSISTENT SET h-prog.
        
        RUN EXECUTE IN h-prog(INPUT c-prog,INPUT c-site).
        
        DELETE PROCEDURE h-prog.


        /*IF NOT VALID-HANDLE(h-boin090) THEN
            RUN inbo/boin090.p PERSISTENT SET h-boin090.*/

        /* Reposiciona registro com base em um rowid */
        RUN repositionRecord IN h-objeto (INPUT gr-docum-est-esp).
    END.

    IF l-achou = NO THEN DO:

        FIND FIRST nfe003 
            WHERE nfe003.ch-acesso-comp-nfe = INPUT FRAME {&FRAME-NAME} c-sefaz 
              AND nfe003.idi-orig-trad = 2 NO-LOCK NO-ERROR.
        
        IF AVAIL nfe003 THEN DO:
            
             IF nfe003.idi-situacao   = 2 THEN
                 RUN utp/ut-msgs.p (INPUT "Show",
                                    INPUT 17006,
                                    INPUT "Nota Fiscal no Monitor!" + "~~" + "Nota Fiscal est† no monitor por apresentar erro de neg¢cio!").
             IF nfe003.idi-situacao   = 5 THEN
                 RUN utp/ut-msgs.p (INPUT "Show",
                                    INPUT 17006,
                                    INPUT "Nota Fiscal no Monitor!" + "~~" + "Nota Fiscal est† no monitor por DANFE n∆o estar autorizado!").
             IF nfe003.idi-situacao   = 6 THEN
                 RUN utp/ut-msgs.p (INPUT "Show",
                                    INPUT 17006,
                                    INPUT "Nota Fiscal no Monitor!" + "~~" + "Nota Fiscal est† no monitor liberada para ser integrada!").
        END.     
        ELSE      
            RUN utp/ut-msgs.p (INPUT "Show",
                               INPUT 17006,
                               INPUT "C¢digo SEFAZ n∆o Localizado!" + "~~" + "C¢digo SEFAZ n∆o vinculado a nenhuma nota fiscal!").
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-sefaz
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-sefaz D-Dialog
ON RETURN OF c-sefaz IN FRAME D-Dialog /* C¢digo SEFAZ */
DO:
  
    FIND FIRST docum-est 
        WHERE SUBSTR(docum-est.char-1,93,60) = INPUT FRAME {&FRAME-NAME} c-sefaz NO-LOCK NO-ERROR.

    IF AVAIL docum-est THEN DO:
        
        ASSIGN gr-docum-est-esp = ROWID(docum-est).

        /*IF NOT VALID-HANDLE(h-boin090) THEN
            RUN inbo/boin090.p PERSISTENT SET h-boin090.*/

        /* Reposiciona registro com base em um rowid */
        RUN repositionRecord IN h-objeto (INPUT gr-docum-est-esp).
        
    END.
    ELSE DO:
        FIND FIRST nfe003 
            WHERE nfe003.ch-acesso-comp-nfe = INPUT FRAME {&FRAME-NAME} c-sefaz 
              AND nfe003.idi-orig-trad = 2 NO-LOCK NO-ERROR.
        IF AVAIL nfe003 THEN DO:

            

             IF nfe003.idi-situacao   = 2 THEN
                 RUN utp/ut-msgs.p (INPUT "Show",
                                    INPUT 17006,
                                    INPUT "Nota Fiscal no Monitor!" + "~~" + "Nota Fiscal est† no monitor por apresentar erro de neg¢cio!").
             IF nfe003.idi-situacao   = 5 THEN
                 RUN utp/ut-msgs.p (INPUT "Show",
                                    INPUT 17006,
                                    INPUT "Nota Fiscal no Monitor!" + "~~" + "Nota Fiscal est† no monitor por DANFE n∆o estar autorizado!").
             IF nfe003.idi-situacao   = 6 THEN
                 RUN utp/ut-msgs.p (INPUT "Show",
                                    INPUT 17006,
                                    INPUT "Nota Fiscal no Monitor!" + "~~" + "Nota Fiscal est† no monitor liberada para ser integrada!").
             
        END.     
        ELSE      
            RUN utp/ut-msgs.p (INPUT "Show",
                               INPUT 17006,
                               INPUT "C¢digo SEFAZ n∆o Localizado!" + "~~" + "C¢digo SEFAZ n∆o vinculado a nenhuma nota fiscal!").
    END.
    
    APPLY 'CHOOSE' TO bt-ok.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY c-sefaz 
      WITH FRAME D-Dialog.
  ENABLE rt-buttom c-sefaz bt-ok bt-cancela bt-ajuda 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy D-Dialog 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  
  FOR EACH nfe017 
     WHERE nfe017.ch-acesso-comp-nfe = c-chave
       AND nfe017.idi-orig-trad  = 2 
       AND nfe017.log-ativo      = YES :

      CREATE RowErrors.
      ASSIGN RowErrors.ErrorNumber       = nfe017.cd-msg    
             RowErrors.ErrorDescription  = nfe017.texto-msg .
  END.
   
  FIND FIRST RowErrors NO-ERROR.
  IF AVAIL rowerrors THEN DO:
      {METHOD/showmessage.i1}
      {METHOD/showmessage.i2 &Modal="yes"}
  END.  

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  
  {utp/ut9000.i "ESNF007" "2.00.00.001"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
  
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

