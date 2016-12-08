&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
/* Procedure Description
"Method Library que contÇm a l¢gica da Main Block."
*/
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Method-Library 
/*--------------------------------------------------------------------------
    Library    : zoom/MainBlock.i
    Purpose    : Method Library que contÇm a l¢gica da Main Block 

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
         HEIGHT             = 4.5
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Method-Library 


/* ***************************  Main Block  *************************** */

/*--- Seta cursor do mouse para espera ---*/
SESSION:SET-WAIT-STATE("GENERAL":U).

/*--- Evento de CLOSE padr∆o para THIS-PROCEDURE ---*/
ON CLOSE OF THIS-PROCEDURE DO:
   RUN destroyInterface IN THIS-PROCEDURE.
END.

/*--- Evento de CTRL-TAB padr∆o para THIS-PROCEDURE ---*/
&IF "{&Folder}":U = "YES":U &THEN
    ON CTRL-TAB ANYWHERE
        RUN nextFolder IN hFolder.
&ENDIF

/*--- Evento de SHIFT-CTRL-TAB padr∆o para THIS-PROCEDURE ---*/
&IF "{&Folder}":U = "YES":U &THEN
    ON SHIFT-CTRL-TAB ANYWHERE
        RUN prevFolder IN hFolder.
&ENDIF


/*--- Evento de CURSOR-DOWN, PAGE-DOWN, OFF-END e END padr∆o para o browse da p†gina 1 ---*/
&IF "{&page1browse}":U <> "":U &THEN
    ON RETURN  OF BROWSE {&page1browse}  DO:
         RUN returnValues IN THIS-PROCEDURE.
         APPLY "CLOSE":U TO THIS-PROCEDURE.
    END.

    &IF "{&CURSOR-DOWN1}":U <> "YES":U &THEN
    ON CURSOR-DOWN OF {&page1browse} IN FRAME fPage1 DO:
        {zoom/cursordown.i &PageNumber="1"}
    END.
    &ENDIF
    
    &IF "{&END1}":U <> "YES":U &THEN
    ON END OF {&page1browse} IN FRAME fPage1 DO:
        {zoom/end.i &PageNumber="1"}
    END.
    &ENDIF
    
    &IF "{&OFF-END1}":U <> "YES":U &THEN
    ON OFF-END OF {&page1browse} IN FRAME fPage1 DO:
        {zoom/offend.i &PageNumber="1"}
    END.
    &ENDIF
    
    &IF "{&PAGE-DOWN1}":U <> "YES":U &THEN
    ON PAGE-DOWN OF {&page1browse} IN FRAME fPage1 DO:
        {zoom/pagedown.i &PageNumber="1"}
    END.
    &ENDIF
    
    &IF "{&VALUE-CHANGED1}":U <> "YES":U &THEN
    ON VALUE-CHANGED OF {&page1browse} IN FRAME fPage1 DO:
        {zoom/valuechanged.i &PageNumber="1"}
    END.
    &ENDIF
    
    &IF "{&MOUSE-SELECT-DBLCLICK1}":U <> "YES":U &THEN
    ON MOUSE-SELECT-DBLCLICK OF {&page1browse} IN FRAME fPage1 DO:
        {zoom/dblclick.i &PageNumber="1"}
    END.
    &ENDIF
    
    &IF "{&CURSOR-UP1}":U <> "YES":U &THEN
    ON CURSOR-UP OF {&page1browse} IN FRAME fPage1 DO:
        {zoom/cursorup.i &PageNumber="1"}
    END.
    &ENDIF
    
    &IF "{&HOME1}":U <> "YES":U &THEN
    ON HOME OF {&page1browse} IN FRAME fPage1 DO:
        {zoom/home.i &PageNumber="1"}
    END.
    &ENDIF
    
    &IF "{&OFF-HOME1}":U <> "YES":U &THEN
    ON OFF-HOME OF {&page1browse} IN FRAME fPage1 DO:
        {zoom/offhome.i &PageNumber="1"}
    END.
    &ENDIF
    
    &IF "{&PAGE-UP1}":U <> "YES":U &THEN
    ON PAGE-UP OF {&page1browse} IN FRAME fPage1 DO:
        {zoom/pageup.i &PageNumber="1"}
    END.
    &ENDIF
&ENDIF

/*--- Evento de CURSOR-DOWN, PAGE-DOWN, OFF-END e END padr∆o para o browse da p†gina 2 ---*/
&IF "{&page2browse}":U <> "":U &THEN
    ON RETURN  OF BROWSE {&page2browse}  DO:
         RUN returnValues IN THIS-PROCEDURE.
         APPLY "CLOSE":U TO THIS-PROCEDURE.
    END.
    &IF "{&CURSOR-DOWN2}" <> "YES":U &THEN
    ON CURSOR-DOWN OF {&page2browse} IN FRAME fPage2 DO:
        {zoom/cursordown.i &PageNumber="2"}
    END.
    &ENDIF
    
    &IF "{&END2}":U <> "YES":U &THEN
    ON END OF {&page2browse} IN FRAME fPage2 DO:
        {zoom/end.i &PageNumber="2"}
    END.
    &ENDIF
    
    &IF "{&OFF-END2}":U <> "YES":U &THEN
    ON OFF-END OF {&page2browse} IN FRAME fPage2 DO:
        {zoom/offend.i &PageNumber="2"}
    END.
    &ENDIF
    
    &IF "{&PAGE-DOWN2}":U <> "YES":U &THEN
    ON PAGE-DOWN OF {&page2browse} IN FRAME fPage2 DO:
        {zoom/pagedown.i &PageNumber="2"}
    END.
    &ENDIF
    
    &IF "{&VALUE-CHANGED2}":U <> "YES":U &THEN
    ON VALUE-CHANGED OF {&page2browse} IN FRAME fPage2 DO:
        {zoom/valuechanged.i &PageNumber="2"}
    END.
    &ENDIF
    
    &IF "{&MOUSE-SELECT-DBLCLICK2}":U <> "YES":U &THEN
    ON MOUSE-SELECT-DBLCLICK OF {&page2browse} IN FRAME fPage2 DO:
        {zoom/dblclick.i &PageNumber="2"}
    END.
    &ENDIF
    
    &IF "{&CURSOR-UP2}":U <> "YES":U &THEN
    ON CURSOR-UP OF {&page2browse} IN FRAME fPage2 DO:
        {zoom/cursorup.i &PageNumber="2"}
    END.
    &ENDIF
    
    &IF "{&HOME2}":U <> "YES":U &THEN
    ON HOME OF {&page2browse} IN FRAME fPage2 DO:
        {zoom/home.i &PageNumber="2"}
    END.
    &ENDIF
    
    &IF "{&OFF-HOME2}":U <> "YES":U &THEN
    ON OFF-HOME OF {&page2browse} IN FRAME fPage2 DO:
        {zoom/offhome.i &PageNumber="2"}
    END.
    &ENDIF
    
    &IF "{&PAGE-UP2}":U <> "YES":U &THEN
    ON PAGE-UP OF {&page2browse} IN FRAME fPage2 DO:
        {zoom/pageup.i &PageNumber="2"}
    END.
    &ENDIF
&ENDIF

/*--- Evento de CURSOR-DOWN, PAGE-DOWN, OFF-END e END padr∆o para o browse da p†gina 3 ---*/
&IF "{&page3browse}":U <> "":U &THEN
    ON RETURN  OF BROWSE {&page3browse}  DO:
         RUN returnValues IN THIS-PROCEDURE.
         APPLY "CLOSE":U TO THIS-PROCEDURE.
    END. 
    &IF "{&CURSOR-DOWN3}":U <> "YES":U &THEN
    ON CURSOR-DOWN OF {&page3browse} IN FRAME fPage3 DO:
        {zoom/cursordown.i &PageNumber="3"}
    END.
    &ENDIF
    
    &IF "{&END3}":U <> "YES":U &THEN
    ON END OF {&page3browse} IN FRAME fPage3 DO:
        {zoom/end.i &PageNumber="3"}
    END.
    &ENDIF
    
    &IF "{&OFF-END3}":U <> "YES":U &THEN
    ON OFF-END OF {&page3browse} IN FRAME fPage3 DO:
        {zoom/offend.i &PageNumber="3"}
    END.
    &ENDIF
    
    &IF "{&PAGE-DOWN3}":U <> "YES":U &THEN
    ON PAGE-DOWN OF {&page3browse} IN FRAME fPage3 DO:
        {zoom/pagedown.i &PageNumber="3"}
    END.
    &ENDIF
    
    &IF "{&VALUE-CHANGED3}":U <> "YES":U &THEN
    ON VALUE-CHANGED OF {&page3browse} IN FRAME fPage3 DO:
        {zoom/valuechanged.i &PageNumber="3"}
    END.
    &ENDIF
    
    &IF "{&MOUSE-SELECT-DBLCLICK3}":U <> "YES":U &THEN
    ON MOUSE-SELECT-DBLCLICK OF {&page3browse} IN FRAME fPage3 DO:
        {zoom/dblclick.i &PageNumber="3"}
    END.
    &ENDIF
    
    &IF "{&CURSOR-UP3}":U <> "YES":U &THEN
    ON CURSOR-UP OF {&page3browse} IN FRAME fPage3 DO:
        {zoom/cursorup.i &PageNumber="3"}
    END.
    &ENDIF
    
    &IF "{&HOME3}":U <> "YES":U &THEN
    ON HOME OF {&page3browse} IN FRAME fPage3 DO:
        {zoom/home.i &PageNumber="3"}
    END.
    &ENDIF
    
    &IF "{&OFF-HOME3}":U <> "YES":U &THEN
    ON OFF-HOME OF {&page3browse} IN FRAME fPage3 DO:
        {zoom/offhome.i &PageNumber="3"}
    END.
    &ENDIF
    
    &IF "{&PAGE-UP3}":U <> "YES":U &THEN
    ON PAGE-UP OF {&page3browse} IN FRAME fPage3 DO:
        {zoom/pageup.i &PageNumber="3"}
    END.
    &ENDIF
&ENDIF

/*--- Evento de CURSOR-DOWN, PAGE-DOWN, OFF-END e END padr∆o para o browse da p†gina 4 ---*/
&IF "{&page4browse}":U <> "":U &THEN
    ON RETURN  OF BROWSE {&page4browse}  DO:
         RUN returnValues IN THIS-PROCEDURE.
         APPLY "CLOSE":U TO THIS-PROCEDURE.
    END.

    &IF "{&CURSOR-DOWN4}":U <> "YES":U &THEN
    ON CURSOR-DOWN OF {&page4browse} IN FRAME fPage4 DO:
        {zoom/cursordown.i &PageNumber="4"}
    END.
    &ENDIF
    
    &IF "{&END4}":U <> "YES":U &THEN
    ON END OF {&page4browse} IN FRAME fPage4 DO:
        {zoom/end.i &PageNumber="4"}
    END.
    &ENDIF
    
    &IF "{&OFF-END4}":U <> "YES":U &THEN
    ON OFF-END OF {&page4browse} IN FRAME fPage4 DO:
        {zoom/offend.i &PageNumber="4"}
    END.
    &ENDIF
    
    &IF "{&PAGE-DOWN4}":U <> "YES":U &THEN
    ON PAGE-DOWN OF {&page4browse} IN FRAME fPage4 DO:
        {zoom/pagedown.i &PageNumber="4"}
    END.
    &ENDIF
    
    &IF "{&VALUE-CHANGED4}":U <> "YES":U &THEN
    ON VALUE-CHANGED OF {&page4browse} IN FRAME fPage4 DO:
        {zoom/valuechanged.i &PageNumber="4"}
    END.
    &ENDIF
    
    &IF "{&MOUSE-SELECT-DBLCLICK4}":U <> "YES":U &THEN
    ON MOUSE-SELECT-DBLCLICK OF {&page4browse} IN FRAME fPage4 DO:
        {zoom/dblclick.i &PageNumber="4"}
    END.
    &ENDIF
    
    &IF "{&CURSOR-UP4}":U <> "YES":U &THEN
    ON CURSOR-UP OF {&page4browse} IN FRAME fPage4 DO:
        {zoom/cursorup.i &PageNumber="4"}
    END.
    &ENDIF
    
    &IF "{&HOME4}":U <> "YES":U &THEN
    ON HOME OF {&page4browse} IN FRAME fPage4 DO:
        {zoom/home.i &PageNumber="4"}
    END.
    &ENDIF
    
    &IF "{&OFF-HOME4}":U <> "YES":U &THEN
    ON OFF-HOME OF {&page4browse} IN FRAME fPage4 DO:
        {zoom/offhome.i &PageNumber="4"}
    END.
    &ENDIF
    
    &IF "{&PAGE-UP4}":U <> "YES":U &THEN
    ON PAGE-UP OF {&page4browse} IN FRAME fPage4 DO:
        {zoom/pageup.i &PageNumber="4"}
    END.
    &ENDIF
&ENDIF

/*--- Evento de CURSOR-DOWN, PAGE-DOWN, OFF-END e END padr∆o para o browse da p†gina 5 ---*/
&IF "{&page5browse}":U <> "":U &THEN
    ON RETURN  OF BROWSE {&page5browse}  DO:
         RUN returnValues IN THIS-PROCEDURE.
         APPLY "CLOSE":U TO THIS-PROCEDURE.
    END. 
    &IF "{&CURSOR-DOWN5}":U <> "YES":U &THEN
    ON CURSOR-DOWN OF {&page5browse} IN FRAME fPage5 DO:
        {zoom/cursordown.i &PageNumber="5"}
    END.
    &endif

    &IF "{&END5}":U <> "YES":U &THEN
    ON END OF {&page5browse} IN FRAME fPage5 DO:
        {zoom/end.i &PageNumber="5"}
    END.
    &ENDIF
    
    &IF "{&OFF-END5}":U <> "YES":U &THEN
    ON OFF-END OF {&page5browse} IN FRAME fPage5 DO:
        {zoom/offend.i &PageNumber="5"}
    END.
    &ENDIF
    
    &IF "{&PAGE-DOWN5}":U <> "YES":U &THEN
    ON PAGE-DOWN OF {&page5browse} IN FRAME fPage5 DO:
        {zoom/pagedown.i &PageNumber="5"}
    END.
    &ENDIF
    
    &IF "{&VALUE-CHANGED5}":U <> "YES":U &THEN
    ON VALUE-CHANGED OF {&page5browse} IN FRAME fPage5 DO:
        {zoom/valuechanged.i &PageNumber="5"}
    END.
    &ENDIF
    
    &IF "{&MOUSE-SELECT-DBLCLICK5}":U <> "YES":U &THEN
    ON MOUSE-SELECT-DBLCLICK OF {&page5browse} IN FRAME fPage5 DO:
        {zoom/dblclick.i &PageNumber="5"}
    END.
    &ENDIF
    
    &IF "{&CURSOR-UP5}":U <> "YES":U &THEN
    ON CURSOR-UP OF {&page5browse} IN FRAME fPage5 DO:
        {zoom/cursorup.i &PageNumber="5"}
    END.
    &ENDIF
    
    &IF "{&HOME5}":U <> "YES":U &THEN
    ON HOME OF {&page5browse} IN FRAME fPage5 DO:
        {zoom/home.i &PageNumber="5"}
    END.
    &ENDIF
    
    &IF "{&OFF-HOME5}":U <> "YES":U &THEN
    ON OFF-HOME OF {&page5browse} IN FRAME fPage5 DO:
        {zoom/offhome.i &PageNumber="5"}
    END.
    &ENDIF
    
    &IF "{&PAGE-UP5}":U <> "YES":U &THEN
    ON PAGE-UP OF {&page5browse} IN FRAME fPage5 DO:
        {zoom/pageup.i &PageNumber="5"}
    END.
    &ENDIF
&ENDIF

/*--- Evento de CURSOR-DOWN, PAGE-DOWN, OFF-END e END padr∆o para o browse da p†gina 6 ---*/
&IF "{&page6browse}":U <> "":U &THEN
    ON RETURN  OF BROWSE {&page6browse}  DO:
         RUN returnValues IN THIS-PROCEDURE.
         APPLY "CLOSE":U TO THIS-PROCEDURE.
    END.
    &IF "{&CURSOR-DOWN6}":U <> "YES":U &THEN
    ON CURSOR-DOWN OF {&page6browse} IN FRAME fPage6 DO:
        {zoom/cursordown.i &PageNumber="6"}
    END.
    &ENDIF
    
    &IF "{&END6}":U <> "YES":U &THEN
    ON END OF {&page6browse} IN FRAME fPage6 DO:
        {zoom/end.i &PageNumber="6"}
    END.
    &ENDIF
    
    &IF "{&OFF-END6}":U <> "YES":U &THEN
    ON OFF-END OF {&page6browse} IN FRAME fPage6 DO:
        {zoom/offend.i &PageNumber="6"}
    END.
    &ENDIF
    
    &IF "{&PAGE-DOWN6}":U <> "YES":U &THEN
    ON PAGE-DOWN OF {&page6browse} IN FRAME fPage6 DO:
        {zoom/pagedown.i &PageNumber="6"}
    END.
    &ENDIF
    
    &IF "{&VALUE-CHANGED6}":U <> "YES":U &THEN
    ON VALUE-CHANGED OF {&page6browse} IN FRAME fPage6 DO:
        {zoom/valuechanged.i &PageNumber="6"}
    END.
    &ENDIF
    
    &IF "{&MOUSE-SELECT-DBLCLICK6}":U <> "YES":U &THEN
    ON MOUSE-SELECT-DBLCLICK OF {&page6browse} IN FRAME fPage6 DO:
        {zoom/dblclick.i &PageNumber="6"}
    END.
    &ENDIF
    
    &IF "{&CURSOR-UP6}":U <> "YES":U &THEN
    ON CURSOR-UP OF {&page6browse} IN FRAME fPage6 DO:
        {zoom/cursorup.i &PageNumber="6"}
    END.
    &ENDIF
    
    &IF "{&HOME6}":U <> "YES":U &THEN
    ON HOME OF {&page6browse} IN FRAME fPage6 DO:
        {zoom/home.i &PageNumber="6"}
    END.
    &ENDIF
    
    &IF "{&OFF-HOME6}":U <> "YES":U &THEN
    ON OFF-HOME OF {&page6browse} IN FRAME fPage6 DO:
        {zoom/offhome.i &PageNumber="6"}
    END.
    &ENDIF
    
    &IF "{&PAGE-UP6}":U <> "YES":U &THEN
    ON PAGE-UP OF {&page6browse} IN FRAME fPage6 DO:
        {zoom/pageup.i &PageNumber="6"}
    END.
    &ENDIF
&ENDIF

/*--- Evento de CURSOR-DOWN, PAGE-DOWN, OFF-END e END padr∆o para o browse da p†gina 7 ---*/
&IF "{&page7browse}":U <> "":U &THEN
    ON RETURN  OF BROWSE {&page7browse}  DO:
         RUN returnValues IN THIS-PROCEDURE.
         APPLY "CLOSE":U TO THIS-PROCEDURE.
    END.

    &IF "{&CURSOR-DOWN7}":U <> "YES":U &THEN
    ON CURSOR-DOWN OF {&page7browse} IN FRAME fPage7 DO:
        {zoom/cursordown.i &PageNumber="7"}
    END.
    &ENDIF
    
    &IF "{&END7}":U <> "YES":U &THEN
    ON END OF {&page7browse} IN FRAME fPage7 DO:
        {zoom/end.i &PageNumber="7"}
    END.
    &ENDIF
    
    &IF "{&OFF-END7}":U <> "YES":U &THEN
    ON OFF-END OF {&page7browse} IN FRAME fPage7 DO:
        {zoom/offend.i &PageNumber="7"}
    END.
    &ENDIF
    
    &IF "{&PAGE-DOWN7}":U <> "YES":U &THEN
    ON PAGE-DOWN OF {&page7browse} IN FRAME fPage7 DO:
        {zoom/pagedown.i &PageNumber="7"}
    END.
    &ENDIF
    
    &IF "{&VALUE-CHANGED7}":U <> "YES":U &THEN
    ON VALUE-CHANGED OF {&page7browse} IN FRAME fPage7 DO:
        {zoom/valuechanged.i &PageNumber="7"}
    END.
    &ENDIF
    
    &IF "{&MOUSE-SELECT-DBLCLICK7}":U <> "YES":U &THEN
    ON MOUSE-SELECT-DBLCLICK OF {&page7browse} IN FRAME fPage7 DO:
        {zoom/dblclick.i &PageNumber="7"}
    END.
    &ENDIF
    
    &IF "{&CURSOR-UP7}":U <> "YES":U &THEN
    ON CURSOR-UP OF {&page7browse} IN FRAME fPage7 DO:
        {zoom/cursorup.i &PageNumber="7"}
    END.
    &ENDIF
    
    &IF "{&HOME7}":U <> "YES":U &THEN
    ON HOME OF {&page7browse} IN FRAME fPage7 DO:
        {zoom/home.i &PageNumber="7"}
    END.
    &ENDIF
    
    &IF "{&OFF-HOME7}":U <> "YES":U &THEN
    ON OFF-HOME OF {&page7browse} IN FRAME fPage7 DO:
        {zoom/offhome.i &PageNumber="7"}
    END.
    &ENDIF
    
    &IF "{&PAGE-UP7}":U <> "YES":U &THEN
    ON PAGE-UP OF {&page7browse} IN FRAME fPage7 DO:
        {zoom/pageup.i &PageNumber="7"}
    END.
    &ENDIF
&ENDIF

/*--- Evento de CURSOR-DOWN, PAGE-DOWN, OFF-END e END padr∆o para o browse da p†gina 8 ---*/
&IF "{&page8browse}":U <> "":U &THEN
    ON RETURN  OF BROWSE {&page8browse}  DO:
         RUN returnValues IN THIS-PROCEDURE.
         APPLY "CLOSE":U TO THIS-PROCEDURE.
    END. 

    &IF "{&CURSOR-DOWN8}":U <> "YES":U &THEN
    ON CURSOR-DOWN OF {&page8browse} IN FRAME fPage8 DO:
        {zoom/cursordown.i &PageNumber="8"}
    END.
    &endif

    &IF "{&END8}":U <> "YES":U &THEN
    ON END OF {&page8browse} IN FRAME fPage8 DO:
        {zoom/end.i &PageNumber="8"}
    END.
    &ENDIF
    
    &IF "{&OFF-END8}":U <> "YES":U &THEN
    ON OFF-END OF {&page8browse} IN FRAME fPage8 DO:
        {zoom/offend.i &PageNumber="8"}
    END.
    &ENDIF
    
    &IF "{&PAGE-DOWN8}":U <> "YES":U &THEN
    ON PAGE-DOWN OF {&page8browse} IN FRAME fPage8 DO:
        {zoom/pagedown.i &PageNumber="8"}
    END.
    &ENDIF
    
    &IF "{&VALUE-CHANGED8}":U <> "YES":U &THEN
    ON VALUE-CHANGED OF {&page8browse} IN FRAME fPage8 DO:
        {zoom/valuechanged.i &PageNumber="8"}
    END.
    &ENDIF
    
    &IF "{&MOUSE-SELECT-DBLCLICK8}":U <> "YES":U &THEN
    ON MOUSE-SELECT-DBLCLICK OF {&page8browse} IN FRAME fPage8 DO:
        {zoom/dblclick.i &PageNumber="8"}
    END.
    &ENDIF
    
    &IF "{&CURSOR-UP8}":U <> "YES":U &THEN
    ON CURSOR-UP OF {&page8browse} IN FRAME fPage8 DO:
        {zoom/cursorup.i &PageNumber="8"}
    END.
    &ENDIF
    
    &IF "{&HOME8}":U <> "YES":U &THEN
    ON HOME OF {&page8browse} IN FRAME fPage8 DO:
        {zoom/home.i &PageNumber="8"}
    END.
    &ENDIF
    
    &IF "{&OFF-HOME8}":U <> "YES":U &THEN
    ON OFF-HOME OF {&page8browse} IN FRAME fPage8 DO:
        {zoom/offhome.i &PageNumber="8"}
    END.
    &ENDIF
    
    &IF "{&PAGE-UP8}":U <> "YES":U &THEN
    ON PAGE-UP OF {&page8browse} IN FRAME fPage8 DO:
        {zoom/pageup.i &PageNumber="8"}
    END.
    &ENDIF
&ENDIF

/*--- Seta CURRENT-WINDOW como sendo a window atual ---*/
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME}
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/*--- Padr∆o para janelas GUI ---*/
PAUSE 0 BEFORE-HIDE.

&IF "{&UIB_is_Running}":U <> "":U &THEN
    /*--- Inicializa programa ---*/
    RUN initializeInterface IN THIS-PROCEDURE.
    IF RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
&ELSE
    IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
        /*--- Inicializa programa ---*/
        RUN initializeInterface IN THIS-PROCEDURE.
        IF RETURN-VALUE = "NOK":U THEN
            RETURN "NOK":U.
    END.
&ENDIF

/*Alteracao 27/07/2005 - tech1007 - procedure para traduzir tooltips de botoes e tambem de menu*/
RUN translate IN THIS-PROCEDURE.
/*Fim alteracao 27/07/2005*/

/*--- Block principal do programa ---*/
DO ON ERROR   UNDO, LEAVE
   ON END-KEY UNDO, LEAVE:
    
    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


