/*******************************************************************************
**
**  Zoom/anykey.i: Include para evento anykey nos fill-ins inicial do 
**                      browser zoom 
**
*******************************************************************************/

DEFINE VARIABLE deTime       AS DECIMAL NO-UNDO.
DEFINE VARIABLE deTimeAux    AS DECIMAL NO-UNDO.
DEFINE VARIABLE mpSystemTime AS MEMPTR  NO-UNDO.
DEFINE VARIABLE lRepeat      AS LOGICAL NO-UNDO.

SET-SIZE(mpSystemTime) = 20.


ASSIGN {&VARIAVEL}:PRIVATE-DATA = {&VARIAVEL}:SCREEN-VALUE.

/*Tratamento do comando CRTL+C nos eventos de AnyKey.*/
IF LASTKEY <> 3 THEN /*3 = CRTL+C*/
    APPLY LASTKEY.
ELSE DO:
    OUTPUT TO "CLIPBOARD":U. /*Area de trabalho do Windows*/
        PUT UNFORMATTED {&VARIAVEL}:SCREEN-VALUE. /*Move o que o usu rio selecionou para a area de trabalho do Windows*/
    OUTPUT CLOSE.
end.
/*Fim do tratamento de CRTL+C nos eventos de AnyKey.*/

ASSIGN INPUT {&VARIAVEL}.

IF {&VARIAVEL}:PRIVATE-DATA <> STRING({&VARIAVEL}) THEN DO:
    RUN GetSystemTime (OUTPUT mpSystemTime).
    ASSIGN deTime = (GET-SHORT(mpSystemTime, 9) * 3600000) + /* Horas */
                    (GET-SHORT(mpSystemTime, 11) * 60000) +  /* Minutos */
                    (GET-SHORT(mpSystemTime, 13) * 1000) +   /* Segundo */
                    (GET-SHORT(mpSystemTime, 15)).           /* Milisegundos */
    
    IF NOT lRepeat THEN DO:
        ASSIGN lRepeat = YES.
        
        DO WHILE lRepeat:
            PROCESS EVENTS.
            
            IF {&VARIAVEL}:PRIVATE-DATA <> {&VARIAVEL}:SCREEN-VALUE THEN DO:
                ASSIGN {&VARIAVEL}:PRIVATE-DATA = {&VARIAVEL}:SCREEN-VALUE.
                
                ASSIGN INPUT {&VARIAVEL}.
                
                RUN GetSystemTime (OUTPUT mpSystemTime).
                ASSIGN deTime = (GET-SHORT(mpSystemTime, 9) * 3600000) + /* Horas */
                                (GET-SHORT(mpSystemTime, 11) * 60000) +  /* Minutos */
                                (GET-SHORT(mpSystemTime, 13) * 1000) +   /* Segundo */
                                (GET-SHORT(mpSystemTime, 15)).           /* Milisegundos */
            END.
            
            RUN GetSystemTime (OUTPUT mpSystemTime).
            ASSIGN deTimeAux = (GET-SHORT(mpSystemTime, 9) * 3600000) + /* Horas */
                               (GET-SHORT(mpSystemTime, 11) * 60000) +  /* Minutos */
                               (GET-SHORT(mpSystemTime, 13) * 1000) +   /* Segundo */
                               (GET-SHORT(mpSystemTime, 15)).           /* Milisegundos */
            
            IF deTimeAux - deTime > 700 THEN /* > 1 OR >= 1 */
                LEAVE.
        END.
        
/*        RUN dispatch IN THIS-PROCEDURE (INPUT "open-query":U).
 *         APPLY "VALUE-CHANGED":U TO BROWSE {&BROWSE-NAME}. */
    &if defined(pageNumber) &then
        APPLY "choose":U to btCheck{&PageNumber} in frame fPage{&PageNumber}.
    &else
        APPLY "choose":U to btCheck1 in frame fPage1.
    &endif

        APPLY "END":U TO {&VARIAVEL}.
        
        ASSIGN lRepeat = NO.
    END.
END.

SET-SIZE(mpSystemTime) = 0.

RETURN NO-APPLY.
/*--- include/i-anykey.i ---*/
