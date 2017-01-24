/*****************************************************************************
 ** PROGRAMA..: DWRE005a.P
 ** OBJETIVO..: Programa Para Gera‡Æo de Etiqueta EAN
 ** AUTOR.....: SZ Solu‡äes
 ** CLIENTE...: DW
 ** VERSAO....: 2.12.00.000 - Outubro/2015 
 ** ALTERA€åES:
 ******************************************************************************/
 
/*************************** PAR¶METROS *************************************/
{cdp/cd0666.i}

DEF INPUT PARAM c-item-ini     AS CHAR.
DEF INPUT PARAM c-item-fim     AS CHAR.
DEF INPUT PARAM i-qt-etiqueta  AS INT.
DEF INPUT PARAM c-usuar-logado AS CHAR.
DEF OUTPUT PARAM TABLE FOR tt-erro.

/*************************** VARIµVEIS *************************************/
DEFINE VARIABLE c-dispositivo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-arquivo     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE h-acomp       AS HANDLE      NO-UNDO.
DEFINE VARIABLE c-observacao  AS CHARACTER                NO-UNDO.
DEFINE VARIABLE i-cont        AS INTEGER                  NO-UNDO.
DEFINE VARIABLE c-desc-1      AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE c-desc-2      AS CHARACTER FORMAT "x(30)" NO-UNDO.

DEF TEMP-TABLE tt-editor NO-UNDO
    FIELD linha          AS INT
    FIELD conteudo       AS CHAR FORMAT "x(80)"
    INDEX editor-id      IS PRIMARY UNIQUE linha.

DEFINE STREAM s-etiq.

FIND imprsor_usuar NO-LOCK
   WHERE imprsor_usuar.cod_usuario = c-usuar-logado
     AND imprsor_usuar.log_imprsor_princ NO-ERROR.

IF AVAIL imprsor_usuar THEN
    ASSIGN c-dispositivo = imprsor_usuar.nom_impressora.
ELSE DO:
    create tt-erro.
    assign tt-erro.cd-erro = 17006
           tt-erro.mensagem = "Usu rio sem impressora definida.".
    RETURN.
END.

ASSIGN c-arquivo = SESSION:TEMP-DIRECTORY + 
                   STRING(RANDOM(1,99999)) + 
                   STRING(TIME)   + 
                   STRING(YEAR( TODAY),"9999")  +
                   STRING(MONTH(TODAY),"99"  )  + 
                   STRING(DAY(  TODAY),"99"  )  + ".tmp".

OS-DELETE VALUE(c-arquivo) NO-ERROR.

DEFINE VARIABLE i AS INTEGER     NO-UNDO.

OUTPUT STREAM s-etiq TO VALUE(c-arquivo)APPEND.

FOR EACH wm-item
    WHERE wm-item.cod-item >= c-item-ini
      AND wm-item.cod-item <= c-item-fim NO-LOCK:

    IF wm-item.cod-barras = "" THEN
        NEXT.

    ASSIGN c-observacao = SUBSTRING(wm-item.des-item,1,60).
         
    IF c-observacao <> "" THEN DO:
    
        RUN pi-print-editor (INPUT TRIM(c-observacao),30).
        
        FOR EACH tt-editor 
            WHERE tt-editor.conteudo <> '':
        
            ASSIGN i-cont = i-cont + 1.
        
            IF i-cont = 1 THEN
                ASSIGN c-desc-1 = STRING(tt-editor.conteudo).
                
            ELSE
                ASSIGN c-desc-2 = STRING(tt-editor.conteudo).
            
        END.
   
    END.   

    DO i = 1 TO i-qt-etiqueta:
        /*RUN pi-acompanhar IN h-acomp (INPUT "Item: " + STRING(wm-item.cod-item)).*/
        /* Zebra S600/S400 -  ZPL */
        /* Datamax*/

        PUT stream s-etiq UNFORMATTED 
            "^XA~TA000~JSN^LT0^MMT^MNW^MTT^PON^PMN^LH0,0^JMA^PR4,4^MD0^JUS^LRN^CI0^XZ" SKIP
            "^XA^LL0472" SKIP
            "^PW709" SKIP
            "^FT24,103^A0N,29,28^FH\^FD" STRING(c-desc-2) "^FS" SKIP
            "^FT24,62^A0N,29,28^FH\^FD" STRING(c-desc-1) "^FS" SKIP
            "^FT26,145^A0N,29,28^FH\^FD" STRING(wm-item.cod-item) "^FS" SKIP
            "^FT359,149^A0N,29,28^FH\^FD" STRING(wm-item.cod-unid-med) "^FS" SKIP
            "^BY1,3,138^FT76,309^B3N,N,,Y,N" SKIP
            "^FD>;" STRING(wm-item.cod-barras) "^FS" SKIP
            "^PQ1,0,1,Y^XZ" SKIP.
        
    END.
END.

PROCEDURE pi-print-editor:

    DEF INPUT PARAM c-editor AS CHAR NO-UNDO.
    DEF INPUT PARAM i-len    AS INT  NO-UNDO.

    DEF VAR i-linha  AS INT  NO-UNDO.
    DEF VAR i-aux    AS INT  NO-UNDO.
    DEF VAR c-aux    AS CHAR NO-UNDO.
    DEF VAR c-ret    AS CHAR NO-UNDO.

    FOR EACH tt-editor:
        DELETE tt-editor.
    END.

    ASSIGN c-ret = CHR(255) + CHR(255).

    DO WHILE c-editor <> "":
        IF c-editor <> "" THEN 
        DO:
            ASSIGN i-aux = INDEX(c-editor, CHR(10)).

            IF i-aux > i-len OR
               (i-aux = 0 AND
                LENGTH(c-editor) > i-len) THEN
                ASSIGN i-aux = R-INDEX(c-editor, " ", i-len + 1).

            IF i-aux = 0 THEN
                ASSIGN c-aux    = SUBSTR(c-editor, 1, i-len)
                       c-editor = SUBSTR(c-editor, i-len + 1).
            ELSE
                ASSIGN c-aux    = SUBSTR(c-editor, 1, i-aux - 1)
                       c-editor = SUBSTR(c-editor, i-aux + 1).

            IF i-len = 0 THEN
                ASSIGN ENTRY(1, c-ret, CHR(255)) = c-aux.
            ELSE 
            DO:
                ASSIGN i-linha = i-linha + 1.

                CREATE tt-editor.

                ASSIGN tt-editor.linha    = i-linha
                       tt-editor.conteudo = c-aux.
            END.
        END.

        IF i-len = 0 THEN RETURN c-ret.
    END.

    RETURN c-ret.

END PROCEDURE. /* pi-print-editor */

OUTPUT STREAM s-etiq CLOSE.
                                  
OS-COMMAND SILENT VALUE("lp -d " + c-dispositivo + " " + c-arquivo).
OS-DELETE VALUE(c-arquivo) NO-ERROR.


