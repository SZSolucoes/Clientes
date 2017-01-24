/*****************************************************************************
 ** PROGRAMA..: UPC-CC0523.P
 ** OBJETIVO..: UPC Consulta Pendàncias Comprador - CC0523
 ** AUTOR.....: SZ
 ** CLIENTE...: Noble
 ** VERSAO....: 2.12.00.000 - 10/01/2017
 ** ALTERAÄÂES:
 ******************************************************************************/

 /***************** Definiá∆o de Parametros *****************************/
 DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
 DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO. 
 DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
 DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
 DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
 DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

 /* Definiá∆o Vari†vel Global */
 /* Este Ç o campo que ser† inserido no browse */
 DEFINE NEW GLOBAL SHARED VARIABLE h-upc-cc0523       AS HANDLE NO-UNDO.
 DEFINE NEW GLOBAL SHARED VARIABLE wh-cc0523-solic    AS WIDGET-HANDLE.
 DEFINE NEW GLOBAL SHARED VARIABLE wh-cc0523-ordem    AS WIDGET-HANDLE.
 DEFINE NEW GLOBAL SHARED VARIABLE wh-cc0523-ordem-a  AS WIDGET-HANDLE.
 DEFINE NEW GLOBAL SHARED VARIABLE wgh-window         AS WIDGET-HANDLE  NO-UNDO.
 
 /***************  Local Variable Definitions *************************/
 DEFINE VARIABLE h-temp                AS WIDGET-HANDLE NO-UNDO.
 DEFINE VARIABLE char-hdl              AS CHARACTER     NO-UNDO.
 DEFINE VARIABLE hfield                AS HANDLE        NO-UNDO.
 DEFINE VARIABLE cBrwss                AS CHARACTER     NO-UNDO.
 DEFINE VARIABLE cBrwssFields          AS CHARACTER     NO-UNDO.
 DEFINE VARIABLE i-cont                AS INTEGER       NO-UNDO.
 DEFINE VARIABLE ponteiro              AS WIDGET-HANDLE NO-UNDO.
 DEFINE VARIABLE c-objeto              AS CHARACTER     NO-UNDO.
 DEFINE VARIABLE i-cont-aux            AS INTEGER       NO-UNDO.
 DEFINE VARIABLE wh-aux                AS WIDGET-HANDLE NO-UNDO.
 DEFINE VARIABLE wh-aux-2              AS WIDGET-HANDLE NO-UNDO.
 DEFINE VARIABLE wh-it-prioridade      AS WIDGET-HANDLE NO-UNDO.
 DEFINE VARIABLE wh-ordem-prioridade   AS WIDGET-HANDLE NO-UNDO.
 DEFINE VARIABLE wh-it-prioridade-a    AS WIDGET-HANDLE NO-UNDO.
 DEFINE VARIABLE wh-ordem-prioridade-a AS WIDGET-HANDLE NO-UNDO.
 DEFINE VARIABLE c-char                AS CHARACTER     NO-UNDO.
 DEFINE VARIABLE wlh-brw               AS WIDGET-HANDLE NO-UNDO.
 DEFINE VARIABLE wlh-qry               AS WIDGET-HANDLE NO-UNDO.
 DEFINE VARIABLE wlh-row               AS WIDGET-HANDLE NO-UNDO.
 DEFINE VARIABLE wlh-cod-estabel       AS WIDGET-HANDLE NO-UNDO.
 DEFINE VARIABLE wlh-numero-ordem      AS WIDGET-HANDLE NO-UNDO.
 DEFINE VARIABLE wlh-numero-ordem-a    AS WIDGET-HANDLE NO-UNDO.
 DEFINE VARIABLE wlh-cod-comprado      AS WIDGET-HANDLE NO-UNDO.
 DEFINE VARIABLE wh-nr-requisicao      AS WIDGET-HANDLE NO-UNDO. 
 DEFINE VARIABLE wh-sequencia          AS WIDGET-HANDLE NO-UNDO.
 DEFINE VARIABLE wh-it-codigo          AS WIDGET-HANDLE NO-UNDO.
 DEFINE VARIABLE wlh-flag-atualiz      AS WIDGET-HANDLE NO-UNDO.
 DEFINE VARIABLE wlh-parcela           AS WIDGET-HANDLE NO-UNDO.

 ASSIGN c-char = ENTRY(NUM-ENTRIES(p-wgh-object:FILE-NAME,"~/"),p-wgh-object:FILE-NAME,"~/").

/*MESSAGE "p-ind-event " p-ind-event  SKIP 
        "p-ind-object" p-ind-object SKIP 
        "p-wgh-object" p-wgh-object SKIP 
        "p-wgh-frame " p-wgh-frame  SKIP 
        "p-cod-table " p-cod-table  SKIP 
        "p-row-table " STRING(p-row-table)  SKIP 
        "c-char      " c-char VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

 /********* Funá∆o padr∆o para buscar handle do campo *********/ 
 FUNCTION buscarHandlecampo RETURNS WIDGET-HANDLE(INPUT campo AS CHARACTER,
                                                  INPUT-OUTPUT pointer AS WIDGET-HANDLE).

     DEFINE VARIABLE wh-grupo AS WIDGET-HANDLE NO-UNDO.
     DEFINE VARIABLE wh-child AS WIDGET-HANDLE NO-UNDO.

     IF pointer <> ? THEN
         wh-grupo  = pointer:NEXT-SIBLING.
     ELSE wh-grupo = p-wgh-frame:FIRST-CHILD.

     DO WHILE VALID-HANDLE(wh-grupo):

         CASE wh-grupo:NAME:
             WHEN campo THEN DO:
                 pointer = wh-grupo:HANDLE.
                 RETURN wh-grupo:HANDLE.
             END.
         END CASE.

         IF wh-grupo:TYPE="field-group" THEN
             wh-grupo = wh-grupo:FIRST-CHILD.
         ELSE
             wh-grupo = wh-grupo:NEXT-SIBLING.
     END.
 END FUNCTION.

  /********* Funá∆o padr∆o para buscar handle do campo *********/ 
 FUNCTION Get-Browse-Column-Number RETURNS INTEGER(INPUT par-brw AS WIDGET-HANDLE , c-param AS CHARACTER).
     DEFINE VARIABLE wh-child AS WIDGET-HANDLE NO-UNDO.
     DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
     DO ii = 1 TO par-brw:NUM-COLUMNS:
         IF par-brw:GET-BROWSE-COLUMN(ii):NAME = c-param THEN RETURN ii.
     END.
 END FUNCTION.


/* Tratamento para a pg2 nío aparecer junto com a pg1 */
IF  p-ind-event  = "INITIALIZE"
AND p-ind-object = "CONTAINER" THEN DO:
    
    ASSIGN wgh-window = p-wgh-object.
END.

IF  p-ind-event  = "AFTER-OPEN-QUERY"
AND p-ind-object = "BROWSER" 
AND c-char       = "b13in055.w" THEN DO:
   
    IF VALID-HANDLE(wgh-window) THEN DO:
        RUN select-page IN wgh-window(INPUT 4).
        RUN select-page IN wgh-window(INPUT 2).
        RUN select-page IN wgh-window(INPUT 1).
    END.
END.
/* FIM DO TRATAMENTO */

/* Folder Solicitaá∆o */
IF p-ind-event = "INITIALIZE" 
 AND p-ind-object = "BROWSER" 
 AND c-char = "b13in055.w" THEN DO:

    ASSIGN wh-aux = p-wgh-frame:FIRST-CHILD. /* pegando o Field-Group*/ 
    ASSIGN wh-aux = wh-aux:FIRST-CHILD.       /* pegando o 1o. Campo*/
    DO WHILE wh-aux <> ?:
        IF wh-aux:NAME = "BR-TABLE" THEN DO:
           wh-cc0523-solic = wh-aux.

            ASSIGN wh-it-prioridade = wh-cc0523-solic:ADD-CALC-COLUMN("CHARACTER", "x(10)" , " " , "Prioridade Solicitaá∆o" , wh-cc0523-solic:NUM-COLUMNS) wh-it-prioridade:NAME = "prioridade-aprov"   wh-it-prioridade:WIDTH = 20.
                
            wh-cc0523-solic:MOVE-COLUMN( Get-Browse-Column-Number(wh-cc0523-solic,"prioridade-aprov"), 2 ).

            ASSIGN wh-cc0523-solic:PRIVATE-DATA = STRING(wh-cc0523-solic:QUERY) + "," + STRING(wh-it-prioridade) + "," + STRING(wh-cc0523-solic).

            LEAVE.
        END.
        ASSIGN wh-aux = wh-aux:NEXT-SIBLING.            
    END.

   /* Torna o programa persistente */
     RUN upc/upc-cc0523.p PERSISTENT SET h-upc-cc0523 (INPUT "",            
                                                       INPUT "",            
                                                       INPUT p-wgh-object,  
                                                       INPUT p-wgh-frame,   
                                                       INPUT "",           
                                                       INPUT p-row-table).

     IF VALID-HANDLE (wh-cc0523-solic) THEN 

         ON ROW-DISPLAY OF wh-cc0523-solic PERSISTENT RUN pi-mostra-valor IN h-upc-cc0523.
     
END.

/* Folder Ordens */
IF p-ind-event = "INITIALIZE" 
AND p-ind-object = "BROWSER" 
AND c-char = "b12in055.w" THEN DO:

    ASSIGN wh-aux = p-wgh-frame:FIRST-CHILD. /* pegando o Field-Group*/ 
    ASSIGN wh-aux = wh-aux:FIRST-CHILD.       /* pegando o 1o. Campo*/
    DO WHILE wh-aux <> ?:
        IF wh-aux:NAME = "BR-TABLE" THEN DO:
           wh-cc0523-ordem = wh-aux.

            ASSIGN wh-ordem-prioridade = wh-cc0523-ordem:ADD-CALC-COLUMN("CHARACTER", "x(10)" , " " , "Prioridade Ordem" , wh-cc0523-ordem:NUM-COLUMNS) wh-ordem-prioridade:NAME = "prioridade-aprov"  wh-ordem-prioridade:WIDTH = 18.
                
            wh-cc0523-ordem:MOVE-COLUMN( Get-Browse-Column-Number(wh-cc0523-ordem,"prioridade-aprov"), 2 ).

            ASSIGN wh-cc0523-ordem:PRIVATE-DATA = STRING(wh-cc0523-ordem:QUERY) + "," + STRING(wh-ordem-prioridade) + "," + STRING(wh-cc0523-ordem).

            LEAVE.
        END.
        ASSIGN wh-aux = wh-aux:NEXT-SIBLING.            
    END.

   /* Torna o programa persistente */
    RUN upc/upc-cc0523.p PERSISTENT SET h-upc-cc0523 (INPUT "",            
                                                      INPUT "",            
                                                      INPUT p-wgh-object,  
                                                      INPUT p-wgh-frame,   
                                                      INPUT "",           
                                                      INPUT p-row-table).

    IF VALID-HANDLE (wh-cc0523-ordem) THEN 

        ON ROW-DISPLAY OF wh-cc0523-ordem PERSISTENT RUN pi-mostra-valor-ordem IN h-upc-cc0523.
     
END.

/* Folder Pedidos */
IF p-ind-event = "INITIALIZE" 
AND p-ind-object = "BROWSER" 
AND c-char = "b15in055.w" THEN DO:

    ASSIGN wh-aux = p-wgh-frame:FIRST-CHILD. /* pegando o Field-Group*/ 
    ASSIGN wh-aux = wh-aux:FIRST-CHILD.       /* pegando o 1o. Campo*/
    DO WHILE wh-aux <> ?:
        IF wh-aux:NAME = "BR-TABLE" THEN DO:
            wh-cc0523-ordem-a = wh-aux.
            
            ASSIGN wh-ordem-prioridade-a = wh-cc0523-ordem-a:ADD-CALC-COLUMN("CHARACTER", "x(10)" , " " , "Prioridade Ordem" , wh-cc0523-ordem-a:NUM-COLUMNS) wh-ordem-prioridade-a:NAME = "prioridade-aprov"  wh-ordem-prioridade-a:WIDTH = 18.
                
            wh-cc0523-ordem-a:MOVE-COLUMN( Get-Browse-Column-Number(wh-cc0523-ordem-a,"prioridade-aprov"), 2 ).
            
            ASSIGN wh-cc0523-ordem-a:PRIVATE-DATA = STRING(wh-cc0523-ordem-a:QUERY) + "," + STRING(wh-ordem-prioridade-a) + ',' + STRING(wh-cc0523-ordem-a).
 
            LEAVE.
        END.
        ASSIGN wh-aux = wh-aux:NEXT-SIBLING.            
    END.

  /* Torna o programa persistente */
    RUN upc/upc-cc0523.p PERSISTENT SET h-upc-cc0523 (INPUT "",            
                                                      INPUT "",            
                                                      INPUT p-wgh-object,  
                                                      INPUT p-wgh-frame,   
                                                      INPUT "",           
                                                      INPUT p-row-table).

    IF VALID-HANDLE (wh-cc0523-ordem-a) THEN DO:
        
        ON ROW-DISPLAY OF wh-cc0523-ordem-a PERSISTENT RUN pi-mostra-valor-pedido IN h-upc-cc0523.

    END.
END.

 /****** Procedure Mostra Valor Solicitacao ******/ 
PROCEDURE pi-mostra-valor: 

    ASSIGN wlh-qry = WIDGET-HANDLE(entry(1,wh-cc0523-solic:PRIVATE-DATA)) 
           wh-it-prioridade = WIDGET-HANDLE(entry(2,wh-cc0523-solic:PRIVATE-DATA))
           wlh-brw = WIDGET-HANDLE(entry(3,wh-cc0523-solic:PRIVATE-DATA)) .
    
    IF VALID-HANDLE(wlh-qry) THEN DO:
        ASSIGN wlh-row          = wlh-qry:GET-BUFFER-HANDLE(1)
               wlh-cod-estabel  = wlh-row:BUFFER-FIELD("cod-estabel")
               wh-nr-requisicao = wlh-row:BUFFER-FIELD("nr-requisicao")
               wh-it-codigo     = wlh-row:BUFFER-FIELD("it-codigo").

        FIND FIRST it-requisicao NO-LOCK
            WHERE it-requisicao.nr-requisicao = INT(wh-nr-requisicao:BUFFER-VALUE)
              AND it-requisicao.cod-estabel = string(wlh-cod-estabel:BUFFER-VALUE)
              AND it-requisicao.it-codigo   = STRING(wh-it-codigo:BUFFER-VALUE) NO-ERROR.

        IF AVAIL it-requisicao THEN DO:

            IF it-requisicao.prioridade-aprov = 150 THEN
                ASSIGN wh-it-prioridade:SCREEN-VALUE = "BAIXA".
                
            IF it-requisicao.prioridade-aprov = 450 THEN 
                ASSIGN wh-it-prioridade:SCREEN-VALUE = "MêDIA".
          
            IF it-requisicao.prioridade-aprov = 750 THEN
                ASSIGN wh-it-prioridade:SCREEN-VALUE = "ALTA".
                
            IF it-requisicao.prioridade-aprov = 999 THEN
                ASSIGN wh-it-prioridade:SCREEN-VALUE = "MUITO ALTA".
                
        END.
    END.
 
END PROCEDURE.

 /****** Procedure Mostra Valor Ordem ******/ 
PROCEDURE pi-mostra-valor-ordem:

    ASSIGN wlh-qry = WIDGET-HANDLE(entry(1,wh-cc0523-ordem:PRIVATE-DATA)) 
           wh-ordem-prioridade = WIDGET-HANDLE(entry(2,wh-cc0523-ordem:PRIVATE-DATA))
           wlh-brw = WIDGET-HANDLE(entry(3,wh-cc0523-ordem:PRIVATE-DATA)) .
    
    IF VALID-HANDLE(wlh-qry) THEN DO:
        ASSIGN wlh-row          = wlh-qry:GET-BUFFER-HANDLE(1)
               wlh-numero-ordem  = wlh-row:BUFFER-FIELD("numero-ordem")
               wlh-cod-comprado  = wlh-row:BUFFER-FIELD("cod-comprado").

        FIND ordem-compra NO-LOCK
            WHERE ordem-compra.numero-ordem = INT(wlh-numero-ordem:BUFFER-VALUE) NO-ERROR.

        IF AVAIL ordem-compra THEN DO:

            IF ordem-compra.prioridade-aprov = 1 THEN
                ASSIGN wh-ordem-prioridade:SCREEN-VALUE = "BAIXA".
                
            IF ordem-compra.prioridade-aprov = 2 THEN 
                ASSIGN wh-ordem-prioridade:SCREEN-VALUE = "MêDIA".
           
            IF ordem-compra.prioridade-aprov = 3 THEN
                ASSIGN wh-ordem-prioridade:SCREEN-VALUE = "ALTA".
                
            IF (ordem-compra.prioridade-aprov = 4 OR
                ordem-compra.prioridade-aprov = 0) OR 
                ordem-compra.prioridade-aprov = ? THEN
                ASSIGN wh-ordem-prioridade:SCREEN-VALUE = "MUITO ALTA".

        END.
        
    END.
 
END PROCEDURE.

 /****** Procedure Mostra Valor Pedido ******/ 
 PROCEDURE pi-mostra-valor-pedido: 

    ASSIGN wlh-qry = WIDGET-HANDLE(entry(1,wh-cc0523-ordem-a:PRIVATE-DATA)) 
           wh-ordem-prioridade-a = WIDGET-HANDLE(entry(2,wh-cc0523-ordem-a:PRIVATE-DATA))
           wlh-brw = WIDGET-HANDLE(entry(3,wh-cc0523-ordem-a:PRIVATE-DATA)) .

    
    IF VALID-HANDLE(wlh-qry) THEN DO:
        ASSIGN wlh-row          = wlh-qry:GET-BUFFER-HANDLE(1)
               wlh-numero-ordem-a = wlh-row:BUFFER-FIELD("numero-ordem").

        FIND ordem-compra NO-LOCK
            WHERE ordem-compra.numero-ordem = INT(wlh-numero-ordem-a:BUFFER-VALUE) NO-ERROR.

        IF AVAIL ordem-compra THEN DO:

            IF ordem-compra.prioridade-aprov = 1 THEN
                ASSIGN wh-ordem-prioridade-a:SCREEN-VALUE = "BAIXA".
                
            IF ordem-compra.prioridade-aprov = 2 THEN 
                ASSIGN wh-ordem-prioridade-a:SCREEN-VALUE = "MêDIA".
           
            IF ordem-compra.prioridade-aprov = 3 THEN
                ASSIGN wh-ordem-prioridade-a:SCREEN-VALUE = "ALTA".
                
            IF (ordem-compra.prioridade-aprov = 4 OR
                ordem-compra.prioridade-aprov = 0) OR 
                ordem-compra.prioridade-aprov = ? THEN
                ASSIGN wh-ordem-prioridade-a:SCREEN-VALUE = "MUITO ALTA".
            
        END.
    END.
 
END PROCEDURE.


 






        




