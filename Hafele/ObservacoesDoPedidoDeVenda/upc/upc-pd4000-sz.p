
/*****************************************************************************
 ** PROGRAMA..: UPC-PD4000-U01.P
 ** OBJETIVO..: UPC NA IMPLANTAÄ«O DE PEDIDOS - PD4000
 ** AUTOR.....: SZ SOLU∞ÜES
 ** CLIENTE...: HAFELE
 ** VERSAO....: 2.12.00.001 - 19/01/2017
 ** ALTERA∞ÜES:
 ******************************************************************************/
 
/*************************** PARÙMETROS PADRÄO *************************************/
DEF INPUT PARAMETER p-ind-event            AS CHAR             NO-UNDO.
DEF INPUT PARAMETER p-ind-object           AS CHAR             NO-UNDO.
DEF INPUT PARAMETER p-wgh-object           AS HANDLE           NO-UNDO.
DEF INPUT PARAMETER p-wgh-frame            AS WIDGET-HANDLE    NO-UNDO.
DEF INPUT PARAMETER p-cod-table            AS CHAR             NO-UNDO.
DEF INPUT PARAMETER p-row-table            AS ROWID            NO-UNDO.

/*************************** GLOBAL VARIABLE DEFINITIONS **************************/
DEF NEW GLOBAL SHARED VAR wh-objeto             AS WIDGET-HANDLE NO-UNDO.    
DEF NEW GLOBAL SHARED VAR h-it-codigo           AS HANDLE           NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-nr-pedcli           AS HANDLE           NO-UNDO. 
DEF NEW GLOBAL SHARED VAR h-nome-abrev          AS HANDLE           NO-UNDO. 
DEF NEW GLOBAL SHARED VAR wh-observacoes        AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-nome-abrev         AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-nr-pedcli          AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-num-pedido-origem  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-cond-espec         AS WIDGET-HANDLE NO-UNDO.

/*************************** LOCAL VARIABLE DEFINITIONS **************************/
DEF VAR h-objeto        AS WIDGET-HANDLE   NO-UNDO.
DEF VAR c-objeto        AS CHAR            NO-UNDO.
DEF VAR h-frame         AS HANDLE          NO-UNDO.
DEF VAR h-frame-aux     AS HANDLE          NO-UNDO.
DEF VAR wh-window       AS HANDLE          NO-UNDO.
DEF VAR wgh-grupo       AS HANDLE          NO-UNDO.
DEF VAR wgh-frame       AS HANDLE          NO-UNDO.
DEF VAR wgh-child       AS HANDLE          NO-UNDO.
DEF VAR h-frame10       AS HANDLE          NO-UNDO.
DEF VAR h-frame1        AS HANDLE          NO-UNDO.

/* main block */
 ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:FILE-NAME, "~/"),
                   p-wgh-object:FILE-NAME, "~/").

 /*MESSAGE "Evento     " p-ind-event  SKIP         
            "Objeto     " p-ind-object SKIP         
            "nome obj   " c-objeto     SKIP         
            "Frame      " p-wgh-frame  SKIP         
            "Tabela     " p-cod-table  SKIP         
            "ROWID      " STRING(p-row-table) SKIP  
            VIEW-AS ALERT-BOX INFORMATION.*/ 


 IF p-ind-event   = "before-initialize"
 AND p-ind-object = "container" THEN DO:
     ASSIGN wgh-grupo = p-wgh-frame:FIRST-CHILD  
            wgh-grupo = wgh-grupo:FIRST-CHILD.

     DO WHILE VALID-HANDLE(wgh-grupo) :

         IF wgh-grupo:TYPE = "FRAME" THEN DO:

             CASE wgh-grupo:NAME:
                 WHEN "fPage10"  THEN DO:
                     ASSIGN wgh-frame = wgh-grupo:HANDLE.

                     h-frame10 = wgh-grupo:FIRST-CHILD.
    
                     DO WHILE valid-hANDle(h-frame10):
                         IF h-frame10:TYPE <> "field-group" THEN DO:
                             IF h-frame10:NAME = "observacoes" THEN 
                                 wh-observacoes = h-frame10:HANDLE.

                             IF  h-frame10:NAME = "cond-espec" THEN
                                 ASSIGN wh-cond-espec = h-frame10:HANDLE. 
        
                             ASSIGN h-frame10 = h-frame10:NEXT-SIBLING.
                         END.
                         ELSE
                             ASSIGN h-frame10 = h-frame10:FIRST-CHILD.
                     END.
                 END.
                 WHEN "fPage1" THEN DO:
                     ASSIGN wgh-frame = wgh-grupo:HANDLE.

                     h-frame1 = wgh-grupo:FIRST-CHILD.
        
                     DO WHILE valid-hANDle(h-frame1):
                         IF h-frame1:TYPE <> "field-group" THEN DO:
                             IF h-frame1:NAME = "num-pedido-origem" THEN 
                                 wh-num-pedido-origem = h-frame1:HANDLE.

                             ASSIGN h-frame1 = h-frame1:NEXT-SIBLING.
                        END.
                        ELSE
                            ASSIGN h-frame1 = h-frame1:FIRST-CHILD.
                     END.
                 END.
             END CASE.
         END.

         ASSIGN wgh-grupo = wgh-grupo:NEXT-SIBLING.

     END.
    
END.

IF p-ind-event = "AFTER-INITIALIZE":U THEN DO:

    ASSIGN wgh-grupo = p-wgh-frame:FIRST-CHILD.

    DO WHILE VALID-HANDLE(wgh-grupo):

        ASSIGN wgh-child = wgh-grupo:FIRST-CHILD. 

        DO WHILE VALID-HANDLE(wgh-child):

            CASE wgh-child:TYPE:

                WHEN "fill-in" THEN DO:
                    
                    IF wgh-child:NAME = "nome-abrev" THEN
                        ASSIGN wh-nome-abrev = wgh-child:HANDLE.

                    IF wgh-child:NAME = "nr-pedcli" THEN
                        ASSIGN wh-nr-pedcli = wgh-child:HANDLE.
                    
                END.                 
            END.

            ASSIGN wgh-child = wgh-child:NEXT-SIBLING NO-ERROR.

        END.
        LEAVE.
    END. 

    ASSIGN wgh-grupo = p-wgh-frame:FIRST-CHILD  
           wgh-grupo = wgh-grupo:FIRST-CHILD.
  
    DO  WHILE VALID-HANDLE(wgh-grupo):

        IF wgh-grupo:TYPE = "FRAME"
            AND wgh-grupo:NAME = "fPage10" THEN
            ASSIGN wgh-frame = wgh-grupo:HANDLE.
        
        IF wgh-grupo:TYPE = "FRAME"
            AND wgh-grupo:NAME = "fPage1" THEN
            ASSIGN wgh-frame = wgh-grupo:HANDLE.
       
        IF VALID-HANDLE(wgh-frame) THEN
            LEAVE.
        ELSE
            ASSIGN wgh-grupo = wgh-grupo:NEXT-SIBLING.
    END.
   
    ASSIGN wgh-grupo = wgh-frame:FIRST-CHILD.

    IF VALID-HANDLE(wh-num-pedido-origem) THEN DO:
        
        ON "ENTRY" OF wh-num-pedido-origem PERSISTENT RUN upc/upc-pd4000-sz.p (INPUT "EntryNumPedidoOrigem",
                                                                               STRING(wh-nr-pedcli),
                                                                               INPUT p-wgh-object,
                                                                               INPUT p-wgh-frame ,
                                                                               INPUT p-cod-table ,
                                                                               INPUT p-row-table).
    END.
END.

IF p-ind-event = 'EntryNumPedidoOrigem' THEN DO:

    FIND emitente NO-LOCK
        WHERE emitente.nome-abrev = wh-nome-abrev:SCREEN-VALUE NO-ERROR.

    IF AVAIL emitente THEN DO:
        FIND ext-emitente NO-LOCK
            WHERE ext-emitente.cod-emitente = emitente.cod-emitente NO-ERROR.
       
        IF AVAIL ext-emitente THEN
            ASSIGN wh-observacoes:SCREEN-VALUE = ext-emitente.observacao-pedido
                   wh-cond-espec:SCREEN-VALUE = ext-emitente.observacao-complentar.
    END.

END.


RETURN "OK".                     
                     

