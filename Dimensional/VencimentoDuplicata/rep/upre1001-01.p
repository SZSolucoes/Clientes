/******************************************************************************
**
**    Programa: upre1001.p 
**       Autor: Israel - Datasul Campinas  
**        Data: JUN/2008
**    Objetivo: UPC de Recebimento 
**    Atualizado em 2014 por Sottelli        
*******************************************************************************/
def input param p-ind-event  as char          no-undo.
def input param p-ind-object as char          no-undo.
def input param p-wgh-object as handle        no-undo.
def input param p-wgh-frame  as widget-handle no-undo.
def input param p-cod-table  as char          no-undo.
def input param p-row-table  as Rowid         no-undo.

/*** Definicao de Variavel Local
********************************/
def var c-objeto     as char          No-undo.
DEF VAR h-objeto     AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-frame      AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-filho      As WIDGET-HANDLE NO-UNDO.
DEF Var h-buffer     AS HANDLE        NO-UNDO.
Def Var l-replica    As Log           No-undo.
Def Var l-natur-prin As LOG           No-undo.
Def Var i-sem-ext    As INT           No-undo.
Def Var l-ok         As Log           No-undo.
Def Var h-acomp      As Handle        No-undo.
Def Var h-prog       As Handle        No-undo.
DEF VAR l-reabre-qry As Log           No-undo.
DEF VAR h_btb901zo     AS HANDLE        NO-UNDO.


/*** Definicao de Variavel Globais
**********************************/
Def New Global Shared Var rw-docum-est-re1001    As Rowid         No-undo.
Def New Global Shared Var rw-item-doc-est-re1001 As Rowid         No-undo.
Def New Global Shared Var wh-btconf-re1001-ems   As Widget-handle No-undo.
Def New Global Shared Var wh-btconf-re1001-cust  As Widget-handle No-undo.
Def New Global Shared Var wh-btextensao-re1001   As Widget-handle No-undo.
Def New Global Shared Var wh-btfirst-re1001      As Widget-handle No-undo.
Def New Global Shared Var wh-btlast-re1001       As Widget-handle No-undo.

Def New Global Shared Var wh-brson1-re1001       As Widget-handle No-undo.
Def New Global Shared Var h-qry-brson1-re1001    AS WIDGET-HANDLE NO-UNDO.
Def New Global Shared Var h-brson1-re1001-col1   As Widget-handle No-undo.
Def New Global Shared Var h-brson1-re1001-col2   As Widget-handle No-undo.
Def New Global Shared Var h-brson1-re1001-col3   As Widget-handle No-undo.

/*--------------------------------------------------------------------------------*/
DEFINE NEW GLOBAL SHARED VAR wgh-f-cod-emitente-upre1001 AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-f-serie-docto-upre1001  AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-f-nro-docto-upre1001    AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-f-nat-operacao-upre1001 AS WIDGET-HANDLE NO-UNDO.
/*--------------------------------------------------------------------------------*/

DEFINE VARIABLE EMITENTE  LIKE DOCUM-EST.COD-EMITENTE.
DEFINE VARIABLE SERIE     LIKE DOCUM-EST.SERIE-DOCTO.
DEFINE VARIABLE NRO-DOCTO LIKE DOCUM-EST.NRO-DOCTO.
DEFINE VARIABLE NAT-OPER  LIKE DOCUM-EST.NAT-OPERACAO.

/*** Definicao de temp-tables
**********************************/
Def Temp-table tt-log  No-undo
    Field serie-docto  As Char Format "x(5)"
    Field nro-docto    As Char Format "x(16)"
    Field cod-emitente As Int  Format ">>>>>>>>9"
    Field nat-operacao As Char Format "x(6)"
    FIELD erro         AS LOG  INIT YES
    Field cod-erro     As Char Format "x(6)" 
    Field des-erro     As Char Format "x(82)".

/*** Definicao de Forms
**********************************/
{include/i-rpvar.i} /* Definicao de variaveis para geracao de Logs */

Form Header
     Fill("-",132) At 1 Format "X(132)"
     c-empresa At 1 c-titulo-relat At 52
     Fill("-", 110) Format "x(110)" Today Format "99/99/9999"
     "-" String(Time, "HH:MM:SS") Skip(2)
With Stream-io Width 132 No-labels No-box Page-top Frame f-cabecalho.

Form Header
     Fill("-", 78) Format "x(76)" "DATASUL EMS 2.04 -" c-sistema Format "X(9)" 
     "-" c-programa "-" "V:" c-versao + c-revisao Format "X(11)" At 122
With Stream-io Width 132 No-labels No-box Page-bottom Frame f-rodape1.

/*** Identificando nome de objeto
***********************************/
c-objeto = Entry(Num-entries(p-wgh-object:Private-data, "~/"), p-wgh-object:Private-data, "~/").

/* Message "Evento     " p-ind-event  Skip */
/*         "Objeto     " p-ind-object Skip */
/*         "nome obj   " c-objeto     Skip */
/*         "Frame      " p-wgh-frame  Skip */
/*         "Nome Frame " p-wgh-frame:Name Skip */
/*         "Tabela     " p-cod-table  Skip */
/*         "ROWID      " String(p-row-table) Skip */
/*         p-wgh-object:File-name */
/*         View-as Alert-box Information. */


If p-ind-event  = "Before-Initialize" And
   p-ind-object = "Container"        Then
Do:
   Assign h-objeto = p-wgh-frame:First-child
          h-objeto = h-objeto:First-child.
   
   Do While h-objeto <> ?:
   
      If h-objeto:Type <> "field-group" Then 
      Do:
         CASE h-objeto:Name :
          
              WHEN "btconf" THEN
              Do:
                 wh-btconf-re1001-ems = h-objeto:Handle.
                
                 CREATE BUTTON wh-btconf-re1001-cust
                 ASSIGN flat-button = true
                        FRAME     = wh-btconf-re1001-ems:Frame
                        TOOLTIP   = wh-btconf-re1001-ems:TOOLTIP
                        HELP      = wh-btconf-re1001-ems:HELP   
                        WIDTH     = wh-btconf-re1001-ems:Width
                        HEIGHT    = wh-btconf-re1001-ems:Height
                        COL       = wh-btconf-re1001-ems:col
                        ROW       = wh-btconf-re1001-ems:Row
                        VISIBLE   = YES
                        SENSITIVE = Yes.
                 
                 wh-btconf-re1001-cust:Load-image (wh-btconf-re1001-ems:Image).
                 /*wh-btconf-re1001-cust:Move-before-tab-item(wh-btconf-re1001-ems). */
                 wh-btconf-re1001-ems:Visible = False.
                 wh-btconf-re1001-ems:sensitive = False.
                 wh-btconf-re1001-ems:MOVE-TO-BOTTOM().        

                 On 'CHOOSE':U Of wh-btconf-re1001-cust Persistent Run rep/upre1001-01.p (Input "CHOOSE_BTCONF_CUST",
                                                                                          Input p-ind-object,
                                                                                          Input p-wgh-object,
                                                                                          Input p-wgh-frame ,
                                                                                          Input p-cod-table ,
                                                                                          Input p-row-table ).
                                                                                         
                 
              End.
              
              WHEN "btfirst" THEN
                 wh-btfirst-re1001 = h-objeto:HANDLE.

              WHEN "btlast" THEN
                 wh-btlast-re1001 = h-objeto:HANDLE.
                 
              WHEN "cod-emitente" THEN
                   ASSIGN wgh-f-cod-emitente-upre1001  = h-objeto:HANDLE.
              WHEN "serie-docto"  THEN 
                   ASSIGN wgh-f-serie-docto-upre1001   = h-objeto:HANDLE.
              WHEN "nro-docto"    THEN 
                   ASSIGN wgh-f-nro-docto-upre1001     = h-objeto:HANDLE.
              WHEN "nat-operacao" THEN
                   ASSIGN wgh-f-nat-operacao-upre1001  = h-objeto:HANDLE.
                             
              WHEN "fpage1" THEN
              DO:
                 ASSIGN h-filho = h-objeto:HANDLE
                        h-filho = h-filho:FIRST-CHILD.
        
                 DO WHILE h-filho <> ? :
        
                    IF h-filho:type <> "field-group" THEN 
                    DO:
                       IF h-filho:NAME = "brson1" THEN
                       DO:
                          Assign wh-brson1-re1001      = h-filho:HANDLE
                                 wh-brson1-re1001:Help = "Tecle <F3> para informar os dados adicionais do item"
                                 h-qry-brson1-re1001   = wh-brson1-re1001:QUERY.
     
                          On 'row-display' Of wh-brson1-re1001 Persistent Run rep/upre1001-01.p (Input "Row-display-browse",
                                                                                                 Input p-ind-object,
                                                                                                 Input p-wgh-object,
                                                                                                 Input p-wgh-frame ,
                                                                                                 Input p-cod-table ,
                                                                                                 Input p-row-table ).
     
                          On 'F3' Of wh-brson1-re1001 Persistent Run rep/upre1001-01.p (Input "F3-browser",
                                                                                        Input p-ind-object,
                                                                                        Input p-wgh-object,
                                                                                        Input p-wgh-frame ,
                                                                                        Input p-cod-table ,
                                                                                        Input p-row-table ).
                                                                                     
                          
                          
                          ASSIGN h-brson1-re1001-col1 = wh-brson1-re1001:Add-calc-column("character","x(6)","","Natur Oper",13)
                                 h-brson1-re1001-col2 = wh-brson1-re1001:Add-calc-column("character","x(3)","","ICMS Retido",14)
                                 h-brson1-re1001-col3 = wh-brson1-re1001:Add-calc-column("decimal",">>>,>>>,>>9.99","","Base Revend",15).
                       END.
     
                       IF h-filho:NAME = "btitens" THEN
                       Do:
                          CREATE BUTTON wh-btextensao-re1001
                          ASSIGN FRAME     = h-filho:Frame
                                 TOOLTIP   = h-filho:TOOLTIP
                                 HELP      = h-filho:HELP   
                                 Label     = "Extens∆o"
                                 WIDTH     = h-filho:Width
                                 HEIGHT    = h-filho:Height
                                 COL       = h-filho:col + 10
                                 ROW       = h-filho:Row
                                 VISIBLE   = YES
                                 SENSITIVE = Yes.
                 
                          wh-btextensao-re1001:Move-after-tab-item(h-filho). 
                          
                          On 'CHOOSE':U Of wh-btextensao-re1001 Persistent Run rep/upre1001-01.p (Input "CHOOSE_bt-extensao",
                                                                                                  Input p-ind-object,
                                                                                                  Input p-wgh-object,
                                                                                                  Input p-wgh-frame ,
                                                                                                  Input p-cod-table ,
                                                                                                  Input p-row-table ).
                       End.
                       
                       
                       
                       ASSIGN h-filho = h-filho:NEXT-SIBLING.
                    END.
        
                    ELSE
                       ASSIGN h-filho = h-filho:FIRST-CHILD.
        
                 END.
              END.

         END CASE.
         
         Assign h-objeto = h-objeto:Next-sibling.
   
      End.
   
      Else 
         Assign h-objeto = h-objeto:First-child.
   
   End.
   
End.

If p-ind-event  = "After-display" And
   p-ind-object = "Container"     Then
   rw-docum-est-re1001 = p-row-table.

If p-ind-event  = "After-value-changed" And
   p-ind-object = "Container"           And
   p-cod-table  = "item-doc-est"        Then
   rw-item-doc-est-re1001 = p-row-table.

If p-ind-event = "CHOOSE_BTCONF_CUST" Then 
Do:   

   ASSIGN  EMITENTE  = int(wgh-f-cod-emitente-upre1001:screen-value)
           SERIE     = wgh-f-serie-docto-upre1001:screen-value 
           NRO-DOCTO = wgh-f-nro-docto-upre1001:screen-value 
           NAT-OPER  = wgh-f-nat-operacao-upre1001:screen-value.    
           
   Find First docum-est 
        Where Rowid(docum-est) = rw-docum-est-re1001 
   No-lock No-error.

   If Not Avail docum-est Then
      Return.
      
      
   run upc/upc-re1001-sz.p (input rowid(docum-est)).      

   ASSIGN l-replica    = No
          l-natur-prin = NO
          i-sem-ext    = ?.

   verifica_replicacao :
   For Each item-doc-est Of docum-est NO-LOCK :

       FIND First mgesp.ext-item-doc-est Of item-doc-est NO-LOCK NO-ERROR.

       IF NOT AVAIL ext-item-doc-est THEN
       DO:
          i-sem-ext = item-doc-est.sequencia.
          Leave verifica_replicacao.
       END.

       IF ext-item-doc-est.nat-oper-ext <> item-doc-est.nat-operacao THEN
          l-replica = Yes.
       ELSE
          l-natur-prin = YES.

   End.
   
    

   IF i-sem-ext <> ? THEN
   DO:
      Run utp/ut-msgs.p (Input "show":U, 
                         Input 17006, 
                         Input "Sequància " + STRING(i-sem-ext) + " sem extens∆o!~~Deve ser informado extens∆o para todos os itens do docto.").

      Return No-apply.
   END.

   IF NOT l-natur-prin THEN
   DO:
      Run utp/ut-msgs.p (Input "show":U, 
                         Input 17006, 
                         Input "Processo inv†lido!~~Deve existir pelo menos 1 item com a mesma natureza do documento principal.").

      Return No-apply.
   END.

   If l-replica Then
   Do:
      Run rep\upre1001-d01.w (Input  Rowid(docum-est),
                              Output l-ok,
                              Output Table tt-log).

      If Not l-ok Then 
         Return No-apply.
         
      /*-------------- UPRE1001-02 VERIFICA ALTERAÄ«O DO C‡DIGO CST --------------*/
      RUN REP/UPRE1001-02.P (INPUT  EMITENTE,
                             INPUT  SERIE,
                             INPUT  NRO-DOCTO,
                             INPUT  NAT-OPER).
      /*--------------------------------------------------------------------------*/

      Find First param-global No-lock No-error.
   
      {utp/ut-liter.i RECEBIMENTO * }
      Assign c-sistema = Return-value.
      
      {utp/ut-liter.i Log_de_replicacao_dos_doctos * }
      Assign c-titulo-relat = Return-value
      
             c-empresa     = param-global.grupo
             c-programa    = "UPRE1001"
             c-versao      = "2.04.00."
             c-revisao     = "001".
   
      Run utp/ut-acomp.p persistent set h-acomp.  
   
      {utp/ut-liter.i Gerando_log_de_replicacao}
      
      Run pi-inicializar in h-acomp (input Return-value). 
   
      Run pi-desabilita-cancela In h-acomp.
   
      /* Log de divergencias */
      Output To Value(Session:Temp-directory + "upre1001.tmp") Page-size 64.

         l-reabre-qry = YES.
   
         View Frame f-cabecalho.
         View Frame f-rodape1.
   
         For Each tt-log No-lock :

             IF tt-log.erro THEN l-reabre-qry = NO.

             Disp tt-log.serie-docto  Column-label "Serie"                   
                  tt-log.nro-docto    Column-label "Docto"                   
                  tt-log.cod-emitente Column-label "Emitente"                
                  tt-log.nat-operacao Column-label "Natureza"                
                  tt-log.cod-erro     Column-label "Cod Msg" 
                  tt-log.des-erro     Column-label "Desc Msg"    
             With Stream-io Width 200.

         End.
         
      Output Close.
   
      /* Mostra divergencias entre recebimento x separacao de pick */
      If Search(Session:Temp-directory + "upre1001.tmp") <> ? Then
      Do:
         Run pi-acompanhar in h-acomp (input "Imprimindo Log...").
   
         Run utp/ut-utils.p Persistent Set h-prog.
   
         Run Execute In h-prog (Input "NOTEPAD.EXE",
                                Input chr(32) + Search(Session:Temp-directory + "upre1001.tmp")).
   
         Delete procedure h-prog.
      End.
      
      Run pi-finalizar in h-acomp.

      IF l-reabre-qry THEN
      DO:
         APPLY "choose" TO wh-btfirst-re1001.
         APPLY "choose" TO wh-btlast-re1001.
      END.
   End.
   Else DO:  
         IF VALID-HANDLE(WH-BTCONF-RE1001-EMS) THEN
            APPLY "CHOOSE" TO WH-BTCONF-RE1001-EMS.     
        
        /*-------------- UPRE1001-02 VERIFICA ALTERAÄ«O DO C‡DIGO CST --------------*/
        RUN REP/UPRE1001-02.P (INPUT  EMITENTE,
                               INPUT  SERIE,
                               INPUT  NRO-DOCTO,
                               INPUT  NAT-OPER).
        /*--------------------------------------------------------------------------*/ 
   END.  
   
End.

IF p-ind-event  = "Row-display-browse" THEN 
DO:
   ASSIGN h-brson1-re1001-col1:SCREEN-VALUE = ""
          h-brson1-re1001-col2:SCREEN-VALUE = ""
          h-brson1-re1001-col3:SCREEN-VALUE = "".

   h-qry-brson1-re1001:Get-current(1) NO-ERROR.

   h-buffer = h-qry-brson1-re1001:Get-buffer-handle(1) NO-ERROR.

   Find First item-doc-est 
        WHERE item-doc-est.serie-docto  = STRING(h-buffer:Buffer-field("serie-docto"):BUFFER-VALUE)
          AND item-doc-est.nro-docto    = STRING(h-buffer:Buffer-field("nro-docto"):BUFFER-VALUE)
          AND item-doc-est.cod-emitente = INT(h-buffer:Buffer-field("cod-emitente"):BUFFER-VALUE)
          AND item-doc-est.nat-operacao = STRING(h-buffer:Buffer-field("nat-operacao"):BUFFER-VALUE)
          AND item-doc-est.sequencia    = Int(h-buffer:Buffer-field("sequencia"):BUFFER-VALUE)
   No-lock No-error.
   
   If Not Avail item-doc-est Then
      Return.

   Find First mgesp.ext-item-doc-est Of item-doc-est No-lock No-error.

   If Avail ext-item-doc-est Then
      Assign h-brson1-re1001-col1:SCREEN-VALUE = ext-item-doc-est.nat-oper-ext
             h-brson1-re1001-col2:SCREEN-VALUE = If ext-item-doc-est.icms-retido Then "SIM" Else "N«O"
             h-brson1-re1001-col3:SCREEN-VALUE = STRING(ext-item-doc-est.base-revend).
END.

IF p-ind-event = "F3-browser"         Or
   p-ind-event = "CHOOSE_bt-extensao" THEN 
DO:
   Run rep\upre1001-d02.w (Input rw-item-doc-est-re1001).

   Apply "row-display" To wh-brson1-re1001.
End.



