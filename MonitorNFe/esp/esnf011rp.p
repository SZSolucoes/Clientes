{include/i-prgvrs.i ESNF011RP 2.06.00.001}  /*** 010001 ***/
/*****************************************************************************
**       Programa: esnf011rp.p
**       Data....: 18/11/2010
**       Autor...: Edson de Souza
*******************************************************************************/

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field emit-ini          as integer format ">>9"
    field emit-fim          as integer format ">>9".

DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita AS RAW.

DEFINE VARIABLE h-acomp             AS HANDLE  NO-UNDO.

DEF INPUT PARAM raw-param AS RAW NO-UNDO.
DEF INPUT PARAM TABLE FOR tt-raw-digita.

DEFINE VARIABLE chexcelapplication AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chworkbook         AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chworksheet        AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE arquivo-at         AS CHAR NO-UNDO.
DEFINE VARIABLE arquivo-cp         AS CHAR NO-UNDO.
DEFINE VARIABLE arquivo-nv         AS CHAR NO-UNDO.
DEFINE VARIABLE i-linha            AS INT  NO-UNDO.
DEFINE VARIABLE iColumn            AS INTEGER INITIAL 1.
DEFINE VARIABLE iColumn2           AS INTEGER INITIAL 1.

DEFINE VARIABLE c-desc-forn AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-desc-item AS CHARACTER   NO-UNDO.

/* Propriedade : HorizontalAlignment (Alinhamento Horizontal) ******************/
&global-define xlHAlignCenter       -4108  /* 01 - Centralizado */
&global-define xlHAlignLeft         -4131  /* 04 - Esquerda */

/* Propriedade : LineStyle  (Estilo de Linha) **********************************/
&global-define xlContinuous     1       /* 01 - Continua */

{include/i-rpvar.i}
/* {cdp/cdcfgmat.i} */
/* {utp/ut-glob.i} */

create tt-param.
raw-transfer raw-param to tt-param.

ASSIGN c-programa = "ESNF011RP"
       c-versao   = "2.06"
       c-revisao  = "001".

{utp/ut-liter.i Relatorio_Item/Fornecedor * r}
ASSIGN c-titulo-relat = RETURN-VALUE.

{include/i-rpcab.i} 
{include/i-rpout.i}

VIEW FRAME f-cabec.
VIEW FRAME f-rodape.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Relatorio_Item/Fornecedor *}

find tt-param no-lock no-error.

if avail tt-param then do:

    assign iColumn = 5.

    if tt-param.classifica = 2 then do:

        CREATE "Excel.Application" chexcelapplication.
        chexcelapplication:visible = no.
        chexcelapplication:displayalerts = no.
        chexcelapplication:Workbooks:OPEN(search("modelo\esnf011-emit.xls")).
              
        for each nfe020
            where nfe020.cod-emitente >= tt-param.emit-ini 
              and nfe020.cod-emitente <= tt-param.emit-fim no-lock:
    
              find item
                  where item.it-codigo = nfe020.it-codigo no-lock no-error.
    
              if avail item then
                  assign c-desc-item = item.desc-item.
    
              find emitente
                  where emitente.cod-emitente = nfe020.cod-emitente no-lock no-error.
              
              if avail emitente then
                  assign c-desc-forn = emitente.nome-abrev.          
             
              run pi-inicializar in h-acomp (input return-value).
          
              assign chexcelapplication:range("A" + STRING(iColumn)):VALUE = nfe020.cod-emitente
                     chexcelapplication:range("A" + STRING(iColumn) + ":" + "B" + string(icolumn)):mergecells = true
                     chexcelapplication:range("A" + STRING(iColumn)):HorizontalAlignment ={&xlHAlignCenter}
                     chexcelapplication:range("C" + STRING(iColumn)):VALUE = c-desc-forn
                     chexcelapplication:range("C" + STRING(iColumn) + ":" + "D" + string(icolumn)):mergecells = true
                     chexcelapplication:range("C" + STRING(iColumn)):HorizontalAlignment ={&xlHAlignCenter}
                     chexcelapplication:range("E" + STRING(iColumn)):VALUE = nfe020.it-codigo
                     chexcelapplication:range("E" + STRING(iColumn) + ":" + "F" + string(icolumn)):mergecells = true
                     chexcelapplication:range("E" + STRING(iColumn)):HorizontalAlignment ={&xlHAlignCenter}
                     chexcelapplication:range("G" + STRING(iColumn)):VALUE = c-desc-item
                     chexcelapplication:range("G" + STRING(iColumn) + ":" + "H" + string(icolumn)):mergecells = true
                     chexcelapplication:range("G" + STRING(iColumn)):HorizontalAlignment ={&xlHAlignCenter}
                     chexcelapplication:range("I" + STRING(iColumn)):VALUE = nfe020.item-do-forn
                     chexcelapplication:range("I" + STRING(iColumn) + ":" + "J" + string(icolumn)):mergecells = true
                     chexcelapplication:range("I" + STRING(iColumn)):HorizontalAlignment ={&xlHAlignCenter}
                     iColumn = iColumn + 1.                                                                      
        end.
    end.

    if tt-param.classifica = 1 then do:

        CREATE "Excel.Application" chexcelapplication.
        chexcelapplication:visible = no.
        chexcelapplication:displayalerts = no.
        chexcelapplication:Workbooks:OPEN(search("modelo\esnf011-item.xls")).

        for each nfe020
            where nfe020.cod-emitente >= tt-param.emit-ini 
              and nfe020.cod-emitente <= tt-param.emit-fim no-lock:

              find item
                  where item.it-codigo = nfe020.it-codigo no-lock no-error.
        
              if avail item then
                  assign c-desc-item = item.desc-item.

              find emitente
                  where emitente.cod-emitente = nfe020.cod-emitente no-lock no-error.

              if avail emitente then
                  assign c-desc-forn = emitente.nome-abrev.          

              run pi-inicializar in h-acomp (input return-value).

              assign chexcelapplication:range("A" + STRING(iColumn)):VALUE = nfe020.it-codigo
                     chexcelapplication:range("A" + STRING(iColumn) + ":" + "B" + string(icolumn)):mergecells = true
                     chexcelapplication:range("A" + STRING(iColumn)):HorizontalAlignment ={&xlHAlignCenter}
                     chexcelapplication:range("C" + STRING(iColumn)):VALUE = c-desc-item
                     chexcelapplication:range("C" + STRING(iColumn) + ":" + "D" + string(icolumn)):mergecells = true
                     chexcelapplication:range("C" + STRING(iColumn)):HorizontalAlignment ={&xlHAlignCenter}
                     chexcelapplication:range("E" + STRING(iColumn)):VALUE = nfe020.item-do-forn
                     chexcelapplication:range("E" + STRING(iColumn) + ":" + "F" + string(icolumn)):mergecells = true
                     chexcelapplication:range("E" + STRING(iColumn)):HorizontalAlignment ={&xlHAlignCenter}
                     chexcelapplication:range("G" + STRING(iColumn)):VALUE = nfe020.cod-emitente
                     chexcelapplication:range("G" + STRING(iColumn) + ":" + "H" + string(icolumn)):mergecells = true
                     chexcelapplication:range("G" + STRING(iColumn)):HorizontalAlignment ={&xlHAlignCenter}
                     chexcelapplication:range("I" + STRING(iColumn)):VALUE = c-desc-forn
                     chexcelapplication:range("I" + STRING(iColumn) + ":" + "J" + string(icolumn)):mergecells = true
                     chexcelapplication:range("I" + STRING(iColumn)):HorizontalAlignment ={&xlHAlignCenter}
                     iColumn = iColumn + 1.                                  
        end.
    end.
end.

run pi-finalizar in h-acomp.
chexcelapplication:ActiveWorkbook:SaveAs(SESSION:TEMP-DIRECTORY + "esnf011" + STRING(TIME) + ".xls",,,,,,).
chexcelapplication:VISIBLE=TRUE.
release object chexcelapplication.


