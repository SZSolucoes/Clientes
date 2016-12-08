/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESNF013RP 2.06.00.001}  /*** 010001 ***/
/*****************************************************************************
**       Programa: ESNF013RP.p
**       Data....: 24/11/2010
**       Objetivo: Relatorio Monitor XML 
**       Vers’o..: 2.06.001 
*******************************************************************************/
define temp-table tt-param no-undo                
    field destino          as integer             
    field arquivo          as char format "x(35)" 
    field usuario          as char format "x(12)" 
    field data-exec        as date                
    field hora-exec        as integer             
    field classifica       as integer             
    field desc-classifica  as char format "x(40)" 
    field modelo-rtf       as char format "x(35)" 
    field l-habilitaRtf    as log                 
    field doc-ini          as char                
    field doc-fim          as char                
    field serie-ini        as char                
    field serie-fim        as char                
    field emit-ini         as integer             
    field emit-fim         as integer             
    field data-emis-ini    as date                
    field data-emis-fim    as date                
    field digit-receb      as log                 
    field nota-erro-neg    as log                 
    field atualiz-receb    as log                 
    field elimin-receb     as log                 
    field danfe-nao-autor  as log                 
    field doc-liber        as log. 

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
DEFINE VARIABLE c-nome-abrev       AS character format "x(12)"   NO-UNDO.

/* Propriedade : HorizontalAlignment (Alinhamento Horizontal) ******************/
&global-define xlHAlignCenter       -4108  /* 01 - Centralizado */
&global-define xlHAlignLeft         -4131  /* 04 - Esquerda */

/* Propriedade : LineStyle  (Estilo de Linha) **********************************/
&global-define xlContinuous     1       /* 01 - Continua */ 


{include/i-rpvar.i}
{cdp/cdcfgmat.i}
{utp/ut-glob.i}

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

ASSIGN c-programa = "ESNF013RP"
       c-versao   = "2.06"
       c-revisao  = "001".

{utp/ut-liter.i Relatorio_Monitor_XML * r}
ASSIGN c-titulo-relat = RETURN-VALUE.

{utp/ut-liter.i Monitor * r}
ASSIGN c-sistema = RETURN-VALUE.

{include/i-rpcab.i} 
{include/i-rpout.i}

VIEW FRAME f-cabec.
VIEW FRAME f-rodape.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Notas_Fiscais_Monitor *}

FILE-INFO:FILE-NAME = "modelo/esnf013.xls".
arquivo-at = FILE-INFO:FULL-PATHNAME.
arquivo-cp = SESSION:TEMP-DIRECTORY + "esnf013.xls".
OS-COMMAND SILENT COPY VALUE(arquivo-at) VALUE(arquivo-cp).
arquivo-nv = "modelo/esnf013.xls".
CREATE "excel.application" chexcelapplication.
chworkbook  = chexcelapplication:workbooks:ADD(arquivo-cp). /*substituir por "arquivo" */
chworksheet = chexcelapplication:sheets:ITEM(1).

RUN pi-inicializar IN h-acomp (INPUT  RETURN-VALUE ).

For Each nfe003 USE-INDEX ch-documento no-lock                                                     
    Where nfe003.nro-docto    >= tt-param.doc-ini                              
      And nfe003.nro-docto    <= tt-param.doc-fim                              
      And nfe003.serie-docto  >= tt-param.serie-ini                          
      And nfe003.serie-docto  <= tt-param.serie-fim                          
      And nfe003.cod-emitente >= tt-param.emit-ini                         
      And nfe003.cod-emitente <= tt-param.emit-fim                         
      And nfe003.dt-emissao   >= tt-param.data-emis-ini                       
      And nfe003.dt-emissao   <= tt-param.data-emis-fim                       
      and nfe003.idi-orig-trad = 2                                          
      break by nfe003.nro-docto:                                            
                                                                            
    RUN pi-acompanhar IN h-acomp (INPUT nfe003.nro-docto). 

    /*if tt-param.digit-receb     = no and nfe003.idi-situacao = 1 then next.
    if tt-param.nota-erro-neg   = no and nfe003.idi-situacao = 2 then next.
    if tt-param.atualiz-receb   = no and nfe003.idi-situacao = 3 then next.
    if tt-param.elimin-receb    = no and nfe003.idi-situacao = 4 then next.
    if tt-param.danfe-nao-autor = no and nfe003.idi-situacao = 5 then next.
    if tt-param.doc-liber       = no and nfe003.idi-situacao = 6 then next.*/
    
    find emitente                                                           
        where emitente.cod-emitente = nfe003.cod-emitente no-lock no-error. 
    if avail emitente then                                                  
        assign c-nome-abrev = emitente.nome-abrev.                            

    assign chworksheet:range("A" + STRING(iColumn)):VALUE = "Estab"
           chWorkSheet:range("A" + STRING(icolumn)):interior:colorindex = 06
           chWorkSheet:Range("A" + STRING(iColumn)):FONT:SIZE = 11                                                          
           chWorkSheet:Range("A" + STRING(iColumn)):FONT:Bold = TRUE                                                        
           chworksheet:range("A" + STRING(iColumn) + ":" + "A" + STRING(iColumn)):Borders(1):LineStyle = {&xlContinuous}    
           chworksheet:range("A" + STRING(iColumn) + ":" + "A" + STRING(iColumn)):Borders(2):LineStyle = {&xlContinuous}    
           chworksheet:range("A" + STRING(iColumn) + ":" + "A" + STRING(iColumn)):Borders(3):LineStyle = {&xlContinuous}    
           chworksheet:range("A" + STRING(iColumn) + ":" + "A" + STRING(iColumn)):Borders(4):LineStyle = {&xlContinuous}    
           chworksheet:range("B" + STRING(iColumn)):VALUE = "Documen"
           chWorkSheet:range("B" + STRING(iColumn)):interior:colorindex = 06
           chWorkSheet:Range("B" + STRING(iColumn)):FONT:SIZE = 11                                                          
           chWorkSheet:Range("B" + STRING(iColumn)):FONT:Bold = TRUE                                                        
           chworksheet:range("B" + STRING(iColumn)):Borders(1):LineStyle = {&xlContinuous}                                  
           chworksheet:range("B" + STRING(iColumn)):Borders(2):LineStyle = {&xlContinuous}                                  
           chworksheet:range("B" + STRING(iColumn)):Borders(3):LineStyle = {&xlContinuous}                                  
           chworksheet:range("B" + STRING(iColumn)):Borders(4):LineStyle = {&xlContinuous}                                  
           chworksheet:range("C" + STRING(iColumn)):VALUE = "S‚rie"
           chWorkSheet:range("C" + STRING(iColumn)):interior:colorindex = 06
           chWorkSheet:Range("C" + STRING(iColumn)):FONT:SIZE = 11                                                          
           chWorkSheet:Range("C" + STRING(iColumn)):FONT:Bold = TRUE                                                        
           chworksheet:range("C" + STRING(iColumn)):Borders(1):LineStyle = {&xlContinuous}                                  
           chworksheet:range("C" + STRING(iColumn)):Borders(2):LineStyle = {&xlContinuous}                                  
           chworksheet:range("C" + STRING(iColumn)):Borders(3):LineStyle = {&xlContinuous}                                  
           chworksheet:range("C" + STRING(iColumn)):Borders(4):LineStyle = {&xlContinuous}                                  
           chworksheet:range("D" + STRING(iColumn)):VALUE = "Emitente"
           chWorkSheet:range("D" + STRING(iColumn)):interior:colorindex = 06
           chWorkSheet:Range("D" + STRING(iColumn)):FONT:SIZE = 11                                                          
           chWorkSheet:Range("D" + STRING(iColumn)):FONT:Bold = TRUE                                                        
           chworksheet:range("D" + STRING(iColumn)):Borders(1):LineStyle = {&xlContinuous}                                  
           chworksheet:range("D" + STRING(iColumn)):Borders(2):LineStyle = {&xlContinuous}                                  
           chworksheet:range("D" + STRING(iColumn)):Borders(3):LineStyle = {&xlContinuous}                                  
           chworksheet:range("D" + STRING(iColumn)):Borders(4):LineStyle = {&xlContinuous}                                  
           chworksheet:range("E" + STRING(iColumn)):VALUE = "Nome Abrev"
           chWorkSheet:range("E" + STRING(iColumn)):interior:colorindex = 06
           chworksheet:Range("E" + STRING(iColumn) + ":" + "G" + STRING(iColumn)):MergeCells  = TRUE
           chWorkSheet:Range("E" + STRING(iColumn)):FONT:SIZE = 11                                                          
           chWorkSheet:Range("E" + STRING(iColumn)):FONT:Bold = TRUE                                                        
           chworksheet:range("E" + STRING(iColumn) + ":" + "G" + STRING(iColumn)):Borders(1):LineStyle = {&xlContinuous}                                  
           chworksheet:range("E" + STRING(iColumn) + ":" + "G" + STRING(iColumn)):Borders(2):LineStyle = {&xlContinuous}                                  
           chworksheet:range("E" + STRING(iColumn) + ":" + "G" + STRING(iColumn)):Borders(3):LineStyle = {&xlContinuous}                                  
           chworksheet:range("E" + STRING(iColumn) + ":" + "G" + STRING(iColumn)):Borders(4):LineStyle = {&xlContinuous}.                                  
    assign chworksheet:range("H" + STRING(iColumn)):VALUE = "Data emissÆo"
           chWorkSheet:range("H" + STRING(iColumn)):interior:colorindex = 06
           chworksheet:Range("H" + STRING(iColumn) + ":" + "I" + STRING(iColumn)):MergeCells  = TRUE
           chWorkSheet:Range("H" + STRING(iColumn)):FONT:SIZE = 11                                                          
           chWorkSheet:Range("H" + STRING(iColumn)):FONT:Bold = TRUE                                                        
           chworksheet:range("H" + STRING(iColumn) + ":" + "I" + STRING(iColumn)):Borders(1):LineStyle = {&xlContinuous}                                  
           chworksheet:range("H" + STRING(iColumn) + ":" + "I" + STRING(iColumn)):Borders(2):LineStyle = {&xlContinuous}                                 
           chworksheet:range("H" + STRING(iColumn) + ":" + "I" + STRING(iColumn)):Borders(3):LineStyle = {&xlContinuous}                                 
           chworksheet:range("H" + STRING(iColumn) + ":" + "I" + STRING(iColumn)):Borders(4):LineStyle = {&xlContinuous}
           chworksheet:range("J" + STRING(iColumn)):VALUE = "Data Transa‡Æo"
           chWorkSheet:range("J" + STRING(iColumn)):interior:colorindex = 06
           chworksheet:Range("J" + STRING(iColumn) + ":" + "K" + STRING(iColumn)):MergeCells  = TRUE 
           chWorkSheet:Range("J" + STRING(iColumn)):FONT:SIZE = 11                                   
           chWorkSheet:Range("J" + STRING(iColumn)):FONT:Bold = TRUE                                 
           chworksheet:range("J" + STRING(iColumn) + ":" + "K" + STRING(iColumn)):Borders(1):LineStyle = {&xlContinuous}           
           chworksheet:range("J" + STRING(iColumn) + ":" + "K" + STRING(iColumn)):Borders(2):LineStyle = {&xlContinuous}           
           chworksheet:range("J" + STRING(iColumn) + ":" + "K" + STRING(iColumn)):Borders(3):LineStyle = {&xlContinuous}           
           chworksheet:range("J" + STRING(iColumn) + ":" + "K" + STRING(iColumn)):Borders(4):LineStyle = {&xlContinuous}
           icolumn = icolumn + 1
           chworksheet:range("A" + STRING(iColumn)):VALUE = nfe003.cod-estabel
           chWorkSheet:Range("A" + STRING(iColumn)):HorizontalAlignment = {&xlHAlignleft}
           chWorkSheet:Range("A" + STRING(iColumn)):FONT:SIZE = 10                                                       
           chWorkSheet:Range("A" + STRING(iColumn)):FONT:Bold = false                                                     
           chworksheet:range("A" + STRING(iColumn) + ":" + "A" + STRING(iColumn)):Borders(1):LineStyle = {&xlContinuous} 
           chworksheet:range("A" + STRING(iColumn) + ":" + "A" + STRING(iColumn)):Borders(2):LineStyle = {&xlContinuous} 
           chworksheet:range("A" + STRING(iColumn) + ":" + "A" + STRING(iColumn)):Borders(3):LineStyle = {&xlContinuous} 
           chworksheet:range("A" + STRING(iColumn) + ":" + "A" + STRING(iColumn)):Borders(4):LineStyle = {&xlContinuous}. 
    assign chworksheet:range("B" + STRING(iColumn)):VALUE = nfe003.nro-docto
           chWorkSheet:Range("B" + STRING(iColumn)):HorizontalAlignment = {&xlHAlignleft}
           chWorkSheet:Range("B" + STRING(iColumn)):FONT:SIZE = 10                                                       
           chWorkSheet:Range("B" + STRING(iColumn)):FONT:Bold = false                                                     
           chworksheet:range("B" + STRING(iColumn)):Borders(1):LineStyle = {&xlContinuous}                               
           chworksheet:range("B" + STRING(iColumn)):Borders(2):LineStyle = {&xlContinuous}                               
           chworksheet:range("B" + STRING(iColumn)):Borders(3):LineStyle = {&xlContinuous}                               
           chworksheet:range("B" + STRING(iColumn)):Borders(4):LineStyle = {&xlContinuous}                               
           chworksheet:range("C" + STRING(iColumn)):VALUE = nfe003.serie-docto
           chWorkSheet:Range("C" + STRING(iColumn)):HorizontalAlignment = {&xlHAlignleft}
           chWorkSheet:Range("C" + STRING(iColumn)):FONT:SIZE = 10                                                       
           chWorkSheet:Range("C" + STRING(iColumn)):FONT:Bold = false                                                     
           chworksheet:range("C" + STRING(iColumn)):Borders(1):LineStyle = {&xlContinuous}                               
           chworksheet:range("C" + STRING(iColumn)):Borders(2):LineStyle = {&xlContinuous}                               
           chworksheet:range("C" + STRING(iColumn)):Borders(3):LineStyle = {&xlContinuous}                               
           chworksheet:range("C" + STRING(iColumn)):Borders(4):LineStyle = {&xlContinuous}                               
           chworksheet:range("D" + STRING(iColumn)):VALUE = nfe003.cod-emitente 
           chWorkSheet:Range("D" + STRING(iColumn)):HorizontalAlignment = {&xlHAlignleft}
           chWorkSheet:Range("D" + STRING(iColumn)):FONT:SIZE = 10                                                       
           chWorkSheet:Range("D" + STRING(iColumn)):FONT:Bold = false                                                     
           chworksheet:range("D" + STRING(iColumn)):Borders(1):LineStyle = {&xlContinuous}                               
           chworksheet:range("D" + STRING(iColumn)):Borders(2):LineStyle = {&xlContinuous}                               
           chworksheet:range("D" + STRING(iColumn)):Borders(3):LineStyle = {&xlContinuous}                               
           chworksheet:range("D" + STRING(iColumn)):Borders(4):LineStyle = {&xlContinuous}                               
           chworksheet:range("E" + STRING(iColumn)):VALUE = c-nome-abrev
           chworksheet:Range("E" + STRING(iColumn) + ":" + "G" + STRING(iColumn)):MergeCells  = TRUE                     
           chWorkSheet:Range("E" + STRING(iColumn)):FONT:SIZE = 10                                                                                     
           chWorkSheet:Range("E" + STRING(iColumn)):FONT:Bold = false                                                                                   
           chworksheet:range("E" + STRING(iColumn) + ":" + "G" + STRING(iColumn)):Borders(1):LineStyle = {&xlContinuous}                               
           chworksheet:range("E" + STRING(iColumn) + ":" + "G" + STRING(iColumn)):Borders(2):LineStyle = {&xlContinuous}                               
           chworksheet:range("E" + STRING(iColumn) + ":" + "G" + STRING(iColumn)):Borders(3):LineStyle = {&xlContinuous}                               
           chworksheet:range("E" + STRING(iColumn) + ":" + "G" + STRING(iColumn)):Borders(4):LineStyle = {&xlContinuous}.                               
    assign chworksheet:range("H" + STRING(iColumn)):VALUE = nfe003.dt-emissao                                               
           chworksheet:Range("H" + STRING(iColumn) + ":" + "I" + STRING(iColumn)):MergeCells  = TRUE
           chWorkSheet:Range("H" + STRING(iColumn)):HorizontalAlignment = {&xlHAlignleft}
           chWorkSheet:Range("H" + STRING(iColumn)):FONT:SIZE = 10                                                       
           chWorkSheet:Range("H" + STRING(iColumn)):FONT:Bold = false                                                     
           chworksheet:range("H" + STRING(iColumn) + ":" + "I" + STRING(iColumn)):Borders(1):LineStyle = {&xlContinuous} 
           chworksheet:range("H" + STRING(iColumn) + ":" + "I" + STRING(iColumn)):Borders(2):LineStyle = {&xlContinuous} 
           chworksheet:range("H" + STRING(iColumn) + ":" + "I" + STRING(iColumn)):Borders(3):LineStyle = {&xlContinuous} 
           chworksheet:range("H" + STRING(iColumn) + ":" + "I" + STRING(iColumn)):Borders(4):LineStyle = {&xlContinuous} 
           chworksheet:range("J" + STRING(iColumn)):VALUE = nfe003.dt-transacao                                             
           chworksheet:Range("J" + STRING(iColumn) + ":" + "K" + STRING(iColumn)):MergeCells  = TRUE
           chWorkSheet:Range("J" + STRING(iColumn)):HorizontalAlignment = {&xlHAlignleft}
           chWorkSheet:Range("J" + STRING(iColumn)):FONT:SIZE = 10                                                       
           chWorkSheet:Range("J" + STRING(iColumn)):FONT:Bold = false                                                     
           chworksheet:range("J" + STRING(iColumn) + ":" + "K" + STRING(iColumn)):Borders(1):LineStyle = {&xlContinuous} 
           chworksheet:range("J" + STRING(iColumn) + ":" + "K" + STRING(iColumn)):Borders(2):LineStyle = {&xlContinuous} 
           chworksheet:range("J" + STRING(iColumn) + ":" + "K" + STRING(iColumn)):Borders(3):LineStyle = {&xlContinuous}                                                                       
           chworksheet:range("J" + STRING(iColumn) + ":" + "K" + STRING(iColumn)):Borders(4):LineStyle = {&xlContinuous}
           icolumn = icolumn + 2.

    for each nfe013 USE-INDEX ch-chave no-lock
        where nfe013.ch-acesso-comp-nfe = nfe003.ch-acesso-comp-nfe 
          and nfe013.idi-orig-trad =  2  
           break by nfe003.ch-acesso-comp-nfe
                 BY nfe013.it-codigo:
        
       if first-of(nfe003.ch-acesso-comp-nfe) AND first-of(nfe013.it-codigo) then do: 

            assign chworksheet:range("A" + STRING(iColumn)):VALUE = "Seq"                                                     
                   chWorkSheet:Range("A" + STRING(iColumn)):FONT:SIZE = 11                                                      
                   chWorkSheet:Range("A" + STRING(iColumn)):FONT:Bold = TRUE                                                    
                   chworksheet:range("A" + STRING(iColumn) + ":" + "A" + STRING(iColumn)):Borders(1):LineStyle = {&xlContinuous}
                   chworksheet:range("A" + STRING(iColumn) + ":" + "A" + STRING(iColumn)):Borders(2):LineStyle = {&xlContinuous}
                   chworksheet:range("A" + STRING(iColumn) + ":" + "A" + STRING(iColumn)):Borders(3):LineStyle = {&xlContinuous}
                   chworksheet:range("A" + STRING(iColumn) + ":" + "A" + STRING(iColumn)):Borders(4):LineStyle = {&xlContinuous}
                   chworksheet:range("B" + STRING(iColumn)):VALUE = "Item" 
                   chworksheet:Range("B" + STRING(iColumn) + ":" + "C" + STRING(iColumn)):MergeCells  = TRUE
                   chWorkSheet:Range("B" + STRING(iColumn)):FONT:SIZE = 11                                                      
                   chWorkSheet:Range("B" + STRING(iColumn)):FONT:Bold = TRUE                                                    
                   chworksheet:range("B" + STRING(iColumn) + ":" + "C" + STRING(iColumn)):Borders(1):LineStyle = {&xlContinuous}                              
                   chworksheet:range("B" + STRING(iColumn) + ":" + "C" + STRING(iColumn)):Borders(2):LineStyle = {&xlContinuous}                              
                   chworksheet:range("B" + STRING(iColumn) + ":" + "C" + STRING(iColumn)):Borders(3):LineStyle = {&xlContinuous}                              
                   chworksheet:range("B" + STRING(iColumn) + ":" + "C" + STRING(iColumn)):Borders(4):LineStyle = {&xlContinuous}                              
                   chworksheet:range("D" + STRING(iColumn)):VALUE = "Descri‡Æo"
                   chworksheet:Range("D" + STRING(iColumn) + ":" + "F" + STRING(iColumn)):MergeCells  = TRUE
                   chWorkSheet:Range("D" + STRING(iColumn)):FONT:SIZE = 11                                                      
                   chWorkSheet:Range("D" + STRING(iColumn)):FONT:Bold = TRUE                                                    
                   chworksheet:range("D" + STRING(iColumn) + ":" + "F" + STRING(iColumn)):Borders(1):LineStyle = {&xlContinuous}                              
                   chworksheet:range("D" + STRING(iColumn) + ":" + "F" + STRING(iColumn)):Borders(2):LineStyle = {&xlContinuous}                              
                   chworksheet:range("D" + STRING(iColumn) + ":" + "F" + STRING(iColumn)):Borders(3):LineStyle = {&xlContinuous}                              
                   chworksheet:range("D" + STRING(iColumn) + ":" + "F" + STRING(iColumn)):Borders(4):LineStyle = {&xlContinuous}                              
                   chworksheet:range("G" + STRING(iColumn)):VALUE = "Qtd Solic"                                                  
                   chWorkSheet:Range("G" + STRING(iColumn)):FONT:SIZE = 11                                                      
                   chWorkSheet:Range("G" + STRING(iColumn)):FONT:Bold = TRUE                                                    
                   chworksheet:range("G" + STRING(iColumn)):Borders(1):LineStyle = {&xlContinuous}                              
                   chworksheet:range("G" + STRING(iColumn)):Borders(2):LineStyle = {&xlContinuous}                              
                   chworksheet:range("G" + STRING(iColumn)):Borders(3):LineStyle = {&xlContinuous}                              
                   chworksheet:range("G" + STRING(iColumn)):Borders(4):LineStyle = {&xlContinuous}                              
                   chworksheet:range("H" + STRING(iColumn)):VALUE = "Pre‡o Unitario"                                                
                   chworksheet:Range("H" + STRING(iColumn) + ":" + "I" + STRING(iColumn)):MergeCells  = TRUE                                                 
                   chWorkSheet:Range("H" + STRING(iColumn)):FONT:SIZE = 11                                                                                   
                   chWorkSheet:Range("H" + STRING(iColumn)):FONT:Bold = TRUE                                                                                 
                   chworksheet:range("H" + STRING(iColumn) + ":" + "I" + STRING(iColumn)):Borders(1):LineStyle = {&xlContinuous}                             
                   chworksheet:range("H" + STRING(iColumn) + ":" + "I" + STRING(iColumn)):Borders(2):LineStyle = {&xlContinuous}                             
                   chworksheet:range("H" + STRING(iColumn) + ":" + "I" + STRING(iColumn)):Borders(3):LineStyle = {&xlContinuous}                             
                   chworksheet:range("H" + STRING(iColumn) + ":" + "I" + STRING(iColumn)):Borders(4):LineStyle = {&xlContinuous}.
            assign chworksheet:range("J" + STRING(iColumn)):VALUE = "Pre‡o Total" 
                   chworksheet:Range("J" + STRING(iColumn) + ":" + "K" + STRING(iColumn)):MergeCells  = TRUE
                   chWorkSheet:Range("J" + STRING(iColumn)):FONT:SIZE = 11                                                      
                   chWorkSheet:Range("J" + STRING(iColumn)):FONT:Bold = TRUE                                                    
                   chworksheet:range("J" + STRING(iColumn) + ":" + "K" + STRING(iColumn)):Borders(1):LineStyle = {&xlContinuous}
                   chworksheet:range("J" + STRING(iColumn) + ":" + "K" + STRING(iColumn)):Borders(2):LineStyle = {&xlContinuous}
                   chworksheet:range("J" + STRING(iColumn) + ":" + "K" + STRING(iColumn)):Borders(3):LineStyle = {&xlContinuous}
                   chworksheet:range("J" + STRING(iColumn) + ":" + "K" + STRING(iColumn)):Borders(4):LineStyle = {&xlContinuous}
                   chworksheet:range("L" + STRING(iColumn)):VALUE = "Ped Compra"                                            
                   chworksheet:Range("L" + STRING(iColumn) + ":" + "M" + STRING(iColumn)):MergeCells  = TRUE                    
                   chWorkSheet:Range("L" + STRING(iColumn)):FONT:SIZE = 11                                                      
                   chWorkSheet:Range("L" + STRING(iColumn)):FONT:Bold = TRUE                                                    
                   chworksheet:range("L" + STRING(iColumn) + ":" + "M" + STRING(iColumn)):Borders(1):LineStyle = {&xlContinuous}
                   chworksheet:range("L" + STRING(iColumn) + ":" + "M" + STRING(iColumn)):Borders(2):LineStyle = {&xlContinuous}
                   chworksheet:range("L" + STRING(iColumn) + ":" + "M" + STRING(iColumn)):Borders(3):LineStyle = {&xlContinuous}
                   chworksheet:range("L" + STRING(iColumn) + ":" + "M" + STRING(iColumn)):Borders(4):LineStyle = {&xlContinuous}
                   chworksheet:range("N" + STRING(iColumn)):VALUE = "Ord Compra"                                                 
                   chworksheet:Range("N" + STRING(iColumn) + ":" + "O" + STRING(iColumn)):MergeCells  = TRUE                     
                   chWorkSheet:Range("N" + STRING(iColumn)):FONT:SIZE = 11                                                       
                   chWorkSheet:Range("N" + STRING(iColumn)):FONT:Bold = TRUE                                                     
                   chworksheet:range("N" + STRING(iColumn) + ":" + "O" + STRING(iColumn)):Borders(1):LineStyle = {&xlContinuous} 
                   chworksheet:range("N" + STRING(iColumn) + ":" + "O" + STRING(iColumn)):Borders(2):LineStyle = {&xlContinuous} 
                   chworksheet:range("N" + STRING(iColumn) + ":" + "O" + STRING(iColumn)):Borders(3):LineStyle = {&xlContinuous} 
                   chworksheet:range("N" + STRING(iColumn) + ":" + "O" + STRING(iColumn)):Borders(4):LineStyle = {&xlContinuous}
                   icolumn = icolumn + 1. 
       end.

       assign chworksheet:range("A" + STRING(iColumn)):VALUE = nfe013.seq-item 
              chWorkSheet:Range("A" + STRING(iColumn)):HorizontalAlignment = {&xlHAlignleft}
              chWorkSheet:Range("A" + STRING(iColumn)):FONT:SIZE = 10                                                      
              chWorkSheet:Range("A" + STRING(iColumn)):FONT:Bold = false                                                    
              chworksheet:range("A" + STRING(iColumn) + ":" + "A" + STRING(iColumn)):Borders(1):LineStyle = {&xlContinuous}
              chworksheet:range("A" + STRING(iColumn) + ":" + "A" + STRING(iColumn)):Borders(2):LineStyle = {&xlContinuous}
              chworksheet:range("A" + STRING(iColumn) + ":" + "A" + STRING(iColumn)):Borders(3):LineStyle = {&xlContinuous}
              chworksheet:range("A" + STRING(iColumn) + ":" + "A" + STRING(iColumn)):Borders(4):LineStyle = {&xlContinuous}
              chworksheet:range("B" + STRING(iColumn)):VALUE = nfe013.it-codigo
              chWorkSheet:Range("B" + STRING(iColumn)):HorizontalAlignment = {&xlHAlignleft}
              chworksheet:Range("B" + STRING(iColumn) + ":" + "C" + STRING(iColumn)):MergeCells  = TRUE                    
              chWorkSheet:Range("B" + STRING(iColumn)):FONT:SIZE = 10                                                                                   
              chWorkSheet:Range("B" + STRING(iColumn)):FONT:Bold = false                                                                                 
              chworksheet:range("B" + STRING(iColumn) + ":" + "C" + STRING(iColumn)):Borders(1):LineStyle = {&xlContinuous}                             
              chworksheet:range("B" + STRING(iColumn) + ":" + "C" + STRING(iColumn)):Borders(2):LineStyle = {&xlContinuous}                             
              chworksheet:range("B" + STRING(iColumn) + ":" + "C" + STRING(iColumn)):Borders(3):LineStyle = {&xlContinuous}                             
              chworksheet:range("B" + STRING(iColumn) + ":" + "C" + STRING(iColumn)):Borders(4):LineStyle = {&xlContinuous}                             
              chworksheet:range("D" + STRING(iColumn)):VALUE = nfe013.desc-item
              chWorkSheet:Range("D" + STRING(iColumn)):HorizontalAlignment = {&xlHAlignleft}
              chworksheet:Range("D" + STRING(iColumn) + ":" + "F" + STRING(iColumn)):MergeCells  = TRUE                    
              chWorkSheet:Range("D" + STRING(iColumn)):FONT:SIZE = 10                                                                                    
              chWorkSheet:Range("D" + STRING(iColumn)):FONT:Bold = false                                                                                  
              chworksheet:range("D" + STRING(iColumn) + ":" + "F" + STRING(iColumn)):Borders(1):LineStyle = {&xlContinuous}                              
              chworksheet:range("D" + STRING(iColumn) + ":" + "F" + STRING(iColumn)):Borders(2):LineStyle = {&xlContinuous}                              
              chworksheet:range("D" + STRING(iColumn) + ":" + "F" + STRING(iColumn)):Borders(3):LineStyle = {&xlContinuous}                              
              chworksheet:range("D" + STRING(iColumn) + ":" + "F" + STRING(iColumn)):Borders(4):LineStyle = {&xlContinuous}                              
              chworksheet:range("G" + STRING(iColumn)):VALUE = nfe013.qtd-comercial
              chworksheet:range("G" + STRING(iColumn)):numberformat = "#.##0,00"
              chWorkSheet:Range("G" + STRING(iColumn)):HorizontalAlignment = {&xlHAlignleft}
              chWorkSheet:Range("G" + STRING(iColumn)):FONT:SIZE = 10                         
              chWorkSheet:Range("G" + STRING(iColumn)):FONT:Bold = false                                                     
              chworksheet:range("G" + STRING(iColumn)):Borders(1):LineStyle = {&xlContinuous}                               
              chworksheet:range("G" + STRING(iColumn)):Borders(2):LineStyle = {&xlContinuous}                               
              chworksheet:range("G" + STRING(iColumn)):Borders(3):LineStyle = {&xlContinuous}                               
              chworksheet:range("G" + STRING(iColumn)):Borders(4):LineStyle = {&xlContinuous}.                   
       assign chworksheet:range("H" + STRING(iColumn)):VALUE = nfe013.preco-unit
              chworksheet:range("H" + STRING(iColumn)):numberformat = "R$" + "#.###.###,99"
              chWorkSheet:Range("H" + STRING(iColumn)):HorizontalAlignment = {&xlHAlignleft}
              chworksheet:Range("H" + STRING(iColumn) + ":" + "I" + STRING(iColumn)):MergeCells  = TRUE                      
              chWorkSheet:Range("H" + STRING(iColumn)):FONT:SIZE = 10                                                                                      
              chWorkSheet:Range("H" + STRING(iColumn)):FONT:Bold = false                                                                                   
              chworksheet:range("H" + STRING(iColumn) + ":" + "I" + STRING(iColumn)):Borders(1):LineStyle = {&xlContinuous}                               
              chworksheet:range("H" + STRING(iColumn) + ":" + "I" + STRING(iColumn)):Borders(2):LineStyle = {&xlContinuous}                               
              chworksheet:range("H" + STRING(iColumn) + ":" + "I" + STRING(iColumn)):Borders(3):LineStyle = {&xlContinuous}                              
              chworksheet:range("H" + STRING(iColumn) + ":" + "I" + STRING(iColumn)):Borders(4):LineStyle = {&xlContinuous}
              chworksheet:range("J" + STRING(iColumn)):VALUE = nfe013.preco-total
              chworksheet:range("J" + STRING(iColumn)):numberformat = "R$" + "#.###.###,99"
              chWorkSheet:Range("J" + STRING(iColumn)):HorizontalAlignment = {&xlHAlignleft}
              chworksheet:Range("J" + STRING(iColumn) + ":" + "K" + STRING(iColumn)):MergeCells  = TRUE                     
              chWorkSheet:Range("J" + STRING(iColumn)):FONT:SIZE = 10                                                       
              chWorkSheet:Range("J" + STRING(iColumn)):FONT:Bold = false                                                      
              chworksheet:range("J" + STRING(iColumn) + ":" + "K" + STRING(iColumn)):Borders(1):LineStyle = {&xlContinuous} 
              chworksheet:range("J" + STRING(iColumn) + ":" + "K" + STRING(iColumn)):Borders(2):LineStyle = {&xlContinuous} 
              chworksheet:range("J" + STRING(iColumn) + ":" + "K" + STRING(iColumn)):Borders(3):LineStyle = {&xlContinuous} 
              chworksheet:range("J" + STRING(iColumn) + ":" + "K" + STRING(iColumn)):Borders(4):LineStyle = {&xlContinuous} 
              chworksheet:range("L" + STRING(iColumn)):VALUE = nfe013.num-pedido
              chWorkSheet:Range("L" + STRING(iColumn)):HorizontalAlignment = {&xlHAlignleft}
              chworksheet:Range("L" + STRING(iColumn) + ":" + "M" + STRING(iColumn)):MergeCells  = TRUE                    
              chWorkSheet:Range("L" + STRING(iColumn)):FONT:SIZE = 10                                                      
              chWorkSheet:Range("L" + STRING(iColumn)):FONT:Bold = false                                                     
              chworksheet:range("L" + STRING(iColumn) + ":" + "M" + STRING(iColumn)):Borders(1):LineStyle = {&xlContinuous}
              chworksheet:range("L" + STRING(iColumn) + ":" + "M" + STRING(iColumn)):Borders(2):LineStyle = {&xlContinuous}
              chworksheet:range("L" + STRING(iColumn) + ":" + "M" + STRING(iColumn)):Borders(3):LineStyle = {&xlContinuous}
              chworksheet:range("L" + STRING(iColumn) + ":" + "M" + STRING(iColumn)):Borders(4):LineStyle = {&xlContinuous}
              chworksheet:range("N" + STRING(iColumn)):VALUE = nfe013.numero-ordem
              chWorkSheet:Range("N" + STRING(iColumn)):HorizontalAlignment = {&xlHAlignleft}
              chworksheet:Range("N" + STRING(iColumn) + ":" + "O" + STRING(iColumn)):MergeCells  = TRUE                    
              chWorkSheet:Range("N" + STRING(iColumn)):FONT:SIZE = 10                                                      
              chWorkSheet:Range("N" + STRING(iColumn)):FONT:Bold = false                                                     
              chworksheet:range("N" + STRING(iColumn) + ":" + "O" + STRING(iColumn)):Borders(1):LineStyle = {&xlContinuous}
              chworksheet:range("N" + STRING(iColumn) + ":" + "O" + STRING(iColumn)):Borders(2):LineStyle = {&xlContinuous}
              chworksheet:range("N" + STRING(iColumn) + ":" + "O" + STRING(iColumn)):Borders(3):LineStyle = {&xlContinuous}
              chworksheet:range("N" + STRING(iColumn) + ":" + "O" + STRING(iColumn)):Borders(4):LineStyle = {&xlContinuous}
              icolumn = icolumn + 1.
    
    end.
    icolumn = icolumn + 1.
End.

RUN pi-finalizar IN h-acomp.         

chExcelApplication:VISIBLE = TRUE.
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet.




RETURN "OK":U. 

