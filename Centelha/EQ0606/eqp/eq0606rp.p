/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i EQ0606RP 2.06.00.001}  /*** 010032 ***/
/******************************************************************************
**  Programa: EQ0606RP.P
**  Data....: 26/08/1992
**  Autor...: DATASUL S.A.
**  Objetivo: Separa‡Æo por Entregas
**  Custom..: Sottelli - 01/01/2014 
******************************************************************************/
{eqp/eq0605.i3}
{cdp/cdcfgdis.i}

&if '{&bf_dis_versao_ems}' >= '2.04' &then
{include/tt-edit.i} /** para impressao da narrativa do item **/
{include/pi-edit.i} /** procedure de impressao da narrativa **/
&endif

def temp-table tt-param
    field destino          as integer
    field arquivo          as char
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field c-cli-fim        as char format "x(12)"
    field c-cli-ini        as char format "x(12)"
    field de-embarque-fim  as DEC
    field c-preparador     as char format "x(12)"
    field de-embarque-ini  as DEC
    field l-imp-embal      as logical format "Sim/NÆo"
    field l-nome-abrev     as logical format "Abreviado/Completo"
    field i-nr-resumo-ini  like pre-fatur.nr-resumo
    field i-nr-resumo-fim  like pre-fatur.nr-resumo
    field l-modo           as logical format "Sim/N’o"
    field i-narrativa      as integer format "9"
    field l-imp-narrat     as logical format "Sim/N’o"
    .
    
{UTP/UT-GLOB.I}
{INCLUDE/I-RPVAR.I}
    
def temp-table tt-raw-digita 
   field raw-digita as raw.
   
def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.


&IF DEFINED(BF_DIS_VERSAO_EMS) &THEN
  &IF {&BF_DIS_VERSAO_EMS} >= 2.02 &THEN
&GLOBAL-DEFINE COD-REFER       cod-refer
  &ENDIF
  &ELSE
&GLOBAL-DEFINE COD-REFER       char-1
  &ENDIF

create tt-param.
raw-transfer raw-param to tt-param.

def temp-table tt-item
    field tt-nome-abrev   like pre-fatur.nome-abrev 
    field tt-nr-pedcli    like pre-fatur.nr-pedcli 
    field tt-nr-resumo    like pre-fatur.nr-resumo 
    field tt-cdd-embarq   like pre-fatur.cdd-embarq
    field tt-cod-refer    like ped-item.cod-refer
    FIELD tt-nr-sequencia LIKE ped-item.nr-sequencia
    field tt-deposito     as character format "x(03)"
    field tt-un           like item.un 
    field tt-it-codigo    like it-dep-fat.it-codigo
    field tt-it-descricao like item.desc-item
    field tt-localizacao  like it-dep-fat.cod-localiz
    field tt-qt-alocada   like it-pre-fat.qt-alocada
    field tt-nr-serie     like it-dep-fat.nr-serlot
    field tt-sigla-emb    like it-pre-emb.sigla-emb.
    
def temp-table tt-embarque    
   field cdd-embarq       like pre-fatur.cdd-embarq
   field nr-resumo        like pre-fatur.nr-resumo
   field nr-pedcli        like pre-fatur.nr-pedcli
   field nome-abrev       like pre-fatur.nome-abrev
   field cod-entrega      like pre-fatur.cod-entrega
   field endereco         like loc-entr.endereco
   field bairro           like loc-entr.bairro
   field cidade           like loc-entr.cidade
   field estado           like loc-entr.estado
   field pais             like loc-entr.pais
   field cep              like loc-entr.cep
   field caixa-postal     like loc-entr.caixa-postal
   field cgc              like loc-entr.cgc
   field ins-estadual     like loc-entr.ins-estadual 
   field tip-entrega      as char
   field observacao       like ped-venda.observacoes
   field dados-adic       as char
   field transporte       like pre-fatur.nome-transp
   field placa            like pre-fatur.placa
   field cod-emitente     like emitente.cod-emitente 
   field tp-pedido        like ped-venda.tp-pedido
   field deposito         like it-dep-fat.cod-depos
   FIELD cod-cond-pag     LIKE cond-pagto.cod-cond-pag
   FIELD des-cond-pag     LIKE cond-pagto.descricao
   FIELD cod-estabel      LIKE estabelec.cod-estabel
   FIELD nome-estabel     LIKE estabelec.nome
   FIELD dt-hr            AS CHAR
   FIELD nome-ab-rep      LIKE ped-repre.nome-ab-rep
   FIELD vl-tot-ped       LIKE ped-venda.vl-tot-ped.
 
def buffer b-pre-fatur   for pre-fatur.
def buffer b-pre-fatur-2 for pre-fatur.
def buffer tt-item-aloc for tt-item.

def new shared var c-it-codigo-x like tt-item.tt-it-codigo.
def new shared var i-tot-vol       as integer format ">>,>>>,>>>,>99" no-undo.

def var d-qt-alocada like it-pre-fat.qt-alocada   no-undo.
def var c-pre-ini    like pre-fatur.identific     no-undo.
def var c-pre-fim    like pre-fatur.identific     no-undo.
def var i-embala-ini like it-pre-emb.nr-embalagem no-undo.
def var i-embala-fim like it-pre-emb.nr-embalagem no-undo.
def var i-quantidade like it-pre-emb.nr-embalagem no-undo.

def var l-imp-nota          as logical   init no             no-undo.
def var l-impr-pedcli       as logical                       no-undo.
def var l-erro-x            as logical                       no-undo.
def var l-ft1009x           as logical                       no-undo. 
def var l-controle          as logical                       no-undo.
def var l-imprimiu          as logical                       no-undo.
def var l-it-dep-fat        as logical                       no-undo.
def var i-cont2             as int                           no-undo.
def var i-cont3             as int                           no-undo.
def var i-cont              as int  format ">>9"             no-undo.
def var valorLinha          as decimal                       no-undo.

def var i-tot-item          as int  format ">>,>>>,>>>,>>9"  no-undo.
def var i-total             as int  format ">>,>>>,>>>,>>9"  no-undo.
def var c-opcao             as char                          no-undo.
def var c-barra             as char format "x(3)" init " - " no-undo.
def var c-data              as char format "x(9)"            no-undo.
def var c-hora              as char format "x(9)"            no-undo.
def var c-usu               as char format "x(10)"           no-undo.
def var c-sequencia         as char format "x(12)"           no-undo.
def var c-ref               as char format "x(14)"           no-undo.
def var c-lote              as char format "x(10)"           no-undo.
def var c-ser               as char format "x(16)"           no-undo.
def var c-resumo            as char format "x(35)"           no-undo.
def var c-tot               as char format "x(7)"            no-undo.
def var c-llocal            as char format "x(10)"           no-undo.
def var c-ldep              as char format "x(4)"            no-undo.
def var c-litem             as char format "x(5)"            no-undo.
def var c-ldesc             as char format "x(12)"           no-undo.
def var c-lun               as char format "x(3)"            no-undo.
def var c-laloc             as char format "x(13)"           no-undo.
def var c-lsepar            as char format "x(8)"            no-undo.
def var c-lped              as char format "x(8)"            no-undo.
def var c-lemit             as char format "x(6)"            no-undo.
def var c-ltip              as char format "x(5)"            no-undo.
def var c-serie             as char format "x(7)"            no-undo.
def var c-nota              as char format "x(6)"            no-undo. 
def var c-destino-impressao as char format "x(15)"           no-undo.
def var c-selecao           as char format "x(10)"           no-undo.
def var c-param             as char format "x(14)"           no-undo.
def var c-imp               as char format "x(15)"           no-undo.
def var c-destino           as char format "x(1)"            no-undo.
def var c-usuario           as char format "x(1)"            no-undo.
def var c-des               as char extent 4                 no-undo.
def var h-acomp             as handle                        no-undo.
def var c-qt-aloc           like tt-item.tt-qt-alocada       no-undo.
DEF VAR l-tip-entrega       AS CHAR FORMAT "x(08)"           NO-UNDO.
&if '{&bf_dis_versao_ems}' >= '2.04' &then
def var c-narrativa         as char format  "x(15)"          no-undo.
def var c-narrat            like item.narrativa              no-undo.
&endif

DEF VAR c-emergencial AS CHAR NO-UNDO.

EMPTY TEMP-TABLE tt-embarque.
EMPTY TEMP-TABLE tt-item.


function roundUp returns integer ( x as decimal ):

  if x = truncate( x, 0 ) then
    return integer( x ).
   else
    return integer( truncate( x, 0 ) + 1 ).

end.
/********************************DEFINICAO DE VARIAVEIS ********************************/
DEFINE VARIABLE I-LINHA         AS INTEGER   INITIAL 0  NO-UNDO.

DEFINE VARIABLE C-ARQ-EXCEL  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE C-ANEXOS     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE CHEXCEL      AS COM-HANDLE  NO-UNDO.
DEFINE VARIABLE CHWBOOK      AS COM-HANDLE  NO-UNDO.
DEFINE VARIABLE CHWSHEET     AS COM-HANDLE  NO-UNDO.

{INCLUDE/I-RPOUT.I}. 
RUN UTP/UT-ACOMP.P PERSISTENT SET H-ACOMP.
RUN PI-INICIALIZAR IN H-ACOMP (INPUT RETURN-VALUE).
FIND FIRST TT-PARAM NO-LOCK NO-ERROR.
 

for each tt-item:
    delete tt-item.
end.

assign c-pre-ini = c-preparador
       c-pre-fim = if c-preparador <> "" then c-preparador
                   else "ZZZZZZZZZZZZ".

&if '{&bf_dis_versao_ems}' >= '2.04' &then
if l-imp-narrat then do:
    if  tt-param.i-narrativa = 1 then
        assign c-narrativa = "Item".
    else
       if tt-param.i-narrativa = 2 then
          assign c-narrativa = "Pedido".
       else
          if tt-param.i-narrativa = 3 then
             assign c-narrativa = "Embarque".
end.
else
   assign c-narrativa = "NÆo".
&endif



for each embarque NO-LOCK use-index ch-emb where
         embarque.cdd-embarq >= de-embarque-ini and
         embarque.cdd-embarq <= de-embarque-fim: 
    for each res-cli no-lock use-index ch-resumo where
             res-cli.cdd-embarq  = embarque.cdd-embarq and
             res-cli.nr-resumo  >= i-nr-resumo-ini      and
             res-cli.nr-resumo  <= i-nr-resumo-fim :
        for each pre-fatur EXCLUSIVE-LOCK use-index ch-embarque where
                 pre-fatur.cdd-embarq  = embarque.cdd-embarq  and
                 pre-fatur.nr-resumo    = res-cli.nr-resumo     and
                 pre-fatur.identific   >= c-pre-ini             and
                 pre-fatur.identific   <= c-pre-fim             and
                 pre-fatur.nome-abrev  >= c-cli-ini             and
                 pre-fatur.nome-abrev  <= c-cli-fim             and
                 pre-fatur.nr-pedcli   <> ""
              break by pre-fatur.cdd-embarq
                    by pre-fatur.nr-resumo
                    by pre-fatur.nr-pedcli:


         RUN PI-ACOMPANHAR IN H-ACOMP (INPUT 'Embarque... ' + STRING(embarque.cdd-embarq) ).

         if pre-fatur.pick-impresso = tt-param.l-modo then next. 
         IF NOT pre-fatur.pick-impresso THEN
             pre-fatur.pick-impresso = YES.
         assign l-imprimiu = yes.
         
         find emitente where
              emitente.nome-abrev = pre-fatur.nome-abrev no-lock no-error.
         IF NOT AVAIL emitente THEN
             NEXT.
    
         find ped-venda use-index ch-pedido where
              ped-venda.nr-pedcli  = pre-fatur.nr-pedcli and
              ped-venda.nome-abrev = emitente.nome-abrev no-lock no-error.
         IF AVAIL ped-venda THEN DO:
             IF ped-venda.cidade-cif <> " " THEN
                ASSIGN l-tip-entrega = "Entrega".
            ELSE
                ASSIGN l-tip-entrega = "Retirada".
         END.
             
         find first tt-pre-fatur where
                    tt-pre-fatur.nr-resumo  = pre-fatur.nr-resumo and
                    tt-pre-fatur.cdd-embarq = pre-fatur.cdd-embarq no-error.
         if not avail tt-pre-fatur then do:
            create tt-pre-fatur.
            assign tt-pre-fatur.cdd-embarq = pre-fatur.cdd-embarq
                   tt-pre-fatur.nr-resumo   = pre-fatur.nr-resumo.
         end.
             
             
         if first-of(pre-fatur.nr-resumo) then do: 
         
            find first transporte
                 where transporte.nome-abrev = ped-venda.nome-tr-red
            no-lock no-error.
                 
            find loc-entr where
                 loc-entr.nome-abrev  = pre-fatur.nome-abrev and
                 loc-entr.cod-entrega = pre-fatur.cod-entrega no-lock no-error. 

            FIND FIRST cond-pagto NO-LOCK
                 WHERE cond-pagto.cod-cond-pag = ped-venda.cod-cond-pag NO-ERROR.
                        
            FIND FIRST estabelec NO-LOCK 
                 WHERE estabelec.cod-estabel = pre-fatur.cod-estabel NO-ERROR.            

            FIND FIRST ped-repre OF ped-venda WHERE nome-ab-rep <> "CENTELHA" NO-LOCK NO-ERROR.   

            FIND FIRST ext-embarque
                 WHERE ext-embarque.cod-estabel = embarque.cod-estabel 
                   AND ext-embarque.cdd-embarq  = embarque.cdd-embarq NO-ERROR.   

             create tt-embarque. 
             assign tt-embarque.cdd-embarq       = pre-fatur.cdd-embarq
                    tt-embarque.nr-resumo        = pre-fatur.nr-resumo
                    tt-embarque.nr-pedcli        = pre-fatur.nr-pedcli
                    tt-embarque.nome-abrev       = if tt-param.l-nome-abrev then pre-fatur.nome-abrev else emitente.nome-emit
                    tt-embarque.cod-entrega      = pre-fatur.cod-entrega
                    tt-embarque.endereco         = loc-entr.endereco
                    tt-embarque.bairro           = loc-entr.bairro
                    tt-embarque.cidade           = loc-entr.cidade
                    tt-embarque.estado           = loc-entr.estado
                    tt-embarque.pais             = loc-entr.pais
                    tt-embarque.cep              = loc-entr.cep
                    tt-embarque.caixa-postal     = loc-entr.caixa-postal
                    tt-embarque.cgc              = loc-entr.cgc
                    tt-embarque.ins-estadual     = loc-entr.ins-estadual 
                    tt-embarque.tip-entrega      = l-tip-entrega
                    tt-embarque.observacao       = ped-venda.observacoes 
                    tt-embarque.dados-adic       = if avail transporte then string(transporte.cod-trans) + " - " + transporte.nome else ""                   
                    tt-embarque.transporte       = pre-fatur.nome-transp
                    tt-embarque.placa            = pre-fatur.placa
                    tt-embarque.cod-emitente     = emitente.cod-emitente
                    tt-embarque.tp-pedido        = ped-venda.tp-pedido
                    tt-embarque.cod-cond-pag     = IF AVAIL cond-pagto THEN cond-pagto.cod-cond-pag ELSE ''
                    tt-embarque.des-cond-pag     = IF AVAIL cond-pagto THEN cond-pagto.descricao    ELSE ''
                    tt-embarque.cod-estabel      = estabelec.cod-estabel    
                    tt-embarque.nome-estabel     = estabelec.nome           
                    tt-embarque.dt-hr            = STRING(embarque.dt-embarque) + " "   + IF AVAIL ext-embarque THEN 
                                                   STRING(INTEGER(truncate(MTIME(hr-embarque)/ 1000,0)),"HH:MM:SS") ELSE ""
                    tt-embarque.nome-ab-rep      = IF AVAIL ped-repre THEN ped-repre.nome-ab-rep ELSE "CENTELHA".
                    tt-embarque.vl-tot-ped       = ped-venda.vl-tot-ped.    

         end.
                                   
            for each it-pre-fat of pre-fatur no-lock
                break by it-pre-fat.nr-pedcli
                      by it-pre-fat.nome-abrev
                      by it-pre-fat.it-codigo:
                      
        
                assign l-it-dep-fat = no.
        
                for each it-dep-fat where
                         it-dep-fat.cdd-embarq   = it-pre-fat.cdd-embarq  and
                         it-dep-fat.nr-resumo    = it-pre-fat.nr-resumo    and
                         it-dep-fat.nome-abrev   = it-pre-fat.nome-abrev   and
                         it-dep-fat.nr-pedcli    = it-pre-fat.nr-pedcli    and
                         it-dep-fat.nr-sequencia = it-pre-fat.nr-sequencia and                         
                         it-dep-fat.nr-entrega   = it-pre-fat.nr-entrega no-lock
                         break by it-dep-fat.nr-resumo:
                         
                    find first item no-lock where item.it-codigo = it-dep-fat.it-codigo no-error.

                    if first-of(it-dep-fat.nr-resumo) then do:
                        if avail tt-embarque then
                        assign tt-embarque.deposito =  it-dep-fat.cod-depos.
                    end.

                    find first tt-item where
                               tt-item.tt-nome-abrev   = pre-fatur.nome-abrev and
                               tt-item.tt-nr-pedcli    = pre-fatur.nr-pedcli and
                               tt-item.tt-nr-resumo    = pre-fatur.nr-resumo and
                               tt-item.tt-cdd-embarq   = pre-fatur.cdd-embarq and
                               tt-item.tt-nr-sequencia = it-dep-fat.nr-sequencia                AND
                               tt-item.tt-it-codigo    = it-dep-fat.it-codigo                   and
                               tt-item.tt-cod-refer    = it-dep-fat.{&cod-refer}                and
                               tt-item.tt-nr-pedcli    = it-dep-fat.nr-pedcli                   and
                               tt-item.tt-deposito     = it-dep-fat.cod-depos                   and
                               tt-item.tt-localizacao  = substring(it-dep-fat.cod-localiz,1,10) and
                               tt-item.tt-nr-serie     = it-dep-fat.nr-serlot no-error.
                    assign l-it-dep-fat = yes.
                    
                    if not avail tt-item then do:
                       create tt-item.
                       assign  tt-item.tt-nome-abrev   = pre-fatur.nome-abrev 
                               tt-item.tt-nr-pedcli    = pre-fatur.nr-pedcli 
                               tt-item.tt-nr-resumo    = pre-fatur.nr-resumo 
                               tt-item.tt-cdd-embarq   = pre-fatur.cdd-embarq 
                               tt-item.tt-nr-sequencia  = it-dep-fat.nr-sequencia
                               tt-item.tt-it-codigo     = it-dep-fat.it-codigo
                               tt-item.tt-un            = item.un
                               tt-item.tt-it-descricao  = item.desc-item
                               tt-item.tt-cod-refer     = it-dep-fat.{&cod-refer}
                               tt-item.tt-nr-pedcli     = it-dep-fat.nr-pedcli
                               tt-item.tt-deposito      = it-dep-fat.cod-depos
                               tt-item.tt-localizacao   = substring(it-dep-fat.cod-localiz,1,10)
                               tt-item.tt-qt-alocada    = it-dep-fat.qt-alocada
                               tt-item.tt-nr-serie      = it-dep-fat.nr-serlot.

                    end.
                    else
                       assign tt-item.tt-qt-alocada = tt-item.tt-qt-alocada + it-dep-fat.qt-alocada.
                end.
                
                if  not l-it-dep-fat then do:
                    find first tt-item where
                               tt-item.tt-nome-abrev   = pre-fatur.nome-abrev and
                               tt-item.tt-nr-pedcli    = pre-fatur.nr-pedcli and
                               tt-item.tt-nr-resumo    = pre-fatur.nr-resumo and
                               tt-item.tt-cdd-embarq   = pre-fatur.cdd-embarq and
                               tt-item.tt-nr-sequencia = it-pre-fat.nr-sequencia AND
                               tt-item.tt-it-codigo    = it-pre-fat.it-codigo    and
                               tt-item.tt-cod-refer    = it-pre-fat.{&cod-refer} and
                               tt-item.tt-nr-pedcli    = it-pre-fat.nr-pedcli    and
                               tt-item.tt-deposito     = "   " +  it-pre-fat.it-codigo no-error.
                    if not avail tt-item then do:
                       create tt-item.
                       assign tt-item.tt-nome-abrev    = pre-fatur.nome-abrev 
                              tt-item.tt-nr-pedcli     = pre-fatur.nr-pedcli 
                              tt-item.tt-nr-resumo     = pre-fatur.nr-resumo 
                              tt-item.tt-cdd-embarq    = pre-fatur.cdd-embarq 
                              tt-item.tt-nr-sequencia  = it-pre-fat.nr-sequencia
                              tt-item.tt-it-codigo     = it-pre-fat.it-codigo
                              tt-item.tt-un            = item.un
                              tt-item.tt-it-descricao  = item.desc-item
                              tt-item.tt-cod-refer     = it-pre-fat.{&cod-refer}
                              tt-item.tt-nr-pedcli     = it-pre-fat.nr-pedcli
                              tt-item.tt-qt-alocada    = it-pre-fat.qt-alocada
                              tt-item.tt-deposito      = "   " + it-pre-fat.it-codigo.

                    end.
                    else assign tt-item.tt-qt-alocada = tt-item.tt-qt-alocada + it-pre-fat.qt-alocada.
                end.           
           end.
       end.
   end.
end.

  
/*--------------------------------IMPRESSAO-------------------------------------------*/
CREATE 'EXCEL.APPLICATION' CHEXCEL.
CHWBOOK = CHEXCEL:WORKBOOKS:ADD().
CHWSHEET = CHEXCEL:SHEETS:ITEM(1).

assign i-linha = 1.
  
FOR EACH tt-embarque BREAK BY tt-embarque.cdd-embarq:


    RUN PI-ACOMPANHAR IN H-ACOMP (INPUT 'Imprimindo Embarque... ' + STRING(tt-embarque.cdd-embarq) ).
  
    /*IF FIRST-OF(tt-embarque.cdd-embarq) THEN DO:*/
       /* ------------------------- Cabe‡alho Embarque -------------------------  */
       CHEXCEL:RANGE("A" + STRING(i-linha) + ":H" + STRING(i-linha)):MERGE.
       CHEXCEL:RANGE("A" + STRING(i-linha) + ":H" + STRING(i-linha)):FONT:BOLD = TRUE.
       CHWSHEET:RANGE("A" + STRING(i-linha) + ":H" + STRING(i-linha)) = "Informa‡äes do Pedido: " + CAPS(tt-embarque.observacao) .
  
       ASSIGN i-linha = i-linha + 1.
  
       CHEXCEL:RANGE("A" + STRING(i-linha) + ":A" + STRING(i-linha + 2)):MERGE.
       CHEXCEL:RANGE("A" + STRING(i-linha)):WrapText = TRUE.
       CHEXCEL:range("A" + STRING(i-linha)):VerticalAlignment = 2.
       CHEXCEL:RANGE("A" + STRING(i-linha)):FONT:BOLD = TRUE.
       CHWSHEET:RANGE("A" + STRING(i-linha)) = "Dados Adicionais:".
  
       CHEXCEL:RANGE("B" + STRING(i-linha) + ":H" + STRING(i-linha + 2)):MERGE.
       CHEXCEL:range("B" + STRING(i-linha)):VerticalAlignment = 2.
       CHWSHEET:RANGE("B" + STRING(i-linha) + ":H" + STRING(i-linha + 2)) = tt-embarque.dados-adic.
  
       CHEXCEL:Range("A4:H4"):Borders(4):Weight = 2.
       
       ASSIGN i-linha = i-linha + 3.  
    /*END.*/
    /* -------------------------- Cabe‡alho Resumo  --------------------------  */
       
    CHEXCEL:RANGE("A" + STRING(i-linha)):FONT:BOLD = TRUE.
    CHWSHEET:RANGE("A" + STRING(i-linha)) = "Embarque:".
    CHEXCEL:RANGE("B" + STRING(i-linha)):HorizontalAlignment = 2.
    CHWSHEET:RANGE("B" + STRING(i-linha)) = STRING(tt-embarque.cdd-embarq).
  
    CHEXCEL:RANGE("C" + STRING(i-linha)):FONT:BOLD = TRUE.
    CHWSHEET:RANGE("C" + STRING(i-linha)) = "Nr Resumo:".
    CHEXCEL:RANGE("D" + STRING(i-linha)):HorizontalAlignment = 2.
    CHWSHEET:RANGE("D" + STRING(i-linha)) = tt-embarque.nr-resumo.
  
    CHEXCEL:RANGE("E" + STRING(i-linha)):FONT:BOLD = TRUE.
    CHWSHEET:RANGE("E" + STRING(i-linha)) = "Transp.:".
    CHEXCEL:RANGE("F" + STRING(i-linha)):ShrinkToFit = TRUE.
    CHEXCEL:RANGE("F" + STRING(i-linha)):HorizontalAlignment = 2.
    CHWSHEET:RANGE("F" + STRING(i-linha)) = tt-embarque.transporte.
  
    CHEXCEL:RANGE("G" + STRING(i-linha)):FONT:BOLD = TRUE.
    CHWSHEET:RANGE("G" + STRING(i-linha)) = "Placa:".
    CHEXCEL:RANGE("H" + STRING(i-linha)):HorizontalAlignment = 2.
    CHWSHEET:RANGE("H" + STRING(i-linha)) = tt-embarque.placa.
  
    ASSIGN i-linha = i-linha + 1.

    CHEXCEL:RANGE("A" + STRING(i-linha)):FONT:BOLD = TRUE.
    CHWSHEET:RANGE("A" + STRING(i-linha)) = "Estabelec:".
    CHEXCEL:RANGE("B" + STRING(i-linha) + ":D" + STRING(i-linha)):MERGE.
    CHEXCEL:RANGE("B" + STRING(i-linha)):ShrinkToFit = TRUE.
    CHEXCEL:RANGE("B" + STRING(i-linha)):HorizontalAlignment = 2.
    CHWSHEET:RANGE("B" + STRING(i-linha)) = tt-embarque.cod-estabel + "   " + tt-embarque.nome-estabel.
    
    CHEXCEL:RANGE("E" + STRING(i-linha)):FONT:BOLD = TRUE.
    CHEXCEL:RANGE("E" + STRING(i-linha)):ShrinkToFit = TRUE.
    CHWSHEET:RANGE("E" + STRING(i-linha)) = "Data/Hora:".
    CHEXCEL:RANGE("F" + STRING(i-linha) + ":H" + STRING(i-linha)):MERGE.
    CHEXCEL:RANGE("F" + STRING(i-linha)):ShrinkToFit = TRUE.
    CHEXCEL:RANGE("F" + STRING(i-linha)):HorizontalAlignment = 2.
    CHWSHEET:RANGE("F" + STRING(i-linha)) = tt-embarque.dt-hr.

    ASSIGN i-linha = i-linha + 1.

    CHEXCEL:RANGE("A" + STRING(i-linha)):FONT:BOLD = TRUE.  
    CHWSHEET:RANGE("A" + STRING(i-linha)) = "Vendedor:".
    CHEXCEL:RANGE("B" + STRING(i-linha) + ":D" + STRING(i-linha)):MERGE.
    CHEXCEL:RANGE("B" + STRING(i-linha)):ShrinkToFit = TRUE.
    CHEXCEL:RANGE("B" + STRING(i-linha)):HorizontalAlignment = 2.
    CHWSHEET:RANGE("B" + STRING(i-linha)) = tt-embarque.nome-ab-rep.

    
    CHEXCEL:RANGE("E" + STRING(i-linha)):FONT:BOLD = TRUE.
    CHWSHEET:RANGE("E" + STRING(i-linha)) = "Tipo Entrega:".
    CHEXCEL:RANGE("F" + STRING(i-linha) + ":H" + STRING(i-linha)):MERGE.
    CHEXCEL:RANGE("F" + STRING(i-linha)):HorizontalAlignment = 2.
    CHWSHEET:RANGE("F" + STRING(i-linha)) = tt-embarque.tip-entrega.
    
    ASSIGN i-linha = i-linha + 2.
  
    CHEXCEL:RANGE("A" + STRING(i-linha)):FONT:BOLD = TRUE.
    CHWSHEET:RANGE("A" + STRING(i-linha)) = "Cliente:".
    CHEXCEL:RANGE("B" + STRING(i-linha) + ":C" + STRING(i-linha)):MERGE.
    CHEXCEL:RANGE("B" + STRING(i-linha)):HorizontalAlignment = 2.
    CHWSHEET:RANGE("B" + STRING(i-linha)) = tt-embarque.nome-abrev.
  
    ASSIGN i-linha = i-linha + 1.
  
    CHEXCEL:RANGE("A" + STRING(i-linha)):FONT:BOLD = TRUE.
    CHWSHEET:RANGE("A" + STRING(i-linha)) = "Endere‡o:".
    CHEXCEL:RANGE("B" + STRING(i-linha) + ":C" + STRING(i-linha)):MERGE.
    CHEXCEL:RANGE("B" + STRING(i-linha)):HorizontalAlignment = 2.
    CHWSHEET:RANGE("B" + STRING(i-linha)) = tt-embarque.endereco.
  
    ASSIGN i-linha = i-linha + 1.
  
    CHEXCEL:RANGE("A" + STRING(i-linha)):FONT:BOLD = TRUE.
    CHWSHEET:RANGE("A" + STRING(i-linha)) = "CEP:".
    CHEXCEL:RANGE("B" + STRING(i-linha) + ":C" + STRING(i-linha)):MERGE.
    CHEXCEL:RANGE("B" + STRING(i-linha)):HorizontalAlignment = 2.
    CHWSHEET:RANGE("B" + STRING(i-linha)) = tt-embarque.cep.
  
    ASSIGN i-linha = i-linha + 1. /* 10 */
  
    CHEXCEL:RANGE("A" + STRING(i-linha)):FONT:BOLD = TRUE.
    CHWSHEET:RANGE("A" + STRING(i-linha)) = "CGCMF:".
    CHEXCEL:RANGE("B" + STRING(i-linha) + ":C" + STRING(i-linha)):MERGE.
    CHEXCEL:RANGE("B" + STRING(i-linha)):HorizontalAlignment = 2.
    CHWSHEET:RANGE("B" + STRING(i-linha)):NUMBERFORMAT = "0.00".
    CHWSHEET:RANGE("B" + STRING(i-linha)) = STRING(tt-embarque.cgc).
  
    ASSIGN i-linha = i-linha - 3. /* 7 */
  
    CHEXCEL:RANGE("E" + STRING(i-linha)):FONT:BOLD = TRUE.
    CHWSHEET:RANGE("E" + STRING(i-linha)) = "Bairro:".
    CHEXCEL:RANGE("F" + STRING(i-linha) + ":H" + STRING(i-linha)):MERGE.
    CHEXCEL:RANGE("E" + STRING(i-linha)):ShrinkToFit = TRUE.
    CHEXCEL:RANGE("F" + STRING(i-linha)):HorizontalAlignment = 2.
    CHWSHEET:RANGE("F" + STRING(i-linha)) = tt-embarque.bairro.
  
    ASSIGN i-linha = i-linha + 1. /* 8 */
  
    CHEXCEL:RANGE("E" + STRING(i-linha)):FONT:BOLD = TRUE.
    CHWSHEET:RANGE("E" + STRING(i-linha)) = "Cidade:".
    CHEXCEL:RANGE("F" + STRING(i-linha) + ":H" + STRING(i-linha)):MERGE.
    CHEXCEL:RANGE("E" + STRING(i-linha)):ShrinkToFit = TRUE.
    CHEXCEL:RANGE("F" + STRING(i-linha)):HorizontalAlignment = 2.
    CHWSHEET:RANGE("F" + STRING(i-linha)) = tt-embarque.cidade.
  
    ASSIGN i-linha = i-linha + 1.

    CHEXCEL:RANGE("E" + STRING(i-linha)):FONT:BOLD = TRUE.
    CHWSHEET:RANGE("E" + STRING(i-linha)) = "UF:".
    CHEXCEL:RANGE("F" + STRING(i-linha)):HorizontalAlignment = 2.
    CHWSHEET:RANGE("F" + STRING(i-linha)) = tt-embarque.estado.
  
    ASSIGN i-linha = i-linha + 1.
  
    CHEXCEL:RANGE("E" + STRING(i-linha)):FONT:BOLD = TRUE.
    CHWSHEET:RANGE("E" + STRING(i-linha)) = "Cx. Postal:".
    CHWSHEET:RANGE("F" + STRING(i-linha)) = tt-embarque.caixa-postal.
  
    ASSIGN i-linha = i-linha + 1.
  
    CHEXCEL:RANGE("E" + STRING(i-linha)):FONT:BOLD = TRUE.
    CHWSHEET:RANGE("E" + STRING(i-linha)) = "Insc. Estad.:".
    CHWSHEET:RANGE("F" + STRING(i-linha)):NUMBERFORMAT = "0.00".
    CHWSHEET:RANGE("F" + STRING(i-linha)) = STRING(tt-embarque.ins-estadual).
  
  
    /* ------------------------- Cabe‡alho Pedido -------------------------  */
    ASSIGN i-linha = i-linha + 2. /* 14 */
    CHEXCEL:RANGE("A" + STRING(i-linha) + ":H" + STRING(i-linha)):FONT:BOLD = TRUE.
    CHEXCEL:RANGE("A" + STRING(i-linha) + ":H" + STRING(i-linha)):Borders(4):Weight = 2.
  
    CHWSHEET:RANGE("A" + STRING(i-linha)) = "Pedido".
    CHWSHEET:RANGE("B" + STRING(i-linha)) = "Dep.".
    CHWSHEET:RANGE("C" + STRING(i-linha)) = "Emitente".
    CHWSHEET:RANGE("D" + STRING(i-linha)) = "Tipo".
    CHWSHEET:RANGE("E" + STRING(i-linha)) = "Cond. Pag.".
    CHEXCEL:RANGE("F" + STRING(i-linha) + ":G" + STRING(i-linha)):MERGE.
    CHWSHEET:RANGE("F" + STRING(i-linha)) = "Descri‡Æo".
  
    ASSIGN i-linha = i-linha + 1.
  
    CHWSHEET:RANGE("A" + STRING(i-linha)) = tt-embarque.nr-pedcli.
    CHWSHEET:RANGE("B" + STRING(i-linha)) = tt-embarque.deposito.
    CHWSHEET:RANGE("C" + STRING(i-linha)) = tt-embarque.cod-emitente.
    CHWSHEET:RANGE("D" + STRING(i-linha)) = tt-embarque.tp-pedido.
    CHWSHEET:RANGE("E" + STRING(i-linha)) = tt-embarque.cod-cond-pag.
    CHEXCEL:RANGE("F" + STRING(i-linha) + ":G" + STRING(i-linha)):MERGE.
    CHWSHEET:RANGE("F" + STRING(i-linha)) = tt-embarque.des-cond-pag.
  
    /* ------------------------- Cabe?alho Itens -------------------------  */
    ASSIGN i-linha = i-linha + 2. /*17*/
    CHEXCEL:RANGE("A" + STRING(i-linha) + ":H" + STRING(i-linha)):FONT:BOLD = TRUE.
    CHEXCEL:Range("A" + STRING(i-linha) + ":H" + STRING(i-linha)):Borders(4):Weight = 2.
  
    CHEXCEL:RANGE("B" + STRING(i-linha) + ":C" + STRING(i-linha)):MERGE.
    CHEXCEL:RANGE("B" + STRING(i-linha)):HorizontalAlignment = 2.

    CHWSHEET:RANGE("A" + STRING(i-linha)) = "Item ".
    CHWSHEET:RANGE("B" + STRING(i-linha)) = "Descri‡Æo".
    CHWSHEET:RANGE("D" + STRING(i-linha)) = "UN".
    CHWSHEET:RANGE("E" + STRING(i-linha)) = "Localiza‡Æo".
    CHWSHEET:RANGE("F" + STRING(i-linha)) = "Dep¢sito".
    CHWSHEET:RANGE("G" + STRING(i-linha)) = "Qt. Alocada".
    CHWSHEET:RANGE("H" + STRING(i-linha)) = "Qt. Separ".
  
    ASSIGN i-linha = i-linha + 1. /* 18 */
  
    FOR EACH tt-item
         WHERE tt-item.tt-nr-pedcli  = tt-embarque.nr-pedcli
         AND tt-item.tt-nr-resumo  = tt-embarque.nr-resumo
         AND tt-item.tt-cdd-embarq = tt-embarque.cdd-embarq
           BY tt-item.tt-localizacao:
        
            CHEXCEL:RANGE("B" + STRING(i-linha) + ":C" + STRING(i-linha)):MERGE.
            CHEXCEL:RANGE("B" + STRING(i-linha)):HorizontalAlignment = 2.

            CHWSHEET:RANGE("A" + STRING(i-linha)) = STRING(tt-item.tt-it-codigo).
            CHEXCEL:RANGE("B" + STRING(i-linha)):ShrinkToFit = TRUE.

            IF tt-item.tt-nr-serie <> "" THEN
                CHWSHEET:RANGE("B" + STRING(i-linha)) = tt-item.tt-it-descricao
                   + " - " + tt-item.tt-nr-serie.
            ELSE
                CHWSHEET:RANGE("B" + STRING(i-linha)) = tt-item.tt-it-descricao.

            CHWSHEET:RANGE("D" + STRING(i-linha)) = tt-item.tt-un .
            CHWSHEET:RANGE("E" + STRING(i-linha)) = tt-localizacao.
            CHWSHEET:RANGE("F" + STRING(i-linha)) = tt-deposito.
            CHWSHEET:RANGE("G" + STRING(i-linha)) = tt-item.tt-qt-alocada.
            CHWSHEET:RANGE("H" + STRING(i-linha)) = "".
  
            ASSIGN i-linha = i-linha + 1.
  
    END.
  
    ASSIGN i-linha = i-linha + 1.

    /* Tratamento de quebra de pagina, 1 pedido por folha */
    ASSIGN valorLinha = i-linha / 50.

    DO i-cont = 1  TO (roundUp(valorLinha) * 50):
        ASSIGN i-cont = i-cont + 1.
    END.

    ASSIGN i-linha = + i-cont - 13.

    REPEAT:
        IF CHWSHEET:RANGE("A" + STRING(i-linha)):VALUE = ? THEN LEAVE.            
        ELSE i-linha = i-cont + 13.
    END.


    CHEXCEL:RANGE("A" + STRING(i-linha) + ":F" + STRING(i-linha)):FONT:BOLD = TRUE.
    CHEXCEL:Range("B" + STRING(i-linha) + ":F" + STRING(i-linha)):Borders(4):Weight = 2.
  
    CHWSHEET:RANGE("B" + STRING(i-linha)) = "Sigla ".
    CHWSHEET:RANGE("C" + STRING(i-linha)) = "Descri‡Æo".
    CHWSHEET:RANGE("F" + STRING(i-linha)) = "Qt. Volumes".
  
    ASSIGN i-linha = i-linha + 1.
  
    CHWSHEET:RANGE("B" + STRING(i-linha)) = "".
    CHWSHEET:RANGE("C" + STRING(i-linha)) = "".
    CHWSHEET:RANGE("F" + STRING(i-linha)) = "".

    
    ASSIGN i-linha = i-linha + 2.  

    CHEXCEL:RANGE("A" + STRING(i-linha) + ":F" + STRING(i-linha)):FONT:BOLD = TRUE.
    CHEXCEL:Range("D" + STRING(i-linha) + ":F" + STRING(i-linha)):Borders(4):Weight = 2.
    CHEXCEL:RANGE("D" + STRING(i-linha) + ":E" + STRING(i-linha)):MERGE.
    CHEXCEL:RANGE("D" + STRING(i-linha)):HorizontalAlignment = 2.

    CHWSHEET:RANGE("D" + STRING(i-linha)) = "Volume".
    CHWSHEET:RANGE("F" + STRING(i-linha)) = "Peso".
   
    ASSIGN i-linha = i-linha + 1.

    CHEXCEL:RANGE("D" + STRING(i-linha) + ":E" + STRING(i-linha)):MERGE.
    CHEXCEL:RANGE("B" + STRING(i-linha)):HorizontalAlignment = 2.
  
    CHWSHEET:RANGE("D" + STRING(i-linha)) = "".
    CHWSHEET:RANGE("F" + STRING(i-linha)) = "".


    ASSIGN i-linha = i-linha + 2.

    CHEXCEL:Range("D" + STRING(i-linha) + ":F" + STRING(i-linha)):Borders(4):Weight = 2.
    CHEXCEL:RANGE("D" + STRING(i-linha) + ":F" + STRING(i-linha)):FONT:BOLD = TRUE.
    CHEXCEL:RANGE("D" + STRING(i-linha) + ":F" + STRING(i-linha)):MERGE.
    CHEXCEL:RANGE("D" + STRING(i-linha)):HorizontalAlignment = 2.
    CHWSHEET:RANGE("D" + STRING(i-linha)) = "Separador".

    ASSIGN i-linha = i-linha + 1.

    CHEXCEL:RANGE("D" + STRING(i-linha) + ":F" + STRING(i-linha)):MERGE.
    CHWSHEET:RANGE("D" + STRING(i-linha)) = "".


    ASSIGN i-linha = i-linha + 2.

    CHEXCEL:Range("D" + STRING(i-linha) + ":F" + STRING(i-linha)):Borders(4):Weight = 2.
    CHEXCEL:RANGE("D" + STRING(i-linha) + ":F" + STRING(i-linha)):FONT:BOLD = TRUE.
    CHEXCEL:RANGE("D" + STRING(i-linha) + ":F" + STRING(i-linha)):MERGE.
    CHEXCEL:RANGE("D" + STRING(i-linha)):HorizontalAlignment = 2.
    CHWSHEET:RANGE("D" + STRING(i-linha)) = "Nome do Conferente".

    ASSIGN i-linha = i-linha + 1.

    CHEXCEL:RANGE("D" + STRING(i-linha) + ":F" + STRING(i-linha)):MERGE.
    CHWSHEET:RANGE("D" + STRING(i-linha)) = "".

    ASSIGN i-linha = i-linha + 2.

    CHEXCEL:RANGE("F" + STRING(i-linha)):FONT:BOLD = TRUE.       
    CHWSHEET:RANGE("F" + STRING(i-linha)) = "Valor Total:".

    CHEXCEL:RANGE("G" + STRING(i-linha) + ":H" + STRING(i-linha)):MERGE.
    CHWSHEET:RANGE("G" + STRING(i-linha)) = "R$ " + (STRING(tt-embarque.vl-tot-ped)).

    ASSIGN i-linha = i-linha + 1.

    CHWSHEET:HPageBreaks:Add(CHWSHEET:Range("A" + STRING(i-linha))).
END.
  
CHEXCEL:RANGE("A:U"):FONT:SIZE = 9.
CHWSHEET:COLUMNS("A:U"):ENTIRECOLUMN:AUTOFIT.
CHEXCEL:COLUMNS("A"):ColumnWidth = 9.71.
CHEXCEL:COLUMNS("C"):ColumnWidth = 34.
CHEXCEL:COLUMNS("E"):ColumnWidth = 8.86.
CHEXCEL:COLUMNS("F"):ColumnWidth = 13.29.
/* CHWSHEET:PageSetup:PaperSize = 9. */
CHWSHEET:PageSetup:HeaderMargin = 0.  
CHWSHEET:PageSetup:FooterMargin = 0.   
CHWSHEET:VPageBreaks:Add(CHWSHEET:Range("I1")).
CHWSHEET:PageSetup:LeftMargin   = 30. /* Margem esquerda */
CHWSHEET:PageSetup:RightMargin  = 30. /* Margem direita */
CHWSHEET:PageSetup:TopMargin    = 30. /* Margem superior */ 
CHWSHEET:PageSetup:BottomMargin = 30. /* Margem inferior */  
/* CHWSHEET:PageSetup:PrintArea = "$A$1:$H$" + STRING(i-linha - 1). */

RUN PI-FINALIZAR IN H-ACOMP.
  
CHEXCEL:VISIBLE = TRUE.
  
IF VALID-HANDLE(CHWSHEET) THEN
    RELEASE OBJECT CHWSHEET.
IF VALID-HANDLE(CHWBOOK) THEN
    RELEASE OBJECT CHWBOOK.
IF VALID-HANDLE(CHEXCEL) THEN
    RELEASE OBJECT CHEXCEL.
