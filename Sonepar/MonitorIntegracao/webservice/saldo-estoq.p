/*************************************************
** Programa   :webservice/saldo-estoq.p         **
** Data       :Dezembro/2017                    **
** Autor      :Joao Pacheco - SZ                **
** Descricao  :Integra‡Æo WSxCRM - Saldo-Estoq  **
**                                              **
** Alteracao  :                                 **
**                                              **
**                                              **
**************************************************/
{utp/ut-glob.i}

function fc-retira-ptvirg returns character
  ( c-desc as character ) :
    
    def var c-temp as char no-undo.

    c-temp = replace(c-desc,";"," ").
    c-temp = replace(c-temp,chr(10)," ").
    c-temp = replace(c-temp,chr(13)," ").

    return c-temp.   /* Function return value. */

end function.

def input param p-row-saldo as rowid no-undo.
def input param p-tp-trans  as int   no-undo.

def var hWebService         as handle   no-undo.
def var hSoap               as handle   no-undo.
def var i-trans             as int      no-undo.
def var de-prevista-entrada as dec      no-undo.
def var de-pendente         as dec      no-undo.
def var lcXMLReturn         as longchar no-undo.

find saldo-estoq no-lock where
     rowid(saldo-estoq) = p-row-saldo no-error.
if avail saldo-estoq then do:

    find item no-lock where 
         item.it-codigo = saldo-estoq.it-codigo no-error.

    find item-uni-estab no-lock where 
         item-uni-estab.it-codigo   = saldo-estoq.it-codigo   and 
         item-uni-estab.cod-estabel = saldo-estoq.cod-estabel no-error.

    assign de-prevista-entrada = 0
           de-pendente         = 0.

    if item-uni-estab.deposito-pad = saldo-estoq.cod-depos   and 
       item-uni-estab.cod-localiz  = saldo-estoq.cod-localiz then do:

        for each ordem-compra no-lock use-index estab-item-sit where 
                 ordem-compra.cod-estabel = saldo-estoq.cod-estabel and 
                 ordem-compra.it-codigo   = saldo-estoq.it-codigo   and 
                (ordem-compra.situacao    = 2),
            each prazo-compra of ordem-compra no-lock where 
                 prazo-compra.quant-saldo > 0:

            assign de-prevista-entrada = de-prevista-entrada + prazo-compra.quant-saldo.
            
        end.

        for each ped-item no-lock use-index planejamento where 
                 ped-item.it-codigo    = saldo-estoq.it-codigo and 
                (ped-item.cod-sit-item = 1 or
                 ped-item.cod-sit-item = 2):
            
            find ped-venda no-lock where
                 ped-venda.nome-abrev = ped-item.nome-abrev and
                 ped-venda.nr-pedcli  = ped-item.nr-pedcli  no-error.

            if ped-venda.cod-estabel <> saldo-estoq.cod-estabel then
                next.

            assign de-pendente = de-pendente + (ped-item.qt-pedida - ped-item.qt-atendida).

        end.

        assign de-pendente = de-pendente - saldo-estoq.qt-alocada - saldo-estoq.qt-aloc-ped.
        
        if de-pendente < 0 then
            assign de-pendente = 0.

    end.    

    do while true:
        assign i-trans = next-value(seq-saldo-estoq-crm).
        
        find crm-web-processados no-lock where
             crm-web-processados.nome-tabela   = 'saldo-estoq' and
             crm-web-processados.num-transacao = i-trans       no-error.
        if not avail crm-web-processados then
            leave.
    end.
    
    create crm-web-processados.
    assign crm-web-processados.num-transacao = i-trans
           crm-web-processados.dt-trans      = today               
           crm-web-processados.hr-trans      = replace(string(time,"HH:MM:SS"),":","")           
           crm-web-processados.tipo-trans    = p-tp-trans
           crm-web-processados.nome-tabela   = 'saldo-estoq'      
           crm-web-processados.chave-tabela  = (saldo-estoq.cod-estabel + ';' + saldo-estoq.cod-depos + ';' + saldo-estoq.cod-localiz + ';' +
                                                saldo-estoq.lote + ';' + saldo-estoq.it-codigo + ';' + saldo-estoq.cod-refer). 

    find first param-integra no-lock no-error.

    create server hWebService.
    hWebService:connect("-WSDL '" + param-integra.web-wsdl + "/Etoque/xml/method?wsdl'").

    run EstoquePort set hSoap on hWebService.

    case p-tp-trans:
        when 1 then do:
            run insert in hSoap (input param-integra.web-user,
                                 input param-integra.web-pswd,
                                 input crm-web-processados.num-transacao,
                                 input i-ep-codigo-usuario,
                                 input fc-retira-ptvirg(saldo-estoq.it-codigo),
                                 input fc-retira-ptvirg(saldo-estoq.cod-estabel),
                                 input fc-retira-ptvirg(saldo-estoq.cod-depos),
                                 input fc-retira-ptvirg(saldo-estoq.lote),
                                 input fc-retira-ptvirg(saldo-estoq.cod-localiz),
                                 input fc-retira-ptvirg(saldo-estoq.cod-refer),
                                 input saldo-estoq.dt-vali-lote,
                                 input item.un,
                                 input saldo-estoq.qtidade-atu,
                                 input saldo-estoq.qt-alocada,
                                 input de-prevista-entrada,
                                 input de-pendente,
                                 input saldo-estoq.qt-aloc-ped,
                                 output lcXMLReturn).
        end.
        when 2 then do:
            run update in hSoap (input param-integra.web-user,
                                 input param-integra.web-pswd,
                                 input crm-web-processados.num-transacao,
                                 input i-ep-codigo-usuario,
                                 input fc-retira-ptvirg(saldo-estoq.it-codigo),
                                 input fc-retira-ptvirg(saldo-estoq.cod-estabel),
                                 input fc-retira-ptvirg(saldo-estoq.cod-depos),
                                 input fc-retira-ptvirg(saldo-estoq.lote),
                                 input fc-retira-ptvirg(saldo-estoq.cod-localiz),
                                 input fc-retira-ptvirg(saldo-estoq.cod-refer),
                                 input saldo-estoq.dt-vali-lote,
                                 input item.un,
                                 input saldo-estoq.qtidade-atu,
                                 input saldo-estoq.qt-alocada,
                                 input de-prevista-entrada,
                                 input de-pendente,
                                 input saldo-estoq.qt-aloc-ped,
                                 output lcXMLReturn).

        end.
        when 3 then do:
            run delete in hSoap (input param-integra.web-user,
                                 input param-integra.web-pswd,
                                 input crm-web-processados.num-transacao,
                                 input i-ep-codigo-usuario,
                                 input fc-retira-ptvirg(saldo-estoq.it-codigo),
                                 input fc-retira-ptvirg(saldo-estoq.cod-estabel),
                                 input fc-retira-ptvirg(saldo-estoq.cod-depos),
                                 input fc-retira-ptvirg(saldo-estoq.lote),
                                 input fc-retira-ptvirg(saldo-estoq.cod-localiz),
                                 input fc-retira-ptvirg(saldo-estoq.cod-refer),
                                 output lcXMLReturn).
        end.
    end case.

    assign crm-web-processados.dt-integra  = today
           crm-web-processados.hr-integra  = replace(string(time,"HH:MM:SS"),":","")
           crm-web-processados.log-erro    = no
           crm-web-processados.retorno-web = lcXMLReturn.

end.
