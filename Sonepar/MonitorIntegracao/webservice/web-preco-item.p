/*************************************************
** Programa   :webservice/web-preco-item.p      **
** Data       :Janeiro/2017                     **
** Autor      :Joao Pacheco - SZ                **
** Descricao  :Integra‡Æo WSxCRM - Preco-Item   **
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

def input param p-row-preco as rowid no-undo.
def input param p-tp-trans  as int   no-undo.

def var hWebService         as handle   no-undo.
def var hSoap               as handle   no-undo.
def var i-trans             as int      no-undo.
def var lcXMLReturn         as longchar no-undo.

find preco-item no-lock where
     rowid(preco-item) = p-row-preco no-error.
if avail preco-item then do:

    do while true:
        assign i-trans = next-value(seq-preco-item-crm).
        
        find crm-web-processados no-lock where
             crm-web-processados.nome-tabela   = 'preco-item' and
             crm-web-processados.num-transacao = i-trans      no-error.
        if not avail crm-web-processados then
            leave.
    end.
    
    create crm-web-processados.
    assign crm-web-processados.num-transacao = i-trans
           crm-web-processados.dt-trans      = today               
           crm-web-processados.hr-trans      = replace(string(time,"HH:MM:SS"),":","")           
           crm-web-processados.tipo-trans    = p-tp-trans
           crm-web-processados.nome-tabela   = 'preco-item'      
           crm-web-processados.chave-tabela  = (preco-item.nr-tabpre + ';' + preco-item.it-codigo + ';' + preco-item.cod-refer + ';' +
                                                preco-item.cod-unid-med + ';' + string(preco-item.dt-inival) + ';' + string(preco-item.quant-min)). 

    /*find first param-integra no-lock no-error.

    create server hWebService.
    hWebService:connect("-WSDL '" + param-integra.web-wsdl + "/Estoque/xml/method?wsdl'").

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
    end case.*/

    assign crm-web-processados.dt-integra  = today
           crm-web-processados.hr-integra  = replace(string(time,"HH:MM:SS"),":","")
           crm-web-processados.log-erro    = no
           crm-web-processados.retorno-web = lcXMLReturn.

end.

