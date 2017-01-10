/*************************************************
** Programa   :esp/escrmapi001.p                **
** Data       :Janeiro/2017                     **
** Autor      :Joao Pacheco - SZ                **
** Descricao  :Reenvio CRM WEB                  **
**                                              **
** Alteracao  :                                 **
**                                              **
**                                              **
**************************************************/

def input param p-row-crm-web-proc as rowid no-undo.


find crm-web-processados no-lock where
     rowid(crm-web-processados) = p-row-crm-web-proc no-error.
if avail crm-web-processados then do:

    case crm-web-processados.nome-tabela:
        when 'saldo-estoq' then
            run pi-saldo-estoq.
    end case.

end.


procedure pi-saldo-estoq:

    def var c-cod-estabel like saldo-estoq.cod-estabel no-undo.
    def var c-cod-depos   like saldo-estoq.cod-depos   no-undo.
    def var c-cod-localiz like saldo-estoq.cod-localiz no-undo.
    def var c-lote        like saldo-estoq.lote        no-undo.
    def var c-it-codigo   like saldo-estoq.it-codigo   no-undo.
    def var c-cod-refer   like saldo-estoq.cod-refer   no-undo.

    find saldo-estoq no-lock where
         saldo-estoq.cod-depos   = c-cod-depos   and
         saldo-estoq.cod-estabel = c-cod-estabel and
         saldo-estoq.cod-localiz = c-cod-localiz and
         saldo-estoq.lote        = c-lote        and
         saldo-estoq.it-codigo   = c-it-codigo   and
         saldo-estoq.cod-refer   = c-cod-refer   no-error.
    if avail saldo-estoq then
        run webservice/web-saldo-estoq.p (input rowid(saldo-estoq),
                                          input crm-web-processados.tipo-trans).

end.
