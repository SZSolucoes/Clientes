/*************************************************
** Programa   :twp/twdi163.p                    **
** Data       :Janeiro/2017                     **
** Autor      :Joao Pacheco - SZ                **
** Descricao  :Trigger Tabela PRE€O-ITEM        **
**                                              **
** Alteracao  :                                 **
**                                              **
**                                              **
**************************************************/

define parameter buffer b-preco-item     for preco-item.
define parameter buffer b-old-preco-item for preco-item.

/*
find first param-integra no-lock no-error.

if not avail param-integra  or 
   not param-integra.l-integra-preco-item then 
    return "OK".*/

run webservice/web-preco-item.p (input rowid(b-preco-item),
                                 input if b-old-preco-item.it-codigo <> '' then 2 else 1).

return 'ok'.

