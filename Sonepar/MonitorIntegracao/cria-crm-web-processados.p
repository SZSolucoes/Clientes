def var i-trans as int no-undo.

for each nota-fiscal no-lock:
    do while true:
        assign i-trans = next-value(seq-nota-fiscal-crm).
        
        find crm-web-processados no-lock where
             crm-web-processados.nome-tabela   = 'nota-fiscal' and
             crm-web-processados.num-transacao = i-trans       no-error.
        if not avail crm-web-processados then
            leave.
    end.
    
    create crm-web-processados.
    assign crm-web-processados.dt-trans      = today    
           crm-web-processados.dt-integra    = today
           crm-web-processados.hr-trans      = replace(string(time,"HH:MM:SS"),":","")
           crm-web-processados.hr-integra    = replace(string(time,"HH:MM:SS"),":","")
           crm-web-processados.tipo-trans    = 2
           crm-web-processados.nome-tabela   = 'nota-fiscal'      
           crm-web-processados.chave-tabela  = (nota-fiscal.cod-estabel + ';' + nota-fiscal.serie + ';' + nota-fiscal.nr-nota-fis)
           crm-web-processados.num-transacao = i-trans
           crm-web-processados.log-erro      = no
           crm-web-processados.retorno-web   = 'TESTE'.

end.
