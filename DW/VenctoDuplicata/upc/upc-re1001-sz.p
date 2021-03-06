/*************************************************
** Programa   :upc/upc-re1001-sz.p              **
** Data       :Novembro/2016                    **
** Autor      :Joao Pacheco - SZ                **
** Descricao  :UPC recalculo Data Vencimento    **
**             das Duplicatas                   **
** Alteracao  :                                 **
**                                              **
**                                              **
**************************************************/

def input param p-row-doc as rowid no-undo.

def var i-avanco-dia  as integer           no-undo.
def var i-dia-aux     as integer           no-undo.
def var i-ultimo-dia  as date              no-undo.
def var da-data       as date extent 12    no-undo.
def var da-partida    as date              no-undo.
def var i-ind         as int               no-undo.
def var l-dt-alt      as log initial no    no-undo.

find first param-global no-lock no-error.

MESSAGE 'p-row-doc - ' string(p-row-doc)
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

find docum-est no-lock where
     rowid(docum-est) = p-row-doc no-error.

MESSAGE avail docum-est
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

if avail docum-est then do:
    if substring(docum-est.char-2,143,8) = '0' then do:
        find emitente no-lock where 
             emitente.cod-emitente = docum-est.cod-emitente no-error.
        if avail emitente then do:
        
            assign da-data = today.
        
            find ext-emitente no-lock where
                 ext-emitente.cod-emitente = emitente.cod-emitente no-error.
            if avail ext-emitente then do:
                if ext-emitente.tp-dt-vencto = 2 then
                    assign da-partida = docum-est.dt-trans.
                else
                    assign da-partida = docum-est.dt-emiss.
            end.
            else
                assign da-partida = docum-est.dt-emiss.
        
            find first item-doc-est of docum-est no-lock no-error.
            if avail item-doc-est then do:
            
                if item-doc-est.num-pedido > 0 then do:
                    find first pedido-compr no-lock where 
                               pedido-compr.num-pedido = item-doc-est.num-pedido no-error.
            
                    find first cond-pagto no-lock where 
                               cond-pagto.cod-cond-pag = pedido-compr.cod-cond-pag no-error.
                end.
                else do:
                    find first rat-ordem no-lock where 
                               rat-ordem.cod-emitente = item-doc-est.cod-emitente and
                               rat-ordem.serie-docto  = item-doc-est.serie-docto  and
                               rat-ordem.nro-docto    = item-doc-est.nro-docto    and
                               rat-ordem.nat-operacao = item-doc-est.nat-operacao and
                               rat-ordem.sequencia    = item-doc-est.sequencia    no-error.
                    if avail rat-ordem then do:
                    
                        find first pedido-compr no-lock where 
                                   pedido-compr.num-pedido = rat-ordem.num-pedido no-error.
            
                        find first cond-pagto no-lock where 
                                   cond-pagto.cod-cond-pag = pedido-compr.cod-cond-pag no-error.
                    end.
                    else
                        find first cond-pagto no-lock where 
                                   cond-pagto.cod-cond-pag = emitente.cod-cond-pag no-error.
                end.
            end.
        
            if avail cond-pagto then do:
                if  cond-pagto.cod-vencto >= 5 and 
                    cond-pagto.cod-vencto <= 8 then
                    assign da-partida = da-partida + 1.
            
                if  cond-pagto.cod-vencto = 5 then
                    do  while day(da-partida) <> 1 and day(da-partida) <> 11
                                                   and day(da-partida) <> 21:
                        assign da-partida = da-partida + 1.
                    end.
            
                if  cond-pagto.cod-vencto = 6 then
                    do  while day(da-partida) <> 1 and day(da-partida) <> 16:
                        assign da-partida = da-partida + 1.
                    end.
            
                if  cond-pagto.cod-vencto = 7 then
                    do  while day(da-partida) <> 1:
                        assign da-partida = da-partida + 1.
                    end.
            
                if  cond-pagto.cod-vencto = 8 then
                    do  while weekday(da-partida) <> 2:
                        assign da-partida = da-partida + 1.
                    end.
            
                if  cond-pagto.dia-mes-base <> 0   and 
                    day(da-partida)<> dia-mes-base then
                    do  while day(da-partida) <> cond-pagto.dia-mes-base:
                        assign da-partida = da-partida + 1.
                    end.
                
                if  cond-pagto.dia-sem-base <> 8       and 
                    cond-pagto.dia-sem-base <> 0       and 
                    weekday(da-partida)<> dia-sem-base then
                    do  while weekday(da-partida) <> cond-pagto.dia-sem-base:
                        assign da-partida = da-partida + 1.
                    end.
                
                do  i-ind = 1 to cond-pagto.num-parcelas:
                    assign da-data[i-ind] = da-partida + cond-pagto.prazos[i-ind].
                    if  cond-pagto.dia-mes-venc >= 29 then
                        assign i-ultimo-dia = da-data[i-ind]  - day(da-data[i-ind]) + 33
                               i-ultimo-dia = i-ultimo-dia - day(i-ultimo-dia)
                               i-dia-aux    = if day(i-ultimo-dia) < cond-pagto.dia-mes-venc
                                                 then day(i-ultimo-dia)
                                                 else cond-pagto.dia-mes-venc.
                    else
                        assign i-dia-aux = cond-pagto.dia-mes-venc.
                    if  cond-pagto.dia-mes-venc <> 0         and 
                        day(da-data[i-ind])     <> i-dia-aux then
                        do  while day(da-data[i-ind]) <> i-dia-aux:
                            assign da-data[i-ind] = da-data[i-ind] + 1.
                        end.
                    if  cond-pagto.dia-sem-venc <> 8 and 
                        cond-pagto.dia-sem-venc <> 0 and 
                        weekday(da-data[i-ind]) <> cond-pagto.dia-sem-venc then
                        do  while weekday(da-data[i-ind]) <> cond-pagto.dia-sem-venc:
                            assign da-data[i-ind] = da-data[i-ind] + 1.
                        end.
                
                    find first calen-coml no-lock where 
                               calen-coml.cod-estabel = docum-est.cod-estabel     and   
                               calen-coml.ep-codigo   = param-global.empresa-prin and   
                               calen-coml.data        = da-data[i-ind]            no-error.
                    i-avanco-dia = 0.
                    if  avail calen-coml then
                        &if defined(bf-mat-comex) &then
                          do while calen-coml.tipo-dia <> "1":          
                        &else
                          do while calen-coml.tipo-dia <> 1:
                        &endif
                            if weekday(da-data[i-ind]) = 1 then /* Domingo */
                                if  emitente.ven-domingo = 3 then 
                                    leave.
                                else do:
                                    if  i-avanco-dia = 0 then
                                        if emitente.ven-domingo = 2 then
                                            assign i-avanco-dia = -1.
                                        else 
                                        if  emitente.ven-domingo = 1 then
                                            assign i-avanco-dia = 1.
                                    da-data[i-ind] = da-data[i-ind] + i-avanco-dia.
                                end.
                            else
                                if  weekday(da-data[i-ind]) = 7 then /* Sabado */
                                    if  emitente.ven-sabado = 3 then 
                                        leave.
                                    else do:
                                        if  i-avanco-dia = 0 then
                                            if  emitente.ven-sabado = 2 then
                                                assign i-avanco-dia = -1.
                                            else
                                            if  emitente.ven-sabado = 1 then
                                                assign i-avanco-dia = 1.
                                        da-data[i-ind] = da-data[i-ind] + i-avanco-dia.
                                    end.
                                else                            /* Feriado */
                                    if  emitente.ven-feriado = 3 then 
                                        leave.
                                    else do:
                                        if  i-avanco-dia = 0 then
                                            if  emitente.ven-feriado = 2 then
                                                assign i-avanco-dia = -1.
                                            else
                                            if  emitente.ven-feriado = 1 then
                                                assign i-avanco-dia = 1.
                                        da-data[i-ind] = da-data[i-ind] + i-avanco-dia.
                                    end.
                            find first calen-coml no-lock where 
                                       calen-coml.cod-estabel = docum-est.cod-estabel     and   
                                       calen-coml.ep-codigo   = param-global.empresa-prin and   
                                       calen-coml.data        = da-data[i-ind] no-error.
                          end.
                end.
            end.
        end.   
    
        output to value (session:temp-directory + "DATAVENCIMENTO.txt").
    
        put unformatted
            skip (2)
            '********** IN�CIO DATA VENCIMENTO ****************':U
            skip (2).
            
        for each dupli-apagar exclusive-lock where
                 dupli-apagar.serie-docto  = docum-est.serie-docto  and
                 dupli-apagar.nro-docto    = docum-est.nro-docto    and
                 dupli-apagar.cod-emitente = docum-est.cod-emitente and
                 dupli-apagar.nat-operacao = docum-est.nat-operacao:
    
            if da-data[int(dupli-apagar.parcela)] <> ?                      and
               da-data[int(dupli-apagar.parcela)] <> dupli-apagar.dt-vencim then do:
    
                assign dupli-apagar.dt-vencim = da-data[int(dupli-apagar.parcela)]
                       l-dt-alt               = yes.
    
                disp dupli-apagar.cod-emitente
                     dupli-apagar.serie-docto
                     dupli-apagar.nro-docto
                     dupli-apagar.nat-operacao
                     dupli-apagar.parcela
                     dupli-apagar.dt-vencim
                     with down frame f.
                     down with frame f.
            end.
        end.
    
        put unformatted
            skip (2)
            '********** FIM DATA VENCIMENTO ****************':U
            skip (2).
    
        output close.
    
        if l-dt-alt then
            dos silent start value(session:temp-directory + "DATAVENCIMENTO.txt") no-error.  
    end.
end.
