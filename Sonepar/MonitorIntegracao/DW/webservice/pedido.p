/*************************************************
** Programa   :webservice/pedido.p              **
** Data       :Dezembro/2017                    **
** Autor      :Joao Pacheco - SZ                **
** Descricao  :integra��o WSxCRM - Pedido DW    **
**                                              **
** Alteracao  :                                 **
**                                              **
**                                              **
**************************************************/

def input param p-xml-ped  as longchar no-undo.
def output param p-xml-ret as longchar no-undo.

{webservice/pedido.i}

/*BO*/
def var bo-ped-venda     as handle  no-undo.
def var bo-ped-venda-sdf as handle  no-undo.
def var bo-ped-venda-com as handle  no-undo.
def var bo-ped-venda-cal as handle  no-undo.
def var bo-ped-venda-can as handle  no-undo.
def var bo-ped-item      as handle  no-undo.
def var bo-ped-item-sdf  as handle  no-undo.
def var bo-ped-item-can  as handle  no-undo.
def var bo-ped-repre     as handle  no-undo.
def var h-alocacao       as handle  no-undo.
/*XML*/
def var hgenxml          as handle  no-undo.
def var hmessagehandler  as handle  no-undo.
def var creturnvalue     as char    no-undo.
def var hdocxml          as handle  no-undo.
def var iid_0            as integer no-undo.
def var iid_1            as integer no-undo.
def var iid_2            as integer no-undo.
def var iid_3            as integer no-undo.
def var iid_4            as integer no-undo.
def var iid_5            as integer no-undo.
def var iid_6            as integer no-undo.
def var iIDSon           as integer no-undo.
def var iIDSon_1         as integer no-undo.
def var iIDSon_2         as integer no-undo.
def var iIDSon_5         as integer no-undo.

def var c-nome-rep-indireto as char no-undo.
def var c-nome-rep-direto   as char no-undo.
def var c-nome-redespacho   as char no-undo.
def var lResult             as log  no-undo.
def var l-arquivo-valido    as log  no-undo.
def var i-trans             as int  no-undo.

def new global shared var gr-ped-venda as rowid no-undo.

{dibo/bodi159cpy.i}


/* verifica se messagehandler.p est� na mem�ria */
{xmlinc/xmlloadmessagehandler.i &messagehandler="hmessagehandler" &mhreturnvalue="creturnvalue"}

/* verifica se apixml esta na memoria */
{xmlinc/xmlloadgenxml.i &genxml="hgenxml" &gxreturnvalue="creturnvalue"}

/* verifica se o ut-genxml foi executado corretamente */
if creturnvalue <> "ok" then
    return creturnvalue.

find first param-integra no-lock no-error.
if avail param-integra            and
   param-integra.l-integra-pedido then do:

    empty temp-table tt-arq-ped.
    empty temp-table tt-arq-ite.
    empty temp-table tt-arq-lote.

    do while true:
        assign i-trans = next-value(seq-ped-venda-crm).
        
        find crm-web-processados no-lock where
             crm-web-processados.nome-tabela   = 'ped-venda' and
             crm-web-processados.num-transacao = i-trans     no-error.
        if not avail crm-web-processados then
            leave.
    end.
    
    create crm-web-processados.
    assign crm-web-processados.num-transacao = i-trans
           crm-web-processados.dt-trans      = today
           crm-web-processados.hr-trans      = replace(string(time,"HH:MM:SS"),":","")
           crm-web-processados.tipo-trans    = 1
           crm-web-processados.nome-tabela   = 'ped-venda'.

    run pi-le-xml.

    if can-find(first tt-arq-ped) then do:

        run pi-validate-pedido.

        run pi-cria-pedido.

    end.

    run pi-gera-resposta.
end.

procedure pi-le-xml:

    def var hDocXmlPedido as handle no-undo.

    assign l-arquivo-valido = no.

    if p-xml-ped <> '' then do:        

        create x-document hDocXmlPedido.
        
        lResult = hDocXmlPedido:load("longchar":U, p-xml-ped, false) no-error.

        if lResult then do:
            assign l-arquivo-valido = yes.
    
            run reset in hGenXml.
            
            run setencoding in hgenxml ("utf-8").      
            
            run loadxml in hGenXml (input hDocXmlPedido).
            
            run searchTag in hGenXml (input "pedido",
                                      input 0,
                                      output iId_0).
    
            if iId_0 <> ? then do:
    
                create tt-arq-ped.
    
                run getSonVal in hGenXml (input iId_0,
                                          input "cod_estabel",
                                          output tt-arq-ped.cod-estabel).
    
                run getSonVal in hGenXml (input iId_0,
                                          input "cod_emitente",
                                          output tt-arq-ped.cod-emitente).
    
                run getSonVal in hGenXml (input iId_0,
                                          input "nr_pedcli",
                                          output tt-arq-ped.nr-pedcli).
    
                run getSonVal in hGenXml (input iId_0,
                                          input "emergencial",
                                          output tt-arq-ped.emergencial).
    
                run getSonVal in hGenXml (input iId_0,
                                          input "dt_entrega",
                                          output tt-arq-ped.dt-entrega).
    
                run getSonVal in hGenXml (input iId_0,
                                          input "cod_cond_pag",
                                          output tt-arq-ped.cod-cond-pag).
    
                run getSonVal in hGenXml (input iId_0,
                                          input "nr_tabpre",
                                          output tt-arq-ped.nr-tabpre).
    
                run getSonVal in hGenXml (input iId_0,
                                          input "cod_priori",
                                          output tt-arq-ped.cod-priori).
    
                run getSonVal in hGenXml (input iId_0,
                                          input "observacoes",
                                          output tt-arq-ped.observacoes).
    
                run getSonVal in hGenXml (input iId_0,
                                          input "nome_transp",
                                          output tt-arq-ped.nome-transp).
    
                run getSonVal in hGenXml (input iId_0,
                                          input "tipo_preco",
                                          output tt-arq-ped.tp-preco).
    
                run getSonVal in hGenXml (input iId_0,
                                          input "mo_codigo",
                                          output tt-arq-ped.mo-codigo).
    
                run getSonVal in hGenXml (input iId_0,
                                          input "no_ab_reppri",
                                          output tt-arq-ped.no-ab-reppri).
    
                run getSonVal in hGenXml (input iId_0,
                                          input "vl_desconto",
                                          output tt-arq-ped.vl-desconto).
    
                run getSonVal in hGenXml (input iId_0,
                                          input "nome_ab_rep",
                                          output tt-arq-ped.nome-ab-rep).
    
                run getSonVal in hGenXml (input iId_0,
                                          input "cod_entrega",
                                          output tt-arq-ped.cod-entrega).
    
                run getSonVal in hGenXml (input iId_0,
                                          input "tipo_fatur",
                                          output tt-arq-ped.tipo-fatur).
    
                run getSonVal in hGenXml (input iId_0,
                                          input "nat_operacao",
                                          output tt-arq-ped.nat-operacao).
    
                run getSonVal in hGenXml (input iId_0,
                                          input "dt_min_fat",
                                          output tt-arq-ped.dt-min-fat).
    
                run getSonVal in hGenXml (input iId_0,
                                          input "transp_redesp",
                                          output tt-arq-ped.transp-redesp).
    
                run getSonVal in hGenXml (input iId_0,
                                          input "nr_pedrep",
                                          output tt-arq-ped.nr-pedrep).
    
                run getSonVal in hGenXml (input iId_0,
                                          input "cidade_cif",
                                          output tt-arq-ped.cidade-cif).
    
                run getSonVal in hGenXml (input iId_0,
                                          input "cod_canal_venda",
                                          output tt-arq-ped.cod-canal-venda).
    
                run getSonVal in hGenXml (input iId_0,
                                          input "e_mail",
                                          output tt-arq-ped.e-mail).
    
                run getSonVal in hGenXml (input iId_0,
                                          input "nome-abrev-tri",
                                          output tt-arq-ped.nome-abrev-tri).
    
                run getSonVal in hGenXml (input iId_0,
                                          input "cod-entrega-tri",
                                          output tt-arq-ped.cod-entrega-tri).
    
                run getSonVal in hGenXml (input iId_0,
                                          input "bu",
                                          output tt-arq-ped.cod-unid-negoc).
    
                run getSonVal in hGenXml (input iId_0,
                                          input "finalidade",
                                          output tt-arq-ped.cod-des-merc).
    
                run getSonVal in hGenXml (input iId_0,
                                          input "tipo_doc_fiscal",
                                          output tt-arq-ped.modal-doc-fiscal).
    
                run getSonVal in hGenXml (input iId_0,
                                          input "prioridade_logistica",
                                          output tt-arq-ped.cod-priori-log).
    
                run searchTag in hGenXml (input "itens",
                                          input iId_0,
                                          output iId_1).
                
                do while iId_1 <> ?:
    
                    run searchTag in hGenXml (input "item",
                                              input iId_1,
                                              output iId_2).
    
                    do while iId_2 <> ?:
    
                        create tt-arq-ite.
    
                        assign tt-arq-ite.cod-emitente = tt-arq-ped.cod-emitente
                               tt-arq-ite.nr-pedcli    = tt-arq-ped.nr-pedcli.
    
                        run getSonVal in hGenXml (input iId_2,
                                                  input "sequencia",
                                                  output tt-arq-ite.nr-sequencia).
    
                        run getSonVal in hGenXml (input iId_2,
                                                  input "it_codigo",
                                                  output tt-arq-ite.it-codigo).
    
                        run getSonVal in hGenXml (input iId_2,
                                                  input "qt_pedida",
                                                  output tt-arq-ite.qt-pedida).
    
                        run getSonVal in hGenXml (input iId_2,
                                                  input "vl_preori",
                                                  output tt-arq-ite.vl-preori).
    
                        run getSonVal in hGenXml (input iId_2,
                                                  input "nr_tabpre",
                                                  output tt-arq-ite.nr-tabpre).
    
                        run getSonVal in hGenXml (input iId_2,
                                                  input "tp_preco",
                                                  output tt-arq-ite.tp-preco).
    
                        run getSonVal in hGenXml (input iId_2,
                                                  input "vl_total",
                                                  output tt-arq-ite.vl-total).
    
                        run getSonVal in hGenXml (input iId_2,
                                                  input "perc_icms",
                                                  output tt-arq-ite.perc-icms).
    
                        run getSonVal in hGenXml (input iId_2,
                                                  input "complemento_descricao",
                                                  output tt-arq-ite.obs-item-ped).
    
                        run getSonVal in hGenXml (input iId_2,
                                                  input "nat_operacao",
                                                  output tt-arq-ite.nat-operacao).
    
                        run getSonVal in hGenXml (input iId_2,
                                                  input "cod_estabel",
                                                  output tt-arq-ite.cod-estabel).
    
                        run getSonVal in hGenXml (input iId_2,
                                                  input "dt_entrega",
                                                  output tt-arq-ite.dt-entrega).
    
                        run getSonVal in hGenXml (input iId_2,
                                                  input "ind-icm-ret",
                                                  output tt-arq-ite.ind-icm-ret).
    
                        run getSonVal in hGenXml (input iId_2,
                                                  input "minimo",
                                                  output tt-arq-ite.lanc-minimo).
    
                        run getSonVal in hGenXml (input iId_2,
                                                  input "obrigatorio",
                                                  output tt-arq-ite.lanc-obrigato).
    
                        run getSonVal in hGenXml (input iId_2,
                                                  input "xml_xped",
                                                  output tt-arq-ite.ped-compr).
    
                        run getSonVal in hGenXml (input iId_2,
                                                  input "xml_nitemped",
                                                  output tt-arq-ite.seq-ped-compr).
    
                        run getSonVal in hGenXml (input iId_2,
                                                  input "bu",
                                                  output tt-arq-ite.cod-unid-negoc).
    
                        run getSonVal in hGenXml (input iId_2,
                                                  input "iva",
                                                  output tt-arq-ite.per-st-icm).
    
                        run getSonVal in hGenXml (input iId_2,
                                                  input "icms_estadual_st",
                                                  output tt-arq-ite.per-icm-estad-st).
    
                        run searchTag in hGenXml (input "lotes",
                                                  input iId_3,
                                                  output iId_4).
        
                        do while iId_4 <> ?:
                            run searchTag in hGenXml (input "lote",
                                                      input iId_4,
                                                      output iId_5).
    
                            do while iId_5 <> ?:
    
                                find emitente no-lock where
                                     emitente.cod-emitente = tt-arq-ped.cod-emitente no-error.
    
                                create tt-arq-lote.
                                assign tt-arq-lote.nome-abrev    = emitente.nome-abrev
                                       tt-arq-lote.nr-pedcli     = tt-arq-ped.nr-pedcli
                                       tt-arq-lote.nr-sequencia  = tt-arq-ite.nr-sequencia
                                       tt-arq-lote.it-codigo     = tt-arq-ite.it-codigo
                                       tt-arq-lote.cod-refer     = "" no-error.
    
                                run getSonVal in hGenXml (input iId_5,
                                                          input "codigo",
                                                          output tt-arq-lote.codigo).
    
                                run getSonVal in hGenXml (input iId_5,
                                                          input "deposito",
                                                          output tt-arq-lote.cod-depos).
    
                                run getSonVal in hGenXml (input iId_5,
                                                          input "localizacao",
                                                          output tt-arq-lote.cod-localiz).
    
                                run getSonVal in hGenXml (input iId_5,
                                                          input "quantidade",
                                                          output tt-arq-lote.quantidade).
    
                                run getSonVal in hGenXml (input iId_5,
                                                          input "validade",
                                                          output tt-arq-lote.dt-validade).
    
                                assign iIDSon_5 = iId_5.
    
                                run GetNextSonId in hGenXml (input iId_4,
                                                             input iIDSon_5,
                                                             output iId_5).
                            end.
                        end.
    
                        assign iIDSon_2 = iId_2.
    
                        run GetNextSonId in hGenXml (input iId_1,
                                                     input iIDSon_2,
                                                     output iId_2).
                    end.
    
                    assign iIDSon_1 = iId_1.
    
                    run GetNextSonId in hGenXml (input iId_0,
                                                 input iIDSon_1,
                                                 output iId_1).
                end.
            end.
        end.
    end.
end.

procedure pi-validate-pedido:
    def var l-erros-item-ped as log no-undo.

    find first tt-arq-ped no-error.

    assign crm-web-processados.chave-tabela = string(tt-arq-ped.cod-emitente) + ';' + tt-arq-ped.nr-pedcli. 

    find emitente no-lock where 
         emitente.cod-emitente = int(tt-arq-ped.cod-emitente) no-error.
    if not avail emitente then do:    
        create tt-log-ped-venda.
        assign tt-log-ped-venda.mensagem    = "Cliente nao Cadastrado"
               tt-log-ped-venda.nome-abrev  = ""
               tt-log-ped-venda.nr-pedcli   = tt-arq-ped.nr-pedcli
               tt-log-ped-venda.importou    = false
               tt-log-ped-venda.data-import = today
               tt-log-ped-venda.hora-import = string(time,"HH:MM:SS").

        run pi-grava-log-pedvenda.

        return "NOK".
    end.

    assign crm-web-processados.chave-tabela = crm-web-processados.chave-tabela + ';' + emitente.nome-abrev.

    find ped-venda no-lock where 
         ped-venda.nome-abrev = emitente.nome-abrev  and 
         ped-venda.nr-pedcli  = tt-arq-ped.nr-pedcli no-error.
    if avail ped-venda then do:
        create tt-log-ped-venda.
        assign tt-log-ped-venda.mensagem    = "Pedido j� Cadastrado."
               tt-log-ped-venda.nome-abrev  = emitente.nome-abrev
               tt-log-ped-venda.nr-pedcli   = tt-arq-ped.nr-pedcli
               tt-log-ped-venda.importou    = false
               tt-log-ped-venda.data-import = today
               tt-log-ped-venda.hora-import = string(time,"HH:MM:SS").

        run pi-grava-log-pedvenda.

        return "NOK".
    end.

    assign c-nome-rep-indireto = ""
           c-nome-rep-direto   = "".

    find repres no-lock where 
         repres.nome-abrev = tt-arq-ped.no-ab-reppri no-error.
    if not avail repres then do:
        create tt-log-ped-venda.
        assign tt-log-ped-venda.mensagem    = "Representante nao Cadastrado inDIRETO"
               tt-log-ped-venda.nome-abrev  = emitente.nome-abrev 
               tt-log-ped-venda.nr-pedcli   = tt-arq-ped.nr-pedcli
               tt-log-ped-venda.importou    = false
               tt-log-ped-venda.data-import = today
               tt-log-ped-venda.hora-import = string(time,"HH:MM:SS").

        run pi-grava-log-pedvenda.

        return "NOK".
    end.
    assign c-nome-rep-indireto = repres.nome-abrev.

    find repres no-lock where repres.cod-rep = emitente.cod-rep no-error.
    if not avail repres then do:
        create tt-log-ped-venda.
        assign tt-log-ped-venda.mensagem    = "Representante do Cliente nao Cadastrado DIRETO"
               tt-log-ped-venda.nome-abrev  = emitente.nome-abrev 
               tt-log-ped-venda.nr-pedcli   = tt-arq-ped.nr-pedcli
               tt-log-ped-venda.importou    = false
               tt-log-ped-venda.data-import = today
               tt-log-ped-venda.hora-import = string(time,"HH:MM:SS").

        run pi-grava-log-pedvenda.

        return "NOK".
    end.
    assign c-nome-rep-direto = repres.nome-abrev.

    find estabelec no-lock where 
         estabelec.cod-estabel = tt-arq-ped.cod-estabel no-error.
    if not avail estabelec then do:
        create tt-log-ped-venda.
        assign tt-log-ped-venda.mensagem    = "Estabelecimento nao Cadastrado"
               tt-log-ped-venda.nome-abrev  = emitente.nome-abrev
               tt-log-ped-venda.nr-pedcli   = tt-arq-ped.nr-pedcli
               tt-log-ped-venda.importou    = false
               tt-log-ped-venda.data-import = today
               tt-log-ped-venda.hora-import = string(time,"HH:MM:SS").

        run pi-grava-log-pedvenda.

        return "NOK".              
               
    end.   

    find natur-oper no-lock where 
         natur-oper.nat-operacao = tt-arq-ped.nat-operacao no-error.
    if avail natur-oper then do:
       if natur-oper.transf = no then do:

            find cond-pagto no-lock where 
                 cond-pagto.cod-cond-pag = int(tt-arq-ped.cod-cond-pag) no-error.
            if not avail cond-pagto then do:
                create tt-log-ped-venda.
                assign tt-log-ped-venda.mensagem    = "Condicao de Pagamento nao Cadastrado"
                       tt-log-ped-venda.nome-abrev  = emitente.nome-abrev 
                       tt-log-ped-venda.nr-pedcli   = tt-arq-ped.nr-pedcli
                       tt-log-ped-venda.importou    = false
                       tt-log-ped-venda.data-import = today
                       tt-log-ped-venda.hora-import = string(time,"HH:MM:SS").
        
                run pi-grava-log-pedvenda.
        
                return "NOK".
            end.
        end. 
    end.    

    assign c-nome-redespacho = "".        

    find transporte no-lock where 
         transporte.nome-abrev = tt-arq-ped.transp-redesp no-error.
    if not avail transporte then do:
        create tt-log-ped-venda.
        assign tt-log-ped-venda.mensagem    = "Transportadora de Redespacho nao Cadastrado"
               tt-log-ped-venda.nome-abrev  = emitente.nome-abrev 
               tt-log-ped-venda.nr-pedcli   = tt-arq-ped.nr-pedcli
               tt-log-ped-venda.importou    = false
               tt-log-ped-venda.data-import = today
               tt-log-ped-venda.hora-import = string(time,"HH:MM:SS").

        run pi-grava-log-pedvenda.

        return "NOK".
    end.    

    assign c-nome-redespacho = transporte.nome-abrev.

    find transporte no-lock where 
         transporte.nome-abrev = tt-arq-ped.nome-transp no-error.
    if not avail transporte then do:
        create tt-log-ped-venda.
        assign tt-log-ped-venda.mensagem    = "Transportadora nao Cadastrado"
               tt-log-ped-venda.nome-abrev  = emitente.nome-abrev 
               tt-log-ped-venda.nr-pedcli   = tt-arq-ped.nr-pedcli
               tt-log-ped-venda.importou    = false
               tt-log-ped-venda.data-import = today
               tt-log-ped-venda.hora-import = string(time,"HH:MM:SS").

        run pi-grava-log-pedvenda.

        return "NOK".
    end.    

    find natur-oper no-lock where 
         natur-oper.nat-operacao = tt-arq-ped.nat-operacao no-error.
    if not avail natur-oper then do: 
        create tt-log-ped-venda.
        assign tt-log-ped-venda.mensagem    = "Natureza de Operacao nao Cadastrado"
               tt-log-ped-venda.nome-abrev  = emitente.nome-abrev 
               tt-log-ped-venda.nr-pedcli   = tt-arq-ped.nr-pedcli
               tt-log-ped-venda.importou    = false
               tt-log-ped-venda.data-import = today
               tt-log-ped-venda.hora-import = string(time,"HH:MM:SS").

        run pi-grava-log-pedvenda.

        return "NOK".
    end.

    find first tt-arq-ite where 
               tt-arq-ite.cod-emitente = tt-arq-ped.cod-emitente and 
               tt-arq-ite.nr-pedcli    = tt-arq-ped.nr-pedcli    no-error.
    if not avail tt-arq-ite then do:
        assign l-erros-item-ped = yes.

        create tt-log-ped-venda.
        assign tt-log-ped-venda.mensagem    = "Nenhum item para o Pedido foi Encontrato, impossivel continuar!"
               tt-log-ped-venda.nome-abrev  = emitente.nome-abrev
               tt-log-ped-venda.nr-pedcli   = tt-arq-ped.nr-pedcli
               tt-log-ped-venda.importou    = false
               tt-log-ped-venda.data-import = today
               tt-log-ped-venda.hora-import = string(time,"HH:MM:SS").

        run pi-grava-log-pedvenda.

        return "NOK".
    end.

    for each tt-arq-ite where 
             tt-arq-ite.cod-emitente = tt-arq-ped.cod-emitente and 
             tt-arq-ite.nr-pedcli    = tt-arq-ped.nr-pedcli:

        find natur-oper no-lock where 
             natur-oper.nat-operacao = tt-arq-ite.nat-operacao no-error.        
        if not avail natur-oper then do: 
           create tt-log-ped-item.
           assign tt-log-ped-item.mensagem     = "Natureza de Operacao do item nao Cadastrada"
                  tt-log-ped-item.nome-abrev   = emitente.nome-abrev 
                  tt-log-ped-item.nr-pedcli    = tt-arq-ped.nr-pedcli
                  tt-log-ped-item.it-codigo    = tt-arq-ite.it-codigo   
                  tt-log-ped-item.cod-refer    = "" 
                  tt-log-ped-item.nr-sequencia = int(tt-arq-ite.nr-sequencia)
                  tt-log-ped-item.importou     = false.
                  
           create tt-log-ped-venda.
           assign tt-log-ped-venda.mensagem    = "Natureza de Operacao do item nao Cadastrada"
                  tt-log-ped-venda.nome-abrev  = emitente.nome-abrev
                  tt-log-ped-venda.nr-pedcli   = tt-arq-ped.nr-pedcli
                  tt-log-ped-venda.importou    = false
                  tt-log-ped-venda.data-import = today
                  tt-log-ped-venda.hora-import = string(time,"HH:MM:SS").      
        
           run pi-grava-log-peditem.
        
           return "NOK".
        
        end.
    end.

    assign l-erros-item-ped = no.

    for each tt-arq-ite where 
             tt-arq-ite.cod-emitente = tt-arq-ped.cod-emitente and 
             tt-arq-ite.nr-pedcli    = tt-arq-ped.nr-pedcli:

        find item no-lock where 
             item.it-codigo = tt-arq-ite.it-codigo no-error.

        if not avail item then do:
            assign l-erros-item-ped = yes.

            create tt-log-ped-venda.
            assign tt-log-ped-venda.mensagem    = "item do Pedido nao Cadastrado, impossovel Criar o Pedido: " + string(tt-arq-ite.it-codigo)
                   tt-log-ped-venda.nome-abrev  = emitente.nome-abrev
                   tt-log-ped-venda.nr-pedcli   = tt-arq-ped.nr-pedcli
                   tt-log-ped-venda.importou    = false
                   tt-log-ped-venda.data-import = today
                   tt-log-ped-venda.hora-import = string(time,"HH:MM:SS").

            run pi-grava-log-pedvenda.
    
            return "NOK".       
        end.
        
        if item.ind-item-fat = no then do:
            assign l-erros-item-ped = yes.

            create tt-log-ped-venda.
            assign tt-log-ped-venda.mensagem    = "item nao Faturavel, impossivel Criar o Pedido: " + string(item.it-codigo)
                   tt-log-ped-venda.nome-abrev  = emitente.nome-abrev
                   tt-log-ped-venda.nr-pedcli   = tt-arq-ped.nr-pedcli
                   tt-log-ped-venda.importou    = false
                   tt-log-ped-venda.data-import = today
                   tt-log-ped-venda.hora-import = string(time,"HH:MM:SS").

            run pi-grava-log-pedvenda.

            return "NOK".       
        end.

        find item-uni-estab no-lock where 
             item-uni-estab.it-codigo   = item.it-codigo and 
             item-uni-estab.cod-estabel = tt-arq-ite.cod-estabel no-error.
        if not avail item-uni-estab then do:
            assign l-erros-item-ped = yes.

            create tt-log-ped-venda.
            assign tt-log-ped-venda.mensagem    = "item Uni Estab nao Cadastrado, impossovel Criar o Pedido: " + string(item.it-codigo) + " Est: " + tt-arq-ite.cod-estabel
                   tt-log-ped-venda.nome-abrev  = emitente.nome-abrev
                   tt-log-ped-venda.nr-pedcli   = tt-arq-ped.nr-pedcli
                   tt-log-ped-venda.importou    = false
                   tt-log-ped-venda.data-import = today
                   tt-log-ped-venda.hora-import = string(time,"HH:MM:SS").

            run pi-grava-log-pedvenda.

            return "NOK".
        end.

        if item-uni-estab.ind-item-fat = no then do:
            assign l-erros-item-ped = yes.

            create tt-log-ped-venda.
            assign tt-log-ped-venda.mensagem    = "item Uni Estab nao Faturavel, impossivel Criar o Pedido: " + string(item.it-codigo)
                   tt-log-ped-venda.nome-abrev  = emitente.nome-abrev
                   tt-log-ped-venda.nr-pedcli   = tt-arq-ped.nr-pedcli
                   tt-log-ped-venda.importou    = false
                   tt-log-ped-venda.data-import = today
                   tt-log-ped-venda.hora-import = string(time,"HH:MM:SS").

            run pi-grava-log-pedvenda.

            return "NOK".
        end.
    end.
    
    if l-erros-item-ped = yes then 
        return "NOK".

    return "OK".
end procedure.

procedure pi-grava-log-pedvenda :
    
    assign tt-log-ped-venda.cod-estabel      = tt-arq-ped.cod-estabel
           tt-log-ped-venda.emergencial      = int(tt-arq-ped.emergencial)
           tt-log-ped-venda.tipo-transacao   = 1                    
           tt-log-ped-venda.dt-implant       = today
           tt-log-ped-venda.dt-entrega       = date(tt-arq-ped.dt-entrega)
           tt-log-ped-venda.nat-operacao     = tt-arq-ped.nat-operacao
           tt-log-ped-venda.cod-cond-pag     = int(tt-arq-ped.cod-cond-pag)
           tt-log-ped-venda.nr-tabpre        = tt-arq-ped.nr-tabpre
           tt-log-ped-venda.cod-priori       = int(tt-arq-ped.cod-priori)
           tt-log-ped-venda.perc-desco1      = 0                            
           tt-log-ped-venda.perc-desco2      = 0
           tt-log-ped-venda.observacoes      = tt-arq-ped.observacoes
           tt-log-ped-venda.nome-transp      = tt-arq-ped.nome-transp
           tt-log-ped-venda.tb-preco         = int(tt-arq-ped.tb-preco)
           tt-log-ped-venda.mo-codigo        = int(tt-arq-ped.mo-codigo)
           tt-log-ped-venda.no-ab-reppri     = tt-arq-ped.no-ab-reppri
           tt-log-ped-venda.vl-desconto      = dec(tt-arq-ped.vl-desconto)
           tt-log-ped-venda.nome-ab-rep      = tt-arq-ped.nome-ab-rep
           tt-log-ped-venda.cod-entrega      = tt-arq-ped.cod-entrega
           tt-log-ped-venda.tipo-fatur       = int(tt-arq-ped.tipo-fatur)
           tt-log-ped-venda.dt-min-fat       = date(tt-arq-ped.dt-min-fat)
           tt-log-ped-venda.transp-redesp    = tt-arq-ped.transp-redesp
           tt-log-ped-venda.nr-pedrep        = tt-arq-ped.nr-pedrep
           tt-log-ped-venda.cidade-cif       = tt-arq-ped.cidade-cif
           tt-log-ped-venda.cod-priori-log   = int(tt-arq-ped.cod-priori-log)
           tt-log-ped-venda.modal-doc-fiscal = tt-arq-ped.modal-doc-fiscal.

end procedure.

procedure pi-cria-pedido :
    
    empty temp-table tt-ped-venda.
    empty temp-table tt-ped-item.

    PedidoTotal:
    do transaction:

        find first param-global no-lock no-error.

        create tt-log-ped-venda.
        assign tt-log-ped-venda.nome-abrev  = emitente.nome-abrev
               tt-log-ped-venda.nr-pedcli   = tt-arq-ped.nr-pedcli
               tt-log-ped-venda.importou    = false
               tt-log-ped-venda.cd-erro     = 0
               tt-log-ped-venda.mensagem    = ""
               tt-log-ped-venda.data-import = today
               tt-log-ped-venda.hora-import = string(time,"HH:MM:SS").

        run pi-grava-log-pedvenda.

        if emitente.e-mail <> tt-arq-ped.e-mail then do:

            disable triggers for load of emitente.
           
            find current emitente exclusive-lock no-error.

            assign emitente.e-mail = tt-arq-ped.e-mail.

            find first cont-emit of emitente exclusive-lock where 
                       cont-emit.int-1 = 2 no-error.
            if not avail cont-emit then do:
                find last buf-cont-emit of emitente no-lock no-error.

                create cont-emit.
                assign cont-emit.cod-emitente = emitente.cod-emitente
                       cont-emit.sequencia    = if not avail buf-cont-emit then 10 else buf-cont-emit.sequencia + 10
                       cont-emit.nome         = "CONTATO NFE"
                       cont-emit.identific    = emitente.identific
                       cont-emit.int-1        = 2.                     
            end.

            assign cont-emit.e-mail = emitente.e-mail.
        end.

        if not valid-handle(bo-ped-venda) then
            run dibo/bodi159.p    persistent set bo-ped-venda.

        if not valid-handle(bo-ped-venda-sdf) then
            run dibo/bodi159sdf.p persistent set bo-ped-venda-sdf.

        if valid-handle(bo-ped-venda) then
            run openQueryStatic in bo-ped-venda (input "ChPedido":U).

        run newRecord in bo-ped-venda.

        run getRecord in bo-ped-venda (output table tt-ped-venda).

        find first tt-ped-venda no-error.

        /*** Cria a tt-ped-venda com os Valores do CRM *******************************************/
        assign tt-ped-venda.nome-abrev  = emitente.nome-abrev
               tt-ped-venda.nr-pedcli   = tt-arq-ped.nr-pedcli
               tt-ped-venda.nr-Pedido   = next-value(seq-nr-Pedido)
               tt-ped-venda.cod-estabel = tt-arq-ped.cod-estabel.

        run inputtable             in bo-ped-venda-sdf (input table tt-ped-venda).
        run setdefaultcustomer     in bo-ped-venda-sdf.
        run setDefaultCentralSales in bo-ped-venda-sdf.
        run outputTable            in bo-ped-venda-sdf (output table tt-ped-venda). 

        find natur-oper no-lock where 
             natur-oper.nat-operacao = tt-arq-ped.nat-operacao no-error.
        
        find first tt-ped-venda.
        /*** Altera Alguns Campos do Pedido passados *********************************************/
        assign tt-ped-venda.nat-operacao    = tt-arq-ped.nat-operacao
               tt-ped-venda.no-ab-reppri    = c-nome-rep-direto
               tt-ped-venda.cod-cond-pag    = if avail natur-oper and (natur-oper.transf or not natur-oper.emite-duplic) then 0 else int(tt-arq-ped.cod-cond-pag)
               tt-ped-venda.dt-emissao      = today
               tt-ped-venda.dt-implant      = today
               tt-ped-venda.dt-entrega      = date(tt-arq-ped.dt-entrega)
               tt-ped-venda.dt-entorig      = date(tt-arq-ped.dt-entrega)
               tt-ped-venda.nome-transp     = tt-arq-ped.nome-transp
               tt-ped-venda.dt-fimvig       = ?
               tt-ped-venda.observacoes     = tt-arq-ped.observacoes
               tt-ped-venda.cond-espec      = ""
               tt-ped-venda.tp-preco        = int(tt-arq-ped.tb-preco)
               tt-ped-venda.cidade-cif      = ""              
               tt-ped-venda.nr-tabpre       = tt-arq-ped.nr-tabpre
               tt-ped-venda.user-alt        = ""  
               tt-ped-venda.dt-useralt      = ?
               tt-ped-venda.nr-pedrep       = tt-arq-ped.nr-pedrep
               tt-ped-venda.nome-tr-red     = c-nome-redespacho
               tt-ped-venda.ind-fat-par     = if int(tt-arq-ped.tipo-fatur) = 2 then yes else no
               tt-ped-venda.cidade-cif      = if tt-arq-ped.cidade-cif = "FOB" then "" else "Cif"
               tt-ped-venda.cod-entrega     = tt-arq-ped.cod-entrega
               tt-ped-venda.cod-canal-venda = (if int(tt-arq-ped.cod-canal-venda) = 0 then tt-ped-venda.cod-canal-venda else int(tt-arq-ped.cod-canal-venda) )
               tt-ped-venda.cod-estabel     = tt-arq-ped.cod-estabel
               tt-ped-venda.nome-abrev-tri  = tt-arq-ped.nome-abrev-tri 
               tt-ped-venda.cod-entrega-tri = tt-arq-ped.cod-entrega-tri
               tt-ped-venda.cod-unid-negoc  = tt-arq-ped.cod-unid-negoc
               tt-ped-venda.cod-des-merc    = int(tt-arq-ped.cod-des-merc).
               
               /**Caso a natureza seja de transferencia, atribuir o Destino da Mercadoria como - Comercio-Industria - Cfreire sottelli**/
               find natur-oper no-lock where 
                    natur-oper.nat-operacao = tt-arq-ped.nat-operacao no-error.
               if avail natur-oper  and 
                  natur-oper.transf then
                   assign tt-ped-venda.cod-des-merc  = 1.

        /*** Defauts Condicao de Pagamento *******************************************************/
        assign tt-ped-venda.nr-tab-finan = if natur-oper.transf = no then cond-pagto.nr-tab-finan else 1
               tt-ped-venda.nr-ind-finan = if natur-oper.transf = no then cond-pagto.nr-ind-finan else 2.
    
        /*** Avaliacao de Credito ****************************************************************/
        assign tt-ped-venda.cod-sit-aval  = 3  /* APROVADO */
               tt-ped-venda.desc-forc-cr  = "Liberado Automatico".
        
        run emptyRowErrors  in bo-ped-venda.
        run setRecord       in bo-ped-venda(input table tt-ped-venda).
        run createRecord    in bo-ped-venda.
    
        run getRowErrors    in bo-ped-venda(output table RowErrors).
            
        if  can-find(first RowErrors where 
                           RowErrors.ErrorSubType = "ERROR":U) then do:
            for each RowErrors where RowErrors.ErrorSubType = "ERROR":U:

                assign tt-log-ped-venda.importou   = false
                       tt-log-ped-venda.cd-erro    = RowErrors.ErrorNumber                        
                       tt-log-ped-venda.mensagem   = tt-log-ped-venda.mensagem + " " + RowErrors.ErrorDescription .
            end.
    
            if valid-handle(bo-ped-venda) then do:
               
                run destroyBo in bo-ped-venda.
                
                if valid-handle(bo-ped-venda) then do:
                    delete procedure bo-ped-venda.
                    assign bo-ped-venda = ?.
                end.
            end.
    
            if valid-handle(bo-ped-venda-sdf) then do:
                delete procedure bo-ped-venda-sdf.
                assign bo-ped-venda-sdf = ?.
            end.

            undo PedidoTotal, return "NOK".
        end.
        
        if  can-find(first RowErrors where 
                           RowErrors.ErrorSubType <> "ERROR":U) then do:
            for each RowErrors where R
                     RowErrors.ErrorSubType <> "ERROR":U:                    
                assign tt-log-ped-venda.cd-erro    = RowErrors.ErrorNumber                        
                       tt-log-ped-venda.mensagem   = tt-log-ped-venda.mensagem + " " + RowErrors.ErrorDescription.
            end.

            if valid-handle(bo-ped-venda) then do:
                run destroyBo in bo-ped-venda.
                
                if valid-handle(bo-ped-venda) then do:
                    delete procedure bo-ped-venda.
                    assign bo-ped-venda = ?.
                end.
            end.

            if valid-handle(bo-ped-venda-sdf) then do:
                delete procedure bo-ped-venda-sdf.
                assign bo-ped-venda-sdf = ?.
            end.    

            undo PedidoTotal, return "NOK".
        end.
        
        run getRecord in bo-ped-venda (output table tt-ped-venda).

        find first tt-ped-venda.
        
        run getrowid in bo-ped-venda(output tt-ped-venda.r-rowid).
        
        if not can-find(first ped-repre where 
                              ped-repre.nr-Pedido   = tt-ped-venda.nr-Pedido and 
                              ped-repre.nome-ab-rep = c-nome-rep-direto)     then do:
            create ped-repre.
            assign ped-repre.nr-Pedido   = tt-ped-venda.nr-Pedido
                   ped-repre.nome-ab-rep = c-nome-rep-direto
                   ped-repre.ind-repbase = yes
                   ped-repre.perc-comis  = repres.comis-direta
                   ped-repre.comis-emis  = repres.comis-emis.
        end.
    
        if not can-find(first ped-repre where 
                              ped-repre.nr-Pedido   = tt-ped-venda.nr-Pedido and 
                              ped-repre.nome-ab-rep = c-nome-rep-indireto)   then do:
            create ped-repre.
            assign ped-repre.nr-Pedido   = tt-ped-venda.nr-Pedido
                   ped-repre.nome-ab-rep = c-nome-rep-indireto
                   ped-repre.ind-repbase = yes
                   ped-repre.perc-comis  = 0
                   ped-repre.comis-emis  = 0.
        end.
        
        find dw-ped-venda exclusive-lock where 
             dw-ped-venda.nr-Pedido = tt-ped-venda.nr-Pedido no-error.  
        if not avail dw-ped-venda then do:
              create dw-ped-venda.
              assign dw-ped-venda.nr-Pedido        = tt-ped-venda.nr-Pedido
                     dw-ped-venda.cod-priori       = int(tt-arq-ped.cod_priori_log)
                     dw-ped-venda.modal-doc-fiscal = tt-arq-ped.modal_doc_fiscal.
        end.
        else
            assign dw-ped-venda.cod-priori       = int(tt-arq-ped.cod_priori_log)
                   dw-ped-venda.modal-doc-fiscal = tt-arq-ped.modal_doc_fiscal.

        if not can-find(first dw-follow-up-ped where 
                              dw-follow-up-ped.nome-abrev = tt-ped-venda.nome-abrev and 
                              dw-follow-up-ped.nr-pedcli  = tt-ped-venda.nr-pedcli) then do:
            create dw-follow-up-ped.
            assign dw-follow-up-ped.nome-abrev   = tt-ped-venda.nome-abrev 
                   dw-follow-up-ped.nr-pedcli    = tt-ped-venda.nr-pedcli
                   dw-follow-up-ped.user-criacao = tt-ped-venda.user-impl
                   dw-follow-up-ped.dt-criacao   = today
                   dw-follow-up-ped.hr-criacao   = string(time,'hh:mm:ss').
        end.
        
        if valid-handle(bo-ped-venda) then do:
            run destroyBo in bo-ped-venda.

            if valid-handle(bo-ped-venda) then do:
                delete procedure bo-ped-venda.
                assign bo-ped-venda = ?.
            end.
        end.

        if valid-handle(bo-ped-venda-sdf) then do:
            delete procedure bo-ped-venda-sdf.
            assign bo-ped-venda-sdf = ?.
        end.    

        for each tt-arq-ite where 
                 tt-arq-ite.cod-emitente = tt-arq-ped.cod-emitente and 
                 tt-arq-ite.nr-pedcli    = tt-arq-ped.nr-pedcli:

            empty temp-table tt-ped-item. 

            create tt-ped-item.
            assign tt-ped-item.nome-abrev   = emitente.nome-abrev
                   tt-ped-item.nr-pedcli    = tt-arq-ped.nr-pedcli 
                   tt-ped-item.nr-sequencia = int(tt-arq-ite.nr-sequencia)
                   tt-ped-item.it-codigo    = tt-arq-ite.it-codigo 
                   tt-ped-item.cod-refer    = ""
                   tt-ped-item.tipo-atend   = if int(tt-arq-ped.tipo-fatur) = 1 then 1 else 2
                   tt-ped-item.cod-entrega  = tt-arq-ped.cod-entrega.

            create tt-log-ped-item.                                     
            assign tt-log-ped-item.nome-abrev   = tt-ped-venda.nome-abrev 
                   tt-log-ped-item.nr-pedcli    = tt-ped-venda.nr-pedcli  
                   tt-log-ped-item.nr-sequencia = tt-ped-item.nr-sequencia
                   tt-log-ped-item.it-codigo    = tt-ped-item.it-codigo   
                   tt-log-ped-item.cod-refer    = tt-ped-item.cod-refer 
                   tt-log-ped-item.importou     = false.

            run pi-grava-log-peditem.

            if not valid-handle(bo-ped-item-sdf) then
                run dibo/bodi154sdf.p persistent set bo-ped-item-sdf.

            run inputtable      in bo-ped-item-sdf (input table tt-ped-item).
            run setdefaultitem  in bo-ped-item-sdf.
            run outputtable     in bo-ped-item-sdf (output table tt-ped-item). 

            if valid-handle(bo-ped-item-sdf) then do:
                delete procedure bo-ped-item-sdf.
                assign bo-ped-item-sdf = ?.
            end.

            find first tt-ped-item.
            assign tt-ped-item.qt-pedida               = dec(tt-arq-ite.qt-pedida)
                   tt-ped-item.vl-preuni               = dec(tt-arq-ite.vl-preori)
                   tt-ped-item.vl-preori               = dec(tt-arq-ite.vl-preori)
                   tt-ped-item.des-pct-desconto-inform = "0"
                  /*tt-ped-item.qt-log-aloca            = tt-arq-ite.qt-pedida*/ 
                   tt-ped-item.dt-min-fat              = date(tt-arq-ped.dt-min-fat)
                   tt-ped-item.observacao              = tt-arq-ite.obs-item-ped
                   tt-ped-item.nat-operacao            = tt-arq-ite.nat-operacao
                   tt-ped-item.dt-entrega              = date(tt-arq-ite.dt-entrega)
                   tt-ped-item.dt-entorig              = tt-ped-item.dt-entrega
                   tt-ped-item.cod-ord-compra          = string(tt-arq-ite.ped_compr)   
                   tt-ped-item.parcela                 = int(tt-arq-ite.seq_ped_compr)
                   tt-ped-item.ind-icm-ret             = (tt-arq-ite.ind-icm-ret = "1")
                   tt-ped-item.cod-unid-negoc          = tt-arq-ite.cod-unid-negoc.

            if not valid-handle(bo-ped-item) then
                run dibo/bodi154.p    persistent set bo-ped-item.

            run openQueryStatic in bo-ped-item (input "Main":U).
            run setRecord       in bo-ped-item (input table tt-ped-item).
            run emptyRowErrors  in bo-ped-item.
            run createRecord    in bo-ped-item.

            run getRowErrors    in bo-ped-item(output table RowErrors).

            if can-find(first RowErrors where
                              RowErrors.ErrorSubType = "ERROR":U) then do:
                for each RowErrors where 
                         RowErrors.ErrorSubType = "ERROR":U:
                    assign tt-log-ped-item.importou = false
                           tt-log-ped-item.cd-erro  = RowErrors.ErrorNumber                        
                           tt-log-ped-item.mensagem = tt-log-ped-venda.mensagem + " " + RowErrors.ErrorDescription.  
                end.

                if valid-handle(bo-ped-venda) then do:
                    run destroyBo in bo-ped-venda.
                   
                    if valid-handle(bo-ped-venda) then do:
                        delete procedure bo-ped-venda.
                        assign bo-ped-venda = ?.
                    end.
                end.
        
                if valid-handle(bo-ped-venda-sdf) then do:
                    delete procedure bo-ped-venda-sdf.
                    assign bo-ped-venda-sdf = ?.
                end.    

                undo PedidoTotal, return "NOK".
            end.
            else do:
                if can-find(first RowErrors where 
                                  RowErrors.ErrorSubType <> "ERROR":U) then do:
                    for each RowErrors where 
                             RowErrors.ErrorSubType <> "ERROR":U:
                        assign tt-log-ped-item.cd-erro  = RowErrors.ErrorNumber                        
                               tt-log-ped-item.mensagem = tt-log-ped-venda.mensagem + " " + RowErrors.ErrorDescription .  
                    end.
                end.
            end.
           
            find first tt-ped-item.
           
            find first esp-ped-item no-lock where  
                       esp-ped-item.nome-abrev        = tt-ped-item.nome-abrev   and    
                       esp-ped-item.nr-pedcli         = tt-ped-item.nr-pedcli    and    
                       esp-ped-item.nr-sequencia      = tt-ped-item.nr-sequencia and    
                       esp-ped-item.it-codigo         = tt-ped-item.it-codigo    and    
                       esp-ped-item.cod-refer         = tt-ped-item.cod-refer    no-error.           
            if not avail esp-ped-item then do:
                create esp-ped-item.
                assign esp-ped-item.nome-abrev        = tt-ped-item.nome-abrev
                       esp-ped-item.nr-pedcli         = tt-ped-item.nr-pedcli
                       esp-ped-item.nr-sequencia      = tt-ped-item.nr-sequencia
                       esp-ped-item.it-codigo         = tt-ped-item.it-codigo
                       esp-ped-item.cod-refer         = tt-ped-item.cod-refer
                       esp-ped-item.lance-minimo      = dec(tt-arq-ite.lanc_minimo)
                       esp-ped-item.lance-obrigatorio = dec(tt-arq-ite.lanc_obrigato)
                       esp-ped-item.ped-compr         = tt-arq-ite.ped_compr    
                       esp-ped-item.seq-ped-compr     = tt-arq-ite.seq_ped_compr
                       /* MODifICADO EM 21/02/2016 Por ISRAEL ABRAHAO A PEDIDO DO SR. JUNIor
                       esp-ped-item.per-sub-icm-trib  = dec(tt-arq-ite.per_st_icm)      
                       esp-ped-item.per-icm-estad-sub = dec(tt-arq-ite.per_icm_estad_st)
                       */
                       esp-ped-item.per-sub-icm-trib  = ?      
                       esp-ped-item.per-icm-estad-sub = ?.
                  
            end.
            
            assign tt-log-ped-item.importou     = true.

            run pi-grava-subt (input tt-arq-ped.cod-estabel,
                               input tt-ped-item.it-codigo,
                               input dec(tt-arq-ite.per_st_icm),
                               input dec(tt-arq-ite.per_icm_estad_st),
                               input int(tt-arq-ite.cod-emitente)).
           
            if valid-handle(bo-ped-item) then do:
                run destroyBo in bo-ped-item.

                if valid-handle(bo-ped-item) then do:
                    delete procedure bo-ped-item.
                    assign bo-ped-item = ?.
                end.
            end.
        end.

        run pi-calcula-Pedido.

        if return-value = "NOK" then
            return "NOK".

        run pi-efetiva-Pedido.

        if return-value = "NOK" then
            return "NOK".

        find ped-venda no-lock where 
             ped-venda.nome-abrev = emitente.nome-abrev  and 
             ped-venda.nr-pedcli  = tt-arq-ped.nr-pedcli no-error.
        if avail ped-venda then do:
            for each ped-item of ped-venda exclusive-lock:
                assign ped-item.qt-un-fat = ped-item.qt-pedida.
            end.

            if ped-venda.cod-estabel <> "716" then do:
                /**********CFF - Aloca��o de Lote 25/02/2015*******************************/
                if can-find(first ped-ent of ped-venda) and 
                   can-find(first tt-arq-lote)          then do:
                    
                    if not valid-handle(bo-ped-venda) then
                        run dibo/bodi159.p    persistent set bo-ped-venda.
                 
                    run emptyRowErrors     in bo-ped-venda.
                    run validateAllocation in bo-ped-venda(input rowid(ped-venda)).
                    run getRowErrors       in bo-ped-venda(output table RowErrors).
                 
                    if can-find(first RowErrors where 
                                      RowErrors.ErrorType <> "INTERNAL":U) then do:
                        for each RowErrors:
                            assign tt-log-ped-venda.cd-erro    = RowErrors.ErrorNumber                        
                                   tt-log-ped-venda.mensagem   = tt-log-ped-venda.mensagem + " " + RowErrors.ErrorDescription .
                        end.
                    end.
                    /*else do:
                        for each ped-item of ped-venda no-lock:
                            find first ped-ent no-lock where 
                                       ped-ent.nome-abrev     = ped-venda.nome-abrev  and 
                                       ped-ent.nr-pedcli      = ped-venda.nr-pedcli   and 
                                       ped-ent.it-codigo      = ped-item.it-codigo    and 
                                       ped-ent.cod-refer      = ped-item.cod-refer    and 
                                       ped-ent.nr-sequencia   = ped-item.nr-sequencia no-error.
                            if avail ped-ent then do:
                                find item no-lock where 
                                     item.it-codigo = ped-ent.it-codigo no-error.

                                for each tt-arq-lote where 
                                         tt-arq-lote.nome-abrev   = ped-venda.nome-abrev  and 
                                         tt-arq-lote.nr-pedcli    = ped-venda.nr-pedcli   and 
                                         tt-arq-lote.nr-sequencia = ped-item.nr-sequencia and 
                                         tt-arq-lote.it-codigo    = ped-item.it-codigo    and 
                                         tt-arq-lote.cod-refer    = ped-item.cod-refer:

                                    for each saldo-estoq no-lock where 
                                             saldo-estoq.cod-depos     = tt-arq-lote.cod-depos           and 
                                             saldo-estoq.cod-estabel   = ped-venda.cod-estabel           and 
                                             saldo-estoq.cod-localiz   = tt-arq-lote.cod-localiz         and 
                                             saldo-estoq.lote          = tt-arq-lote.codigo              and 
                                             saldo-estoq.it-codigo     = ped-ent.it-codigo               and 
                                             saldo-estoq.cod-refer     = ped-ent.cod-refer               and 
                                            (saldo-estoq.qtidade-atu   - (saldo-estoq.qt-alocada  +      
                                                                          saldo-estoq.qt-aloc-ped +                      
                                                                          saldo-estoq.qt-aloc-prod)) > 0 and 
                                            (item.tipo-con-est         < 3  or 
                                             saldo-estoq.dt-vali-lote >= ped-ent.dt-entrega),
                                        first deposito no-lock where 
                                              deposito.cod-depos = saldo-estoq.cod-depos and 
                                              deposito.ind-acabado:   

                                        if (saldo-estoq.qtidade-atu - (saldo-estoq.qt-alocada  +                      
                                                                       saldo-estoq.qt-aloc-ped +                      
                                                                       saldo-estoq.qt-aloc-prod)) >= tt-arq-lote.quantidade  then do:
                                            
                                            /*  run pi-aloca-fisica-man (input rowid(ped-ent), */
/*                                                                       input-output tt-arq-lote.quantidade, */
/*                                                                       input rowid(saldo-estoq)). */ /*retirada esta logica devido a implamtacao do dwpd002*/
                                        end.
                                        else
                                            assign tt-log-ped-venda.cd-erro    = 99999                        
                                                   tt-log-ped-venda.mensagem   = tt-log-ped-venda.mensagem + " " + "Quantidade informada do lote maior que quantiade disponivel." .
                                     end.                                                                                
                                end.
                            end.
                        end.
                    end.*/
                end.
                
                if valid-handle (h-alocacao) then do:
                    delete procedure h-alocacao.
                       h-alocacao = ?.
                end.
                if valid-handle(bo-ped-venda) then do:
                      run destroyBo in bo-ped-venda.
                       
                      if valid-handle(bo-ped-venda) then do:
                          delete procedure bo-ped-venda.
                          bo-ped-venda = ?.
                      end.
                
                end.
            end.
           
            if emitente.estado = "PR" and ped-venda.cod-estabel = "716" then
                run pi-copia-Pedido (input rowid(ped-venda)).
        end.

        assign tt-log-ped-venda.importou = true.

        release ped-venda.
        release ped-item.
        release ped-ent.
        release emitente.
        release cont-emit.
        release dw-ped-venda.
        release dw-follow-up-ped.
        
    end.
    
    return "NOK".

end procedure.

procedure pi-grava-log-peditem :
    assign tt-log-ped-item.cod-estabel     = tt-arq-ite.cod-estabel          
           tt-log-ped-item.tipo-transacao  = 1
           tt-log-ped-item.nat-operacao    = tt-arq-ite.nat-operacao
           tt-log-ped-item.qt-pedida       = dec(tt-arq-ite.qt-pedida)
           tt-log-ped-item.vl-preori       = dec(tt-arq-ite.vl-preori)
           tt-log-ped-item.vl-preuni       = dec(tt-arq-ite.vl-preori)
           tt-log-ped-item.per-des-item    = 0
           tt-log-ped-item.nr-tabpre       = tt-arq-ite.nr-tabpre
           tt-log-ped-item.tb-preco        = int(tt-arq-ite.tb-preco)
           tt-log-ped-item.vl-total        = dec(tt-arq-ite.vl-total)
           tt-log-ped-item.perc-icms       = dec(tt-arq-ite.perc-icms)
           tt-log-ped-item.obs-item-ped    = tt-arq-ite.obs-item-ped.

end procedure.

procedure pi-grava-subt:
    def input param pi-cod-estabel      like tt-arq-ped.cod-estabel no-undo.              
    def input param pi-it-codigo        like tt-arq_ite.c_arq_ite_it-codigo   no-undo.                         
    def input param pi-per_st_icm       as dec                                no-undo.
    def input param pi-per_icm_estad_st as dec                                no-undo.
    def input param pi-cod-emitente     like ped-venda.cod-emitente           no-undo.
    
    if pi-per_st_icm <> ? and pi-per_icm_estad_st <> ? and
       pi-per_st_icm <> 0 and pi-per_icm_estad_st <> 0 then do:
        assign c-estado-origem  = ""
               c-estado-destino = "".
                           
        find estabelec no-lock where 
             estabelec.cod-estabel = pi-cod-estabel no-error.
        if avail estabelec then
            assign c-estado-origem = estabelec.estado.

        find emitente no-lock where 
             emitente.cod-emitente = pi-cod-emitente no-error.
        if avail emitente then
            assign c-estado-destino = emitente.estado.       

        /* Validacao incluida para evitar erro gerado no caso de inclus�o de casas decimais no XML que causou erro alterado aliquotas de ICMS para 120%, 170% e 180% */
        if pi-per_icm_estad_st < 100 and pi-per_st_icm < 100 then do:
            find first item-uf exclusive-lock where 
                       item-uf.it-codigo       = pi-it-codigo     and   
                       item-uf.cod-estado-orig = c-estado-origem  and   
                       item-uf.estado          = c-estado-destino no-error.
            if avail item-uf then
                assign item-uf.per-sub-tri = pi-per_st_icm
                       item-uf.dec-1       = pi-per_icm_estad_st.
            else do:
                create item-uf.
                assign item-uf.it-codigo       = pi-it-codigo    
                       item-uf.cod-estado-orig = c-estado-origem 
                       item-uf.estado          = c-estado-destino
                       item-uf.per-sub-tri     = pi-per_st_icm       
                       item-uf.dec-1           = pi-per_icm_estad_st.
            end.
        end.
    end.
           
end procedure.

procedure pi-calcula-Pedido :
    if not valid-handle(bo-ped-venda-cal) then
        run dibo/bodi159cal.p persistent set bo-ped-venda-cal.

    find first tt-ped-venda no-error.

    run calculateOrder in bo-ped-venda-cal(input tt-ped-venda.r-rowid).

    if can-find(first RowErrors where 
                      RowErrors.ErrorSubType = "ERROR":U) then do:

        for each RowErrors where 
                 RowErrors.ErrorSubType = "ERROR":U:
            assign tt-log-ped-venda.mensagem = tt-log-ped-venda.mensagem + " " + RowErrors.ErrorDescription
                   tt-log-ped-venda.cd-erro  = RowErrors.ErrorNumber
                   tt-log-ped-venda.importou = false.
        end.

        return "NOK":U.

    end.

    if valid-handle(bo-ped-venda-cal) then do:
        delete procedure bo-ped-venda-cal.
        assign bo-ped-venda-cal = ?.
    end.

    return "OK".

end procedure.

procedure pi-efetiva-Pedido :

    find first tt-ped-venda no-error.
    
    if not valid-handle(bo-ped-venda-com) then
        run dibo/bodi159com.p persistent set bo-ped-venda-com.
    
    run completeOrder in bo-ped-venda-com(input  tt-ped-venda.r-rowid,
                                          output table RowErrors).
    
    run destroyBO in bo-ped-venda-com.
    delete procedure bo-ped-venda-com.
    assign bo-ped-venda-com = ?.    

    if can-find(first RowErrors where 
                      RowErrors.ErrorSubType = "ERROR":U ) then do:
        for each RowErrors where 
                 RowErrors.ErrorSubType = "ERROR":U:
            assign tt-log-ped-venda.importou = false
                   tt-log-ped-venda.cd-erro  = RowErrors.ErrorNumber
                   tt-log-ped-venda.mensagem = tt-log-ped-venda.mensagem + " " + RowErrors.ErrorDescription.
        end.

        return "NOK":U.
    end.
    
    if not can-find(first RowErrors where 
                          RowErrors.ErrorSubType = "ERROR":U)  then
        assign tt-ped-venda.completo = yes.

    return "OK".

end procedure.

procedure pi-copia-Pedido:

    def input parameter p-rw-ped-venda as rowid no-undo.

    if not valid-handle(bo-ped-venda-sdf) then
        run dibo/bodi159sdf.p persistent set bo-ped-venda-sdf.
    
    run SETDefaultOrderNumber in bo-ped-venda-sdf (output i-sequencia). 

    if valid-handle(bo-ped-venda-sdf) then
        delete procedure bo-ped-venda-sdf.
    
    if not valid-handle(bo-ped-venda-cpy) then
        run dibo/bodi159cpy.p persistent set bo-ped-venda-cpy.

    for each RowErrors:
        delete RowErrors.
    end.
    
    empty temp-table tt-ped-copia      no-error.
    empty temp-table tt-item-copia     no-error.
    empty temp-table tt-arq-lote-copia no-error.
    
    find ped-venda no-lock where 
         rowid(ped-venda) = p-rw-ped-venda no-error.
    if avail ped-venda then do:
        assign i-nr-Pedido-orig    = ped-venda.nr-Pedido
               c-nat-operacao-orig = ped-venda.nat-operacao
               c-cod-estabel-orig  = ped-venda.cod-estabel
               i-cod-emitente      = ped-venda.cod-emitente.

        assign c-nr-pedcli-original  = ped-venda.nr-pedcli
               c-nome-abrev-original = ped-venda.nome-abrev.

        find dw-ped-venda no-lock where 
             dw-ped-venda.nr-Pedido = ped-venda.nr-Pedido no-error.
        if avail dw-ped-venda then
            assign i-cod-priori-orig  = dw-ped-venda.cod-priori
                   c-modal-doc-fiscal = dw-ped-venda.modal-doc-fiscal.

        find first dw-param-transf-oper no-lock where 
                   dw-param-transf-oper.cod-estabel-ori  = c-cod-estabel-orig  and 
                   dw-param-transf-oper.cod-estabel-dest = "701"               and 
                   dw-param-transf-oper.nat-operacao     = c-nat-operacao-orig no-error.
        if avail dw-param-transf-oper then do:
            find estabelec no-lock where 
                 estabelec.cod-estabel = dw-param-transf-oper.cod-estabel-ori no-error.
            if avail estabelec then do:
                find emitente no-lock where 
                     emitente.cod-emitente = estabelec.cod-emitente no-error.
                if avail emitente then
                    assign i-cod-emitente = emitente.cod-emitente.
            end.
        end.
        else
            return "NOK".

        find emitente no-lock where 
             emitente.cod-emitente = i-cod-emitente no-error.

        create tt-ped-copia.
        assign tt-ped-copia.nome-abrev   = emitente.nome-abrev
               tt-ped-copia.nr-Pedido    = i-sequencia
               tt-ped-copia.nr-pedcli    = "PD" + trim(ped-venda.nr-pedcli)
               tt-ped-copia.dt-emissao   = ped-venda.dt-emissao
               tt-ped-copia.dt-entrega   = ped-venda.dt-entrega
               tt-ped-copia.nr-tab-finan = ped-venda.nr-tab-finan
               tt-ped-copia.nr-ind-finan = ped-venda.nr-ind-finan
               tt-ped-copia.cod-cond-pag = 0 /*ped-venda.cod-cond-pag*/
               tt-ped-copia.cod-entrega  = "Padr�o"
               tt-ped-copia.nome-transp  = ped-venda.nome-transp
               tt-ped-copia.perc-desco1  = ped-venda.perc-desco1
               tt-ped-copia.esp-ped      = ped-venda.esp-ped
               tt-ped-copia.tp-preco     = ped-venda.tp-preco
               tt-ped-copia.vl-desconto  = ped-venda.vl-desconto
               tt-ped-copia.e-mail       = emitente.e-mail
               tt-ped-copia.ind-apr-cred = emitente.ind-apr-cred
               tt-ped-copia.ind-cre-cli  = emitente.ind-cre-cli.
    
        for each ped-item of ped-venda no-lock:
            create tt-item-copia.
            assign tt-item-copia.nr-sequencia = ped-item.nr-sequencia
                   tt-item-copia.it-codigo    = ped-item.it-codigo
                   tt-item-copia.qt-pedida    = ped-item.qt-pedida
                   tt-item-copia.dt-entrega   = ped-item.dt-entrega
                   tt-item-copia.selecionado  = yes.
    
            find item no-lock where 
                 item.it-codigo = ped-item.it-codigo no-error.
            if avail item then
                assign tt-item-copia.desc-item = item.desc-item.
        end.
    end.

    run copyOrder in bo-ped-venda-cpy(input  p-rw-ped-venda,
                                      input  table tt-ped-copia,
                                      input  table tt-item-copia,
                                      input  i-situacao,
                                      input  i-natur-oper,
                                      input  l-exp-dt-entrega,
                                      output gr-ped-venda,
                                      output table RowErrors).

    delete procedure bo-ped-venda-cpy.
    
    if not can-find(first RowErrors where 
                          RowErrors.ErrorTYPE <> "INTERNAL":U and 
                          RowErrors.ErrorSubTYPE = "Error":U) then do:
        for each esp-ped-item of ped-venda no-lock:
            create bf-esp-ped-item.
            buffer-copy esp-ped-item except esp-ped-item.nr-pedcli esp-ped-item.nome-abrev to bf-esp-ped-item.
            assign bf-esp-ped-item.nome-abrev        = tt-ped-copia.nome-abrev
                   bf-esp-ped-item.nr-pedcli         = tt-ped-copia.nr-pedcli.
        end.
    
        for each tt-ped-copia:
            delete tt-ped-copia.
        end.
    end.

    find ped-venda exclusive-lock where 
         rowid(ped-venda) = gr-ped-venda no-error.
    if avail ped-venda then do:
        assign i-nr-Pedido-novo = ped-venda.nr-Pedido.

        find first dw-param-transf-oper no-lock where 
                   dw-param-transf-oper.cod-estabel-ori  = c-cod-estabel-orig  and 
                   dw-param-transf-oper.cod-estabel-dest = "701"               and 
                   dw-param-transf-oper.nat-operacao     = c-nat-operacao-orig no-error.

        if avail dw-param-transf-oper then
            assign ped-venda.cod-estabel  = dw-param-transf-oper.cod-estabel-dest
                   ped-venda.nat-operacao = dw-param-transf-oper.nat-operacao-dest.
                
        assign ped-venda.observacoes = "Pedido Origem: " + string(i-nr-Pedido-orig).

        find emitente no-lock where 
             emitente.nome-abrev = ped-venda.nome-abrev no-error.

        for each ped-item of ped-venda exclusive-lock:
            find first dw-param-transf-oper no-lock where 
                       dw-param-transf-oper.cod-estabel-ori  = c-cod-estabel-orig    and 
                       dw-param-transf-oper.cod-estabel-dest = "701"                 and 
                       dw-param-transf-oper.nat-operacao     = ped-item.nat-operacao no-error.
          
            if avail dw-param-transf-oper then
                assign ped-item.nat-operacao = dw-param-transf-oper.nat-operacao-dest.

            assign ped-item.qt-un-fat = ped-item.qt-pedida.

            if ped-venda.cod-estabel = "701" and emitente.estado = "PR" and ped-item.nat-operacao begins("54") then
                assign ped-item.ind-icm-ret = yes.
            else
                assign ped-item.ind-icm-ret = no.
        end.
      
        find dw-ped-venda exclusive-lock where 
             dw-ped-venda.nr-Pedido = ped-venda.nr-Pedido no-error.
        if not avail dw-ped-venda then do:
            create dw-ped-venda.
            assign dw-ped-venda.nr-Pedido        = ped-venda.nr-Pedido
                   dw-ped-venda.cod-priori       = i-cod-priori-orig
                   dw-ped-venda.modal-doc-fiscal = c-modal-doc-fiscal.
        end.
        
        find dw-ped-venda exclusive-lock where 
             dw-ped-venda.nr-Pedido = i-nr-Pedido-orig no-error.
        if avail dw-ped-venda then
            assign dw-ped-venda.nr-Pedido-transf = i-nr-Pedido-novo. 
        else do:
            create dw-ped-venda.
            assign dw-ped-venda.nr-Pedido        = i-nr-Pedido-orig
                   dw-ped-venda.cod-priori       = i-cod-priori-orig
                   dw-ped-venda.modal-doc-fiscal = c-modal-doc-fiscal
                   dw-ped-venda.nr-Pedido-transf = i-nr-Pedido-novo.
        end.

        find dw-follow-up-ped no-lock where 
             dw-follow-up-ped.nome-abrev = ped-venda.nome-abrev and 
             dw-follow-up-ped.nr-pedcli  = ped-venda.nr-pedcli  no-error.
        if not avail dw-follow-up-ped then do:
            create dw-follow-up-ped.
            assign dw-follow-up-ped.nome-abrev   = ped-venda.nome-abrev 
                   dw-follow-up-ped.nr-pedcli    = ped-venda.nr-pedcli
                   dw-follow-up-ped.user-criacao = ped-venda.user-impl
                   dw-follow-up-ped.dt-criacao   = today
                   dw-follow-up-ped.hr-criacao   = string(time,'hh:mm:ss').
        end.

        /*** Aloca��o de Estoque - 4make - Wanderley - 17/12/2015 ***/
        for each tt-arq-lote where 
                 tt-arq-lote.nr-pedcli  = c-nr-pedcli-original and 
                 tt-arq-lote.nome-abrev = c-nome-abrev-original:
          create tt-arq-lote-copia.
          assign tt-arq-lote-copia.nome-abrev    = ped-venda.nome-abrev 
                 tt-arq-lote-copia.nr-pedcli     = ped-venda.nr-pedcli 
                 tt-arq-lote-copia.nr-sequencia  = tt-arq-lote.nr-sequencia
                 tt-arq-lote-copia.it-codigo     = tt-arq-lote.it-codigo   
                 tt-arq-lote-copia.cod-refer     = tt-arq-lote.cod-refer   
                 tt-arq-lote-copia.codigo        = tt-arq-lote.codigo      
                 tt-arq-lote-copia.cod-depos     = tt-arq-lote.cod-depos   
                 tt-arq-lote-copia.cod-localiz   = tt-arq-lote.cod-localiz 
                 tt-arq-lote-copia.quantidade    = tt-arq-lote.quantidade  
                 tt-arq-lote-copia.dt-validade   = tt-arq-lote.dt-validade.
        end.

        if can-find(first ped-ent of ped-venda) and 
           can-find(first tt-arq-lote-copia)    then do:

            if not valid-handle(bo-ped-venda) then
                run dibo/bodi159.p persistent set bo-ped-venda.


            run emptyRowErrors     in bo-ped-venda.
            run validateAllocation in bo-ped-venda(input rowid(ped-venda)).
            run getRowErrors       in bo-ped-venda(output table RowErrors).
         
            if can-find(first RowErrors where 
                              RowErrors.ErrorType <> "INTERNAL":U) then do:
                for each RowErrors:
                    assign tt-log-ped-venda.cd-erro  = RowErrors.ErrorNumber
                           tt-log-ped-venda.mensagem = tt-log-ped-venda.mensagem + " " + RowErrors.ErrorDescription.
                end.
            end.
            else do:
                for each ped-item of ped-venda no-lock:
                    find first ped-ent no-lock where 
                               ped-ent.nome-abrev   = ped-item.nome-abrev   and 
                               ped-ent.nr-pedcli    = ped-item.nr-pedcli    and 
                               ped-ent.it-codigo    = ped-item.it-codigo    and 
                               ped-ent.cod-refer    = ped-item.cod-refer    and 
                               ped-ent.nr-sequencia = ped-item.nr-sequencia no-error.
                    if avail ped-ent then do:
                        find item no-lock where 
                             item.it-codigo = ped-ent.it-codig no-error.

                        for each tt-arq-lote-copia where 
                                 tt-arq-lote-copia.nome-abrev   = ped-venda.nome-abrev  and 
                                 tt-arq-lote-copia.nr-pedcli    = ped-venda.nr-pedcli   and 
                                 tt-arq-lote-copia.nr-sequencia = ped-item.nr-sequencia and 
                                 tt-arq-lote-copia.it-codigo    = ped-item.it-codigo    and 
                                 tt-arq-lote-copia.cod-refer    = ped-item.cod-refer:

                            for each saldo-estoq no-lock where 
                                     saldo-estoq.cod-depos   = tt-arq-lote-copia.cod-depos   and 
                                     saldo-estoq.cod-estabel = ped-venda.cod-estabel         and 
                                     saldo-estoq.cod-localiz = tt-arq-lote-copia.cod-localiz and 
                                     saldo-estoq.lote        = tt-arq-lote-copia.codigo      and 
                                     saldo-estoq.it-codigo   = ped-ent.it-codigo             and 
                                     saldo-estoq.cod-refer   = ped-ent.cod-refer             and 
                                    (saldo-estoq.qtidade-atu - (saldo-estoq.qt-alocada  +                      
                                                                saldo-estoq.qt-aloc-ped +                      
                                                                saldo-estoq.qt-aloc-prod)) > 0 and 
                                    (item.tipo-con-est         < 3                   or 
                                     saldo-estoq.dt-vali-lote >= ped-ent.dt-entrega) no-lock,
                               first deposito no-lock where 
                                     deposito.cod-depos   = saldo-estoq.cod-depos and 
                                     deposito.ind-acabado = yes:   

                                if (saldo-estoq.qtidade-atu - (saldo-estoq.qt-alocada  +                      
                                                               saldo-estoq.qt-aloc-ped +                      
                                                               saldo-estoq.qt-aloc-prod)) >= tt-arq-lote-copia.quantidade  then do:

                                    run pi-aloca-fisica-man (input rowid(ped-ent),
                                                             input-output tt-arq-lote-copia.quantidade, 
                                                             input rowid(saldo-estoq)).
                                end.
                                else
                                    assign tt-log-ped-venda.cd-erro    = 99999                        
                                           tt-log-ped-venda.mensagem   = tt-log-ped-venda.mensagem + " " + "Quantidade informada do lote maior que quantiade disponivel." .
                            end.
                        end.
                    end.
                end.
            end.

            if valid-handle (h-alocacao) then do:
                delete procedure h-alocacao.
                assign h-alocacao = ?.
            end.

            if valid-handle(bo-ped-venda) then do:
                run destroyBo in bo-ped-venda.

                if valid-handle(bo-ped-venda) then do:
                    delete procedure bo-ped-venda.
                    assign bo-ped-venda = ?.
                end.
            end.
        end.

        run pi-efetiva-Pedido-novo.
        
    end.
    release ped-venda.
    release ped-item.
    release ped-ent.
    release dw-ped-venda.

end procedure.

procedure pi-efetiva-Pedido-novo:

    if not valid-handle(bo-ped-venda-com) then
        run dibo/bodi159com.p persistent set bo-ped-venda-com.

    run completeOrder in bo-ped-venda-com(input  rowid(ped-venda),
                                          output table RowErrors).

    run destroyBO in bo-ped-venda-com.
    delete procedure bo-ped-venda-com.
    assign bo-ped-venda-com = ?.

    if can-find(first RowErrors where 
                      RowErrors.ErrorSubType = "ERROR":U ) then do:
        for each RowErrors where 
                 RowErrors.ErrorSubType = "ERROR":U:
            create tt-erro-copia.
            assign tt-erro-copia.cod-emitente     = ped-venda.cod-emitente 
                   tt-erro-copia.nr-pedcli        = ped-venda.nr-pedcli  
                   tt-erro-copia.importou         = yes
                   tt-erro-copia.errorNumber      = RowErrors.ErrorNumber
                   tt-erro-copia.errorDescription = RowErrors.ErrorDescription. 
        end.
    
        return "NOK":U.
    end.
    
    return "OK".

end procedure.

procedure pi-gera-resposta :
    def var c-msg-aux  as char  no-undo.

    create x-document hdocxml.
    
    run reset in hgenxml.
    run setEncoding in hgenxml ("utf-8").
    
    run addnode in hgenxml (0,
                            "Resposta_Pedido",
                            "",
                            output iid_1).

    assign crm-web-processados.dt-integra  = today
           crm-web-processados.hr-integra  = replace(string(time,"HH:MM:SS"),":","").
    
    if l-arquivo-valido then do:
        find first tt-erro-copia no-error.
        if avail tt-erro-copia then do:

            run addnode in hgenxml (iid_1,
                                    "cod_emitente",
                                    tt-erro-copia.cod-emitente,
                                    output iid_2).

            run addnode in hgenxml (iid_1,
                                    "nr_pedcli",
                                    tt-erro-copia.nr-pedcli,
                                    output iid_2).

            run addnode in hgenxml (iid_1,
                                    "importou",
                                    (if tt-erro-copia.importou then 'yes' else 'no'),
                                    output iid_2).

            assign c-msg-aux                       = "Pedido Criado mas n�o completado - " + trim(replace(replace(tt-erro-copia.errorDescription,chr(13)," "),chr(10),""))
                   crm-web-processados.retorno-web = c-msg-aux
                   crm-web-processados.log-erro    = tt-erro-copia.importou.

            run addnode in hgenxml (iid_1,
                                    "mensagem",
                                    c-msg-aux,
                                    output iid_2).

            if can-find(first tt-log-ped-item) then do:
                run addnode in hgenxml (iid_2,
                                        "Itens",
                                        "",
                                        output iid_3).

                for each tt-log-ped-item:

                    run addnode in hgenxml (iid_3,
                                            "Item",
                                            "",
                                            output iid_4).

                    run addnode in hgenxml (iid_4,
                                            "sequencia",
                                            tt-log-ped-item.nr-sequencia,
                                            output iid_5).

                    run addnode in hgenxml (iid_4,
                                            "it_codigo",
                                            tt-log-ped-item.it-codigo,
                                            output iid_5).

                    run addnode in hgenxml (iid_4,
                                            "importou",
                                            (if tt-log-ped-item.importou then 'yes' else 'no'),
                                            output iid_5).

                    assign c-msg-aux = replace(replace(tt-log-ped-item.mensagem,chr(13)," "),chr(10),"").

                    run addnode in hgenxml (iid_4,
                                            "mensagem",
                                            c-msg-aux,
                                            output iid_5).
                end.
            end.
        end.
        else do:
            find first tt-log-ped-venda no-error.
            if avail tt-log-ped-venda then do:
                find emitente no-lock where 
                     emitente.nome-abrev = tt-log-ped-venda.nome-abrev no-error.

                run addnode in hgenxml (iid_1,
                                        "cod_emitente",
                                        (if avail emitente then emitente.cod-emitente else int(tt-arq-ped.cod-emitente)),
                                        output iid_2).
    
                run addnode in hgenxml (iid_1,
                                        "nr_pedcli",
                                        tt-log-ped-venda.nr-pedcli,
                                        output iid_2).
    
                run addnode in hgenxml (iid_1,
                                        "importou",
                                        (if tt-log-ped-venda.importou then 'yes' else 'no'),
                                        output iid_2).
    
                assign c-msg-aux                       = replace(replace(tt-log-ped-venda.mensagem,chr(13)," "),chr(10),"")
                       crm-web-processados.retorno-web = c-msg-aux
                       crm-web-processados.log-erro    = tt-log-ped-venda.importou.
    
                run addnode in hgenxml (iid_1,
                                        "mensagem",
                                        c-msg-aux,
                                        output iid_2).
    
                if can-find(first tt-log-ped-item) then do:
                    run addnode in hgenxml (iid_2,
                                            "Itens",
                                            "",
                                            output iid_3).
    
                    for each tt-log-ped-item:
    
                        run addnode in hgenxml (iid_3,
                                                "Item",
                                                "",
                                                output iid_4).
    
                        run addnode in hgenxml (iid_4,
                                                "sequencia",
                                                tt-log-ped-item.nr-sequencia,
                                                output iid_5).
    
                        run addnode in hgenxml (iid_4,
                                                "it_codigo",
                                                tt-log-ped-item.it-codigo,
                                                output iid_5).
    
                        run addnode in hgenxml (iid_4,
                                                "importou",
                                                (if tt-log-ped-item.importou then 'yes' else 'no',
                                                output iid_5).
    
                        assign c-msg-aux = replace(replace(tt-log-ped-item.mensagem,chr(13)," "),chr(10),"").
    
                        run addnode in hgenxml (iid_4,
                                                "mensagem",
                                                c-msg-aux,
                                                output iid_5).
                    end.
                end.
            end.
        end.
    end.
    else do:
        find first tt-arq-ped no-error.
        
        find emitente no-lock where 
             emitente.cod-emitente = int(tt-arq-ped.cod-emitente) no-error.

        create tt-log-ped-venda.
        assign tt-log-ped-venda.nome-abrev = (if avail emitente then emitente.nome-abrev else "")
               tt-log-ped-venda.nr-pedcli  = (if avail tt-arq-ped then tt-arq-ped.nr-pedcli else "")
               tt-log-ped-venda.importou   = no
               tt-log-ped-venda.mensagem   = "ERRO: N�o foi possivel importar o aquivo, pois o mesmo est� corrompido ***.".

        run addnode in hgenxml (iid_1,
                                "cod_emitente",
                                (if avail tt-arq-ped then int(tt-arq-ped.cod-emitente) else 0),
                                output iid_2).
    
        run addnode in hgenxml (iid_1,
                                "nr_pedcli",
                                (if avail tt-arq-ped then tt-arq-ped.nr-pedcli else ""),
                                output iid_2).
    
        run addnode in hgenxml (iid_1,
                                "importou",
                                'no',
                                output iid_2).
    
        assign c-msg-aux                       = "ERRO: N�o foi possivel importar o aquivo, pois o mesmo est� corrompido ***."
               crm-web-processados.retorno-web = c-msg-aux
               crm-web-processados.log-erro    = tt-log-ped-venda.importou.
    
        run addnode in hgenxml (iid_1,
                                "mensagem",
                                c-msg-aux,
                                output iid_2).
    end.

    run generatexml in hgenxml (output hdocxml).
    
    hdocxml:save ("longchar", p-xml-ret).
    
    delete object hdocxml   no-error.

end procedure.