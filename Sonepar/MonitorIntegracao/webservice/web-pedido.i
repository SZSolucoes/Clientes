def temp-table tt-ped-venda  no-undo like ped-venda
    field r-rowid as rowid.

def temp-table tt-ped-item   no-undo like ped-item
    field r-rowid as rowid.

def new shared temp-table RowErrors no-undo
    field errorSequence         as int
    field errorNumber           as int
    field errorDescription      as char
    field errorParameters       as char
    field errorType             as char
    field errorHelp             as char
    field errorsubtype          as char.

def temp-table tt-arq-ped no-undo
    field cod-estabel     like ped-venda.cod-estabel    
    field cod-emitente    like ped-venda.cod-emitente   
    field nr-pedcli       like ped-venda.nr-pedcli      
    field emergencial     as char
    field dt-entrega      like ped-venda.dt-entrega     
    field cod-cond-pag    like ped-venda.cod-cond-pag   
    field nr-tabpre       like ped-venda.nr-tabpre      
    field cod-priori      like ped-venda.cod-priori     
    field observacoes     like ped-venda.observacoes    
    field nome-transp     like ped-venda.nome-transp    
    field tp-preco        like ped-venda.tp-preco       
    field mo-codigo       like ped-venda.mo-codigo      
    field no-ab-reppri    like ped-venda.no-ab-reppri   
    field vl-desconto     like ped-venda.vl-desconto    
    field nome-ab-rep     like ped-repre.nome-ab-rep
    field cod-entrega     like ped-venda.cod-entrega    
    field tipo-fatur      as char
    field nat-operacao    like ped-venda.nat-operacao   
    field dt-min-fat      as char
    field transp-redesp   like transporte.nome-abrev
    field nr-pedrep       like ped-venda.nr-pedrep      
    field cidade-cif      like ped-venda.cidade-cif     
    field cod-canal-venda like ped-venda.cod-canal-venda
    field e-mail          like ped-venda.e-mail         
    field nome-abrev-tri  like ped-venda.nome-abrev-tri 
    field cod-entrega-tri like ped-venda.cod-entrega-tri
    field cod-unid-negoc  like ped-venda.cod-unid-negoc 
    field cod-des-merc    like ped-venda.cod-des-merc.

def temp-table tt-arq-ite no-undo
    field cod-emitente     like ped-venda.cod-emitente    
    field nr-pedcli        like ped-item.nr-pedcli       
    field nr-sequencia     like ped-item.nr-sequencia    
    field it-codigo        like ped-item.it-codigo       
    field qt-pedida        like ped-item.qt-pedida       
    field vl-preori        like ped-item.vl-preori       
    field nr-tabpre        like ped-item.nr-tabpre       
    field tp-preco         like ped-item.tp-preco        
    field vl-total         as char
    field perc-icms        as char
    field obs-item-ped     like ped-item.observacao    
    field nat-operacao     like ped-item.nat-operacao    
    field cod-estabel      like ped-venda.cod-estabel     
    field dt-entrega       like ped-item.dt-entrega      
    field ind-icm-ret      as char    
    field lanc-minimo      as char
    field lanc-obrigato    as char
    field ped-compr        as char
    field seq-ped-compr    as char
    field cod-unid-negoc   like ped-venda.cod-unid-negoc 
    field per-st-icm       as char
    field per-icm-estad-st as char.

def temp-table tt-arq-lote no-undo
    field nome-abrev    like ped-item.nome-abrev 
    field nr-pedcli     like ped-item.nr-pedcli  
    field nr-sequencia  like ped-item.nr-sequenci
    field it-codigo     like ped-item.it-codigo  
    field cod-refer     like ped-item.cod-refer  
    field codigo        as char
    field cod-depos     as char
    field cod-localiz   as char
    field quantidade    as dec
    field dt-validade   as date.

def temp-table tt-log-ped-venda no-undo
    field nome-abrev            like ped-item.nome-abrev
    field nr-pedcli             like ped-item.nr-pedcli
    field importou              as   logical format "Sim/Nao" init "Nao"
    field data-import           as   date format "99/99/9999"
    field hora-import           as   char format "x(10)" 
    field cd-erro               as   int
    field mensagem              as   char format "x(300)"
    field cod-estabel           like estabelec.cod-estabel   
    field emergencial           as   integer
    field tipo-transacao        as   integer
    field dt-implant            as   date
    field dt-entrega            as   date
    field nat-operacao          as   char
    field cod-cond-pag          as   integer
    field nr-tabpre             as   char
    field cod-priori            as   integer
    field perc-desco1           as   decimal
    field perc-desco2           as   decimal
    field observacoes           as   char                                          
    field nome-transp           as   char
    field tp-preco              as   integer
    field mo-codigo             as   integer
    field no-ab-reppri          as   char
    field vl-desconto           as   decimal
    field nome-ab-rep           as   char
    field cod-entrega           as   char
    field tipo-fatur            as   integer
    field dt-min-fat            as   date
    field transp-redesp         as   char
    field nr-pedrep             as   char
    field cidade-cif            as   char
    index ch-pedido is primary
          nome-abrev
          nr-pedcli.

def temp-table tt-log-ped-item  no-undo
    field nome-abrev            like ped-item.nome-abrev
    field nr-pedcli             like ped-item.nr-pedcli
    field nr-sequencia          like ped-item.nr-sequencia
    field it-codigo             like ped-item.it-codigo
    field cod-refer             like ped-item.cod-refer
    field importou              as   logical  format "Sim/Nao" init "Nao"
    field cd-erro               as   int
    field mensagem              as   char format "x(300)"
    field cod-estabel           like estabelec.cod-estabel          
    field tipo-transacao        as   integer
    field nat-operacao          like natur-oper.nat-operacao         
    field qt-pedida             as   decimal
    field vl-preori             as   decimal
    field vl-preuni             as   decimal
    field per-des-item          as   decimal
    field nr-tabpre             as   char
    field tp-preco              as   integer
    field vl-total              as   decimal
    field perc-icms             as   decimal
    field obs-item-ped          as   char
    index ch-item-ped is primary
          nome-abrev
          nr-pedcli
          nr-sequencia
          it-codigo
          cod-refer.

