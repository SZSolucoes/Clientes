def temp-table tt-ped-venda  no-undo like ped-venda
    field r-rowid as rowid.

def temp-table tt-ped-item   no-undo like ped-item
    field r-rowid as rowid.

def new shared temp-table RowErrors no-undo
    field errorSequence         as int
    field errorNumber           as int
    field errorDescription      as char
    field errorPARAMeters       as char
    field errorType             as char
    field errorHelp             as char
    field errorsubtype          as char.

def temp-table tt-erro-copia
    field cod-emitente          like ped-venda.cod-emitente
    field nr-pedcli             like ped-item.nr-pedcli
    field importou              as log format "Sim/Nao" init "Nao"
    field errorNumber           as int
    field errorDescription      as char.

def temp-table tt-log-ped-venda no-undo
    field nome-abrev            like ped-item.nome-abrev
    field nr-pedcli             like ped-item.nr-pedcli
    field importou              as   log  format "Sim/Nao" init "Nao"
    field data-import           as   date format "99/99/9999"
    field hora-import           as   char format "x(10)" 
    field cd-erro               as   int
    field mensagem              as   char format "x(300)"
    field cod-estabel           like estabelec.cod-estabel   
    field emergencial           as   int
    field tipo-transacao        as   int
    field dt-implant            as   date
    field dt-entrega            as   date
    field nat-operacao          as   char
    field cod-cond-pag          as   int
    field nr-tabpre             as   char
    field cod-priori            as   int
    field perc-desco1           as   dec
    field perc-desco2           as   dec
    field observacoes           as   char                                          
    field nome-transp           as   char
    field tb-preco              as   int
    field mo-codigo             as   int
    field no-ab-reppri          as   char
    field vl-desconto           as   dec
    field nome-ab-rep           as   char
    field cod-entrega           as   char
    field tipo-fatur            as   int
    field dt-min-fat            as   date
    field transp-redesp         as   char
    field nr-pedrep             as   char
    field cidade-cif            as   char
    field cod-priori-log        as   int
    field modal-doc-fiscal      as   char
    index ch-Pedido is primary
          nome-abrev
          nr-pedcli.

def temp-table tt-log-ped-item  no-undo
    field nome-abrev            like ped-item.nome-abrev
    field nr-pedcli             like ped-item.nr-pedcli
    field nr-sequencia          like ped-item.nr-sequencia
    field it-codigo             like ped-item.it-codigo
    field cod-refer             like ped-item.cod-refer
    field importou              as   log  format "Sim/Nao" init "Nao"
    field cd-erro               as   int
    field mensagem              as   char format "x(300)"
    field cod-estabel           like estabelec.cod-estabel          
    field tipo-transacao        as   int
    field nat-operacao          like natur-oper.nat-operacao         
    field qt-pedida             as   dec
    field vl-preori             as   dec
    field vl-preuni             as   dec
    field per-des-item          as   dec
    field nr-tabpre             as   char
    field tb-preco              as   int
    field vl-total              as   dec
    field perc-icms             as   dec
    field obs-item-ped          as   char
    index ch-item-ped is primary
          nome-abrev
          nr-pedcli
          nr-sequencia
          it-codigo
          cod-refer.

def temp-table tt-arq-ped no-undo
    field cod-estabel      as char
    field cod-emitente     as char
    field nr-pedcli        as char
    field emergencial      as char
    field dt-entrega       as char
    field cod-cond-pag     as char
    field nr-tabpre        as char
    field cod-priori       as char
    field observacoes      as char
    field nome-transp      as char
    field tb-preco         as char
    field mo-codigo        as char
    field no-ab-reppri     as char
    field vl-desconto      as char
    field nome-ab-rep      as char
    field cod-entrega      as char
    field tipo-fatur       as char
    field nat-operacao     as char
    field dt-min-fat       as char
    field transp-redesp    as char
    field nr-pedrep        as char
    field cidade-cif       as char
    field cod-canal-venda  as char
    field e-mail           as char
    field nome-abrev-tri   as char
    field cod-entrega-tri  as char
    field cod-unid-negoc   as char
    field cod-des-merc     as char
    field cod-priori-log   as char
    field modal-doc-fiscal as char.

def temp-table tt-arq-ite no-undo
    field cod-emitente     as char
    field nr-pedcli        as char
    field nr-sequencia     as char
    field it-codigo        as char
    field qt-pedida        as char
    field vl-preori        as char
    field nr-tabpre        as char
    field tb-preco         as char
    field vl-total         as char
    field perc-icms        as char
    field obs-item-ped     as char
    field nat-operacao     as char
    field cod-estabel      as char
    field dt-entrega       as char
    field ind-icm-ret      as char
    field lanc_minimo      as char
    field lanc_obrigato    as char       
    field ped_compr        as char
    field seq_ped_compr    as char
    field cod-unid-negoc   as char
    field per-st-icm       as char
    field per-icm-estad-st as char.

def temp-table tt-arq-lote
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

def temp-table tt-arq-lote-copia
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
