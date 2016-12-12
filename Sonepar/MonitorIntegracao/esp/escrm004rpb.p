/******************************************************************************
** Programa: ES000-EMBIMPR.p
** Objetivo: Imprime o Embarque, utilizando o RP da Datasul 
** Autor...: Rafael Moscatelli - Datasul Campinas
** Data....: 30/03/2004
**
******************************************************************************
** Aleracao:    30/03/2004 - Rafael Moscatelli - Datasul Campinas
**                           Implantacao Inicial
******************************************************************************/

/*** Parametros Recebidos ****************************************************/
DEFINE INPUT PARAMETER p-embarque LIKE embarque.cdd-embarq NO-UNDO.

/*** Definicao ***************************************************************/
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR FORMAT "x(12)" NO-UNDO.

{ftp/ft1008.i3}
{cdp/cdcfgdis.i}

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
    field i-embarque-fim   as int format ">>>>,>>9"
    field c-preparador     as char format "x(12)"
    field i-embarque-ini   as int format ">>>>,>>9"
    field l-imp-embal      as logical format "Sim/NÆo"
    field l-nome-abrev     as logical format "Abreviado/Completo"
    field i-nr-resumo-ini  like pre-fatur.nr-resumo
    field i-nr-resumo-fim  like pre-fatur.nr-resumo
    field l-modo           as logical format "Sim/NÆo"
    &if '{&bf_dis_versao_ems}' >= '2.04' &then
    field i-narrativa      as integer format "9"
    field l-imp-narrat     as logical format "Sim/NÆo"
    &endif.
          
def temp-table tt-raw-digita
    fields raw-digita as raw.

{include/tt-edit.i}
          
/*** Inicia a Verificacao ****************************************************/
FIND FIRST crm-param NO-LOCK NO-ERROR.
IF NOT AVAIL crm-param THEN RETURN.

CREATE tt-param.
ASSIGN tt-param.destino         = 1
       tt-param.arquivo         = crm-param.imp-embarque
       tt-param.usuario         = c-seg-usuario
       tt-param.data-exec       = TODAY
       tt-param.hora-exec       = TIME
       tt-param.i-embarque-ini  = p-embarque
       tt-param.i-embarque-fim  = p-embarque
       tt-param.i-nr-resumo-ini = 0
       tt-param.i-nr-resumo-fim = 999999999
       tt-param.c-cli-ini       = ""
       tt-param.c-cli-fim       = "ZZZZZZZZZZZZ"
       tt-param.c-preparador    = CAPS(c-seg-usuario)
       tt-param.l-imp-embal     = NO 
       tt-param.l-nome-abrev    = YES 
       tt-param.l-modo          = YES
       tt-param.l-imp-narrat    = NO
       tt-param.i-narrativa     = 1.

/*** Executa *****************************************************************/

Define Variable r-raw As Raw.

Raw-transfer tt-param To r-raw.

Run ftp/ft1009rp.p (input r-raw,
                    Input Table tt-raw-digita).
