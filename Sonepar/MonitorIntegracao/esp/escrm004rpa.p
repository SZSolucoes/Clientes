/******************************************************************************
** Programa: ES000-EMBARQUE.P
** Objetivo: Gera Automaticamente os Embarques para o Pedido
** Autor...: Rafael Moscatelli - Datasul Campinas
** Data....: 29/03/2004
**
******************************************************************************
** Aleracao:    30/03/2004 - Rafael Moscatelli - Datasul Campinas
**                           Implantacao Inicial
**              07/04/2004 - Rafael Moscatelli - Datasul Campinas ( na minha casa )
**                           Retirado o FIND est-ped-venda quando embarque tiver 
**                           sido Gerado  
**              12/04/2004 - Rafael Moscatelli - Datasul Campinas
**                           Incluido para gerar Embarque somente com 
**                           Data de Entrega  Inferior a Data Atual + 30, e
**                           retirado o est-ped-venda onde agora e retornado e 
**                           gravado no est-ped-venda pois ocorria o erro: 4407
******************************************************************************/

/*** Parametros Recebidos para Criar o Embarque *****************************/
DEFINE INPUT  PARAMETER p-nome-abrev AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER p-nr-pedcli  AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER p-embarque   AS INTEGER    NO-UNDO. /* 1 - Gera, 2 - Emb. Gerado, 3 - Nao Gera */

/*** Definicao de Temp Tables da APIFT3000 ***********************************/
def temp-table tt-embarque no-undo like embarque use-index ch-emb
    field i-sequen as int
    field ind-oper as int. /* 1 - InclusÆo
                              2 - Altera‡Æo
                              3 - Elimina‡Æo */

def temp-table tt-ped-venda no-undo
    field i-sequen    as int
    field cdd-embarq  like embarque.cdd-embarq
    field nome-abrev  like ped-venda.nome-abrev
    field nr-pedcli   like ped-venda.nr-pedcli
    field ind-oper    as int /* 1 - Alocar
                                2 - Desalocar */

    index ch-pedido is primary
        nome-abrev
        nr-pedcli.

def temp-table tt-ped-ent no-undo
    field nome-abrev   like ped-ent.nome-abrev
    field nr-pedcli    like ped-ent.nr-pedcli
    field nr-sequencia like ped-ent.nr-sequencia
    field it-codigo    like ped-ent.it-codigo
    field cod-refer    like ped-ent.cod-refer
    field qt-alocada   like ped-ent.qt-alocada
    field qt-log-aloca like ped-ent.qt-log-aloca
    field qt-pedida    like ped-ent.qt-pedida 
    field qt-atendida  like ped-ent.qt-atendida
    field dt-entrega   like ped-ent.dt-entrega
    field nr-entrega   like ped-ent.nr-entrega 
    field hr-entrega   like ped-ent.hr-entrega    
    field nr-proc-exp  like ped-ent.nr-proc-exp
    field num-sequencia-bonif like ped-item.num-sequencia-bonif    
    field nat-operacao    like ped-item.nat-operacao
    
    field dec-1           like ped-ent.dec-1
    field qt-a-alocar     as decimal
    field i-sequen        as int
    field cdd-embarq      as dec
    field nr-seq-registro as integer init 1
    
    index ch-ent is primary    
        nr-proc-exp
        nome-abrev
        nr-pedcli
        nr-sequencia
        it-codigo
        cod-refer         
        nr-entrega.
        
def temp-table tt-it-pre-fat no-undo
    field cdd-embarq   as DEC
    field nr-resumo    as int
    field nome-abrev   as char
    field nr-pedcli    as char
    field nr-sequencia as int
    field it-codigo    as char
    field qt-a-alocar  as deci
    field i-sequen     as int
    field nr-entrega   as int
    index ch-it-pre-fat is primary
        cdd-embarq
        nr-resumo
        nome-abrev
        nr-pedcli
        nr-sequencia
        it-codigo
        nr-entrega.

def temp-table tt-deposito no-undo
    field sequen as int
    field cod-depos like deposito.cod-depos
    index ch-aeq-dep is primary unique
        sequen.

def temp-table tt-it-narrativa no-undo
    field nome-abrev   as char
    field nr-pedcli    as char
    field nr-sequencia as int
    field it-codigo    as char
    field cod-refer    as char
    field nr-entrega   as int
    field cdd-embarq   as dec
    field nr-resumo    as int
    field narrativa    as char.

def temp-table tt-item-filho no-undo
    field nome-abrev   as char
    field nr-pedcli    as char
    field nr-sequencia as int
    field it-codigo    as char
    field cod-refer    as char
    field nr-entrega   as int
    field qt-a-alocar  as deci
    index ch-entrega is primary
        nome-abrev
        nr-pedcli
        nr-sequencia
        it-codigo
        cod-refer
        nr-entrega.

DEF TEMP-TABLE tt-erro NO-UNDO
   FIELD i-sequen      AS INT
   FIELD cd-erro       AS INT
   FIELD mensagem      AS CHAR
   FIELD parametro     AS CHAR.

DEF VAR h-api AS HANDLE NO-UNDO.

/*** Definicao de Variaveis e Temp Tables Internas ***************************/
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR FORMAT "x(12)" NO-UNDO.

DEF VAR de-cdd-embarq   LIKE embarque.cdd-embarq   NO-UNDO.
DEF VAR i-sequen        AS   INT                    NO-UNDO.
DEF VAR c-diretorio     AS   CHAR FORMAT "x(40)"    NO-UNDO.
DEF VAR l-erros-api     AS   LOGICAL                NO-UNDO.
DEF VAR l-existe-ent    AS   LOGICAL                NO-UNDO.

/*** Definicao de Stream **************************************************/
DEF STREAM str-rp.

/*** Deposito em poder de Terceiros *****************************************/
DEF TEMP-TABLE tt-depter
    FIELD cod-depos LIKE deposito.cod-depos.

DO TRANSACTION:

    FOR EACH tt-depter:
        DELETE tt-depter.
    END.
    RUN pi-deposito-terceiro.
  
    ASSIGN p-embarque = 3. /* Nao Gera Embarque */

    /*** Verifica Parametros da Integracao ************************************/
    FIND FIRST param-integra NO-LOCK NO-ERROR.
    IF NOT AVAIL param-integra THEN RETURN.
    
    IF param-integra.embarque-autom = NO THEN RETURN. /* Embarque Manual */
    

    ASSIGN c-diretorio = param-integra.dir-arq-err + 
                                           "/Pedido-Inc - " +
                                           STRING(DAY(TODAY),"99")    + 
                                           STRING(MONTH(TODAY),"99")  + 
                                           STRING(YEAR(TODAY),"9999") + ".txt".
        
    OUTPUT STREAM str-rp TO VALUE(c-diretorio) APPEND .
    
    PUT STREAM str-rp
        SKIP(1)
        "Nome Abrev   Nr Pedcli    Embarque  Data       Hora       Mensagem                                                                                            " SKIP
        "------------ ------------ --------- ---------- ---------- ----------------------------------------------------------------------------------------------------" SKIP.
        
    /*** INICIO DO PROGRAMA ******************************************************/
    ASSIGN i-sequen    = 1.
    
    /*** Localiza os Parametros do Pedido ****************************************/
    FIND FIRST para-ped NO-LOCK NO-ERROR.
    IF NOT AVAIL para-ped THEN 
    DO:
    
        PUT STREAM str-rp
            p-nome-abrev FORMAT "x(12)" AT 01
            p-nr-pedcli  FORMAT "x(12)" AT 14
            "NAO GEROU"  AT 27
            STRING(TODAY,"99/99/9999") FORMAT "x(10)" AT 37
            STRING(TIME,"HH:MM:SS")    AT 48
            "Parametros do Pedido nao Encontrado - Param-Ped" FORMAT "x(100)" AT 59
            SKIP.
            
        OUTPUT STREAM str-rp 
               CLOSE.
    
        RETURN.
    
    END.
        
    /*** Localiza o Pedido de Venda **********************************************/
    FIND FIRST ped-venda
         WHERE ped-venda.nome-abrev = p-nome-abrev
           AND ped-venda.nr-pedcli  = p-nr-pedcli
           NO-LOCK NO-ERROR.
    IF NOT AVAIL ped-venda THEN 
    DO:    
        PUT STREAM str-rp
            p-nome-abrev FORMAT "x(12)" AT 01
            p-nr-pedcli  FORMAT "x(12)" AT 14
            "NAO GEROU"  AT 27
            STRING(TODAY,"99/99/9999") FORMAT "x(10)" AT 37
            STRING(TIME,"HH:MM:SS")    AT 48
            "Pedido nao Encontrado ( Cliente: " + 
            STRING(p-nome-abrev) + " - Pedido do Cliente: " + 
            STRING(p-nr-pedcli) + " ) " FORMAT "x(100)" AT 59
            SKIP.
    
        OUTPUT STREAM str-rp 
               CLOSE.
    
        RETURN.
    
    END.
    
    /*** Verifica se Existe Item para o Pedido ************************************/
    FIND FIRST ped-item OF ped-venda NO-LOCK NO-ERROR.
    IF NOT AVAIL ped-item THEN 
    DO:    
        PUT STREAM str-rp
            p-nome-abrev FORMAT "x(12)" AT 01
            p-nr-pedcli  FORMAT "x(12)" AT 14
            "NAO GEROU"  AT 27
            STRING(TODAY,"99/99/9999") FORMAT "x(10)" AT 37
            STRING(TIME,"HH:MM:SS")    AT 48
            "Pedido sem Itens Cadastrados ( Cliente: " + 
            STRING(p-nome-abrev) + " - Pedido do Cliente: " + 
            STRING(p-nr-pedcli) + " ) " FORMAT "x(100)" AT 59
            SKIP.
    
        OUTPUT STREAM str-rp 
               CLOSE.
    
        RETURN.
    
    END.
    
    /*** Busca o Ultimo Numero do Embarque ****************************************/
    FIND LAST embarque NO-LOCK NO-ERROR.
    IF AVAIL embarque THEN ASSIGN de-cdd-embarq = embarque.cdd-embarq + 1.
    ELSE ASSIGN de-cdd-embarq = 1.
    
    ASSIGN l-existe-ent = NO.
    
    /*** Cria os Depositos para Embarcar ******************************************/
    RUN pi-cria-deposito.

    /*** PI Cria tt-Embarque ******************************************************/
    RUN pi-cria-tt-embarque.
    
    /*** PI Cria tt-ped-venda *****************************************************/
    RUN pi-cria-tt-ped-venda.
    
    FOR EACH ped-item OF ped-venda NO-LOCK:
        IF ped-item.cod-sit-item = 6 THEN NEXT.  /* Cancelado */
    
        FOR EACH ped-ent OF ped-item NO-LOCK:
            IF ped-ent.dt-entrega >= (TODAY + param-integra.dias-ent-futur) THEN NEXT.
            
            ASSIGN l-existe-ent = YES.
    
            /*** PI Cria tt-Ped-Ent *******************************************************/
            RUN pi-cria-tt-ped-ent.
              
        END.
    
    END.
    
    IF l-existe-ent = NO THEN
    DO:
    
        PUT STREAM str-rp
            p-nome-abrev FORMAT "x(12)" AT 01
            p-nr-pedcli  FORMAT "x(12)" AT 14
            "NAO GEROU"  AT 27
            STRING(TODAY,"99/99/9999") FORMAT "x(10)" AT 37
            STRING(TIME,"HH:MM:SS")    AT 48
            "Itens do Pedido sem Entregas Amarradas ( Cliente: " + 
            STRING(p-nome-abrev) + " - Pedido do Cliente: " + 
            STRING(p-nr-pedcli) + " )" FORMAT "x(100)" AT 59
            SKIP.
    
        OUTPUT STREAM str-rp 
               CLOSE.
    
        RETURN.
    
    END.
    
    /*** Executa a API de Forma Persistent ****************************************/
    RUN ftp/ftapi300.p PERSISTENT SET h-api.
    
    /*** l-erros-api: 
    **** Indica a Cada processo realizaro se na houve erros *********/
    IF l-erros-api = NO THEN
    DO: 
        /*** Cria o Embarque **********************************************************/
        RUN pi-embarque(OUTPUT l-erros-api).
    
    END.
    IF l-erros-api = NO THEN
    DO: 
    
        IF ped-venda.ind-fat-par = YES THEN
        DO:
            /*** Cria os Pedidos no Embarque **********************************************/
            RUN pi-embarque-pedidos(OUTPUT l-erros-api).
        END.
        ELSE
        DO:
            /*** Cria os Pedidos no Embarque **********************************************/
            RUN pi-embarque-pedidos-ped-venda(OUTPUT l-erros-api).
        END.
        
    END. 
    IF l-erros-api = NO THEN
    DO:
    
        /*** Encerra a API ( Realizando os Processamentos Finais ) ********************/
        RUN pi-encerra-embarque     IN h-api (INPUT TRUE).
        RUN pi-devolve-tt-erro      IN h-api (OUTPUT TABLE tt-erro).
        
        FOR EACH tt-erro NO-LOCK:
        
             PUT STREAM str-rp
                 p-nome-abrev FORMAT "x(12)" AT 01
                 p-nr-pedcli  FORMAT "x(12)" AT 14
                 de-cdd-embarq AT 27
                 STRING(TODAY,"99/99/9999") FORMAT "x(10)" AT 37
                 STRING(TIME,"HH:MM:SS")    AT 48
                 "Nao e Possivel Encerrar o Embarque: " + 
                 tt-erro.mensagem + " - " + STRING(cd-erro) FORMAT "x(100)" AT 59
                 SKIP.
                    
        END.
    
    END.
    
    /*** Finaliza a API de FTAPI300 ***********************************************/
    RUN pi-finalizar IN h-api.
    
    /*** Deixa o Embarque Pronto para Faturamento ****************************/
    FIND FIRST embarque 
         WHERE embarque.cdd-embarq = de-cdd-embarq
         NO-LOCK NO-ERROR.
    
    IF AVAIL embarque THEN
    DO:
        FOR EACH pre-fatur OF embarque:
    
            ASSIGN pre-fatur.ind-sit-embarque = 1 . /*1-Livre, 2-Em Uso, 3-Calculando*/
    
        END.
    
        ASSIGN p-embarque = 2. /* Embarque Gerado */
           
        PUT STREAM str-rp
            p-nome-abrev FORMAT "x(12)" AT 01
            p-nr-pedcli  FORMAT "x(12)" AT 14
            de-cdd-embarq AT 27
            STRING(TODAY,"99/99/9999") FORMAT "x(10)" AT 37
            STRING(TIME,"HH:MM:SS")    AT 48
            "Embarque pronto para ser Faturado" FORMAT "x(100)" AT 59
            SKIP.
    
        /*** IMPRIME O EMBARQUE *************************************/
        /*RUN esp/escrm004rpb.p (INPUT de-cdd-embarq).*/
    
    END.
    
    OUTPUT STREAM str-rp 
           CLOSE.

END.

/*** FIM DO PROGRAMA **********************************************************/


/*** PROCEDURES INTERNAS ******************************************************/

/*** Depositos para Embarcar ***/
PROCEDURE pi-cria-deposito:
    DEF VAR i-sequen AS INT NO-UNDO.
                                   
    ASSIGN i-sequen = 0.
    FOR EACH deposito NO-LOCK:
        IF deposito.ind-acabado = NO THEN NEXT.

        FIND FIRST tt-depter 
             WHERE tt-depter.cod-depos = deposito.cod-depos
             NO-LOCK NO-ERROR.
        IF AVAIL tt-depter THEN NEXT.

        ASSIGN i-sequen = i-sequen + 1.

        CREATE tt-deposito.
        ASSIGN tt-deposito.sequen    = i-sequen.
               tt-deposito.cod-depos = deposito.cod-depos.

    END.

END PROCEDURE.

PROCEDURE pi-cria-tt-embarque:

    /*** Cria a tt-embarque ***************************************************/
    CREATE tt-embarque.
    ASSIGN tt-embarque.cdd-embarq      = de-cdd-embarq          
           tt-embarque.usuario          = c-seg-usuario    
           tt-embarque.dt-embarque      = TODAY         
           tt-embarque.cod-tipo         = "" /* tipo do embarque */    
           tt-embarque.cod-rota         = ""           
           tt-embarque.nome-transp      = ped-venda.nome-transp    
           tt-embarque.placa            = "" /* placa do veiculo */    
           tt-embarque.cod-estabel      = para-ped.estab-padrao         
           tt-embarque.situacao         = 1 /* 1-alocado, 2-calculado, 3-confirmado*/    
           tt-embarque.data-pick[1]     = ?    
           tt-embarque.data-pick[2]     = ?    
           tt-embarque.hora-pick[1]     = ""
           tt-embarque.hora-pick[2]     = ""
           tt-embarque.user-pick[1]     = ""    
           tt-embarque.user-pick[2]     = ""    
           tt-embarque.ind-dimen        = NO    
           tt-embarque.peso-bru-tot     = 0    
           tt-embarque.peso-liq-tot     = 0    
           tt-embarque.volume           = 0    
           tt-embarque.identific        = c-seg-usuario         
           tt-embarque.cod-dep-ext      = ""    
           tt-embarque.char-1           = ""    
           tt-embarque.char-2           = ""    
           tt-embarque.dec-1            = 0    
           tt-embarque.dec-2            = 0    
           tt-embarque.int-1            = 0    
           tt-embarque.int-2            = 0    
           tt-embarque.log-1            = NO    
           tt-embarque.log-2            = NO    
           tt-embarque.data-1           = ?    
           tt-embarque.data-2           = ?    
           tt-embarque.cod-canal-venda  = 0   
           tt-embarque.motorista        = "" /* nome do motorista */            
           tt-embarque.nat-operacao     = ""    
           tt-embarque.check-sum        = ""

           /*** Controle da API *****************************************************/

           tt-embarque.i-sequen         = i-sequen
           tt-embarque.ind-oper         = 1. /*1-Inclusao, 2-Alteracao, 3-Eliminacao */

END PROCEDURE.

PROCEDURE pi-cria-tt-ped-ent:

    CREATE tt-ped-ent.
    ASSIGN tt-ped-ent.nome-abrev   = ped-ent.nome-abrev
           tt-ped-ent.nr-pedcli    = ped-ent.nr-pedcli
           tt-ped-ent.nr-sequencia = ped-ent.nr-sequencia
           tt-ped-ent.it-codigo    = ped-ent.it-codigo
           tt-ped-ent.cod-refer    = ped-ent.cod-refer
           tt-ped-ent.nr-entrega   = ped-ent.nr-entrega
           tt-ped-ent.qt-a-alocar  = ped-ent.qt-pedida
           tt-ped-ent.i-sequen     = i-sequen
           tt-ped-ent.cdd-embarq   = de-cdd-embarq.

END PROCEDURE.

PROCEDURE pi-cria-tt-ped-venda:

    CREATE tt-ped-venda.
    ASSIGN tt-ped-venda.i-sequen     = i-sequen
           tt-ped-venda.cdd-embarq   = de-cdd-embarq
           tt-ped-venda.nome-abrev   = ped-venda.nome-abrev
           tt-ped-venda.nr-pedcli    = ped-venda.nr-pedcli
           tt-ped-venda.ind-oper     = 1 . /* Aloca */

END PROCEDURE.


PROCEDURE pi-elimina-embarque:

    FIND FIRST tt-embarque NO-ERROR.
    IF AVAIL tt-embarque THEN
        ASSIGN tt-embarque.ind-oper = 3.

END PROCEDURE.


PROCEDURE pi-embarque:
    DEFINE OUTPUT PARAMETER p-erro AS LOGICAL    NO-UNDO.

    DEF VAR l-houve-erros AS LOGICAL NO-UNDO.

    ASSIGN p-erro        = NO
           l-houve-erros = YES.


    /*** Passa os Depositos que seram Utilizado para Busca de SAldo dos Itens ****/
    RUN pi-recebe-tt-deposito IN h-api(INPUT TABLE tt-deposito).

    /*****************************************************************************/
    /*** EMBARQUE ****************************************************************/
    /*****************************************************************************/
    RUN pi-recebe-tt-embarque   IN h-api (INPUT TABLE tt-embarque).
    RUN pi-trata-tt-embarque    IN h-api (INPUT "msg", INPUT TRUE).
    RUN pi-devolve-tt-erro      IN h-api (OUTPUT TABLE tt-erro).
    
    ASSIGN l-houve-erros = NO.
    FOR EACH tt-erro NO-LOCK:
        
        ASSIGN l-houve-erros = YES
               p-erro        = YES.
    
        PUT STREAM str-rp
            p-nome-abrev FORMAT "x(12)" AT 01
            p-nr-pedcli  FORMAT "x(12)" AT 14
            "NAO GEROU"  AT 27
            STRING(TODAY,"99/99/9999") FORMAT "x(10)" AT 37
            STRING(TIME,"HH:MM:SS")    AT 48
            tt-erro.mensagem  + " - " + STRING(cd-erro) FORMAT "x(100)" AT 59
            SKIP.
    
    END.
    IF l-houve-erros = NO THEN
    DO:
        PUT STREAM str-rp
            p-nome-abrev FORMAT "x(12)" AT 01
            p-nr-pedcli  FORMAT "x(12)" AT 14
            de-cdd-embarq  AT 27
            STRING(TODAY,"99/99/9999") FORMAT "x(10)" AT 37
            STRING(TIME,"HH:MM:SS")    AT 48
            "" FORMAT "x(100)" AT 59
            SKIP.
    END.
    ELSE
    DO:
        /*** ELIMINA EMBARQUE ***************************************/

        RUN pi-elimina-embarque.

        /*** Envia a tt-embarque com Parametro de Eliminar *******************/
        RUN pi-recebe-tt-embarque   IN h-api (INPUT TABLE tt-embarque).
        RUN pi-trata-tt-embarque    IN h-api (INPUT "msg", INPUT YES).
        RUN pi-devolve-tt-erro      IN h-api (OUTPUT TABLE tt-erro).

        FOR EACH tt-erro NO-LOCK:

            PUT STREAM str-rp
                p-nome-abrev FORMAT "x(12)" AT 01
                p-nr-pedcli  FORMAT "x(12)" AT 14
                de-cdd-embarq  AT 27
                STRING(TODAY,"99/99/9999") FORMAT "x(10)" AT 37
                STRING(TIME,"HH:MM:SS")    AT 48
                "Nao foi Possivel Eliminar Embarque: " + 
                tt-erro.mensagem  + " - " + STRING(cd-erro) FORMAT "x(100)" AT 59
                SKIP.

        END.

    END.

END PROCEDURE.

PROCEDURE pi-embarque-pedidos:
    DEFINE OUTPUT PARAMETER p-erro AS LOGICAL    NO-UNDO.
    
    DEF VAR l-houve-erros AS LOGICAL NO-UNDO.
    
    ASSIGN p-erro        = NO
           l-houve-erros = NO.

    /*****************************************************************************/
    /*** PEDIDOS DO EMBARQUE *****************************************************/
    /*****************************************************************************/
    RUN pi-recebe-tt-ped-ent    IN h-api (INPUT TABLE tt-ped-ent).
    RUN pi-trata-tt-ped-ent     IN h-api (INPUT TRUE).
    RUN pi-devolve-tt-erro      IN h-api (OUTPUT TABLE tt-erro).

    ASSIGN l-houve-erros = NO.
    FOR EACH tt-erro NO-LOCK:

        ASSIGN p-erro        = YES 
               l-houve-erros = YES.

        PUT STREAM str-rp
            p-nome-abrev FORMAT "x(12)" AT 01
            p-nr-pedcli  FORMAT "x(12)" AT 14
            de-cdd-embarq  AT 27
            STRING(TODAY,"99/99/9999") FORMAT "x(10)" AT 37
            STRING(TIME,"HH:MM:SS")    AT 48
            tt-erro.mensagem  + " - " + STRING(cd-erro) FORMAT "x(100)" AT 59
            SKIP.

    END.
    IF l-houve-erros = YES THEN
    DO:
        FIND FIRST embarque 
             WHERE embarque.cdd-embarq = de-cdd-embarq
             NO-LOCK NO-ERROR.
        IF AVAIL embarque THEN
        DO:

            FIND FIRST it-pre-fat OF embarque NO-LOCK NO-ERROR.
            IF NOT AVAIL it-pre-fat THEN
            DO:
                /*** Elimina o Embarque Gerado ****************************************/
                RUN pi-elimina-embarque.
        
                /*** Envia a tt-embarque com Parametro de Eliminar *******************/
                RUN pi-recebe-tt-embarque   IN h-api (INPUT TABLE tt-embarque).
                RUN pi-trata-tt-embarque    IN h-api (INPUT "msg", INPUT YES).
                RUN pi-devolve-tt-erro      IN h-api (OUTPUT TABLE tt-erro).
                
                FIND FIRST tt-erro NO-LOCK NO-ERROR.
                IF AVAIL tt-erro THEN
                DO:
                    /*** Houve ERROS na Eliminacao *********************************/
                    FOR EACH tt-erro NO-LOCK:
            
                        PUT STREAM str-rp
                            p-nome-abrev FORMAT "x(12)" AT 01
                            p-nr-pedcli  FORMAT "x(12)" AT 14
                            de-cdd-embarq  AT 27
                            STRING(TODAY,"99/99/9999") FORMAT "x(10)" AT 37
                            STRING(TIME,"HH:MM:SS")    AT 48
                            "Nao foi Possivel Eliminar Embarque: " + 
                            tt-erro.mensagem   + " - " + STRING(cd-erro) FORMAT "x(100)" AT 59
                            SKIP.
            
                    END.
                END.
                ELSE
                DO:
                    /*** Nao Houve ERROS na Eliminacao, Eliminou o Embarque *************/
                    PUT STREAM str-rp
                        p-nome-abrev FORMAT "x(12)" AT 01
                        p-nr-pedcli  FORMAT "x(12)" AT 14
                        "NAO GEROU"  AT 27
                        STRING(TODAY,"99/99/9999") FORMAT "x(10)" AT 37
                        STRING(TIME,"HH:MM:SS")    AT 48
                        "Embarque Eliminado ( " + STRING(de-cdd-embarq) + " )" FORMAT "x(100)" AT 59
                        SKIP.
                END.
               
            END.

        END.
            
    END.
    
END PROCEDURE.

PROCEDURE pi-embarque-pedidos-ped-venda:
    DEFINE OUTPUT PARAMETER p-erro AS LOGICAL    NO-UNDO.
    
    DEF VAR l-houve-erros AS LOGICAL NO-UNDO.
    
    ASSIGN p-erro        = NO
           l-houve-erros = NO.

    /*****************************************************************************/
    /*** PEDIDOS DO EMBARQUE *****************************************************/
    /*****************************************************************************/
    RUN pi-recebe-tt-ped-venda  IN h-api (INPUT TABLE tt-ped-venda).
    RUN pi-trata-tt-ped-venda   IN h-api (INPUT TRUE).
    RUN pi-devolve-tt-erro      IN h-api (OUTPUT TABLE tt-erro).

    ASSIGN l-houve-erros = NO.
    FOR EACH tt-erro NO-LOCK:

        ASSIGN p-erro        = YES 
               l-houve-erros = YES.

        PUT STREAM str-rp
            p-nome-abrev FORMAT "x(12)" AT 01
            p-nr-pedcli  FORMAT "x(12)" AT 14
            de-cdd-embarq  AT 27
            STRING(TODAY,"99/99/9999") FORMAT "x(10)" AT 37
            STRING(TIME,"HH:MM:SS")    AT 48
            tt-erro.mensagem  + " - " + STRING(cd-erro) FORMAT "x(100)" AT 59
            SKIP.

    END.
    IF l-houve-erros = YES THEN
    DO:
        FIND FIRST embarque 
             WHERE embarque.cdd-embarq = de-cdd-embarq
             NO-LOCK NO-ERROR.
        IF AVAIL embarque THEN
        DO:

            FIND FIRST it-pre-fat OF embarque NO-LOCK NO-ERROR.
            IF NOT AVAIL it-pre-fat THEN
            DO:
                /*** Elimina o Embarque Gerado ****************************************/
                RUN pi-elimina-embarque.
        
                /*** Envia a tt-embarque com Parametro de Eliminar *******************/
                RUN pi-recebe-tt-embarque   IN h-api (INPUT TABLE tt-embarque).
                RUN pi-trata-tt-embarque    IN h-api (INPUT "msg", INPUT YES).
                RUN pi-devolve-tt-erro      IN h-api (OUTPUT TABLE tt-erro).
                
                FIND FIRST tt-erro NO-LOCK NO-ERROR.
                IF AVAIL tt-erro THEN
                DO:
                    /*** Houve ERROS na Eliminacao *********************************/
                    FOR EACH tt-erro NO-LOCK:
            
                        PUT STREAM str-rp
                            p-nome-abrev FORMAT "x(12)" AT 01
                            p-nr-pedcli  FORMAT "x(12)" AT 14
                            de-cdd-embarq  AT 27
                            STRING(TODAY,"99/99/9999") FORMAT "x(10)" AT 37
                            STRING(TIME,"HH:MM:SS")    AT 48
                            "Nao foi Possivel Eliminar Embarque: " + 
                            tt-erro.mensagem  + " - " + STRING(cd-erro) FORMAT "x(100)" AT 59
                            SKIP.
            
                    END.
                END.
                ELSE
                DO:
                    /*** Nao Houve ERROS na Eliminacao, Eliminou o Embarque *************/
                    PUT STREAM str-rp
                        p-nome-abrev FORMAT "x(12)" AT 01
                        p-nr-pedcli  FORMAT "x(12)" AT 14
                        "NAO GEROU"  AT 27
                        STRING(TODAY,"99/99/9999") FORMAT "x(10)" AT 37
                        STRING(TIME,"HH:MM:SS")    AT 48
                        "Embarque Eliminado ( " + STRING(de-cdd-embarq) + " )" FORMAT "x(100)" AT 59
                        SKIP.
                END.
               
            END.

        END.
            
    END.
    
END PROCEDURE.

/*** Neste Arquivo sao Relacionados os Depositos que sao de Saldo de Terceiro
e que nao sera enviado para o CRM ***/

PROCEDURE pi-deposito-terceiro:
    DEF VAR c-linha AS CHAR FORMAT "x(03)" NO-UNDO.
    
    CREATE tt-depter.
    ASSIGN tt-depter.cod-depos = "TER".

END PROCEDURE.
