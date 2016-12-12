/****************************************************************************************************
**    Programa: twin403.p
**   Descricao: Triggers de Write Tabela Saldo de Estoque
**       Autor: Datasul Bandeirantes - Unidade Campinas
**        Data: 18/07/2006
****************************************************************************************************/

DEFINE PARAMETER BUFFER b-saldo-estoq     FOR saldo-estoq.
DEFINE PARAMETER BUFFER b-old-saldo-estoq FOR saldo-estoq.

/*********************************************************************/
/* Autor: Glaucio Dias - GTPlan               */
/* Integr Datasul - GTPlan (Planejamento)     */
/* Data: dez/14                               */
/**********************************************/

def new global shared var c-seg-usuario as char no-undo.

DEFINE VARIABLE i-empresa LIKE param-global.empresa-prin     NO-UNDO.

FIND FIRST param-global NO-LOCK NO-ERROR.
IF AVAIL param-global THEN
    ASSIGN i-empresa = param-global.empresa-prin.

DEFINE VARIABLE c-estab AS CHARACTER   NO-UNDO.
DEFINE VARIABLE l-integra-gtplan AS LOGICAL     NO-UNDO.

/*centelha*/
IF string(i-empresa) = '2' THEN
    ASSIGN c-estab = '201;202;203;204;205;208;'.

/*proex*/
IF string(i-empresa) = '4' THEN
    ASSIGN c-estab = '401'.
    
IF string(i-empresa) = '2' OR string(i-empresa) = '4' THEN DO:
    IF AVAIL b-saldo-estoq THEN DO:
    
        FIND FIRST ITEM 
             WHERE ITEM.it-codigo = b-saldo-estoq.it-codigo NO-LOCK NO-ERROR.
        IF AVAIL ITEM THEN DO:
            IF ITEM.fm-cod-com <> '000' THEN
                ASSIGN l-integra-gtplan = YES.
            ELSE 
                ASSIGN l-integra-gtplan = NO.
        END.
        ELSE ASSIGN l-integra-gtplan = NO.
    
        IF l-integra-gtplan AND b-saldo-estoq.it-codigo <> '' THEN DO:
            IF INDEX(c-estab,b-saldo-estoq.cod-estabel) > 0 AND b-saldo-estoq.it-codigo <> '' THEN DO:
            
                IF NOT CAN-FIND( FIRST gtp-fila-integr 
                             WHERE gtp-fila-integr.cod-empresa   = STRING(i-empresa)
                               AND gtp-fila-integr.cod-trans     = 2 
                               AND gtp-fila-integr.cod-chave     = b-saldo-estoq.cod-estabel + "|" + b-saldo-estoq.it-codigo
                               AND gtp-fila-integr.tipo-trans    = 1 
                               AND gtp-fila-integr.dt-hr-integr  = ? ) THEN DO:
        
                    create gtp-fila-integr.
                    assign gtp-fila-integr.cod-empresa   = string(i-empresa)
                           gtp-fila-integr.id-fila       = next-value(gtp-seq-fila)
                           gtp-fila-integr.tipo-trans    = 1
                           gtp-fila-integr.dt-hr-integr  = ?
                           gtp-fila-integr.dt-hr-criacao = now
                           gtp-fila-integr.cod-usuario   = c-seg-usuario
                           gtp-fila-integr.cod-trans     = 02
                           gtp-fila-integr.cod-chave     = b-saldo-estoq.cod-estabel + "|" + b-saldo-estoq.it-codigo.
                END.
            END.
        END.
    END.
END.


/*********************************************************************/

/* Busca Parƒmetros */
FIND FIRST param-integra NO-LOCK NO-ERROR.

/* Se NÆo encontrar parƒmetro ou nÆo habilitado integra‡Æo sair */
IF NOT AVAIL param-integra 
OR NOT param-integra.l-integra-estoque THEN RETURN "OK".

RUN twp/twes001.p (INPUT "saldo-estoq",
                   INPUT STRING(b-saldo-estoq.cod-depos,"x(3)") + STRING(b-saldo-estoq.cod-estabel,"x(3)") + 
                         STRING(b-saldo-estoq.cod-localiz,"x(20)") + STRING(b-saldo-estoq.lote,"x(40)") + 
                         STRING(b-saldo-estoq.it-codigo,"x(16)") + STRING(b-saldo-estoq.cod-refer,"x(8)"),
                   INPUT (b-old-saldo-estoq.cod-estabel = "")).


/*Integra‡Æo WebService*/
run webservice/saldo-estoq.p (input rowid(b-saldo-estoq),
                              input if avail b-old-saldo-estoq then 2 else 1).
/*Integra‡Æo WebService*/


RETURN RETURN-VALUE.
