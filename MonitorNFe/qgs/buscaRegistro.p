DEF INPUT PARAMETER pr-cod-estabel   LIKE docum-est.cod-estabel.
DEF INPUT PARAMETER pr-cod-emitente  LIKE item-doc-est.cod-emitente.
DEF INPUT PARAMETER pr-it-codigo     LIKE item-doc-est.it-codigo.
DEF OUTPUT PARAMETER pr-num-pedido   LIKE item-doc-est.num-pedido.
DEF OUTPUT PARAMETER pr-numero-ordem LIKE item-doc-est.numero-ordem.
DEF OUTPUT PARAMETER pr-parcela      LIKE item-doc-est.parcela.
DEF OUTPUT PARAMETER pr-class-fisc   AS CHARACTER FORMAT "x(12)".
DEF OUTPUT PARAMETER pr-cod-depos    LIKE item-doc-est.cod-depos.
DEF OUTPUT PARAMETER pr-cod-localiz  LIKE item-doc-est.cod-localiz.
DEF OUTPUT PARAMETER pr-lote-serie   LIKE item-doc-est.lote.
DEF OUTPUT PARAMETER pr-dt-vali-lote LIKE item-doc-est.dt-vali-lote.
DEF OUTPUT PARAMETER pr-cod-refer    LIKE item-doc-est.cod-refer.
DEF OUTPUT PARAMETER pr-log-fifo-oc  LIKE item-doc-est.log-fifo-oc.

/*
MESSAGE "API TESTE de RETORNO DADOS"   SKIP
        "Registros Recebidos"          SKIP
        "Cod-estabel:" pr-cod-estabel  SKIP
        "Emitente...:" pr-cod-emitente SKIP
        "Item.......:" pr-it-codigo    VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/
FIND ordem-compra
    WHERE ordem-compra.cod-estabel = pr-cod-estabel
      AND ordem-compra.num-pedido  = 83
      AND ordem-compra.it-codigo   = pr-it-codigo NO-LOCK NO-ERROR.
IF AVAIL ordem-compra THEN
    ASSIGN pr-num-pedido   = ordem-compra.num-pedido
           pr-numero-ordem = ordem-compra.numero-ordem
           pr-parcela      = 1
           pr-class-fisc   = "1010.10.10"
           pr-cod-depos    = "EXP"
           pr-cod-localiz  = ""
           pr-lote-serie   = "BRANCO"
           pr-dt-vali-lote = TODAY
           pr-cod-refer    = ""
           pr-log-fifo-oc  = NO.

/*
MESSAGE "API TESTE de RETORNO DADOS"    SKIP
        "Registros Retornado"           SKIP
        "num-pedido..:" pr-num-pedido   SKIP
        "numero-ordem:" pr-numero-ordem SKIP
        "parcela.....:" pr-parcela      SKIP
        "class-fisc..:" pr-class-fisc   SKIP
        "cod-depos...:" pr-cod-depos    SKIP
        "cod-localiz.:" pr-cod-localiz  SKIP
        "lote-serie..:" pr-lote-serie   SKIP
        "dt-vali-lote:" pr-dt-vali-lote SKIP
        "cod-refer...:" pr-cod-refer    SKIP
        "log-fifo-oc.:" pr-log-fifo-oc  VIEW-AS ALERT-BOX.
*/
