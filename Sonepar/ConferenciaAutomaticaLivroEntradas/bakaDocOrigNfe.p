FIND FIRST doc-fiscal NO-LOCK
    WHERE doc-orig-nfe.idi-orig-trad = 2
      AND doc-orig-nfe.cod-emitente  = 
      AND doc-orig-nfe.cod-estabel   = ""
      AND doc-orig-nfe.serie         = ""
      AND doc-orig-nfe.nro-docto     = ""
      AND doc-orig-nfe.dt-emissao    = ""
      AND doc-orig-nfe.dt-transacao  = "" NO-ERROR.


IF AVAIL doc-orig-nfe THEN DO:
    MESSAGE "Chave" doc-orig-nfe.ch-acesso-comp-nfe SKIP
            "idi-orig-trad" doc-orig-nfe.idi-orig-trad SKIP
            "valor-cofins" doc-orig-nfe.valor-cofins SKIP
            "valor-cofins-iss" doc-orig-nfe.valor-cofins-iss SKIP
            "valor-cofins-ret" doc-orig-nfe.valor-cofins-ret SKIP
            "valor-cofins" doc-orig-nfe.valor-cofins SKIP
            "valor-csll-ret" doc-orig-nfe.valor-csll-ret SKIP
            "valor-dar" doc-orig-nfe.valor-dar SKIP
            "valor-desc-fatura" doc-orig-nfe.valor-desc-fatura SKIP
            "valor-desconto" doc-orig-nfe.valor-desconto SKIP
            "valor-frete" doc-orig-nfe.valor-frete SKIP
            "valor-icms" doc-orig-nfe.valor-icms SKIP
            "valor-icms-ret" doc-orig-nfe.valor-icms-ret SKIP
            "valor-icms-subs" doc-orig-nfe.valor-icms-subs SKIP
            "valor-ii" doc-orig-nfe.valor-ii SKIP
            "valor-ipi" doc-orig-nfe.valor-ipi SKIP
            "valor-irrf" doc-orig-nfe.valor-irrf SKIP
            "valor-iss" doc-orig-nfe.valor-iss SKIP
            "valor-liq-fatura" doc-orig-nfe.valor-liq-fatura SKIP
            "valor-orig-fatura" doc-orig-nfe.valor-orig-fatura SKIP
            "valor-outros" doc-orig-nfe.valor-outros SKIP
            "valor-pis" doc-orig-nfe.valor-pis SKIP
            "valor-pis-iss" doc-orig-nfe.valor-pis-iss SKIP
            "valor-pis-ret" doc-orig-nfe.valor-pis-ret SKIP
            "valor-prev-ret" doc-orig-nfe.valor-prev-ret SKIP
            "valor-produto" doc-orig-nfe.valor-produto SKIP
            "valor-seguro" doc-orig-nfe.valor-seguro SKIP
            "valor-serv-ret-trans" doc-orig-nfe.valor-serv-ret-trans SKIP
            "valor-tot-iss" doc-orig-nfe.valor-tot-iss SKIP
            "valor-total" doc-orig-nfe.valor-total 
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
