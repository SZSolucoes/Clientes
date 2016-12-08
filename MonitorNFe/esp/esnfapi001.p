/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESNFAPI001 2.00.00.000}  /*** 010000 ***/
/*******************************************************************************
**       Programa: ESNFAPI001
**       Data....: Novembro 2011
**       Objetivo: API Gera‡Æo Documento
**       Versao..: 2.04.032
*******************************************************************************/

/* Temp-table's */

{cdp/cd9043.i} /* Defini‡Æo RowErrors */
{cdp/cdcfgmat.i}
{inbo/boin090.i tt-docum-est}       
{inbo/boin176.i tt-item-doc-est}    
{inbo/boin092.i tt-dupli-apagar}    
{inbo/boin567.i tt-dupli-imp}       
{inbo/boin366.i tt-rat-docum}
{utp/ut-glob.i}

DEF TEMP-TABLE tt-nfe003  LIKE nfe003.
DEF TEMP-TABLE tt-nfe004  LIKE nfe004.
DEF TEMP-TABLE tt-nfe001  LIKE nfe001.
DEF TEMP-TABLE tt-nfe021  LIKE nfe021.
DEF TEMP-TABLE tt-nfe002  LIKE nfe002.
DEF TEMP-TABLE tt-nfe005  LIKE nfe005.
DEF TEMP-TABLE tt-nfe013  LIKE nfe013.
DEF TEMP-TABLE tt-nfe009  LIKE nfe009.
DEF TEMP-TABLE tt-nfe010  LIKE nfe010.
DEF TEMP-TABLE tt-nfe011  LIKE nfe011.
DEF TEMP-TABLE tt-nfe012  LIKE nfe012.

DEF TEMP-TABLE tt-erro-aux NO-UNDO
    FIELD identif-segment AS CHAR
    FIELD cd-erro         AS INTEGER
    FIELD desc-erro       AS CHAR FORMAT "x(80)".

DEF TEMP-TABLE tt-erro NO-UNDO
    FIELD identif-segment AS CHAR
    FIELD cd-erro         AS INTEGER
    FIELD desc-erro       AS CHAR FORMAT "x(80)".

DEFINE TEMP-TABLE tt-variavel
    FIELD cod-var-oper   AS CHAR FORMAT "x(16)"
    FIELD valor-variavel AS CHAR FORMAT "x(60)".

DEFINE TEMP-TABLE tt-retorno
    FIELD valor-retorno AS CHAR FORMAT "x(60)"
    FIELD campo-retorno AS CHAR FORMAT "x(60)"
    FIELD perc-rateio   AS DECIMAL FORMAT ">>9,99".

DEF TEMP-TABLE tt-natureza NO-UNDO
    FIELD nat-operacao LIKE nfe013.nat-operacao.

DEF INPUT        PARAMETER TABLE FOR tt-nfe003.
DEF INPUT        PARAMETER TABLE FOR tt-nfe001.
DEF INPUT        PARAMETER TABLE FOR tt-nfe002.
DEF INPUT        PARAMETER TABLE FOR tt-nfe005.
DEF INPUT        PARAMETER TABLE FOR tt-nfe004.
DEF INPUT        PARAMETER TABLE FOR tt-nfe021.
DEF INPUT        PARAMETER TABLE FOR tt-nfe013.
DEF INPUT        PARAMETER TABLE FOR tt-nfe009.
DEF INPUT        PARAMETER TABLE FOR tt-nfe010.
DEF INPUT        PARAMETER TABLE FOR tt-nfe011.
DEF INPUT        PARAMETER TABLE FOR tt-nfe012.
DEFINE TEMP-TABLE tt-doc-fisico NO-UNDO LIKE doc-fisico .

DEFINE TEMP-TABLE tt-it-doc-fisico NO-UNDO LIKE it-doc-fisico.
DEF INPUT-OUTPUT PARAMETER TABLE FOR tt-erro-aux.

DEFINE BUFFER bf-nfe013 FOR nfe013.       
DEFINE BUFFER bf-nfe003  FOR nfe003. 
DEFINE BUFFER b2nfe013   FOR nfe013. 
DEFINE BUFFER b2nfe003   FOR nfe003.
DEFINE BUFFER btt-nfe013 FOR tt-nfe013.
DEFINE BUFFER bemitente  FOR emitente.

DEFINE VARIABLE i-cod-regra-utilizada AS INTEGER                            NO-UNDO.
DEFINE VARIABLE i-empresa             LIKE mguni.empresa.ep-codigo                NO-UNDO.
DEFINE VARIABLE de-indice             AS DECIMAL                            NO-UNDO.
DEFINE VARIABLE i-seq-item            AS INTEGER                            NO-UNDO.
DEFINE VARIABLE i-seq-nfe013          AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-cont-cfop           AS INTEGER                            NO-UNDO.
DEFINE VARIABLE c-cod-emitente        LIKE emitente.cod-emitente            NO-UNDO.
DEFINE VARIABLE l-sugere-serie        AS LOGICAL                            NO-UNDO.
DEFINE VARIABLE c-serie-docto-novo    LIKE docum-est.serie-docto            NO-UNDO.
DEFINE VARIABLE c-cod-observa-novo    LIKE docum-est.cod-observa            NO-UNDO.
DEFINE VARIABLE c-cod-natur-novo      LIKE nfe013.nat-operacao   NO-UNDO.
DEFINE VARIABLE c-fifo-novo           LIKE nfe013.log-fifo-oc    NO-UNDO.
DEFINE VARIABLE c-ordem-novo          LIKE nfe013.numero-ordem   NO-UNDO.
DEFINE VARIABLE c-pedido-novo         LIKE nfe013.num-pedido     NO-UNDO.
DEFINE VARIABLE c-parcela-novo        LIKE nfe013.parcela        NO-UNDO.
DEFINE VARIABLE c-class-fisc-novo     LIKE nfe013.class-fiscal   NO-UNDO.
DEFINE VARIABLE c-nr-ord-prod-novo    LIKE nfe013.nr-ord-produ   NO-UNDO.
DEFINE VARIABLE c-conta-novo          LIKE nfe013.conta-contabil NO-UNDO.
DEFINE VARIABLE c-refer-novo          LIKE nfe013.cod-refer      NO-UNDO.
DEFINE VARIABLE c-depos-novo          LIKE nfe013.cod-depos      NO-UNDO.
DEFINE VARIABLE c-localiz-novo        LIKE nfe013.cod-localiz    NO-UNDO.
DEFINE VARIABLE c-lote-novo           LIKE nfe013.lote           NO-UNDO.
DEFINE VARIABLE c-dt-lote-novo        LIKE nfe013.dt-vali-lote   NO-UNDO.
DEFINE VARIABLE c-depos-aux           AS CHARACTER               NO-UNDO.
DEFINE VARIABLE r-rowid-emitente      AS ROWID                   NO-UNDO.
DEFINE VARIABLE p-log-fifo-oc         LIKE nfe013.log-fifo-oc    NO-UNDO.
DEFINE VARIABLE p-numero-ordem        LIKE nfe013.numero-ordem   NO-UNDO.
DEFINE VARIABLE p-num-pedido          LIKE nfe013.num-pedido     NO-UNDO.
DEFINE VARIABLE p-parcela             LIKE nfe013.parcela        NO-UNDO.
DEFINE VARIABLE p-class-fisc-novo     LIKE nfe013.class-fiscal   NO-UNDO.
DEFINE VARIABLE p-cod-refer           LIKE nfe013.cod-refer      NO-UNDO.
DEFINE VARIABLE p-cod-depos           LIKE nfe013.cod-depos      NO-UNDO.
DEFINE VARIABLE p-cod-localiz         LIKE nfe013.cod-localiz    NO-UNDO.
DEFINE VARIABLE p-lote-serie          LIKE nfe013.lote           NO-UNDO.
DEFINE VARIABLE p-dt-vali-lote        LIKE nfe013.dt-vali-lote   NO-UNDO.
DEFINE VARIABLE p-class-fisc          LIKE nfe013.class-fiscal   NO-UNDO.
DEFINE VARIABLE de-saldo-ordem        AS DECIMAL     NO-UNDO.

DEF TEMP-TABLE tt-parcelas NO-UNDO
    FIELD r-rowid AS ROWID
    FIELD parcela AS INT
    FIELD qtd-aloc AS DEC
    FIELD qtd-saldo AS DEC.

DEFINE new global SHARED VARIABLE c-newCpfDestino AS CHARACTER NO-UNDO.

FIND FIRST param-re    WHERE param-re.usuario = c-seg-usuario NO-LOCK NO-ERROR.
FIND FIRST param-estoq                                        NO-LOCK NO-ERROR.
FIND FIRST param-global                                       NO-LOCK NO-ERROR.

EMPTY TEMP-TABLE tt-erro-aux.

function fn_only_numbers returns character
    (input cValueString as character):

    define variable iPos     as integer   no-undo.
    define variable iLength  as integer   no-undo.
    define variable cData    as character no-undo.
    define variable cDataNew as character no-undo.


    assign iLength = length(cValueString).

    do iPos = 1 to iLength:
        assign cData = substring(cValueString,iPos,1).
        
        if cData < '0' or cData > '9' then
            next.

        assign cDataNew = cDataNew + cData.
    end.

    return cDataNew.
end function.

FOR EACH tt-nfe003:

    empty temp-table tt-erro-aux.
    IF NOT CAN-FIND (FIRST nfe003 
                     WHERE nfe003.ch-acesso-comp-nfe = tt-nfe003.ch-acesso-comp-nfe        
                     AND   nfe003.idi-orig-trad      = tt-nfe003.idi-orig-trad) THEN DO:

        /** CRIA ORIGEM **/
        CREATE  nfe003.
        BUFFER-COPY tt-nfe003 except cnae-fiscal email-dest TO nfe003 .
        ASSIGN nfe003.dt-transacao  =  TODAY
               nfe003.nat-oper-comp =  ""
               nfe003.idi-situacao  =  1
               nfe003.idi-orig-trad =  1
               nfe003.cnae-fiscal   = substring(tt-nfe003.cnae-fiscal,1,1)
               nfe003.email-dest    = SUBSTRING(tt-nfe003.email-dest,1,28).
    END.
    ELSE DO:
        CREATE tt-erro-aux.
        ASSIGN tt-erro-aux.cd-erro   = 17006
               tt-erro-aux.desc-erro = "J ÿ existe registro com a chave de acesso "  + tt-nfe003.ch-acesso-comp-nfe + " e documento de origem igual " + string(tt-nfe003.idi-orig-trad).
    END.
    

    IF NOT CAN-FIND (FIRST tt-erro-aux) THEN DO:
        /** CRIA DESTINO **/
        IF NOT CAN-FIND (FIRST nfe003 
                         WHERE nfe003.ch-acesso-comp-nfe = tt-nfe003.ch-acesso-comp-nfe 
                         AND   nfe003.idi-orig-trad      = 2) THEN DO:

            CREATE nfe003.
            BUFFER-COPY tt-nfe003 EXCEPT idi-orig-trad cnae-fiscal email-dest TO nfe003.                                          
             
            FOR FIRST estabelec FIELDS (cod-estabel)
                WHERE estabelec.cgc = nfe003.cnpj-destino 
                  /*AND estabelec.ins-estadual = nfe003.insc-estadual-dest*/
                 NO-LOCK:
            END.
            IF AVAIL estabelec THEN
                ASSIGN nfe003.cod-estabel = estabelec.cod-estabel.
            ELSE 
                nfe003.cod-estabel   =  param-estoq.estabel-pad .
                    
            assign c-newCpfDestino = nfe003.cnpj-destino. 
                
                    
            ASSIGN nfe003.dt-transacao  =  TODAY
                   /*nfe003.nat-oper-comp =  ""
                   nfe003.cod-estabel   =  param-estoq.estabel-pad*/
                   nfe003.idi-situacao  =  1
                   nfe003.idi-orig-trad =  2 
                   nfe003.cnae-fiscal   = substring(tt-nfe003.cnae-fiscal,1,1)
                   nfe003.email-dest    = SUBSTRING(tt-nfe003.email-dest,1,28). 

            FIND nfe008 NO-LOCK
                WHERE nfe008.cod-estabel  = nfe003.cod-estabel
                  AND nfe008.cod-emitente = nfe003.cod-emitente NO-ERROR.

            ASSIGN nfe003.nat-oper-comp =  IF AVAIL nfe008 THEN nfe008.nat-operacao ELSE "".


            ASSIGN r-rowid-emitente = ?.
            FOR EACH bemitente NO-LOCK
               WHERE bemitente.cgc = nfe003.cnpj:
                IF fn_only_numbers(bemitente.ins-estadual) = nfe003.insc-estadual THEN DO:
                    ASSIGN r-rowid-emitente = ROWID(bemitente).
                    LEAVE.
                END.
            END.

            /** BUSCA CODIGO EMITENTE **/
            FOR FIRST emitente FIELDS (cod-emitente)
                WHERE rowid(emitente) = r-rowid-emitente NO-LOCK:
            END.
            IF NOT AVAIL emitente THEN
                FOR FIRST emitente FIELDS (cod-emitente)
                    WHERE emitente.CGC = nfe003.cpf NO-LOCK:
                END.

            IF AVAIL emitente THEN
                ASSIGN nfe003.cod-emitente = emitente.cod-emitente
                       c-cod-emitente      = emitente.cod-emitente.
            ELSE
                ASSIGN nfe003.cod-emitente = 0
                       c-cod-emitente      = 0.

        END.

        
        FOR EACH tt-nfe001
           WHERE tt-nfe001.ch-acesso-comp-nfe = nfe003.ch-acesso-comp-nfe:

                        /** CRIA ORIGEM **/
            IF NOT CAN-FIND (FIRST nfe001
                             WHERE nfe001.ch-acesso-comp-nfe = tt-nfe001.ch-acesso-comp-nfe
                             AND   nfe001.idi-orig-trad      = tt-nfe001.idi-orig-trad       
                             AND   nfe001.nro-duplicata      = tt-nfe001.nro-duplicata) THEN  DO:
                CREATE nfe001.
                BUFFER-COPY tt-nfe001 TO nfe001.
            END.

            /** CRIA DESTINO **/
            IF NOT CAN-FIND (FIRST nfe001
                             WHERE nfe001.ch-acesso-comp-nfe = tt-nfe001.ch-acesso-comp-nfe
                             AND   nfe001.idi-orig-trad      = 2             
                             AND   nfe001.nro-duplicata      = tt-nfe001.nro-duplicata  ) THEN DO: 
                CREATE nfe001.
                BUFFER-COPY  tt-nfe001 EXCEPT tt-nfe001.idi-orig-trad
                                                        TO nfe001.
                ASSIGN nfe001.idi-orig-trad = 2.
            END.
        END.
        
        FOR EACH tt-nfe021
           WHERE tt-nfe021.ch-acesso-comp-nfe =  nfe003.ch-acesso-comp-nfe:

            /** CRIA ORIGEM **/
            IF NOT CAN-FIND (FIRST nfe021
                             WHERE nfe021.ch-acesso-comp-nfe = tt-nfe021.ch-acesso-comp-nfe 
                             AND   nfe021.idi-orig-trad      = tt-nfe021.idi-orig-trad )    THEN DO:
                CREATE nfe021.
                BUFFER-COPY tt-nfe021 TO nfe021.
            END.

            /** CRIA DESTINO **/
            IF NOT CAN-FIND (FIRST nfe021                                                           
                             WHERE nfe021.ch-acesso-comp-nfe = tt-nfe021.ch-acesso-comp-nfe    
                             AND   nfe021.idi-orig-trad      = 2 )   THEN DO:                   
                CREATE nfe021.
                BUFFER-COPY tt-nfe021 EXCEPT tt-nfe021.idi-orig-trad
                                                       TO nfe021.
                ASSIGN nfe021.idi-orig-trad = 2.
            END.
        END.
            
        FOR EACH tt-nfe002
            WHERE tt-nfe002.ch-acesso-comp-nfe = nfe003.ch-acesso-comp-nfe:

            
            /** CRIA ORIGEM **/
            IF NOT CAN-FIND (FIRST nfe002
                             WHERE nfe002.ch-acesso-comp-nfe = tt-nfe002.ch-acesso-comp-nfe
                             AND   nfe002.idi-orig-trad      = tt-nfe002.idi-orig-trad) THEN DO:
                CREATE nfe002.
                BUFFER-COPY tt-nfe002 TO nfe002.
            END.

            /** CRIA DESTINO **/
            IF NOT CAN-FIND (FIRST nfe002
                             WHERE nfe002.ch-acesso-comp-nfe = tt-nfe002.ch-acesso-comp-nfe
                             AND   nfe002.idi-orig-trad      = 2 ) THEN DO:
                CREATE nfe002.
                BUFFER-COPY tt-nfe002 EXCEPT tt-nfe002.idi-orig-trad
                                                      TO nfe002.
                ASSIGN nfe002.idi-orig-trad = 2.
            END.
        END.
        
        FOR EACH  tt-nfe005
            WHERE tt-nfe005.ch-acesso-comp-nfe =  tt-nfe003.ch-acesso-comp-nfe:

            
            IF  tt-nfe005.nro-docto     = ?
            AND tt-nfe005.ch-acesso-nfe = "":U THEN
                NEXT.

            /** CRIA ORIGEM **/
            IF NOT CAN-FIND (FIRST nfe005
                             WHERE nfe005.ch-acesso-comp-nfe = tt-nfe005.ch-acesso-comp-nfe          
                             AND   nfe005.idi-orig-trad      = tt-nfe005.idi-orig-trad
                             AND   nfe005.num-seq            = tt-nfe005.num-seq) THEN DO:
                CREATE nfe005.
                BUFFER-COPY tt-nfe005 TO nfe005.
            END.
    
            /** CRIA DESTINO **/
            IF NOT CAN-FIND (FIRST nfe005
                             WHERE nfe005.ch-acesso-comp-nfe = tt-nfe005.ch-acesso-comp-nfe
                             AND   nfe005.idi-orig-trad      = 2
                             AND   nfe005.num-seq            = tt-nfe005.num-seq)  THEN DO:       
                CREATE nfe005.
                BUFFER-COPY  tt-nfe005  EXCEPT tt-nfe005.idi-orig-trad 
                                                        TO nfe005.

                ASSIGN nfe005.idi-orig-trad = 2.

                IF nfe005.ch-acesso-nfe <> "" THEN
                    ASSIGN nfe005.cnpj          = SUBSTRING(nfe005.ch-acesso-nfe,7,14)
                           nfe005.serie-docto   = STRING(INT(SUBSTRING(nfe005.ch-acesso-nfe,23,3)),"99")
                           nfe005.num-documento = INT(SUBSTRING(nfe005.ch-acesso-nfe,26,9)).
                           nfe005.nro-docto     = STRING(nfe005.num-documento, "9999999").

                FOR FIRST estabelec FIELDS (cod-estabel)
                    WHERE estabelec.cgc = nfe005.cnpj NO-LOCK:
                END.
                IF AVAIL estabelec THEN
                    ASSIGN nfe005.cod-estabel = estabelec.cod-estabel.
            END.
        END.                    
        
        FOR EACH tt-nfe004
            WHERE tt-nfe004.ch-acesso-comp-nfe = tt-nfe003.ch-acesso-comp-nfe:

                        
            /** CRIA ORIGEM **/
            IF NOT CAN-FIND (FIRST nfe004
                             WHERE nfe004.ch-acesso-comp-nfe = tt-nfe004.ch-acesso-comp-nfe 
                             AND   nfe004.idi-orig-trad      = tt-nfe004.idi-orig-trad) THEN DO:
                CREATE nfe004.
                BUFFER-COPY tt-nfe004 TO nfe004.
            END.
    
            /** CRIA DESTINO **/
            IF NOT CAN-FIND (FIRST nfe004
                             WHERE nfe004.ch-acesso-comp-nfe = tt-nfe004.ch-acesso-comp-nfe
                             AND   nfe004.idi-orig-trad      = 2 ) THEN DO:
                CREATE nfe004.
                BUFFER-COPY  tt-nfe004  EXCEPT tt-nfe004.idi-orig-trad 
                                                           TO nfe004.
                ASSIGN nfe004.idi-orig-trad = 2.
            END.
        END.
    
        empty temp-table tt-parcelas.
        
        FOR EACH tt-nfe013
            WHERE tt-nfe013.ch-acesso-comp-nfe = tt-nfe003.ch-acesso-comp-nfe:

            /** CRIA ORIGEM **/
            IF NOT CAN-FIND (FIRST nfe013
                             WHERE nfe013.ch-acesso-comp-nfe = tt-nfe013.ch-acesso-comp-nfe
                             AND   nfe013.idi-orig-trad      = tt-nfe013.idi-orig-trad 
                             AND   nfe013.seq-item           = tt-nfe013.seq-item) THEN DO:
                CREATE nfe013.
                BUFFER-COPY tt-nfe013 TO nfe013.
            END.

            
            /** CRIA DESTINO **/
            IF NOT CAN-FIND (FIRST nfe013 
                             WHERE nfe013.ch-acesso-comp-nfe =  tt-nfe013.ch-acesso-comp-nfe
                             AND   nfe013.idi-orig-trad      =  2
                             AND   nfe013.seq-item           =  tt-nfe013.seq-item) THEN DO:
                CREATE nfe013.
                BUFFER-COPY tt-nfe013 EXCEPT tt-nfe013.idi-orig-trad 
                                                     TO nfe013.
                ASSIGN nfe013.idi-orig-trad = 2.

                /** DE-PARA FORNECEDOR **/
                FOR FIRST item-fornec 
                    WHERE item-fornec.item-do-forn = nfe013.it-codigo          
                    AND   item-fornec.cod-emitente = nfe003.cod-emitente NO-LOCK:
                END.
                IF AVAIL item-fornec THEN DO:
                  ASSIGN nfe013.it-codigo      = item-fornec.it-codigo
                         nfe013.un-comercial   = item-fornec.unid-med-for
                         de-indice             = item-fornec.fator-conver / exp(10,item-fornec.num-casa-dec)
                         nfe013.qtd-interna    = nfe013.qtd-comercial / de-indice
                         nfe013.preco-unit     = nfe013.preco-total / nfe013.qtd-interna .
                END.


                IF NOT AVAIL item-fornec THEN DO:
                    FOR FIRST ITEM 
                         WHERE ITEM.it-codigo = nfe013.it-codigo NO-LOCK :

                        ASSIGN nfe013.it-codigo      = item.it-codigo
                               nfe013.un-comercial   = item.un
                               nfe013.un-interna     = item.un
                               de-indice             = 1
                               nfe013.qtd-interna    = nfe013.qtd-comercial * de-indice
                               nfe013.preco-unit     = nfe013.preco-total / nfe013.qtd-interna
                               nfe013.class-fiscal = ITEM.class-fiscal.
                    END.

                    If Not Avail Item Then Do:
                        Find First nfe020
                            Where nfe020.cod-emitente = nfe003.cod-emitente
                              And nfe020.item-do-forn = nfe013.it-codigo No-lock No-error.
                        If Avail nfe020 Then Do:
                            Find ITEM 
                                WHERE ITEM.it-codigo = nfe020.it-codigo No-lock No-error.
                            If Avail Item Then Do:
                                ASSIGN nfe013.it-codigo      = item.it-codigo
                                       nfe013.un-comercial   = item.un
                                       nfe013.un-interna     = item.un
                                       de-indice             = 1
                                       nfe013.qtd-interna    = nfe013.qtd-comercial * de-indice
                                       nfe013.preco-unit     = nfe013.preco-total / nfe013.qtd-interna 
                                       nfe013.class-fiscal   = ITEM.class-fiscal.
                            End.
                        End.
                    End.    
                END.
                
                If Not Avail Item Then Do:
                    FOR FIRST ITEM 
                        WHERE ITEM.it-codigo = nfe013.it-codigo NO-LOCK:
                    END.
                    IF AVAIL ITEM THEN
                        ASSIGN nfe013.class-fiscal = ITEM.class-fiscal
                               nfe013.un-interna   = ITEM.un.
                End.

                Find FIRST item-uni-estab 
                WHERE item-uni-estab.it-codigo   = nfe013.it-codigo
                  AND item-uni-estab.cod-estabel = nfe003.cod-estabel No-lock No-error.
                
                Find estabelec
                    Where estabelec.cod-estabel = nfe003.cod-estabel No-lock No-error.

               
                FOR FIRST ordem-compra FIELDS(dep-almoxar)
                    WHERE ordem-compra.numero-ordem = nfe013.numero-ordem NO-LOCK: END.
                IF  AVAIL ordem-compra THEN
                    ASSIGN c-depos-aux = ordem-compra.dep-almoxar.
                ELSE DO:
                    IF  AVAIL item-uni-estab THEN
                        ASSIGN c-depos-aux = item-uni-estab.deposito-pad.
                END.
                
                Assign nfe013.cod-depos = c-depos-aux.

                /************************/

                /* fifo*/

                FOR EACH prazo-compra NO-LOCK
                    WHERE prazo-compra.it-codigo    =  nfe013.it-codigo   
                      AND prazo-compra.situacao     =  2                       
                      AND prazo-compra.quant-saldo  >  0 
                       BY prazo-compra.data-entrega:
                   
                    FIND ordem-compra 
                        WHERE ordem-compra.numero-ordem = prazo-compra.numero-ordem NO-LOCK NO-ERROR.
                    IF  NOT AVAIL ordem-compra THEN
                        NEXT.
                   
                    IF ordem-compra.cod-emitente <> nfe003.cod-emitente THEN  
                        NEXT.
                  
                    IF ordem-compra.cod-estabel <> nfe003.cod-estabel THEN  
                        NEXT.  
                    
                    /* Diminui Quantidade Alocada */
                    ASSIGN de-saldo-ordem  = prazo-compra.quant-saldo - prazo-compra.dec-1.
                   
                    if  de-saldo-ordem <= 0 then
                        next.

                    FIND FIRST tt-parcelas
                         WHERE tt-parcelas.r-rowid = ROWID(prazo-compra) NO-ERROR.
                    IF NOT AVAIL (tt-parcelas) THEN DO:
                         CREATE tt-parcelas.
                         ASSIGN tt-parcelas.r-rowid = ROWID(prazo-compra)
                                tt-parcelas.parcela = prazo-compra.parcela.

                         FOR EACH b2nfe013 NO-LOCK
                            WHERE b2nfe013.it-codigo    = prazo-compra.it-codigo
                              AND b2nfe013.numero-ordem = prazo-compra.numero-ordem
                              AND b2nfe013.parcela      = prazo-compra.parcela:

                             FIND FIRST b2nfe003 NO-LOCK
                                  WHERE b2nfe003.ch-acesso-comp-nfe = b2nfe013.ch-acesso-comp-nfe NO-ERROR.
                            
                             FIND FIRST docum-est NO-LOCK
                                  WHERE docum-est.serie-docto   = b2nfe003.serie-docto
                                    AND docum-est.nro-docto     = b2nfe003.nro-docto
                                    AND docum-est.cod-emitente  = b2nfe003.cod-emitente
                                    AND docum-est.nat-operacao  = b2nfe013.nat-operacao NO-ERROR.
                             IF NOT AVAIL docum-est THEN
                                 ASSIGN tt-parcelas.qtd-aloc  = tt-parcelas.qtd-aloc + b2nfe013.qtd-interna
                                        tt-parcelas.qtd-saldo = (prazo-compra.quant-saldo - prazo-compra.dec-1) - tt-parcelas.qtd-aloc.
                         END.
                    END.

                    IF ((prazo-compra.quant-saldo - prazo-compra.dec-1) - tt-parcelas.qtd-aloc) <= 0 THEN
                        NEXT.

                    IF  nfe013.qtd-interna > ((prazo-compra.quant-saldo - prazo-compra.dec-1) - tt-parcelas.qtd-aloc) THEN DO:
                        FIND LAST btt-nfe013 NO-ERROR.
                        IF AVAIL btt-nfe013 THEN
                            ASSIGN i-seq-nfe013 = btt-nfe013.seq-item + 1.
                        ELSE
                            ASSIGN i-seq-nfe013 = 1.

                        CREATE btt-nfe013.
                        BUFFER-COPY tt-nfe013 EXCEPT seq-item TO btt-nfe013.
                        ASSIGN btt-nfe013.seq-item      = i-seq-nfe013
                               btt-nfe013.qtd-comercial = (nfe013.qtd-interna - ((prazo-compra.quant-saldo - prazo-compra.dec-1) - tt-parcelas.qtd-aloc)) * de-indice
                               btt-nfe013.qtd-interna   = btt-nfe013.qtd-comercial / de-indice
                               btt-nfe013.preco-total   = nfe013.preco-unit * btt-nfe013.qtd-interna
                               btt-nfe013.qtd-interna   = 0
                               btt-nfe013.idi-orig-trad = 1.
    
                        ASSIGN nfe013.qtd-interna    = ((prazo-compra.quant-saldo - prazo-compra.dec-1) - tt-parcelas.qtd-aloc)
                               nfe013.qtd-comercial  = nfe013.qtd-interna * de-indice
                               nfe013.preco-total    = nfe013.preco-unit * nfe013.qtd-interna.

                        FIND FIRST b2nfe013 EXCLUSIVE-LOCK
                             WHERE b2nfe013.ch-acesso-comp-nfe    = nfe013.ch-acesso-comp-nfe
                               AND b2nfe013.idi-orig              = 1
                               AND b2nfe013.seq-item              = nfe013.seq-item NO-ERROR.
                        IF AVAIL b2nfe013 THEN
                            ASSIGN b2nfe013.qtd-comercial = nfe013.qtd-comercial
                                   b2nfe013.preco-total   = nfe013.preco-total.
                    END.
        
                    ASSIGN tt-parcelas.qtd-aloc = tt-parcelas.qtd-aloc + nfe013.qtd-interna
                           tt-parcelas.qtd-saldo = (prazo-compra.quant-saldo - prazo-compra.dec-1) - tt-parcelas.qtd-aloc.
            
                    ASSIGN nfe013.num-pedido     = ordem-compra.num-pedido
                           nfe013.numero-ordem   = ordem-compra.numero-ordem
                           nfe013.parcela        = prazo-compra.parcela
                           nfe013.conta-contabil = ordem-compra.conta-contabil. 

                    LEAVE.
                   
                END.
            END.
            
            FOR EACH  tt-nfe009
                WHERE tt-nfe009.ch-acesso-comp-nfe = tt-nfe013.ch-acesso-comp-nfe:

                                /** CRIA ORIGEM **/
                IF NOT CAN-FIND (FIRST nfe009
                                 WHERE nfe009.ch-acesso-comp-nfe = tt-nfe009.ch-acesso-comp-nfe     
                                 AND   nfe009.idi-orig-trad      = tt-nfe009.idi-orig-trad 
                                 AND   nfe009.seq-item           = tt-nfe009.seq-item) THEN DO:
                    CREATE nfe009.
                    BUFFER-COPY tt-nfe009 TO nfe009.
                END.
    
                /** CRIA DESTINO **/
                IF NOT CAN-FIND (FIRST nfe009
                                 WHERE nfe009.ch-acesso-comp-nfe = tt-nfe009.ch-acesso-comp-nfe
                                 AND   nfe009.idi-orig-trad      = 2      
                                 AND   nfe009.seq-item           = tt-nfe009.seq-item) THEN DO:
                    CREATE nfe009.
                    BUFFER-COPY  tt-nfe009  EXCEPT tt-nfe009.idi-orig-trad 
                                                                TO nfe009.
                    ASSIGN nfe009.idi-orig-trad = 2.
                END.
            END.
              
            FOR EACH tt-nfe010
                WHERE tt-nfe010.ch-acesso-comp-nfe = tt-nfe013.ch-acesso-comp-nfe:

                /** CRIA ORIGEM **/
                IF NOT CAN-FIND (FIRST nfe010
                                 WHERE nfe010.ch-acesso-comp-nfe = tt-nfe010.ch-acesso-comp-nfe    
                                 AND   nfe010.idi-orig-trad      = tt-nfe010.idi-orig-trad        
                                 AND   nfe010.seq-item           = tt-nfe010.seq-item) THEN DO:
                    CREATE nfe010.
                    BUFFER-COPY tt-nfe010 TO nfe010.
                END.
    
                /** CRIA DESTINO **/
                IF NOT CAN-FIND (FIRST nfe010
                                 WHERE nfe010.ch-acesso-comp-nfe = tt-nfe010.ch-acesso-comp-nfe
                                 AND   nfe010.idi-orig-trad      = 2         
                                 AND   nfe010.seq-item           = tt-nfe010.seq-item ) THEN DO:
                    CREATE nfe010.
                    BUFFER-COPY tt-nfe010  EXCEPT tt-nfe010.idi-orig-trad 
                                                               TO nfe010.
                    ASSIGN nfe010.idi-orig-trad = 2.
                END.
            END.
            
            FOR EACH tt-nfe011
                WHERE tt-nfe011.ch-acesso-comp-nfe = tt-nfe013.ch-acesso-comp-nfe:

                /** CRIA ORIGEM **/
                IF NOT CAN-FIND (FIRST nfe011
                                 WHERE nfe011.ch-acesso-comp-nfe = tt-nfe011.ch-acesso-comp-nfe
                                 AND   nfe011.idi-orig-trad      = tt-nfe011.idi-orig-trad            
                                 AND   nfe011.seq-item           = tt-nfe011.seq-item) THEN DO:
                    CREATE nfe011.
                    BUFFER-COPY tt-nfe011 TO nfe011.
                END.
    
                /** CRIA DESTINO **/
                IF NOT CAN-FIND (FIRST nfe011 
                                 WHERE nfe011.ch-acesso-comp-nfe = tt-nfe013.ch-acesso-comp-nfe
                                 AND   nfe011.idi-orig-trad      = 2        
                                 AND   nfe011.seq-item           = tt-nfe011.seq-item) THEN DO:
                    CREATE nfe011.
                    BUFFER-COPY  tt-nfe011  EXCEPT tt-nfe011.idi-orig-trad 
                                                              TO nfe011.
                    ASSIGN nfe011.idi-orig-trad = 2.
                END.
            END.
    
            FOR EACH tt-nfe012
                WHERE tt-nfe012.ch-acesso-comp-nfe = tt-nfe013.ch-acesso-comp-nfe:

                
                /** CRIA ORIGEM **/
                IF NOT CAN-FIND (FIRST nfe012
                                 WHERE nfe012.ch-acesso-comp-nfe = tt-nfe012.ch-acesso-comp-nfe 
                                 AND   nfe012.idi-orig-trad      = tt-nfe012.idi-orig-trad 
                                 AND   nfe012.seq-item           = tt-nfe012.seq-item) THEN DO:
                    CREATE nfe012.
                    BUFFER-COPY tt-nfe012 TO nfe012.
                END.
    
                /** CRIA DESTINO **/
                IF NOT CAN-FIND (FIRST nfe012
                                 WHERE nfe012.ch-acesso-comp-nfe = tt-nfe013.ch-acesso-comp-nfe
                                 AND   nfe012.idi-orig-trad      = 2     
                                 AND   nfe012.seq-item           = tt-nfe012.seq-item) THEN DO:
                    CREATE nfe012.
                    BUFFER-COPY tt-nfe012 EXCEPT tt-nfe012.idi-orig-trad
                                                             TO nfe012.
                    ASSIGN nfe012.idi-orig-trad =  2.
                END.
            END.
        END. /*for each tt-nfe013*/

        RUN pi-registro-traduzido.    
    END.  /*can-find de erro*/
END. /*For each  tt-nfe003*/



PROCEDURE pi-registro-traduzido:
    
    EMPTY TEMP-TABLE tt-docum-est    NO-ERROR. 
    EMPTY TEMP-TABLE tt-item-doc-est NO-ERROR. 
    EMPTY TEMP-TABLE tt-erro         NO-ERROR. 
    EMPTY TEMP-TABLE tt-natureza     NO-ERROR.


    /** EXECUTA ENGINE **/
    
    
    RUN pi-carrega-variaveis.   
    

    
    /** VERIFICA MAIS DE UMA CFOP **/
    ASSIGN i-cont-cfop = 0.
    FOR EACH nfe013 FIELDS (nat-operacao)
        WHERE nfe013.ch-acesso-comp-nfe = tt-nfe003.ch-acesso-comp-nfe
          AND nfe013.idi-orig-trad      = 2 NO-LOCK:

        IF NOT CAN-FIND(FIRST tt-natureza
                        WHERE tt-natureza.nat-operacao = nfe013.nat-operacao) THEN DO:
            CREATE tt-natureza.
            ASSIGN tt-natureza.nat-operacao = nfe013.nat-operacao
                   i-cont-cfop              = i-cont-cfop + 1.
        END.
    END.
  
    IF CAN-FIND (FIRST nfe007
                 WHERE nfe007.cod-emitente = c-cod-emitente 
                 AND   nfe007.log-entrada-aut) THEN DO:
        
        FOR EACH tt-natureza:
           
            /** CRIA DOCUMENTO FISICO **/
            RUN pi-cria-tt-doc-fisico (INPUT tt-natureza.nat-operacao).

            /** CRIA DOCUMENTO FISCAL**/
            RUN pi-cria-tt-docum-est (INPUT tt-natureza.nat-operacao).  
    
            /** CRIA ITENS DO DOCUMENTO **/
            ASSIGN i-seq-item = IF AVAIL param-re THEN param-re.seq-item-um ELSE 1.
            
            FOR EACH nfe013 FIELDS (it-codigo     num-pedido     numero-ordem  
                                               parcela       nr-ord-produ   qtd-comercial 
                                               un-comercial  qtd-interna    preco-unit    
                                               preco-total   class-fiscal   log-fifo-oc   
                                               log-fifo-oc   conta-contabil cod-refer     
                                               cod-depos     cod-localiz    lote          
                                               dt-vali-lote  nat-operacao   seq-item)
                WHERE nfe013.ch-acesso-comp-nfe = tt-nfe003.ch-acesso-comp-nfe
                  AND nfe013.idi-orig-trad      = 2 NO-LOCK:
                
                
                IF nfe013.nat-operacao <> tt-natureza.nat-operacao THEN
                    NEXT.

    
                /** ITENS RECEBIMENTO FISICO **/
                RUN pi-cria-tt-item-doc-fisico (INPUT tt-doc-fisico.cod-emitente,
                                                INPUT tt-doc-fisico.nro-docto,
                                                INPUT tt-doc-fisico.serie-docto). 
               
    
                /** ITENS RECEBIMENTO FISCAL **/
                RUN pi-cria-tt-item-doc-est (INPUT tt-docum-est.cod-emitente,
                                             INPUT tt-docum-est.nro-docto,
                                             INPUT tt-docum-est.serie-docto,
                                             INPUT tt-docum-est.nat-operacao).   
            END.
        END.      

        /*DO TRANSACTION ON ERROR UNDO, LEAVE:

            RUN pi-cria-nota (OUTPUT TABLE tt-erro).

            IF CAN-FIND(FIRST tt-erro) THEN
                UNDO,LEAVE.

        END.*/

        RUN rep/reapi316b.p (INPUT  "ADD",
                             INPUT  TABLE tt-docum-est,
                             INPUT  TABLE tt-rat-docum,
                             INPUT  TABLE tt-item-doc-est,
                             INPUT  TABLE tt-dupli-apagar,
                             INPUT  TABLE tt-dupli-imp,
                             OUTPUT TABLE tt-erro ) NO-ERROR.

        FOR EACH tt-erro:
            CREATE tt-erro-aux.
            BUFFER-COPY tt-erro TO tt-erro-aux.
        END.
    END.
    ELSE DO:
        CREATE tt-erro-aux.
        ASSIGN tt-erro-aux.cd-erro   = 99999
               tt-erro-aux.desc-erro = "Emitente nÆo parametrizado para receber automaticamente o documento".
    END.
   
    DO ON ERROR UNDO:
        FOR FIRST bf-nfe003 
            WHERE bf-nfe003.ch-acesso-comp-nfe = tt-nfe003.ch-acesso-comp-nfe
            AND   bf-nfe003.idi-orig-trad      = 2 EXCLUSIVE-LOCK:
    
            IF CAN-FIND(FIRST tt-erro-aux WHERE tt-erro-aux.cd-erro   = 99999) THEN 
                ASSIGN bf-nfe003.idi-situacao   = 6
                   bf-nfe003.log-multi-cfop = i-cont-cfop > 1. 
            ELSE
                ASSIGN bf-nfe003.idi-situacao   = IF CAN-FIND(FIRST tt-erro-aux) THEN 2 ELSE 1
                       bf-nfe003.log-multi-cfop = i-cont-cfop > 1. 
        END.
        RELEASE bf-nfe003.
    END.

    /** CRIACAO TABELA DE ERROS **/
    FOR EACH  tt-erro-aux:
        RUN pi-gera-erro (INPUT tt-erro-aux.cd-erro, 
                          INPUT tt-erro-aux.desc-erro).
    END.

    RETURN "OK":U.

END PROCEDURE.

PROCEDURE pi-cria-erro:
    DEF INPUT PARAM p-identif-segment LIKE tt-erro.identif-segment NO-UNDO.
    DEF INPUT PARAM p-cd-erro LIKE tt-erro.cd-erro NO-UNDO.
    DEF INPUT PARAM p-desc-erro LIKE tt-erro.desc-erro NO-UNDO.

    create tt-erro.
    assign tt-erro.identif-segment = p-identif-segment
           tt-erro.cd-erro         = p-cd-erro
           tt-erro.desc-erro       = p-desc-erro.

end procedure.

PROCEDURE pi-cria-tt-doc-fisico:

    DEFINE INPUT PARAM p-nat-oper LIKE nfe013.cod-cfop NO-UNDO.
   
    IF NOT CAN-FIND (FIRST tt-doc-fisico
                     WHERE tt-doc-fisico.serie-docto  = nfe003.serie-docto
                     AND   tt-doc-fisico.nro-docto    = nfe003.nro-docto
                     AND   tt-doc-fisico.cod-emitente = nfe003.cod-emitente
                     AND   tt-doc-fisico.tipo-nota    = 1) THEN DO:

            FOR FIRST nfe003 NO-LOCK
                WHERE nfe003.ch-acesso-comp-nfe = tt-nfe003.ch-acesso-comp-nfe 
                AND   nfe003.idi-orig-trad      = 2:
            END.

            CREATE tt-doc-fisico.
            ASSIGN tt-doc-fisico.tipo-nota    = 1
                   tt-doc-fisico.nro-docto    = nfe003.nro-docto
                   tt-doc-fisico.serie-docto  = nfe003.serie-docto
                   tt-doc-fisico.dt-emissao   = nfe003.dt-emissao  
                   tt-doc-fisico.dt-trans     = nfe003.dt-transacao
                   tt-doc-fisico.situacao     = 1
                   tt-doc-fisico.valor-outras = 0
                   tt-doc-fisico.despesa-nota = 0
                   tt-doc-fisico.tot-peso     = 0
                   tt-doc-fisico.tot-desconto = 0
                   tt-doc-fisico.valor-mercad = 0
                   tt-doc-fisico.cod-emitente = nfe003.cod-emitente
                   tt-doc-fisico.cod-estabel  = nfe003.cod-estabel.

    END.

END PROCEDURE.

PROCEDURE pi-cria-tt-docum-est: /*  Cria registro na temp-table tt-docum-est  */

    DEFINE INPUT PARAM p-nat-oper LIKE nfe013.cod-cfop NO-UNDO.


    IF NOT CAN-FIND (FIRST tt-docum-est
                     WHERE tt-docum-est.serie-docto   = nfe003.serie-docto
                     AND   tt-docum-est.nro-docto     = nfe003.nro-docto
                     AND   tt-docum-est.cod-emitente  = nfe003.cod-emitente
                     AND   tt-docum-est.nat-operacao  = p-nat-oper) THEN DO:

        FOR FIRST nfe003 NO-LOCK
            WHERE nfe003.ch-acesso-comp-nfe = tt-nfe003.ch-acesso-comp-nfe 
            AND   nfe003.idi-orig-trad      = 2:
        END.

        CREATE tt-docum-est.
        ASSIGN tt-docum-est.cod-emitente         = nfe003.cod-emitente
               tt-docum-est.nro-docto            = nfe003.nro-docto  
               tt-docum-est.nat-operacao         = p-nat-oper
               tt-docum-est.serie-docto          = nfe003.serie-docto
               tt-docum-est.cod-observa          = nfe003.cod-observa
               tt-docum-est.cod-estabel          = nfe003.cod-estabel
               tt-docum-est.dt-emissao           = nfe003.dt-emissao
               tt-docum-est.dt-trans             = nfe003.dt-transacao
               tt-docum-est.mod-frete            = int(nfe003.mod-frete) 

        &IF "{&bf_mat_versao_ems}" < "2.07" &THEN
              OVERLAY(tt-docum-est.char-1,93,60) = nfe003.ch-acesso-comp-nfe
              OVERLAY(tt-docum-est.char-1,153,1) = "1".
        &ELSE
              tt-docum-est.cod-chave-aces-nf-eletro = nfe003.ch-acesso-comp-nfe
              tt-docum-est.cdn-sit-nfe              = 1.
        &ENDIF
        
        &IF "{&bf_dis_versao_ems}" >= "2.062" &THEN
            IF  nfe003.razao-social-transp <> ? THEN
                ASSIGN tt-docum-est.nome-transp = nfe003.razao-social-transp.

            IF  nfe003.placa <> ? THEN
                ASSIGN tt-docum-est.cod-placa[1] = nfe003.placa .

            IF nfe003.uf-placa <> ? THEN
                ASSIGN tt-docum-est.cod-uf-placa[1] = nfe003.uf-placa.
        &ELSE
            IF nfe003.razao-social-transp <> ? THEN
                ASSIGN OVERLAY(tt-docum-est.char-2,102,12) = nfe003.razao-social-transp.

            IF nfe003.placa <> ? THEN
                ASSIGN OVERLAY(tt-docum-est.char-2,115,7) = nfe003.placa.

            IF nfe003.uf-placa <> ? THEN
                ASSIGN OVERLAY(tt-docum-est.char-2,136,2) = nfe003.uf-placa.
        &ENDIF
    END.

    RETURN "OK":U.
END PROCEDURE.

PROCEDURE pi-cria-tt-item-doc-fisico: /*  Cria registro na temp-table tt-it-doc-fisico  */

    DEF INPUT PARAM p-cod-emitente       LIKE tt-docum-est.cod-emitente         NO-UNDO.
    DEF INPUT PARAM p-nro-docto          LIKE tt-docum-est.nro-docto            NO-UNDO.
    DEF INPUT PARAM p-serie-docto        LIKE tt-docum-est.serie-docto          NO-UNDO.
    
    IF NOT CAN-FIND (FIRST tt-it-doc-fisico
                     WHERE tt-it-doc-fisico.serie-docto  = p-serie-docto 
                     AND   tt-it-doc-fisico.nro-docto    = p-nro-docto   
                     AND   tt-it-doc-fisico.cod-emitente = p-cod-emitente
                     AND   tt-it-doc-fisico.tipo-nota    = 1
                     AND   tt-it-doc-fisico.sequencia    = i-seq-item) THEN DO:

        FIND ITEM 
            WHERE ITEM.it-codigo = tt-it-doc-fisico.it-codigo NO-LOCK NO-ERROR.

        CREATE tt-it-doc-fisico.
        ASSIGN tt-it-doc-fisico.serie-docto    = p-serie-docto
               tt-it-doc-fisico.nro-docto      = p-nro-docto
               tt-it-doc-fisico.cod-emitente   = p-cod-emitente
               tt-it-doc-fisico.tipo-nota      = 1
               tt-it-doc-fisico.num-pedido     = nfe013.num-pedido
               tt-it-doc-fisico.quantidade     = nfe013.qtd-interna
               tt-it-doc-fisico.sequencia      = i-seq-item
               i-seq-item                      = i-seq-item + IF AVAIL param-re THEN param-re.inc-seq ELSE 1
               tt-it-doc-fisico.it-codigo      = nfe013.it-codigo
               tt-it-doc-fisico.qt-do-forn     = nfe013.qtd-comercial
               tt-it-doc-fisico.desconto[1]    = 0
               tt-it-doc-fisico.preco-unit[1]  = nfe013.preco-unit
               tt-it-doc-fisico.numero-ordem   = nfe013.numero-ordem
               tt-it-doc-fisico.parcela        = nfe013.parcela     
               tt-it-doc-fisico.nr-ord-produ   = nfe013.nr-ord-produ
               tt-it-doc-fisico.un             = nfe013.un-comercial
               tt-it-doc-fisico.preco-total    = nfe013.preco-total
               tt-it-doc-fisico.log-1          = nfe013.log-fifo-oc
               tt-it-doc-fisico.fifo-oc        = nfe013.log-fifo-oc
               tt-it-doc-fisico.conta-contabil = IF AVAIL ITEM THEN ITEM.conta-aplicacao ELSE ""
               tt-it-doc-fisico.cod-refer      = nfe013.cod-refer     
               tt-it-doc-fisico.cod-depos      = nfe013.cod-depos     
               tt-it-doc-fisico.cod-localiz    = nfe013.cod-localiz   
               tt-it-doc-fisico.lote           = nfe013.lote          
               tt-it-doc-fisico.dt-vali-lote   = nfe013.dt-vali-lote.
        

    END.

END PROCEDURE.

PROCEDURE pi-cria-tt-item-doc-est:

    DEF INPUT PARAM p-cod-emitente       LIKE tt-docum-est.cod-emitente         NO-UNDO.
    DEF INPUT PARAM p-nro-docto          LIKE tt-docum-est.nro-docto            NO-UNDO.
    DEF INPUT PARAM p-serie-docto        LIKE tt-docum-est.serie-docto          NO-UNDO.
    DEF INPUT PARAM p-nat-operacao       LIKE tt-docum-est.nat-operacao         NO-UNDO.

    IF NOT CAN-FIND (FIRST tt-item-doc-est
                     WHERE tt-item-doc-est.serie-docto  = p-serie-docto 
                     AND   tt-item-doc-est.nro-docto    = p-nro-docto   
                     AND   tt-item-doc-est.cod-emitente = p-cod-emitente
                     AND   tt-item-doc-est.nat-operacao = p-nat-operacao
                     AND   tt-item-doc-est.sequencia    = i-seq-item) THEN DO:
        
        CREATE tt-item-doc-est.
        ASSIGN tt-item-doc-est.serie-docto    = p-serie-docto  
               tt-item-doc-est.nro-docto      = p-nro-docto    
               tt-item-doc-est.cod-emitente   = p-cod-emitente 
               tt-item-doc-est.nat-operacao   = p-nat-operacao 
               tt-item-doc-est.sequencia      = i-seq-item
               i-seq-item                     = i-seq-item + IF AVAIL param-re THEN param-re.inc-seq ELSE 1

               tt-item-doc-est.it-codigo      = nfe013.it-codigo
               tt-item-doc-est.num-pedido     = nfe013.num-pedido  
               tt-item-doc-est.numero-ordem   = nfe013.numero-ordem
               tt-item-doc-est.parcela        = nfe013.parcela     
               tt-item-doc-est.nr-ord-produ   = nfe013.nr-ord-produ
               tt-item-doc-est.qt-do-forn     = nfe013.qtd-comercial
               tt-item-doc-est.un             = nfe013.un-comercial
               tt-item-doc-est.quantidade     = nfe013.qtd-interna
               tt-item-doc-est.preco-unit[1]  = nfe013.preco-unit   
               tt-item-doc-est.preco-total[1] = nfe013.preco-total
               tt-item-doc-est.class-fiscal   = nfe013.class-fiscal
               tt-item-doc-est.log-1          = yes /*nfe013.log-fifo-oc*/
               tt-item-doc-est.log-fifo-oc    = yes /*nfe013.log-fifo-oc*/
               tt-item-doc-est.conta-contabil = nfe013.conta-contabil
               tt-item-doc-est.cod-refer      = nfe013.cod-refer     
               tt-item-doc-est.cod-depos      = nfe013.cod-depos     
               tt-item-doc-est.cod-localiz    = nfe013.cod-localiz   
               tt-item-doc-est.lote           = nfe013.lote          
               tt-item-doc-est.dt-vali-lote   = nfe013.dt-vali-lote
               tt-item-doc-est.cd-trib-icm    = 0
               tt-item-doc-est.cd-trib-ipi    = 0.
    END.

    RETURN "OK":U.
    
END PROCEDURE.

PROCEDURE pi-carrega-variaveis:
    

    ASSIGN l-sugere-serie = YES.

    /** CARREGA TT-VARIAVEL **/
    EMPTY TEMP-TABLE tt-variavel.

    FOR EACH  nfe003 FIELDS (cod-estabel serie-docto cod-observa cod-emitente ch-acesso-comp-nfe)
        WHERE nfe003.ch-acesso-comp-nfe = tt-nfe003.ch-acesso-comp-nfe
          AND nfe003.idi-orig-trad      = 2 NO-LOCK: 

         
        /** INICIALIZA VARIAVEIS **/
        ASSIGN c-serie-docto-novo = nfe003.serie-docto
               c-cod-observa-novo = nfe003.cod-observa.

        FOR EACH nfe013 FIELDS (it-codigo     num-pedido     numero-ordem  
                                           parcela       nr-ord-produ   qtd-comercial 
                                           un-comercial  qtd-interna    preco-unit    
                                           preco-total   class-fiscal   seq-item   
                                           log-fifo-oc   conta-contabil cod-refer     
                                           cod-depos     cod-localiz    lote          
                                           dt-vali-lote  cod-cfop       nat-operacao)
            WHERE nfe013.ch-acesso-comp-nfe = nfe003.ch-acesso-comp-nfe
              AND nfe013.idi-orig-trad      = 2 NO-LOCK: 

            EMPTY TEMP-TABLE tt-variavel NO-ERROR.

            CREATE tt-variavel.                                                           
            ASSIGN tt-variavel.cod-var-oper     = "serie-docto":U                         
                   tt-variavel.valor-variavel   = nfe003.serie-docto.               
                                                                                          
            CREATE tt-variavel.                                                           
            ASSIGN tt-variavel.cod-var-oper     = "cod-emitente":U                        
                   tt-variavel.valor-variavel   = string(nfe003.cod-emitente).      
                                                                                          
            CREATE tt-variavel.                                                           
            ASSIGN tt-variavel.cod-var-oper     = "cod-observa":U                         
                   tt-variavel.valor-variavel   = STRING(nfe003.cod-observa).       
                                                                                          
            CREATE tt-variavel.                                                           
            ASSIGN tt-variavel.cod-var-oper     = "cod-estabel":U                         
                   tt-variavel.valor-variavel   = nfe003.cod-estabel.               
                                                                                          
            CREATE tt-variavel.                                                           
            ASSIGN tt-variavel.cod-var-oper     = "it-codigo":U                           
                   tt-variavel.valor-variavel   = STRING(nfe013.it-codigo).    
                                                                                          
            CREATE tt-variavel.                                                           
            ASSIGN tt-variavel.cod-var-oper     = "nat-operacao":U                        
                   tt-variavel.valor-variavel   = nfe013.cod-cfop.             
                                                                                          
            CREATE tt-variavel.                                                           
            ASSIGN tt-variavel.cod-var-oper     = "log-fifo-oc":U                               
                   tt-variavel.valor-variavel   = STRING(nfe013.log-fifo-oc).  
                                                                                          
            CREATE tt-variavel.                                                           
            ASSIGN tt-variavel.cod-var-oper     = "numero-ordem":U                        
                   tt-variavel.valor-variavel   = STRING(nfe013.numero-ordem). 

            CREATE tt-variavel.                                                           
            ASSIGN tt-variavel.cod-var-oper     = "num-pedido":U                          
                   tt-variavel.valor-variavel   = STRING(nfe013.num-pedido).   
            
            CREATE tt-variavel.                                                           
            ASSIGN tt-variavel.cod-var-oper     = "parcela":U                             
                   tt-variavel.valor-variavel   = STRING(nfe013.parcela).      
                                                                                          
            CREATE tt-variavel.                                                           
            ASSIGN tt-variavel.cod-var-oper     = "class-fiscal":U                        
                   tt-variavel.valor-variavel   = nfe013.class-fiscal.         
                                                                                          
            CREATE tt-variavel.                                                           
            ASSIGN tt-variavel.cod-var-oper     = "nr-ord-produ":U                        
                   tt-variavel.valor-variavel   = STRING(nfe013.nr-ord-produ). 
                                                                                          
            CREATE tt-variavel.                                                           
            ASSIGN tt-variavel.cod-var-oper     = "conta-contabil":U                      
                   tt-variavel.valor-variavel   = nfe013.conta-contabil.       
                                                                                          
            CREATE tt-variavel.                                                           
            ASSIGN tt-variavel.cod-var-oper     = "cod-refer":U                           
                   tt-variavel.valor-variavel   = nfe013.cod-refer.            
                                                                                          
            CREATE tt-variavel.                                                           
            ASSIGN tt-variavel.cod-var-oper     = "cod-depos":U                           
                   tt-variavel.valor-variavel   = nfe013.cod-depos.            
                                                                                          
            CREATE tt-variavel.                                                           
            ASSIGN tt-variavel.cod-var-oper     = "cod-localiz":U                         
                   tt-variavel.valor-variavel   = nfe013.cod-localiz.          
                                                                                          
            CREATE tt-variavel.                                                           
            ASSIGN tt-variavel.cod-var-oper     = "lote":U                                
                   tt-variavel.valor-variavel   = nfe013.lote.                 
                                                                                          
            CREATE tt-variavel.                                                           
            ASSIGN tt-variavel.cod-var-oper     = "dt-vali-lote":U                        
                   tt-variavel.valor-variavel   = STRING(nfe013.dt-vali-lote). 

            /** INICIALIZA VARIAVEIS **/                                                  
            ASSIGN c-cod-natur-novo   = nfe013.nat-operacao                    
                   c-fifo-novo        = nfe013.log-fifo-oc                     
                   c-ordem-novo       = nfe013.numero-ordem                    
                   c-pedido-novo      = nfe013.num-pedido                      
                   c-parcela-novo     = nfe013.parcela                         
                   c-class-fisc-novo  = nfe013.class-fiscal                    
                   c-nr-ord-prod-novo = nfe013.nr-ord-produ                    
                   c-conta-novo       = nfe013.conta-contabil                  
                   c-refer-novo       = nfe013.cod-refer                       
                   c-depos-novo       = nfe013.cod-depos                       
                   c-localiz-novo     = nfe013.cod-localiz                     
                   c-lote-novo        = nfe013.lote                            
                   c-dt-lote-novo     = nfe013.dt-vali-lote.  
            
            RUN pi-buscaRelac (INPUT TABLE tt-variavel).
        END.                                                                            
    END. 
                                                                    
    RETURN "OK":U.                                                                      
                                                                                        
END PROCEDURE.                                                                          

PROCEDURE pi-BuscaRelac:                                                   
    DEF INPUT PARAM TABLE FOR tt-variavel.                             
                                                                       
    DEFINE VARIABLE c-cod-cfop AS CHARACTER   NO-UNDO.

    EMPTY TEMP-TABLE tt-retorno NO-ERROR.                              
    EMPTY TEMP-TABLE RowErrors  NO-ERROR. 
    
    FOR FIRST estabelec FIELDS(log-1)                                  
        WHERE estabelec.cod-estabel = nfe003.cod-estabel NO-LOCK:        
    END.                                                               
    IF AVAIL estabelec THEN                                            
        run cdp/cd9970.p (INPUT ROWID(estabelec), OUTPUT i-empresa).   
                                                                       
    find nfe008
        WHERE nfe008.cod-estabel = nfe003.cod-estabel
          and nfe008.cod-emitente = nfe003.cod-emitente NO-LOCK NO-ERROR.
    IF AVAIL nfe008 THEN DO:

        ASSIGN c-serie-docto-novo = nfe008.serie
               c-cod-natur-novo   = nfe008.nat-operacao
               c-conta-novo       = nfe008.conta-contabil.

         /*
        FIND FIRST Itemnfe008
            WHERE Itemnfe008.cod-emitente = nfe008.cod-emitente 
              AND Itemnfe008.it-codigo    = item-doc-orig-nf.it-codigo NO-LOCK NO-ERROR.
        IF AVAIL Itemnfe008 THEN
            ASSIGN c-fifo-novo        = Itemnfe008.log-fifo-oc
                   c-ordem-novo       = Itemnfe008.numero-ordem
                   c-pedido-novo      = Itemnfe008.num-pedido
                   c-parcela-novo     = Itemnfe008.parcela
                   c-class-fisc-novo  = Itemnfe008.class-fiscal
                   c-localiz-novo     = Itemnfe008.cod-localiz
                   c-depos-novo       = Itemnfe008.cod-depos
                   c-dt-lote-novo     = Itemnfe008.dt-vali-lote
                   c-lote-novo        = Itemnfe008.lote-serie
                   c-refer-novo       = Itemnfe008.cod-refer.     
         */           

    END.

    /*
    RUN esp/buscaRegistro.p (INPUT nfe003.cod-estabel,
                             INPUT nfe003.cod-emitente,
                             INPUT item-doc-orig-nf.it-codigo,
                             OUTPUT p-num-pedido,
                             OUTPUT p-numero-ordem,
                             OUTPUT p-parcela,
                             OUTPUT p-class-fisc,
                             OUTPUT p-cod-depos,
                             OUTPUT p-cod-localiz,
                             OUTPUT p-lote-serie,
                             OUTPUT p-dt-vali-lote,
                             OUTPUT p-cod-refer,
                             OUTPUT p-log-fifo-oc).
    
    ASSIGN c-fifo-novo        = p-log-fifo-oc      
           c-ordem-novo       = p-numero-ordem     
           c-pedido-novo      = p-num-pedido    
           c-parcela-novo     = p-parcela 
           c-class-fisc-novo  = p-class-fisc
           c-localiz-novo     = p-cod-localiz   
           c-depos-novo       = p-cod-depos     
           c-dt-lote-novo     = p-dt-vali-lote   
           c-lote-novo        = p-lote-serie      
           c-refer-novo       = p-cod-refer*/ .        


    /** ALTERA ITENS TRADUZIDOS **/
    FOR FIRST bf-nfe013 
        WHERE bf-nfe013.ch-acesso-comp-nfe = tt-nfe003.ch-acesso-comp-nfe
          AND bf-nfe013.idi-orig-trad      = 2 
          AND bf-nfe013.seq-item           = nfe013.seq-item EXCLUSIVE-LOCK:
    END.

    IF AVAIL bf-nfe013 THEN DO:

        /*FOR FIRST funcao 
            WHERE funcao.cd-funcao = "USA-MULT-NAT-RECEB"
              AND funcao.ativo NO-LOCK:    END.
        IF AVAIL funcao THEN DO:*/
            FIND FIRST es-natur-cfop NO-LOCK
				 WHERE es-natur-cfop.cod-cfop = bf-nfe013.cod-cfop use-index cod_seq no-error.
			IF AVAIL es-natur-cfop then 
				ASSIGN c-cod-natur-novo = es-natur-cfop.nat-operacao.
			ELSE DO:
				ASSIGN c-cod-cfop = IF bf-nfe013.cod-cfop BEGINS "5" THEN "1" + substring(bf-nfe013.cod-cfop, 2, LENGTH(bf-nfe013.cod-cfop)) ELSE bf-nfe013.cod-cfop.

				find first natur-oper
					 where natur-oper.cod-cfop = c-cod-cfop no-lock no-error.
				if avail natur-oper THEN
					assign c-cod-natur-novo = natur-oper.nat-operacao.
			END.
        /*END.*/  

        ASSIGN bf-nfe013.nat-operacao   = c-cod-natur-novo  
               bf-nfe013.log-fifo-oc    = c-fifo-novo       
               bf-nfe013.numero-ordem   = c-ordem-novo      
               bf-nfe013.num-pedido     = c-pedido-novo     
               bf-nfe013.parcela        = c-parcela-novo    
               bf-nfe013.class-fiscal   = c-class-fisc-novo 
               bf-nfe013.nr-ord-produ   = c-nr-ord-prod-novo
               /*bf-nfe013.conta-contabil = c-conta-novo*/      
               bf-nfe013.cod-refer      = c-refer-novo      
               bf-nfe013.cod-depos      = c-depos-novo      
               bf-nfe013.cod-localiz    = c-localiz-novo    
               bf-nfe013.lote           = c-lote-novo       
               bf-nfe013.dt-vali-lote   = c-dt-lote-novo.
    END.

    RELEASE bf-nfe013.

    IF c-serie-docto-novo <> '' THEN DO:
        FOR FIRST bf-nfe003 FIELDS (cod-estabel serie-docto cod-observa cod-emitente ch-acesso-comp-nfe)
            WHERE bf-nfe003.ch-acesso-comp-nfe = tt-nfe003.ch-acesso-comp-nfe
              AND bf-nfe003.idi-orig-trad      = 2 EXCLUSIVE-LOCK:
    
              ASSIGN bf-nfe003.serie-docto = c-serie-docto-novo .
    
        END.
    END.

    RELEASE bf-nfe003.
    
    RETURN "OK":U.

END PROCEDURE.
           
PROCEDURE pi-gera-erro:
    DEFINE INPUT PARAM pcd-erro   AS INTEGER             NO-UNDO.
    DEFINE INPUT PARAM pdesc-erro AS CHAR FORMAT "x(80)" NO-UNDO.

    CREATE nfe017.
    ASSIGN nfe017.ch-acesso-comp-nfe = tt-nfe003.ch-acesso-comp-nfe
           nfe017.idi-orig-trad      = 2
           nfe017.dt-msg             = TODAY
           nfe017.hr-msg             = string(TIME, "HH:MM:SS")
           nfe017.log-ativo          = YES
           nfe017.cd-msg             = pcd-erro 
           nfe017.texto-msg          = pdesc-erro
           nfe017.seq-msg            = NEXT-VALUE(seq-msg-nfe).

    RETURN "OK":U.

END PROCEDURE.

PROCEDURE pi-cria-nota:
    
    DEF OUTPUT PARAM TABLE FOR tt-erro. 

    DEFINE VARIABLE i-nro-docto  AS INTEGER FORMAT ">>>>>>>>>>>9" NO-UNDO.
    DEFINE VARIABLE de-tot-preco AS DECIMAL NO-UNDO.
    DEFINE VARIABLE de-tot-peso  AS DECIMAL NO-UNDO.

    ASSIGN i-nro-docto = int(tt-doc-fisico.nro-docto).

    DO TRANS:

        IF NOT CAN-FIND (FIRST emitente 
                         WHERE emitente.cod-emitente = tt-doc-fisico.cod-emitente ) THEN DO:
             RUN pi-cria-erro (INPUT '',
                               INPUT 2,
                               INPUT "Fornecedor " + STRING(tt-doc-fisico.cod-emitente) + " nÆo cadastrado.").
        END.
        
        IF NOT CAN-FIND(FIRST estabelec 
                        WHERE estabelec.cod-estabel = tt-doc-fisico.cod-estabel) THEN DO:
            RUN pi-cria-erro (INPUT '',
                              INPUT 2,
                              INPUT "Estabelecimento " + STRING(tt-doc-fisico.cod-estabel) + " nÆo cadastrado.").
        END.
       
        IF NOT CAN-FIND(FIRST serie 
                        WHERE serie.serie = tt-doc-fisico.serie-docto) THEN DO:
            RUN pi-cria-erro (INPUT '',
                              INPUT 2,
                              INPUT "S‚rie " + STRING(tt-doc-fisico.serie-docto) + " nÆo cadastrada.").
        END.
        
        IF tt-doc-fisico.cod-emitente = 0 THEN DO:
            RUN pi-cria-erro (INPUT '',
                              INPUT 6189,
                              INPUT "Emitente nÆo pode ser desconhecido (zero).").
        END.
        
        IF tt-doc-fisico.nro-docto = "0000000":U 
        OR tt-doc-fisico.nro-docto = "":U THEN DO:
            RUN pi-cria-erro (INPUT '',
                              INPUT 16686,
                              INPUT "N£mero de Documento deve ser diferente de zero").
        END.
    
        IF i-nro-docto > 999999 THEN DO:
            RUN pi-cria-erro (INPUT '',
                              INPUT 18247,
                              INPUT "N£mero de Documento maior que 999999").
        END. 
    
        if tt-doc-fisico.dt-emissao < (TODAY - param-re.var-emis) THEN DO:
            RUN pi-cria-erro (INPUT '',
                              INPUT 8824,
                              INPUT "Data de EmissÆo " + STRING(tt-doc-fisico.dt-emissao) + " fora da varia‡Æo permitida.").
        END.
    
        IF tt-doc-fisico.dt-trans < (TODAY - param-re.var-atual) THEN DO:
            RUN pi-cria-erro (INPUT '',
                              INPUT 8824,
                              INPUT "Data Transa‡Æo " + STRING(tt-doc-fisico.dt-trans) + " fora da varia‡Æo permitida.").
        END.
    
        IF tt-doc-fisico.dt-trans > TODAY THEN DO:
            RUN pi-cria-erro (INPUT '',
                              INPUT 1788,
                              INPUT "Data transa‡Æo " + STRING(tt-doc-fisico.dt-trans) + " deve ser maior que a data corrente.").
        END.
    
        if tt-doc-fisico.dt-trans  < tt-doc-fisico.dt-emissao THEN DO:
            RUN pi-cria-erro (INPUT '',
                              INPUT 89,
                              INPUT "Data transa‡Æo menor que a data de emissÆo.").
        END .
    
        IF param-estoq.tp-fech = 2 THEN DO:
             FIND estab-mat 
                 WHERE estab-mat.cod-estabel = tt-doc-fisico.cod-estabel NO-LOCK NO-ERROR.
             IF AVAIL estab-mat AND tt-doc-fisico.dt-trans <= estab-mat.mensal-ate THEN DO:
                  RUN pi-cria-erro (INPUT '',
                                    INPUT 1586,
                                    INPUT "Data de Transa‡Æo deve ser maior que £ltimo m‚dio calculado.").
    
             END.
             ELSE  
                IF param-estoq.log-1 AND tt-doc-fisico.dt-trans <= param-estoq.mensal-ate THEN DO:
                    RUN pi-cria-erro (INPUT '',
                                      INPUT 1586,
                                      INPUT "Data de Transa‡Æo deve ser maior que £ltimo m‚dio calculado.").
                END.
        END.
    
        FIND FIRST doc-fisico
             WHERE doc-fisico.serie-docto  = tt-doc-fisico.serie-docto
               AND doc-fisico.nro-docto    = tt-doc-fisico.nro-docto
               AND doc-fisico.cod-emitente = tt-doc-fisico.cod-emitente
               AND doc-fisico.tipo-nota    = 1 NO-LOCK NO-ERROR.
    
        IF AVAIL doc-fisico THEN DO:
            RUN pi-cria-erro (INPUT '',
                              INPUT 1,
                              INPUT "Registro duplicado.").
            
        END.
        
        ELSE DO:
            CREATE doc-fisico.
            ASSIGN doc-fisico.tipo-nota    = tt-doc-fisico.tipo-nota        
                   doc-fisico.nro-docto    = tt-doc-fisico.nro-docto        
                   doc-fisico.serie-docto  = tt-doc-fisico.serie-docto      
                   doc-fisico.dt-emissao   = tt-doc-fisico.dt-emissao       
                   doc-fisico.dt-trans     = tt-doc-fisico.dt-trans         
                   doc-fisico.situacao     = tt-doc-fisico.situacao        
                   doc-fisico.valor-outras = tt-doc-fisico.valor-outras     
                   doc-fisico.despesa-nota = tt-doc-fisico.despesa-nota     
                   doc-fisico.tot-peso     = tt-doc-fisico.tot-peso         
                   doc-fisico.tot-desconto = tt-doc-fisico.tot-desconto     
                   doc-fisico.valor-mercad = tt-doc-fisico.valor-mercad     
                   doc-fisico.cod-estabel  = tt-doc-fisico.cod-estabel      
                   doc-fisico.tot-desconto = tt-doc-fisico.tot-desconto     
                   doc-fisico.valor-frete  = tt-doc-fisico.valor-frete      
                   doc-fisico.valor-seguro = tt-doc-fisico.valor-seguro     
                   doc-fisico.valor-mercad = tt-doc-fisico.valor-mercad
                   doc-fisico.cod-emitente = tt-doc-fisico.cod-emitente.
        
            ASSIGN de-tot-preco = 0
                   de-tot-peso  = 0.

            FOR EACH tt-it-doc-fisico:

                ASSIGN de-tot-preco = de-tot-preco + tt-it-doc-fisico.preco-total[1]
                       de-tot-peso  = de-tot-peso  + tt-it-doc-fisico.peso-liquido.
        
                FIND FIRST rat-lote
                    WHERE rat-lote.serie-docto  = tt-it-doc-fisico.serie-docto
                      AND rat-lote.nro-docto    = tt-it-doc-fisico.nro-docto
                      AND rat-lote.cod-emitente = tt-it-doc-fisico.cod-emitente
                      AND rat-lote.nat-operacao = ""
                      AND rat-lote.sequencia    = tt-it-doc-fisico.sequencia NO-LOCK NO-ERROR.
                IF AVAIL rat-lote THEN DO:

                    CREATE rat-lote.
                    ASSIGN rat-lote.cod-emitente = tt-it-doc-fisico.cod-emitente
                           rat-lote.serie-docto  = tt-it-doc-fisico.serie-docto
                           rat-lote.nro-docto    = tt-it-doc-fisico.nro-docto
                           rat-lote.it-codigo    = tt-it-doc-fisico.it-codigo
                           rat-lote.nat-operacao = ""
                           rat-lote.tipo-nota    = tt-it-doc-fisico.tipo-nota
                           rat-lote.sequencia    = tt-it-doc-fisico.sequencia
                           rat-lote.quantidade   = tt-it-doc-fisico.quantidade
                           rat-lote.cod-depos    = tt-it-doc-fisico.cod-depos
                           rat-lote.lote         = tt-it-doc-fisico.lote.
                END.
    
                FIND item 
                    WHERE item.it-codigo = tt-it-doc-fisico.it-codigo NO-LOCK NO-ERROR.
                IF NOT AVAILABLE item THEN DO:
                    RUN pi-cria-erro (INPUT '',
                                      INPUT 47,
                                      INPUT "Item" + STRING(tt-it-doc-fisico.it-codigo) + " nÆo cadastrado.").
                END.
    
                /** Verifica Item x Referˆncia - CD1506 **/
                IF AVAIL item AND item.tipo-con-est = 4 /* Referˆncia */ 
                AND NOT CAN-FIND(FIRST ref-item 
                                 WHERE ref-item.it-codigo = tt-it-doc-fisico.it-codigo 
                                   AND ref-item.cod-refer = tt-it-doc-fisico.cod-refer) THEN DO:   
    
                    RUN pi-cria-erro (INPUT '',
                                      INPUT 33928,
                                      INPUT "Referˆncia nÆo cadastrada para o item " + STRING(tt-it-doc-fisico.it-codigo)).
    
                END.
    
                FIND FIRST it-doc-fisico
                    WHERE it-doc-fisico.serie-docto  = tt-it-doc-fisico.serie-docto
                      AND it-doc-fisico.nro-docto    = tt-it-doc-fisico.nro-docto
                      AND it-doc-fisico.cod-emitente = tt-it-doc-fisico.cod-emitente
                      AND it-doc-fisico.tipo-nota    = 1
                      AND it-doc-fisico.sequencia    = tt-it-doc-fisico.sequencia NO-LOCK NO-ERROR.
                IF AVAIL it-doc-fisico THEN DO:
                    RUN pi-cria-erro (INPUT '',
                                      INPUT 424,
                                      INPUT "Sequencia " + STRING(tt-it-doc-fisico.sequencia) +  " do item " + STRING(tt-it-doc-fisico.it-codigo) + "ja cadastrada.").
                END.

                ELSE DO:
                    
                    CREATE it-doc-fisico.
                    ASSIGN it-doc-fisico.serie-docto    = tt-it-doc-fisico.serie-docto
                           it-doc-fisico.nro-docto      = tt-it-doc-fisico.nro-docto
                           it-doc-fisico.cod-emitente   = tt-it-doc-fisico.cod-emitente
                           it-doc-fisico.tipo-nota      = tt-it-doc-fisico.tipo-nota
                           it-doc-fisico.sequencia      = tt-it-doc-fisico.sequencia
                           it-doc-fisico.num-pedido     = tt-it-doc-fisico.num-pedido
                           it-doc-fisico.it-codigo      = tt-it-doc-fisico.it-codigo
                           it-doc-fisico.un             = tt-it-doc-fisico.un
                           it-doc-fisico.quantidade     = tt-it-doc-fisico.quantidade
                           it-doc-fisico.qt-do-forn     = tt-it-doc-fisico.qt-do-forn
                           it-doc-fisico.preco-unit     = tt-it-doc-fisico.preco-unit[1]
                           it-doc-fisico.desconto       = tt-it-doc-fisico.desconto[1]
                           it-doc-fisico.preco-total    = tt-it-doc-fisico.preco-total[1]
                           it-doc-fisico.numero-ordem   = tt-it-doc-fisico.numero-ordem
                           it-doc-fisico.parcela        = tt-it-doc-fisico.parcela
                           it-doc-fisico.conta-contabil = tt-it-doc-fisico.conta-contabil.
                END.
                
            END.
            ASSIGN doc-fisico.tot-peso     = de-tot-peso
                   doc-fisico.valor-mercad = de-tot-preco.
            
        END.
    END.
END PROCEDURE.
OUTPUT CLOSE.









