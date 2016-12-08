&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DBOProgram 
/*:T--------------------------------------------------------------------------
    File       : dbo.p
    Purpose    : O DBO (Datasul Business Objects) ‚ um programa PROGRESS 
                 que cont‚m a l¢gica de neg¢cio e acesso a dados para uma 
                 tabela do banco de dados.

    Parameters : 

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  **************************** */

/*:T--- Diretrizes de defini‡Æo ---*/
&GLOBAL-DEFINE DBOName BONFE013
&GLOBAL-DEFINE DBOVersion 
&GLOBAL-DEFINE DBOCustomFunctions 
&GLOBAL-DEFINE TableName nfe013
&GLOBAL-DEFINE TableLabel 
&GLOBAL-DEFINE QueryName qr{&TableName} 

/* DBO-XML-BEGIN */
/*:T Pre-processadores para ativar XML no DBO */
/*:T Retirar o comentario para ativar 
&GLOBAL-DEFINE XMLProducer YES    /*:T DBO atua como producer de mensagens para o Message Broker */
&GLOBAL-DEFINE XMLTopic           /*:T Topico da Mensagem enviada ao Message Broker, geralmente o nome da tabela */
&GLOBAL-DEFINE XMLTableName       /*:T Nome da tabela que deve ser usado como TAG no XML */ 
&GLOBAL-DEFINE XMLTableNameMult   /*:T Nome da tabela no plural. Usado para multiplos registros */ 
&GLOBAL-DEFINE XMLPublicFields    /*:T Lista dos campos (c1,c2) que podem ser enviados via XML. Ficam fora da listas os campos de especializacao da tabela */ 
&GLOBAL-DEFINE XMLKeyFields       /*:T Lista dos campos chave da tabela (c1,c2) */
&GLOBAL-DEFINE XMLExcludeFields   /*:T Lista de campos a serem excluidos do XML quando PublicFields = "" */

&GLOBAL-DEFINE XMLReceiver YES    /*:T DBO atua como receiver de mensagens enviado pelo Message Broker (m‚todo Receive Message) */
&GLOBAL-DEFINE QueryDefault       /*:T Nome da Query que d  acessos a todos os registros, exceto os exclu¡dos pela constraint de seguran‡a. Usada para receber uma mensagem XML. */
&GLOBAL-DEFINE KeyField1 cust-num /*:T Informar os campos da chave quando o Progress nÆo conseguir resolver find {&TableName} OF RowObject. */
*/
/* DBO-XML-END */

/*:T--- Include com defini‡Æo da temptable RowObject ---*/
/*:T--- Este include deve ser copiado para o diret¢rio do DBO e, ainda, seu nome
      deve ser alterado a fim de ser idˆntico ao nome do DBO mas com 
      extensÆo .i ---*/
{esbo/boes011.i RowObject}
{utp/ut-glob.i}
{cdp/cdcfgmat.i}

&IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
    {cdp/cd9590.i}
&ENDIF

/*:T--- Include com defini‡Æo da query para tabela {&TableName} ---*/
/*:T--- Em caso de necessidade de altera‡Æo da defini‡Æo da query, pode ser retirada
      a chamada ao include a seguir e em seu lugar deve ser feita a defini‡Æo 
      manual da query ---*/
{method/dboqry.i}


/*:T--- Defini‡Æo de buffer que ser  utilizado pelo m‚todo goToKey ---*/
DEFINE BUFFER bf{&TableName} FOR {&TableName}.


DEFINE VARIABLE c-chave-acesso    LIKE nfe013.ch-acesso-comp-nfe NO-UNDO.
DEFINE VARIABLE i-idi-origem      LIKE nfe013.idi-orig-trad      NO-UNDO.
DEFINE VARIABLE l-pendente          AS LOGICAL                              NO-UNDO.
DEFINE VARIABLE de-qtd-fornec-receb AS DECIMAL                              NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DBOProgram
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DBOProgram
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW DBOProgram ASSIGN
         HEIGHT             = 11.29
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "DBO 2.0 Wizard" DBOProgram _INLINE
/* Actions: wizard/dbowizard.w ? ? ? ? */
/* DBO 2.0 Wizard (DELETE)*/
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB DBOProgram 
/* ************************* Included-Libraries *********************** */

{method/dbo.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DBOProgram 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calculaDataEntrega DBOProgram 
PROCEDURE calculaDataEntrega :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM pinumero-ordem  LIKE ordem-compra.numero-ordem NO-UNDO.
    DEF OUTPUT PARAM piDtEntrega     AS CHAR                        NO-UNDO.   
    
    DEF VAR i-cont                   AS INT   NO-UNDO.
    
    ASSIGN i-cont = 0.
    FOR EACH prazo-compra  FIELDS (data-entrega)
       WHERE prazo-compra.numero-ordem = pinumero-ordem NO-LOCK:

        ASSIGN i-cont = i-cont + 1.

        IF  i-cont = 1 THEN
            ASSIGN piDtEntrega = STRING(prazo-compra.data-entrega,"99/99/9999").
        IF  i-cont = 2 THEN DO:
            ASSIGN piDtEntrega = "PARCELADA".
            LEAVE.
        END.
    END.
    

    RETURN "OK":U.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calculaPrecoOC DBOProgram 
PROCEDURE calculaPrecoOC :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  Retorna o pre?o unitÿrio do item atrav?s de sua ordem compra.      
------------------------------------------------------------------------------*/

    DEF INPUT  PARAM r-ordem-compra AS ROWID   NO-UNDO.
    DEF INPUT  PARAM r-item         AS ROWID   NO-UNDO.
    DEF INPUT  PARAM p-dt-emissao   AS DATE    NO-UNDO.
    DEF OUTPUT PARAM de-preco-ordem AS DECIMAL NO-UNDO.
    
    def var de-val-contrato         as decimal no-undo.
    def var de-val-formula          as decimal no-undo.
    def var de-taxa                 as decimal no-undo.
    def var de-preco-cc             as decimal no-undo.
    def var de-desconto-cc          as decimal no-undo.
    def var de-val-aux              as decimal no-undo.
    DEF VAR da-dt-emis              AS DATE    NO-UNDO.

    FOR FIRST ordem-compra
        WHERE ROWID(ordem-compra) = r-ordem-compra NO-LOCK:
    END.

    FOR FIRST bf{&TableName}
        WHERE ROWID(bf{&TableName}) = r-item NO-LOCK:
    END.
    
    if  not avail nfe013
    or  not avail ordem-compra then
        return "NOK":U.

    FIND FIRST param-global NO-LOCK NO-ERROR.
    
    IF da-dt-emis = ? THEN
        assign da-dt-emis = p-dt-emissao.

    /*  Cÿlculo Pre?o Unitÿrio */
    run rep/re1908.p (rowid(ordem-compra),
                      da-dt-emis,
                      output de-preco-cc,  
                      output de-desconto-cc,
                      output de-taxa).

                               
    if  param-global.modulo-cn 
    and ordem-compra.nr-contrato <> 0
    and avail RowObject then do:
        run cnp/cn9060.p (&IF "{&bf_mat_versao_ems}":U >= "2.062":U &THEN
                              INPUT bf{&TableName}.int-1,
                          &ENDIF
                          ?,
                          ordem-compra.numero-ordem,
                          0,
                          no,
                          bf{&TableName}.qtd-interna,
                          p-dt-emissao,
                          output de-val-contrato,
                          output de-val-aux,
                          output de-val-formula,
                          output de-val-aux).                          
        if  de-val-formula > 0 then
            assign de-preco-cc = de-val-formula.
        else
            if  de-val-contrato > 0 then
                assign de-preco-cc = de-val-contrato.
    end.       

    assign de-preco-ordem = de-preco-cc - de-desconto-cc.                                       

    if  de-qtd-fornec-receb > 0 then 
        assign de-preco-ordem = ( de-preco-ordem + de-desconto-cc ) * de-qtd-fornec-receb.
      
    return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calculaQtdSaldo DBOProgram 
PROCEDURE calculaQtdSaldo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def input  param l-cons-dt-entrega      as logical no-undo.
    def input  param l-calcula-qtd-saldo    as logical no-undo.
    def input  param p-numero-ordem         as INT     no-undo.
    def output param de-qtd-rec-forn        as decimal no-undo.
    def output param de-qtd-receber         as decimal no-undo.
    

    def var de-indice-aux                   as decimal no-undo.
    

    for each prazo-compra           
       fields ( numero-ordem    parcela         quant-saldo
                situacao        data-entrega    dec-1
                quant-rejei     qtd-sal-for     quantidade
                quant-receb     cod-refer                   )
       where prazo-compra.numero-ordem = p-numero-ordem
         and prazo-compra.situacao     =  2                 /* CONFIRMADA */
         and ( prazo-compra.quant-saldo - prazo-compra.dec-1 ) > 0 no-lock 
       break by parcela:
             

        if  first(parcela) then 
            assign de-indice-aux = prazo-compra.qtd-sal-forn 
                                 / prazo-compra.quant-saldo
                   de-indice-aux = if  de-indice-aux = 0 or de-indice-aux = ? then 1
                                   else de-indice-aux.
             

        if  l-cons-dt-entrega 
        and not l-calcula-qtd-saldo 
        and ( prazo-compra.data-entrega > docum-est.dt-trans + param-re.variacao) then
            next.

        /* DIMINUI QUANTIDADE ALOCADA PARA CALCULAR A QUANTIDADE A RETORNAR.*/
        assign de-qtd-receber = de-qtd-receber
                                    + ( prazo-compra.quant-saldo - prazo-compra.dec-1 ).
    end.

    /* Calcula Quantidade a Receber na Unidade Medida do Fornecedor */
    assign de-qtd-rec-forn = de-qtd-receber * de-indice-aux.

    /* Atualiza Quantidade para Calculo Preco Total */
    if  not l-calcula-qtd-saldo then 
        assign de-qtd-fornec-receb = de-qtd-rec-forn.
    
    
    return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableFields DBOProgram 
PROCEDURE enableFields :
/*------------------------------------------------------------------------------
  Purpose:     retornar se campo deve ou nÆo ser habilitado
  Parameters:  pFieldName   -> atributo da tabela nfe013
               pFieldEnable -> habilita ou nÆo o atributo
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  pFieldName   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pFieldEnable AS LOGICAL   NO-UNDO.

    IF NOT AVAIL nfe003 THEN
        RETURN "NOK":U.

    IF NOT AVAIL natur-oper THEN
        RETURN "NOK":U.

    CASE pFieldName:
        WHEN "numero-ordem":U THEN
            ASSIGN pFieldEnable =      NOT natur-oper.transf
                                  AND (natur-oper.tp-oper-terc = 3 OR NOT natur-oper.terceiros)
                                  AND  param-global.modulo-cc.
        WHEN "num-pedido":U THEN
            ASSIGN pFieldEnable =      NOT natur-oper.transf
                                  AND (natur-oper.tp-oper-terc = 3 OR NOT natur-oper.terceiros)
                                  AND  param-global.modulo-cc.
        WHEN "log-fifo-oc":U THEN
            ASSIGN pFieldEnable = AVAIL param-re
                                  AND param-re.log-1  /*Fifo*/
                                  AND NOT natur-oper.transf
                                  AND NOT natur-oper.nota-rateio 
                                  AND (natur-oper.tp-oper-terc = 3 OR NOT natur-oper.terceiros)
                                  AND  param-global.modulo-cc.
        WHEN "qtd-interna":U THEN
            ASSIGN pFieldEnable =      NOT natur-oper.transf
                                  AND (NOT natur-oper.terceiros OR natur-oper.tp-oper-terc <> 6).
        WHEN "preco-unit":U THEN
            ASSIGN pFieldEnable =      NOT natur-oper.transf
                                  AND (    NOT natur-oper.terceiros 
                                       OR  INT(SUBSTR(natur-oper.char-2,36,5)) = 1
                                       OR (    natur-oper.tp-oper-terc <> 2 
                                           AND natur-oper.tp-oper-terc <> 4 
                                           AND natur-oper.tp-oper-terc <> 5))
                                  AND AVAIL param-re 
                                  AND param-re.preco-unit.
        WHEN "preco-total":U THEN
            ASSIGN pFieldEnable =      NOT natur-oper.transf
                                  AND (    NOT natur-oper.terceiros 
                                       OR  INT(SUBSTR(natur-oper.char-2,36,5)) = 1
                                       OR (    natur-oper.tp-oper-terc <> 2 
                                           AND natur-oper.tp-oper-terc <> 4 
                                           AND natur-oper.tp-oper-terc <> 5)).
        WHEN "class-fiscal":U THEN
            ASSIGN pFieldEnable =      AVAIL param-re
                                  AND (   param-re.pd-cf-item
                                       OR (AVAIL ITEM AND ITEM.tipo-contr = 4)).
        WHEN "conta-contabil":U THEN
            ASSIGN pFieldEnable = AVAIL RowObject
                                  AND RowObject.nr-ord-prod = 0 
                                  AND AVAIL natur-oper 
                                  AND (   nfe003.cod-observa <> 3 /* Devolu‡Æo */
                                       OR ( AVAIL ITEM AND CAN-DO("1,4", STRING(ITEM.tipo-contr)) ) 
                                       OR ( natur-oper.terceiros AND NOT CAN-DO("4,6", STRING(natur-oper.tp-oper-terc)) )).
        WHEN "nr-ord-produ":U THEN
            ASSIGN pFieldEnable =     ( param-global.modulo-cp   OR param-global.modulo-mi ) 
                                  AND ( not natur-oper.terceiros OR NOT CAN-DO("4,6", STRING(natur-oper.tp-oper-terc)) ).
        
        &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
            WHEN "int-1":U THEN
                ASSIGN pFieldEnable = param-global.modulo-cn.

            WHEN "cod-unid-negoc":U THEN 
                ASSIGN pFieldEnable =  IF  (AVAIL param-re 
                                       AND param-re.log-pedir-unid-negoc 
                                       AND l-mat-unid-negoc) THEN YES 
                                       ELSE NO.
        &ENDIF
    END CASE.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE findTabelas DBOProgram 
PROCEDURE findTabelas :
/*------------------------------------------------------------------------------
  Purpose:     Inicializar tabelas para procedure enableFields    
  Parameters:  prParent -> rowid tabela nfe003 (PAI)
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER prParent AS ROWID NO-UNDO. 
    DEFINE INPUT PARAMETER pNatOper AS CHAR  NO-UNDO.

    FIND FIRST param-global NO-LOCK NO-ERROR.

    FOR FIRST nfe003 NO-LOCK
        WHERE ROWID(nfe003) = prParent:
    END.

    FOR FIRST natur-oper FIELDS (transf tp-oper-terc terceiros char-2)
        WHERE natur-oper.nat-operacao = pNatOper NO-LOCK:
    END.
    
    FOR FIRST param-re /*FIELDS (preco-unit pd-cf-item log-1
                               &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN log-pedir-unid-negoc &ENDIF ) */
        WHERE param-re.usuario = c-seg-usuario NO-LOCK :
    END.

    FOR FIRST ITEM FIELDS (tipo-contr)
        WHERE ITEM.it-codigo = RowObject.it-codigo NO-LOCK:
    END.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCharField DBOProgram 
PROCEDURE getCharField :
/*------------------------------------------------------------------------------
  Purpose:     Retorna valor de campos do tipo caracter
  Parameters:  
               recebe nome do campo
               retorna valor do campo
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pFieldName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pFieldValue AS CHARACTER NO-UNDO.

    /*--- Verifica se temptable RowObject est  dispon¡vel, caso nÆo esteja ser 
          retornada flag "NOK":U ---*/
    IF NOT AVAILABLE RowObject THEN 
        RETURN "NOK":U.

    CASE pFieldName:
        WHEN "aliq-cofins-cst":U THEN ASSIGN pFieldValue = RowObject.aliq-cofins-cst.
        WHEN "aliq-pis-cst":U THEN ASSIGN pFieldValue = RowObject.aliq-pis-cst.
        WHEN "ano-fab":U THEN ASSIGN pFieldValue = RowObject.ano-fab.
        WHEN "ano-mod":U THEN ASSIGN pFieldValue = RowObject.ano-mod.
        WHEN "ch-acesso-comp-nfe":U THEN ASSIGN pFieldValue = RowObject.ch-acesso-comp-nfe.
        WHEN "class-ipi":U THEN ASSIGN pFieldValue = RowObject.class-ipi.
        WHEN "cmkg":U THEN ASSIGN pFieldValue = RowObject.cmkg.
        WHEN "cnpj-produtor":U THEN ASSIGN pFieldValue = RowObject.cnpj-produtor.
        WHEN "cod-cfop":U THEN ASSIGN pFieldValue = RowObject.cod-cfop.
        WHEN "cod-chassi":U THEN ASSIGN pFieldValue = RowObject.cod-chassi.
        WHEN "cod-codif":U THEN ASSIGN pFieldValue = RowObject.cod-codif.
        WHEN "cod-cond-veic":U THEN ASSIGN pFieldValue = RowObject.cod-cond-veic.
        WHEN "cod-cor-veic":U THEN ASSIGN pFieldValue = RowObject.cod-cor-veic.
        WHEN "cod-ean":U THEN ASSIGN pFieldValue = RowObject.cod-ean.
        WHEN "cod-ean-trib":U THEN ASSIGN pFieldValue = RowObject.cod-ean-trib.
        WHEN "cod-enq-ipi":U THEN ASSIGN pFieldValue = RowObject.cod-enq-ipi.
        WHEN "cod-ext-ipi":U THEN ASSIGN pFieldValue = RowObject.cod-ext-ipi.
        WHEN "cod-genero":U THEN ASSIGN pFieldValue = RowObject.cod-genero.
        WHEN "cod-lista-serv-iss":U THEN ASSIGN pFieldValue = RowObject.cod-lista-serv-iss.
        WHEN "cod-marca-mod":U THEN ASSIGN pFieldValue = RowObject.cod-marca-mod.
        WHEN "cod-ncm":U THEN ASSIGN pFieldValue = RowObject.cod-ncm.
        WHEN "cod-prod-anp":U THEN ASSIGN pFieldValue = RowObject.cod-prod-anp.
        WHEN "cod-selo-ipi":U THEN ASSIGN pFieldValue = RowObject.cod-selo-ipi.
        WHEN "cod-sit-trib-ipi":U THEN ASSIGN pFieldValue = RowObject.cod-sit-trib-ipi.
        WHEN "cod-sit-trib-ipi-int":U THEN ASSIGN pFieldValue = RowObject.cod-sit-trib-ipi-int.
        WHEN "cod-vin":U THEN ASSIGN pFieldValue = RowObject.cod-vin.
        WHEN "cofins-int-cst":U THEN ASSIGN pFieldValue = RowObject.cofins-int-cst.
        WHEN "cofins-outros-cst":U THEN ASSIGN pFieldValue = RowObject.cofins-outros-cst.
        WHEN "cofins-qtd-cst":U THEN ASSIGN pFieldValue = RowObject.cofins-qtd-cst.
        WHEN "desc-cor-veic":U THEN ASSIGN pFieldValue = RowObject.desc-cor-veic.
        WHEN "desc-item":U THEN ASSIGN pFieldValue = RowObject.desc-item.
        WHEN "dist-eixos":U THEN ASSIGN pFieldValue = RowObject.dist-eixos.
        WHEN "esp-veiculo":U THEN ASSIGN pFieldValue = RowObject.esp-veiculo.
        WHEN "icms-cst[1]":U THEN ASSIGN pFieldValue = RowObject.icms-cst[1].
        WHEN "icms-cst[2]":U THEN ASSIGN pFieldValue = RowObject.icms-cst[2].
        WHEN "icms-cst[3]":U THEN ASSIGN pFieldValue = RowObject.icms-cst[3].
        WHEN "icms-cst[4]":U THEN ASSIGN pFieldValue = RowObject.icms-cst[4].
        WHEN "icms-cst[5]":U THEN ASSIGN pFieldValue = RowObject.icms-cst[5].
        WHEN "icms-cst[6]":U THEN ASSIGN pFieldValue = RowObject.icms-cst[6].
        WHEN "icms-cst[7]":U THEN ASSIGN pFieldValue = RowObject.icms-cst[7].
        WHEN "icms-cst[8]":U THEN ASSIGN pFieldValue = RowObject.icms-cst[8].
        WHEN "icms-cst[9]":U THEN ASSIGN pFieldValue = RowObject.icms-cst[9].
        WHEN "icms-mod-bc[1]":U THEN ASSIGN pFieldValue = RowObject.icms-mod-bc[1].
        WHEN "icms-mod-bc[2]":U THEN ASSIGN pFieldValue = RowObject.icms-mod-bc[2].
        WHEN "icms-mod-bc[3]":U THEN ASSIGN pFieldValue = RowObject.icms-mod-bc[3].
        WHEN "icms-mod-bc[4]":U THEN ASSIGN pFieldValue = RowObject.icms-mod-bc[4].
        WHEN "icms-mod-bc[5]":U THEN ASSIGN pFieldValue = RowObject.icms-mod-bc[5].
        WHEN "icms-mod-bc[6]":U THEN ASSIGN pFieldValue = RowObject.icms-mod-bc[6].
        WHEN "icms-orig[1]":U THEN ASSIGN pFieldValue = RowObject.icms-orig[1].
        WHEN "icms-orig[2]":U THEN ASSIGN pFieldValue = RowObject.icms-orig[2].
        WHEN "icms-orig[3]":U THEN ASSIGN pFieldValue = RowObject.icms-orig[3].
        WHEN "icms-orig[4]":U THEN ASSIGN pFieldValue = RowObject.icms-orig[4].
        WHEN "icms-orig[5]":U THEN ASSIGN pFieldValue = RowObject.icms-orig[5].
        WHEN "icms-orig[6]":U THEN ASSIGN pFieldValue = RowObject.icms-orig[6].
        WHEN "icms-orig[7]":U THEN ASSIGN pFieldValue = RowObject.icms-orig[7].
        WHEN "icms-orig[8]":U THEN ASSIGN pFieldValue = RowObject.icms-orig[8].
        WHEN "icms-orig[9]":U THEN ASSIGN pFieldValue = RowObject.icms-orig[9].
        WHEN "icms10-mod-bc-st":U THEN ASSIGN pFieldValue = RowObject.icms10-mod-bc-st.
        WHEN "icms30-mod-bc-st":U THEN ASSIGN pFieldValue = RowObject.icms30-mod-bc-st.
        WHEN "icms70-mod-bc-st":U THEN ASSIGN pFieldValue = RowObject.icms70-mod-bc-st.
        WHEN "icms70-mod-bc-st-1":U THEN ASSIGN pFieldValue = RowObject.icms70-mod-bc-st-1.
        WHEN "icms90-mod-bc-st":U THEN ASSIGN pFieldValue = RowObject.icms90-mod-bc-st.
        WHEN "inf-complementar":U THEN ASSIGN pFieldValue = RowObject.inf-complementar.
        WHEN "it-codigo":U THEN ASSIGN pFieldValue = RowObject.it-codigo.
        WHEN "num-motor":U THEN ASSIGN pFieldValue = RowObject.num-motor.
        WHEN "num-serie":U THEN ASSIGN pFieldValue = RowObject.num-serie.
        WHEN "peso-bruto":U THEN ASSIGN pFieldValue = RowObject.peso-bruto.
        WHEN "peso-liq":U THEN ASSIGN pFieldValue = RowObject.peso-liq.
        WHEN "pis-int-cst":U THEN ASSIGN pFieldValue = RowObject.pis-int-cst.
        WHEN "pis-outros-cst":U THEN ASSIGN pFieldValue = RowObject.pis-outros-cst.
        WHEN "pis-qtd-cst":U THEN ASSIGN pFieldValue = RowObject.pis-qtd-cst.
        WHEN "pot-cm3":U THEN ASSIGN pFieldValue = RowObject.pot-cm3.
        WHEN "pot-motor":U THEN ASSIGN pFieldValue = RowObject.pot-motor.
        WHEN "qtd-selo-ipi":U THEN ASSIGN pFieldValue = RowObject.qtd-selo-ipi.
        WHEN "renavam":U THEN ASSIGN pFieldValue = RowObject.renavam.
        WHEN "tp-combustivel":U THEN ASSIGN pFieldValue = RowObject.tp-combustivel.
        WHEN "tp-oper-veic":U THEN ASSIGN pFieldValue = RowObject.tp-oper-veic.
        WHEN "tp-pintura":U THEN ASSIGN pFieldValue = RowObject.tp-pintura.
        WHEN "tp-veiculo":U THEN ASSIGN pFieldValue = RowObject.tp-veiculo.
        WHEN "uf-cons":U THEN ASSIGN pFieldValue = RowObject.uf-cons.
        WHEN "un-comercial":U THEN ASSIGN pFieldValue = RowObject.un-comercial.
        WHEN "un-trib":U THEN ASSIGN pFieldValue = RowObject.un-trib.
        OTHERWISE RETURN "NOK":U.
    END CASE.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getDateField DBOProgram 
PROCEDURE getDateField :
/*------------------------------------------------------------------------------
  Purpose:     Retorna valor de campos do tipo data
  Parameters:  
               recebe nome do campo
               retorna valor do campo
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pFieldName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pFieldValue AS DATE NO-UNDO.

    /*--- Verifica se temptable RowObject est  dispon¡vel, caso nÆo esteja ser 
          retornada flag "NOK":U ---*/
    IF NOT AVAILABLE RowObject THEN 
        RETURN "NOK":U.

    CASE pFieldName:
        OTHERWISE RETURN "NOK":U.
    END CASE.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getDecField DBOProgram 
PROCEDURE getDecField :
/*------------------------------------------------------------------------------
  Purpose:     Retorna valor de campos do tipo decimal
  Parameters:  
               recebe nome do campo
               retorna valor do campo
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pFieldName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pFieldValue AS DECIMAL NO-UNDO.

    /*--- Verifica se temptable RowObject est  dispon¡vel, caso nÆo esteja ser 
          retornada flag "NOK":U ---*/
    IF NOT AVAILABLE RowObject THEN 
        RETURN "NOK":U.

    CASE pFieldName:
        WHEN "aliq-cofins":U THEN ASSIGN pFieldValue = RowObject.aliq-cofins.
        WHEN "aliq-cofins-outros":U THEN ASSIGN pFieldValue = RowObject.aliq-cofins-outros.
        WHEN "aliq-cofins-st":U THEN ASSIGN pFieldValue = RowObject.aliq-cofins-st.
        WHEN "aliq-ipi":U THEN ASSIGN pFieldValue = RowObject.aliq-ipi.
        WHEN "aliq-iss":U THEN ASSIGN pFieldValue = RowObject.aliq-iss.
        WHEN "aliq-pis":U THEN ASSIGN pFieldValue = RowObject.aliq-pis.
        WHEN "aliq-pis-outros":U THEN ASSIGN pFieldValue = RowObject.aliq-pis-outros.
        WHEN "aliq-pis-st":U THEN ASSIGN pFieldValue = RowObject.aliq-pis-st.
        WHEN "icms-aliq[1]":U THEN ASSIGN pFieldValue = RowObject.icms-aliq[1].
        WHEN "icms-aliq[2]":U THEN ASSIGN pFieldValue = RowObject.icms-aliq[2].
        WHEN "icms-aliq[3]":U THEN ASSIGN pFieldValue = RowObject.icms-aliq[3].
        WHEN "icms-aliq[4]":U THEN ASSIGN pFieldValue = RowObject.icms-aliq[4].
        WHEN "icms-aliq[5]":U THEN ASSIGN pFieldValue = RowObject.icms-aliq[5].
        WHEN "icms-aliq[6]":U THEN ASSIGN pFieldValue = RowObject.icms-aliq[6].
        WHEN "icms-base[1]":U THEN ASSIGN pFieldValue = RowObject.icms-base[1].
        WHEN "icms-base[2]":U THEN ASSIGN pFieldValue = RowObject.icms-base[2].
        WHEN "icms-base[3]":U THEN ASSIGN pFieldValue = RowObject.icms-base[3].
        WHEN "icms-base[4]":U THEN ASSIGN pFieldValue = RowObject.icms-base[4].
        WHEN "icms-base[5]":U THEN ASSIGN pFieldValue = RowObject.icms-base[5].
        WHEN "icms-base[6]":U THEN ASSIGN pFieldValue = RowObject.icms-base[6].
        WHEN "icms-valor[1]":U THEN ASSIGN pFieldValue = RowObject.icms-valor[1].
        WHEN "icms-valor[2]":U THEN ASSIGN pFieldValue = RowObject.icms-valor[2].
        WHEN "icms-valor[3]":U THEN ASSIGN pFieldValue = RowObject.icms-valor[3].
        WHEN "icms-valor[4]":U THEN ASSIGN pFieldValue = RowObject.icms-valor[4].
        WHEN "icms-valor[5]":U THEN ASSIGN pFieldValue = RowObject.icms-valor[5].
        WHEN "icms-valor[6]":U THEN ASSIGN pFieldValue = RowObject.icms-valor[6].
        WHEN "icms10-aliq-icms-st":U THEN ASSIGN pFieldValue = RowObject.icms10-aliq-icms-st.
        WHEN "icms10-perc-red-bc-icms-st":U THEN ASSIGN pFieldValue = RowObject.icms10-perc-red-bc-icms-st.
        WHEN "icms10-perc-vl-adic-icms":U THEN ASSIGN pFieldValue = RowObject.icms10-perc-vl-adic-icms.
        WHEN "icms10-vl-bc-st":U THEN ASSIGN pFieldValue = RowObject.icms10-vl-bc-st.
        WHEN "icms10-vl-st":U THEN ASSIGN pFieldValue = RowObject.icms10-vl-st.
        WHEN "icms20-reduc-bc":U THEN ASSIGN pFieldValue = RowObject.icms20-reduc-bc.
        WHEN "icms30-aliq-icms-st":U THEN ASSIGN pFieldValue = RowObject.icms30-aliq-icms-st.
        WHEN "icms30-perc-vl-adic-icms":U THEN ASSIGN pFieldValue = RowObject.icms30-perc-vl-adic-icms.
        WHEN "icms30-red-bc-icms-st":U THEN ASSIGN pFieldValue = RowObject.icms30-red-bc-icms-st.
        WHEN "icms30-vl-bc-icms-st":U THEN ASSIGN pFieldValue = RowObject.icms30-vl-bc-icms-st.
        WHEN "icms30-vl-icms-st":U THEN ASSIGN pFieldValue = RowObject.icms30-vl-icms-st.
        WHEN "icms51-reduc-bc":U THEN ASSIGN pFieldValue = RowObject.icms51-reduc-bc.
        WHEN "icms60-vl-bc-st":U THEN ASSIGN pFieldValue = RowObject.icms60-vl-bc-st.
        WHEN "icms60-vl-st":U THEN ASSIGN pFieldValue = RowObject.icms60-vl-st.
        WHEN "icms70-aliq-st":U THEN ASSIGN pFieldValue = RowObject.icms70-aliq-st.
        WHEN "icms70-perc-margem-vl-adic":U THEN ASSIGN pFieldValue = RowObject.icms70-perc-margem-vl-adic.
        WHEN "icms70-perc-red-bc-icms-st":U THEN ASSIGN pFieldValue = RowObject.icms70-perc-red-bc-icms-st.
        WHEN "icms70-perc-reduc-bc":U THEN ASSIGN pFieldValue = RowObject.icms70-perc-reduc-bc.
        WHEN "icms70-vl-bc-st":U THEN ASSIGN pFieldValue = RowObject.icms70-vl-bc-st.
        WHEN "icms70-vl-st":U THEN ASSIGN pFieldValue = RowObject.icms70-vl-st.
        WHEN "icms90-aliq-st":U THEN ASSIGN pFieldValue = RowObject.icms90-aliq-st.
        WHEN "icms90-perc-margem-vl-adic":U THEN ASSIGN pFieldValue = RowObject.icms90-perc-margem-vl-adic.
        WHEN "icms90-perc-red-bc-icms-st":U THEN ASSIGN pFieldValue = RowObject.icms90-perc-red-bc-icms-st.
        WHEN "icms90-reduc-bc":U THEN ASSIGN pFieldValue = RowObject.icms90-reduc-bc.
        WHEN "icms90-vl-bc-st":U THEN ASSIGN pFieldValue = RowObject.icms90-vl-bc-st.
        WHEN "icms90-vl-st":U THEN ASSIGN pFieldValue = RowObject.icms90-vl-st.
        WHEN "preco-total":U THEN ASSIGN pFieldValue = RowObject.preco-total.
        WHEN "preco-unit":U THEN ASSIGN pFieldValue = RowObject.preco-unit.
        WHEN "qtd-cofins-outros":U THEN ASSIGN pFieldValue = RowObject.qtd-cofins-outros.
        WHEN "qtd-cofins-prod":U THEN ASSIGN pFieldValue = RowObject.qtd-cofins-prod.
        WHEN "qtd-cofins-st":U THEN ASSIGN pFieldValue = RowObject.qtd-cofins-st.
        WHEN "qtd-combustivel":U THEN ASSIGN pFieldValue = RowObject.qtd-combustivel.
        WHEN "qtd-comercial":U THEN ASSIGN pFieldValue = RowObject.qtd-comercial.
        WHEN "qtd-interna":U THEN ASSIGN pFieldValue = RowObject.qtd-interna.
        WHEN "qtd-pis-outros-vend":U THEN ASSIGN pFieldValue = RowObject.qtd-pis-outros-vend.
        WHEN "qtd-pis-prod-vend":U THEN ASSIGN pFieldValue = RowObject.qtd-pis-prod-vend.
        WHEN "qtd-pis-st-vend":U THEN ASSIGN pFieldValue = RowObject.qtd-pis-st-vend.
        WHEN "qtd-tot-ipi":U THEN ASSIGN pFieldValue = RowObject.qtd-tot-ipi.
        WHEN "qtd-trib":U THEN ASSIGN pFieldValue = RowObject.qtd-trib.
        WHEN "qtde-bc-prod":U THEN ASSIGN pFieldValue = RowObject.qtde-bc-prod.
        WHEN "vl-aliq-cide":U THEN ASSIGN pFieldValue = RowObject.vl-aliq-cide.
        WHEN "vl-aliq-cofins-outros":U THEN ASSIGN pFieldValue = RowObject.vl-aliq-cofins-outros.
        WHEN "vl-aliq-cofins-qtd":U THEN ASSIGN pFieldValue = RowObject.vl-aliq-cofins-qtd.
        WHEN "vl-aliq-cofins-st":U THEN ASSIGN pFieldValue = RowObject.vl-aliq-cofins-st.
        WHEN "vl-aliq-pis-outros":U THEN ASSIGN pFieldValue = RowObject.vl-aliq-pis-outros.
        WHEN "vl-aliq-pis-qtd":U THEN ASSIGN pFieldValue = RowObject.vl-aliq-pis-qtd.
        WHEN "vl-aliq-pis-st-prod":U THEN ASSIGN pFieldValue = RowObject.vl-aliq-pis-st-prod.
        WHEN "vl-bc-cofins":U THEN ASSIGN pFieldValue = RowObject.vl-bc-cofins.
        WHEN "vl-bc-cofins-outros":U THEN ASSIGN pFieldValue = RowObject.vl-bc-cofins-outros.
        WHEN "vl-bc-cofins-st":U THEN ASSIGN pFieldValue = RowObject.vl-bc-cofins-st.
        WHEN "vl-bc-icms-prop":U THEN ASSIGN pFieldValue = RowObject.vl-bc-icms-prop.
        WHEN "vl-bc-icms-st-cons":U THEN ASSIGN pFieldValue = RowObject.vl-bc-icms-st-cons.
        WHEN "vl-bc-icms-st-dest":U THEN ASSIGN pFieldValue = RowObject.vl-bc-icms-st-dest.
        WHEN "vl-bc-icms-st-prop":U THEN ASSIGN pFieldValue = RowObject.vl-bc-icms-st-prop.
        WHEN "vl-bc-ii":U THEN ASSIGN pFieldValue = RowObject.vl-bc-ii.
        WHEN "vl-bc-ipi":U THEN ASSIGN pFieldValue = RowObject.vl-bc-ipi.
        WHEN "vl-bc-iss":U THEN ASSIGN pFieldValue = RowObject.vl-bc-iss.
        WHEN "vl-bc-pis":U THEN ASSIGN pFieldValue = RowObject.vl-bc-pis.
        WHEN "vl-bc-pis-outros":U THEN ASSIGN pFieldValue = RowObject.vl-bc-pis-outros.
        WHEN "vl-bc-pis-st":U THEN ASSIGN pFieldValue = RowObject.vl-bc-pis-st.
        WHEN "vl-bruto-item":U THEN ASSIGN pFieldValue = RowObject.vl-bruto-item.
        WHEN "vl-cide":U THEN ASSIGN pFieldValue = RowObject.vl-cide.
        WHEN "vl-cofins-aliq":U THEN ASSIGN pFieldValue = RowObject.vl-cofins-aliq.
        WHEN "vl-cofins-outros":U THEN ASSIGN pFieldValue = RowObject.vl-cofins-outros.
        WHEN "vl-cofins-qtd":U THEN ASSIGN pFieldValue = RowObject.vl-cofins-qtd.
        WHEN "vl-cofins-st":U THEN ASSIGN pFieldValue = RowObject.vl-cofins-st.
        WHEN "vl-desconto":U THEN ASSIGN pFieldValue = RowObject.vl-desconto.
        WHEN "vl-desp-aduaneira":U THEN ASSIGN pFieldValue = RowObject.vl-desp-aduaneira.
        WHEN "vl-frete":U THEN ASSIGN pFieldValue = RowObject.vl-frete.
        WHEN "vl-icms-prop":U THEN ASSIGN pFieldValue = RowObject.vl-icms-prop.
        WHEN "vl-icms-st-cons":U THEN ASSIGN pFieldValue = RowObject.vl-icms-st-cons.
        WHEN "vl-icms-st-dest":U THEN ASSIGN pFieldValue = RowObject.vl-icms-st-dest.
        WHEN "vl-icms-st-prop":U THEN ASSIGN pFieldValue = RowObject.vl-icms-st-prop.
        WHEN "vl-ii":U THEN ASSIGN pFieldValue = RowObject.vl-ii.
        WHEN "vl-iof":U THEN ASSIGN pFieldValue = RowObject.vl-iof.
        WHEN "vl-ipi":U THEN ASSIGN pFieldValue = RowObject.vl-ipi.
        WHEN "vl-iss":U THEN ASSIGN pFieldValue = RowObject.vl-iss.
        WHEN "vl-pis":U THEN ASSIGN pFieldValue = RowObject.vl-pis.
        WHEN "vl-pis-outros":U THEN ASSIGN pFieldValue = RowObject.vl-pis-outros.
        WHEN "vl-pis-qtd":U THEN ASSIGN pFieldValue = RowObject.vl-pis-qtd.
        WHEN "vl-pis-st":U THEN ASSIGN pFieldValue = RowObject.vl-pis-st.
        WHEN "vl-seguro":U THEN ASSIGN pFieldValue = RowObject.vl-seguro.
        WHEN "vl-unid-ipi":U THEN ASSIGN pFieldValue = RowObject.vl-unid-ipi.
        WHEN "vl-unit-comercial":U THEN ASSIGN pFieldValue = RowObject.vl-unit-comercial.
        WHEN "vl-unit-trib":U THEN ASSIGN pFieldValue = RowObject.vl-unit-trib.
        OTHERWISE RETURN "NOK":U.
    END CASE.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getIntField DBOProgram 
PROCEDURE getIntField :
/*------------------------------------------------------------------------------
  Purpose:     Retorna valor de campos do tipo inteiro
  Parameters:  
               recebe nome do campo
               retorna valor do campo
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pFieldName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pFieldValue AS INTEGER NO-UNDO.

    /*--- Verifica se temptable RowObject est  dispon¡vel, caso nÆo esteja ser 
          retornada flag "NOK":U ---*/
    IF NOT AVAILABLE RowObject THEN 
        RETURN "NOK":U.

    CASE pFieldName:
        WHEN "cod-municipio-iss":U THEN ASSIGN pFieldValue = RowObject.cod-municipio-iss.
        WHEN "idi-orig-trad":U THEN ASSIGN pFieldValue = RowObject.idi-orig-trad.
        WHEN "num-pedido":U THEN ASSIGN pFieldValue = RowObject.num-pedido.
        WHEN "numero-ordem":U THEN ASSIGN pFieldValue = RowObject.numero-ordem.
        WHEN "parcela":U THEN ASSIGN pFieldValue = RowObject.parcela.
        WHEN "seq-item":U THEN ASSIGN pFieldValue = RowObject.seq-item.
        OTHERWISE RETURN "NOK":U.
    END CASE.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getKey DBOProgram 
PROCEDURE getKey :
/*------------------------------------------------------------------------------
  Purpose:     Retorna valores dos campos do ¡ndice unico
  Parameters:  
               retorna valor do campo ch-acesso-comp-nfe
               retorna valor do campo idi-orig-trad
               retorna valor do campo seq-item
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER pch-acesso-comp-nfe LIKE nfe013.ch-acesso-comp-nfe NO-UNDO.
    DEFINE OUTPUT PARAMETER pidi-orig-trad LIKE nfe013.idi-orig-trad NO-UNDO.
    DEFINE OUTPUT PARAMETER pseq-item LIKE nfe013.seq-item NO-UNDO.

    /*--- Verifica se temptable RowObject est  dispon¡vel, caso nÆo esteja ser 
          retornada flag "NOK":U ---*/
    IF NOT AVAILABLE RowObject THEN 
       RETURN "NOK":U.

    ASSIGN pch-acesso-comp-nfe = RowObject.ch-acesso-comp-nfe
           pidi-orig-trad = RowObject.idi-orig-trad
           pseq-item = RowObject.seq-item.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getLogField DBOProgram 
PROCEDURE getLogField :
/*------------------------------------------------------------------------------
  Purpose:     Retorna valor de campos do tipo l¢gico
  Parameters:  
               recebe nome do campo
               retorna valor do campo
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pFieldName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pFieldValue AS LOGICAL NO-UNDO.

    /*--- Verifica se temptable RowObject est  dispon¡vel, caso nÆo esteja ser 
          retornada flag "NOK":U ---*/
    IF NOT AVAILABLE RowObject THEN 
        RETURN "NOK":U.

    CASE pFieldName:
        OTHERWISE RETURN "NOK":U.
    END CASE.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getRawField DBOProgram 
PROCEDURE getRawField :
/*------------------------------------------------------------------------------
  Purpose:     Retorna valor de campos do tipo raw
  Parameters:  
               recebe nome do campo
               retorna valor do campo
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pFieldName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pFieldValue AS RAW NO-UNDO.

    /*--- Verifica se temptable RowObject est  dispon¡vel, caso nÆo esteja ser 
          retornada flag "NOK":U ---*/
    IF NOT AVAILABLE RowObject THEN 
        RETURN "NOK":U.

    CASE pFieldName:
        OTHERWISE RETURN "NOK":U.
    END CASE.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getRecidField DBOProgram 
PROCEDURE getRecidField :
/*------------------------------------------------------------------------------
  Purpose:     Retorna valor de campos do tipo recid
  Parameters:  
               recebe nome do campo
               retorna valor do campo
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pFieldName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pFieldValue AS RECID NO-UNDO.

    /*--- Verifica se temptable RowObject est  dispon¡vel, caso nÆo esteja ser 
          retornada flag "NOK":U ---*/
    IF NOT AVAILABLE RowObject THEN 
        RETURN "NOK":U.

    CASE pFieldName:
        OTHERWISE RETURN "NOK":U.
    END CASE.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE goToKey DBOProgram 
PROCEDURE goToKey :
/*------------------------------------------------------------------------------
  Purpose:     Reposiciona registro com base no ¡ndice unico
  Parameters:  
               recebe valor do campo ch-acesso-comp-nfe
               recebe valor do campo idi-orig-trad
               recebe valor do campo seq-item
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pch-acesso-comp-nfe LIKE nfe013.ch-acesso-comp-nfe NO-UNDO.
    DEFINE INPUT PARAMETER pidi-orig-trad LIKE nfe013.idi-orig-trad NO-UNDO.
    DEFINE INPUT PARAMETER pseq-item LIKE nfe013.seq-item NO-UNDO.

    FIND FIRST bfnfe013 WHERE 
        bfnfe013.ch-acesso-comp-nfe = pch-acesso-comp-nfe AND 
        bfnfe013.idi-orig-trad = pidi-orig-trad AND 
        bfnfe013.seq-item = pseq-item NO-LOCK NO-ERROR.

    /*--- Verifica se registro foi encontrado, em caso de erro ser  retornada flag "NOK":U ---*/
    IF NOT AVAILABLE bfnfe013 THEN 
        RETURN "NOK":U.

    /*--- Reposiciona query atrav‚s de rowid e verifica a ocorrˆncia de erros, caso
          existam erros ser  retornada flag "NOK":U ---*/
    RUN repositionRecord IN THIS-PROCEDURE (INPUT ROWID(bfnfe013)).
    IF RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openQueryMain DBOProgram 
PROCEDURE openQueryMain :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    OPEN QUERY {&QueryName} FOR EACH {&TableName} NO-LOCK.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openQueryMonitor DBOProgram 
PROCEDURE openQueryMonitor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    OPEN QUERY {&QueryName} 
        FOR EACH {&TableName} NO-LOCK
            WHERE {&TableName}.ch-acesso-comp-nfe = c-chave-acesso
              AND {&TableName}.idi-orig-trad      = i-idi-origem
               BY {&TableName}.seq-item  .

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE retornaDescItem DBOProgram 
PROCEDURE retornaDescItem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAM p-it-codigo LIKE ITEM.it-codigo NO-UNDO.
    DEFINE OUTPUT PARAM p-desc-item LIKE ITEM.desc-item NO-UNDO.

    FOR FIRST ITEM FIELDS (desc-item)
        WHERE ITEM.it-codigo = p-it-codigo NO-LOCK:
    END.
    IF AVAIL ITEM THEN
        ASSIGN p-desc-item = ITEM.desc-item.
    ELSE
        ASSIGN p-desc-item = "".

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE retornaFormatoConta DBOProgram 
PROCEDURE retornaFormatoConta :
/*------------------------------------------------------------------------------
  Purpose:     Retorna Formato Conta Cont bil
  Parameters:  pFormato -> formato conta cont bil
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER pFormato LIKE param-global.formato-conta-contabil NO-UNDO.

    IF NOT AVAIL param-global THEN
        FIND FIRST param-global NO-LOCK NO-ERROR.

    ASSIGN pFormato = param-global.formato-conta-contabil.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE retornaNomeEmit DBOProgram 
PROCEDURE retornaNomeEmit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAM p-cod-emitente LIKE emitente.cod-emitente NO-UNDO.
    DEFINE OUTPUT PARAM p-nome-emit    LIKE emitente.nome-emit    NO-UNDO.

    FOR FIRST emitente FIELDS (nome-emit)
        WHERE emitente.cod-emitente = p-cod-emitente NO-LOCK:
    END.
    IF AVAIL emitente THEN
        ASSIGN p-nome-emit = emitente.nome-emit.
    ELSE
        ASSIGN p-nome-emit = "".

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE retornaValidLote DBOProgram 
PROCEDURE retornaValidLote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT  PARAM p-it-codigo  LIKE nfe013.it-codigo    NO-UNDO.
    DEF INPUT  PARAM p-lote       LIKE nfe013.lote-serie   NO-UNDO.
    DEF INPUT  PARAM p-cod-refer  LIKE nfe013.cod-refer    NO-UNDO.
    DEF OUTPUT PARAM p-valid-lote LIKE nfe013.dt-vali-lote NO-UNDO.

    IF AVAIL nfe003 THEN DO:
        FOR FIRST ITEM FIELDS (tipo-con-est)
            WHERE ITEM.it-codigo = p-it-codigo NO-LOCK:
        END.
        
        IF  AVAIL ITEM
        AND (ITEM.tipo-con-est = 3 OR ITEM.tipo-con-est = 4) THEN DO:
            FIND LAST saldo-estoq USE-INDEX item-lote
                WHERE saldo-estoq.it-codigo   = p-it-codigo
                  AND saldo-estoq.cod-estabel = nfe003.cod-estabel 
                  AND saldo-estoq.lote        = p-lote
                  AND saldo-estoq.cod-refer   = p-cod-refer NO-LOCK NO-ERROR.
            IF AVAIL saldo-estoq THEN
                ASSIGN p-valid-lote = saldo-estoq.dt-vali-lote.
            ELSE
                ASSIGN p-valid-lote = ?.
        END.
    END.
    
    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setConstraintMonitor DBOProgram 
PROCEDURE setConstraintMonitor :
/*------------------------------------------------------------------------------
  Purpose:     Recebe parametros para realizar abertura da query
  Parameters:  Chave acesso Completa NF-e
               Origem NF-e (1 - Original e 2 - Traduzida) 
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-chave-acesso LIKE nfe013.ch-acesso-comp-nfe NO-UNDO.
    DEF INPUT PARAMETER p-idi-origem   LIKE nfe013.idi-orig-trad      NO-UNDO.

    ASSIGN c-chave-acesso = p-chave-acesso
           i-idi-origem   = p-idi-origem  .

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validateRecord DBOProgram 
PROCEDURE validateRecord :
/*:T------------------------------------------------------------------------------
  Purpose:     Valida‡äes pertinentes ao DBO
  Parameters:  recebe o tipo de valida‡Æo (Create, Delete, Update)
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE INPUT PARAMETER pType AS CHARACTER NO-UNDO.
    
    /*:T--- Utilize o parƒmetro pType para identificar quais as valida‡äes a serem
          executadas ---*/
    /*:T--- Os valores poss¡veis para o parƒmetro sÆo: Create, Delete e Update ---*/
    /*:T--- Devem ser tratados erros PROGRESS e erros do Produto, atrav‚s do 
          include: method/svc/errors/inserr.i ---*/
    /*:T--- Inclua aqui as valida‡äes ---*/

    IF pType = "UPDATE":U THEN DO:
        FOR FIRST nfe003 FIELDS (cod-emitente cod-estabel)
            WHERE nfe003.ch-acesso-comp-nfe = RowObject.ch-acesso-comp-nfe
              AND nfe003.idi-orig-trad      = RowObject.idi-orig-trad NO-LOCK:
        END.

        FIND FIRST param-compra  NO-LOCK NO-ERROR.
        FIND FIRST param-contrat NO-LOCK NO-ERROR.

        /** VALIDA€åES PEDIDO E ORDEM **/
        IF RowObject.numero-ordem <> 0 THEN DO:
            FOR FIRST ordem-compra FIELDS(num-pedido it-codigo situacao)
                WHERE ordem-compra.numero-ordem = RowObject.numero-ordem NO-LOCK:
            END.
            IF NOT AVAIL ordem-compra THEN DO:
                 {method/svc/errors/inserr.i &ErrorNumber="2"
                                             &ErrorType="EMS"
                                             &ErrorSubType="ERROR"
                                             &ErrorParameters="'Ordem de Compra'"}
            END.
            ELSE DO:
                IF  RowObject.num-pedido <> ordem-compra.num-pedido THEN DO:
                    {method/svc/errors/inserr.i &ErrorNumber="7342"  /*Ordem de compra n?o pertence ao pedido*/
                                                &ErrorType="EMS"
                                                &ErrorSubType="ERROR"}
                END.
            
                IF  RowObject.it-codigo <> ordem-compra.it-codigo THEN DO:
                    {method/svc/errors/inserr.i &ErrorNumber="15187"  /*Item da ordem n?o confere com item da nota fiscal*/
                                                &ErrorType="EMS"
                                                &ErrorSubType="ERROR"}
                END.

                IF  ordem-compra.situacao <> 2 
                /*and not docum-est.rec-fisico*/ THEN DO:
                    {method/svc/errors/inserr.i &ErrorNumber="15188"  /*Situa¯?o da ordem n?o permite movimenta¯?o*/
                                                &ErrorType="EMS"
                                                &ErrorSubType="ERROR"}
                END.

                IF  param-compra.log-1 THEN DO:
                    ASSIGN l-pendente = NO.

                    RUN cdp/cdapi172.p (INPUT 4, 
                                        INPUT ROWID(ordem-compra), 
                                        OUTPUT l-pendente).

                    IF  NOT l-pendente THEN
                        RUN cdp/cdapi172.p (INPUT 6, 
                                            INPUT ROWID(ordem-compra), 
                                            OUTPUT l-pendente).

                    IF  l-pendente THEN DO:
                        {method/svc/errors/inserr.i &ErrorNumber="17022"  /*Documento estÿ pendente de aprova?’o*/
                                                    &ErrorType="EMS"
                                                    &ErrorSubType="ERROR"}
                    END.
                END.
            END.
        END.

        IF RowObject.num-pedido <> 0 THEN DO:
            FOR FIRST pedido-compr FIELDS (situacao end-entrega cod-emitente)
                WHERE pedido-compr.num-pedido = RowObject.num-pedido NO-LOCK:
            END.
            IF NOT AVAIL pedido-compr THEN DO:
                 {method/svc/errors/inserr.i &ErrorNumber="2"
                                             &ErrorType="EMS"
                                             &ErrorSubType="ERROR"
                                             &ErrorParameters="'Pedido de Compra'"}
            END.
            ELSE DO:
                IF  pedido-compr.situacao = 3 THEN DO:
                    {method/svc/errors/inserr.i &ErrorNumber="7169"  /*Pedido de compra eliminado*/
                                                &ErrorType="EMS"
                                                &ErrorSubType="ERROR"}
                END.

                IF  pedido-compr.end-entrega <> nfe003.cod-estabel THEN DO:
                    {method/svc/errors/inserr.i &ErrorNumber="18800"  /*Estabelecimento de entrega do pedido ì diferente do estabelecimento do documento.*/
                                                &ErrorType="EMS"
                                                &ErrorSubType="WARNING"}
                END.

                IF  pedido-compr.cod-emitente <> nfe003.cod-emitente then do:
                    IF  param-re.rec-out-for THEN DO: 
                        {method/svc/errors/inserr.i &ErrorNumber="18801" /*Fornecedor da nota difere do pedido.*/  
                                                    &ErrorType="EMS"
                                                    &ErrorSubType="WARNING"}
                    END.
                    ELSE DO:
                       {method/svc/errors/inserr.i &ErrorNumber="8851"  /*Fornecedor da nota difere do pedido*/
                                                   &ErrorType="EMS"
                                                   &ErrorSubType="ERROR"}
                 
                    END.
                END.
            END.
        END.
        /*******************************/

        /*
        /** VALIDA€ÇO NATUREZA **/
        IF NOT CAN-FIND(FIRST natur-oper
                        WHERE natur-oper.nat-operacao = RowObject.nat-operacao) THEN DO:
            {method/svc/errors/inserr.i &ErrorNumber="2"
                                        &ErrorType="EMS"
                                        &ErrorSubType="ERROR"
                                        &ErrorParameters="'Natureza de Opera‡Æo'"}
        END.
        */

        /** VALIDA€ÇO CLASSIFICA€ÇO FISCAL **/
        IF  RowObject.class-fiscal <> ""
        AND NOT CAN-FIND(FIRST classif-fisc
                        WHERE classif-fisc.class-fiscal = RowObject.class-fiscal) THEN DO:
            {method/svc/errors/inserr.i &ErrorNumber="2"
                                        &ErrorType="EMS"
                                        &ErrorSubType="ERROR"
                                        &ErrorParameters="'Classifica‡Æo Fiscal'"}
        END.

        /** VALIDA€ÇO CONTA CONTABIL **/
        IF  RowObject.conta-contabil <> ""
        AND NOT CAN-FIND(FIRST conta-contab
                         WHERE conta-contab.ep-codigo      = i-ep-codigo-usuario
                           AND conta-contab.conta-contabil = RowObject.conta-contabil) THEN DO:
            {method/svc/errors/inserr.i &ErrorNumber="2"
                                        &ErrorType="EMS"
                                        &ErrorSubType="ERROR"
                                        &ErrorParameters="'Conta Cont bil'"}
        END.

        /** VALIDA€ÇO ORDEM DE PRODUCAO **/
        IF  RowObject.nr-ord-prod <> 0
        AND NOT CAN-FIND(FIRST ord-prod
                         WHERE ord-prod.nr-ord-prod = RowObject.nr-ord-prod) THEN DO:
            {method/svc/errors/inserr.i &ErrorNumber="2"
                                        &ErrorType="EMS"
                                        &ErrorSubType="ERROR"
                                        &ErrorParameters="'Ordem de Produ‡Æo'"}
        END.

        /** VALIDA€ÇO REFERENCIA **/
        FOR FIRST ITEM FIELDS(tipo-con-est)
            WHERE ITEM.it-codigo = RowObject.it-codigo NO-LOCK:
        END.
        
        IF  AVAIL ITEM
        AND ITEM.tipo-con-est = 4
        AND NOT CAN-FIND(FIRST ref-item
                         WHERE ref-item.it-codigo = RowObject.it-codigo
                           AND ref-item.cod-refer = RowObject.cod-refer) THEN DO:
            {method/svc/errors/inserr.i &ErrorNumber="33928"
                                        &ErrorType="EMS":U
                                        &ErrorSubType="ERROR":U
                                        &ErrorParameters="RowObject.cod-refer + '~~~~' + RowObject.it-codigo"}
        END.

        /** VALIDA€åES LOTE/SERIE **/
        /*
        IF  AVAIL ITEM
        AND (   ITEM.tipo-con-est = 2
             OR ITEM.tipo-con-est = 3 
             OR ITEM.tipo-con-est = 4)
        AND RowObject.lote-serie = "" THEN DO:
            {method/svc/errors/inserr.i &ErrorNumber="1818"
                                        &ErrorType="EMS"
                                        &ErrorSubType="ERROR"}
        END.

        IF AVAIL ITEM
        AND (   ITEM.tipo-con-est = 3 
             OR ITEM.tipo-con-est = 4) THEN DO:

            IF RowObject.dt-vali-lote = ? 
            OR RowObject.dt-vali-lote < TODAY THEN DO:
                {method/svc/errors/inserr.i &ErrorNumber="1247" 
                                            &ErrorType="EMS"
                                            &ErrorSubType="ERROR" }
            END.
        END.
        */
        /***************************/

        /** VALIDA€ÇO DEPOSITO **/
        IF  RowObject.cod-depos <> ""
        AND NOT CAN-FIND(FIRST deposito
                         WHERE deposito.cod-depos = RowObject.cod-depos) THEN DO:
            {method/svc/errors/inserr.i &ErrorNumber="2"
                                        &ErrorType="EMS"
                                        &ErrorSubType="ERROR"
                                        &ErrorParameters="'Dep¢sito'"}
        END.

        /** VALIDA€ÇO LOCALIZA€ÇO **/
        IF  RowObject.cod-localiz <> ""
        AND NOT CAN-FIND(FIRST mgind.localizacao
                         WHERE localizacao.cod-estabel = nfe003.cod-estabel
                           AND localizacao.cod-depos   = RowObject.cod-depos
                           AND localizacao.cod-localiz = RowObject.cod-localiz) THEN DO:
            {method/svc/errors/inserr.i &ErrorNumber="2"
                                        &ErrorType="EMS"
                                        &ErrorSubType="ERROR"
                                        &ErrorParameters="'Localiza‡Æo'"}
        END.
    
        /** VALIDA€ÇO UNIDADE DE NEGOCIO **/
        &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
            IF  RowObject.cod-unid-negoc <> "" 
            AND NOT CAN-FIND(FIRST unid-negoc
                             WHERE unid-negoc.cod-unid-negoc = RowObject.cod-unid-negoc) THEN DO:
                {method/svc/errors/inserr.i &ErrorNumber="2"
                                            &ErrorType="EMS"
                                            &ErrorSubType="ERROR"
                                            &ErrorParameters="'Unidade de Neg¢cio'"}
            END.
        &ENDIF
        
        if  nfe003.lg-devol then do:
            find first it-nota-fisc no-lock
                 where it-nota-fisc.nr-nota-fis = rowObject.nro-comp
                   and it-nota-fisc.serie       = rowObject.serie-comp
                   and it-nota-fisc.cod-estabel = nfe003.cod-estabel 
                   and it-nota-fisc.it-codigo   = rowObject.it-codigo
                   and it-nota-fisc.cod-refer   = rowObject.cod-refer
                   and it-nota-fisc.nr-seq-fat  = rowObject.seq-comp no-error.

            if  not avail it-nota-fisc then do:
                {method/svc/errors/inserr.i &ErrorNumber="17006"
                                            &ErrorType="EMS"
                                            &ErrorSubType="ERROR"
                                            &ErrorParameters="'Item da nota de devolu‡Æo inexistente!'"}
            end.
            else do:
                for first nota-fiscal no-lock
                    where nota-fiscal.nr-nota-fis   = it-nota-fisc.nr-nota-fis
                      and nota-fiscal.cod-estabel   = it-nota-fisc.cod-estabel
                      and nota-fiscal.serie         = it-nota-fisc.serie 
                      and nota-fiscal.cod-emitente <> nfe003.cod-emitente:
                    {method/svc/errors/inserr.i &ErrorNumber="17006"
                                                &ErrorType="EMS"
                                                &ErrorSubType="ERROR"
                                                &ErrorParameters="'Emitente da nota de devolu‡Æo diferente do importado.'"}

                end.
            end.
        end.
    END.
    
    /*:T--- Verifica ocorrˆncia de erros ---*/
    IF  CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorSubType = "ERROR":U) THEN
        RETURN "NOK":U.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

