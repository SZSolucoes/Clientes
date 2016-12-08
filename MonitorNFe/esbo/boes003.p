
&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DBOProgram 
/*:T--------------------------------------------------------------------------
    File       : dbo.p
    Purpose    : O DBO (Datasul Business Objects) ? um programa PROGRESS 
                 que cont?m a l½gica de neg½cio e acesso a dados para uma 
                 tabela do banco de dados.

    Parameters : 

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  **************************** */

/*:T--- Diretrizes de defini?Æo ---*/
&GLOBAL-DEFINE DBOName BOES003
&GLOBAL-DEFINE DBOVersion 
&GLOBAL-DEFINE DBOCustomFunctions 
&GLOBAL-DEFINE TableName nfe003
&GLOBAL-DEFINE TableLabel 
&GLOBAL-DEFINE QueryName qr{&TableName} 

{cdp/cdcfgdis.i}       
{cdp/cdcfgmat.i}      
{esbo/boes003.i RowObject}
{inbo/boin090.i tt-docum-est}       
{inbo/boin176.i tt-item-doc-est}  
{inbo/boin176.i tt2-item-doc-est}  
{inbo/boin092.i tt-dupli-apagar}    
{inbo/boin567.i tt-dupli-imp}       
{inbo/boin366.i tt-rat-docum} 
{inbo/boin368.i tt-rat-ordem}   
{utp/ut-glob.i}
{utp/utapi019.i}

/*:T--- Include com defini?Æo da query para tabela {&TableName} ---*/
/*:T--- Em caso de necessidade de altera?Æo da defini?Æo da query, pode ser retirada
      a chamada ao include a seguir e em seu lugar deve ser feita a defini?Æo 
      manual da query ---*/
{method/dboqry.i}


/*:T--- Defini?Æo de buffer que serÿ utilizado pelo m?todo goToKey ---*/
DEFINE BUFFER bf{&TableName}  FOR {&TableName}.
DEFINE BUFFER bf-nfe013 FOR nfe013.

    DEF TEMP-TABLE tt-erro NO-UNDO
    FIELD identif-segment AS CHAR
    FIELD cd-erro         AS INTEGER
    FIELD desc-erro       AS CHAR FORMAT "x(80)".

DEF TEMP-TABLE tt-erro-aux NO-UNDO
    FIELD identif-segment AS CHAR
    FIELD cd-erro         AS INTEGER
    FIELD desc-erro       AS CHAR FORMAT "x(80)"
    FIELD tipo            AS CHAR .

DEFINE TEMP-TABLE tt-ordem-compra NO-UNDO LIKE ordem-compra 
    FIELD deQtdeSaldo   AS DECIMAL   FORMAT "->>>,>>>,>>9.99"    COLUMN-LABEL "Saldo" 
    FIELD cDescItem     AS CHARACTER FORMAT "X(30)"              COLUMN-LABEL "Descri?Æo Item"
    FIELD cUn           AS CHARACTER FORMAT "X(02)"              COLUMN-LABEL "Un"
    FIELD deQtdeReceb   AS DECIMAL   FORMAT "->>>,>>>,>>9.99"    COLUMN-LABEL "Nossa Qtde" 
    FIELD dePrTotal     AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" COLUMN-LABEL "Pre?o Total" 
    FIELD cCodDepos     AS CHARACTER FORMAT "X(03)"              COLUMN-LABEL "Dep"
    FIELD cCodLocaliz   AS CHARACTER FORMAT "X(10)"              COLUMN-LABEL "Localiza?Æo"
    FIELD cLote         AS CHARACTER FORMAT "x(40)"              COLUMN-LABEL "Lote/Serie"
    FIELD dtValidade    AS DATE      FORMAT "99/99/9999"         COLUMN-LABEL "Validade Lote"
    FIELD cContaContab  AS CHARACTER FORMAT 'X(17)'              COLUMN-LABEL 'Conta Contÿbil'
    FIELD rRowid        AS ROWID  
    FIELD seq           AS INT .

/* versÆo > 2.06 */
/*define temp-table tt-saldo no-undo
    like saldo-terc.*/

/* versÆo 2.06 */
define temp-table tt-saldo no-undo
    like saldo-terc
    fields qtd-alocada as dec.

def temp-table tt-item-terc no-undo
    field rw-saldo-terc   as rowid
    field quantidade      like saldo-terc.quantidade
    field preco-total     like componente.preco-total extent 0
    field desconto        like componente.desconto    extent 0    
    field cod-depos       like saldo-terc.cod-depos
    field nr-ord-prod     like saldo-terc.nr-ord-prod
    FIELD nat-of          LIKE item-doc-est.nat-of.

DEF TEMP-TABLE tt-natureza NO-UNDO
    FIELD nat-operacao LIKE natur-oper.nat-operacao.

DEF TEMP-TABLE tt-movto-pend NO-UNDO
    LIKE movto-pend
    FIELD r-rowid AS ROWID.

DEFINE VARIABLE tg-digitada          AS LOGICAL                    NO-UNDO.
DEFINE VARIABLE tg-erro-neg          AS LOGICAL                    NO-UNDO.
DEFINE VARIABLE tg-atualizada        AS LOGICAL                    NO-UNDO.
DEFINE VARIABLE tg-eliminada         AS LOGICAL                    NO-UNDO.
DEFINE VARIABLE tg-danfe             AS LOGICAL                    NO-UNDO.
DEFINE VARIABLE tg-liberado          AS LOGICAL                    NO-UNDO.
DEFINE VARIABLE tg-conferido         AS LOGICAL                    NO-UNDO.
DEFINE VARIABLE c-cod-emitente-ini LIKE emitente.cod-emitente      NO-UNDO.
DEFINE VARIABLE c-cod-emitente-fim LIKE emitente.cod-emitente      NO-UNDO.
DEFINE VARIABLE c-cod-estabel-ini  LIKE docum-est.cod-estabel      NO-UNDO.
DEFINE VARIABLE c-cod-estabel-fim  LIKE docum-est.cod-estabel      NO-UNDO.
DEFINE VARIABLE c-serie-ini        LIKE docum-est.serie-docto      NO-UNDO.
DEFINE VARIABLE c-serie-fim        LIKE docum-est.serie-docto      NO-UNDO.
DEFINE VARIABLE c-nro-docto-ini    LIKE docum-est.nro-docto        NO-UNDO.
DEFINE VARIABLE c-nro-docto-fim    LIKE docum-est.nro-docto        NO-UNDO.
DEFINE VARIABLE dt-emissao-ini     LIKE docum-est.dt-emissao       NO-UNDO.
DEFINE VARIABLE dt-emissao-fim     LIKE docum-est.dt-emissao       NO-UNDO.
DEFINE VARIABLE i-idi-orig-trad    LIKE nfe003.idi-orig-trad NO-UNDO.
DEFINE VARIABLE i-nr-ord-produ     AS INTEGER     NO-UNDO.


DEFINE VARIABLE de-qtd-saldo AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-qtd-aux AS DECIMAL     NO-UNDO.
DEFINE VARIABLE c-serie-docto       LIKE rat-ordem.serie-docto     NO-UNDO.
DEFINE VARIABLE c-nro-docto         LIKE rat-ordem.nro-docto       NO-UNDO.
DEFINE VARIABLE c-nat-operacao      LIKE rat-ordem.nat-operacao    NO-UNDO.
DEFINE VARIABLE i-ordem-ini         LIKE ordem-compra.numero-ordem NO-UNDO.
DEFINE VARIABLE i-ordem-fim         LIKE ordem-compra.numero-ordem NO-UNDO.
DEFINE VARIABLE c-estab-ini         LIKE estabelec.cod-estabel     NO-UNDO.
DEFINE VARIABLE c-estab-fim         LIKE estabelec.cod-estabel     NO-UNDO.
DEFINE VARIABLE c-item-ini          LIKE ordem-compra.it-codigo    NO-UNDO.
DEFINE VARIABLE c-item-fim          LIKE ordem-compra.it-codigo    NO-UNDO.
DEFINE VARIABLE dt-entr-ini         LIKE prazo-compra.data-entrega NO-UNDO.
DEFINE VARIABLE dt-entr-fim         LIKE prazo-compra.data-entrega NO-UNDO.
DEFINE VARIABLE c-cod-unid-negoc    LIKE item-doc-est.cod-unid-negoc NO-UNDO.
DEFINE VARIABLE c-natureza-operacao AS CHARACTER   NO-UNDO.

DEF VAR h-boin090    AS HANDLE  NO-UNDO.
DEF VAR h-boin176    AS HANDLE  NO-UNDO.
def var de-frete     as decimal no-undo.
DEF VAR h-boin368    AS HANDLE NO-UNDO.

DEFINE NEW GLOBAL SHARED VAR v_cod_usuar_corren LIKE usuar_mestre.cod_usuario NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DBOProgram
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnConverteQtdTotalInterna DBOProgram 
FUNCTION fnConverteQtdTotalInterna RETURNS DECIMAL
  (   INPUT p-qtde AS DECIMAL, 
      input pc-un-fornec as char  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnConverteInterna DBOProgram 
FUNCTION fnConverteInterna RETURNS DECIMAL
  ( INPUT p-qtde AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
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
         HEIGHT             = 13.29
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enviaEmail DBOProgram 
PROCEDURE enviaEmail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAM c-comprador    AS CHARACTER. 
DEFINE INPUT PARAM p-email-padrao AS CHARACTER.

DEFINE VARIABLE c-email-comprador LIKE usuar_mestre.cod_e_mail_local NO-UNDO.
DEFINE VARIABLE c-email-usuario   LIKE usuar_mestre.cod_e_mail_local NO-UNDO.


IF NOT AVAIL param-global THEN
    FIND FIRST param-global NO-LOCK NO-ERROR.


/* Busca e-mail do comprador */
FIND FIRST usuar_mestre 
    WHERE usuar_mestre.cod_usuario = c-comprador NO-LOCK NO-ERROR.
IF AVAIL usuar_mestre THEN
    ASSIGN c-email-comprador = usuar_mestre.cod_e_mail_local.

/* Busca e-mail usuÿrio logado */
FIND FIRST usuar_mestre 
    WHERE usuar_mestre.cod_usuario = v_cod_usuar_corren NO-LOCK NO-ERROR.
IF AVAIL usuar_mestre THEN
    ASSIGN c-email-usuario = usuar_mestre.cod_e_mail_local.

  /* descomentar  
  CREATE es-email.
  ASSIGN es-email.chave            = 'ESNF003' + "_" + STRING(TODAY) + "_" + STRING(TIME,"HH:MM:SS") 
         es-email.AC               = ''
         es-email.assunto          = 'Varia?Æo Recebimento'
         es-email.destino          = c-email-usuario
         es-email.data-solicitacao = TODAY               
         es-email.hora-solicitacao = STRING(TIME,"HH:MM")
         es-email.prog-solicitante = 'ESNF003'  
         es-email.tentativas       = 1
         es-email.texto            = p-email-padrao
         es-email.email-from       = c-email-comprador
         es-email.user-solicitante = 'SUPER'
         /*es-email.arquivo-anexo    = 'indicadores_manutencao.txt':u
         es-email.arquivo-anexo    = tt-param.arquivo*/ .
  */

  RUN utp/ut-msgs.p ("show",
                       6474,
                       "").

/*
EMPTY TEMP-TABLE tt-envio2.

CREATE tt-envio2.
ASSIGN tt-envio2.versao-integracao = 1
       tt-envio2.exchange          = param-global.log-1
       tt-envio2.remetente         = /*c-email-usuario*/  'glauciojose.dias@gmail.com'
       tt-envio2.destino           = /*c-email-comprador*/ 'glauciojose.dias@gmail.com'
       tt-envio2.servidor          = param-global.serv-mail
       tt-envio2.porta             = param-global.porta-mail
       tt-envio2.assunto           = "Diferen?a de Pre?o/Quantidade/Dias de entrega"
       tt-envio2.formato           = "HTML"
       tt-envio2.mensagem          = p-email-padrao.    

RUN utp/utapi019.p PERSISTENT SET h-utapi019.
RUN pi-execute IN h-utapi019 (INPUT TABLE tt-envio2,
                              OUTPUT TABLE tt-erros).    

IF RETURN-VALUE = "NOK" THEN DO:
    FIND FIRST tt-erros NO-ERROR.    
    RUN utp/ut-msgs.p ("show",
                       27979,
                       "NÆo foi poss­vel enviar e-mail!~~Foi encontrado o seguinte erro: " +
                       CHR(10) + IF AVAIL tt-erros THEN tt-erros.desc-erro ELSE "").
END.
ELSE 
    RUN utp/ut-msgs.p ("show",
                       6474,
                       "").
FOR EACH tt-erros:
    DELETE tt-erros.
END.

DELETE PROCEDURE h-utapi019.

*/

RETURN RETURN-VALUE.        

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

    /*--- Verifica se temptable RowObject estÿ dispon­vel, caso nÆo esteja serÿ
          retornada flag "NOK":U ---*/
    IF NOT AVAILABLE RowObject THEN 
        RETURN "NOK":U.

    CASE pFieldName:
        WHEN "Bairro":U THEN ASSIGN pFieldValue = RowObject.Bairro.
        WHEN "bairro-dest":U THEN ASSIGN pFieldValue = RowObject.bairro-dest.
        WHEN "bairro-ent":U THEN ASSIGN pFieldValue = RowObject.bairro-ent.
        WHEN "bairro-ret":U THEN ASSIGN pFieldValue = RowObject.bairro-ret.
        WHEN "cep":U THEN ASSIGN pFieldValue = RowObject.cep.
        WHEN "cep-destino":U THEN ASSIGN pFieldValue = RowObject.cep-destino.
        WHEN "cfop-ret":U THEN ASSIGN pFieldValue = RowObject.cfop-ret.
        WHEN "ch-acesso-comp-nfe":U THEN ASSIGN pFieldValue = RowObject.ch-acesso-comp-nfe.
        WHEN "cod-cond-pag":U THEN ASSIGN pFieldValue = RowObject.cod-cond-pag.
        WHEN "cnae-fiscal":U THEN ASSIGN pFieldValue = RowObject.cnae-fiscal.
        WHEN "cnpj":U THEN ASSIGN pFieldValue = RowObject.cnpj.
        WHEN "cnpj-destino":U THEN ASSIGN pFieldValue = RowObject.cnpj-destino.
        WHEN "cnpj-emissor":U THEN ASSIGN pFieldValue = RowObject.cnpj-emissor.
        WHEN "cnpj-entrega":U THEN ASSIGN pFieldValue = RowObject.cnpj-entrega.
        WHEN "cnpj-retirada":U THEN ASSIGN pFieldValue = RowObject.cnpj-retirada.
        WHEN "cnpj-transp":U THEN ASSIGN pFieldValue = RowObject.cnpj-transp.
        WHEN "cod-dv":U THEN ASSIGN pFieldValue = RowObject.cod-dv.
        WHEN "cod-estabel":U THEN ASSIGN pFieldValue = RowObject.cod-estabel.
        WHEN "cod-fatura":U THEN ASSIGN pFieldValue = RowObject.cod-fatura.
        WHEN "cod-finalidade":U THEN ASSIGN pFieldValue = RowObject.cod-finalidade.
        WHEN "cod-rntc":U THEN ASSIGN pFieldValue = RowObject.cod-rntc.
        WHEN "cod-rntc-reboque[1]":U THEN ASSIGN pFieldValue = RowObject.cod-rntc-reboque[1].
        WHEN "cod-rntc-reboque[2]":U THEN ASSIGN pFieldValue = RowObject.cod-rntc-reboque[2].
        WHEN "complemento":U THEN ASSIGN pFieldValue = RowObject.complemento.
        WHEN "complemento-dest":U THEN ASSIGN pFieldValue = RowObject.complemento-dest.
        WHEN "complemento-ent":U THEN ASSIGN pFieldValue = RowObject.complemento-ent.
        WHEN "complemento-ret":U THEN ASSIGN pFieldValue = RowObject.complemento-ret.
        WHEN "cpf":U THEN ASSIGN pFieldValue = RowObject.cpf.
        WHEN "cpf-destino":U THEN ASSIGN pFieldValue = RowObject.cpf-destino.
        WHEN "cpf-transp":U THEN ASSIGN pFieldValue = RowObject.cpf-transp.
        WHEN "end-transp":U THEN ASSIGN pFieldValue = RowObject.end-transp.
        WHEN "fone":U THEN ASSIGN pFieldValue = RowObject.fone.
        WHEN "fone-destino":U THEN ASSIGN pFieldValue = RowObject.fone-destino.
        WHEN "fone-emit":U THEN ASSIGN pFieldValue = RowObject.fone-emit.
        WHEN "Forma-emissao":U THEN ASSIGN pFieldValue = RowObject.Forma-emissao.
        WHEN "Forma-imp":U THEN ASSIGN pFieldValue = RowObject.Forma-imp.
        WHEN "inf-adicionais":U THEN ASSIGN pFieldValue = RowObject.inf-adicionais.
        WHEN "inf-complement":U THEN ASSIGN pFieldValue = RowObject.inf-complement.
        WHEN "inf-contrato":U THEN ASSIGN pFieldValue = RowObject.inf-contrato.
        WHEN "inf-pedido":U THEN ASSIGN pFieldValue = RowObject.inf-pedido.
        WHEN "insc-estad-subs-dest":U THEN ASSIGN pFieldValue = RowObject.insc-estad-subs-dest.
        WHEN "insc-estadual":U THEN ASSIGN pFieldValue = RowObject.insc-estadual.
        WHEN "insc-estadual-dest":U THEN ASSIGN pFieldValue = RowObject.insc-estadual-dest.
        WHEN "insc-estadual-subs":U THEN ASSIGN pFieldValue = RowObject.insc-estadual-subs.
        WHEN "insc-estadual-transp":U THEN ASSIGN pFieldValue = RowObject.insc-estadual-transp.
        WHEN "insc-municipal":U THEN ASSIGN pFieldValue = RowObject.insc-municipal.
        WHEN "localiz-embarque":U THEN ASSIGN pFieldValue = RowObject.localiz-embarque.
        WHEN "logradouro":U THEN ASSIGN pFieldValue = RowObject.logradouro.
        WHEN "logradouro-dest":U THEN ASSIGN pFieldValue = RowObject.logradouro-dest.
        WHEN "logradouro-ent":U THEN ASSIGN pFieldValue = RowObject.logradouro-ent.
        WHEN "logradouro-ret":U THEN ASSIGN pFieldValue = RowObject.logradouro-ret.
        WHEN "matr-agente":U THEN ASSIGN pFieldValue = RowObject.matr-agente.
        WHEN "mod-doc-fisc":U THEN ASSIGN pFieldValue = RowObject.mod-doc-fisc.
        WHEN "mod-frete":U THEN ASSIGN pFieldValue = RowObject.mod-frete.
        WHEN "nom-agente":U THEN ASSIGN pFieldValue = RowObject.nom-agente.
        WHEN "nom-municipio-end":U THEN ASSIGN pFieldValue = RowObject.nom-municipio-end.
        WHEN "nom-municipio-ent":U THEN ASSIGN pFieldValue = RowObject.nom-municipio-ent.
        WHEN "nom-municipio-ret":U THEN ASSIGN pFieldValue = RowObject.nom-municipio-ret.
        WHEN "nom-pais":U THEN ASSIGN pFieldValue = RowObject.nom-pais.
        WHEN "nom-pais-dest":U THEN ASSIGN pFieldValue = RowObject.nom-pais-dest.
        WHEN "nome-fantasia":U THEN ASSIGN pFieldValue = RowObject.nome-fantasia.
        WHEN "nota-empenho":U THEN ASSIGN pFieldValue = RowObject.nota-empenho.
        WHEN "nro-docto":U THEN ASSIGN pFieldValue = RowObject.nro-docto.
        WHEN "num-dar":U THEN ASSIGN pFieldValue = RowObject.num-dar.
        WHEN "num-end-dest":U THEN ASSIGN pFieldValue = RowObject.num-end-dest.
        WHEN "num-end-emit":U THEN ASSIGN pFieldValue = RowObject.num-end-emit.
        WHEN "num-end-ent":U THEN ASSIGN pFieldValue = RowObject.num-end-ent.
        WHEN "num-end-ret":U THEN ASSIGN pFieldValue = RowObject.num-end-ret.
        WHEN "obs-cont[1]":U THEN ASSIGN pFieldValue = RowObject.obs-cont[1].
        WHEN "obs-cont[2]":U THEN ASSIGN pFieldValue = RowObject.obs-cont[2].
        WHEN "obs-cont[3]":U THEN ASSIGN pFieldValue = RowObject.obs-cont[3].
        WHEN "obs-cont[4]":U THEN ASSIGN pFieldValue = RowObject.obs-cont[4].
        WHEN "obs-cont[5]":U THEN ASSIGN pFieldValue = RowObject.obs-cont[5].
        WHEN "obs-cont[6]":U THEN ASSIGN pFieldValue = RowObject.obs-cont[6].
        WHEN "obs-cont[7]":U THEN ASSIGN pFieldValue = RowObject.obs-cont[7].
        WHEN "obs-cont[8]":U THEN ASSIGN pFieldValue = RowObject.obs-cont[8].
        WHEN "obs-cont[9]":U THEN ASSIGN pFieldValue = RowObject.obs-cont[9].
        WHEN "obs-cont[10]":U THEN ASSIGN pFieldValue = RowObject.obs-cont[10].
        WHEN "obs-cont-txt[1]":U THEN ASSIGN pFieldValue = RowObject.obs-cont-txt[1].
        WHEN "obs-cont-txt[2]":U THEN ASSIGN pFieldValue = RowObject.obs-cont-txt[2].
        WHEN "obs-cont-txt[3]":U THEN ASSIGN pFieldValue = RowObject.obs-cont-txt[3].
        WHEN "obs-cont-txt[4]":U THEN ASSIGN pFieldValue = RowObject.obs-cont-txt[4].
        WHEN "obs-cont-txt[5]":U THEN ASSIGN pFieldValue = RowObject.obs-cont-txt[5].
        WHEN "obs-cont-txt[6]":U THEN ASSIGN pFieldValue = RowObject.obs-cont-txt[6].
        WHEN "obs-cont-txt[7]":U THEN ASSIGN pFieldValue = RowObject.obs-cont-txt[7].
        WHEN "obs-cont-txt[8]":U THEN ASSIGN pFieldValue = RowObject.obs-cont-txt[8].
        WHEN "obs-cont-txt[9]":U THEN ASSIGN pFieldValue = RowObject.obs-cont-txt[9].
        WHEN "obs-cont-txt[10]":U THEN ASSIGN pFieldValue = RowObject.obs-cont-txt[10].
        WHEN "obs-fisco[1]":U THEN ASSIGN pFieldValue = RowObject.obs-fisco[1].
        WHEN "obs-fisco[2]":U THEN ASSIGN pFieldValue = RowObject.obs-fisco[2].
        WHEN "obs-fisco[3]":U THEN ASSIGN pFieldValue = RowObject.obs-fisco[3].
        WHEN "obs-fisco[4]":U THEN ASSIGN pFieldValue = RowObject.obs-fisco[4].
        WHEN "obs-fisco[5]":U THEN ASSIGN pFieldValue = RowObject.obs-fisco[5].
        WHEN "obs-fisco[6]":U THEN ASSIGN pFieldValue = RowObject.obs-fisco[6].
        WHEN "obs-fisco[7]":U THEN ASSIGN pFieldValue = RowObject.obs-fisco[7].
        WHEN "obs-fisco[8]":U THEN ASSIGN pFieldValue = RowObject.obs-fisco[8].
        WHEN "obs-fisco[9]":U THEN ASSIGN pFieldValue = RowObject.obs-fisco[9].
        WHEN "obs-fisco[10]":U THEN ASSIGN pFieldValue = RowObject.obs-fisco[10].
        WHEN "obs-fisco-txt[1]":U THEN ASSIGN pFieldValue = RowObject.obs-fisco-txt[1].
        WHEN "obs-fisco-txt[2]":U THEN ASSIGN pFieldValue = RowObject.obs-fisco-txt[2].
        WHEN "obs-fisco-txt[3]":U THEN ASSIGN pFieldValue = RowObject.obs-fisco-txt[3].
        WHEN "obs-fisco-txt[4]":U THEN ASSIGN pFieldValue = RowObject.obs-fisco-txt[4].
        WHEN "obs-fisco-txt[5]":U THEN ASSIGN pFieldValue = RowObject.obs-fisco-txt[5].
        WHEN "obs-fisco-txt[6]":U THEN ASSIGN pFieldValue = RowObject.obs-fisco-txt[6].
        WHEN "obs-fisco-txt[7]":U THEN ASSIGN pFieldValue = RowObject.obs-fisco-txt[7].
        WHEN "obs-fisco-txt[8]":U THEN ASSIGN pFieldValue = RowObject.obs-fisco-txt[8].
        WHEN "obs-fisco-txt[9]":U THEN ASSIGN pFieldValue = RowObject.obs-fisco-txt[9].
        WHEN "obs-fisco-txt[10]":U THEN ASSIGN pFieldValue = RowObject.obs-fisco-txt[10].
        WHEN "org-emissor":U THEN ASSIGN pFieldValue = RowObject.org-emissor.
        WHEN "placa":U THEN ASSIGN pFieldValue = RowObject.placa.
        WHEN "placa-reboque[1]":U THEN ASSIGN pFieldValue = RowObject.placa-reboque[1].
        WHEN "placa-reboque[2]":U THEN ASSIGN pFieldValue = RowObject.placa-reboque[2].
        WHEN "proc-emissao":U THEN ASSIGN pFieldValue = RowObject.proc-emissao.
        WHEN "razao-social":U THEN ASSIGN pFieldValue = RowObject.razao-social.
        WHEN "razao-social-dest":U THEN ASSIGN pFieldValue = RowObject.razao-social-dest.
        WHEN "razao-social-transp":U THEN ASSIGN pFieldValue = RowObject.razao-social-transp.
        WHEN "RepEmit":U THEN ASSIGN pFieldValue = RowObject.RepEmit.
        WHEN "serie-docto":U THEN ASSIGN pFieldValue = RowObject.serie-docto.
        WHEN "tipo-amb":U THEN ASSIGN pFieldValue = RowObject.tipo-amb.
        WHEN "tp-nf":U THEN ASSIGN pFieldValue = RowObject.tp-nf.
        WHEN "uf":U THEN ASSIGN pFieldValue = RowObject.uf.
        WHEN "uf-avulsa":U THEN ASSIGN pFieldValue = RowObject.uf-avulsa.
        WHEN "uf-destino":U THEN ASSIGN pFieldValue = RowObject.uf-destino.
        WHEN "uf-embarque":U THEN ASSIGN pFieldValue = RowObject.uf-embarque.
        WHEN "uf-end":U THEN ASSIGN pFieldValue = RowObject.uf-end.
        WHEN "uf-entrega":U THEN ASSIGN pFieldValue = RowObject.uf-entrega.
        WHEN "uf-placa":U THEN ASSIGN pFieldValue = RowObject.uf-placa.
        WHEN "uf-reboque[1]":U THEN ASSIGN pFieldValue = RowObject.uf-reboque[1].
        WHEN "uf-reboque[2]":U THEN ASSIGN pFieldValue = RowObject.uf-reboque[2].
        WHEN "uf-retirada":U THEN ASSIGN pFieldValue = RowObject.uf-retirada.
        WHEN "uf-transp":U THEN ASSIGN pFieldValue = RowObject.uf-transp.
        WHEN "versao-proc":U THEN ASSIGN pFieldValue = RowObject.versao-proc.
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

    /*--- Verifica se temptable RowObject estÿ dispon­vel, caso nÆo esteja serÿ
          retornada flag "NOK":U ---*/
    IF NOT AVAILABLE RowObject THEN 
        RETURN "NOK":U.

    CASE pFieldName:
        WHEN "dt-emissao":U THEN ASSIGN pFieldValue = RowObject.dt-emissao.
        WHEN "dt-emissao-dar":U THEN ASSIGN pFieldValue = RowObject.dt-emissao-dar.
        WHEN "dt-entrada":U THEN ASSIGN pFieldValue = RowObject.dt-entrada.
        WHEN "dt-pag-dar":U THEN ASSIGN pFieldValue = RowObject.dt-pag-dar.
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

    /*--- Verifica se temptable RowObject estÿ dispon­vel, caso nÆo esteja serÿ
          retornada flag "NOK":U ---*/
    IF NOT AVAILABLE RowObject THEN 
        RETURN "NOK":U.

    CASE pFieldName:
        WHEN "aliq-icms-ret":U THEN ASSIGN pFieldValue = RowObject.aliq-icms-ret.
        WHEN "base-icms":U THEN ASSIGN pFieldValue = RowObject.base-icms.
        WHEN "base-icms-ret":U THEN ASSIGN pFieldValue = RowObject.base-icms-ret.
        WHEN "base-icms-subs":U THEN ASSIGN pFieldValue = RowObject.base-icms-subs.
        WHEN "base-iss":U THEN ASSIGN pFieldValue = RowObject.base-iss.
        WHEN "base-prev-ret":U THEN ASSIGN pFieldValue = RowObject.base-prev-ret.
        WHEN "valor-cofins":U THEN ASSIGN pFieldValue = RowObject.valor-cofins.
        WHEN "valor-cofins-iss":U THEN ASSIGN pFieldValue = RowObject.valor-cofins-iss.
        WHEN "valor-cofins-ret":U THEN ASSIGN pFieldValue = RowObject.valor-cofins-ret.
        WHEN "valor-csll-ret":U THEN ASSIGN pFieldValue = RowObject.valor-csll-ret.
        WHEN "valor-dar":U THEN ASSIGN pFieldValue = RowObject.valor-dar.
        WHEN "valor-desc-fatura":U THEN ASSIGN pFieldValue = RowObject.valor-desc-fatura.
        WHEN "valor-desconto":U THEN ASSIGN pFieldValue = RowObject.valor-desconto.
        WHEN "valor-frete":U THEN ASSIGN pFieldValue = RowObject.valor-frete.
        WHEN "valor-icms":U THEN ASSIGN pFieldValue = RowObject.valor-icms.
        WHEN "valor-icms-ret":U THEN ASSIGN pFieldValue = RowObject.valor-icms-ret.
        WHEN "valor-icms-subs":U THEN ASSIGN pFieldValue = RowObject.valor-icms-subs.
        WHEN "valor-ii":U THEN ASSIGN pFieldValue = RowObject.valor-ii.
        WHEN "valor-ipi":U THEN ASSIGN pFieldValue = RowObject.valor-ipi.
        WHEN "valor-iss":U THEN ASSIGN pFieldValue = RowObject.valor-iss.
        WHEN "valor-liq-fatura":U THEN ASSIGN pFieldValue = RowObject.valor-liq-fatura.
        WHEN "valor-orig-fatura":U THEN ASSIGN pFieldValue = RowObject.valor-orig-fatura.
        WHEN "valor-outros":U THEN ASSIGN pFieldValue = RowObject.valor-outros.
        WHEN "valor-pis":U THEN ASSIGN pFieldValue = RowObject.valor-pis.
        WHEN "valor-pis-iss":U THEN ASSIGN pFieldValue = RowObject.valor-pis-iss.
        WHEN "valor-pis-ret":U THEN ASSIGN pFieldValue = RowObject.valor-pis-ret.
        WHEN "valor-prev-ret":U THEN ASSIGN pFieldValue = RowObject.valor-prev-ret.
        WHEN "valor-produto":U THEN ASSIGN pFieldValue = RowObject.valor-produto.
        WHEN "valor-seguro":U THEN ASSIGN pFieldValue = RowObject.valor-seguro.
        WHEN "valor-serv-ret-trans":U THEN ASSIGN pFieldValue = RowObject.valor-serv-ret-trans.
        WHEN "valor-total":U THEN ASSIGN pFieldValue = RowObject.valor-total.
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

    /*--- Verifica se temptable RowObject estÿ dispon­vel, caso nÆo esteja serÿ
          retornada flag "NOK":U ---*/
    IF NOT AVAILABLE RowObject THEN 
        RETURN "NOK":U.

    CASE pFieldName:
        WHEN "ch-acesso-nfe":U THEN ASSIGN pFieldValue = RowObject.ch-acesso-nfe.
        WHEN "cod-emitente":U THEN ASSIGN pFieldValue = RowObject.cod-emitente.
        WHEN "cod-municipio":U THEN ASSIGN pFieldValue = RowObject.cod-municipio.
        WHEN "cod-municipio-dest":U THEN ASSIGN pFieldValue = RowObject.cod-municipio-dest.
        WHEN "cod-municipio-end":U THEN ASSIGN pFieldValue = RowObject.cod-municipio-end.
        WHEN "cod-municipio-ent":U THEN ASSIGN pFieldValue = RowObject.cod-municipio-ent.
        WHEN "cod-municipio-fg":U THEN ASSIGN pFieldValue = RowObject.cod-municipio-fg.
        WHEN "cod-municipio-ret":U THEN ASSIGN pFieldValue = RowObject.cod-municipio-ret.
        WHEN "cod-municipio-transp":U THEN ASSIGN pFieldValue = RowObject.cod-municipio-transp.
        WHEN "cod-pais":U THEN ASSIGN pFieldValue = RowObject.cod-pais.
        WHEN "cod-pais-dest":U THEN ASSIGN pFieldValue = RowObject.cod-pais-dest.
        WHEN "cod-observa":U THEN ASSIGN pFieldValue = RowObject.cod-observa.
        WHEN "idi-orig-trad":U THEN ASSIGN pFieldValue = RowObject.idi-orig-trad.
        WHEN "idi-situacao":U THEN ASSIGN pFieldValue = RowObject.idi-situacao.
        OTHERWISE RETURN "NOK":U.
    END CASE.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getKey DBOProgram 
PROCEDURE getKey :
/*------------------------------------------------------------------------------
  Purpose:     Retorna valores dos campos do ­ndice unico
  Parameters:  
               retorna valor do campo ch-acesso-comp-nfe
               retorna valor do campo idi-orig-trad
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER pch-acesso-comp-nfe LIKE nfe003.ch-acesso-comp-nfe NO-UNDO.
    DEFINE OUTPUT PARAMETER pidi-orig-trad LIKE nfe003.idi-orig-trad NO-UNDO.

    /*--- Verifica se temptable RowObject estÿ dispon­vel, caso nÆo esteja serÿ
          retornada flag "NOK":U ---*/
    IF NOT AVAILABLE RowObject THEN 
       RETURN "NOK":U.

    ASSIGN pch-acesso-comp-nfe = RowObject.ch-acesso-comp-nfe
           pidi-orig-trad = RowObject.idi-orig-trad.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getLogField DBOProgram 
PROCEDURE getLogField :
/*------------------------------------------------------------------------------
  Purpose:     Retorna valor de campos do tipo l½gico
  Parameters:  
               recebe nome do campo
               retorna valor do campo
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pFieldName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pFieldValue AS LOGICAL NO-UNDO.

    /*--- Verifica se temptable RowObject estÿ dispon­vel, caso nÆo esteja serÿ
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

    /*--- Verifica se temptable RowObject estÿ dispon­vel, caso nÆo esteja serÿ
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

    /*--- Verifica se temptable RowObject estÿ dispon­vel, caso nÆo esteja serÿ
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
  Purpose:     Reposiciona registro com base no ­ndice unico
  Parameters:  
               recebe valor do campo ch-acesso-comp-nfe
               recebe valor do campo idi-orig-trad
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pch-acesso-comp-nfe LIKE nfe003.ch-acesso-comp-nfe NO-UNDO.
    DEFINE INPUT PARAMETER pidi-orig-trad LIKE nfe003.idi-orig-trad NO-UNDO.

    FIND FIRST bfnfe003 WHERE 
        bfnfe003.ch-acesso-comp-nfe = pch-acesso-comp-nfe AND 
        bfnfe003.idi-orig-trad = pidi-orig-trad NO-LOCK NO-ERROR.

    /*--- Verifica se registro foi encontrado, em caso de erro serÿ retornada flag "NOK":U ---*/
    IF NOT AVAILABLE bfnfe003 THEN 
        RETURN "NOK":U.

    /*--- Reposiciona query atrav?s de rowid e verifica a ocorr?ncia de erros, caso
          existam erros serÿ retornada flag "NOK":U ---*/
    RUN repositionRecord IN THIS-PROCEDURE (INPUT ROWID(bfnfe003)).
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
            WHERE {&TableName}.idi-orig-trad  = i-idi-orig-trad
              AND {&TableName}.cod-emitente  >= c-cod-emitente-ini
              AND {&TableName}.cod-emitente  <= c-cod-emitente-fim
              AND {&TableName}.cod-estabel   >= c-cod-estabel-ini 
              AND {&TableName}.cod-estabel   <= c-cod-estabel-fim 
              AND {&TableName}.serie-docto   >= c-serie-ini       
              AND {&TableName}.serie-docto   <= c-serie-fim       
              AND {&TableName}.nro-docto     >= c-nro-docto-ini   
              AND {&TableName}.nro-docto     <= c-nro-docto-fim   
              AND {&TableName}.dt-emissao    >= dt-emissao-ini    
              AND {&TableName}.dt-emissao    <= dt-emissao-fim    
              AND (     ({&TableName}.idi-situacao = 1 AND tg-digitada)
                   OR   ({&TableName}.idi-situacao = 2 AND tg-erro-neg)
                   OR   ({&TableName}.idi-situacao = 3 AND tg-atualizada)
                   OR   ({&TableName}.idi-situacao = 4 AND tg-eliminada)
                   OR   ({&TableName}.idi-situacao = 5 AND tg-danfe)
                   OR   ({&TableName}.idi-situacao = 6 AND tg-liberado)
                   OR   ({&TableName}.idi-situacao = 7 AND tg-conferido)
                   )
        .

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openQueryTraduzido DBOProgram 
PROCEDURE openQueryTraduzido :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    OPEN QUERY {&QueryName} 
        FOR EACH {&TableName} NO-LOCK
            WHERE {&TableName}.idi-orig-trad = 2.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-rateio-frete DBOProgram 
PROCEDURE pi-rateio-frete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def input  param de-vl-tot-item  as decimal        no-undo.
    def output param de-rateio       as decimal        no-undo.

    def var de-valor-aux             as decimal          no-undo.
    def var de-tot-preco             as decimal          no-undo.
    def var de-tot-peso              as decimal          no-undo.
    def var de-tot-rat               as decimal          no-undo.

    assign de-valor-aux = abs(tt-docum-est.despesa-nota - tt-docum-est.valor-frete).

    if  de-valor-aux <= 0 then 
        return.
    
    assign de-rateio = truncate( de-vl-tot-item * de-valor-aux / 
                               ((tt-docum-est.valor-mercad + int(tt-docum-est.valor-mercad = 0)) ), 2).

    assign de-tot-preco = 0
           de-tot-peso  = 0
           de-tot-rat   = 0.
    
    for each tt-item-doc-est {cdp/cd8900.i tt-item-doc-est tt-docum-est } no-lock:
    
        if (    not tt-docum-est.rec-fisico
            and ((tt-item-doc-est.despesas[1]  <> 0)) )
        then assign de-tot-preco = de-tot-preco + tt-item-doc-est.preco-total[1]
                    de-tot-peso  = de-tot-peso  + tt-item-doc-est.peso-liquido
                    de-tot-rat   = de-tot-rat   + (tt-item-doc-est.despesas[1] - tt-item-doc-est.pr-total-cmi).
    end.
    
    if ( de-tot-preco + de-vl-tot-item) >= tt-docum-est.valor-mercad then 
        assign de-rateio = (tt-docum-est.despesa-nota - 
                            tt-docum-est.valor-frete) - de-tot-rat.

                             
    if  de-rateio < 0 
    or  de-rateio = ? then 
        assign de-rateio = 0.
    
    /* fim do programa */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE geraAgregado DBOProgram 
PROCEDURE geraAgregado :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE i-seq-item AS INTEGER     NO-UNDO.

    DEFINE VARIABLE h-boin223  AS HANDLE      NO-UNDO.
    DEFINE VARIABLE h-boin223a AS HANDLE      NO-UNDO.
    DEFINE BUFFER bnfe003 FOR nfe003.
    DEFINE BUFFER bnfe013 FOR nfe013.
        
    DEFINE VARIABLE i-pedido  AS INTEGER     NO-UNDO.
    DEFINE VARIABLE i-ordem   AS INTEGER     NO-UNDO.
    DEFINE VARIABLE i-parcela AS INTEGER     NO-UNDO.

    DEFINE VARIABLE h-acomp   AS HANDLE      NO-UNDO.

    empty temp-table tt2-item-doc-est.

    FOR FIRST nfe016
        WHERE nfe016.ch-acesso-comp-nfe = bf{&TableName}.ch-acesso-comp-nfe NO-LOCK:
    END.
    IF AVAIL nfe016 THEN do:
        run utp/ut-acomp.p persistent set h-acomp.

        run pi-inicializar in h-acomp ("Gerando Agregado").

        FIND first bnfe003 EXCLUSIVE-LOCK WHERE 
             bnfe003.cod-emitente = nfe016.cod-emitente AND
             bnfe003.serie-docto  = nfe016.serie-docto AND
             bnfe003.nro-docto    = nfe016.nro-docto AND
             bnfe003.ch-acesso-comp-nfe <> bf{&TableName}.ch-acesso-comp-nfe and
            (bnfe003.idi-situacao = 6 
             OR bnfe003.idi-situacao = 7) NO-ERROR.
        IF NOT AVAIL bnfe003 THEN DO:
            CREATE tt-erro.
            ASSIGN tt-erro.cd-erro   = 17006
                   tt-erro.desc-erro = "Nota agregada nÆo encontrada ou situa‡Æo difere de ~"Liberado para Integra‡Æo~"".
            run pi-finalizar in h-acomp.
            RETURN "NOK".
        END.
        
        CREATE tt-movto-pend.
        ASSIGN tt-movto-pend.cod-emitente   = docum-est.cod-emitente
               tt-movto-pend.serie-docto    = docum-est.serie-docto  
               tt-movto-pend.nro-docto      = docum-est.nro-docto
               tt-movto-pend.nat-operacao   = docum-est.nat-operacao
             
               tt-movto-pend.cod-observa    = bf{&TableName}.cod-observa
               tt-movto-pend.conta-contabil = nfe016.conta-contabil
               tt-movto-pend.nro-comp       = nfe016.nro-docto
               tt-movto-pend.serie-comp     = nfe016.serie-docto
               tt-movto-pend.nat-comp       = nfe016.nat-operacao
               tt-movto-pend.tipo           = 2 /* Agregado */ 
               SUBSTRING(tt-movto-pend.char-1,8,60) = bnfe003.ch-acesso-comp-nfe.
               .

        IF NOT VALID-HANDLE(h-boin223) THEN
            RUN inbo/boin223.p PERSISTENT SET h-boin223.

        RUN openQueryStatic   IN h-boin223 (INPUT "Main":U) NO-ERROR.

        run setHandleDocumEst in h-boin223 (h-boin090, rowid(docum-est)).
        
        RUN emptyRowErrors    IN h-boin223 .
        RUN findDocumento     IN h-boin223 (INPUT tt-movto-pend.cod-emitente,
                                            INPUT tt-movto-pend.serie-docto,
                                            INPUT tt-movto-pend.nro-docto,
                                            INPUT tt-movto-pend.nat-operacao).
        RUN findUsuario       IN h-boin223 .  
        RUN newRecord         IN h-boin223 .  
        RUN setRecord         IN h-boin223 (INPUT TABLE tt-movto-pend).
        RUN createRecord      IN h-boin223 .

        run getRowErrors IN h-boin223 (output table RowErrors).

        for each rowErrors:
            create tt-erro.
            assign tt-erro.cd-erro   = RowErrors.ErrorNumber
                   tt-erro.desc-erro = RowErrors.ErrorDescription.
            run pi-finalizar in h-acomp.
            return "NOK".
        end.

        i-seq-item = 10.

        empty temp-table tt-item-doc-est.

        for each bnfe013 no-lock 
           where bnfe013.ch-acesso-comp-nfe = bnfe003.ch-acesso-comp-nfe
             and bnfe013.idi-orig-trad      = 2:

            assign i-pedido  = bnfe013.num-pedido  
                   i-ordem   = bnfe013.numero-ordem
                   i-parcela = bnfe013.parcela.

            if i-ordem = 0 then do:
                find ordem-compra no-lock where 
                     ordem-compra.ordem-servic = if bnfe013.nr-ord-produ <> 0 then bnfe013.nr-ord-produ else i-nr-ord-produ no-error.
                if avail ordem-compra then do:
                    find first prazo-compra no-lock 
                         where prazo-compra.numero-ordem = ordem-compra.numero-ordem 
                           and prazo-compra.situacao     = 2 no-error.
                    if avail prazo-compra then
                        assign i-pedido  = ordem-compra.num-pedido  
                               i-ordem   = ordem-compra.numero-ordem
                               i-parcela = prazo-compra.parcela.
                end.
            end.

            create tt-item-doc-est.
            assign tt-item-doc-est.serie-docto    = nfe016.serie-docto     
                   tt-item-doc-est.nro-docto      = nfe016.nro-docto       
                   tt-item-doc-est.cod-emitente   = docum-est.cod-emitente    
                   tt-item-doc-est.nat-operacao   = nfe016.nat-operacao       
    
                   tt-item-doc-est.sequencia      = i-seq-item                     
                   i-seq-item                     = i-seq-item + param-re.inc-seq
                   tt-item-doc-est.it-codigo      = IF AVAIL item-fornec THEN item-fornec.it-codigo ELSE bnfe013.it-codigo
                   tt-item-doc-est.num-pedido     = i-pedido 
                   tt-item-doc-est.numero-ordem   = i-ordem  
                   tt-item-doc-est.parcela        = i-parcela
                   tt-item-doc-est.nr-ord-produ   = if bnfe013.nr-ord-produ <> 0 then bnfe013.nr-ord-produ else i-nr-ord-produ
                   tt-item-doc-est.qt-do-forn     = bnfe013.qtd-comercial
                   tt-item-doc-est.un             = bnfe013.un-interna
                   tt-item-doc-est.quantidade     = bnfe013.qtd-interna
                   tt-item-doc-est.preco-unit[1]  = bnfe013.preco-unit   
                   tt-item-doc-est.preco-total[1] = bnfe013.preco-total
                   tt-item-doc-est.class-fiscal   = REPLACE(bnfe013.class-fiscal,".", "")
                   tt-item-doc-est.log-1          = bnfe013.log-fifo-oc
                   tt-item-doc-est.log-fifo-oc    = bnfe013.log-fifo-oc
                   tt-item-doc-est.conta-contabil = REPLACE(bnfe013.conta-contabil,".", "")
                   tt-item-doc-est.ct-codigo      = REPLACE(bnfe013.conta-contabil,".", "")
                   tt-item-doc-est.sc-codigo      = REPLACE(bnfe013.centro-custo,".", "")
                   tt-item-doc-est.cod-refer      = bnfe013.cod-refer     
                   tt-item-doc-est.cod-depos      = bnfe013.cod-depos     
                   tt-item-doc-est.cod-localiz    = bnfe013.cod-localiz   
                   tt-item-doc-est.lote           = bnfe013.lote          
                   tt-item-doc-est.dt-vali-lote   = bnfe013.dt-vali-lote
    
                   &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
                        tt-item-doc-est.cod-unid-negoc = bnfe013.cod-unid-negoc
                        tt-item-doc-est.int-1          = bnfe013.int-1
                   &ENDIF.
        END.

        if not valid-handle(h-boin090) then
            run inbo/boin090.p persistent set h-boin090.

        run openQueryStatic IN h-boin090("main"). 
        run gotoKey in h-boin090 (input nfe016.serie-docto,
                                  input nfe016.nro-docto,
                                  input nfe016.cod-emitente,
                                  input nfe016.nat-operacao).

        run emptyRowErrors in h-boin090.

        run getRecord in h-boin090 (output table tt-docum-est).
        find first tt-docum-est no-error.

        if not valid-handle(h-boin176) then
            run inbo/boin176.p persistent set h-boin176.

        run setConstraintOfDocumEst IN h-boin176(input tt-docum-est.cod-emitente,
                                                 input tt-docum-est.serie-docto,
                                                 input tt-docum-est.nro-docto,
                                                 input tt-docum-est.nat-operacao).

        run openQueryStatic in h-boin176 ('main':u).

        run emptyRowErrors in h-boin176.
        
        run setDefaultCalculoImpostos in h-boin176
            (input true,   /* codigo de tributacao */
             input true).  /* aliquota */
        
        run linktoDocumEst in h-boin176(h-boin090).
        
        run emptyRowObject in h-boin176.     

        CREATE tt2-item-doc-est.
        FOR EACH tt-item-doc-est:
            BUFFER-COPY tt-item-doc-est TO tt2-item-doc-est.
        
        run setRecord in h-boin176 (table tt2-item-doc-est).

        run getRowErrors IN h-boin176 (output table RowErrors ).

        find first rowErrors 
             where RowErrors.ErrorSubType = "ERROR":U
               and RowErrors.ErrorType    <> "INTERNAL":U no-error.
        if avail RowErrors then do:
            create tt-erro.
            assign tt-erro.cd-erro   = RowErrors.ErrorNumber
                   tt-erro.desc-erro = RowErrors.ErrorDescription.
            run pi-finalizar in h-acomp.
            return "NOK".
        end.
                        
        run validateRecord in h-boin176 (input 'Create':u).
        run getRowErrors   in h-boin176 (output table RowErrors ).

        find first rowErrors 
             where RowErrors.ErrorSubType = "ERROR":U
               and RowErrors.ErrorType    <> "INTERNAL":U no-error.
        if avail RowErrors then do:
            create tt-erro.
            assign tt-erro.cd-erro   = RowErrors.ErrorNumber
                   tt-erro.desc-erro = RowErrors.ErrorDescription.
            run pi-finalizar in h-acomp.
            return "NOK".
        end.
        
        run createRecord in h-boin176.
        end.
        run getRowErrors IN h-boin176 (output table RowErrors ).

        find first rowErrors 
             where RowErrors.ErrorSubType = "ERROR":U
               and RowErrors.ErrorType    <> "INTERNAL":U no-error.
        if avail RowErrors then do:
            create tt-erro.
            assign tt-erro.cd-erro   = RowErrors.ErrorNumber
                   tt-erro.desc-erro = RowErrors.ErrorDescription.
            run pi-finalizar in h-acomp.
            return "NOK".
        end.
        
        run emptyRowErrors in h-boin090.
        run setHandleDocumEst in h-boin176 (input h-boin090).

        run getTotalizaNota in h-boin176 (input false).

        run getRowErrors IN h-boin176 (output table RowErrors ).

        find first rowErrors 
             where RowErrors.ErrorSubType = "ERROR":U
               and RowErrors.ErrorType    <> "INTERNAL":U no-error.
        if avail RowErrors then do:
            create tt-erro.
            assign tt-erro.cd-erro   = RowErrors.ErrorNumber
                   tt-erro.desc-erro = RowErrors.ErrorDescription. 
            run pi-finalizar in h-acomp.
            return "NOK".
        end.

        run transferTotalItensNota in h-boin176
            (input tt-docum-est.cod-emitente,
             input tt-docum-est.serie-docto,
             input tt-docum-est.nro-docto,
             input tt-docum-est.nat-operacao).

        run getRowErrors in h-boin176 (output table RowErrors ).

        find first rowErrors 
             where RowErrors.ErrorSubType = "ERROR":U
               and RowErrors.ErrorType    <> "INTERNAL":U no-error.
        if avail RowErrors then do:
            create tt-erro.
            assign tt-erro.cd-erro   = RowErrors.ErrorNumber
                   tt-erro.desc-erro = RowErrors.ErrorDescription.
            run pi-finalizar in h-acomp.
            return "NOK".
        end.
        

        run validateValues in h-boin090.
        if return-value <> 'ok':u then do:
            run getRowErrors IN h-boin090 (output table RowErrors ).
            find first rowErrors 
                 where RowErrors.ErrorSubType = "ERROR":U
                   and RowErrors.ErrorType    <> "INTERNAL":U no-error.
            if avail RowErrors then do:
                create tt-erro.
                assign tt-erro.cd-erro   = RowErrors.ErrorNumber
                       tt-erro.desc-erro = RowErrors.ErrorDescription.
                run pi-finalizar in h-acomp.
                return "NOK".
            end.                        
        end.

        run getRowid in h-boin090 (output tt-docum-est.r-rowid).

        if can-find(natur-oper where
                    natur-oper.nat-operacao = tt-docum-est.nat-operacao and
                    natur-oper.emite-duplic = true) then do:
            /* gera duplicatas */
            run rep/re9341.p (tt-docum-est.r-rowid, input no).
            
            if return-value <> 'ok':u then do:
                CREATE RowErrors.
                ASSIGN RowErrors.errorSequence    = 99
                       RowErrors.ErrorNumber      = 17006
                       RowErrors.ErrorType        = "EMS":U
                       RowErrors.ErrorSubType     = "ERROR":U
                       RowErrors.ErrorDescription = "Erro na cria‡Æo das duplicatas!".
            end.
            find first rowErrors 
                 where RowErrors.ErrorSubType = "ERROR":U
                   and RowErrors.ErrorType    <> "INTERNAL":U no-error.
            if avail RowErrors then do:
                create tt-erro.
                assign tt-erro.cd-erro   = RowErrors.ErrorNumber
                       tt-erro.desc-erro = RowErrors.ErrorDescription.
                run pi-finalizar in h-acomp.
                return "NOK".
            end.
        end. 

        if not valid-handle(h-boin223a) then
            run inbo/boin223.p persistent set h-boin223a.

        run openQueryStatic in h-boin223a (input "Main":U) no-error.

        run findDocumento in h-boin223a( input tt-docum-est.cod-emitente,
                                         input tt-docum-est.serie-docto,
                                         input tt-docum-est.nro-docto,
                                         input tt-docum-est.nat-operacao ).

        run CreateAcabadoByOP IN h-boin223a.
        if return-value = "NOK":U then do:
            run getRowErrors in h-boin223a ( output table RowErrors ).
            create tt-erro.
            assign tt-erro.cd-erro   = RowErrors.ErrorNumber
                   tt-erro.desc-erro = "Acabado: " + RowErrors.ErrorDescription.
            run pi-finalizar in h-acomp.
            return "NOK".
        end.

        assign bnfe003.idi-situacao = 1.

        delete procedure h-boin223a.
        assign h-boin223a = ?.

        delete procedure h-boin223.
        assign h-boin223 = ?.

        run pi-finalizar in h-acomp.

    end.
    
    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reprocessaNota DBOProgram 
PROCEDURE reprocessaNota :
/*------------------------------------------------------------------------------
  Purpose:     Reprocessa NF-e 
  Parameters:  pr-nota -> ROWID nota fiscal
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER pr-nota AS ROWID    NO-UNDO.
    DEFINE INPUT  PARAMETER l-integra AS LOGICAL  NO-UNDO.
    DEFINE OUTPUT PARAMETER l-ok      AS LOGICAL  NO-UNDO.

    DEFINE VARIABLE i-seq-item       AS INTEGER     NO-UNDO.
    DEFINE VARIABLE de-indice        AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE de-qtd-saldo     AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE l-agregado       AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE c-conta-contabil AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE l-falta-nat AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE c-nat-inf AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE c-conta AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE c-centro-custo AS CHARACTER   NO-UNDO.
    
    EMPTY TEMP-TABLE tt-docum-est.
    EMPTY TEMP-TABLE tt-item-doc-est.
    EMPTY TEMP-TABLE tt-natureza.
    EMPTY TEMP-TABLE tt-erro.
    EMPTY TEMP-TABLE tt2-item-doc-est.
    empty temp-table tt-saldo.
    empty temp-table tt-item-terc.

    FOR FIRST param-re FIELDS (inc-seq seq-item-um)
        WHERE param-re.usuario = c-seg-usuario NO-LOCK:
    END.

    IF  NOT AVAIL param-re THEN
        RETURN "NOK":U.

    ASSIGN l-falta-nat = NO 
           c-nat-inf = '' .
    FOR FIRST bf{&TableName} NO-LOCK
        WHERE ROWID(bf{&TableName}) = pr-nota:
        /** VERIFICA MAIS DE UMA NATUREZA DE OPERACAO **/
        FOR EACH nfe013 FIELDS (nat-operacao it-codigo)
            WHERE nfe013.ch-acesso-comp-nfe = bf{&TableName}.ch-acesso-comp-nfe
              AND nfe013.idi-orig-trad      = 2 NO-LOCK:

            IF trim(nfe013.nat-operacao) = '' THEN DO:
                ASSIGN l-falta-nat = YES 
                       c-nat-inf = c-nat-inf + ' Item: ' + nfe013.it-codigo + chr(13) .

            END.
        END.
    END.

    IF l-falta-nat = YES THEN do trans:
        IF c-nat-inf <> '' THEN
            ASSIGN c-nat-inf = c-nat-inf + ' estÆo sem nat opera‡Æo !!' .


        IF l-integra THEN
            RUN utp/ut-msgs.p (INPUT "SHOW":U,
                               INPUT 17006,
                               INPUT "Preencher Natureza dos Itens!" + "~~" + c-nat-inf).

        /*RUN ze-erro.p (1,
                       'Preencher Natureza dos Itens' ,
                        c-nat-inf).*/
                        
        CREATE nfe017.
        ASSIGN nfe017.ch-acesso-comp-nfe = bf{&TableName}.ch-acesso-comp-nfe
               nfe017.idi-orig-trad      = 2
               nfe017.dt-msg             = TODAY
               nfe017.hr-msg             = STRING(TIME, "HH:MM:SS")
               nfe017.log-ativo          = YES
               nfe017.cd-msg             = 777
               nfe017.texto-msg          = 'Proc Fiscal nÆo pode ser executado com itens sem natureza opera‡Æo'
               nfe017.seq-msg            = NEXT-VALUE(seq-msg-nfe).
        
        IF NOT l-integra THEN DO:
            FOR FIRST bf{&TableName} FIELDS (idi-situacao)
                WHERE ROWID(bf{&TableName}) = pr-nota EXCLUSIVE-LOCK:
                ASSIGN bf{&TableName}.idi-situacao = 7 /* Conferido para integra‡Æo - Simula‡Æo */ .
            END.
        
            RELEASE bf{&TableName}.

        END.
        RETURN "NOK":U.

    END.    

    ASSIGN l-ok = YES.
    FOR FIRST bf{&TableName} NO-LOCK
        WHERE ROWID(bf{&TableName}) = pr-nota:
        /** VERIFICA MAIS DE UMA NATUREZA DE OPERACAO **/
        FOR EACH nfe013 FIELDS (nat-operacao conta-contabil centro-custo)
            WHERE nfe013.ch-acesso-comp-nfe = bf{&TableName}.ch-acesso-comp-nfe
              AND nfe013.idi-orig-trad      = 2 NO-LOCK:
            IF NOT CAN-FIND(FIRST tt-natureza
                            WHERE tt-natureza.nat-operacao = nfe013.nat-operacao) THEN DO:
                CREATE tt-natureza.
                ASSIGN tt-natureza.nat-operacao = nfe013.nat-operacao.
            END.
            IF  nfe013.conta-contabil <> "" THEN
                ASSIGN c-conta = REPLACE(nfe013.conta-contabil,".", "")
                       c-centro-custo = REPLACE(nfe013.centro-custo,".", "").
        END.

        DO TRANSACTION ON ERROR UNDO, LEAVE:

            /** CRIA CONFORME CADA NATUREZA DE OPERACAO **/
            FOR EACH tt-natureza:
                
                EMPTY TEMP-TABLE tt-docum-est.
                EMPTY TEMP-TABLE tt-item-doc-est.
    
                /***** DOCUMENTO *****/
                CREATE tt-docum-est.
                ASSIGN tt-docum-est.cod-emitente         = bf{&TableName}.cod-emitente
                       tt-docum-est.nro-docto            = bf{&TableName}.nro-docto  
                       tt-docum-est.nat-operacao         = tt-natureza.nat-operacao
                       tt-docum-est.serie-docto          = bf{&TableName}.serie-docto
                       tt-docum-est.cod-observa          = bf{&TableName}.cod-observa
                       tt-docum-est.cod-estabel          = bf{&TableName}.cod-estabel
                       tt-docum-est.dt-emissao           = bf{&TableName}.dt-emissao
                       tt-docum-est.dt-trans             = bf{&TableName}.dt-transacao
                       tt-docum-est.mod-frete            = int(bf{&TableName}.mod-frete) 
                       tt-docum-est.valor-frete          = bf{&TableName}.valor-frete
                       tt-docum-est.ct-transit           = c-conta
                       tt-docum-est.sc-transit           = c-centro-custo
                       tt-docum-est.esp-docto            = if bf{&TableName}.lg-devol then 20
                                                                                      else 21
                &IF "{&bf_mat_versao_ems}" < "2.07" &THEN
                  OVERLAY(tt-docum-est.char-1,93,60) = bf{&TableName}.ch-acesso-comp-nfe /*nfe003.ch-acesso-comp-nfe*/
                  OVERLAY(tt-docum-est.char-1,153,1) = "1".
                &ELSE
                      tt-docum-est.cod-chave-aces-nf-eletro = bf{&TableName}.ch-acesso-comp-nfe /*nfe003.ch-acesso-comp-nfe*/
                      tt-docum-est.cdn-sit-nfe              = 1.
                &ENDIF
    
                
                &IF "{&bf_dis_versao_ems}" >= "2.062" &THEN
                    IF  bf{&TableName}.razao-social-transp <> ? THEN
                        ASSIGN tt-docum-est.nome-transp = bf{&TableName}.razao-social-transp.
        
                    IF  bf{&TableName}.placa <> ? THEN
                        ASSIGN tt-docum-est.cod-placa[1] = bf{&TableName}.placa .
        
                    IF bf{&TableName}.uf-placa <> ? THEN
                        ASSIGN tt-docum-est.cod-uf-placa[1] = bf{&TableName}.uf-placa.
                &ELSE
                    IF bf{&TableName}.razao-social-transp <> ? THEN
                        ASSIGN OVERLAY(tt-docum-est.char-2,102,12) = bf{&TableName}.razao-social-transp.
        
                    IF bf{&TableName}.placa <> ? THEN
                        ASSIGN OVERLAY(tt-docum-est.char-2,115,7) = bf{&TableName}.placa.
        
                    IF bf{&TableName}.uf-placa <> ? THEN
                        ASSIGN OVERLAY(tt-docum-est.char-2,136,2) = bf{&TableName}.uf-placa.
                &ENDIF
                
                /***** DOCUMENTO *****/   
                /***** ITENS DO DOCUMENTO *****/
                ASSIGN i-seq-item = param-re.seq-item-um.
    
                FIND FIRST nfe019 NO-LOCK NO-ERROR.
                
                FOR EACH nfe013 NO-LOCK
                    WHERE nfe013.ch-acesso-comp-nfe = bf{&TableName}.ch-acesso-comp-nfe
                      AND nfe013.idi-orig-trad      = 2:
                    
                    /*RUN verificaDiferencaCompras (OUTPUT l-ok).*/
                    
                    IF l-ok = NO THEN
                        NEXT.
                    
                    IF nfe013.nat-operacao <> tt-natureza.nat-operacao THEN
                        NEXT.
    
                    /** DE-PARA DO C…DIGO DO ITEM *
                    FOR FIRST bf-nfe013 FIELDS (it-codigo)
                        WHERE bf-nfe013.ch-acesso-comp-nfe = bf{&TableName}.ch-acesso-comp-nfe
                          AND bf-nfe013.idi-orig-trad      = 1 
                          AND bf-nfe013.seq-item           = nfe013.seq-item NO-LOCK:
    
                        FOR FIRST item-fornec FIELDS (it-codigo)
                            WHERE item-fornec.item-do-forn = bf-nfe013.it-codigo          
                              AND item-fornec.cod-emitente = bf{&TableName}.cod-emitente NO-LOCK:
                        END.
                    END.
                    * DE-PARA DO C…DIGO DO ITEM **/
                    
                    IF NOT AVAIL natur-oper
                    OR (    AVAIL natur-oper
                        AND natur-oper.nat-operacao <> tt-natureza.nat-operacao) THEN
                        FOR FIRST natur-oper FIELDS (tp-oper-terc log-oper-triang nat-operacao)
                            WHERE natur-oper.nat-operacao = tt-natureza.nat-operacao NO-LOCK:
                        END.
                    
                    IF CAN-FIND(FIRST nfe014
                                WHERE nfe014.ch-acesso-comp-nfe = nfe013.ch-acesso-comp-nfe
                                  AND nfe014.seq-item           = nfe013.seq-item          ) THEN DO:

                        FOR EACH nfe014 NO-LOCK
                            WHERE nfe014.ch-acesso-comp-nfe = nfe013.ch-acesso-comp-nfe
                              AND nfe014.seq-item           = nfe013.seq-item:

                            FOR FIRST saldo-terc FIELDS (quantidade dec-1)
                                WHERE saldo-terc.cod-emitente = nfe014.cod-emitente
                                  AND saldo-terc.nro-docto    = nfe014.nro-docto   
                                  AND saldo-terc.serie-docto  = nfe014.serie-docto 
                                  AND saldo-terc.nat-operacao = nfe014.nat-operacao
                                  AND saldo-terc.it-codigo    = nfe014.it-codigo   
                                  AND saldo-terc.cod-refer    = nfe013.cod-refer   
                                  AND saldo-terc.sequencia    = nfe014.seq-saldo-terc NO-LOCK:   
                            END.
                            IF  AVAIL saldo-terc 
                            AND saldo-terc.quantidade - saldo-terc.dec-1 < nfe014.qtd-alocada THEN DO:
                                CREATE tt-erro.
                                RUN utp/ut-msgs.p (INPUT "HELP":U,
                                                   INPUT 5934,
                                                   INPUT nfe013.seq-item).
                                ASSIGN tt-erro.cd-erro   = 5934
                                       tt-erro.desc-erro = RETURN-VALUE
                                       l-ok              = NO.
    
                                NEXT.
                            END.
    
                            ASSIGN de-indice = nfe014.qtd-alocada / nfe013.qtd-comercial.

                            CREATE tt-item-doc-est.
                            ASSIGN tt-item-doc-est.serie-docto    = bf{&TableName}.serie-docto     
                                   tt-item-doc-est.nro-docto      = bf{&TableName}.nro-docto       
                                   tt-item-doc-est.cod-emitente   = bf{&TableName}.cod-emitente    
                                   tt-item-doc-est.nat-operacao   = tt-natureza.nat-operacao       
                                   tt-item-doc-est.sequencia      = i-seq-item                     
                                   i-seq-item                     = i-seq-item + param-re.inc-seq  
                    
                                   tt-item-doc-est.it-codigo      = IF AVAIL item-fornec THEN item-fornec.it-codigo ELSE nfe013.it-codigo
                                   tt-item-doc-est.num-pedido     = nfe013.num-pedido  
                                   tt-item-doc-est.numero-ordem   = nfe013.numero-ordem
                                   tt-item-doc-est.parcela        = nfe013.parcela     
                                   tt-item-doc-est.nr-ord-produ   = nfe013.nr-ord-produ
                                   tt-item-doc-est.qt-do-forn     = nfe014.qtd-alocada
                                   tt-item-doc-est.un             = nfe013.un-comercial
                                   tt-item-doc-est.quantidade     = nfe013.qtd-interna * de-indice
                                   tt-item-doc-est.preco-unit[1]  = nfe013.preco-unit
                                   tt-item-doc-est.preco-total[1] = nfe013.preco-total * de-indice
                                   tt-item-doc-est.class-fiscal   = REPLACE(nfe013.class-fiscal,".", "")
                                   tt-item-doc-est.log-1          = /*YES*/ nfe013.log-fifo-oc
                                   tt-item-doc-est.log-fifo-oc    = /*YES*/ nfe013.log-fifo-oc
                                   tt-item-doc-est.ct-codigo      = REPLACE(nfe013.conta-contabil,".", "")
                                   tt-item-doc-est.sc-codigo      = REPLACE(nfe013.centro-custo,".", "")
                                   tt-item-doc-est.cod-refer      = nfe013.cod-refer     
                                   tt-item-doc-est.cod-depos      = nfe013.cod-depos     
                                   tt-item-doc-est.cod-localiz    = nfe013.cod-localiz   
                                   tt-item-doc-est.lote           = nfe013.lote          
                                   tt-item-doc-est.dt-vali-lote   = nfe013.dt-vali-lote
                                   tt-item-doc-est.pr-total-cmi   = nfe013.vl-frete
                                   tt-item-doc-est.despesas       = nfe013.vl-frete
                                   tt-item-doc-est.cd-trib-icm    = 0
                                   tt-item-doc-est.cd-trib-ipi    = 0
                                   /* Devolu‡Æo */
                                   tt-item-doc-est.nro-comp       = nfe013.nro-comp   when bf{&TableName}.lg-devol
                                   tt-item-doc-est.serie-comp     = nfe013.serie-comp when bf{&TableName}.lg-devol
                                   tt-item-doc-est.seq-comp       = nfe013.seq-comp   when bf{&TableName}.lg-devol
                                   tt-item-doc-est.reabre-pd      = nfe013.reabre-pd  when bf{&TableName}.lg-devol
            
                                   &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
                                        tt-item-doc-est.cod-unid-negoc = nfe013.cod-unid-negoc
                                        tt-item-doc-est.int-1          = nfe013.int-1
                                   &ENDIF.
                                   
                            IF AVAIL natur-oper THEN DO:
                                IF  natur-oper.tp-oper-terc = 2       /* Retorno Beneficiamento  */ 
                                 OR natur-oper.tp-oper-terc = 4       /* Faturamento Consignacao */
                                 OR natur-oper.tp-oper-terc = 5 THEN /* Devolucao Consignacao   */ 
                                    ASSIGN tt-item-doc-est.serie-comp = nfe014.serie-docto
                                           tt-item-doc-est.nro-comp   = nfe014.nro-docto
                                           tt-item-doc-est.nat-comp   = nfe014.nat-operacao
                                           tt-item-doc-est.seq-comp   = nfe014.seq-saldo-terc
                                           tt-item-doc-est.emite-comp = nfe014.cod-emitente.
        
                                IF  natur-oper.log-oper-triang THEN
                                    ASSIGN tt-item-doc-est.serie-terc     = nfe014.serie-docto
                                           tt-item-doc-est.nro-docto-terc = nfe014.nro-docto
                                           tt-item-doc-est.nat-terc       = nfe014.nat-operacao
                                           tt-item-doc-est.seq-terc       = nfe014.seq-saldo-terc
                                           tt-item-doc-est.cod-emit-terc  = nfe014.cod-emitente.                    
                            END.
                        END.
                    END.
                    ELSE DO:
                        IF AVAIL natur-oper 
                        AND (natur-oper.tp-oper-terc = 4          /* Faturamento Consignacao */
                         OR  natur-oper.tp-oper-terc = 5) THEN DO: /* Devolucao Consignacao   */ 
                            
                            ASSIGN de-qtd-saldo = 0.
                            FOR EACH saldo-terc FIELDS (quantidade dec-1)
                                WHERE saldo-terc.cod-emitente = nfe014.cod-emitente
                                  AND saldo-terc.it-codigo    = nfe014.it-codigo   
                                  AND saldo-terc.cod-refer    = nfe013.cod-refer NO-LOCK:   
                                ASSIGN de-qtd-saldo = de-qtd-saldo + (saldo-terc.quantidade - saldo-terc.dec-1).
                            END.
                            IF  de-qtd-saldo < nfe013.qtd-comercial THEN DO:
                                CREATE tt-erro.
                                RUN utp/ut-msgs.p (INPUT "HELP":U,
                                                   INPUT 5934,
                                                   INPUT nfe013.seq-item).
                                ASSIGN tt-erro.cd-erro   = 5934
                                       tt-erro.desc-erro = RETURN-VALUE
                                       l-ok              = NO.
    
                                NEXT.
                            END.
                        END.

                        define variable de-qt-alocada as decimal     no-undo.
                        define variable de-qt-alocar  as decimal     no-undo.
                        define variable de-saldo      as decimal     no-undo.
                        DEFINE VARIABLE de-vl-merc-liq AS DECIMAL     NO-UNDO.
                        DEFINE VARIABLE de-vl-desc    AS DECIMAL     NO-UNDO.
                        DEFINE VARIABLE h-boin404re AS HANDLE      NO-UNDO.
                        

                        /*Tratamento para beneficiamento com baixa do saldo por fifo*/
                        if avail natur-oper and natur-oper.tp-oper-terc = 2 then do:
                            

                            assign de-qt-alocada = 0.
                            for each saldo-terc 
                                where saldo-terc.cod-estabel  = tt-docum-est.cod-estabel 
                                  and saldo-terc.it-codigo    = nfe013.it-codigo  
                                  and saldo-terc.cod-emitente = tt-docum-est.cod-emitente no-lock
                                   by saldo-terc.dt-retorno:

                                find first tt-saldo
                                     where tt-saldo.serie-docto    = saldo-terc.serie-docto 
                                       and tt-saldo.nro-docto      = saldo-terc.nro-docto
                                       and tt-saldo.cod-emitente   = saldo-terc.cod-emitente
                                       and tt-saldo.nat-operacao   = saldo-terc.nat-operacao
                                       and tt-saldo.sequencia      = saldo-terc.sequencia
                                       and tt-saldo.it-codigo      = saldo-terc.it-codigo no-error.
                                if avail tt-saldo then do:
                                    if tt-saldo.qtd-alocada >= saldo-terc.quantidade - saldo-terc.dec-1 then
                                        next.
                                end.

                                IF saldo-terc.quantidade - saldo-terc.dec-1 <= 0 THEN
                                    next.

                                if  not valid-handle(h-boin404re) then 
                                    run inbo/boin404re.p persistent set h-boin404re.
                    
                                run findNaturOper in h-boin404re ( saldo-terc.nat-operacao ).
                        
                                run findSaldoTerc in h-boin404re ( saldo-terc.cod-emitente,
                                                                   saldo-terc.serie-docto,
                                                                   saldo-terc.nro-docto,
                                                                   saldo-terc.nat-operacao,
                                                                   saldo-terc.sequencia,
                                                                   saldo-terc.it-codigo,
                                                                   saldo-terc.cod-refer ).
                                if  return-value <> "OK":U then DO:
                                    if valid-handle(h-boin404re) then do:
                                       run destroy in h-boin404re.
                                       assign h-boin404re = ?.                
                                    end.
                                    return "NOK":U.
                                END.
                                
                                run getValuesTerceiros in h-boin404re ( input tt-docum-est.dt-trans,
                                                                        input 0,
                                                                        input nfe013.qtd-interna,
                                                                        output de-vl-merc-liq,
                                                                        output de-vl-desc ).

                                if valid-handle(h-boin404re) then do:
                                    run destroy in h-boin404re.
                                    assign h-boin404re = ?.                
                                end.

                                IF abs(TRUNC((de-vl-merc-liq - de-vl-desc) / nfe013.qtd-interna,4) - nfe013.preco-unit) > 0.001 THEN DO:
                                    NEXT.
                                END.

                                assign de-saldo = saldo-terc.quantidade - saldo-terc.dec-1.
                                
                                find first tt-saldo
                                     where tt-saldo.serie-docto    = saldo-terc.serie-docto 
                                       and tt-saldo.nro-docto      = saldo-terc.nro-docto
                                       and tt-saldo.cod-emitente   = saldo-terc.cod-emitente
                                       and tt-saldo.nat-operacao   = saldo-terc.nat-operacao
                                       and tt-saldo.sequencia      = saldo-terc.sequencia
                                       and tt-saldo.it-codigo      = saldo-terc.it-codigo no-error.
                                if not avail tt-saldo then do:
                                    create tt-saldo.
                                    assign tt-saldo.cod-emitente = saldo-terc.cod-emitente
                                           tt-saldo.it-codigo    = saldo-terc.it-codigo
                                           tt-saldo.nat-operacao = saldo-terc.nat-operacao
                                           tt-saldo.nro-docto    = saldo-terc.nro-docto
                                           tt-saldo.sequencia    = saldo-terc.sequencia
                                           tt-saldo.serie-docto  = saldo-terc.serie-docto.
                                end.
                        
                                assign de-saldo = de-saldo - tt-saldo.qtd-alocada.
                        
                                if de-saldo > nfe013.qtd-interna - de-qt-alocada then
                                    assign de-qt-alocar = nfe013.qtd-interna - de-qt-alocada.
                                else
                                    assign de-qt-alocar = de-saldo.
                        
                                assign tt-saldo.qtd-alocada = tt-saldo.qtd-alocada + de-qt-alocar.
                        
                                create tt-item-terc.
                                assign tt-item-terc.rw-saldo-terc = rowid(saldo-terc)
                                       tt-item-terc.quantidade    = de-qt-alocar
                                       tt-item-terc.preco-total   = nfe013.preco-unit * de-qt-alocar
                                       tt-item-terc.desconto      = 0
                                       tt-item-terc.nr-ord-prod   = nfe013.nr-ord-prod.

                                assign de-qt-alocada = de-qt-alocada + de-qt-alocar.

                                IF de-qt-alocada = nfe013.qtd-interna THEN
                                    LEAVE.
                            end.
                            
                            if de-qt-alocada <> nfe013.qtd-interna then do:
                                RUN utp/ut-msgs.p (INPUT "HELP":U,
                                                   INPUT 17006,
                                                   INPUT "Erro ao Alocar saldo Terceiro para sequˆncia " + string(nfe013.seq-item) 
                                                        + "~~Quantidade Alocada difere da quantidade recebida, ou saldo insuficiente. Sequˆncia " + string(nfe013.seq-item) + string(de-qt-alocada) + " <> " + string(nfe013.qtd-interna)).
                                create tt-erro.
                                ASSIGN tt-erro.cd-erro   = 17006
                                       tt-erro.desc-erro = return-value
                                       l-ok              = no.
                                next.
                            end.

                            next.
                        end.
                         
                        FOR FIRST natur-oper 
                            WHERE natur-oper.nat-operacao = tt-natureza.nat-operacao NO-LOCK:
                        END.

                        
                        IF NOT natur-oper.transf THEN DO: /* Nota Transferencia nÆo cria item do XML importado, busca da origem os itens */
                            CREATE tt-item-doc-est.
                            ASSIGN tt-item-doc-est.serie-docto    = bf{&TableName}.serie-docto     
                                   tt-item-doc-est.nro-docto      = bf{&TableName}.nro-docto       
                                   tt-item-doc-est.cod-emitente   = bf{&TableName}.cod-emitente    
                                   tt-item-doc-est.nat-operacao   = tt-natureza.nat-operacao       
                                   tt-item-doc-est.sequencia      = i-seq-item                     
                                   i-seq-item                     = i-seq-item + param-re.inc-seq
                                   tt-item-doc-est.it-codigo      = IF AVAIL item-fornec THEN item-fornec.it-codigo ELSE nfe013.it-codigo
                                   tt-item-doc-est.num-pedido     = nfe013.num-pedido  
                                   tt-item-doc-est.numero-ordem   = nfe013.numero-ordem
                                   tt-item-doc-est.parcela        = nfe013.parcela     
                                   tt-item-doc-est.nr-ord-produ   = nfe013.nr-ord-produ
                                   tt-item-doc-est.qt-do-forn     = nfe013.qtd-comercial
                                   tt-item-doc-est.un             = nfe013.un-comercial
                                   tt-item-doc-est.quantidade     = nfe013.qtd-interna
                                   tt-item-doc-est.preco-unit[1]  = nfe013.preco-unit   
                                   tt-item-doc-est.preco-total[1] = nfe013.preco-total
                                   tt-item-doc-est.class-fiscal   = REPLACE(nfe013.class-fiscal,".", "")
                                   tt-item-doc-est.log-1          = /*YES*/ nfe013.log-fifo-oc
                                   tt-item-doc-est.log-fifo-oc    = /*YES*/ nfe013.log-fifo-oc
                                   tt-item-doc-est.ct-codigo      = REPLACE(nfe013.conta-contabil,".", "")
                                   tt-item-doc-est.sc-codigo      = REPLACE(nfe013.centro-custo,".", "")
                                   tt-item-doc-est.cod-refer      = nfe013.cod-refer     
                                   tt-item-doc-est.cod-depos      = nfe013.cod-depos     
                                   tt-item-doc-est.cod-localiz    = nfe013.cod-localiz   
                                   tt-item-doc-est.lote           = nfe013.lote          
                                   tt-item-doc-est.dt-vali-lote   = nfe013.dt-vali-lote
        
                                   /* Devolu‡Æo */
                                   tt-item-doc-est.nro-comp       = nfe013.nro-comp   when bf{&TableName}.lg-devol
                                   tt-item-doc-est.serie-comp     = nfe013.serie-comp when bf{&TableName}.lg-devol
                                   tt-item-doc-est.seq-comp       = nfe013.seq-comp   when bf{&TableName}.lg-devol
                                   tt-item-doc-est.reabre-pd      = nfe013.reabre-pd  when bf{&TableName}.lg-devol
            
                                   &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
                                        tt-item-doc-est.cod-unid-negoc = nfe013.cod-unid-negoc
                                        tt-item-doc-est.int-1          = nfe013.int-1
                                   &ENDIF.
                        END.
                    END.
                END.

                /***** ITENS DO DOCUMENTO *****/
                IF l-ok then do:                            
    
                    IF NOT VALID-HANDLE(h-boin090) THEN
                        RUN inbo/boin090.p PERSISTENT SET h-boin090.
                    RUN openQueryStatic IN h-boin090("main"). 
                    RUN setRecord IN h-boin090(INPUT TABLE tt-docum-est).
                    RUN emptyRowErrors IN h-boin090.
            
                    IF NOT VALID-HANDLE(h-boin176) THEN
                        RUN inbo/boin176.p PERSISTENT SET h-boin176.
                    RUN setConstraintOfDocumEst IN h-boin176(INPUT tt-docum-est.cod-emitente,
                                                             INPUT tt-docum-est.serie-docto,
                                                             INPUT tt-docum-est.nro-docto,
                                                             INPUT tt-docum-est.nat-operacao).
    
                    RUN createRecord IN h-boin090.
    
                    if  return-value = "NOK" then do:
                        RUN getRowErrors IN h-boin090 (OUTPUT TABLE RowErrors ).
                        find first rowErrors 
                             where RowErrors.ErrorSubType = "ERROR":U
                               and RowErrors.ErrorType    <> "INTERNAL":U 
                               and rowErrors.errorNumber  <> 6218 no-error.
                        if avail RowErrors then do:
                            CREATE tt-erro.
                            ASSIGN tt-erro.cd-erro   = RowErrors.ErrorNumber
                                   tt-erro.desc-erro = RowErrors.ErrorDescription.
                            /*undo bloco-a, leave bloco-a.*/
                        end.
                    end. 
    
                    run getRecord in h-boin090 (output table tt-docum-est).
                    find first tt-docum-est no-error.

                    RUN setConstraintOfDocumEst IN h-boin176 (INPUT tt-docum-est.cod-emitente,
                                                              INPUT tt-docum-est.serie-docto,
                                                              INPUT tt-docum-est.nro-docto,
                                                              INPUT tt-docum-est.nat-operacao).

                    if can-find(first tt-item-terc) then do:
                        run createItemOfComponente IN h-boin176 ( INPUT h-boin090,
                                                                  INPUT TABLE tt-item-terc ).


                    end.
                    else do:
        
                        run openQueryStatic in h-boin176 ('main':u).
            
                        run emptyRowErrors in h-boin176.
                        
                        run setDefaultCalculoImpostos in h-boin176
                            (input true,   /* codigo de tributacao */
                             input true).  /* aliquota */
                        
                        run linktoDocumEst in h-boin176(h-boin090).
                        
                        run emptyRowObject in h-boin176.                         
            
                        CREATE tt2-item-doc-est.
                        FOR EACH tt-item-doc-est:
                            BUFFER-COPY tt-item-doc-est TO tt2-item-doc-est.
        
            
                        run setRecord in h-boin176 (table tt2-item-doc-est).
                        
                        run getRowErrors IN h-boin176 (OUTPUT TABLE RowErrors ).
        
                        find first rowErrors 
                             where RowErrors.ErrorSubType = "ERROR":U
                               and RowErrors.ErrorType    <> "INTERNAL":U no-error.
                        if avail RowErrors then do:
                            CREATE tt-erro.
                            ASSIGN tt-erro.cd-erro   = RowErrors.ErrorNumber
                                   tt-erro.desc-erro = RowErrors.ErrorDescription.
                            /*undo bloco-a, leave bloco-a.*/
                        end.
                        
                        run validateRecord in h-boin176 (input 'Create':u).
                        run getRowErrors IN h-boin176 (OUTPUT TABLE RowErrors ).
        
                        find first rowErrors 
                             where RowErrors.ErrorSubType = "ERROR":U
                               and RowErrors.ErrorType    <> "INTERNAL":U no-error.
                        if avail RowErrors then do:
                            CREATE tt-erro.
                            ASSIGN tt-erro.cd-erro   = RowErrors.ErrorNumber
                                   tt-erro.desc-erro = RowErrors.ErrorDescription.
                            /*undo bloco-a, leave bloco-a.*/
                        end.
                        
                        run createRecord in h-boin176.
                    end.
                    
                    RUN getRowErrors IN h-boin176 (OUTPUT TABLE RowErrors ).
    
                    find first rowErrors 
                         where RowErrors.ErrorSubType = "ERROR":U
                           and RowErrors.ErrorType    <> "INTERNAL":U no-error.
                    if avail RowErrors then do:
                        CREATE tt-erro.
                        ASSIGN tt-erro.cd-erro   = RowErrors.ErrorNumber
                               tt-erro.desc-erro = RowErrors.ErrorDescription.
                        /*undo bloco-a, leave bloco-a.*/
                    end.
                    END.
        
                    run emptyRowErrors in h-boin090.
                    run setHandleDocumEst in h-boin176 (input h-boin090).
        
                    run getTotalizaNota in h-boin176 (input false).
        
                    RUN getRowErrors IN h-boin176 (OUTPUT TABLE RowErrors ).
    
                    find first rowErrors 
                         where RowErrors.ErrorSubType = "ERROR":U
                           and RowErrors.ErrorType    <> "INTERNAL":U no-error.
                    if avail RowErrors then do:
                        CREATE tt-erro.
                        ASSIGN tt-erro.cd-erro   = RowErrors.ErrorNumber
                               tt-erro.desc-erro = RowErrors.ErrorDescription.                        
                    end.
        
                    run transferTotalItensNota in h-boin176
                        (input tt-docum-est.cod-emitente,
                         input tt-docum-est.serie-docto,
                         input tt-docum-est.nro-docto,
                         input tt-docum-est.nat-operacao).
        
                    RUN getRowErrors IN h-boin176 (OUTPUT TABLE RowErrors ).
    
                    find first rowErrors 
                         where RowErrors.ErrorSubType = "ERROR":U
                           and RowErrors.ErrorType    <> "INTERNAL":U no-error.
                    if avail RowErrors then do:
                        CREATE tt-erro.
                        ASSIGN tt-erro.cd-erro   = RowErrors.ErrorNumber
                               tt-erro.desc-erro = RowErrors.ErrorDescription.
                    end.
        
                    run validateValues in h-boin090.
                    if return-value <> 'ok':u then do:
                        RUN getRowErrors IN h-boin090 (OUTPUT TABLE RowErrors ).
                        find first rowErrors 
                             where RowErrors.ErrorSubType = "ERROR":U
                               and RowErrors.ErrorType    <> "INTERNAL":U no-error.
                        if avail RowErrors then do:
                            CREATE tt-erro.
                            ASSIGN tt-erro.cd-erro   = RowErrors.ErrorNumber
                                   tt-erro.desc-erro = RowErrors.ErrorDescription.
                        end.                        
                    end.
        
                    run getRowid in h-boin090 (output tt-docum-est.r-rowid).
        
                    if can-find(natur-oper of tt-docum-est
                                where natur-oper.emite-duplic = true) then do:
                        /* gera duplicatas */
                        run rep/re9341.p (tt-docum-est.r-rowid, input no).
        
                        if return-value <> 'ok':u then do:
                            CREATE RowErrors.
                            ASSIGN RowErrors.errorSequence    = 99
                                   RowErrors.ErrorNumber      = 17006
                                   RowErrors.ErrorType        = "EMS":U
                                   RowErrors.ErrorSubType     = "ERROR":U
                                   RowErrors.ErrorDescription = "Erro na cria‡Æo das duplicatas!".
                        end.
                        find first rowErrors 
                             where RowErrors.ErrorSubType = "ERROR":U
                               and RowErrors.ErrorType    <> "INTERNAL":U no-error.
                        if avail RowErrors then do:
                            CREATE tt-erro.
                            ASSIGN tt-erro.cd-erro   = RowErrors.ErrorNumber
                                   tt-erro.desc-erro = RowErrors.ErrorDescription.
                        end.
                    end.    
        
                END.
            end.

            for first docum-est no-lock where 
                rowid(docum-est) = tt-docum-est.r-rowid,
                 each item-doc-est exclusive
                   of docum-est:
    
                assign item-doc-est.despesas[1]  = item-doc-est.despesas[1] - item-doc-est.pr-total-cmi
                       item-doc-est.pr-total-cmi = 0.

                assign i-nr-ord-produ = item-doc-est.nr-ord-produ.

                if can-find (first tt-item-terc) and
                    item-doc-est.nr-ord-produ = 0 and item-doc-est.numero-ordem = 0 then do:
                    assign item-doc-est.conta-contabil = "".
                end.
            end.
    
            for first docum-est no-lock where 
                rowid(docum-est) = tt-docum-est.r-rowid,
                 each item-doc-est exclusive
                   of docum-est:
    
                run rep/re1001r.p (rowid(docum-est),
                                   if avail param-re and param-re.rateia-frete = 1 then item-doc-est.peso-liquido
                                                                                   else item-doc-est.preco-total[1],
                                   2,
                                   output de-frete).
    
                assign item-doc-est.pr-total-cmi = de-frete
                       item-doc-est.despesas[1]  = item-doc-est.despesas[1] + de-frete.
            end. 

            RUN geraAgregado .
            IF RETURN-VALUE = "NOK" THEN
                UNDO, LEAVE.
            
            IF CAN-FIND(FIRST tt-erro) OR NOT l-integra THEN
                UNDO,LEAVE.
        end.

        do transaction:
            
            FOR EACH nfe017 EXCLUSIVE-LOCK
                WHERE nfe017.ch-acesso-comp-nfe = bf{&TableName}.ch-acesso-comp-nfe
                  AND nfe017.idi-orig-trad      = 2:
                ASSIGN nfe017.log-ativo = NO.
            END.
        end.

        IF CAN-FIND(FIRST tt-erro) THEN do transaction:
            FOR EACH tt-erro:

                FIND FIRST nfe017
                    WHERE nfe017.ch-acesso-comp-nfe = bf{&TableName}.ch-acesso-comp-nfe
                      AND nfe017.cd-msg             = tt-erro.cd-erro
                      AND nfe017.texto-msg          = tt-erro.desc-erro 
                      AND nfe017.dt-msg             = TODAY
                      AND nfe017.hr-msg             = STRING(TIME, "HH:MM:SS") NO-LOCK NO-ERROR.

                IF NOT AVAIL nfe017 THEN DO:
                    CREATE nfe017.
                    ASSIGN nfe017.ch-acesso-comp-nfe = bf{&TableName}.ch-acesso-comp-nfe
                           nfe017.idi-orig-trad      = 2
                           nfe017.dt-msg             = TODAY
                           nfe017.hr-msg             = STRING(TIME, "HH:MM:SS")
                           nfe017.log-ativo          = YES
                           nfe017.cd-msg             = tt-erro.cd-erro
                           nfe017.texto-msg          = tt-erro.desc-erro
                           nfe017.seq-msg            = NEXT-VALUE(seq-msg-nfe).
                END.
            END.

            ASSIGN l-ok = NO.
        END.
    END.

    IF l-integra THEN DO:
        IF l-ok THEN do transaction:
            FOR FIRST bf{&TableName} FIELDS (idi-situacao)
                WHERE ROWID(bf{&TableName}) = pr-nota EXCLUSIVE-LOCK:
                ASSIGN bf{&TableName}.idi-situacao = 1 /* Digitada Recebimento */ .
            END.
    
            RELEASE bf{&TableName}.
        END.
        ELSE do transaction:
            FOR FIRST bf{&TableName} FIELDS (idi-situacao)
                WHERE ROWID(bf{&TableName}) = pr-nota EXCLUSIVE-LOCK:
                ASSIGN bf{&TableName}.idi-situacao = 2 /* Digitada Recebimento */ .
            END.
    
            RELEASE bf{&TableName}.
        END.
    END.
    ELSE do transaction:
        FOR FIRST bf{&TableName} FIELDS (idi-situacao)
            WHERE ROWID(bf{&TableName}) = pr-nota EXCLUSIVE-LOCK:
            ASSIGN bf{&TableName}.idi-situacao = 7 /* Conferido para integra‡Æo - Simula‡Æo */ .
        END.
    
        RELEASE bf{&TableName}.
    END.

    IF VALID-HANDLE(h-boin090) THEN
        DELETE PROCEDURE h-boin090.
    ASSIGN h-boin090 = ?.

    IF VALID-HANDLE(h-boin176) THEN
        DELETE PROCEDURE h-boin176.
    ASSIGN h-boin176 = ?.


    RETURN "OK":U.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reprocessaNotaMultiplasNaturezas DBOProgram 
PROCEDURE reprocessaNotaMultiplasNaturezas :
/*------------------------------------------------------------------------------
  Purpose:     Reprocessa NF-e 
  Parameters:  pr-nota -> ROWID nota fiscal
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER pr-nota AS ROWID    NO-UNDO.
    DEFINE INPUT  PARAMETER l-integra AS LOGICAL  NO-UNDO.
    DEFINE OUTPUT PARAMETER l-ok      AS LOGICAL  NO-UNDO.

    DEFINE VARIABLE i-seq-item       AS INTEGER     NO-UNDO.
    DEFINE VARIABLE de-indice        AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE de-qtd-saldo     AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE l-agregado       AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE c-conta-contabil AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE l-falta-nat AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE c-nat-inf AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE c-conta     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE c-centro-custo AS CHARACTER   NO-UNDO.
    
    EMPTY TEMP-TABLE tt-docum-est.
    EMPTY TEMP-TABLE tt-item-doc-est.
    EMPTY TEMP-TABLE tt-erro.
    EMPTY TEMP-TABLE tt2-item-doc-est.
    empty temp-table tt-saldo.
    empty temp-table tt-item-terc.

    FOR FIRST param-re FIELDS (inc-seq seq-item-um)
        WHERE param-re.usuario = c-seg-usuario NO-LOCK:
    END.

    IF  NOT AVAIL param-re THEN
        RETURN "NOK":U.

    ASSIGN l-falta-nat = NO 
           c-nat-inf = '' .
    FOR FIRST bf{&TableName} NO-LOCK
        WHERE ROWID(bf{&TableName}) = pr-nota:
        /** VERIFICA MAIS DE UMA NATUREZA DE OPERACAO **/
        FOR EACH nfe013 FIELDS (nat-operacao it-codigo conta-contabil centro-custo)
            WHERE nfe013.ch-acesso-comp-nfe = bf{&TableName}.ch-acesso-comp-nfe
              AND nfe013.idi-orig-trad      = 2 NO-LOCK:

            IF trim(nfe013.nat-operacao) = '' THEN DO:
                ASSIGN l-falta-nat = YES 
                       c-nat-inf = c-nat-inf + ' Item: ' + nfe013.it-codigo + chr(13) .

            END.
            ELSE
                ASSIGN c-conta = REPLACE(nfe013.conta-contabil,".", "")
                       c-centro-custo = REPLACE(nfe013.centro-custo,".", "").
        END.
    END.

    IF l-falta-nat = YES THEN do trans:
        IF c-nat-inf <> '' THEN
            ASSIGN c-nat-inf = c-nat-inf + ' estÆo sem nat opera‡Æo !!' .


        IF l-integra THEN
            RUN utp/ut-msgs.p (INPUT "SHOW":U,
                               INPUT 17006,
                               INPUT "Preencher Natureza dos Itens!" + "~~" + c-nat-inf).

        CREATE nfe017.
        ASSIGN nfe017.ch-acesso-comp-nfe = bf{&TableName}.ch-acesso-comp-nfe
               nfe017.idi-orig-trad      = 2
               nfe017.dt-msg             = TODAY
               nfe017.hr-msg             = STRING(TIME, "HH:MM:SS")
               nfe017.log-ativo          = YES
               nfe017.cd-msg             = 777
               nfe017.texto-msg          = 'Proc Fiscal nÆo pode ser executado com itens sem natureza opera‡Æo'
               nfe017.seq-msg            = NEXT-VALUE(seq-msg-nfe).
        
        IF NOT l-integra THEN DO:
            FOR FIRST bf{&TableName} FIELDS (idi-situacao)
                WHERE ROWID(bf{&TableName}) = pr-nota EXCLUSIVE-LOCK:
                ASSIGN bf{&TableName}.idi-situacao = 7 /* Conferido para integra‡Æo - Simula‡Æo */ .
            END.
        
            RELEASE bf{&TableName}.

        END.
        RETURN "NOK":U.

    END.

    ASSIGN l-ok = YES.
    FOR FIRST bf{&TableName} NO-LOCK
        WHERE ROWID(bf{&TableName}) = pr-nota:

        DO TRANSACTION ON ERROR UNDO, LEAVE:

            EMPTY TEMP-TABLE tt-docum-est.
            EMPTY TEMP-TABLE tt-item-doc-est.

            /*find nfe008 
                WHERE nfe008.cod-estabel  = bf{&TableName}.cod-estabel
                  and nfe008.cod-emitente = bf{&TableName}.cod-emitente NO-LOCK NO-ERROR.*/ 

            if bf{&TableName}.nat-oper-comp = "" then do:
                find first nfe013 no-lock
                    where nfe013.ch-acesso-comp-nfe = bf{&TableName}.ch-acesso-comp-nfe
                      AND nfe013.idi-orig-trad      = 2 no-error.
                if avail nfe013 then
                    assign c-natureza-operacao = nfe013.nat-operacao.
                
            end.
            
            /***** DOCUMENTO *****/
            CREATE tt-docum-est.
            ASSIGN tt-docum-est.cod-emitente         = bf{&TableName}.cod-emitente
                   tt-docum-est.nro-docto            = bf{&TableName}.nro-docto  
                   tt-docum-est.nat-operacao         = if bf{&TableName}.nat-oper-comp <> "" then bf{&TableName}.nat-oper-comp else c-natureza-operacao /*nfe008.nat-operacao when avail nfe008*/
                   tt-docum-est.serie-docto          = bf{&TableName}.serie-docto
                   tt-docum-est.cod-observa          = bf{&TableName}.cod-observa
                   tt-docum-est.cod-estabel          = bf{&TableName}.cod-estabel
                   tt-docum-est.dt-emissao           = bf{&TableName}.dt-emissao
                   tt-docum-est.dt-trans             = bf{&TableName}.dt-transacao
                   tt-docum-est.mod-frete            = int(bf{&TableName}.mod-frete) 
                   tt-docum-est.valor-frete          = bf{&TableName}.valor-frete
                   tt-docum-est.ct-transit           = c-conta
                   tt-docum-est.sc-transit           = c-centro-custo
                   tt-docum-est.esp-docto            = if bf{&TableName}.lg-devol then 20
                                                                                  else 21
            &IF "{&bf_mat_versao_ems}" < "2.07" &THEN
              OVERLAY(tt-docum-est.char-1,93,60) = bf{&TableName}.ch-acesso-comp-nfe /*nfe003.ch-acesso-comp-nfe*/
              OVERLAY(tt-docum-est.char-1,153,1) = "1".
            &ELSE
                  tt-docum-est.cod-chave-aces-nf-eletro = bf{&TableName}.ch-acesso-comp-nfe /*nfe003.ch-acesso-comp-nfe*/
                  tt-docum-est.cdn-sit-nfe              = 1.
            &ENDIF
    
            
            &IF "{&bf_dis_versao_ems}" >= "2.062" &THEN
                IF  bf{&TableName}.razao-social-transp <> ? THEN
                    ASSIGN tt-docum-est.nome-transp = bf{&TableName}.razao-social-transp.
        
                IF  bf{&TableName}.placa <> ? THEN
                    ASSIGN tt-docum-est.cod-placa[1] = bf{&TableName}.placa .
        
                IF bf{&TableName}.uf-placa <> ? THEN
                    ASSIGN tt-docum-est.cod-uf-placa[1] = bf{&TableName}.uf-placa.
            &ELSE
                IF bf{&TableName}.razao-social-transp <> ? THEN
                    ASSIGN OVERLAY(tt-docum-est.char-2,102,12) = bf{&TableName}.razao-social-transp.
        
                IF bf{&TableName}.placa <> ? THEN
                    ASSIGN OVERLAY(tt-docum-est.char-2,115,7) = bf{&TableName}.placa.
        
                IF bf{&TableName}.uf-placa <> ? THEN
                    ASSIGN OVERLAY(tt-docum-est.char-2,136,2) = bf{&TableName}.uf-placa.
            &ENDIF
            
            /***** DOCUMENTO *****/   
            /***** ITENS DO DOCUMENTO *****/
            ASSIGN i-seq-item = param-re.seq-item-um.
    
            FIND FIRST nfe019 NO-LOCK NO-ERROR.
            
            FOR EACH nfe013 NO-LOCK
                WHERE nfe013.ch-acesso-comp-nfe = bf{&TableName}.ch-acesso-comp-nfe
                  AND nfe013.idi-orig-trad      = 2:
                
                /*RUN verificaDiferencaCompras (OUTPUT l-ok).*/
                
                IF l-ok = NO THEN
                    NEXT.
                
                IF NOT AVAIL natur-oper
                OR (    AVAIL natur-oper
                    AND natur-oper.nat-operacao <> nfe013.nat-operacao) THEN
                    FOR FIRST natur-oper FIELDS (tp-oper-terc log-oper-triang nat-operacao)
                        WHERE natur-oper.nat-operacao = nfe013.nat-operacao NO-LOCK:
                    END.
                
                IF CAN-FIND(FIRST nfe014
                            WHERE nfe014.ch-acesso-comp-nfe = nfe013.ch-acesso-comp-nfe
                              AND nfe014.seq-item           = nfe013.seq-item          ) THEN DO:

                    FOR EACH nfe014 NO-LOCK
                        WHERE nfe014.ch-acesso-comp-nfe = nfe013.ch-acesso-comp-nfe
                          AND nfe014.seq-item           = nfe013.seq-item:

                        FOR FIRST saldo-terc FIELDS (quantidade dec-1)
                            WHERE saldo-terc.cod-emitente = nfe014.cod-emitente
                              AND saldo-terc.nro-docto    = nfe014.nro-docto   
                              AND saldo-terc.serie-docto  = nfe014.serie-docto 
                              AND saldo-terc.nat-operacao = nfe014.nat-operacao
                              AND saldo-terc.it-codigo    = nfe014.it-codigo   
                              AND saldo-terc.cod-refer    = nfe013.cod-refer   
                              AND saldo-terc.sequencia    = nfe014.seq-saldo-terc NO-LOCK:   
                        END.
                        IF  AVAIL saldo-terc 
                        AND saldo-terc.quantidade - saldo-terc.dec-1 < nfe014.qtd-alocada THEN DO:
                            CREATE tt-erro.
                            RUN utp/ut-msgs.p (INPUT "HELP":U,
                                               INPUT 5934,
                                               INPUT nfe013.seq-item).
                            ASSIGN tt-erro.cd-erro   = 5934
                                   tt-erro.desc-erro = RETURN-VALUE
                                   l-ok              = NO.
    
                            NEXT.
                        END.
    
                        ASSIGN de-indice = nfe014.qtd-alocada / nfe013.qtd-comercial.

                        CREATE tt-item-doc-est.
                        ASSIGN tt-item-doc-est.serie-docto    = bf{&TableName}.serie-docto     
                               tt-item-doc-est.nro-docto      = bf{&TableName}.nro-docto       
                               tt-item-doc-est.cod-emitente   = bf{&TableName}.cod-emitente    
                               tt-item-doc-est.nat-operacao   = tt-docum-est.nat-operacao
                               tt-item-doc-est.sequencia      = i-seq-item                     
                               i-seq-item                     = i-seq-item + param-re.inc-seq  
                
                               tt-item-doc-est.it-codigo      = IF AVAIL item-fornec THEN item-fornec.it-codigo ELSE nfe013.it-codigo
                               tt-item-doc-est.num-pedido     = nfe013.num-pedido  
                               tt-item-doc-est.numero-ordem   = nfe013.numero-ordem
                               tt-item-doc-est.parcela        = nfe013.parcela     
                               tt-item-doc-est.nr-ord-produ   = nfe013.nr-ord-produ
                               tt-item-doc-est.qt-do-forn     = nfe014.qtd-alocada
                               tt-item-doc-est.un             = nfe013.un-comercial
                               tt-item-doc-est.quantidade     = nfe013.qtd-interna * de-indice
                               tt-item-doc-est.preco-unit[1]  = nfe013.preco-unit
                               tt-item-doc-est.preco-total[1] = nfe013.preco-total * de-indice
                               tt-item-doc-est.class-fiscal   = REPLACE(nfe013.class-fiscal,".", "")
                               tt-item-doc-est.log-1          = /*YES*/ nfe013.log-fifo-oc
                               tt-item-doc-est.log-fifo-oc    = /*YES*/ nfe013.log-fifo-oc
                               tt-item-doc-est.ct-codigo      = REPLACE(nfe013.conta-contabil,".", "")
                               tt-item-doc-est.sc-codigo      = REPLACE(nfe013.centro-custo,".", "")
                               tt-item-doc-est.cod-refer      = nfe013.cod-refer     
                               tt-item-doc-est.cod-depos      = nfe013.cod-depos     
                               tt-item-doc-est.cod-localiz    = nfe013.cod-localiz   
                               tt-item-doc-est.lote           = nfe013.lote          
                               tt-item-doc-est.dt-vali-lote   = nfe013.dt-vali-lote
                               tt-item-doc-est.pr-total-cmi   = nfe013.vl-frete
                               tt-item-doc-est.despesas       = nfe013.vl-frete
                               tt-item-doc-est.cd-trib-icm    = 0
                               tt-item-doc-est.cd-trib-ipi    = 0
                               /* Devolu‡Æo */
                               tt-item-doc-est.nro-comp       = nfe013.nro-comp   when bf{&TableName}.lg-devol
                               tt-item-doc-est.serie-comp     = nfe013.serie-comp when bf{&TableName}.lg-devol
                               tt-item-doc-est.seq-comp       = nfe013.seq-comp   when bf{&TableName}.lg-devol
                               tt-item-doc-est.reabre-pd      = nfe013.reabre-pd  when bf{&TableName}.lg-devol
                               tt-item-doc-est.nat-of         = nfe013.nat-operacao
            
                               &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
                                    tt-item-doc-est.cod-unid-negoc = nfe013.cod-unid-negoc
                                    tt-item-doc-est.int-1          = nfe013.int-1
                               &ENDIF.
                               
                        IF AVAIL natur-oper THEN DO:
                            IF  natur-oper.tp-oper-terc = 2       /* Retorno Beneficiamento  */ 
                             OR natur-oper.tp-oper-terc = 4       /* Faturamento Consignacao */
                             OR natur-oper.tp-oper-terc = 5 THEN /* Devolucao Consignacao   */ 
                                ASSIGN tt-item-doc-est.serie-comp = nfe014.serie-docto
                                       tt-item-doc-est.nro-comp   = nfe014.nro-docto
                                       tt-item-doc-est.nat-comp   = nfe014.nat-operacao
                                       tt-item-doc-est.seq-comp   = nfe014.seq-saldo-terc
                                       tt-item-doc-est.emite-comp = nfe014.cod-emitente.
        
                            IF  natur-oper.log-oper-triang THEN
                                ASSIGN tt-item-doc-est.serie-terc     = nfe014.serie-docto
                                       tt-item-doc-est.nro-docto-terc = nfe014.nro-docto
                                       tt-item-doc-est.nat-terc       = nfe014.nat-operacao
                                       tt-item-doc-est.seq-terc       = nfe014.seq-saldo-terc
                                       tt-item-doc-est.cod-emit-terc  = nfe014.cod-emitente.                    
                        END.
                    END.
                END.
                ELSE DO:
                    IF AVAIL natur-oper 
                    AND (natur-oper.tp-oper-terc = 4          /* Faturamento Consignacao */
                     OR  natur-oper.tp-oper-terc = 5) THEN DO: /* Devolucao Consignacao   */ 
                        
                        ASSIGN de-qtd-saldo = 0.
                        FOR EACH saldo-terc FIELDS (quantidade dec-1)
                            WHERE saldo-terc.cod-emitente = nfe014.cod-emitente
                              AND saldo-terc.it-codigo    = nfe014.it-codigo   
                              AND saldo-terc.cod-refer    = nfe013.cod-refer NO-LOCK:   
                            ASSIGN de-qtd-saldo = de-qtd-saldo + (saldo-terc.quantidade - saldo-terc.dec-1).
                        END.
                        IF  de-qtd-saldo < nfe013.qtd-comercial THEN DO:
                            CREATE tt-erro.
                            RUN utp/ut-msgs.p (INPUT "HELP":U,
                                               INPUT 5934,
                                               INPUT nfe013.seq-item).
                            ASSIGN tt-erro.cd-erro   = 5934
                                   tt-erro.desc-erro = RETURN-VALUE
                                   l-ok              = NO.
    
                            NEXT.
                        END.
                    END.

                    define variable de-qt-alocada as decimal     no-undo.
                    define variable de-qt-alocar  as decimal     no-undo.
                    define variable de-saldo      as decimal     no-undo.
                    DEFINE VARIABLE de-vl-merc-liq AS DECIMAL     NO-UNDO.
                    DEFINE VARIABLE de-vl-desc    AS DECIMAL     NO-UNDO.
                    DEFINE VARIABLE h-boin404re AS HANDLE      NO-UNDO.
                    

                    /*Tratamento para beneficiamento com baixa do saldo por fifo*/
                    if avail natur-oper and natur-oper.tp-oper-terc = 2 then do:
                        

                        assign de-qt-alocada = 0.
                        for each saldo-terc 
                            where saldo-terc.cod-estabel  = tt-docum-est.cod-estabel 
                              and saldo-terc.it-codigo    = nfe013.it-codigo  
                              and saldo-terc.cod-emitente = tt-docum-est.cod-emitente no-lock
                               by saldo-terc.dt-retorno:

                            find first tt-saldo
                                 where tt-saldo.serie-docto    = saldo-terc.serie-docto 
                                   and tt-saldo.nro-docto      = saldo-terc.nro-docto
                                   and tt-saldo.cod-emitente   = saldo-terc.cod-emitente
                                   and tt-saldo.nat-operacao   = saldo-terc.nat-operacao
                                   and tt-saldo.sequencia      = saldo-terc.sequencia
                                   and tt-saldo.it-codigo      = saldo-terc.it-codigo no-error.
                            if avail tt-saldo then do:
                                if tt-saldo.qtd-alocada >= saldo-terc.quantidade - saldo-terc.dec-1 then
                                    next.
                            end.

                            IF saldo-terc.quantidade - saldo-terc.dec-1 <= 0 THEN
                                next.

                            if  not valid-handle(h-boin404re) then 
                                run inbo/boin404re.p persistent set h-boin404re.
                
                            run findNaturOper in h-boin404re ( saldo-terc.nat-operacao ).
                    
                            run findSaldoTerc in h-boin404re ( saldo-terc.cod-emitente,
                                                               saldo-terc.serie-docto,
                                                               saldo-terc.nro-docto,
                                                               saldo-terc.nat-operacao,
                                                               saldo-terc.sequencia,
                                                               saldo-terc.it-codigo,
                                                               saldo-terc.cod-refer ).
                            if  return-value <> "OK":U then DO:
                                if valid-handle(h-boin404re) then do:
                                   run destroy in h-boin404re.
                                   assign h-boin404re = ?.                
                                end.
                                return "NOK":U.
                            END.
                            
                            run getValuesTerceiros in h-boin404re ( input tt-docum-est.dt-trans,
                                                                    input 0,
                                                                    input nfe013.qtd-interna,
                                                                    output de-vl-merc-liq,
                                                                    output de-vl-desc ).

                            if valid-handle(h-boin404re) then do:
                                run destroy in h-boin404re.
                                assign h-boin404re = ?.                
                            end.

                            IF abs(TRUNC((de-vl-merc-liq - de-vl-desc) / nfe013.qtd-interna,4) - nfe013.preco-unit) > 0.001 THEN DO:
                                NEXT.
                            END.

                            assign de-saldo = saldo-terc.quantidade - saldo-terc.dec-1.
                    
                            find first tt-saldo
                                 where tt-saldo.serie-docto    = saldo-terc.serie-docto 
                                   and tt-saldo.nro-docto      = saldo-terc.nro-docto
                                   and tt-saldo.cod-emitente   = saldo-terc.cod-emitente
                                   and tt-saldo.nat-operacao   = saldo-terc.nat-operacao
                                   and tt-saldo.sequencia      = saldo-terc.sequencia
                                   and tt-saldo.it-codigo      = saldo-terc.it-codigo no-error.
                            if not avail tt-saldo then do:
                                create tt-saldo.
                                assign tt-saldo.cod-emitente = saldo-terc.cod-emitente
                                       tt-saldo.it-codigo    = saldo-terc.it-codigo
                                       tt-saldo.nat-operacao = saldo-terc.nat-operacao
                                       tt-saldo.nro-docto    = saldo-terc.nro-docto
                                       tt-saldo.sequencia    = saldo-terc.sequencia
                                       tt-saldo.serie-docto  = saldo-terc.serie-docto.
                            end.
                    
                            assign de-saldo = de-saldo - tt-saldo.qtd-alocada.
                    
                            if de-saldo > nfe013.qtd-interna - de-qt-alocada then
                                assign de-qt-alocar = nfe013.qtd-interna - de-qt-alocada.
                            else
                                assign de-qt-alocar = de-saldo.
                    
                            assign tt-saldo.qtd-alocada = tt-saldo.qtd-alocada + de-qt-alocar.
                    
                            create tt-item-terc.
                            assign tt-item-terc.rw-saldo-terc = rowid(saldo-terc)
                                   tt-item-terc.quantidade    = de-qt-alocar
                                   tt-item-terc.preco-total   = nfe013.preco-unit * de-qt-alocar
                                   tt-item-terc.desconto      = 0
                                   tt-item-terc.nr-ord-prod   = nfe013.nr-ord-prod.

                            assign de-qt-alocada = de-qt-alocada + de-qt-alocar.

                            IF de-qt-alocada = nfe013.qtd-interna THEN
                                LEAVE.
                        end.
                        
                        if de-qt-alocada <> nfe013.qtd-interna then do:
                            RUN utp/ut-msgs.p (INPUT "HELP":U,
                                               INPUT 17006,
                                               INPUT "Erro ao Alocar saldo Terceiro para sequˆncia " + string(nfe013.seq-item) 
                                                    + "~~Quantidade Alocada difere da quantidade recebida, ou saldo insuficiente. Sequˆncia " + string(nfe013.seq-item) ).
                            create tt-erro.
                            ASSIGN tt-erro.cd-erro   = 17006
                                   tt-erro.desc-erro = return-value
                                   l-ok              = no.
    
                            next.
                        end.

                        next.
                    end.
                     
                    FOR FIRST natur-oper 
                        WHERE natur-oper.nat-operacao = nfe013.nat-operacao NO-LOCK:
                    END.

                    
                    IF NOT natur-oper.transf THEN DO: /* Nota Transferencia nÆo cria item do XML importado, busca da origem os itens */
                        CREATE tt-item-doc-est.
                        ASSIGN tt-item-doc-est.serie-docto    = bf{&TableName}.serie-docto     
                               tt-item-doc-est.nro-docto      = bf{&TableName}.nro-docto       
                               tt-item-doc-est.cod-emitente   = bf{&TableName}.cod-emitente    
                               tt-item-doc-est.nat-operacao   = tt-docum-est.nat-operacao
                               tt-item-doc-est.sequencia      = i-seq-item                     
                               i-seq-item                     = i-seq-item + param-re.inc-seq
                               tt-item-doc-est.it-codigo      = IF AVAIL item-fornec THEN item-fornec.it-codigo ELSE nfe013.it-codigo
                               tt-item-doc-est.num-pedido     = nfe013.num-pedido  
                               tt-item-doc-est.numero-ordem   = nfe013.numero-ordem
                               tt-item-doc-est.parcela        = nfe013.parcela     
                               tt-item-doc-est.nr-ord-produ   = nfe013.nr-ord-produ
                               tt-item-doc-est.qt-do-forn     = nfe013.qtd-comercial
                               tt-item-doc-est.un             = nfe013.un-comercial
                               tt-item-doc-est.quantidade     = nfe013.qtd-interna
                               tt-item-doc-est.preco-unit[1]  = nfe013.preco-unit   
                               tt-item-doc-est.preco-total[1] = nfe013.preco-total
                               tt-item-doc-est.class-fiscal   = REPLACE(nfe013.class-fiscal,".", "")
                               tt-item-doc-est.log-1          = /*YES*/ nfe013.log-fifo-oc
                               tt-item-doc-est.log-fifo-oc    = /*YES*/ nfe013.log-fifo-oc
                               tt-item-doc-est.ct-codigo      = REPLACE(nfe013.conta-contabil,".", "")
                               tt-item-doc-est.sc-codigo      = REPLACE(nfe013.centro-custo,".", "")
                               tt-item-doc-est.cod-refer      = nfe013.cod-refer     
                               tt-item-doc-est.cod-depos      = nfe013.cod-depos     
                               tt-item-doc-est.cod-localiz    = nfe013.cod-localiz   
                               tt-item-doc-est.lote           = nfe013.lote          
                               tt-item-doc-est.dt-vali-lote   = nfe013.dt-vali-lote
                               tt-item-doc-est.nat-of         = nfe013.nat-operacao
        
                               /* Devolu‡Æo */
                               tt-item-doc-est.nro-comp       = nfe013.nro-comp   when bf{&TableName}.lg-devol
                               tt-item-doc-est.serie-comp     = nfe013.serie-comp when bf{&TableName}.lg-devol
                               tt-item-doc-est.seq-comp       = nfe013.seq-comp   when bf{&TableName}.lg-devol
                               tt-item-doc-est.reabre-pd      = nfe013.reabre-pd  when bf{&TableName}.lg-devol
            
                               &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
                                    tt-item-doc-est.cod-unid-negoc = nfe013.cod-unid-negoc
                                    tt-item-doc-est.int-1          = nfe013.int-1
                               &ENDIF.
                    END.
                END.
            END.

            /***** ITENS DO DOCUMENTO *****/
            IF l-ok then do:  
            bloco-a:
            do trans:
                IF NOT VALID-HANDLE(h-boin090) THEN
                    RUN inbo/boin090.p PERSISTENT SET h-boin090.
                RUN openQueryStatic IN h-boin090("main"). 

                /*assign tt-docum-est.nat-operacao = natur-oper.nat-operacao.*/
                RUN setRecord IN h-boin090(INPUT TABLE tt-docum-est).
                RUN emptyRowErrors IN h-boin090.
            
                IF NOT VALID-HANDLE(h-boin176) THEN
                    RUN inbo/boin176.p PERSISTENT SET h-boin176.
                RUN setConstraintOfDocumEst IN h-boin176(INPUT tt-docum-est.cod-emitente,
                                                         INPUT tt-docum-est.serie-docto,
                                                         INPUT tt-docum-est.nro-docto,
                                                         INPUT tt-docum-est.nat-operacao).
    
                RUN createRecord IN h-boin090.
    
                if  return-value = "NOK" then do:
                    RUN getRowErrors IN h-boin090 (OUTPUT TABLE RowErrors ).
                    find first rowErrors 
                         where RowErrors.ErrorSubType = "ERROR":U
                           and RowErrors.ErrorType    <> "INTERNAL":U 
                           and rowErrors.errorNumber  <> 6218 no-error.
                    if avail RowErrors then do:
                        CREATE tt-erro.
                        ASSIGN tt-erro.cd-erro   = RowErrors.ErrorNumber
                               tt-erro.desc-erro = RowErrors.ErrorDescription.
                        undo bloco-a, leave bloco-a.
                    end.
                end. 
    
                run getRecord in h-boin090 (output table tt-docum-est).
                find first tt-docum-est no-error.
                if not avail tt-docum-est then do:
                    CREATE tt-erro.
                    ASSIGN tt-erro.cd-erro   = 17006
                           tt-erro.desc-erro = "Documento nÆo pode ser processado.".
                    undo bloco-a, leave bloco-a.
                end.
                
                RUN setConstraintOfDocumEst IN h-boin176 (INPUT tt-docum-est.cod-emitente,
                                                          INPUT tt-docum-est.serie-docto,
                                                          INPUT tt-docum-est.nro-docto,
                                                          INPUT tt-docum-est.nat-operacao).

                if can-find(first tt-item-terc) then do:
                    run createItemOfComponente IN h-boin176 ( INPUT h-boin090,
                                                              INPUT TABLE tt-item-terc ).


                end.
                else do:
        
                    run openQueryStatic in h-boin176 ('main':u).
            
                    run emptyRowErrors in h-boin176.
                    
                    run setDefaultCalculoImpostos in h-boin176
                        (input true,   /* codigo de tributacao */
                         input true).  /* aliquota */
                    
                    run linktoDocumEst in h-boin176(h-boin090).
                    
                    run emptyRowObject in h-boin176.                         
            
                    CREATE tt2-item-doc-est.
                    FOR EACH tt-item-doc-est:
                        BUFFER-COPY tt-item-doc-est TO tt2-item-doc-est.
        
            
                    run setRecord in h-boin176 (table tt2-item-doc-est).
                    
                    run getRowErrors IN h-boin176 (OUTPUT TABLE RowErrors ).
        
                    find first rowErrors 
                         where RowErrors.ErrorSubType = "ERROR":U
                           and RowErrors.ErrorType    <> "INTERNAL":U no-error.
                    if avail RowErrors then do:
                        CREATE tt-erro.
                        ASSIGN tt-erro.cd-erro   = RowErrors.ErrorNumber
                               tt-erro.desc-erro = RowErrors.ErrorDescription.
                        undo bloco-a, leave bloco-a.
                    end.
                    
                    run validateRecord in h-boin176 (input 'Create':u).
                    run getRowErrors IN h-boin176 (OUTPUT TABLE RowErrors ).
        
                    find first rowErrors 
                         where RowErrors.ErrorSubType = "ERROR":U
                           and RowErrors.ErrorType    <> "INTERNAL":U no-error.
                    if avail RowErrors then do:
                        CREATE tt-erro.
                        ASSIGN tt-erro.cd-erro   = RowErrors.ErrorNumber
                               tt-erro.desc-erro = RowErrors.ErrorDescription.
                        undo bloco-a, leave bloco-a.
                    end.
                    
                    run createRecord in h-boin176.
                end.
                
                RUN getRowErrors IN h-boin176 (OUTPUT TABLE RowErrors ).
    
                find first rowErrors 
                     where RowErrors.ErrorSubType = "ERROR":U
                       and RowErrors.ErrorType    <> "INTERNAL":U no-error.
                if avail RowErrors then do:
                    CREATE tt-erro.
                    ASSIGN tt-erro.cd-erro   = RowErrors.ErrorNumber
                           tt-erro.desc-erro = RowErrors.ErrorDescription.
                    undo bloco-a, leave bloco-a.
                end.
                END.
        
                run emptyRowErrors in h-boin090.
                run setHandleDocumEst in h-boin176 (input h-boin090).
        
                run getTotalizaNota in h-boin176 (input false).
        
                RUN getRowErrors IN h-boin176 (OUTPUT TABLE RowErrors ).
    
                find first rowErrors 
                     where RowErrors.ErrorSubType = "ERROR":U
                       and RowErrors.ErrorType    <> "INTERNAL":U no-error.
                if avail RowErrors then do:
                    CREATE tt-erro.
                    ASSIGN tt-erro.cd-erro   = RowErrors.ErrorNumber
                           tt-erro.desc-erro = RowErrors.ErrorDescription.                        
                end.
        
                run transferTotalItensNota in h-boin176
                    (input tt-docum-est.cod-emitente,
                     input tt-docum-est.serie-docto,
                     input tt-docum-est.nro-docto,
                     input tt-docum-est.nat-operacao).
        
                RUN getRowErrors IN h-boin176 (OUTPUT TABLE RowErrors ).
    
                find first rowErrors 
                     where RowErrors.ErrorSubType = "ERROR":U
                       and RowErrors.ErrorType    <> "INTERNAL":U no-error.
                if avail RowErrors then do:
                    CREATE tt-erro.
                    ASSIGN tt-erro.cd-erro   = RowErrors.ErrorNumber
                           tt-erro.desc-erro = RowErrors.ErrorDescription.
                end.
        
                run validateValues in h-boin090.
                if return-value <> 'ok':u then do:
                    RUN getRowErrors IN h-boin090 (OUTPUT TABLE RowErrors ).
                    find first rowErrors 
                         where RowErrors.ErrorSubType = "ERROR":U
                           and RowErrors.ErrorType    <> "INTERNAL":U no-error.
                    if avail RowErrors then do:
                        CREATE tt-erro.
                        ASSIGN tt-erro.cd-erro   = RowErrors.ErrorNumber
                               tt-erro.desc-erro = RowErrors.ErrorDescription.
                    end.                        
                end.
        
                run getRowid in h-boin090 (output tt-docum-est.r-rowid).
        
                if can-find(natur-oper of tt-docum-est
                            where natur-oper.emite-duplic = true) then do:
                    /* gera duplicatas */
                    run rep/re9341.p (tt-docum-est.r-rowid, input no).
        
                    if return-value <> 'ok':u then do:
                        CREATE RowErrors.
                        ASSIGN RowErrors.errorSequence    = 99
                               RowErrors.ErrorNumber      = 17006
                               RowErrors.ErrorType        = "EMS":U
                               RowErrors.ErrorSubType     = "ERROR":U
                               RowErrors.ErrorDescription = "Erro na cria‡Æo das duplicatas!".
                    end.
                    find first rowErrors 
                         where RowErrors.ErrorSubType = "ERROR":U
                           and RowErrors.ErrorType    <> "INTERNAL":U no-error.
                    if avail RowErrors then do:
                        CREATE tt-erro.
                        ASSIGN tt-erro.cd-erro   = RowErrors.ErrorNumber
                               tt-erro.desc-erro = RowErrors.ErrorDescription.
                    end.
                end.    
            END.
            END.
            

            for first docum-est no-lock where 
                rowid(docum-est) = tt-docum-est.r-rowid,
                 each item-doc-est exclusive
                   of docum-est:
    
                assign item-doc-est.despesas[1]  = item-doc-est.despesas[1] - item-doc-est.pr-total-cmi
                       item-doc-est.pr-total-cmi = 0.

                assign i-nr-ord-produ = item-doc-est.nr-ord-produ.

                if can-find (first tt-item-terc) and
                    item-doc-est.nr-ord-produ = 0 and item-doc-est.numero-ordem = 0 then do:
                    assign item-doc-est.conta-contabil = "".
                end.
            end.
    
            for first docum-est no-lock where 
                rowid(docum-est) = tt-docum-est.r-rowid,
                 each item-doc-est exclusive
                   of docum-est:
    
                run rep/re1001r.p (rowid(docum-est),
                                   if avail param-re and param-re.rateia-frete = 1 then item-doc-est.peso-liquido
                                                                                   else item-doc-est.preco-total[1],
                                   2,
                                   output de-frete).
    
                assign item-doc-est.pr-total-cmi = de-frete
                       item-doc-est.despesas[1]  = item-doc-est.despesas[1] + de-frete.
            end. 

            IF NOT CAN-FIND(FIRST tt-erro) THEN 
                RUN geraAgregado.

            IF RETURN-VALUE = "NOK" THEN
                UNDO, LEAVE.
            
            IF CAN-FIND(FIRST tt-erro) OR NOT l-integra THEN
                UNDO,LEAVE.
        end.

        do transaction:
            
            FOR EACH nfe017 EXCLUSIVE-LOCK
                WHERE nfe017.ch-acesso-comp-nfe = bf{&TableName}.ch-acesso-comp-nfe
                  AND nfe017.idi-orig-trad      = 2:
                ASSIGN nfe017.log-ativo = NO.
            END.
        end.

        IF CAN-FIND(FIRST tt-erro) THEN do transaction:
            FOR EACH tt-erro:

                FIND FIRST nfe017
                    WHERE nfe017.ch-acesso-comp-nfe = bf{&TableName}.ch-acesso-comp-nfe
                      AND nfe017.cd-msg             = tt-erro.cd-erro
                      AND nfe017.texto-msg          = tt-erro.desc-erro 
                      AND nfe017.dt-msg             = TODAY
                      AND nfe017.hr-msg             = STRING(TIME, "HH:MM:SS") NO-LOCK NO-ERROR.

                IF NOT AVAIL nfe017 THEN DO:
                    CREATE nfe017.
                    ASSIGN nfe017.ch-acesso-comp-nfe = bf{&TableName}.ch-acesso-comp-nfe
                           nfe017.idi-orig-trad      = 2
                           nfe017.dt-msg             = TODAY
                           nfe017.hr-msg             = STRING(TIME, "HH:MM:SS")
                           nfe017.log-ativo          = YES
                           nfe017.cd-msg             = tt-erro.cd-erro
                           nfe017.texto-msg          = tt-erro.desc-erro
                           nfe017.seq-msg            = NEXT-VALUE(seq-msg-nfe).
                END.
            END.

            ASSIGN l-ok = NO.
        END.
    END.

    IF l-integra THEN DO:
        IF l-ok THEN do transaction:
            FOR FIRST bf{&TableName} FIELDS (idi-situacao)
                WHERE ROWID(bf{&TableName}) = pr-nota EXCLUSIVE-LOCK:
                ASSIGN bf{&TableName}.idi-situacao = 1 /* Digitada Recebimento */ .
            END.
    
            RELEASE bf{&TableName}.
        END.
        ELSE do transaction:
            FOR FIRST bf{&TableName} FIELDS (idi-situacao)
                WHERE ROWID(bf{&TableName}) = pr-nota EXCLUSIVE-LOCK:
                ASSIGN bf{&TableName}.idi-situacao = 2 /* Digitada Recebimento */ .
            END.
    
            RELEASE bf{&TableName}.
        END.
    END.
    ELSE do transaction:
        FOR FIRST bf{&TableName} FIELDS (idi-situacao)
            WHERE ROWID(bf{&TableName}) = pr-nota EXCLUSIVE-LOCK:
            ASSIGN bf{&TableName}.idi-situacao = 7 /* Conferido para integra‡Æo - Simula‡Æo */ .
        END.
    
        RELEASE bf{&TableName}.
    END.

    IF VALID-HANDLE(h-boin090) THEN
        DELETE PROCEDURE h-boin090.
    ASSIGN h-boin090 = ?.

    IF VALID-HANDLE(h-boin176) THEN
        DELETE PROCEDURE h-boin176.
    ASSIGN h-boin176 = ?.


    RETURN "OK":U.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE retornaDocumento DBOProgram 
PROCEDURE retornaDocumento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER pserie-docto  LIKE docum-est.serie-docto  NO-UNDO.
    DEFINE INPUT  PARAMETER pnro-docto    LIKE docum-est.nro-docto    NO-UNDO.
    DEFINE INPUT  PARAMETER pcod-emitente LIKE docum-est.cod-emitente NO-UNDO.
    DEFINE OUTPUT PARAMETER pr-docto        AS ROWID                  NO-UNDO.

    FOR FIRST docum-est /*FIELDS (log-1)*/
        WHERE docum-est.serie-docto  = pserie-docto 
          AND docum-est.nro-docto    = pnro-docto 
          AND docum-est.cod-emitente = pcod-emitente NO-LOCK:
    END.
    IF AVAIL docum-est THEN
        ASSIGN pr-docto = ROWID(docum-est).
    ELSE
        ASSIGN pr-docto = ?.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE retornaNomeAbrev DBOProgram 
PROCEDURE retornaNomeAbrev :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT  PARAMETER p-cod-emitente LIKE emitente.cod-emitente NO-UNDO.
    DEF OUTPUT PARAMETER p-nome-abrev   LIKE emitente.nome-abrev   NO-UNDO.

    FOR FIRST emitente FIELDS (nome-abrev)
        WHERE emitente.cod-emitente = p-cod-emitente NO-LOCK:
    END.
    IF AVAIL emitente THEN
        ASSIGN p-nome-abrev = emitente.nome-abrev.
    ELSE 
        ASSIGN p-nome-abrev = "".

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setConstraintMonitor DBOProgram 
PROCEDURE setConstraintMonitor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAM p-tg-digitada        AS LOGICAL                    NO-UNDO.
    DEFINE INPUT PARAM p-tg-erro-neg        AS LOGICAL                    NO-UNDO.
    DEFINE INPUT PARAM p-tg-atualizada      AS LOGICAL                    NO-UNDO.
    DEFINE INPUT PARAM p-tg-eliminada       AS LOGICAL                    NO-UNDO.
    DEFINE INPUT PARAM p-tg-danfe           AS LOGICAL                    NO-UNDO.
    DEFINE INPUT PARAM p-tg-liberado        AS LOGICAL                    NO-UNDO.
    DEFINE INPUT PARAM p-tg-conferido       AS LOGICAL                    NO-UNDO.
    DEFINE INPUT PARAM p-cod-emitente-ini LIKE emitente.cod-emitente      NO-UNDO.
    DEFINE INPUT PARAM p-cod-emitente-fim LIKE emitente.cod-emitente      NO-UNDO.
    DEFINE INPUT PARAM p-cod-estabel-ini  LIKE docum-est.cod-estabel      NO-UNDO.
    DEFINE INPUT PARAM p-cod-estabel-fim  LIKE docum-est.cod-estabel      NO-UNDO.
    DEFINE INPUT PARAM p-serie-ini        LIKE docum-est.serie-docto      NO-UNDO.
    DEFINE INPUT PARAM p-serie-fim        LIKE docum-est.serie-docto      NO-UNDO.
    DEFINE INPUT PARAM p-nro-docto-ini    LIKE docum-est.nro-docto        NO-UNDO.
    DEFINE INPUT PARAM p-nro-docto-fim    LIKE docum-est.nro-docto        NO-UNDO.
    DEFINE INPUT PARAM p-dt-emissao-ini   LIKE docum-est.dt-emissao       NO-UNDO.
    DEFINE INPUT PARAM p-dt-emissao-fim   LIKE docum-est.dt-emissao       NO-UNDO.
    DEFINE INPUT PARAM p-idi-orig-trad    LIKE nfe003.idi-orig-trad NO-UNDO.

    ASSIGN tg-digitada        = p-tg-digitada     
           tg-erro-neg        = p-tg-erro-neg     
           tg-atualizada      = p-tg-atualizada   
           tg-eliminada       = p-tg-eliminada
           tg-danfe           = p-tg-danfe
           tg-liberado        = p-tg-liberado
           tg-conferido       = p-tg-conferido
           c-cod-emitente-ini = p-cod-emitente-ini
           c-cod-emitente-fim = p-cod-emitente-fim
           c-cod-estabel-ini  = p-cod-estabel-ini 
           c-cod-estabel-fim  = p-cod-estabel-fim 
           c-serie-ini        = p-serie-ini       
           c-serie-fim        = p-serie-fim       
           c-nro-docto-ini    = p-nro-docto-ini   
           c-nro-docto-fim    = p-nro-docto-fim   
           dt-emissao-ini     = p-dt-emissao-ini  
           dt-emissao-fim     = p-dt-emissao-fim
           i-idi-orig-trad    = p-idi-orig-trad.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validateRecord DBOProgram 
PROCEDURE validateRecord :
/*:T------------------------------------------------------------------------------
  Purpose:     Valida?„es pertinentes ao DBO
  Parameters:  recebe o tipo de valida?Æo (Create, Delete, Update)
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE INPUT PARAMETER pType AS CHARACTER NO-UNDO.
    
    /*:T--- Utilize o par?metro pType para identificar quais as valida?„es a serem
          executadas ---*/
    /*:T--- Os valores poss­veis para o par?metro sÆo: Create, Delete e Update ---*/
    /*:T--- Devem ser tratados erros PROGRESS e erros do Produto, atrav?s do 
          include: method/svc/errors/inserr.i ---*/
    /*:T--- Inclua aqui as valida?„es ---*/

    IF pType = "UPDATE":U THEN do transaction:

        /** VALIDA?€O EMITENTE **/
        IF NOT CAN-FIND(FIRST emitente
                        WHERE emitente.cod-emitente = RowObject.cod-emitente) THEN DO:
            {method/svc/errors/inserr.i &ErrorNumber="2"
                                        &ErrorType="EMS"
                                        &ErrorSubType="ERROR"
                                        &ErrorParameters="'Emitente'"}
        END.
        ELSE DO:
            FOR FIRST emitente FIELDS (identif)
                WHERE emitente.cod-emitente = RowObject.cod-emitente NO-LOCK:
    
                IF emitente.identif = 1 THEN DO:
                    {method/svc/errors/inserr.i &ErrorNumber="7060"
                                                &ErrorType="EMS"
                                                &ErrorSubType="ERROR"}
                END.
            END.
        END.

        /** VALIDA?€O SERIE **/
        IF NOT CAN-FIND(FIRST serie
                        WHERE serie.serie = RowObject.serie-docto) THEN DO:
            {method/svc/errors/inserr.i &ErrorNumber="2"
                                        &ErrorType="EMS"
                                        &ErrorSubType="ERROR"
                                        &ErrorParameters="'Serie'"}
        END.

        /** VALIDA?€O NATUREZA COMPLEMENTAR **/
        /*
        IF  RowObject.nat-oper-comp <> ""
        AND NOT CAN-FIND(FIRST natur-oper
                        WHERE natur-oper.nat-operacao = RowObject.nat-oper-comp) THEN DO:
            {method/svc/errors/inserr.i &ErrorNumber="2"
                                        &ErrorType="EMS"
                                        &ErrorSubType="ERROR"
                                        &ErrorParameters="'Natureza Complementar'"}
        END.
        */

        /** VALIDA?€O ESTABELECIMENTO **/
        IF NOT CAN-FIND(FIRST estabelec
                        WHERE estabelec.cod-estabel = RowObject.cod-estabel) THEN DO:
            {method/svc/errors/inserr.i &ErrorNumber="2"
                                        &ErrorType="EMS"
                                        &ErrorSubType="ERROR"
                                        &ErrorParameters="'Estabelecimento'"}
        END.

        /** VALIDA?†ES DATAS **/
        IF  RowObject.dt-trans > TODAY THEN DO:
            {method/svc/errors/inserr.i &ErrorNumber="1788"
                                        &ErrorType="EMS":U
                                        &ErrorSubType="ERROR":U }
        END.

        IF RowObject.dt-trans < RowObject.dt-emissao THEN DO:
             {method/svc/errors/inserr.i &ErrorNumber="89"
                                         &ErrorType="EMS":U
                                         &ErrorSubType="ERROR":U
                                         &ErrorParameters="'Transa?Æo~~~~' + 'EmissÆo'"  }
        END.

        if  rowObject.lg-devol then do:
            for first natur-oper no-lock
                where natur-oper.nat-operacao = RowObject.nat-oper-com:
                if  natur-oper.especie-doc <> 'NFD' then do:
                    {method/svc/errors/inserr.i &ErrorNumber="17006"
                                                &ErrorType="EMS":U
                                                &ErrorSubType="ERROR":U
                                                &ErrorParameters="'Natureza deve ser de devolu‡Æo.'"  }
                end.                                        
            end.
        end.
        
        IF RowObject.nat-oper-com <> "" THEN
            FOR EACH nfe013 exclusive-LOCK
               WHERE nfe013.ch-acesso-comp-nfe = RowObject.ch-acesso-comp-nfe
                 AND nfe013.idi-orig-trad      = 2:
                 
                 assign nfe013.nat-operacao = RowObject.nat-oper-com .
            END.

        
    END.
    
    /*:T--- Verifica ocorr?ncia de erros ---*/
    IF CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorSubType = "ERROR":U) THEN
        RETURN "NOK":U.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE verificaDiferencaCompras DBOProgram 
PROCEDURE verificaDiferencaCompras :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAM l-semErro AS LOGICAL.

DEFINE VARIABLE d-variacao-qtde   AS INTEGER NO-UNDO.
DEFINE VARIABLE d-diferenca-qtd   AS INTEGER NO-UNDO.
DEFINE VARIABLE d-variacao-valor  AS DECIMAL NO-UNDO.
DEFINE VARIABLE d-diferenca-valor AS DECIMAL NO-UNDO.
DEFINE VARIABLE d-diferenca-dias  AS INTEGER NO-UNDO.
DEFINE VARIABLE c-email-padrao    AS CHARACTER NO-UNDO.

ASSIGN d-variacao-qtde   = 0
       d-diferenca-qtd   = 0
       d-variacao-valor  = 0
       d-diferenca-valor = 0
       d-diferenca-dias  = 0
       l-semErro         = YES.

FIND ordem-compra
    WHERE ordem-compra.num-pedido   = nfe013.num-pedido
      AND ordem-compra.numero-ordem = nfe013.numero-ordem NO-LOCK NO-ERROR.
IF AVAIL ordem-compra THEN DO:
    
    /* Validacao Quantidade da Ordem de Compra x Quantidade da Nota do XML */
    ASSIGN d-variacao-qtde = (ordem-compra.qt-solic * nfe019.lim-var-qtd-perc) / 100.
    
    IF ordem-compra.qt-solic > nfe013.qtd-interna THEN
        ASSIGN d-diferenca-qtd = ordem-compra.qt-solic - nfe013.qtd-interna.
    IF ordem-compra.qt-solic < nfe013.qtd-interna THEN
        ASSIGN d-diferenca-qtd = nfe013.qtd-interna - ordem-compra.qt-solic.

    IF d-diferenca-qtd > d-variacao-qtde THEN DO:
        CREATE tt-erro.
        ASSIGN l-semErro         = NO
               tt-erro.cd-erro   = 9996
               tt-erro.desc-erro = "Diferen?a Qtd Ordem Compra maior que a permitida!" + 
                                   " Item:" + nfe013.it-codigo + " Ordem:" + STRING(nfe013.numero-ordem).

        ASSIGN c-email-padrao = "Diverg?ncias de Quantidade entre a ordem de compra e XML." + CHR(13) +
                                "A nota fiscal nÆo foi digitada automaticamente pois possui divergencia de quantidade entre" + CHR(13) +
                                "a ordem de compra e a nota fiscal enviada via XML."    + CHR(13) + CHR(13) +
                                "Estabelecimento:" + ordem-compra.cod-estabel           + CHR(13) + CHR(13) +
                                "Fornecedor:"      + STRING(ordem-compra.cod-emitente)  + CHR(13) + CHR(13) +
                                "Pedido Compra:"   + STRING(ordem-compra.num-pedido)    + CHR(13) + CHR(13) + 
                                "Ordem Compra:"    + STRING(ordem-compra.numero-ordem)  + CHR(13) + CHR(13) + 
                                "Qtd Ordem:"       + STRING(ordem-compra.qt-solic)      + CHR(13) + CHR(13) + 
                                "Qtd XML:"         + STRING(nfe013.qtd-interna).
        
        RUN enviaEmail (INPUT ordem-compra.cod-comprado,
                        INPUT c-email-padrao).

    END.

    /* Validacao Valor da Ordem de Compra x Valor do Item da Nota do XML */
    ASSIGN d-variacao-valor = (ordem-compra.pre-unit-for * nfe019.lim-var-valor-perc) / 100.

    IF ordem-compra.pre-unit-for > nfe013.preco-unit THEN
        ASSIGN d-diferenca-valor = ordem-compra.pre-unit-for - nfe013.preco-unit.

    IF ordem-compra.pre-unit-for < nfe013.preco-unit THEN
        ASSIGN d-diferenca-valor = nfe013.preco-unit - ordem-compra.pre-unit-for.

    IF d-diferenca-valor > d-variacao-valor THEN DO:
        CREATE tt-erro.
        ASSIGN l-semErro         = NO
               tt-erro.cd-erro   = 9997
               tt-erro.desc-erro = "Diferen?a Valor Ordem Compra maior que a permitida!" + 
                                   " Item:" + nfe013.it-codigo + " Ordem:" + STRING(nfe013.numero-ordem).

        ASSIGN c-email-padrao = "Diverg?ncias de valor entre a ordem de compra e XML." + CHR(13) +
                                "A nota fiscal nÆo foi digitada automaticamente pois possui divergencia de valor entre" + CHR(13) +
                                "a ordem de compra e a nota fiscal enviada via XML."    + CHR(13) + CHR(13) +
                                "Estabelecimento:" + ordem-compra.cod-estabel           + CHR(13) + CHR(13) +
                                "Fornecedor:"      + STRING(ordem-compra.cod-emitente)  + CHR(13) + CHR(13) +
                                "Pedido Compra:"   + STRING(ordem-compra.num-pedido)    + CHR(13) + CHR(13) + 
                                "Ordem Compra:"    + STRING(ordem-compra.numero-ordem)  + CHR(13) + CHR(13) + 
                                "Pre?o Unitÿrio na Ordem:"       + STRING(ordem-compra.pre-unit-for)      + CHR(13) + CHR(13) + 
                                "Pre?o Unitÿrio no XML:"         + STRING(nfe013.preco-unit).
        
        RUN enviaEmail (INPUT ordem-compra.cod-comprado,
                        INPUT c-email-padrao).

    END.

    FIND prazo-compra
        WHERE prazo-compra.numero-ordem = ordem-compra.numero-ordem
          AND prazo-compra.parcela = nfe013.parcela NO-LOCK NO-ERROR.
    IF AVAIL prazo-compra THEN DO:
        /* Validacao Data Parcela da Ordem de Compra x Data Entrega Item da Nota do XML */
        
        IF prazo-compra.data-entrega > bf{&TableName}.dt-transacao THEN
            ASSIGN d-diferenca-dias = prazo-compra.data-entrega - bf{&TableName}.dt-transacao.
    
        IF prazo-compra.data-entrega < bf{&TableName}.dt-transacao THEN
            ASSIGN d-diferenca-dias = bf{&TableName}.dt-transacao - prazo-compra.data-entrega.
    
        IF d-diferenca-dias > nfe019.lim-dias-entrega THEN DO:
            CREATE tt-erro.
            ASSIGN l-semErro         = NO
                   tt-erro.cd-erro   = 9998
                   tt-erro.desc-erro = "Diferen?a Data Entrega Ordem maior que a permitida!" + 
                                       " Item:" + nfe013.it-codigo + " Ordem:" + STRING(nfe013.numero-ordem).

            ASSIGN c-email-padrao = "Diverg?ncias de data de entrega entre a ordem de compra e XML." + CHR(13) +
                                    "A nota fiscal nÆo foi digitada automaticamente pois possui divergencia de data de entrega entre" + CHR(13) +
                                    "a ordem de compra e a nota fiscal enviada via XML."    + CHR(13) + CHR(13) +
                                    "Estabelecimento:" + ordem-compra.cod-estabel           + CHR(13) + CHR(13) +
                                    "Fornecedor:"      + STRING(ordem-compra.cod-emitente)  + CHR(13) + CHR(13) +
                                    "Pedido Compra:"   + STRING(ordem-compra.num-pedido)    + CHR(13) + CHR(13) + 
                                    "Ordem Compra:"    + STRING(ordem-compra.numero-ordem)  + CHR(13) + CHR(13) + 
                                    "Data Entrega Parcela da Ordem:"       + STRING(prazo-compra.data-entrega)      + CHR(13) + CHR(13) + 
                                    "Data Transa?Æo do XML:"         + STRING(bf{&TableName}.dt-transacao).
        
            RUN enviaEmail (INPUT ordem-compra.cod-comprado,
                            INPUT c-email-padrao).

        END.
    END.

END.

IF CAN-FIND(FIRST tt-erro) THEN do transaction:
     FOR EACH tt-erro:
         CREATE nfe017.
         ASSIGN nfe017.ch-acesso-comp-nfe = bf{&TableName}.ch-acesso-comp-nfe
                nfe017.idi-orig-trad      = 2
                nfe017.dt-msg             = TODAY
                nfe017.hr-msg             = STRING(TIME, "HH:MM:SS")
                nfe017.log-ativo          = YES
                nfe017.cd-msg             = tt-erro.cd-erro
                nfe017.texto-msg          = tt-erro.desc-erro
                nfe017.seq-msg            = NEXT-VALUE(seq-msg-nfe).
     END.

     ASSIGN l-semErro = NO.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE verificaNaturezas DBOProgram 
PROCEDURE verificaNaturezas :
/*------------------------------------------------------------------------------
  Purpose:    Verificar se hÿ mais de uma natureza de opera?Æo 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pr-nota AS ROWID                                         NO-UNDO.
    DEFINE INPUT PARAMETER pch-acesso-comp-nfe LIKE nfe003.ch-acesso-comp-nfe NO-UNDO.
    
    EMPTY TEMP-TABLE tt-natureza.

    DEFINE VARIABLE i-cont AS INTEGER     NO-UNDO.

    /** VERIFICA MAIS DE UMA NATUREZA DE OPERACAO **/
    ASSIGN i-cont = 0.
    FOR EACH nfe013 FIELDS (nat-operacao)
        WHERE nfe013.ch-acesso-comp-nfe = pch-acesso-comp-nfe
          AND nfe013.idi-orig-trad      = 2 NO-LOCK:

        IF NOT CAN-FIND(FIRST tt-natureza
                        WHERE tt-natureza.nat-operacao = nfe013.nat-operacao) THEN DO:
            CREATE tt-natureza.
            ASSIGN tt-natureza.nat-operacao = nfe013.nat-operacao
                   i-cont                   = i-cont + 1.
        END.

        IF i-cont > 1 THEN
            LEAVE.
    END.
    do trans:
        FOR FIRST bf{&TableName} EXCLUSIVE-LOCK
            WHERE ROWID(bf{&TableName}) = pr-nota:
            ASSIGN bf{&TableName}.log-multi-cfop = IF i-cont > 1 THEN YES ELSE NO.
        END.
    end.
    RELEASE bf{&TableName}.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

