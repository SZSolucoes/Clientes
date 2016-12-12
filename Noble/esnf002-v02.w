&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgdes            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESNF002-V02 2.06.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/viewerd.w



DEFINE VARIABLE c-numero-ordem            AS CHARacter  NO-UNDO.
DEFINE VARIABLE c-numero-ordem-add        AS CHARacter  NO-UNDO.
DEFINE VARIABLE c-numero-ordem-placa-veic AS CHARACTER  NO-UNDO.


DEFINE BUFFER b-ordem-carregamento FOR ordem-carregamento.
DEFINE BUFFER bf-ordem-carregamento FOR ordem-carregamento.


DEFINE VARIABLE h-handle-v01        AS HANDLE      NO-UNDO.
DEFINE VARIABLE c_nr_cartao_pedagio AS CHARACTER   NO-UNDO.

{esp/esnf002.i " " " "}
{esp/esnf002a.i}

DEFINE VARIABLE SINCONSVEICULO          AS LONGCHAR   NO-UNDO.
DEFINE VARIABLE MTCONSVEICULORESULT     AS LONGCHAR   NO-UNDO.

DEFINE VARIABLE SINCONSTPOP             AS LONGCHAR   NO-UNDO.
DEFINE VARIABLE MTCONSTPOPRESULT        AS LONGCHAR   NO-UNDO.

DEFINE VARIABLE v-cod-filial             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE h-label                  AS HANDLE     NO-UNDO.
DEFINE VARIABLE p-retorno-cod-placa      AS CHARACTER NO-UNDO.
DEFINE VARIABLE p-retorno-motorista      AS CHARACTER NO-UNDO.
DEFINE VARIABLE p-retorno-tp-oper        AS INTEGER NO-UNDO.

DEF TEMP-TABLE tt-placa-veiculo NO-UNDO
    FIELD cod-tipo-veic AS CHARACTER
    FIELD cod-veic      AS CHARACTER
    FIELD desc-tp-veic  AS CHARACTER
    FIELD cod-placa     AS CHARACTER
    FIELD uf-placa      AS CHARACTER
    INDEX idx-placa AS PRIMARY cod-placa.

DEF TEMP-TABLE tt-tp-operacao NO-UNDO
    FIELD CDTPOP AS INTEGER
    FIELD DSTPOP AS CHARACTER
    INDEX idx-cdtpop AS PRIMARY CDTPOP.

def temp-table tt-aux no-undo
    field cod-tp as char
    field des-tp as char.

/* Local Variable Definitions ---                                       */
DEFINE VAR v-row-parent AS ROWID NO-UNDO.

DEFINE NEW GLOBAL SHARED VAR hProgramZoom AS WIDGET-HANDLE   NO-UNDO.
DEFINE VARIABLE c-cgc-transp AS CHARACTER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES ordem-carregamento
&Scoped-define FIRST-EXTERNAL-TABLE ordem-carregamento


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ordem-carregamento.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ordem-carregamento.cnh ~
ordem-carregamento.tp-motorista ordem-carregamento.nome-transp ordem-carregamento.cod-redespachante ~
ordem-carregamento.placa-veiculo ordem-carregamento.placa-carreta ~
ordem-carregamento.placa-carreta-2 ordem-carregamento.qtd-km-percorridos ordem-carregamento.tp-operacao ordem-carregamento.nome-propriet ~
ordem-carregamento.cidade-propriet ordem-carregamento.estado-propriet ~
ordem-carregamento.tel-propriet-1 ordem-carregamento.tel-propriet-2 ~
/*ordem-carregamento.nr-tabela-frete*/ ordem-carregamento.cod-destinatario ordem-carregamento.cod-local ~
ordem-carregamento.observacao 
&Scoped-define ENABLED-TABLES ordem-carregamento
&Scoped-define FIRST-ENABLED-TABLE ordem-carregamento
&Scoped-Define ENABLED-OBJECTS rt-mold RECT-18 
&Scoped-Define DISPLAYED-FIELDS ordem-carregamento.cnh ~
ordem-carregamento.tp-motorista ordem-carregamento.NR_CARTAO_PEDAGIO ~
ordem-carregamento.nome-transp ordem-carregamento.cod-redespachante ordem-carregamento.placa-veiculo ~
ordem-carregamento.placa-carreta ordem-carregamento.placa-carreta-2 ~
ordem-carregamento.nome-propriet ordem-carregamento.tp-operacao ordem-carregamento.cidade-propriet ~
ordem-carregamento.estado-propriet ordem-carregamento.tel-propriet-1 ~
ordem-carregamento.tel-propriet-2 /*ordem-carregamento.nr-tabela-frete*/ ordem-carregamento.cod-destinatario ~
ordem-carregamento.cod-local ordem-carregamento.observacao ~
ordem-carregamento.VL_PEDAGIO ordem-carregamento.VL_PEDAGIO_MANUAL 
&Scoped-define DISPLAYED-TABLES ordem-carregamento
&Scoped-define FIRST-DISPLAYED-TABLE ordem-carregamento
&Scoped-Define DISPLAYED-OBJECTS fi_nome_motorista fi_cidade_motorista ~
fi_estado_motorista fi_desc_transportador c-cd-tipo-veiculo fi-desc-tp-operacao ~
c-ds-tipo-veiculo c-desc-destinatario c-desc-local c-cidade-origem c-uf-origem ~
c-cidade-destino c-uf-destino 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-ASSIGN-FIELDS fi_desc_transportador 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE c-cd-tipo-veiculo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tipo Ve°culo" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE c-cidade-destino AS CHARACTER FORMAT "X(256)":U 
     LABEL "Destino" 
     VIEW-AS FILL-IN 
     SIZE 23 BY .88 NO-UNDO.

DEFINE VARIABLE c-cidade-origem AS CHARACTER FORMAT "X(256)":U 
     LABEL "Origem" 
     VIEW-AS FILL-IN 
     SIZE 23 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-local AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 73.86 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-destinatario AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 75.72 BY .88 NO-UNDO.

DEFINE VARIABLE c-ds-tipo-veiculo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 73.86 BY .88 NO-UNDO.

DEFINE VARIABLE c-uf-destino AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 4.72 BY .88 NO-UNDO.

DEFINE VARIABLE c-uf-origem AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 4.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi_cidade_motorista AS CHARACTER FORMAT "X(60)":U 
     LABEL "Cidade" 
     VIEW-AS FILL-IN 
     SIZE 49.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi_desc_transportador AS CHARACTER FORMAT "X(60)":U 
     VIEW-AS FILL-IN 
     SIZE 70.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi_desc_redespachante AS CHARACTER FORMAT "X(60)":U 
     VIEW-AS FILL-IN 
     SIZE 70.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-tp-operacao AS CHARACTER FORMAT "X(60)":U 
     VIEW-AS FILL-IN 
     SIZE 78.12 BY .88 NO-UNDO.

DEFINE VARIABLE fi_estado_motorista AS CHARACTER FORMAT "X(60)":U 
     LABEL "UF" 
     VIEW-AS FILL-IN 
     SIZE 8.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi_nome_motorista AS CHARACTER FORMAT "X(60)":U 
     VIEW-AS FILL-IN 
     SIZE 62.72 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 2.63.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 110 BY 17.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     ordem-carregamento.cnh AT ROW 1.42 COL 15.86 COLON-ALIGNED
          LABEL "CNH Motorista"
          VIEW-AS FILL-IN 
          SIZE 20 BY .88
     fi_nome_motorista AT ROW 1.42 COL 36.29 COLON-ALIGNED NO-LABEL
     fi_cidade_motorista AT ROW 2.42 COL 15.86 COLON-ALIGNED
     fi_estado_motorista AT ROW 2.42 COL 90.29 COLON-ALIGNED
     ordem-carregamento.tp-motorista AT ROW 3.42 COL 15.86 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 2
          LIST-ITEM-PAIRS "Pr¢prio",1,
                     "Terceiro",2
          DROP-DOWN-LIST
          SIZE 20.14 BY 1
     ordem-carregamento.NR_CARTAO_PEDAGIO AT ROW 3.42 COL 76.43 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 22.57 BY .88
     ordem-carregamento.nome-transp AT ROW 4.42 COL 15.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     fi_desc_transportador AT ROW 4.42 COL 28.29 COLON-ALIGNED NO-LABEL

     ordem-carregamento.cod-redespachante  AT ROW 5.42 COL 15.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     fi_desc_redespachante AT ROW 5.42 COL 28.29 COLON-ALIGNED NO-LABEL

     ordem-carregamento.placa-veiculo AT ROW 6.42 COL 15.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     ordem-carregamento.placa-carreta AT ROW 6.42 COL 41.72 COLON-ALIGNED
          LABEL "Placa Carreta (1)"
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     ordem-carregamento.placa-carreta-2 AT ROW 6.42 COL 67.5 COLON-ALIGNED
          LABEL "Placa Carreta (2)"
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     ordem-carregamento.qtd-km-percorridos AT ROW 6.42 COL 83.5 COLON-ALIGNED
          LABEL "KM"
          VIEW-AS FILL-IN 
          SIZE 15.4 BY .88
     c-cd-tipo-veiculo AT ROW 7.42 COL 15.86 COLON-ALIGNED WIDGET-ID 10
     c-ds-tipo-veiculo AT ROW 7.42 COL 25.14 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     ordem-carregamento.nome-propriet AT ROW 8.42 COL 15.86 COLON-ALIGNED
          LABEL "Propriet†rio Ve°culo"
          VIEW-AS FILL-IN 
          SIZE 83.14 BY .88
     ordem-carregamento.tp-operacao AT ROW 9.42 COL 15.86 COLON-ALIGNED
          LABEL "Tipo de Operaá∆o" 
          VIEW-AS FILL-IN 
          SIZE 4.54 BY .88
     fi-desc-tp-operacao AT ROW 9.42 COL 20.9 COLON-ALIGNED NO-LABEL

     ordem-carregamento.cidade-propriet AT ROW 10.42 COL 15.86 COLON-ALIGNED
          LABEL "Cidade Propriet†rio"
          VIEW-AS FILL-IN 
          SIZE 49.14 BY .88
     ordem-carregamento.estado-propriet AT ROW 10.42 COL 90.29 COLON-ALIGNED
          LABEL "UF Propriet†rio"
          VIEW-AS FILL-IN 
          SIZE 8.72 BY .88
     ordem-carregamento.tel-propriet-1 AT ROW 11.42 COL 15.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16.14 BY .88
     ordem-carregamento.tel-propriet-2 AT ROW 11.42 COL 48.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16.14 BY .88
    /* ordem-carregamento.nr-tabela-frete AT ROW 10.42 COL 15.86 COLON-ALIGNED WIDGET-ID 32
          VIEW-AS FILL-IN 
          SIZE 5 BY .88*/
     ordem-carregamento.cod-destinatario AT ROW 12.42 COL 15.86 COLON-ALIGNED WIDGET-ID 32         
           VIEW-AS FILL-IN                                                                      
           SIZE 7 BY .88                                                                      
     c-desc-destinatario AT ROW 12.42 COL 23.29 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     ordem-carregamento.cod-local AT ROW 13.42 COL 15.86 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 8.86 BY .88
     c-desc-local AT ROW 13.42 COL 25.14 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     ordem-carregamento.observacao AT ROW 14.5 COL 15.86 COLON-ALIGNED
          LABEL "Observaá∆o"
          VIEW-AS EDITOR
          SIZE 83.14 BY .88
     c-cidade-origem AT ROW 16.21 COL 17 COLON-ALIGNED WIDGET-ID 18
     c-uf-origem AT ROW 16.21 COL 40.29 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     ordem-carregamento.VL_PEDAGIO AT ROW 16.21 COL 67.14 COLON-ALIGNED WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 11 BY .88
     c-cidade-destino AT ROW 17.21 COL 17 COLON-ALIGNED WIDGET-ID 20
     c-uf-destino AT ROW 17.21 COL 40.29 COLON-ALIGNED NO-LABEL WIDGET-ID 24
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME f-main
     ordem-carregamento.VL_PEDAGIO_MANUAL AT ROW 17.21 COL 67.14 COLON-ALIGNED WIDGET-ID 30
          LABEL "Vl. Ped†gio Manual"
          VIEW-AS FILL-IN 
          SIZE 11 BY .88
     "Ped†gio" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 15.42 COL 13.43 WIDGET-ID 16
     rt-mold AT ROW 1.25 COL 2
     RECT-18 AT ROW 15.67 COL 11 WIDGET-ID 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ordem-carregamento
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 17.75
         WIDTH              = 111.43.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{include/c-viewer.i}
{utp/ut-glob.i}
{include/i_dbtype.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN c-cd-tipo-veiculo IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-cidade-destino IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-cidade-origem IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-desc-local IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-desc-destinatario IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-ds-tipo-veiculo IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-uf-destino IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-uf-origem IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ordem-carregamento.cidade-propriet IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ordem-carregamento.cnh IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ordem-carregamento.estado-propriet IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN fi_cidade_motorista IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_desc_transportador IN FRAME f-main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_estado_motorista IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_nome_motorista IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ordem-carregamento.nome-propriet IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ordem-carregamento.NR_CARTAO_PEDAGIO IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ordem-carregamento.placa-carreta IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ordem-carregamento.placa-carreta-2 IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ordem-carregamento.VL_PEDAGIO IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ordem-carregamento.VL_PEDAGIO_MANUAL IN FRAME f-main
   NO-ENABLE EXP-LABEL                                                  */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME ordem-carregamento.tp-operacao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-carregamento.tp-operacao V-table-Win
ON F5 OF ordem-carregamento.tp-operacao IN FRAME f-main /* CNH Motorista */
DO:

    empty temp-table tt-tp-operacao.

    for each GV4010 no-lock where
             GV4010.GV4_SIT = '1':

        if GV4010.GV4_CDTPOP = 'ISENTO' then
            next.

        create tt-tp-operacao.
        assign tt-tp-operacao.CDTPOP = int(GV4010.GV4_CDTPOP)
               tt-tp-operacao.DSTPOP = GV4010.GV4_DSTPOP.
    end.

    ASSIGN FRAME f-main:SENSITIVE = NO.
    RUN espzoom/z03esnf002.w (INPUT TABLE tt-tp-operacao, OUTPUT p-retorno-tp-oper).

    IF  p-retorno-tp-oper <> 0 THEN DO:
        ASSIGN ordem-carregamento.tp-operacao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(p-retorno-tp-oper).

        FIND FIRST tt-tp-operacao
             WHERE tt-tp-operacao.CDTPOP = p-retorno-tp-oper NO-ERROR.
        IF AVAIL tt-tp-operacao THEN DO:

            ASSIGN fi-desc-tp-operacao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = tt-tp-operacao.DSTPOP. 
        END.
    END.
    ASSIGN FRAME f-main:SENSITIVE = YES.
    APPLY "entry" TO ordem-carregamento.tp-operacao IN FRAME f-main.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-carregamento.tp-operacao V-table-Win
ON LEAVE OF ordem-carregamento.tp-operacao IN FRAME f-main /* CNH Motorista */
DO:
    FIND FIRST tt-tp-operacao
         WHERE tt-tp-operacao.CDTPOP = INPUT FRAME {&FRAME-NAME} ordem-carregamento.tp-operacao NO-ERROR.
    IF AVAIL tt-tp-operacao THEN DO:
        ASSIGN fi-desc-tp-operacao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = tt-tp-operacao.DSTPOP. 
    END.
    else
        ASSIGN fi-desc-tp-operacao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".    

END.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-carregamento.tp-operacao V-table-Win
ON MOUSE-SELECT-DBLCLICK OF ordem-carregamento.tp-operacao IN FRAME f-main /* CNH Motorista */
DO:
    APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ordem-carregamento.cnh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-carregamento.cnh V-table-Win
ON F5 OF ordem-carregamento.cnh IN FRAME f-main /* CNH Motorista */
DO:

    empty temp-table tt-motorista.  
  
    for each GUU010 no-lock where
             GUU010.GUU_SIT = '1':
        create tt-motorista.
        assign tt-motorista.CDMTR  = GUU010.GUU_CDMTR
               tt-motorista.CRPEDG = GUU010.GUU_CRPEDG
               tt-motorista.ESTCNH = GUU010.GUU_ESTCNH
               tt-motorista.FILIAL = GUU010.GUU_FILIAL
               tt-motorista.IDFED  = GUU010.GUU_IDFED
               tt-motorista.MUNCNH = GUU010.GUU_MUNCNH
               tt-motorista.NMMTR  = GUU010.GUU_NMMTR
               tt-motorista.NUMCNH = GUU010.GUU_NUMCNH
               tt-motorista.RG     = GUU010.GUU_RG.
    end.

    ASSIGN FRAME f-main:SENSITIVE = NO.

    RUN espzoom/z02esnf002.w (INPUT TABLE tt-motorista, OUTPUT p-retorno-motorista).

    IF  p-retorno-motorista <> "" THEN DO:
        ASSIGN ordem-carregamento.cnh:SCREEN-VALUE IN FRAME {&FRAME-NAME} = p-retorno-motorista.

        FIND FIRST tt-motorista
             WHERE tt-motorista.NUMCNH = p-retorno-motorista NO-ERROR.
        IF AVAIL tt-motorista THEN DO:

            ASSIGN fi_nome_motorista  :SCREEN-VALUE IN FRAME {&FRAME-NAME} = tt-motorista.NMMTR 
                   fi_cidade_motorista:SCREEN-VALUE IN FRAME {&FRAME-NAME} = tt-motorista.MUNCNH
                   fi_estado_motorista:SCREEN-VALUE IN FRAME {&FRAME-NAME} = tt-motorista.ESTCNH
                   ordem-carregamento.NR_CARTAO_PEDAGIO:SCREEN-VALUE IN FRAME {&FRAME-NAME} = tt-motorista.CRPEDG.
        END.
    END.
    ASSIGN FRAME f-main:SENSITIVE = YES.
    APPLY "entry" TO ordem-carregamento.cnh IN FRAME f-main.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-carregamento.cnh V-table-Win
ON LEAVE OF ordem-carregamento.cnh IN FRAME f-main /* CNH Motorista */
DO:
    FIND FIRST tt-motorista
         WHERE tt-motorista.NUMCNH = INPUT FRAME {&FRAME-NAME} ordem-carregamento.cnh NO-ERROR.
    IF AVAIL tt-motorista THEN DO:

        ASSIGN fi_nome_motorista  :SCREEN-VALUE IN FRAME {&FRAME-NAME} = tt-motorista.NMMTR 
               fi_cidade_motorista:SCREEN-VALUE IN FRAME {&FRAME-NAME} = tt-motorista.MUNCNH
               fi_estado_motorista:SCREEN-VALUE IN FRAME {&FRAME-NAME} = tt-motorista.ESTCNH
               ordem-carregamento.NR_CARTAO_PEDAGIO:SCREEN-VALUE IN FRAME {&FRAME-NAME} = tt-motorista.CRPEDG.
    END.
    else do:

        ASSIGN fi_nome_motorista:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = ""
                   fi_cidade_motorista:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
                   fi_estado_motorista:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
                   ordem-carregamento.NR_CARTAO_PEDAGIO:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".            
    end.

   /* busca dados da ultima ocorrencia do ord_motorista */
   FIND LAST b-ordem-carregamento NO-LOCK
       WHERE b-ordem-carregamento.cnh = INPUT FRAME {&FRAME-NAME} ordem-carregamento.cnh USE-INDEX cnh_data NO-ERROR.
   IF AVAIL b-ordem-carregamento THEN DO:

       IF ordem-carregamento.nome-transp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN DO:

           ASSIGN ordem-carregamento.nome-transp:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = b-ordem-carregamento.nome-transp 
                  ordem-carregamento.cidade-propriet:SCREEN-VALUE IN FRAME {&FRAME-NAME} = b-ordem-carregamento.cidade-propriet
                  ordem-carregamento.estado-propriet:SCREEN-VALUE IN FRAME {&FRAME-NAME} = b-ordem-carregamento.estado-propriet
                  ordem-carregamento.nome-propriet:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = b-ordem-carregamento.nome-propriet  
                  ordem-carregamento.nome-transp:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = b-ordem-carregamento.nome-transp    
                  ordem-carregamento.placa-carreta:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = b-ordem-carregamento.placa-carreta  
                  ordem-carregamento.placa-carreta-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = b-ordem-carregamento.placa-carreta-2
                  ordem-carregamento.placa-veiculo:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = b-ordem-carregamento.placa-veiculo  
                  ordem-carregamento.tel-propriet-1:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = b-ordem-carregamento.tel-propriet-1 
                  ordem-carregamento.tel-propriet-2:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = b-ordem-carregamento.tel-propriet-2 
                  ordem-carregamento.tp-motorista:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = STRING(b-ordem-carregamento.tp-motorista).   

           FIND FIRST transporte NO-LOCK
                WHERE transporte.nome-abrev = INPUT FRAME {&FRAME-NAME} ordem-carregamento.nome-transp NO-ERROR.
           IF AVAIL transporte THEN
              ASSIGN fi_desc_transportador:SCREEN-VALUE IN FRAME {&FRAME-NAME} = transporte.nome.
           ELSE
              ASSIGN fi_desc_transportador:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
           
       END.
   END.

   RUN pi-atualiza-pedagio IN THIS-PROCEDURE (INPUT "").

   /*Busca Cartao Pedagio do WebService do GFE*/
/*    RUN pi-buscaCartaoPedagio. */
END.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-carregamento.cnh V-table-Win
ON MOUSE-SELECT-DBLCLICK OF ordem-carregamento.cnh IN FRAME f-main /* CNH Motorista */
DO:
    APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ordem-carregamento.cod-local
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-carregamento.cod-local V-table-Win
ON F5 OF ordem-carregamento.cod-local IN FRAME f-main /* C¢digo Local */
DO:
  {include/zoomvar.i &prog-zoom  = "espzoom/escd001-z01.w"
                     &campo      = ordem-carregamento.cod-local
                     &campozoom  = cod-local
                     &campo2     = c-desc-local
                     &campozoom2 = desc-local}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-carregamento.cod-local V-table-Win
ON LEAVE OF ordem-carregamento.cod-local IN FRAME f-main /* C¢digo Local */
DO:
  
    FIND FIRST esp-local
         WHERE esp-local.cod-local = INPUT FRAME {&FRAME-NAME} ordem-carregamento.cod-local NO-LOCK NO-ERROR.
    IF AVAIL esp-local THEN
        ASSIGN c-desc-local:SCREEN-VALUE IN FRAME {&FRAME-NAME} = esp-local.desc-local.
    ELSE
        ASSIGN c-desc-local:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-carregamento.cod-local V-table-Win
ON MOUSE-SELECT-DBLCLICK OF ordem-carregamento.cod-local IN FRAME f-main /* C¢digo Local */
DO:
  APPLY "F5":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-carregamento.cod-redespachante V-table-Win
ON MOUSE-SELECT-DBLCLICK OF ordem-carregamento.cod-redespachante IN FRAME f-main /* C¢digo Local */
DO:
  APPLY "F5":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-carregamento.placa-veiculo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF ordem-carregamento.placa-veiculo IN FRAME f-main /* C¢digo Local */
DO:
  APPLY "F5":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-carregamento.placa-carreta V-table-Win
ON MOUSE-SELECT-DBLCLICK OF ordem-carregamento.placa-carreta IN FRAME f-main /* C¢digo Local */
DO:
  APPLY "F5":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-carregamento.placa-carreta-2 V-table-Win
ON MOUSE-SELECT-DBLCLICK OF ordem-carregamento.placa-carreta-2 IN FRAME f-main /* C¢digo Local */
DO:
  APPLY "F5":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME ordem-carregamento.nome-transp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-carregamento.nome-transp V-table-Win
ON F5 OF ordem-carregamento.nome-transp IN FRAME f-main /* Transportador */
DO:
    {include/zoomvar.i &prog-zoom=adzoom/z01ad268.w
                       &campo=ordem-carregamento.nome-transp
                       &campo2=fi_desc_transportador
                       &campozoom=nome-abrev
                       &campozoom2=nome}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME ordem-carregamento.cod-redespachante
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-carregamento.cod-redespachante V-table-Win
ON F5 OF ordem-carregamento.cod-redespachante IN FRAME f-main /* Transportador */
DO:
      {include/zoomvar.i &prog-zoom=adzoom/z01ad268.w
                         &campo=ordem-carregamento.cod-redespachante
                         &campo2=fi_desc_redespachante
                         &campozoom=cod-transp
                         &campozoom2=nome}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-carregamento.cod-redespachante V-table-Win
ON LEAVE OF ordem-carregamento.cod-redespachante IN FRAME f-main /* Transportador */
DO:
    
    FIND FIRST transporte
         WHERE transporte.cod-transp = INPUT FRAME {&FRAME-NAME} ordem-carregamento.cod-redespachante NO-LOCK NO-ERROR.
    IF AVAIL transporte THEN
        ASSIGN fi_desc_redespachante:SCREEN-VALUE IN FRAME {&FRAME-NAME} = transporte.nome.
    ELSE
        ASSIGN fi_desc_redespachante:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-carregamento.nome-transp V-table-Win
ON LEAVE OF ordem-carregamento.nome-transp IN FRAME f-main /* Transportador */
DO:
    FIND FIRST transporte WHERE transporte.nome-abrev = INPUT FRAME {&FRAME-NAME} ordem-carregamento.nome-transp NO-LOCK NO-ERROR.
    IF AVAIL transporte THEN
       ASSIGN fi_desc_transportador:SCREEN-VALUE IN FRAME {&FRAME-NAME} = transporte.nome.
    ELSE
       ASSIGN fi_desc_transportador:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-carregamento.nome-transp V-table-Win
ON MOUSE-SELECT-DBLCLICK OF ordem-carregamento.nome-transp IN FRAME f-main /* Transportador */
DO:
    {include/zoomvar.i &prog-zoom=adzoom/z01ad268.w
                       &campo=ordem-carregamento.nome-transp
                       &campo2=fi_desc_transportador
                       &campozoom=nome-abrev
                       &campozoom2=nome}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ordem-carregamento.cod-destinatario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-carregamento.cod-destinatario V-table-Win
ON F5 OF ordem-carregamento.cod-destinatario IN FRAME f-main /* Tab Frete */
DO:

  /*--- ZOOM SMART OBJECT ---*/ 
    {include/zoomvar.i &prog-zoom=adzoom/z07ad098.w
                       &campo=ordem-carregamento.cod-destinatario
                       &campozoom=cod-emitente
                       &campo2=c-desc-destinatario
                       &campozoom2=nome-abrev
                       &frame="f-main"}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-carregamento.ci8d-destinatario V-table-Win
ON LEAVE OF ordem-carregamento.cod-destinatario IN FRAME f-main /* Tab Frete */
DO:

    FIND FIRST emitente NO-LOCK
         WHERE emitente.cod-emitente = int(ordem-carregamento.cod-destinatario:SCREEN-VALUE IN FRAME {&FRAME-NAME})  NO-ERROR.
    IF AVAIL emitente THEN
       ASSIGN c-desc-destinatario:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-abrev.
    ELSE
       ASSIGN c-desc-destinatario:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-carregamento.cod-destinatario V-table-Win
ON MOUSE-SELECT-DBLCLICK OF ordem-carregamento.cod-destinatario IN FRAME f-main /* Tab Frete */
DO:
    APPLY "F5" TO SELF.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-carregamento.placa-veiculo V-table-Win
ON LEAVE OF ordem-carregamento.placa-veiculo IN FRAME f-main /* Placa Veiculo */
DO: 
    IF  REPLACE(INPUT FRAME {&FRAME-NAME} ordem-carregamento.placa-veiculo,"-","") <> "" THEN DO:
    
        IF NOT CAN-FIND(FIRST tt-placa-veiculo
                        WHERE tt-placa-veiculo.cod-placa = REPLACE(INPUT FRAME {&FRAME-NAME} ordem-carregamento.placa-veiculo,"-","") NO-LOCK) THEN DO: 

            RUN zep\zeutp\ze-erro.p (INPUT 1, 
                                     INPUT "Veiculo n∆o cadastrado com a placa ve°culo informada.", 
                                     INPUT "N∆o foi encontrado nenhum veiculo cadastrado no sistema com a placa informada."). 
            ASSIGN ordem-carregamento.placa-veiculo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
            RETURN NO-APPLY. 
            
        END.
    
    
        RUN busca-tipo-veiculo.
    END.
        
	
    /*FIND FIRST veiculo 
         WHERE veiculo.placa-veiculo = INPUT FRAME {&FRAME-NAME} ordem-carregamento.placa-veiculo NO-LOCK NO-ERROR.
    IF AVAIL veiculo THEN DO:
        ASSIGN ordem-carregamento.placa-carreta:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = veiculo.placa-carreta_1  
               ordem-carregamento.placa-carreta-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = veiculo.placa-carreta_2  
               ordem-carregamento.nome-propriet:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = veiculo.nome-propriet
               ordem-carregamento.tel-propriet-1:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = veiculo.tel-propriet-1
               ordem-carregamento.tel-propriet-2:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = veiculo.tel-propriet-2
               ordem-carregamento.cidade-propriet:SCREEN-VALUE IN FRAME {&FRAME-NAME} = veiculo.cidade-propriet
               ordem-carregamento.estado-propriet:SCREEN-VALUE IN FRAME {&FRAME-NAME} = veiculo.estado-propriet.
	
         RUN pi-atualiza-pedagio IN THIS-PROCEDURE ( INPUT c-numero-ordem-placa-veic).
	
	END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ordem-carregamento.placa-veiculo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-carregamento.placa-veiculo V-table-Win
ON F5 OF ordem-carregamento.placa-veiculo IN FRAME f-main /* Placa Veiculo */
DO:

    empty temp-table tt-placa-veiculo.
    empty temp-table tt-aux.

    for each GV3010 no-lock:
    
        create tt-aux.
        assign tt-aux.cod-tp = GV3010.GV3_CDTPVC
               tt-aux.des-tp = GV3010.GV3_DSTPVC.
    
    end.

    for each GU8010 no-lock where
             GU8010.GU8_SIT = '1':

        find first tt-aux where
                   tt-aux.cod-tp = GU8010.GU8_CDTPVC no-error.

        create tt-placa-veiculo.
        assign tt-placa-veiculo.cod-tipo-veic = GU8010.GU8_CDTPVC 
               tt-placa-veiculo.cod-veic      = GU8010.GU8_CDVEIC
               tt-placa-veiculo.desc-tp-veic  = if avail tt-aux then tt-aux.des-tp else ''
               tt-placa-veiculo.cod-placa     = GU8010.GU8_PLACA 
               tt-placa-veiculo.uf-placa      = GU8010.GU8_UFPLAC.
    end.

/*     ASSIGN l-implanta = YES. */
    ASSIGN FRAME f-main:SENSITIVE = NO.   
    RUN espzoom/z01esnf002.w (INPUT TABLE tt-placa-veiculo, OUTPUT p-retorno-cod-placa).

    IF  p-retorno-cod-placa <> "" THEN
        ASSIGN ordem-carregamento.placa-veiculo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = p-retorno-cod-placa.
    
    RUN busca-tipo-veiculo.
    ASSIGN FRAME f-main:SENSITIVE = YES.
    APPLY "entry" TO ordem-carregamento.placa-veiculo IN FRAME f-main.
    RETURN "OK":U.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME ordem-carregamento.placa-carreta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-carregamento.placa-carreta V-table-Win
ON F5 OF ordem-carregamento.placa-carreta IN FRAME f-main /* Placa Veiculo */
DO:

    empty temp-table tt-placa-veiculo.
    empty temp-table tt-aux.

    for each GV3010 no-lock:
    
        create tt-aux.
        assign tt-aux.cod-tp = GV3010.GV3_CDTPVC
               tt-aux.des-tp = GV3010.GV3_DSTPVC.
    
    end.

    for each GU8010 no-lock where
             GU8010.GU8_SIT = '1':

        find first tt-aux where
                   tt-aux.cod-tp = GU8010.GU8_CDTPVC no-error.

        create tt-placa-veiculo.
        assign tt-placa-veiculo.cod-tipo-veic = GU8010.GU8_CDTPVC 
               tt-placa-veiculo.cod-veic      = GU8010.GU8_CDVEIC
               tt-placa-veiculo.desc-tp-veic  = if avail tt-aux then tt-aux.des-tp else ''
               tt-placa-veiculo.cod-placa     = GU8010.GU8_PLACA 
               tt-placa-veiculo.uf-placa      = GU8010.GU8_UFPLAC.
    end.

    ASSIGN FRAME f-main:SENSITIVE = NO.
    RUN espzoom/z01esnf002.w (INPUT TABLE tt-placa-veiculo, OUTPUT p-retorno-cod-placa).

    IF  p-retorno-cod-placa <> "" THEN
        ASSIGN ordem-carregamento.placa-carreta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = p-retorno-cod-placa.
    RUN busca-tipo-carreta-1.
    ASSIGN FRAME f-main:SENSITIVE = YES.
    APPLY "entry" TO ordem-carregamento.placa-carreta IN FRAME f-main.
    RETURN "OK":U.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-carregamento.placa-carreta V-table-Win
ON LEAVE OF ordem-carregamento.placa-carreta IN FRAME f-main /* Placa Veiculo */
DO: 
    IF REPLACE(INPUT FRAME {&FRAME-NAME} ordem-carregamento.placa-carreta,"-","") <> "" AND
       NOT CAN-FIND(FIRST tt-placa-veiculo
                    WHERE tt-placa-veiculo.cod-placa = REPLACE(INPUT FRAME {&FRAME-NAME} ordem-carregamento.placa-carreta,"-","") NO-LOCK) THEN DO: 

         RUN zep\zeutp\ze-erro.p (INPUT 1, 
                                  INPUT "Veiculo n∆o cadastrado com a placa carreta(1) informada.", 
                                  INPUT "N∆o foi encontrado nenhum veiculo cadastrado no sistema com a placa informada."). 
         ASSIGN ordem-carregamento.placa-carreta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
         APPLY "leave" TO ordem-carregamento.placa-veiculo IN FRAME f-main.
         RETURN NO-APPLY. 
    END.

    RUN busca-tipo-carreta-1.
      
END.



&Scoped-define SELF-NAME ordem-carregamento.placa-carreta-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-carregamento.placa-carreta-2 V-table-Win
ON F5 OF ordem-carregamento.placa-carreta-2 IN FRAME f-main /* Placa Veiculo */
DO:

    empty temp-table tt-placa-veiculo.
    empty temp-table tt-aux.

    for each GV3010 no-lock:
    
        create tt-aux.
        assign tt-aux.cod-tp = GV3010.GV3_CDTPVC
               tt-aux.des-tp = GV3010.GV3_DSTPVC.
    
    end.

    for each GU8010 no-lock where
             GU8010.GU8_SIT = '1':

        find first tt-aux where
                   tt-aux.cod-tp = GU8010.GU8_CDTPVC no-error.

        create tt-placa-veiculo.
        assign tt-placa-veiculo.cod-tipo-veic = GU8010.GU8_CDTPVC 
               tt-placa-veiculo.cod-veic      = GU8010.GU8_CDVEIC
               tt-placa-veiculo.desc-tp-veic  = if avail tt-aux then tt-aux.des-tp else ''
               tt-placa-veiculo.cod-placa     = GU8010.GU8_PLACA 
               tt-placa-veiculo.uf-placa      = GU8010.GU8_UFPLAC.
    end.

    ASSIGN FRAME f-main:SENSITIVE = NO.
    RUN espzoom/z01esnf002.w (INPUT TABLE tt-placa-veiculo, OUTPUT p-retorno-cod-placa).
    IF  p-retorno-cod-placa <> "" THEN
        ASSIGN ordem-carregamento.placa-carreta-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = p-retorno-cod-placa.

    RUN busca-tipo-carreta-2.
    ASSIGN FRAME f-main:SENSITIVE = YES.
    APPLY "entry" TO ordem-carregamento.placa-carreta-2 IN FRAME f-main.
    RETURN "OK":U.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ordem-carregamento.placa-carreta-2 V-table-Win
ON LEAVE OF ordem-carregamento.placa-carreta-2 IN FRAME f-main /* Placa Veiculo */
DO: 
    IF REPLACE(INPUT FRAME {&FRAME-NAME} ordem-carregamento.placa-carreta-2,"-","") <> "" AND
       NOT CAN-FIND(FIRST tt-placa-veiculo
                    WHERE tt-placa-veiculo.cod-placa = REPLACE(INPUT FRAME {&FRAME-NAME} ordem-carregamento.placa-carreta-2,"-","") NO-LOCK) THEN DO: 

         RUN zep\zeutp\ze-erro.p (INPUT 1, 
                                  INPUT "Veiculo n∆o cadastrado com a placa carreta(2) informada.", 
                                  INPUT "N∆o foi encontrado nenhum veiculo cadastrado no sistema com a placa informada."). 
         ASSIGN ordem-carregamento.placa-carreta-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
         APPLY "leave" TO ordem-carregamento.placa-carreta IN FRAME f-main.
         RETURN NO-APPLY.
    END.

    RUN busca-tipo-carreta-2.
    
    
END.


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
  ordem-carregamento.cnh:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.  
  ordem-carregamento.nome-transp:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.  
  ordem-carregamento.tp-operacao:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  ordem-carregamento.placa-veiculo:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  ordem-carregamento.cod-redespachante:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}. 
  ordem-carregamento.placa-carreta:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}. 
  ordem-carregamento.placa-carreta-2:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}. 
  ordem-carregamento.cod-local:LOAD-MOUSE-POINTER("image/lupa.cur").
  ordem-carregamento.cod-destinatario:LOAD-MOUSE-POINTER("image/lupa.cur").
/*  ordem-carregamento.nr-tabela-frete:LOAD-MOUSE-POINTER("image/lupa.cur").*/

  IF ordem-carregamento.placa-veiculo:MODIFIED IN FRAME {&FRAME-NAME} THEN
     ASSIGN ordem-carregamento.placa-veiculo:FORMAT IN FRAME {&FRAME-NAME} = "XXX-9999".
  ELSE
     ASSIGN ordem-carregamento.placa-veiculo:FORMAT IN FRAME {&FRAME-NAME} = "x(8)".


  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "ordem-carregamento"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ordem-carregamento"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME f-main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .



FIND LAST control-ordem-carreg NO-LOCK NO-ERROR.
If avail control-ordem-carreg then
    ASSIGN c-numero-ordem-add = STRING(control-ordem-carreg.num-corrente + 1).
else  
    ASSIGN c-numero-ordem-add = "1".


ASSIGN c-numero-ordem-placa-veic = c-numero-ordem-add.

IF adm-new-record THEN 
      ASSIGN ordem-carregamento.cod-destinatario:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = "9999999". 
           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VAR c-alterado AS CHAR NO-UNDO.
    DEFINE VARIABLE c-replace-rg   AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE c-replace-cpf  AS CHARACTER   NO-UNDO.

    /* Code placed here will execute PRIOR to standard behavior. */
    {include/i-valid.i}
    
    /*:T Ponha na pi-validate todas as validaá‰es */
    /*:T N∆o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    FIND FIRST transporte NO-LOCK
          WHERE transporte.nome-abrev = INPUT FRAME {&FRAME-NAME} ordem-carregamento.nome-transp NO-ERROR.
      IF AVAIL transporte THEN
          ASSIGN c-cgc-transp = transporte.cgc.
      ELSE
          ASSIGN c-cgc-transp = "".

    IF adm-new-record = YES THEN DO:
        ASSIGN c-alterado = "I".
    END.
    ELSE DO: /* alteracao */
        IF AVAIL ordem-carregamento AND
          (/*ordem-carregamento.capacidade    <> INPUT FRAME {&FRAME-NAME} ordem-carregamento.capacidade    OR */
           ordem-carregamento.cnh           <> INPUT FRAME {&FRAME-NAME} ordem-carregamento.cnh           OR
           ordem-carregamento.nome-transp   <> INPUT FRAME {&FRAME-NAME} ordem-carregamento.nome-transp   OR
           ordem-carregamento.placa-carreta <> INPUT FRAME {&FRAME-NAME} ordem-carregamento.placa-carreta OR
           ordem-carregamento.placa-veiculo <> INPUT FRAME {&FRAME-NAME} ordem-carregamento.placa-veiculo) THEN DO:
            ASSIGN c-alterado = "A".
        END.
    END.

    ASSIGN c_nr_cartao_pedagio = ordem-carregamento.nr_cartao_pedagio:SCREEN-VALUE IN FRAME {&FRAME-NAME}.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    IF RETURN-VALUE = 'ADM-ERROR':U THEN  
        RETURN 'ADM-ERROR':U.    
    
    /*:T Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */
    IF c-alterado <> "" THEN
       ASSIGN ordem-carregamento.reg_alterado = c-alterado.

    if not can-find(first tt-motorista) then do:
  
        for each GUU010 no-lock where
                 GUU010.GUU_SIT = '1':
            create tt-motorista.
            assign tt-motorista.CDMTR  = GUU010.GUU_CDMTR
                   tt-motorista.CRPEDG = GUU010.GUU_CRPEDG
                   tt-motorista.ESTCNH = GUU010.GUU_ESTCNH
                   tt-motorista.FILIAL = GUU010.GUU_FILIAL
                   tt-motorista.IDFED  = GUU010.GUU_IDFED
                   tt-motorista.MUNCNH = GUU010.GUU_MUNCNH
                   tt-motorista.NMMTR  = GUU010.GUU_NMMTR
                   tt-motorista.NUMCNH = GUU010.GUU_NUMCNH
                   tt-motorista.RG     = GUU010.GUU_RG.
        end.
    end.

    if not can-find(first tt-placa-veiculo) then do:
        for each GV3010 no-lock:
       
            create tt-aux.
            assign tt-aux.cod-tp = GV3010.GV3_CDTPVC
                   tt-aux.des-tp = GV3010.GV3_DSTPVC.
       
        end.
    
        for each GU8010 no-lock where
                 GU8010.GU8_SIT = '1':
    
            find first tt-aux where
                       tt-aux.cod-tp = GU8010.GU8_CDTPVC no-error.
    
            create tt-placa-veiculo.
            assign tt-placa-veiculo.cod-tipo-veic = GU8010.GU8_CDTPVC 
                   tt-placa-veiculo.cod-veic      = GU8010.GU8_CDVEIC
                   tt-placa-veiculo.desc-tp-veic  = if avail tt-aux then tt-aux.des-tp else ''
                   tt-placa-veiculo.cod-placa     = GU8010.GU8_PLACA 
                   tt-placa-veiculo.uf-placa      = GU8010.GU8_UFPLAC.
        end.
    end.

    FIND FIRST tt-motorista NO-LOCK
         WHERE tt-motorista.NUMCNH = ordem-carregamento.cnh:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-ERROR.
    IF AVAIL tt-motorista THEN DO:
           
       ASSIGN c-replace-rg  = REPLACE(REPLACE(tt-motorista.RG, ".", ""), "-", "")
              c-replace-cpf = REPLACE(REPLACE(tt-motorista.IDFED, ".", ""), "-", "").
     
       ASSIGN OVERLAY(ordem-carregamento.char-2,1,60)  = tt-motorista.NMMTR
              OVERLAY(ordem-carregamento.char-2,61,12) = c-replace-rg  /*rg*/
              OVERLAY(ordem-carregamento.char-2,73,16) = c-replace-cpf  /*cpf*/       
              OVERLAY(ordem-carregamento.char-2,99,15) = tt-motorista.MUNCNH /*este overlay vem depois do tipo-veiculo*/
              OVERLAY(ordem-carregamento.char-2,114,2) = tt-motorista.ESTCNH.
              
    END.
    
    FIND FIRST tt-placa-veiculo 
         WHERE tt-placa-veiculo.cod-placa = ordem-carregamento.placa-veiculo:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-ERROR.
    IF AVAIL tt-placa-veiculo THEN DO:
       ASSIGN OVERLAY(ordem-carregamento.char-2,116,2) = tt-placa-veiculo.uf-placa.    
    END. 
              
    ASSIGN ordem-carregamento.nr_cartao_pedagio = c_nr_cartao_pedagio
           OVERLAY(ordem-carregamento.char-2,89,10) = c-cd-tipo-veiculo:SCREEN-VALUE IN FRAME {&FRAME-NAME} .
   
    ASSIGN c-numero-ordem-add = "".
    
    validate ordem-carregamento.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                            */
    
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .
       
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    disable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cod-placa AS CHARACTER   NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAIL ordem-carregamento THEN DO:
      FIND FIRST tt-motorista 
           WHERE tt-motorista.NUMCNH = ordem-carregamento.cnh NO-LOCK NO-ERROR.
      IF AVAIL tt-motorista THEN
          ASSIGN fi_nome_motorista  :SCREEN-VALUE IN FRAME {&FRAME-NAME} = tt-motorista.NMMTR 
                 fi_cidade_motorista:SCREEN-VALUE IN FRAME {&FRAME-NAME} = tt-motorista.MUNCNH
                 fi_estado_motorista:SCREEN-VALUE IN FRAME {&FRAME-NAME} = tt-motorista.ESTCNH
                 ordem-carregamento.NR_CARTAO_PEDAGIO:SCREEN-VALUE IN FRAME {&FRAME-NAME} = tt-motorista.CRPEDG.
      ELSE 
          ASSIGN fi_nome_motorista  :SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
                 fi_cidade_motorista:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
                 fi_estado_motorista:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
                 ordem-carregamento.NR_CARTAO_PEDAGIO:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

      FIND FIRST tt-tp-operacao NO-LOCK
           WHERE tt-tp-operacao.CDTPOP = INPUT FRAME {&FRAME-NAME} ordem-carregamento.tp-operacao NO-ERROR.
      IF AVAIL tt-tp-operacao THEN
          ASSIGN fi-desc-tp-operacao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = tt-tp-operacao.DSTPOP.
      ELSE
          ASSIGN fi-desc-tp-operacao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

      FIND FIRST transporte 
           WHERE transporte.nome-abrev = ordem-carregamento.nome-transp NO-LOCK NO-ERROR.
      IF AVAIL transporte THEN
          ASSIGN fi_desc_transportador:SCREEN-VALUE IN FRAME {&FRAME-NAME} = transporte.nome.
      ELSE
          ASSIGN fi_desc_transportador:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
          
      FIND FIRST transporte
           WHERE transporte.cod-transp = INPUT FRAME {&FRAME-NAME} ordem-carregamento.cod-redespachante NO-LOCK NO-ERROR.
      IF AVAIL transporte THEN
          ASSIGN fi_desc_redespachante:SCREEN-VALUE IN FRAME {&FRAME-NAME} = transporte.nome.
      ELSE
          ASSIGN fi_desc_redespachante:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".    

      FIND FIRST emitente NO-LOCK
           WHERE emitente.cod-emitente = int(ordem-carregamento.cod-destinatario:SCREEN-VALUE IN FRAME {&FRAME-NAME})  NO-ERROR.
      IF AVAIL emitente THEN
           ASSIGN c-desc-destinatario:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-abrev.
      ELSE
           ASSIGN c-desc-destinatario:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
       
      /*FIND FIRST tf-cliente 
          WHERE tf-cliente.nr-tabela-frete = int(ordem-carregamento.nr-tabela-frete:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
      IF AVAIL tf-cliente THEN
         ASSIGN c-desc-tab-frete:SCREEN-VALUE IN FRAME {&FRAME-NAME} = tf-cliente.ds-tabela-frete.
      ELSE
         ASSIGN c-desc-tab-frete:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".*/

/*       FIND FIRST veiculo WHERE veiculo.placa-veiculo = ordem-carregamento.placa-veiculo NO-LOCK NO-ERROR.      */
/*       IF AVAIL veiculo THEN DO:                                                                                */
/*                                                                                                                */
/*            FIND FIRST esp-tipo-veiculo                                                                         */
/*                WHERE esp-tipo-veiculo.cd-tipo-veiculo = veiculo.cd_tp_veiculo NO-LOCK NO-ERROR.                */
/*            IF AVAIL esp-tipo-veiculo THEN                                                                      */
/*               ASSIGN c-cd-tipo-veiculo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = esp-tipo-veiculo.cd-tipo-veiculo  */
/*                      c-ds-tipo-veiculo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = esp-tipo-veiculo.ds-tipo-veiculo. */
/*            ELSE                                                                                                */
/*               ASSIGN c-cd-tipo-veiculo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""                                */
/*                      c-ds-tipo-veiculo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".                               */
/*       END.                                                                                                     */

            
         ASSIGN cod-placa = "".
         IF ordem-carregamento.placa-carreta-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" THEN DO:

            ASSIGN cod-placa = ordem-carregamento.placa-carreta-2:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
         END.
         ELSE IF ordem-carregamento.placa-carreta:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" THEN DO:

            ASSIGN cod-placa = ordem-carregamento.placa-carreta:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
         END.
         ELSE IF ordem-carregamento.placa-veiculo:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> ""  THEN DO:

            ASSIGN cod-placa = ordem-carregamento.placa-veiculo:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
         END.
        
         FIND FIRST tt-placa-veiculo 
              WHERE tt-placa-veiculo.cod-placa = cod-placa NO-ERROR.
         IF AVAIL tt-placa-veiculo THEN DO:
             ASSIGN c-cd-tipo-veiculo = string(tt-placa-veiculo.cod-tipo-veic)
                    c-ds-tipo-veiculo = tt-placa-veiculo.desc-tp-veic.
         END.
         ELSE DO:
             ASSIGN c-cd-tipo-veiculo = ""
                    c-ds-tipo-veiculo = "".
         END.
         DISPLAY c-cd-tipo-veiculo c-ds-tipo-veiculo with frame {&FRAME-NAME}.
  END.

  FIND esp-local
        WHERE esp-local.cod-local = INPUT FRAME {&FRAME-NAME} ordem-carregamento.cod-local NO-LOCK NO-ERROR.
    IF AVAIL esp-local THEN
        ASSIGN c-desc-local:SCREEN-VALUE IN FRAME {&FRAME-NAME} = esp-local.desc-local.
    ELSE
        ASSIGN c-desc-local:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

  /*
  RUN pi-getOrdem IN h-handle-v01 (OUTPUT c-numero-ordem).
  */
  ASSIGN c-numero-ordem-add = "".
  RUN pi-atualiza-pedagio IN THIS-PROCEDURE (INPUT "").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    if adm-new-record = yes then
        enable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  define variable cURL as character no-undo.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  /*------ Procedure de Conex∆o do WebService*/

  empty temp-table tt-motorista.  
  
  for each GUU010 no-lock where
           GUU010.GUU_SIT = '1':
      create tt-motorista.
      assign tt-motorista.CDMTR  = GUU010.GUU_CDMTR
             tt-motorista.CRPEDG = GUU010.GUU_CRPEDG
             tt-motorista.ESTCNH = GUU010.GUU_ESTCNH
             tt-motorista.FILIAL = GUU010.GUU_FILIAL
             tt-motorista.IDFED  = GUU010.GUU_IDFED
             tt-motorista.MUNCNH = GUU010.GUU_MUNCNH
             tt-motorista.NMMTR  = GUU010.GUU_NMMTR
             tt-motorista.NUMCNH = GUU010.GUU_NUMCNH
             tt-motorista.RG     = GUU010.GUU_RG.
  end.


  empty temp-table tt-placa-veiculo.
  empty temp-table tt-aux.

  for each GV3010 no-lock:
  
      create tt-aux.
      assign tt-aux.cod-tp = GV3010.GV3_CDTPVC
             tt-aux.des-tp = GV3010.GV3_DSTPVC.
  
  end.

  for each GU8010 no-lock where
           GU8010.GU8_SIT = '1':

      find first tt-aux where
                 tt-aux.cod-tp = GU8010.GU8_CDTPVC no-error.

      create tt-placa-veiculo.
      assign tt-placa-veiculo.cod-tipo-veic = GU8010.GU8_CDTPVC 
             tt-placa-veiculo.cod-veic      = GU8010.GU8_CDVEIC
             tt-placa-veiculo.desc-tp-veic  = if avail tt-aux then tt-aux.des-tp else ''
             tt-placa-veiculo.cod-placa     = GU8010.GU8_PLACA 
             tt-placa-veiculo.uf-placa      = GU8010.GU8_UFPLAC.
  end.

  empty temp-table tt-tp-operacao.

  for each GV4010 no-lock where
           GV4010.GV4_SIT = '1':

      if GV4010.GV4_CDTPOP = 'ISENTO' then
          next.

      create tt-tp-operacao.
      assign tt-tp-operacao.CDTPOP = int(GV4010.GV4_CDTPOP)
             tt-tp-operacao.DSTPOP = GV4010.GV4_DSTPOP.
  end.

  
  /*RUN Conecta(INPUT NO).

  /*Busca os c¢digos das placas no GFE*/
  IF l-WSConectado THEN DO:
           EMPTY TEMP-TABLE tt-XML.
           EMPTY TEMP-TABLE tt-placa-veiculo.
           
           ASSIGN SINCONSVEICULO      = ""
                  MTCONSVEICULORESULT = "".      
                  
           FIND FIRST tab-generica EXCLUSIVE-LOCK
                WHERE tab-generica.utilizacao = "WS-ESPEC-NOBLE":U NO-ERROR.
           IF AVAILABLE tab-generica THEN DO:
              ASSIGN cURL = substring(tab-generica.char-2,46,50).         
           END.      
                                                      
           ASSIGN SINCONSVEICULO = "<ns0:MTCONSVEICULO xmlns:ns0='http://" + ENTRY(3,cURL,"/") + "/'>" 
                  SINCONSVEICULO = SINCONSVEICULO + "<ns0:SINCONSVEICULO>" 
                  SINCONSVEICULO = SINCONSVEICULO + "<ns0:PLACA></ns0:PLACA>" 
                  SINCONSVEICULO = SINCONSVEICULO + "</ns0:SINCONSVEICULO>" 
                  SINCONSVEICULO = SINCONSVEICULO + "</ns0:MTCONSVEICULO>".    
        
            
           RUN MTCONSVEICULO IN hWSSERVICOSNOBLESOAP(INPUT SINCONSVEICULO, OUTPUT MTCONSVEICULORESULT).
           RUN pi-LerXML(INPUT MTCONSVEICULORESULT).

           ASSIGN i-cont = 1.

           CREATE tt-placa-veiculo.
           FOR EACH tt-XML BREAK BY tt-XML.tt-seq:
               
               CASE tt-XML.tt-Field:
                   WHEN "CDTPVC" THEN /*codigo do tipo de veiculo*/
                       ASSIGN tt-placa-veiculo.cod-tipo-veic = tt-XML.tt-Value.
                   WHEN "CDVEIC"  THEN /*codigo de veiculo*/
                       ASSIGN tt-placa-veiculo.cod-veic = tt-XML.tt-Value.
                   WHEN "DSTPVC" THEN  /*descricao do tipo de veiculo*/
                       ASSIGN tt-placa-veiculo.desc-tp-veic = tt-XML.tt-Value. 
                   WHEN "PLACA" THEN   /*placa*/
                       ASSIGN tt-placa-veiculo.cod-placa = tt-XML.tt-Value.
                   WHEN "UFPLAC" THEN  /*uf da placa*/
                       ASSIGN tt-placa-veiculo.uf-placa = tt-XML.tt-Value. 
               END CASE.

               IF i-cont MOD 5 = 0 AND NOT LAST(tt-XML.tt-seq) THEN DO:
                  RELEASE tt-placa-veiculo.
                  CREATE  tt-placa-veiculo.
               END.
               ASSIGN i-cont = i-cont + 1.
           END.

           RUN piCarrega-ttMotorista(INPUT "").

           RUN piCarrega-ttTipoOperacao(INPUT "").
  END.*/
  
  /********************  Ao inicializar carrega o tipo de ve°culo em tela  ********************/
  IF AVAIL ordem-carregamento THEN DO:

      IF SUBSTRING(ordem-carregamento.char-2,89,10) <> "" THEN DO:
        
          ASSIGN c-cd-tipo-veiculo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SUBSTRING(ordem-carregamento.char-2,89,10).
                 
          FIND FIRST tt-placa-veiculo 
               WHERE tt-placa-veiculo.cod-tipo-veic = SUBSTRING(ordem-carregamento.char-2,89,10) NO-ERROR.
          IF AVAIL tt-placa-veiculo THEN
             ASSIGN c-ds-tipo-veiculo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = tt-placa-veiculo.desc-tp-veic.
      END.

      FIND FIRST tt-tp-operacao NO-LOCK
           WHERE tt-tp-operacao.CDTPOP = ordem-carregamento.tp-operacao NO-ERROR.
      IF AVAIL tt-tp-operacao THEN
         ASSIGN fi-desc-tp-operacao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = tt-tp-operacao.DSTPOP.
  END.
 
  /* Code placed here will execute AFTER standard behavior.    */
/*   ASSIGN ordem-carregamento.capacidade:FORMAT IN FRAME {&FRAME-NAME}      = ">>,>>9.99". */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-parent V-table-Win 
PROCEDURE pi-atualiza-parent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define input parameter v-row-parent-externo as rowid no-undo.
    
    assign v-row-parent = v-row-parent-externo.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-pedagio V-table-Win 
PROCEDURE pi-atualiza-pedagio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER c-numero-ordem-placa AS CHAR NO-UNDO.

    DEFINE VAR d-vl-pedagio AS DEC NO-UNDO.

    FIND FIRST tt-placa-veiculo WHERE tt-placa-veiculo.cod-placa = INPUT FRAME {&FRAME-NAME} ordem-carregamento.placa-veiculo NO-LOCK NO-ERROR.
    IF AVAIL tt-placa-veiculo THEN DO:
        /**************  calculo pedagio ***************/        
        ASSIGN d-vl-pedagio = 0.

        IF c-numero-ordem-add = "" AND AVAIL ordem-carregamento THEN
            ASSIGN c-numero-ordem = string(ordem-carregamento.numero-ordem).
        ELSE 
            ASSIGN c-numero-ordem = c-numero-ordem-add.

        IF c-numero-ordem-placa <> "" THEN
            ASSIGN c-numero-ordem = c-numero-ordem-placa.


        FIND FIRST embarque NO-LOCK WHERE
            embarque.nr-embarque = 0 AND
            embarque.cdd-embarq = DEC(c-numero-ordem) NO-ERROR.

        ASSIGN c-numero-ordem-placa-veic = c-numero-ordem.

        IF AVAIL embarque THEN DO:

            FOR EACH pre-fatur OF embarque NO-LOCK,
                EACH ped-venda OF pre-fatur NO-LOCK: 
            
                IF ped-venda.cidade-cif = "" THEN NEXT. /* FOB */

                ASSIGN c-cidade-destino:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ped-venda.cidade
                       c-uf-destino:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ped-venda.estado.
            END. /* FOR EACH pre-fatur, each ped-venda */                                            
        END. /* AVAIL embarque */

      /*  IF AVAIL esp-tipo-veiculo THEN
            ASSIGN d-vl-pedagio = d-vl-pedagio * esp-tipo-veiculo.qt-fator-pedagio.*/

        ASSIGN ordem-carregamento.vl_pedagio:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(d-vl-pedagio).

        IF INPUT FRAME {&FRAME-NAME} ordem-carregamento.vl_pedagio = 0 OR 
           INPUT FRAME {&FRAME-NAME} ordem-carregamento.vl_pedagio = ? THEN DO:

            FIND CURRENT ordem-carregamento EXCLUSIVE-LOCK NO-ERROR.

            IF AVAIL ordem-carregamento THEN
                ASSIGN ordem-carregamento.vl_pedagio = d-vl-pedagio.

            FIND CURRENT ordem-carregamento NO-LOCK NO-ERROR.
        END.
        /*********************************************/
    END. /* AVAIL veiculo */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-recebe-objeto V-table-Win 
PROCEDURE pi-recebe-objeto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-handle-v01 AS HANDLE NO-UNDO.

    ASSIGN h-handle-v01 = p-handle-v01.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-recebe-value V-table-Win 
PROCEDURE pi-recebe-value :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER p-numero-ordem AS CHAR.
       


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate V-table-Win 
PROCEDURE Pi-validate :
/*:T------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: N∆o fazer assign aqui. Nesta procedure
  devem ser colocadas apenas validaá‰es, pois neste ponto do programa o registro 
  ainda n∆o foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /*:T Validaá∆o de dicion†rio */
    
/*:T    Segue um exemplo de validaá∆o de programa */
/*       find tabela where tabela.campo1 = c-variavel and               */
/*                         tabela.campo2 > i-variavel no-lock no-error. */
      
      /*:T Este include deve ser colocado sempre antes do ut-msgs.p */
/*       {include/i-vldprg.i}                                             */
/*       run utp/ut-msgs.p (input "show":U, input 7, input return-value). */
/*       return 'ADM-ERROR':U.                                            */

    FIND FIRST esp-bloqueio NO-LOCK
        WHERE (esp-bloqueio.cnh = INPUT FRAME {&FRAME-NAME} ordem-carregamento.cnh
               OR esp-bloqueio.cnh = c-cgc-transp) 
           AND esp-bloqueio.dt-bloq-fin >= TODAY NO-ERROR.
    IF AVAIL esp-bloqueio THEN DO:
        IF esp-bloqueio.cnh = INPUT FRAME {&FRAME-NAME} ordem-carregamento.cnh THEN
            RUN utp/ut-msgs.p (INPUT "show",
                               INPUT 17006,
                               INPUT "Motorista  Bloqueado" +
                                     "~~" +
                                     "Motorista encontram-se bloqueado.").
        ELSE
            RUN utp/ut-msgs.p (INPUT "show",
                               INPUT 17006,
                               INPUT "Transportador Bloqueado" +
                                     "~~" +
                                     "Transportador encontram-se bloqueado.").
        RETURN 'ADM-ERROR':U.
    END.

    IF adm-new-record = YES THEN DO:
        FIND FIRST bf-ordem-carregamento
            WHERE bf-ordem-carregamento.placa-veiculo = INPUT FRAME {&FRAME-NAME} ordem-carregamento.placa-veiculo
              AND bf-ordem-carregamento.id_status <> 6 NO-LOCK NO-ERROR.
        IF AVAIL bf-ordem-carregamento THEN DO:
            RUN zep\zeutp\ze-erro.p (INPUT 1,
                                 INPUT "Placa informada j† em uso na OC " + STRING(bf-ordem-carregamento.numero-ordem), 
                                 INPUT "Placa informada j† em uso em outro carregamento com ordem em aberto n£mero " + STRING(bf-ordem-carregamento.numero-ordem)).
            RETURN 'ADM-ERROR':U.
        END.
    END.

    IF adm-new-record = YES THEN DO:
        FIND FIRST bf-ordem-carregamento
            WHERE bf-ordem-carregamento.cnh = INPUT FRAME {&FRAME-NAME} ordem-carregamento.cnh
              AND bf-ordem-carregamento.id_status <> 6 NO-LOCK NO-ERROR.
        IF AVAIL bf-ordem-carregamento THEN DO:
            RUN zep\zeutp\ze-erro.p (INPUT 1,
                                 INPUT "CNH informada j† em uso na OC " + STRING(bf-ordem-carregamento.numero-ordem), 
                                 INPUT "CNH informada j† em uso em outro carregamento com ordem em aberto n£mero " + STRING(bf-ordem-carregamento.numero-ordem)).
            RETURN 'ADM-ERROR':U.
        END.
    END.

    IF NOT CAN-FIND(FIRST tt-tp-operacao 
                    WHERE tt-tp-operacao.CDTPOP = INPUT FRAME {&FRAME-NAME} ordem-carregamento.tp-operacao NO-LOCK) THEN DO:

        RUN zep\zeutp\ze-erro.p (INPUT 1,
                                 INPUT "Tipo de Operaá∆o n∆o cadastrado.",
                                 INPUT "O Tipo de Operaá∆o informado n∆o esta cadastrado no sistema.").
        RETURN 'ADM-ERROR':U.
    END.

    IF NOT CAN-FIND(FIRST tt-motorista 
                    WHERE tt-motorista.NUMCNH = INPUT FRAME {&FRAME-NAME} ordem-carregamento.cnh NO-LOCK) THEN DO:

        RUN zep\zeutp\ze-erro.p (INPUT 1,
                                 INPUT "Motorista n∆o cadastrado.", 
                                 INPUT "O motorista informado n∆o esta cadastrado no sistema.").
        RETURN 'ADM-ERROR':U.
    END.
    
    IF NOT CAN-FIND(FIRST transporte WHERE transporte.nome-abrev = INPUT FRAME {&FRAME-NAME} ordem-carregamento.nome-transp NO-LOCK) THEN DO:
        RUN zep\zeutp\ze-erro.p (INPUT 1,
                             INPUT "Transportador n∆o cadastrado.", 
                             INPUT "O transportador informado n∆o esta cadastrado no sistema.").
        RETURN 'ADM-ERROR':U.
    END.

    IF  NOT CAN-FIND(FIRST tt-placa-veiculo
                     WHERE tt-placa-veiculo.cod-placa = REPLACE(INPUT FRAME {&FRAME-NAME} ordem-carregamento.placa-veiculo,"-","") NO-LOCK) THEN DO: 
    
             RUN zep\zeutp\ze-erro.p (INPUT 1, 
                                      INPUT "Veiculo n∆o cadastrado com a placa informada.", 
                                      INPUT "N∆o foi encontrado nenhum veiculo cadastrado no sistema com a placa informada."). 
             RETURN 'ADM-ERROR':U. 
    END.
    
    IF REPLACE(INPUT FRAME {&FRAME-NAME} ordem-carregamento.placa-carreta,"-","") <> "" AND
       NOT CAN-FIND(FIRST tt-placa-veiculo
                    WHERE tt-placa-veiculo.cod-placa = REPLACE(INPUT FRAME {&FRAME-NAME} ordem-carregamento.placa-carreta,"-","") NO-LOCK) THEN DO: 
    
             RUN zep\zeutp\ze-erro.p (INPUT 1, 
                                      INPUT "Veiculo n∆o cadastrado com a placa carreta(1) informada.", 
                                      INPUT "N∆o foi encontrado nenhum veiculo cadastrado no sistema com a placa informada."). 
             RETURN 'ADM-ERROR':U. 
    END.
    
    IF REPLACE(INPUT FRAME {&FRAME-NAME} ordem-carregamento.placa-carreta-2,"-","") <> "" AND
       NOT CAN-FIND(FIRST tt-placa-veiculo
                    WHERE tt-placa-veiculo.cod-placa = REPLACE(INPUT FRAME {&FRAME-NAME} ordem-carregamento.placa-carreta-2,"-","") NO-LOCK) THEN DO: 
    
             RUN zep\zeutp\ze-erro.p (INPUT 1, 
                                      INPUT "Veiculo n∆o cadastrado com a placa carreta(2) informada.", 
                                      INPUT "N∆o foi encontrado nenhum veiculo cadastrado no sistema com a placa informada."). 
             RETURN 'ADM-ERROR':U. 
    END.   

    IF INPUT FRAME {&FRAME-NAME} ordem-carregamento.cod-local <> "0" THEN DO:
        FIND esp-local
            WHERE esp-local.cod-local = INPUT FRAME {&FRAME-NAME} ordem-carregamento.cod-local NO-LOCK NO-ERROR.
        IF NOT AVAIL esp-local THEN DO:
            RUN zep\zeutp\ze-erro.p (INPUT 1,
                                     INPUT "C¢digo local informado n∆o cadastrado.", 
                                     INPUT "C¢digo local informado n∆o cadastrado no programa ESCD001.").
            RETURN 'ADM-ERROR':U.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "ordem-carregamento"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE busca-tipo-veiculo:
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
    IF ordem-carregamento.placa-veiculo:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" AND
       ordem-carregamento.placa-carreta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""  AND 
       ordem-carregamento.placa-carreta-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN DO:

                FIND FIRST tt-placa-veiculo
                     WHERE tt-placa-veiculo.cod-placa = ordem-carregamento.placa-veiculo:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-ERROR.
                IF AVAIL tt-placa-veiculo THEN DO:
                      ASSIGN c-cd-tipo-veiculo = string(tt-placa-veiculo.cod-tipo-veic)
                             c-ds-tipo-veiculo = tt-placa-veiculo.desc-tp-veic.
                END.
                ELSE DO:
                      ASSIGN c-cd-tipo-veiculo = ""
                             c-ds-tipo-veiculo = "".
                END.
    END.
    ELSE DO:
       IF ordem-carregamento.placa-veiculo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" AND
          ordem-carregamento.placa-carreta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" AND
          ordem-carregamento.placa-carreta-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN DO:

           ASSIGN c-cd-tipo-veiculo = ""
                  c-ds-tipo-veiculo = "".
       END.
    END.

    DISPLAY c-cd-tipo-veiculo c-ds-tipo-veiculo with frame {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE busca-tipo-carreta-1:
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
    IF ordem-carregamento.placa-carreta:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> ""  AND
       ordem-carregamento.placa-carreta-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN DO:

         FIND FIRST tt-placa-veiculo 
              WHERE tt-placa-veiculo.cod-placa = ordem-carregamento.placa-carreta:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-ERROR.
         IF AVAIL tt-placa-veiculo THEN DO:
            ASSIGN c-cd-tipo-veiculo = string(tt-placa-veiculo.cod-tipo-veic)
                   c-ds-tipo-veiculo = tt-placa-veiculo.desc-tp-veic.
         END.
         ELSE DO:
            ASSIGN c-cd-tipo-veiculo = ""
                   c-ds-tipo-veiculo = "".
         END.
    END.
    ELSE DO:
       IF ordem-carregamento.placa-veiculo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" AND
          ordem-carregamento.placa-carreta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" AND
          ordem-carregamento.placa-carreta-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN DO:

          ASSIGN c-cd-tipo-veiculo = ""
                 c-ds-tipo-veiculo = "".
       END.
    END.

    display c-cd-tipo-veiculo c-ds-tipo-veiculo with frame {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE busca-tipo-carreta-2:
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/

    IF ordem-carregamento.placa-carreta-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" THEN DO:

            FIND FIRST tt-placa-veiculo 
                 WHERE tt-placa-veiculo.cod-placa = ordem-carregamento.placa-carreta-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-ERROR.
            IF AVAIL tt-placa-veiculo THEN DO:
               ASSIGN c-cd-tipo-veiculo = string(tt-placa-veiculo.cod-tipo-veic)
                      c-ds-tipo-veiculo = tt-placa-veiculo.desc-tp-veic.
            END.
            ELSE DO:
               ASSIGN c-cd-tipo-veiculo = ""
                      c-ds-tipo-veiculo = "".
            END.
    END.
    ELSE DO:
            IF ordem-carregamento.placa-veiculo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" AND
               ordem-carregamento.placa-carreta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" AND
               ordem-carregamento.placa-carreta-2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN DO:

               ASSIGN c-cd-tipo-veiculo = ""
                      c-ds-tipo-veiculo = "".
            END.
    END.

    display c-cd-tipo-veiculo c-ds-tipo-veiculo with frame {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE piCarrega-ttTipoOperacao:
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-tp-operacao AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cURL AS CHARACTER   NO-UNDO.

    EMPTY TEMP-TABLE tt-XML.
    EMPTY TEMP-TABLE tt-tp-operacao.
           
    ASSIGN SINCONSTPOP      = ""
           MTCONSTPOPRESULT = "".

    FIND FIRST tab-generica EXCLUSIVE-LOCK
         WHERE tab-generica.utilizacao = "WS-ESPEC-NOBLE":U NO-ERROR.
    IF AVAILABLE tab-generica THEN DO:
       ASSIGN cURL = substring(tab-generica.char-2,46,50).         
    END.  

    ASSIGN SINCONSTPOP = "<ns0:MTCONSTPOP xmlns:ns0='http://" + ENTRY(3,cURL,"/") + "/'>"
           SINCONSTPOP = SINCONSTPOP + "<ns0:SINCONSTPOP>"
           SINCONSTPOP = SINCONSTPOP + "<ns0:CDTPOP> + p-tp-operacao + </ns0:CDTPOP>"
           SINCONSTPOP = SINCONSTPOP + "</ns0:SINCONSTPOP>"
           SINCONSTPOP = SINCONSTPOP + "</ns0:MTCONSTPOP>".

    RUN MTCONSTPOP IN hWSSERVICOSNOBLESOAP(INPUT SINCONSTPOP, OUTPUT MTCONSTPOPRESULT).
    RUN pi-LerXML(INPUT MTCONSTPOPRESULT).
  
    ASSIGN i-cont = 1.
  
    CREATE tt-tp-operacao.
    FOR EACH tt-XML BREAK BY tt-XML.tt-seq:  
                                             
        CASE tt-XML.tt-Field:
            WHEN "CDTPOP" THEN /*codigo do tipo de operaá∆o*/
                ASSIGN tt-tp-operacao.CDTPOP = int(tt-XML.tt-Value).
            WHEN "DSTPOP"  THEN /*descriá∆o tipo de operaá∆o*/
                ASSIGN tt-tp-operacao.DSTPOP = tt-XML.tt-Value.
        END CASE.

        IF i-cont MOD 2 = 0 AND NOT LAST(tt-XML.tt-seq) THEN DO:
           RELEASE tt-tp-operacao.
           CREATE  tt-tp-operacao.
        END.
        ASSIGN i-cont = i-cont + 1.
    END.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



