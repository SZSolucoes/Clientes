/***********************************************************************************
**       Programa: escm108rp.p
**       Data....: 26/10/2016.
**       Autor...: SZ Solu‡oes
**       Objetivo: Exporta‡Æo/Importa‡Æo de Dados do Acordo Comercial
**       VersÆo: 001 - Desenvolvimento Inicial
************************************************************************************/
{include/i-prgvrs.i ESCM009RP 2.00.00.001}

DEFINE TEMP-TABLE tt-param NO-UNDO
    FIELD destino          AS INTEGER 
    FIELD arquivo          AS CHAR FORMAT "x(35)"
    FIELD usuario          AS CHAR FORMAT "x(12)"
    FIELD data-exec        AS DATE
    FIELD hora-exec        AS INTEGER
    FIELD classifica       AS INTEGER
    FIELD desc-classifica  AS CHAR FORMAT "x(40)"
    FIELD modelo-rtf       AS CHAR FORMAT "x(35)"
    FIELD l-habilitaRtf    AS LOG
    FIELD rs-tipo          AS INT
    FIELD arq-entrada      AS CHAR
    FIELD arq-entrada-2    AS CHAR
    FIELD cod-emitente-ini AS INT
    FIELD cod-emitente-fim AS INT.

DEFINE TEMP-TABLE tt-digita NO-UNDO
    FIELD ordem            AS CHAR FORMAT "x(5)" 
    FIELD exemplo          AS CHAR FORMAT "x(3)" 
    INDEX id ordem exemplo.

DEF TEMP-TABLE tt-raw-digita
   FIELD raw-digita      AS RAW.

DEF STREAM s-imp.
DEF STREAM str-rp.

DEF TEMP-TABLE tt-ext-emitente NO-UNDO
    FIELD cod-emitente       LIKE ext-emitente.cod-emitente
    FIELD nome-abrev         LIKE emitente.nome-abrev
    FIELD acordo-comerc      AS CHAR 
    FIELD vl-max-acordo      LIKE ext-emitente.vl-max-acordo 
    FIELD cod-area           LIKE ext-emitente.cod-area
    FIELD percentual-maximo  LIKE ext-emitente.percentual-maximo
    FIELD id                 AS INTEGER
    INDEX ch-id AS PRIMARY UNIQUE id.

DEF TEMP-TABLE tt-erro NO-UNDO
    FIELD i-sequen AS INT             
    FIELD cd-erro  AS INT
    FIELD mensagem AS CHAR FORMAT "x(100)".

DEF INPUT PARAM raw-param AS RAW NO-UNDO.
DEF INPUT PARAM TABLE FOR tt-raw-digita.

DEFINE VARIABLE h-acomp           AS HANDLE  NO-UNDO.
DEFINE VARIABLE h-eshr-depara     AS HANDLE  NO-UNDO.

DEFINE VARIABLE c-status          AS CHAR FORMAT "x(12)" NO-UNDO.
DEFINE VARIABLE c-diretorio2      AS CHAR      NO-UNDO.
DEFINE VARIABLE i-cont            AS INTEGER   NO-UNDO.
DEFINE VARIABLE i-seq             AS INTEGER   NO-UNDO.
DEFINE VARIABLE c-cabec           AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-acordo          AS LOGICAL     NO-UNDO.

/*----- DEFINICAO DE VARIAVEL GLOBAL -----*/
DEFINE NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR FORMAT "x(12)" NO-UNDO.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

{include/i-rpvar.i}
{include/i-rpout.i &TOFILE=tt-param.arquivo}
{include/i-rpcab.i}

FIND tt-param NO-LOCK  NO-ERROR.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

RUN pi-inicializar IN h-acomp ("Executando...").

VIEW FRAME f-cabec.
VIEW FRAME f-rodape.

FOR EACH tt-erro.
    DELETE tt-erro.
END.

FOR EACH tt-ext-emitente:
   DELETE tt-ext-emitente.
END.


IF tt-param.rs-tipo = 2 THEN DO:

    INPUT STREAM s-imp FROM VALUE(arq-entrada-2).
    
    REPEAT:
        ASSIGN i-cont = i-cont + 1.
        IF i-cont > 1 THEN DO:
            CREATE tt-ext-emitente.
            IMPORT STREAM s-imp DELIMITER ";" tt-ext-emitente.

            ASSIGN tt-ext-emitente.id                = i-cont
                   tt-ext-emitente.cod-emitente      = INT(tt-ext-emitente.cod-emitente)
                   tt-ext-emitente.nome-abrev        = tt-ext-emitente.nome-abrev
                   tt-ext-emitente.acordo-comerc     = tt-ext-emitente.acordo-comerc
                   tt-ext-emitente.vl-max-acordo     = DEC(STRING(tt-ext-emitente.vl-max-acordo,">>>,>>>,>>>,>>>,>>>,>>9.99"))
                   tt-ext-emitente.cod-area          = INT(tt-ext-emitente.cod-area)
                   tt-ext-emitente.percentual-maximo = tt-ext-emitente.percentual-maximo.
                   
        END.
        ELSE
            IMPORT STREAM s-imp DELIMITER ";"  c-cabec.
    END.
    FOR EACH tt-ext-emitente NO-LOCK
        BREAK BY tt-ext-emitente.id:

        FIND emitente NO-LOCK
            WHERE emitente.cod-emitente = tt-ext-emitente.cod-emitente
              AND emitente.identific    = 1 NO-ERROR.

        IF AVAIL emitente THEN DO:

            FIND es-acordo-area NO-LOCK
                WHERE es-acordo-area.cod-area = tt-ext-emitente.cod-area NO-ERROR.

            IF AVAIL es-acordo-area THEN DO:
           
                FIND ext-emitente EXCLUSIVE-LOCK
                    WHERE ext-emitente.cod-emitente = INT(tt-ext-emitente.cod-emitente) NO-ERROR.
                
                IF AVAIL ext-emitente THEN
                    ASSIGN ext-emitente.acordo-comerc     = IF tt-ext-emitente.acordo-comerc = "Sim" THEN YES ELSE NO   
                           ext-emitente.vl-max-acordo     = tt-ext-emitente.vl-max-acordo    
                           ext-emitente.cod-area          = tt-ext-emitente.cod-area         
                           ext-emitente.percentual-maximo = tt-ext-emitente.percentual-maximo
                           ext-emitente.usuario           = c-seg-usuario
                           ext-emitente.data-alter        = TODAY
                           ext-emitente.hora-alter        = STRING(TIME,"HH:MM:SS"). 
                ELSE DO:
                    CREATE ext-emitente.
                    ASSIGN ext-emitente.cod-emitente      = tt-ext-emitente.cod-emitente
                           ext-emitente.acordo-comerc     = IF tt-ext-emitente.acordo-comerc = "Sim" THEN YES ELSE NO    
                           ext-emitente.vl-max-acordo     = tt-ext-emitente.vl-max-acordo    
                           ext-emitente.cod-area          = tt-ext-emitente.cod-area         
                           ext-emitente.percentual-maximo = tt-ext-emitente.percentual-maximo 
                           ext-emitente.usuario           = c-seg-usuario
                           ext-emitente.data-alter        = TODAY
                           ext-emitente.hora-alter        = STRING(TIME,"HH:MM:SS").
                
                END.
            END.
            ELSE DO:
                CREATE tt-erro.
                ASSIGN tt-erro.i-sequen = tt-ext-emitente.id
                       tt-erro.cd-erro  = 17006
                       tt-erro.mensagem = "NÆo foi Encontrado Area Comercial com o C¢digo" + " " + STRING(tt-ext-emitente.cod-area).
            END.
        END.

        ELSE DO:
            CREATE tt-erro.
            ASSIGN tt-erro.i-sequen = tt-ext-emitente.id
                   tt-erro.cd-erro  = 17006
                   tt-erro.mensagem = "NÆo foi Encontrado Emitente com o C¢digo" + " " + STRING(tt-ext-emitente.cod-emitente).
        END.
    END.

    ASSIGN i-seq = 0. 
    IF CAN-FIND(FIRST tt-erro) THEN DO:
        FOR EACH tt-erro:
            ASSIGN i-seq = i-seq + 1.
            IF i-seq = 1 THEN
              put unformatted
              "Seq." AT 1
              "Erro" AT 10
              "Mensagem" AT 20 skip
              tt-erro.i-sequen AT 1
              tt-erro.cd-erro AT 10
              tt-erro.mensagem AT 20.
            ELSE DO:
                put UNFORMATTED 
                tt-erro.i-sequen AT 1
                tt-erro.cd-erro AT 10
                tt-erro.mensagem AT 20 SKIP.
            END.
        END.
    END.
    ELSE DO:
        PUT UNFORMATTED "Dados do Acordo Comercial Importados Com Sussesso!".
    END.
    {include/i-rpclo.i}
    INPUT STREAM s-imp CLOSE.
END.

ELSE DO:

    RUN esp/escm108a.p (INPUT tt-param.arq-entrada,
                              tt-param.cod-emitente-ini,
                              tt-param.cod-emitente-fim).

    PUT UNFORMATTED "Dados do Acordo Comercial Exportados Com Sussesso!" SKIP(2) 
                     "Arquivo Disponivel Em:" + "" + STRING(tt-param.arq-entrada) + "\Export_Import_Acordo_comercial.csv!".

END.

RUN pi-finalizar in h-acomp.

RETURN "OK".



