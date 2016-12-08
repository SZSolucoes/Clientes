/*********************************************************************************
**  
**  Objetivo.: Importacao de XMLs de Notas Fiscais de Entrada
**  Programa.: ESNF002rp.p
**  
*********************************************************************************/

{include/i-prgvrs.i ESNF002RP 2.00.00.000}

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
    FIELD arq-destino      AS CHAR 
    FIELD arq-entrada      AS CHAR.

DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita AS RAW.

DEF INPUT PARAM raw-param AS RAW NO-UNDO.
DEF INPUT PARAM TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

DEF STREAM s-imp.
DEFINE VARIABLE c-arquivo    AS CHARACTER  NO-UNDO EXTENT 3.
DEF VAR c-arq AS CHAR NO-UNDO.

DEF TEMP-TABLE tt-dir
    FIELD diretorio AS CHAR FORMAT "X(50)" LABEL "Diret¢rio"
    FIELD arquivo AS CHAR FORMAT "X(50)" LABEL "Arquivo"
    FIELD l-erro  AS LOG.



DEF VAR v_des_arq AS CHAR                     NO-UNDO.
DEF VAR c-hora    AS CHARACTER FORMAT "x(08)" NO-UNDO.
DEF VAR c-data    AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEF VAR h-doc     AS HANDLE  NO-UNDO.

DEFINE VARIABLE c-arquivo-new AS CHARACTER   NO-UNDO.
def var h-axres002 as handle no-undo. /* Xml v2.0 */
DEF VAR h-acomp    AS HANDLE NO-UNDO.
DEF VAR cNomePasta AS CHARACTER NO-UNDO.
DEF VAR cNomePasta1 AS CHARACTER NO-UNDO.
DEFINE new global SHARED VARIABLE c-newCpfDestino AS CHARACTER NO-UNDO.

{cdp/cdcfgmat.i}
{utp/ut-glob.i}
{cdp/cd0666.i}

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

/*
RUN pi-inicializar IN h-acomp (INPUT "Acessando e-mail..." ).

RUN pi-pop3. */

{utp/ut-liter.i XMLs_Integrando *}
RUN pi-inicializar IN h-acomp (INPUT  RETURN-VALUE ).
/* Busca todos os arquivos do diret¢rio de interface e grava na Temp-Table */

IF substring(tt-param.arq-entrada, LENGTH(tt-param.arq-entrada),1) = "\" OR
   substring(tt-param.arq-entrada, LENGTH(tt-param.arq-entrada),1) = "/" THEN
    ASSIGN tt-param.arq-entrada = substring(tt-param.arq-entrada, 1, LENGTH(tt-param.arq-entrada) - 1).

ASSIGN FILE-INFO:FILE-NAME = tt-param.arq-entrada.
IF FILE-INFO:PATHNAME <> ? THEN DO:
    INPUT STREAM s-imp FROM OS-DIR(tt-param.arq-entrada).
    REPEAT:
        IMPORT STREAM s-imp c-arquivo.
        
        IF c-arquivo[3] <> "F" THEN NEXT.

        IF R-INDEX(c-arquivo[1],"_xml") <> 0 THEN DO:
            ASSIGN c-arquivo-new = tt-param.arq-entrada + "\" + REPLACE(SUBSTRING(c-arquivo[1],1,R-INDEX(c-arquivo[1],"_xml") + 3),"_xml",".xml").
            OS-RENAME VALUE(c-arquivo[2]) VALUE(c-arquivo-new).
            ASSIGN c-arquivo[2] = c-arquivo-new
                   c-arquivo[1] = REPLACE(SUBSTRING(c-arquivo[1],1,R-INDEX(c-arquivo[1],"_xml") + 3),"_xml",".xml").
        END.

        IF R-INDEX(c-arquivo[1],".xml") = 0 THEN DO:
            OS-DELETE VALUE(c-arquivo[2]).
            NEXT.
        END.

        CREATE X-DOCUMENT h-doc.
        h-doc:LOAD("file",c-arquivo[2],NO) NO-ERROR.
        
        IF ERROR-STATUS:GET-NUMBER[1] <> 0 THEN DO:
            OS-DELETE VALUE(c-arquivo[2]).
            NEXT.
        END.
        DELETE OBJECT h-doc.
        
        CREATE tt-dir.
        ASSIGN tt-dir.diretorio = tt-param.arq-entrada
               tt-dir.arquivo = c-arquivo[1]
               tt-dir.l-erro  = NO.
    END.

    INPUT STREAM s-imp CLOSE.    
END.

if  not valid-handle(h-axres002) then
    run adapters\xml\esp\axres002.p PERSISTENT SET h-axres002.

/* Importa cada arquivo do diret¢rio da interface */
FOR EACH tt-dir EXCLUSIVE-LOCK:    
    ASSIGN FILE-INFO:FILE-NAME = tt-dir.diretorio + "\" + tt-dir.arquivo.
    IF FILE-INFO:PATHNAME = ? THEN NEXT.
    IF FILE-INFO:FILE-TYPE = "drw" THEN NEXT.   

    RUN pi-acompanhar IN h-acomp(INPUT STRING(tt-dir.arquivo)).

    run piUpsert in h-axres002 (input FILE-INFO:FILE-NAME). 
    FIND param-estoq NO-LOCK NO-ERROR.
    FOR FIRST estabelec FIELDS (cod-estabel)
        WHERE estabelec.cgc = c-newCpfDestino NO-LOCK:
    END.
    IF AVAIL estabelec THEN
        ASSIGN cNomePasta = estabelec.cod-estabel.
    ELSE 
        cNomePasta   =  param-estoq.estabel-pad.
    ASSIGN cNomePasta = REPLACE(cNomePasta,"\","-")
           cNomePasta = REPLACE(cNomePasta,"/","-").
     
   
    ASSIGN cNomePasta1 = STRING(TODAY)
           cNomePasta1 = REPLACE(cNomePasta1,"\","-")
           cNomePasta1 = REPLACE(cNomePasta1,"/","-").
    OS-CREATE-DIR VALUE (tt-dir.diretorio + "\" + cNomePasta) NO-ERROR.
    /*OS-CREATE-DIR VALUE (tt-dir.diretorio + cNomePasta + "\" + cNomePasta1) NO-ERROR.*/
    OS-COPY VALUE( tt-dir.diretorio + "\" + tt-dir.arquivo) VALUE(tt-dir.diretorio + "\" + cNomePasta).
    
    OS-DELETE VALUE (tt-dir.diretorio + "\" + tt-dir.arquivo).

END. /* FOR EACH tt-dir */

output close.

IF VALID-HANDLE(h-axres002) THEN DO:
    DELETE PROCEDURE h-axres002.
    ASSIGN h-axres002 = ?.
END.

RUN pi-finalizar IN h-acomp.

PROCEDURE pi-outlook:
    DEFINE VARIABLE chOutlook        AS COM-HANDLE  NO-UNDO.
    DEFINE VARIABLE chNameSpace      AS COM-HANDLE  NO-UNDO.
    DEFINE VARIABLE chAnexos         AS COM-HANDLE  NO-UNDO.
    DEFINE VARIABLE chInbox          AS COM-HANDLE  NO-UNDO.
    DEFINE VARIABLE chMailItem       AS COM-HANDLE  NO-UNDO.
    
    DEFINE VARIABLE iNumAnexos       AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iCounter         AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cAttachmentPath  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cAttachmentName  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE lXml             AS LOGICAL     NO-UNDO.
    
    DEFINE VARIABLE iContMail AS INTEGER     NO-UNDO.
    
    CREATE "Outlook.Application" chOutlook.
    
    ASSIGN chNameSpace   = chOutlook:GetNameSpace("MAPI")
           chInbox      = chNameSpace:GetDefaultFolder(6). /*Caixa de Entrada*/
    
    DO iContMail = 1 TO chInbox:items:COUNT:
    
        chMailItem = chInbox:items(iContMail).
        
        ASSIGN chAnexos = chMailItem:Attachments
                iNumAnexos = chAnexos:COUNT
                cAttachmentPath = tt-param.arq-entrada.
    
        ASSIGN lXml = NO.
        DO iCounter = 1 TO iNumAnexos:
            ASSIGN cAttachmentName = chAnexos:Item(iCounter):FILENAME.
    
            IF ENTRY(NUM-ENTRIES(cAttachmentName,"."),cAttachmentName,".") = "xls" THEN DO:
                chAnexos:Item(iCounter):SaveAsFile(cAttachmentPath + cAttachmentName).
    
                ASSIGN lXml = YES.
            END.
        END.
    
        IF lXml THEN
            chMailItem:DELETE().
    END.
    
    RELEASE OBJECT chOutlook.
    RELEASE OBJECT chAnexos.
    RELEASE OBJECT chNameSpace.
    RELEASE OBJECT chInbox    .
    RELEASE OBJECT chMailItem .
    
    ASSIGN chOutlook   = ?
           chAnexos    = ?
           chNameSpace = ?  
           chInbox     = ? 
           chMailItem  = ? .
END.

PROCEDURE pi-pop3:
    DEFINE VARIABLE c-exec   AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE c-file   AS CHARACTER   NO-UNDO EXTENT 3.
    
    ASSIGN c-exec = SEARCH("mail/bin/popclient.exe").
    
    FILE-INFO:FILE-NAME = "mail/config".
    
    INPUT FROM OS-DIR(FILE-INFO:full-pathname).
    REPEAT:
        IMPORT c-file.
    
        IF c-file[3] <> "F" OR ENTRY(NUM-ENTRIES(c-file[1],"."),c-file[1],".") <> "xml" THEN
            NEXT.
        
        DOS SILENT VALUE(c-exec + " -configfile " + c-file[2]).
    END.
    INPUT CLOSE.
END.




