/**                                                             
*
* PROGRAMA:
* esp/esof002crp.p
*
**/

{include/i-prgvrs.i ESOF002CRP 2.12.00.001}
{utp/ut-glob.i}

DEFINE TEMP-TABLE tt-concilia-fiscal NO-UNDO 
    field cod-emitente like movto-estoq.cod-emitente
    field cod-estabel  like movto-estoq.cod-estabel
    field serie-docto  like movto-estoq.serie-docto
    field nro-docto    like movto-estoq.nro-docto
    field nat-operacao like movto-estoq.nat-operacao 
    field dt-trans     like movto-estoq.dt-trans
    field valor-ce-db as dec
    field valor-ce-cr as dec
    field valor-ft-cr as dec
    field valor-ft-db as dec
    field valor-of-cr as dec
    field valor-of-db as dec
    field valor-dif   as dec.

def input parameter table for tt-concilia-fiscal.

def var arquivoCSV      as char                               no-undo. 
def var arquivoXLS      as char                               no-undo. 
def var chExcel         as com-handle no-undo .
def var chSheet         as com-handle no-undo .
def var chQueryTable    as com-handle no-undo .

def stream st-csv.

assign arquivoCSV = session:temp-directory + "esof002_" + c-seg-usuario + "_" + string(etime) + ".csv".

output stream st-csv to value(arquivoCSV).

for each tt-concilia-fiscal,
   first emitente no-lock
   where emitente.cod-emitente = tt-concilia-fiscal.cod-emitente
    by tt-concilia-fiscal.cod-estab:

    put stream st-csv unform
              tt-concilia-fiscal.cod-estabel  ";"
              tt-concilia-fiscal.cod-emitente ";"
              emitente.nome-abrev             ";"
              tt-concilia-fiscal.nro-docto    ";"
              tt-concilia-fiscal.serie        ";"
              tt-concilia-fiscal.nat-operacao ";"
              tt-concilia-fiscal.dt-trans     ";"
              tt-concilia-fiscal.valor-ce-db  ";"
              tt-concilia-fiscal.valor-ce-cr  ";"
              tt-concilia-fiscal.valor-ft-db  ";"
              tt-concilia-fiscal.valor-ft-cr  ";"
              tt-concilia-fiscal.valor-of-db  ";"
              tt-concilia-fiscal.valor-of-cr  ";"
              tt-concilia-fiscal.valor-dif    skip.
end.

assign arquivoXLS = session:temp-dir + "ESOF002_" + string(etime) + ".xlsx" .

output stream st-csv close.

/*Copia o modelo p/ diretorio temporario*/
os-copy value(search("modelos/esof002.xlsx")) value(arquivoXLS) .

/*Abre o arquivo movido e seta configuracoes*/
create "Excel.Application" chExcel.
chExcel:visible = no . 
chExcel:DisplayAlerts = no .
chExcel:Workbooks:open(arquivoXLS) .
chSheet = chExcel:Sheets:ITEM(1) .


/*Link com .csv*/
chSheet:QueryTables:ADD("TEXT;" + arquivoCSV , chSheet:cells(2 , 1)) .
ASSIGN
    chQueryTable = chSheet:QueryTables(1)
    chQueryTable:FieldNames = false
    chQueryTable:RowNumbers = false
    chQueryTable:FillAdjacentFormulas = false
    chQueryTable:PreserveFormatting = true
    chQueryTable:RefreshOnFileOpen = false
    chQueryTable:RefreshStyle = 0
    chQueryTable:SavePassword = false
    chQueryTable:SaveData = TRUE
    chQueryTable:AdjustColumnWidth = false
    chQueryTable:RefreshPeriod = 0
    chQueryTable:TextFilePromptOnRefresh = false
    chQueryTable:TextFilePlatform = 437
    chQueryTable:TextFileStartRow = 1
    chQueryTable:TextFileParseType = 1
    chQueryTable:TextFileTextQualifier = 2
    chQueryTable:TextFileConsecutiveDelimiter = false
    chQueryTable:TextFileTabDelimiter = false
    chQueryTable:TextFileSemicolonDelimiter = TRUE
    chQueryTable:TextFileCommaDelimiter = false
    chQueryTable:TextFileSpaceDelimiter = false
    chQueryTable:TextFileTrailingMinusNumbers = TRUE.

chQueryTable:REFRESH NO-ERROR .
ASSIGN chQueryTable:BackgroundQuery = false.

/*Fim Excel*/
chExcel:ActiveWorkbook:SAVE() .
chExcel:VISIBLE = YES .
RELEASE OBJECT chQueryTable .
RELEASE OBJECT chSheet. 
RELEASE OBJECT chExcel.

os-delete value(arquivoCSV) .
