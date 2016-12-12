/*************************************************
** Programa   : upc/upc-cd0401.p                **
** Data       : Novembro/2016                   **
** Autor      : Joao Pacheco - SZ               **
** Descriá∆o  : Novo Campo Vencimento Duplicata **
**                                              **
** Alteraá∆o  :                                 **
**                                              **
**                                              **
**************************************************/

{utp/ut-glob.i}

def input param p-ind-event  as char          no-undo.
def input param p-ind-object as char          no-undo.
def input param p-wgh-object as handle        no-undo.
def input param p-wgh-frame  as widget-handle no-undo.
def input param p-cod-table  as char          no-undo.
def input param p-row-table  as rowid         no-undo.

def new global shared var h-portador-aux-cd0401 as handle no-undo.

def new global shared var wh-retangulo-vencimento-label as widget-handle no-undo.
def new global shared var wh-retangulo-vencimento       as widget-handle no-undo.
def new global shared var wh-escd0401-radio-set         as widget-handle no-undo.

if p-ind-event            = 'BEFORE-INITIALIZE' and
   p-ind-object           = 'VIEWER'            and
   p-wgh-object:file-name = 'cdp/cd0401-v04.w'  then do:

    create text wh-retangulo-vencimento-label
        assign frame          = p-wgh-frame
               width-chars    = 17
               height-chars   = 0.88
               row            = 1
               col            = 64.3
               format         = "x(20)"
               screen-value   = "Vencimento Duplicata"
               visible        = yes.

    create rectangle wh-retangulo-vencimento
        assign frame         = p-wgh-frame
               width         = 22
               height        = 1.8
               row           = 1.55
               col           = 63.4
               visible       = yes
               sensitive     = yes
               fgcolor       = 7.

    create radio-set wh-escd0401-radio-set
        assign frame          = p-wgh-frame
               width          = 16
               height         = 1.5
               row            = 1.8
               col            = 66.3
               font           = 1
               horizontal     = no
               radio-buttons  = "Data de Emiss∆o,1,Data de Transaá∆o,2"
               sensitive      = no
               visible        = yes.

end.

if p-ind-event            = 'DISPLAY'          and
   p-ind-object           = 'VIEWER'           and
   p-wgh-object:file-name = 'cdp/cd0401-v04.w' then do:

    if valid-handle(p-wgh-frame) then do:
        find emitente no-lock where 
             rowid(emitente) = p-row-table no-error.
        if avail emitente then do:
            find ext-emitente no-lock where 
                 ext-emitente.cod-emitente = emitente.cod-emitente no-error.
            if avail ext-emitente then 
                assign wh-escd0401-radio-set:screen-value = string(ext-emitente.tp-dt-vencto).
            else
                assign wh-escd0401-radio-set:screen-value = string(1).
        end.
    end.

end.

if  p-ind-event            = "ENABLE"           and 
    p-ind-object           = "VIEWER"           and 
    p-wgh-object:file-name = "cdp/cd0401-v04.w" then do:
    
    if valid-handle(wh-escd0401-radio-set) then
        assign wh-escd0401-radio-set:sensitive = yes.

end.

if p-ind-event            = "disable"          and 
   p-ind-object           = "VIEWER"           and 
   p-wgh-object:file-name = "cdp/cd0401-v04.w" then do:

    if valid-handle(wh-escd0401-radio-set) then
        assign wh-escd0401-radio-set:sensitive = no.

end.

if p-ind-event            = "ASSIGN"           and 
   p-ind-object           = "VIEWER"           and 
   p-wgh-object:file-name = "cdp/cd0401-v04.w" then do:

    find emitente no-lock where 
         rowid(emitente) = p-row-table no-error.
    if avail emitente then do:
        find ext-emitente exclusive-lock where 
             ext-emitente.cod-emitente = emitente.cod-emitente no-error.
        if avail ext-emitente then 
            assign ext-emitente.tp-dt-vencto = int(wh-escd0401-radio-set:screen-value). 
        else do:
            create ext-emitente.
            assign ext-emitente.cod-emitente = emitente.cod-emitente
                   ext-emitente.tp-dt-vencto = int(wh-escd0401-radio-set:screen-value).
        end.
    end.

end.
