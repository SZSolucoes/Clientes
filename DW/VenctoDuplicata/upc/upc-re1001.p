/***************************************************************
**                                                            **
** Programa: upc/upc-re1001.p                                 **
** Data       : Novembro/2015                                 **
** Autor      : SZ - Joao Pacheco                             **
** Descri‡Æo: Vinculo Placa                                   **
**                                                            **
***************************************************************/

define input parameter p-ind-event   as character      no-undo.
define input parameter p-ind-object  as character      no-undo.
define input parameter p-wgh-object  as handle         no-undo.
define input parameter p-wgh-frame   as widget-handle  no-undo.
define input parameter p-cod-table   as character      no-undo.
define input parameter p-row-table   as rowid          no-undo.

define new global shared var g-row-docum-est-upc as rowid         no-undo.
define new global shared var h-bt-esocial-re1001 as widget-handle no-undo.
define new global shared var h-bt-conf-re1001    as widget-handle no-undo.
define new global shared var h-bt-clone-re1001   as widget-handle no-undo.
define new global shared var wh-bt-placa-re1001  as widget-handle no-undo.
define new global shared var wh-page1            as widget-handle no-undo.

define variable c-objeto    as char no-undo.

if p-wgh-object <> ? then
    assign c-objeto = entry(num-entries(p-wgh-object:file-name,'/'),
                                        p-wgh-object:file-name,'/').

if p-ind-event  = "AFTER-DISPLAY" and
   p-ind-object = "CONTAINER"     then
    assign g-row-docum-est-upc = p-row-table.                                           
                                      
if p-ind-event  = "BEFORE-INITIALIZE" and 
   p-ind-object = "CONTAINER"         and 
   c-objeto     = "re1001"          then do:

    assign wh-page1 = p-wgh-frame:first-child  
           wh-page1 = wh-page1:first-child.

    do while valid-handle(wh-page1):   
        if wh-page1:name = "bt-esocial" then
            assign h-bt-esocial-re1001 = wh-page1.

        if wh-page1:name = "btConf" then
            assign h-bt-conf-re1001 = wh-page1.
 
        assign wh-page1 = wh-page1:next-sibling.
    end.
        
    if valid-handle(h-bt-conf-re1001) then do:

        create button h-bt-clone-re1001
        assign frame             = h-bt-conf-re1001:frame
               width             = h-bt-conf-re1001:width
               height            = h-bt-conf-re1001:height
               label             = h-bt-conf-re1001:label
               column            = h-bt-conf-re1001:column
               row               = h-bt-conf-re1001:row
               convert-3d-colors = h-bt-conf-re1001:convert-3d-colors
               sensitive         = h-bt-conf-re1001:sensitive
               visible           = yes
      
        triggers:
            on 'choose' persistent run upc\upc-re1001.p(input "bt-clone-conf",
                                                        input p-ind-object,
                                                        input p-wgh-object,
                                                        input p-wgh-frame ,
                                                        input p-cod-table ,
                                                        input p-row-table ).
        end triggers.
           
        h-bt-clone-re1001:load-image-up("image\im-sav.bmp":U).
        h-bt-clone-re1001:load-image-insensitive("image\ii-sav.bmp":U).
        h-bt-clone-re1001:move-to-top().

    end.
    
    if valid-handle(h-bt-esocial-re1001) then do:

        assign h-bt-esocial-re1001:columns = h-bt-esocial-re1001:columns + 2.

        create button wh-bt-placa-re1001    
        assign frame     = h-bt-esocial-re1001:frame
               width     = h-bt-esocial-re1001:width
               height    = h-bt-esocial-re1001:height
               label     = "Placa"
               sensitive = yes
               visible   = yes
               name      = "placa"
               col       = h-bt-esocial-re1001:columns - 5
               row       = h-bt-esocial-re1001:row
               height    = h-bt-esocial-re1001:height
               font      = h-bt-esocial-re1001:font
               tooltip   = "Relaciona Placa"
               help      = "Relaciona Placa"
               triggers:
                    on 'choose' persistent run upc\upc-re1001.p(input "bt-placa",
                                                                input p-ind-object,
                                                                input p-wgh-object,
                                                                input p-wgh-frame ,
                                                                input p-cod-table ,
                                                                input p-row-table ).
               end triggers.

        wh-bt-placa-re1001:load-image("image\toolbar\im-carg.bmp").	  
        wh-bt-placa-re1001:load-image-down("image\toolbar\im-carg.bmp").
        wh-bt-placa-re1001:load-image-up("image\toolbar\im-carg.bmp").  
        wh-bt-placa-re1001:load-image-insensitive("image\toolbar\im-carg.bmp").  
        
        wh-bt-placa-re1001:move-after-tab-item(h-bt-esocial-re1001) no-error.  
    end.
end.

if p-ind-event = "bt-placa" then do:

    p-wgh-frame:window:sensitive = no.

    run esp/dwre006.w.    

    p-wgh-frame:window:sensitive = yes.
    
end.

if p-ind-event = 'bt-clone-conf' then do:

    run upc/upc-re1001.p (input g-row-docum-est-upc).

    if valid-handle(h-bt-conf-re1001) then
        apply 'choose' to h-bt-conf-re1001.

end.

if valid-handle(h-bt-conf-re1001)  and 
   valid-handle(h-bt-clone-re1001) then do:

    assign h-bt-clone-re1001:sensitive = h-bt-conf-re1001:sensitive
           h-bt-clone-re1001:visible   = yes.
end.
