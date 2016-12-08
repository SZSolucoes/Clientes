/***********************************************************************
**  /*   */
**  UT9000.I - Defini‡Æo das vari veis de ambiente do Magnus 97
**  {1} = programa provido pelo Roundtable
**  {2} = versao   provido pelo Roundtable
************************************************************************/

/* System Variable Definitions ---                                       */
{include/i-sysvar.i}
def var c_cod_empres_usuar as char no-undo.
def var c_nom_razao_social as char no-undo.

&IF  '{1}' = ' ' or  '{1}' = '' 
 OR  '{2}' = ' ' or  '{2}' = '' 
 OR  '{1}' = '"' or  '{2}' = '"' &THEN 

    message 'A chamada do include UT9000.I est  sem os parƒmetros necess rios,' skip
            'ou em local incorreto. Vocˆ deve editar o programa no UIB e ' skip
            'verificar a chamada que deve estar no formato: ' skip(1)
            '~{utp/ut9000.i "XX9999" "9.99.99.999"~}'
            view-as alert-box error.
    quit.

&ELSE    

    /*rodar pi-rsocial persistent para verifica‡Æo empresa usuario*/
    if not valid-handle(h-rsocial)
    or h-rsocial:type <> "procedure":U
    or h-rsocial:file-name <> "utp/ut-rsocial.p":U then do:
        if l-achou-prog then
            run utp/ut-rsocial.p persistent set h-rsocial.
    end.
    if l-achou-prog then
        run pi-rsocial in h-rsocial (output c_cod_empres_usuar, output c_nom_razao_social).

    assign c-programa-mg97 = caps("{1}")
           c-versao-mg97   = "{2}".

&IF "{&emsfnd_version}" >= "1.00" &THEN
    
    IF NOT VALID-HANDLE(gh-bofn068) OR
       NOT VALID-HANDLE(gh-bofn088) OR
       NOT VALID-HANDLE(gh-bofn116) 
    THEN DO:         
        RUN fnbo/bofn068.p PERSISTENT SET gh-bofn068.
        RUN fnbo/bofn088.p PERSISTENT SET gh-bofn088.
        RUN fnbo/bofn116.p PERSISTENT SET gh-bofn116.
    END.
    
    EMPTY TEMP-TABLE ttprog_dtsul.
    RUN openQueryStatic IN gh-bofn068("Main").
    RUN gotoKey IN gh-bofn068 (c-programa-mg97).
    RUN getRecord IN gh-bofn068 (OUTPUT TABLE ttprog_dtsul).
  
    FIND FIRST ttprog_dtsul no-lock
        where ttprog_dtsul.cod_prog_dtsul = c-programa-mg97 no-error.
    if  avail ttprog_dtsul then do:
        assign c-titulo-prog-mg97    = ttprog_dtsul.des_prog_dtsul
               c-nom-prog-upc-mg97   = ttprog_dtsul.nom_prog_upc
               c-nom-prog-appc-mg97  = ttprog_dtsul.nom_prog_appc
               c-nom-prog-dpc-mg97   = ttprog_dtsul.nom_prog_dpc
               i-num-topico-hlp-mg97 = ttprog_dtsul.num_topico.
       &IF DEFINED(TransformacaoWindow) <> 0 &THEN
          if session:window-system <> "TTY" then 
            assign i-template          = ttprog_dtsul.idi_template.
       &ENDIF

        
        EMPTY TEMP-TABLE ttprocedimento.
        RUN openQueryStatic IN gh-bofn088("Default").
        RUN gotoKey IN gh-bofn088 (ttprog_dtsul.cod_proced).
        RUN getRecord IN gh-bofn088 (OUTPUT TABLE ttprocedimento).

        find FIRST ttprocedimento no-lock
            /*where procedimento.cod_proced = prog_dtsul.cod_proced*/ no-error.
        if  avail ttprocedimento then do:
            EMPTY TEMP-TABLE ttmodul_dtsul.
            RUN openQueryStatic IN gh-bofn116("Default").
            RUN gotoKey IN gh-bofn116 (ttprocedimento.cod_modul_dtsul).
            RUN getRecord IN gh-bofn116 (OUTPUT TABLE ttmodul_dtsul).

            find FIRST ttmodul_dtsul no-lock
                /*where modul_dtsul.cod_modul_dtsul = procedimento.cod_modul_dtsul*/ no-error. 
            if  avail ttmodul_dtsul then do:
                assign c-modulo-mg97         = caps(ttmodul_dtsul.nom_modul_dtsul_menu)
                       c-cod-mod-mg97        = caps(ttmodul_dtsul.cod_modul_dtsul)
                       c-nom-manual-hlp-mg97 = "dochlp~/" + string(ttmodul_dtsul.num_manual_documen, "999999") + ".hlp".


&ELSE
    FIND FIRST prog_dtsul no-lock
        where prog_dtsul.cod_prog_dtsul = c-programa-mg97 no-error.
    if  avail prog_dtsul then do:
        assign c-titulo-prog-mg97    = prog_dtsul.des_prog_dtsul
               c-nom-prog-upc-mg97   = prog_dtsul.nom_prog_upc
               c-nom-prog-appc-mg97  = prog_dtsul.nom_prog_appc
               c-nom-prog-dpc-mg97   = prog_dtsul.nom_prog_dpc
               i-num-topico-hlp-mg97 = prog_dtsul.num_topico.
       &IF DEFINED(TransformacaoWindow) <> 0 &THEN
          if session:window-system <> "TTY" then 
            /*assign i-template          = prog_dtsul.ind_template.*/ /*aqui*/
       &ENDIF

        find FIRST procedimento no-lock
            where procedimento.cod_proced = prog_dtsul.cod_proced no-error.
        if  avail procedimento then do:
            find FIRST modul_dtsul no-lock
                where modul_dtsul.cod_modul_dtsul = procedimento.cod_modul_dtsul no-error. 
            if  avail modul_dtsul then do:
                assign c-modulo-mg97         = caps(modul_dtsul.nom_modul_dtsul_menu)
                       c-cod-mod-mg97        = caps(modul_dtsul.cod_modul_dtsul)
                       c-nom-manual-hlp-mg97 = "dochlp~/" + string(modul_dtsul.num_manual_documen, "999999") + ".hlp".

&ENDIF

            end.
        end.
    end.                                                      
    else do:

        assign c-titulo-prog-mg97    = caps(c-programa-mg97)
               c-nom-prog-upc-mg97   = ""
               c-nom-prog-appc-mg97  = ""
               i-num-topico-hlp-mg97 = 0
               c-nom-manual-hlp-mg97 = "dochlp~/000000.hlp".
    end.                 
     
    /* Tradu‡Æo T¡tulo dos Programas */
    run utp/ut-liter.p (input replace(c-titulo-prog-mg97," ","_"),
                        input "*":U,
                        input "":U). 

    Assign c-titulo-prog-mg97 = Return-value.

    &IF "{&INCLUDE_LM}" = "TRUE" &THEN
    
        RUN btb/btb970aa.p PERSISTENT SET h_btb970aa.
        RUN pi-retorna-inf-modulo IN h_btb970aa (INPUT c-lm-modulo,
                                                 OUTPUT c_id_modulo_ls,
                                                 OUTPUT c_desc_modulo_ls).

        IF VALID-HANDLE(h_btb970aa) THEN
            DELETE PROCEDURE h_btb970aa.

    &ELSE
    
        DEF VAR c_id_modulo_ls        AS CHAR NO-UNDO.
        DEF VAR c_desc_modulo_ls      AS CHAR NO-UNDO.

        IF INDEX(PROGRAM-NAME(1),"btb")    > 0 OR
           INDEX(PROGRAM-NAME(1),"sec")    > 0 OR
           INDEX(PROGRAM-NAME(1),"men")    > 0 OR
           INDEX(PROGRAM-NAME(1),"utp")    > 0 OR
           INDEX(PROGRAM-NAME(1),"fnzoom") > 0 OR 
           INDEX(PROGRAM-NAME(1),"fngo")   > 0 THEN DO:
            IF INDEX(PROGRAM-NAME(1),"btb") > 0 THEN
                ASSIGN c_id_modulo_ls = "5550"
                       c_desc_modulo_ls = "B sico (NPS)".
            ELSE IF INDEX(PROGRAM-NAME(1),"sec") > 0 THEN
                ASSIGN c_id_modulo_ls = "5551"
                       c_desc_modulo_ls = "Seguran‡a".
            ELSE IF INDEX(PROGRAM-NAME(1),"men") > 0 THEN
                ASSIGN c_id_modulo_ls = "5549"
                       c_desc_modulo_ls = "Menu".
            ELSE IF INDEX(PROGRAM-NAME(1),"utp")    > 0 OR
                    INDEX(PROGRAM-NAME(1),"fnzoom") > 0 OR
                    INDEX(PROGRAM-NAME(1),"fngo")   > 0 THEN
                ASSIGN c_id_modulo_ls = "5553"
                       c_desc_modulo_ls = "Utilit rios".            
        END.
        ELSE DO:
            ASSIGN c_id_modulo_ls = "????"
                   c_desc_modulo_ls = "????????????????????".
        END.
    &ENDIF       

    &IF  "{&WINDOW-NAME}" <> "" AND "{&WINDOW-NAME}" <> "CURRENT-WINDOW" &THEN 
         assign {&WINDOW-NAME}:title = if l-achou-prog then
                                       "06.9."
                                     + c_id_modulo_ls
                                     + " - "
                                     + c-programa-mg97 
                                     + " - " 
                                     + c-versao-mg97  
                                     + " - "
                                     + c-titulo-prog-mg97
                                     + " - " 
                                     + c_cod_empres_usuar
                                     + " - " 
                                     + c_nom_razao_social
                                     else 
                                       "06.9."
                                     + c_id_modulo_ls
                                     + " - "                                    
                                     + c-programa-mg97 
                                     + " - " 
                                     + c-versao-mg97
                                     + " - " 
                                     + c-titulo-prog-mg97.
    &ELSE
         assign frame {&FRAME-NAME}:title = if l-achou-prog THEN
                                              "06.9."
                                            + c_id_modulo_ls
                                            + " - "
                                            + c-programa-mg97 
                                            + " - " 
                                            + c-versao-mg97
                                            + " - "                                 
                                            + c-titulo-prog-mg97  
                                            + " - " 
                                            + c_cod_empres_usuar
                                            + " - " 
                                            + c_nom_razao_social
                                            else
                                             "06.9."
                                            + c_id_modulo_ls
                                            + " - "                                                                                  
                                            + c-programa-mg97
                                            + " - " 
                                            + c-versao-mg97 
                                            + " - " 
                                            + c-titulo-prog-mg97.

    &ENDIF
    
 if today > 03/01/1998 then do:    
 /******************************* Valida‡Æo ***********************************/   

    /* Verifica‡Æo do registro do produto */
    if c-programa-mg97 <> "UT-CONF" then do:
      run utp/ut-vfreg.p (output l-acesso-livre).
      if l-acesso-livre = no then do:
        run utp/ut-msgs.p (input "show",
                           input 8525,
                           input "").      
        apply "close" to this-procedure.
        return.
      end.    
    end.  

    /* Verifica‡Æo da data de validade do contrato */
    if c-programa-mg97 <> "UT-CONF" then do:
      run utp/ut-vfvld.p (output d-data-contrato).
      if d-data-contrato < today then do:
        run utp/ut-msgs.p (input "show",
                           input 8536,
                           input string(d-data-contrato)).      
        apply "close" to this-procedure.
        return.
      end.
    end.  

    /* Verifica‡Æo do acesso ao modulo do programa com base no contrato */
    if c-programa-mg97 <> "UT-CONF" then do:
      run utp/ut-vfmod.p (input c-cod-mod-mg97, output l-acesso-livre).
      if l-acesso-livre = no then do:
        run utp/ut-msgs.p (input "show",
                           input 8527,
                           input c-cod-mod-mg97).      
        apply "close" to this-procedure.
        return.
      end.  
    end.  
    
    /* Verifica‡Æo de usu rios ativos */
    if c-programa-mg97 <> "UT-CONF" then do:
      run utp/ut-vfusr.p (output i-user-conectados, output i-licenca-usuar).
      if i-user-conectados > i-licenca-usuar then do:
        run utp/ut-msgs.p (input "show",
                           input 8532,
                           input string(i-user-conectados) + "~~" +
                                 string(i-licenca-usuar)).      
        apply "close" to this-procedure.
        return.
      end.
    end.  

/******************************************************************************/
 end.
    
    /* Verifica‡Æo da seguran‡a e login informado */
    {utp/ut-vfsec.i}
    
    /* Inicio do log de execu‡Æo de programas */
    {include/i-logini.i} 
    
    &IF DEFINED(TransformacaoWindow) <> 0 &THEN
      if session:window-system <> "TTY" then do:
       {include/i-trswin.i}
      end. 
    &ENDIF
&ENDIF

/* ut9000.i */
