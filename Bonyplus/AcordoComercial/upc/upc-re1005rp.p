/*****************************************************************************
 ** PROGRAMA..: UPC-RE1005RP.P
 ** OBJETIVO..: UPC NA ATUALIZA€ÇO DOCTO FISCAL - RE1005RP.P
 ** AUTOR.....: SZsolucoes
 ** CLIENTE...: Bonyplus
 ** VERSAO....: 2.12.00.001 - 07/11/2016 
 ******************************************************************************/

{include/i-prgvrs.i upc-re1005rp 2.12.00.001} 
{utp/ut-glob.i}

/* Definicao de parametros de entrada */
{include/i-epc200.i re1005rp}

DEF TEMP-TABLE tt-erro2 NO-UNDO
    FIELD i-sequen         AS INTEGER
    FIELD cd-erro          AS INTEGER
    FIELD mensagem         AS CHARACTER FORMAT "x(150)".

/*************** Defini‡Æo de Variavel ********************************************/
DEFINE VARIABLE i-cont AS INTEGER     NO-UNDO.

/*************************** PAR¶METROS PADRÇO *************************************/
DEFINE INPUT        PARAMETER p-ind-event AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt-epc.

IF p-ind-event = "inicio-atualizacao" THEN DO:

    ASSIGN i-cont = 0.
   
    FIND FIRST tt-epc
        WHERE tt-epc.cod-event     = p-ind-event
        AND   tt-epc.cod-parameter = "docum-est rowid"  NO-ERROR.
    IF AVAIL tt-epc THEN DO:
        EMPTY TEMP-TABLE tt-erro2.
        FOR FIRST docum-est NO-LOCK
            WHERE ROWID(docum-est) = TO-ROWID(tt-epc.val-parameter):

            FIND ext-docum-est NO-LOCK
                WHERE ext-docum-est.serie-docto      = docum-est.serie-docto 
                  AND ext-docum-est.nro-docto        = docum-est.nro-docto   
                  AND ext-docum-est.cod-emitente     = docum-est.cod-emitente
                  AND ext-docum-est.nat-operacao     = docum-est.nat-operacao 
                  AND ext-docum-est.nr-acordo-comerc <> "" NO-ERROR.

            IF AVAIL ext-docum-est THEN DO:

                FIND es-acordo-comerc NO-LOCK
                    WHERE es-acordo-comer.nr-acordo-comerc = ext-docum-est.nr-acordo-comerc NO-ERROR.

                IF AVAIL es-acordo-comerc THEN DO:
                    
                    IF es-acordo-comer.cod-emitente <> docum-est.cod-emitente THEN DO:

                        ASSIGN i-cont = i-cont + 10.
                        CREATE tt-erro2.
                        ASSIGN tt-erro2.i-sequen = i-cont
                               tt-erro2.cd-erro  = 17006
                               tt-erro2.mensagem = "Acordo Comercial: " + string(es-acordo-comer.nr-acordo-comerc) + "  "  + " NÆo pertence ao Emitente " + string(docum-est.cod-emitente) + " ! " . 
                    END.

                    IF es-acordo-comerc.impresso = NO THEN DO:

                        ASSIGN i-cont = i-cont + 10.
                        CREATE tt-erro2.
                        ASSIGN tt-erro2.i-sequen = i-cont
                               tt-erro2.cd-erro  = 17006
                               tt-erro2.mensagem = "Acordo Comercial: " + string(es-acordo-comer.nr-acordo-comerc) + "" + " deve estar impresso para efetuar o recebimento!".

                    END.

                    FIND FIRST es-acordo-pendencia NO-LOCK
                        WHERE es-acordo-pendencia.nr-acordo-comerc = es-acordo-comerc.nr-acordo-comerc 
                          AND es-acordo-pendencia.ind-situacao <> 1 NO-ERROR. /* NÆo aprovado*/
                   
                    IF AVAIL es-acordo-pendencia THEN DO:

                        ASSIGN i-cont = i-cont + 10.
                        CREATE tt-erro2.
                        ASSIGN tt-erro2.i-sequen = i-cont
                               tt-erro2.cd-erro  = 17006
                               tt-erro2.mensagem = "Acordo Comercial: " + string(es-acordo-comer.nr-acordo-comerc) + "" + " NÆo est  aprovado, Favor verificar a situa‡Æo".

                        
                    END.

                END.
                ELSE DO:

                    ASSIGN i-cont = i-cont + 10.
                        CREATE tt-erro2.
                        ASSIGN tt-erro2.i-sequen = i-cont
                               tt-erro2.cd-erro  = 17006
                               tt-erro2.mensagem = "Acordo Comercial: " + string(es-acordo-comer.nr-acordo-comerc) + "" + "Inexistente!".

                END.

            END.

            IF CAN-FIND(first tt-erro2) THEN

                PUT  "Documento n’o pode ser Atualizado." AT 01 SKIP
                     "Seq           Cod-Erro       Mensagem " AT 01 SKIP.
                
                FOR EACH tt-erro2
                    BREAK BY tt-erro2.i-sequen :
                    
                    PUT UNFORMATTED  tt-erro2.i-sequen AT 01
                                     tt-erro2.cd-erro  AT 15 
                                     tt-erro2.mensagem AT 30 SKIP.
                        
                END.
                IF CAN-FIND(first tt-erro2) THEN 
                   RETURN "NOK".

        END.
    END.
END.


IF p-ind-event = "apos-finalizar" THEN DO: 
    FOR FIRST tt-epc 
        WHERE tt-epc.cod-event     = "apos-finalizar"
        AND   tt-epc.cod-parameter = "docum-est ROWID" NO-LOCK:

        FIND FIRST docum-est
            WHERE ROWID(docum-est) = TO-ROWID(tt-epc.val-parameter) NO-LOCK NO-ERROR.

        IF AVAIL docum-est THEN DO:
                        
            FIND ems2custom.ext-docum-est NO-LOCK
                WHERE ext-docum-est.serie-docto  = docum-est.serie-docto 
                  AND ext-docum-est.nro-docto    = docum-est.nro-docto   
                  AND ext-docum-est.cod-emitente = docum-est.cod-emitente
                  AND ext-docum-est.nat-operacao = docum-est.nat-operacao NO-ERROR.

            IF AVAIL ext-docum-est
                AND ext-docum-est.nr-acordo-comerc <> "" THEN DO:

                FOR EACH tit_ap NO-LOCK
                    WHERE tit_ap.cod_estab        = docum-est.cod-estab
                      AND tit_ap.cdn_fornecedor   = docum-est.cod-emitente
                      AND tit_ap.cod_espec_docto  = "DP"
                      AND tit_ap.cod_ser_docto    = docum-est.serie-docto
                      AND tit_ap.cod_tit_ap       = docum-est.nro-docto:

                    FIND es-tit_ap EXCLUSIVE-LOCK
                        WHERE  es-tit_ap.cod_estab       = tit_ap.cod_estab       
                           AND es-tit_ap.cdn_fornecedor  = tit_ap.cdn_fornecedor  
                           AND es-tit_ap.cod_espec_docto = tit_ap.cod_espec_docto 
                           AND es-tit_ap.cod_ser_docto   = tit_ap.cod_ser_docto   
                           AND es-tit_ap.cod_tit_ap      = tit_ap.cod_tit_ap 
                           AND es-tit_ap.cod_parcela     = tit_ap.cod_parcela NO-ERROR.

                    IF NOT AVAIL es-tit_ap THEN DO:
                        CREATE es-tit_ap.
                        ASSIGN es-tit_ap.cod_estab         = tit_ap.cod_estab      
                               es-tit_ap.cdn_fornecedor    = tit_ap.cdn_fornecedor 
                               es-tit_ap.cod_espec_docto   = tit_ap.cod_espec_docto
                               es-tit_ap.cod_ser_docto     = tit_ap.cod_ser_docto  
                               es-tit_ap.cod_tit_ap        = tit_ap.cod_tit_ap     
                               es-tit_ap.cod_parcela       = tit_ap.cod_parcela .

                    END.

                    ASSIGN es-tit_ap.nr-acordo-comerc  = ext-docum-est.nr-acordo-comerc.

                END.

            END.

        END.
    END.
END.
