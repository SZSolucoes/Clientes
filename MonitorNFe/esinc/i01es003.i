/***************************************************************************
**     i01in490.i - Campo: idi-situacao  Tabela: nfe003
***************************************************************************/

{include/i-lgcode.i}

&IF "{&LANGUAGE-CODE}" = "POR" &THEN
&glob val1 Digitada Recebimento
&glob val2 Nota com Erro Neg¢cio
&glob val3 Atualizada Recebimento
&glob val4 Eliminada  Recebimento
&glob val5 DANFE n∆o Autorizado
&glob val6 Liberado para Integraá∆o
&glob val7 Conferido para Integraá∆o
&ENDIF
&IF "{&LANGUAGE-CODE}" = "ESP" &THEN
&ENDIF
&IF "{&LANGUAGE-CODE}" = "ING" &THEN
&ENDIF

{include/ind01-10.i {1} {2}}
/* Fim */

