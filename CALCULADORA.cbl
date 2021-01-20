       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULADORA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           77 op PIC 9(1) VALUE 0.
           77 num1 PIC 9(5).
           77 num2 PIC 9(5).
           77 res PIC 9(10).
           77 resto PIC 9(1).
           77 sair PIC 9(1).
        SCREEN SECTION.
           01 menu-principal.
               02 BLANK SCREEN.
               02 LINE 01 COL 01 VALUE "******************************".
               02 LINE 02 COL 13 VALUE "MENU".
               02 LINE 03 COL 03 VALUE "NUMERO 1: ".
               02 LINE 03 COL 14 PIC X(5) TO num1.
               02 LINE 04 COL 03 VALUE "NUMERO 2: ".
               02 LINE 04 COL 14 PIC X(5) TO num2.
               02 LINE 06 COL 01 VALUE "******************************".
               02 LINE 07 COL 03 VALUE "(1) SOMA".
               02 LINE 08 COL 03 VALUE "(2) SUBTRACAO".
               02 LINE 09 COL 03 VALUE "(3) MULTIPLICACAO".
               02 LINE 10 COL 03 VALUE "(4) DIVISAO".
               02 LINE 11 COL 03 VALUE "(5) SAIR".
               02 LINE 12 COL 03 PIC X TO op.
               02 LINE 14 COL 01 VALUE "******************************".
            01 SOMA.
               02 BLANK SCREEN.
               02 LINE 01 COL 01 VALUE "******************************".
               02 LINE 02 COL 13 VALUE "SOMA".
               02 LINE 03 COL 03 VALUE "RESULTADO: ".
               02 LINE 03 COL 15 PIC Z(10) FROM res.
               02 LINE 04 COL 03 PIC Z(1) TO sair.
               02 LINE 05 COL 01 VALUE "******************************".
            01 SUBT.
               02 BLANK SCREEN.
               02 LINE 01 COL 01 VALUE "******************************".
               02 LINE 02 COL 10 VALUE "SUBTRACAO".
               02 LINE 03 COL 03 VALUE "RESULTADO: ".
               02 LINE 03 COL 15 PIC X(10) FROM res.
               02 LINE 04 COL 03 PIC Z(1) TO sair.
               02 LINE 05 COL 01 VALUE "******************************".
            01 MULT.
               02 BLANK SCREEN.
               02 LINE 01 COL 01 VALUE "******************************".
               02 LINE 02 COL 08 VALUE "MULTIPLICACAO".
               02 LINE 03 COL 03 VALUE "RESULTADO: ".
               02 LINE 03 COL 15 PIC Z(10) FROM res.
               02 LINE 04 COL 03 PIC Z(1) TO sair.
               02 LINE 05 COL 01 VALUE "******************************".
            01 DIVI.
               02 BLANK SCREEN.
               02 LINE 01 COL 01 VALUE "******************************".
               02 LINE 02 COL 12 VALUE "DIVISAO".
               02 LINE 03 COL 03 VALUE "RESULTADO: ".
               02 LINE 04 COL 03 VALUE "RESTO: ".
               02 LINE 04 COL 03 PIC X(1) FROM resto.
               02 LINE 03 COL 15 PIC Z(10) FROM res.
               02 LINE 05 COL 03 PIC Z(1) TO sair.
               02 LINE 06 COL 01 VALUE "******************************".
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
        INICIO.
           DISPLAY menu-principal.
           ACCEPT menu-principal.
           EVALUATE op
               WHEN 1
                   COMPUTE res=num1+num2
                   DISPLAY SOMA
                   ACCEPT SOMA
                   GO TO INICIO
                WHEN 2
                   COMPUTE res=num1 - num2
                   DISPLAY SUBT
                   ACCEPT SUBT
                   GO TO INICIO
                WHEN 3
                   COMPUTE res=num1*num2
                   DISPLAY MULT
                   ACCEPT MULT
                   GO TO INICIO
                WHEN 4
                   DIVIDE num1 BY num2 GIVING res REMAINDER resto 
                   DISPLAY DIVI
                   ACCEPT DIVI
                   GO TO INICIO
                WHEN OTHER
                   STOP RUN
            END-EVALUATE
            STOP RUN.
       END PROGRAM CALCULADORA.
