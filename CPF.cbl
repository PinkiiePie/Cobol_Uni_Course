       IDENTIFICATION DIVISION.
       PROGRAM-ID. VER-CPF.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           77 CPF PIC X(11).
           77 NCPF REDEFINES CPF PIC 9(1) OCCURS 11. 
           77 ACC PIC 9(3) VALUE ZERO.
           77 DIGIT PIC 9(1) OCCURS 2.
           77 I PIC 9(2).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Digite seu CPF: ".
            ACCEPT CPF.
            PERFORM VARYING I FROM 1 BY 1 UNTIL I = 10
               COMPUTE ACC = ACC + (NCPF(I) * (11 - I))
            END-PERFORM.
            COMPUTE ACC = FUNCTION MOD(ACC, 11).
            COMPUTE ACC = 11 - ACC.
            IF ACC > 9
                MOVE ZEROES TO ACC
            END-IF.
            MOVE ACC TO DIGIT(1).
            MOVE ZEROES TO ACC.
            PERFORM VARYING I FROM 1 BY 1 UNTIL I = 10
               COMPUTE ACC = ACC + (NCPF(I) * (12 - I))
            END-PERFORM.
            COMPUTE ACC = ACC + (DIGIT(1) * 2).
            COMPUTE ACC = FUNCTION MOD(ACC, 11).
            COMPUTE ACC = 11 - ACC.
            IF ACC > 9
                MOVE ZEROES TO ACC
            END-IF.
            MOVE ACC TO DIGIT(2).
            IF DIGIT(1) = NCPF(10) AND DIGIT(2) = NCPF(11)
                DISPLAY "CPF OK!"
            ELSE
                DISPLAY "CPF ~OK!"
            END-IF.
            STOP RUN.
       END PROGRAM VER-CPF.