       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOCADORA.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ARQ-CLIENTES 
           ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS IS RANDOM RECORD KEY IS CCPF
               FILE STATUS IS WS-FS.
       SELECT ARQ-FILMES
           ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS IS RANDOM RECORD KEY IS CODIGO
               FILE STATUS IS WS-FS-2.
       DATA DIVISION.
       FILE SECTION.
       FD ARQ-CLIENTES VALUE OF FILE-ID IS "CLIENTES.dat".
           COPY REG-DATA-CLIENTES.
       FD ARQ-FILMES VALUE OF FILE-ID IS "FILMES.dat".
           COPY REG-FILME.
       WORKING-STORAGE SECTION.
           77 WS-FS PIC 99.
           77 WS-FS-2 PIC 99.
           77 OP PIC 9 VALUE 0.
           77 INUTIL PIC 9.
           COPY WS-CLIENTE.
           COPY WS-FILME.
        SCREEN SECTION.
           COPY S-MENU.
           COPY S-ALUGAR.
           COPY S-DEVOLVER.
           COPY S-GERENCIAR.
           COPY S-CAD-FILME.
           COPY S-CAD-CLIENTES.
           COPY S-FILE-ERROR.
           COPY S-CONSULTAC.
           COPY S-CLIENTE.
           COPY S-CONSULTAF.
           COPY S-FILME.
           COPY S-EXCLUIC.
           COPY S-EXCLUIF.
           COPY S-ALTERAC.
           COPY S-BLANK.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN I-O ARQ-CLIENTES.
                IF WS-FS <> 0 
                    DISPLAY "ERRO NA ABERTURA WS-FS: " WS-FS
                    CLOSE ARQ-CLIENTES
                    DISPLAY FILE-ERRO-SCREEN
                    ACCEPT INUTIL
                    GO TO MAIN-PROCEDURE
                END-IF.
           OPEN I-O ARQ-FILMES.
                IF WS-FS-2 <> 0
                    DISPLAY "ERRO NA ABERTURA WS-FS: " WS-FS-2
                    CLOSE ARQ-FILMES
                    DISPLAY FILE-ERRO-SCREEN
                    ACCEPT INUTIL
                    GO TO MAIN-PROCEDURE
                END-IF.
        VOLTAR.
           DISPLAY MENUU.
           ACCEPT MENUU.
           EVALUATE OP
               WHEN 1
                   GO TO ALUGAR-PROCEDURE
               WHEN 2
                   GO TO DEVOLVER-PROCEDURE
               WHEN 3
                   GO TO GERENCIAR-PROCEDURE
               WHEN 4
                   CLOSE ARQ-CLIENTES
                   CLOSE ARQ-FILMES
                   STOP RUN
           END-EVALUATE.
       ALUGAR-PROCEDURE.
           DISPLAY ALUGAR.
           ACCEPT ALUGAR.
           READ ARQ-CLIENTES RECORD INTO WS-CLIENTES
               KEY IS CCPF
               INVALID KEY
                   DISPLAY BLANK-SCREEN
                   DISPLAY ' CLIENTE NAO ENCONTRADO'
                   ACCEPT INUTIL
                   GO GERENCIAR-PROCEDURE
            END-READ.
            READ ARQ-FILMES RECORD INTO WS-FILME
               KEY IS CODIGO
               INVALID KEY
                   DISPLAY BLANK-SCREEN
                   DISPLAY ' FILME NAO ENCONTRADO'
                   ACCEPT INUTIL
            END-READ.
            IF WS-ISTATUS = 0
               DISPLAY BLANK-SCREEN
               DISPLAY ' FILME JA ALUGADO'
               ACCEPT INUTIL
               GO VOLTAR
            END-IF
            IF WS-FILME-ALUGADO <> 0
               DISPLAY BLANK-SCREEN
               DISPLAY ' CLIENTE JA POSSUI UM FILME ALUGADO'
               ACCEPT INUTIL
               GO VOLTAR
            END-IF
            MOVE CODIGO TO WS-FILME-ALUGADO.
            MOVE WS-CLIENTES TO REG-DATA.
            REWRITE REG-DATA
               INVALID KEY
                   DISPLAY BLANK-SCREEN 
                   DISPLAY ' ERRO AO ALUGAR'
                   ACCEPT INUTIL
               NOT INVALID KEY CONTINUE
            END-REWRITE.
            MOVE 0 TO WS-ISTATUS.
            MOVE WS-FILME TO REG-FILME.
            REWRITE REG-FILME
               INVALID KEY
                   DISPLAY BLANK-SCREEN 
                   DISPLAY ' ERRO AO ALUGAR'
                   ACCEPT INUTIL
               NOT INVALID KEY CONTINUE
            END-REWRITE.
            GO VOLTAR.
       DEVOLVER-PROCEDURE.
           DISPLAY DEVOLVER.
           ACCEPT DEVOLVER.
           READ ARQ-CLIENTES RECORD INTO WS-CLIENTES
               KEY IS CCPF
               INVALID KEY
                   DISPLAY BLANK-SCREEN
                   DISPLAY ' CLIENTE NAO ENCONTRADO'
                   ACCEPT INUTIL
                   GO GERENCIAR-PROCEDURE
            END-READ.
            MOVE ZEROES TO WS-FILME-ALUGADO.
            MOVE WS-CLIENTES TO REG-DATA.
            REWRITE REG-DATA
               INVALID KEY
                   DISPLAY BLANK-SCREEN 
                   DISPLAY ' ERRO AO DEVOLVER'
                   ACCEPT INUTIL
               NOT INVALID KEY CONTINUE
            END-REWRITE.
            READ ARQ-FILMES RECORD INTO WS-FILME
               KEY IS CODIGO
               INVALID KEY
                   DISPLAY BLANK-SCREEN
                   DISPLAY ' FILME NAO ENCONTRADO'
                   ACCEPT INUTIL
            END-READ.
            MOVE 1 TO WS-ISTATUS.
            MOVE WS-FILME TO REG-FILME.
            REWRITE REG-FILME
               INVALID KEY
                   DISPLAY BLANK-SCREEN 
                   DISPLAY ' ERRO AO DEVOLVER'
                   ACCEPT INUTIL
               NOT INVALID KEY CONTINUE
            END-REWRITE.
            GO VOLTAR.
       GERENCIAR-PROCEDURE.
           DISPLAY GERENCIAR.
           ACCEPT GERENCIAR.
           EVALUATE OP
               WHEN 1
      *             CADASTRO-CLIENTE
                   DISPLAY CADCLIENTE
                   ACCEPT CADCLIENTE
                   MOVE 000 TO WS-FILME-ALUGADO
                   MOVE WS-CLIENTES TO REG-DATA
                   WRITE REG-DATA
                       INVALID KEY 
                           DISPLAY BLANK-SCREEN
                           DISPLAY ' CLIENTE JA EXISTE'
                           ACCEPT INUTIL
                           GO GERENCIAR-PROCEDURE
                   END-WRITE
                   DISPLAY CLIENT-DATA
                   ACCEPT INUTIL
                   GO GERENCIAR-PROCEDURE
               WHEN 2
      *             CADASTRO-FILME
                   DISPLAY CADFILME
                   ACCEPT CADFILME
                   MOVE 1 TO WS-ISTATUS
                   MOVE WS-FILME TO REG-FILME
                   WRITE REG-FILME
                       INVALID KEY
                           DISPLAY BLANK-SCREEN 
                           DISPLAY ' FILME JA CADASTRADO'
                           ACCEPT INUTIL
                           GO GERENCIAR-PROCEDURE
                   END-WRITE
                   DISPLAY FILME-DATA
                   ACCEPT INUTIL
                   GO GERENCIAR-PROCEDURE 
               WHEN 3
      *             CONSULTAR-CLIENTE
                   DISPLAY CONSULTAC
                   ACCEPT CONSULTAC
                   READ ARQ-CLIENTES RECORD INTO WS-CLIENTES
                       KEY IS CCPF
                       INVALID KEY 
                           DISPLAY BLANK-SCREEN
                           DISPLAY ' CLIENTE NAO ENCONTRADO'
                       NOT INVALID KEY DISPLAY CLIENT-DATA
                   END-READ
                   ACCEPT INUTIL
                   GO GERENCIAR-PROCEDURE
                WHEN 4
      *             CONSULTAR-FILME
                   DISPLAY CONSULTAF 
                   ACCEPT CONSULTAF 
                   READ ARQ-FILMES RECORD INTO WS-FILME
                       KEY IS CODIGO
                       INVALID KEY
                           DISPLAY BLANK-SCREEN 
                           DISPLAY ' FILME NAO ENCONTRADO'
                       NOT INVALID KEY DISPLAY FILME-DATA
                   END-READ
                   ACCEPT INUTIL
                   GO GERENCIAR-PROCEDURE
                WHEN 5
      *             ALTERAR-CLIENTE
                   DISPLAY ALTERAC
                   ACCEPT ALTERAC
                   READ ARQ-CLIENTES
                       KEY IS CCPF
                       INVALID KEY
                           DISPLAY BLANK-SCREEN 
                           DISPLAY ' CLIENTE NAO ENCONTRADO'
                           ACCEPT INUTIL
                           GO GERENCIAR-PROCEDURE
                       NOT INVALID KEY CONTINUE
                   END-READ
                   DISPLAY CADCLIENTE
                   ACCEPT CADCLIENTE
                   MOVE WS-CLIENTES TO REG-DATA
                   REWRITE REG-DATA
                       INVALID KEY
                           DISPLAY BLANK-SCREEN 
                           DISPLAY ' ERRO AO GRAVAR'
                       NOT INVALID KEY CONTINUE
                   END-REWRITE
                   GO GERENCIAR-PROCEDURE
                WHEN 6
      *             EXCLUIR-CLIENTE
                   DISPLAY EXCLUIC
                   ACCEPT EXCLUIC
                   READ ARQ-CLIENTES
                       KEY IS CCPF
                       INVALID KEY
                           DISPLAY BLANK-SCREEN 
                           DISPLAY ' CLIENTE NAO ENCONTRADO'
                           ACCEPT INUTIL
                           GO GERENCIAR-PROCEDURE
                       NOT INVALID KEY CONTINUE
                   END-READ
                   DELETE ARQ-CLIENTES
                       INVALID KEY
                           DISPLAY BLANK-SCREEN 
                           DISPLAY ' ERRO AO EXCLUIR'
                           ACCEPT INUTIL
                       NOT INVALID KEY
                           DISPLAY BLANK-SCREEN
                           DISPLAY ' CPF EXCLUIDO: ' CCPF
                           ACCEPT INUTIL
                   END-DELETE
                   GO GERENCIAR-PROCEDURE
                WHEN 7
      *             EXCLUIR-FILME
                   DISPLAY EXCLUIF
                   ACCEPT EXCLUIF
                   READ ARQ-FILMES
                       KEY IS CODIGO
                       INVALID KEY
                           DISPLAY BLANK-SCREEN 
                           DISPLAY ' FILME NAO ENCONTRADO'
                           ACCEPT INUTIL
                           GO GERENCIAR-PROCEDURE
                       NOT INVALID KEY CONTINUE
                   END-READ
                   DELETE ARQ-FILMES
                       INVALID KEY
                           DISPLAY BLANK-SCREEN 
                           DISPLAY ' ERRO AO EXCLUIR'
                           ACCEPT INUTIL
                       NOT INVALID KEY 
                           DISPLAY BLANK-SCREEN
                           DISPLAY ' FILME EXCLUIDO: ' CODIGO
                           ACCEPT INUTIL
                   END-DELETE
                   GO GERENCIAR-PROCEDURE
                WHEN 8
                   GO VOLTAR
           END-EVALUATE.
       END PROGRAM LOCADORA.
