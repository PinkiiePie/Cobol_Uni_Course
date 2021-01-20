       01 CADCLIENTE.
            02 BLANK SCREEN.
            02 LINE 01 COL 01 VALUE "******************************".
            02 LINE 02 COL 05 VALUE "CADASTRAR CLIENTE".
            02 LINE 03 COL 02 VALUE "CPF: ".
            02 LINE 03 COL 13 PIC Z(11) TO WS-CCPF.
            02 LINE 04 COL 02 VALUE "NOME: ".
            02 LINE 04 COL 13 PIC X(50) TO WS-CNOME.
            02 LINE 06 COL 05 VALUE "ENDERECO".
            02 LINE 07 COL 02 VALUE "CEP: ".
            02 LINE 07 COL 13 PIC Z(8) TO WS-CCEP.
            02 LINE 08 COL 02 VALUE "RUA: ".
            02 LINE 08 COL 13 PIC X(50) TO WS-CRUA.
            02 LINE 09 COL 02 VALUE "NUMERO: ".
            02 LINE 09 COL 14 PIC Z(5) TO WS-CNUM.
            02 LINE 10 COL 02 VALUE "BAIRRO: ".
            02 LINE 10 COL 13 PIC X(50) TO WS-CBAI.
            02 LINE 11 COL 02 VALUE "CIDADE: ".
            02 LINE 11 COL 13 PIC X(50) TO WS-CCID.
            02 LINE 12 COL 02 VALUE "ESTADO: ".
            02 LINE 12 COL 13 PIC X(2) TO WS-CUF.
