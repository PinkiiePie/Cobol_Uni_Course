       01 FILME-DATA.
            02 BLANK SCREEN.
            02 LINE 01 COL 01 VALUE "******************************".
            02 LINE 02 COL 05 VALUE "CADASTRO FILME".
            02 LINE 03 COL 02 VALUE "CODIGO: ".
            02 LINE 03 COL 13 PIC Z(3) FROM WS-CODIGO.
            02 LINE 04 COL 02 VALUE "NOME: ".
            02 LINE 04 COL 13 PIC X(80) FROM WS-FNOME.
            02 LINE 05 COL 02 VALUE "ANO: ".
            02 LINE 05 COL 13 PIC Z(4) FROM WS-ANO.
            02 LINE 06 COL 02 VALUE "SINOPSE: ".
            02 LINE 06 COL 13 PIC X(200) FROM WS-SINOPSE.
            02 LINE 07 COL 02 VALUE "STATUS: ".
            02 LINE 07 COL 13 PIC 9(1) FROM WS-ISTATUS.
