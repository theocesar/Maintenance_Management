       IDENTIFICATION DIVISION.
       PROGRAM-ID. RELATORIO.
      ******************************************************************
      * Author: Théo César
      * Date: 06/02/2024
      * Purpose: Relatorio final com os custos
      * Modifications:
      *    06/02/2024 - Relatorio's structure finished.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT RELATO ASSIGN TO
       'C:/Users/Theo/Desktop/Escola/Volvo/COBOL/RelatorioFinal.txt'
       FILE STATUS IS AS-STATUS-S.

       DATA DIVISION.
       FILE SECTION.

       FD RELATO
          RECORDING MODE IS F
          BLOCK CONTAINS 0 RECORDS.

       01 ARQ-RELATO                    PIC X(62).


       WORKING-STORAGE SECTION.

       01 AS-STATUS-S                   PIC 9(02) VALUE ZEROS.

      *-----------------------------------------------------------------
      *         DEFINIÇÃO DE CABEÇALHO
      *-----------------------------------------------------------------

       01 WS-CABEC-REL1                 PIC X(60) VALUE ALL '='.

       01 WS-CABEC-REL2.
           05 WS-CABEC-REL2-PGM         PIC X(15) VALUE
                                               'PROJETO FINAL'.
           05 WS-CABEC-REL2-FL1         PIC X(13) VALUE SPACES.
           05 WS-CABEC-REL2-DES         PIC X(09) VALUE 'VOLVO S.A'.
           05 WS-CABEC-REL2-FL2         PIC X(13) VALUE SPACES.
           05 WS-CABEC-REL2-DT          PIC X(10) VALUE SPACES.

       01 WS-CABEC-REL3.
           05 WS-CABEC-REL3-HR          PIC X(08) VALUE SPACES.
           05 WS-CABEC-REL3-FL1         PIC X(10) VALUE SPACES.
           05 WS-CABEC-REL3-DES         PIC X(25) VALUE
                                       'RELATORIO TOTAL DE CUSTOS'.

      *-----------------------------------------------------------------
      *         DEFINIÇÃO DE CABEÇALHO
      *-----------------------------------------------------------------


      *-----------------------------------------------------------------
      *         DEFINIÇÃO DE DETALHE
      *-----------------------------------------------------------------
       01 LINDET01-REL.
           05 LINDET01-REL-SPACE0          PIC X(06) VALUE SPACES.
           05 LINDET01-REL-CLIENTE         PIC X(07) VALUE 'CLIENTE'.
           05 LINDET01-REL-SPACE1          PIC X(06) VALUE SPACES.
           05 LINDET01-REL-PLACA           PIC X(05) VALUE 'PLACA'.
           05 LINDET01-REL-SPACE2          PIC X(06) VALUE SPACES.
           05 LINDET01-REL-TIPO-SERVICO    PIC X(07) VALUE
                                                   'SERVICO'.
           05 LINDET01-REL-SPACE3        PIC X(06) VALUE SPACES.
           05 LINDET01-REL-DATA          PIC X(04) VALUE 'DATA'.
           05 LINDET01-REL-SPACE4        PIC X(07) VALUE SPACES.
           05 LINDET01-REL-VALOR         PIC X(08) VALUE 'TOTAL R$'.


       01 LINDET02-REL.
           05 LINDET01-REL-SPACE0          PIC X(03) VALUE SPACES.
           05 LINDET02-REL-CLIENTE       PIC X(10) VALUE SPACES.
           05 LINDET02-REL-SPACE1        PIC X(03) VALUE ' | '.
           05 LINDET02-REL-PLACA         PIC X(08) VALUE SPACES.
           05 LINDET02-REL-SPACE2        PIC X(03) VALUE ' | '.
           05 LINDET02-REL-TIPO-SERVICO  PIC X(10) VALUE SPACES.
           05 LINDET02-REL-SPACE3        PIC X(03) VALUE ' | '.
           05 LINDET02-REL-DATA          PIC X(10) VALUE SPACES.
           05 LINDET02-REL-SPACE4        PIC X(03) VALUE ' | '.
           05 LINDET02-REL-VALOR         PIC 9(06) VALUE ZEROS.

      *-----------------------------------------------------------------
      *         DEFINIÇÃO DE DETALHE
      *-----------------------------------------------------------------


      *-----------------------------------------------------------------
      *                    DEFINIÇÃO DAS VARIAVEIS
      *-----------------------------------------------------------------

       01 AS-FIM                         PIC X(01) VALUE 'N'.

       01 WS-DATA                        PIC X(08) VALUE SPACES.
       01 WS-HORA                        PIC X(06) VALUE SPACES.


       LINKAGE SECTION.

       01 WS-SUB-PLACA                     PIC X(08) VALUE SPACES.
       01 WS-SUB-TIPO-SERVICO              PIC X(10) VALUE SPACES.
       01 WS-SUB-DATA                      PIC X(10) VALUE SPACES.
       01 WS-SUB-VALOR                     PIC 9(06) VALUE ZEROS.
       01 WS-SUB-CLIENTE                   PIC X(10) VALUE SPACES.

      *-----------------------------------------------------------------
      *                    DEFINIÇÃO DAS VARIAVEIS
      *-----------------------------------------------------------------


       PROCEDURE DIVISION USING WS-SUB-CLIENTE, WS-SUB-PLACA,
                       WS-SUB-TIPO-SERVICO, WS-SUB-DATA, WS-SUB-VALOR.
       MAIN-PROCEDURE.

           PERFORM 1000-INICIALIZAR.
           PERFORM 2000-PROCESSAR UNTIL AS-FIM = 'S'.
           PERFORM 3000-FINALIZAR.


            STOP RUN.

      *-----------------------------------------------------------------
      *         INICIALIZAÇÃO
      *-----------------------------------------------------------------

       1000-INICIALIZAR                SECTION.

           ACCEPT WS-DATA FROM DATE YYYYMMDD
           ACCEPT WS-HORA FROM TIME

           MOVE WS-DATA(1:4) TO WS-CABEC-REL2-DT(7:4)
           MOVE WS-DATA(5:2) TO WS-CABEC-REL2-DT(4:2)
           MOVE WS-DATA(7:2) TO WS-CABEC-REL2-DT(1:2)
           MOVE '/'          TO WS-CABEC-REL2-DT(3:1)
                                WS-CABEC-REL2-DT(6:1)


           MOVE WS-HORA(1:2) TO WS-CABEC-REL3-HR(1:2)
           MOVE WS-HORA(3:2) TO WS-CABEC-REL3-HR(4:2)
           MOVE WS-HORA(5:2) TO WS-CABEC-REL3-HR(7:2)
           MOVE ':'          TO WS-CABEC-REL3-HR(3:1)
                                WS-CABEC-REL3-HR(6:1)

           OPEN OUTPUT RELATO
           IF AS-STATUS-S NOT EQUALS ZEROS
               DISPLAY 'DEU ERRO NA ABERTURA ' AS-STATUS-S
           END-IF



           MOVE WS-CABEC-REL1 TO ARQ-RELATO
           WRITE ARQ-RELATO

           MOVE WS-CABEC-REL2 TO ARQ-RELATO
           WRITE ARQ-RELATO

           MOVE WS-CABEC-REL3 TO ARQ-RELATO
           WRITE ARQ-RELATO

           MOVE WS-CABEC-REL1 TO ARQ-RELATO
           WRITE ARQ-RELATO

           MOVE LINDET01-REL TO ARQ-RELATO
           WRITE ARQ-RELATO

           .
       1000-INICIALIZAR-FIM.
           EXIT.

      *-----------------------------------------------------------------
      *         PROCESSAMENTO
      *-----------------------------------------------------------------


       2000-PROCESSAR                  SECTION.

           MOVE WS-SUB-CLIENTE TO LINDET02-REL-CLIENTE
           MOVE WS-SUB-PLACA TO LINDET02-REL-PLACA
           MOVE WS-SUB-TIPO-SERVICO TO LINDET02-REL-TIPO-SERVICO
           MOVE WS-SUB-DATA TO LINDET02-REL-DATA
           MOVE WS-SUB-VALOR TO LINDET02-REL-VALOR


           MOVE LINDET02-REL TO ARQ-RELATO
           WRITE ARQ-RELATO

           MOVE 'S' TO AS-FIM
           .
       2000-PROCESSAR-FIM.
           EXIT.



       3000-FINALIZAR                  SECTION.

           CLOSE RELATO
           IF AS-STATUS-S NOT EQUALS ZEROS
               DISPLAY 'DEU ERRO NO FECHAMENTO ' AS-STATUS-S
           END-IF

            EXIT PROGRAM
           .
       3000-FINALIZAR-FIM.
           EXIT.

       END PROGRAM RELATORIO.
