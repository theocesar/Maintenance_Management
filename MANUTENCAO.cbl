       IDENTIFICATION DIVISION.
       PROGRAM-ID. MANUTENCAO.
      ******************************************************************
      * Author: Théo César
      * Date: 04/02/2024
      * Purpose: Maintenance program
      * Modifications:
      *    04/02/2024 - Created the initial structure
      *    05/02/2024 - Initial structure consolidated.
      *    05/02/2024 - CREATE done.
      *    05/02/2024 - READ done.
      *    05/02/2024 - Fixed a open/close file bug.
      *    05/02/2024 - Implemented changes to the output file.
      *    06/02/2024 - Added two more inputs to each maintenance. The
      *    ID and the amount of parts used.
      *    06/02/2024 - READ active maintenances done.
      *    06/02/2024 - Generated the pseudo-code to calculate the
      *    maintenance costs.
      *    06/02/2024 - Organized the sections.
      *    06/02/2024 - Created sections to organize the open/close file
      *    validations.
      *    07/02/2024 - Costs calculation done.
      *    07/02/2024 - Costs report also done.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT MAINTENANCE ASSIGN TO
       'C:/Users/Theo/Desktop/Escola/Volvo/COBOL/Manutencao.txt'
       ORGANIZATION IS SEQUENTIAL
       FILE STATUS IS AS-STATUS-S.

       SELECT PECAS ASSIGN TO
       'C:/Users/Theo/Desktop/Escola/Volvo/COBOL/Relatorio-pecas.txt'
       ORGANIZATION IS SEQUENTIAL
       FILE STATUS IS AS-STATUS-E1.

       SELECT VEICULOS ASSIGN TO
       'C:/Users/Theo/Desktop/Escola/Volvo/COBOL/Relatorio-veiculos.txt'
       ORGANIZATION IS SEQUENTIAL
       FILE STATUS IS AS-STATUS-E2.

       DATA DIVISION.
       FILE SECTION.

       FD MAINTENANCE
          RECORDING MODE IS F
          BLOCK CONTAINS 0 RECORDS.

       01 AGENDA-MANUTENCAO-RECORD         PIC X(62).


       FD PECAS
          RECORDING MODE IS F
          BLOCK CONTAINS 0 RECORDS.

       01 ARQ-PECAS                        PIC X(73).


       FD VEICULOS
          RECORDING MODE IS F
          BLOCK CONTAINS 0 RECORDS.

       01 ARQ-VEICULOS                     PIC X(78).


       WORKING-STORAGE SECTION.

       01 AS-STATUS-S                      PIC 9(2)    VALUE ZEROS.
       01 AS-STATUS-E1                     PIC 9(2)    VALUE ZEROS.
       01 AS-STATUS-E2                     PIC 9(2)    VALUE ZEROS.

      *-----------------------------------------------------------------
      *                    DEFINIÇÃO DE CABEÇALHO
      *-----------------------------------------------------------------

       01 WS-CABEC-REL1                    PIC X(60) VALUE ALL '='.

       01 WS-CABEC-REL2.
           05 WS-CABEC-REL2-PLACA          PIC X(05) VALUE 'PLACA'.
           05 WS-CABEC-REL2-SPACE1         PIC X(02) VALUE SPACES.
           05 WS-CABEC-REL2-DATA           PIC X(04) VALUE 'DATA'.
           05 WS-CABEC-REL2-SPACE2         PIC X(02) VALUE SPACES.
           05 WS-CABEC-REL2-TIPO-SERVICO   PIC X(15) VALUE
                                                   'TIPO DO SERVICO'.
           05 WS-CABEC-REL2-SPACE3         PIC X(02) VALUE SPACES.
           05 WS-CABEC-REL2-STATUS         PIC X(06) VALUE 'STATUS'.
           05 WS-CABEC-REL2-SPACE4         PIC X(02) VALUE SPACES.
           05 WS-CABEC-REL2-PECA           PIC X(07) VALUE 'ID-PECA'.
           05 WS-CABEC-REL2-SPACE5         PIC X(02) VALUE SPACES.
           05 WS-CABEC-REL2-QTD            PIC X(03) VALUE 'QTD'.

      *-----------------------------------------------------------------
      *                    DEFINIÇÃO DE CABEÇALHO
      *-----------------------------------------------------------------


      *-----------------------------------------------------------------
      *                    DEFINIÇÃO DE DETALHE
      *-----------------------------------------------------------------

       01 LINDET01-REL.
           05 LINDET01-REL-PLACA           PIC X(08)   VALUE SPACES.
           05 LINDET01-REL-SPACE1          PIC X(01)   VALUE '|'.
           05 LINDET01-REL-DATA            PIC X(10)   VALUE ZEROS.
           05 LINDET01-REL-SPACE2          PIC X(01)   VALUE '|'.
           05 LINDET01-REL-TIPO-SERVICO    PIC X(10)   VALUE SPACES.
           05 LINDET01-REL-SPACE3          PIC X(01)   VALUE '|'.
           05 LINDET01-REL-STATUS          PIC X(10)   VALUE SPACES.
           05 LINDET01-REL-SPACE4          PIC X(01)   VALUE '|'.
           05 LINDET01-REL-PECA            PIC X(06)   VALUE SPACES.
           05 LINDET01-REL-SPACE5          PIC X(01)   VALUE '|'.
           05 LINDET01-REL-QTD             PIC 9(04)   VALUE ZEROS.

       01 WS-LINDET-SPACES                 PIC X(60)   VALUE ALL SPACES.

      *-----------------------------------------------------------------
      *                    DEFINIÇÃO DE DETALHE
      *-----------------------------------------------------------------


      *-----------------------------------------------------------------
      *                    DEFINIÇÃO DAS VARIAVEIS
      *-----------------------------------------------------------------

       01 WS-INPUT-STRING                  PIC X(50)   VALUE SPACES.
       01 WS-EXISTE-DADOS                  PIC X(01)   VALUE SPACES.
       01 WS-CLOSE-FILE                    PIC X(01)   VALUE 'S'.
       01 WS-CLOSE-PECA                    PIC X(01)   VALUE 'S'.
       01 WS-CLOSE-VEICULO                 PIC X(01)   VALUE 'S'.
       01 WS-DADOS                         PIC X(62)   VALUE SPACES.
       01 WS-OPCAO                         PIC X(01)   VALUE SPACES.
       01 WS-NOVO-STATUS                   PIC X(12)   VALUE SPACES.
       01 WS-PLACA                         PIC X(10)   VALUE SPACES.
       01 WS-VALOR-PECAS                   PIC 9(06)   VALUE ZEROS.
       01 WS-VALOR-TOTAL                   PIC 9(06)   VALUE ZEROS.
       01 WS-DADOS-PECAS                   PIC X(73)   VALUE SPACES.
       01 WS-DADOS-VEICULOS                PIC X(78)   VALUE SPACES.
       01 WS-VEICULO-CL                    PIC X(15)   VALUE SPACES.


       01 WS-PECA1                         PIC X(06)   VALUE SPACES.
       01 WS-MARCA2                        PIC X(11)   VALUE SPACES.
       01 WS-DESC3                         PIC X(24)   VALUE SPACES.
       01 WS-QTD4                          PIC 9(04)   VALUE ZEROS.
       01 WS-VALOR5                        PIC 9(06)   VALUE ZEROS.

       01 ARQ-R-PLACA                      PIC X(13)   VALUE SPACES.
       01 ARQ-R-MARCA                      PIC X(15)   VALUE SPACES.
       01 ARQ-R-MODELO                     PIC X(15)   VALUE SPACES.
       01 ARQ-R-ANO                        PIC X(15)   VALUE SPACES.
       01 ARQ-R-PROPRIETARIO               PIC X(15)   VALUE SPACES.

       01 WS-PREV-VALOR                    PIC 9(06) VALUE 300.
       01 WS-CORR-VALOR                    PIC 9(06) VALUE 600.



       LINKAGE SECTION.

       01 WS-SUB-OPTION                    PIC X(01)   VALUE SPACES.
      *-----------------------------------------------------------------
      *                    DEFINIÇÃO DAS VARIAVEIS
      *-----------------------------------------------------------------


      *-----------------------------------------------------------------
      *                        MAIN PROCEDURE
      *-----------------------------------------------------------------

       PROCEDURE DIVISION USING WS-SUB-OPTION.
       MAIN-PROCEDURE.

           PERFORM 1000-INICIALIZAR
           PERFORM 3000-FINALIZAR

           .
      *-----------------------------------------------------------------
      *                        MAIN PROCEDURE
      *-----------------------------------------------------------------


      *-----------------------------------------------------------------
      *                        INICIALIZAÇÃO
      *-----------------------------------------------------------------

       1000-INICIALIZAR                        SECTION.

           PERFORM 1050-VERIFICAR-DADOS

           IF WS-EXISTE-DADOS = 'S'
               OPEN EXTEND MAINTENANCE
               PERFORM 1200-VERIFICAR-ABERTURA

           ELSE
               OPEN OUTPUT MAINTENANCE
               PERFORM 1200-VERIFICAR-ABERTURA

               MOVE WS-CABEC-REL1 TO AGENDA-MANUTENCAO-RECORD
               WRITE AGENDA-MANUTENCAO-RECORD

               MOVE WS-CABEC-REL2 TO AGENDA-MANUTENCAO-RECORD
               WRITE AGENDA-MANUTENCAO-RECORD

               MOVE WS-CABEC-REL1 TO AGENDA-MANUTENCAO-RECORD
               WRITE AGENDA-MANUTENCAO-RECORD

           END-IF

           PERFORM 1100-DISPLAY-MENU

           .
       1000-INICIALIZAR-FIM.
           EXIT.

       1050-VERIFICAR-DADOS                    SECTION.

           OPEN INPUT MAINTENANCE
           IF AS-STATUS-S <> 35
               CLOSE MAINTENANCE
               PERFORM 1300-VERIFICAR-FECHAMENTO

               OPEN INPUT MAINTENANCE
               PERFORM 1200-VERIFICAR-ABERTURA

               READ MAINTENANCE
                   AT END
                       MOVE 'N' TO WS-EXISTE-DADOS
                   NOT AT END
                       MOVE 'S' TO WS-EXISTE-DADOS
               END-READ

               CLOSE MAINTENANCE
               PERFORM 1300-VERIFICAR-FECHAMENTO

           ELSE
               OPEN OUTPUT MAINTENANCE
               PERFORM 1200-VERIFICAR-ABERTURA

               CLOSE MAINTENANCE
               PERFORM 1300-VERIFICAR-FECHAMENTO

               OPEN INPUT MAINTENANCE
               PERFORM 1200-VERIFICAR-ABERTURA

               READ MAINTENANCE
                   AT END
                       MOVE 'N' TO WS-EXISTE-DADOS
                   NOT AT END
                       MOVE 'S' TO WS-EXISTE-DADOS
               END-READ

               CLOSE MAINTENANCE
               PERFORM 1300-VERIFICAR-FECHAMENTO
           END-IF
           .
       1050-VERIFICAR-DADOS-FIM.
           EXIT.

       1100-DISPLAY-MENU                       SECTION.

           DISPLAY "|---------------------------------------|"
           DISPLAY "|          MENU DE MANUTENCOES          |"
           DISPLAY "|---------------------------------------|"
           DISPLAY "| 1. Agendar Manutencao                 |"
           DISPLAY "| 2. Consultar manutencoes ativas.      |"
           DISPLAY "| 3. Atualizar o status da manutencao . |"
           DISPLAY "| 4. Consultar historico de manutencoes.|"
           DISPLAY "| 5. Gerar relatorio de custos.         |"
           DISPLAY "|                                       |"
           DISPLAY "| Press 'S' to exit                     |"
           DISPLAY "|---------------------------------------|"
           DISPLAY "Escolha uma opcao:"
           ACCEPT WS-OPCAO

               EVALUATE WS-OPCAO
           WHEN '1'
               PERFORM 2000-AGENDAR-MANUTENCAO
           WHEN '2'
               PERFORM 2300-CONSULTAR-ATIVAS
           WHEN '3'
               PERFORM 2200-ATUALIZAR-STATUS
           WHEN '4'
               PERFORM 2100-CONSULTAR-HISTORICO
           WHEN '5'
               PERFORM 2400-CALCULAR-CUSTOS
               DISPLAY "RELATORIO GERADO COM SUCESSO!"
           WHEN '9'
               PERFORM 3000-FINALIZAR
           WHEN OTHER DISPLAY "Opção invalida. Tente novamente."
           END-EVALUATE
           .
       1100-DISPLAY-MENU-FIM.
           EXIT.

       1200-VERIFICAR-ABERTURA                 SECTION.

           IF AS-STATUS-S NOT EQUALS ZEROS
               DISPLAY "DEU ERRO NA ABERTURA " AS-STATUS-S
           END-IF

           .
       1200-VERIFICAR-ABERTURA-FIM.
           EXIT.

       1300-VERIFICAR-FECHAMENTO               SECTION.

           IF AS-STATUS-S NOT EQUALS ZEROS
               DISPLAY "DEU ERRO NO FECHAMENTO " AS-STATUS-S
           END-IF

           .
       1300-VERIFICAR-FECHAMENTO-FIM.
           EXIT.

      *-----------------------------------------------------------------
      *                        INICIALIZAÇÃO
      *-----------------------------------------------------------------


      *-----------------------------------------------------------------
      *                        PROCESSAMENTO
      *-----------------------------------------------------------------

       2000-AGENDAR-MANUTENCAO                 SECTION.

           DISPLAY "Digite a placa do veiculo: "
           ACCEPT LINDET01-REL-PLACA
           DISPLAY "Digite a data da manutencao (DD/MM/AAAA): "
           ACCEPT LINDET01-REL-DATA
           DISPLAY "Digite o tipo de servico: "
           ACCEPT LINDET01-REL-TIPO-SERVICO
           MOVE "AGENDADO" TO LINDET01-REL-STATUS
           DISPLAY "Digite o ID da peca que vai ser utilizada: "
           ACCEPT LINDET01-REL-PECA
           DISPLAY "Digite a quantidade da peca utilizada: "
           ACCEPT LINDET01-REL-QTD

           MOVE LINDET01-REL TO AGENDA-MANUTENCAO-RECORD
           WRITE AGENDA-MANUTENCAO-RECORD

           MOVE WS-LINDET-SPACES TO AGENDA-MANUTENCAO-RECORD
           WRITE AGENDA-MANUTENCAO-RECORD

           DISPLAY "Manutencao agendada com sucesso!"
           .
       2000-AGENDAR-MANUTENCAO-FIM.
           EXIT.

       2100-CONSULTAR-HISTORICO                SECTION.

           CLOSE MAINTENANCE
           PERFORM 1300-VERIFICAR-FECHAMENTO

           OPEN INPUT MAINTENANCE
           PERFORM 1200-VERIFICAR-ABERTURA

           PERFORM UNTIL WS-CLOSE-FILE = 'N'
               READ MAINTENANCE INTO WS-DADOS
                   AT END
                       MOVE 'N' TO WS-CLOSE-FILE
                   NOT AT END
                       DISPLAY WS-DADOS
               END-READ
           END-PERFORM
           .
       2100-CONSULTAR-HISTORICO-FIM.
           EXIT.

       2200-ATUALIZAR-STATUS                   SECTION.

           DISPLAY "Qual a placa do veiculo em manutencao?"
           ACCEPT WS-PLACA

           PERFORM 1050-VERIFICAR-DADOS
           IF WS-EXISTE-DADOS = 'S'

               OPEN I-O MAINTENANCE
               PERFORM 1200-VERIFICAR-ABERTURA

               PERFORM UNTIL WS-CLOSE-FILE = 'N'
                   READ MAINTENANCE INTO WS-DADOS
                       AT END
                           MOVE 'N' TO WS-CLOSE-FILE
                       NOT AT END
                           UNSTRING WS-DADOS DELIMITED BY '|' INTO
                                               LINDET01-REL-PLACA
                                               LINDET01-REL-DATA
                                               LINDET01-REL-TIPO-SERVICO
                                               LINDET01-REL-STATUS
                                               LINDET01-REL-PECA
                                               LINDET01-REL-QTD

                           IF LINDET01-REL-PLACA EQUALS WS-PLACA

                               DISPLAY
                                   "Qual o novo status da manutencao?"
                               ACCEPT WS-NOVO-STATUS

                               MOVE WS-NOVO-STATUS TO
                                                   LINDET01-REL-STATUS

                               STRING LINDET01-REL-PLACA DELIMITED
                               BY SIZE
                               '|' DELIMITED BY SIZE
                               LINDET01-REL-DATA DELIMITED BY SIZE
                               '|' DELIMITED BY SIZE
                               LINDET01-REL-TIPO-SERVICO DELIMITED BY
                               SIZE
                               '|' DELIMITED BY SIZE
                               LINDET01-REL-STATUS DELIMITED BY SIZE
                               '|' DELIMITED BY SIZE
                               LINDET01-REL-PECA DELIMITED BY SIZE
                               '|' DELIMITED BY SIZE
                               LINDET01-REL-QTD DELIMITED BY SIZE
                               INTO LINDET01-REL

                               MOVE LINDET01-REL TO
                                               AGENDA-MANUTENCAO-RECORD
                               REWRITE AGENDA-MANUTENCAO-RECORD
                               MOVE 'N' TO WS-CLOSE-FILE
                            END-IF
                   END-READ
               END-PERFORM
           ELSE
               DISPLAY 'NAO HA MANUTENCOES AGENDADAS'
           END-IF

           DISPLAY 'STATUS ATUALIZADO COM SUCESSO'
           .
       2200-ATUALIZAR-STATUS-FIM.
           EXIT.

       2300-CONSULTAR-ATIVAS                   SECTION.

           CLOSE MAINTENANCE
           PERFORM 1300-VERIFICAR-FECHAMENTO

           OPEN INPUT MAINTENANCE
           PERFORM 1200-VERIFICAR-ABERTURA

           PERFORM UNTIL WS-CLOSE-FILE = 'N'
               READ MAINTENANCE INTO WS-DADOS
                   AT END
                       MOVE 'N' TO WS-CLOSE-FILE
                   NOT AT END
                       UNSTRING WS-DADOS DELIMITED BY '|' INTO
                                               LINDET01-REL-PLACA
                                               LINDET01-REL-DATA
                                               LINDET01-REL-TIPO-SERVICO
                                               LINDET01-REL-STATUS
                                               LINDET01-REL-PECA
                                               LINDET01-REL-QTD

                       IF LINDET01-REL-STATUS = 'ATIVO'
                           DISPLAY WS-DADOS
                       END-IF
               END-READ
           END-PERFORM
           .
       2300-CONSULTAR-ATIVAS-FIM.
           EXIT.

       2400-CALCULAR-CUSTOS                    SECTION.

           DISPLAY "Qual a placa do veiculo em manutencao?"
           ACCEPT WS-PLACA

           CLOSE MAINTENANCE
           PERFORM 1300-VERIFICAR-FECHAMENTO

           OPEN INPUT MAINTENANCE
           PERFORM 1200-VERIFICAR-ABERTURA

           PERFORM UNTIL WS-CLOSE-FILE = 'N'
               READ MAINTENANCE INTO WS-DADOS
                   AT END
                       MOVE 'N' TO WS-CLOSE-FILE
                   NOT AT END
                       UNSTRING WS-DADOS DELIMITED BY '|' INTO
                                               LINDET01-REL-PLACA
                                               LINDET01-REL-DATA
                                               LINDET01-REL-TIPO-SERVICO
                                               LINDET01-REL-STATUS
                                               LINDET01-REL-PECA
                                               LINDET01-REL-QTD

                       IF LINDET01-REL-STATUS = 'ATIVO' AND
                           LINDET01-REL-PLACA = WS-PLACA

                           IF LINDET01-REL-TIPO-SERVICO = 'PREVENTIVO'


                               PERFORM 2500-VALOR-PECAS
                               COMPUTE WS-VALOR-TOTAL = WS-VALOR-PECAS *
                                                  LINDET01-REL-QTD +
                                                  WS-PREV-VALOR

                               PERFORM 2600-VEICULO-PROP
                               CALL 'RELATORIO' USING
                               WS-VEICULO-CL,
                               LINDET01-REL-PLACA,
                               LINDET01-REL-TIPO-SERVICO,
                               LINDET01-REL-DATA, WS-VALOR-TOTAL
                           ELSE

                               PERFORM 2500-VALOR-PECAS
                               COMPUTE WS-VALOR-TOTAL = WS-VALOR-PECAS *
                                                  LINDET01-REL-QTD +
                                                  WS-CORR-VALOR

                               PERFORM 2600-VEICULO-PROP
                               CALL 'RELATORIO' USING
                               WS-VEICULO-CL,
                               LINDET01-REL-PLACA,
                               LINDET01-REL-TIPO-SERVICO,
                               LINDET01-REL-DATA, WS-VALOR-TOTAL
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           .
       2400-CALCULAR-CUSTOS-FIM.
           EXIT.

       2500-VALOR-PECAS                        SECTION.

           OPEN INPUT PECAS
           IF AS-STATUS-E1 NOT EQUALS ZERO
               DISPLAY "DEU ERRO NA ABERTURA " AS-STATUS-E1
           END-IF

           PERFORM UNTIL WS-CLOSE-PECA = 'N'
               READ PECAS INTO WS-DADOS-PECAS
                   AT END
                       MOVE 'N' TO WS-CLOSE-PECA
                   NOT AT END
                       UNSTRING WS-DADOS-PECAS DELIMITED BY '|' INTO
                                               WS-PECA1
                                               WS-MARCA2
                                               WS-DESC3
                                               WS-QTD4
                                               WS-VALOR5


                       IF LINDET01-REL-PECA = WS-PECA1
                           MOVE WS-VALOR5 TO WS-VALOR-PECAS
                           MOVE 'N' TO WS-CLOSE-PECA
                       END-IF
               END-READ
           END-PERFORM

           CLOSE PECAS
           IF AS-STATUS-E1 NOT EQUALS ZEROS
               DISPLAY "DEU ERRO NO FECHAMENTO" AS-STATUS-E1
           END-IF
           .
       2500-VALOR-PECAS-FIM.
           EXIT.

       2600-VEICULO-PROP                       SECTION.

           OPEN INPUT VEICULOS
           IF AS-STATUS-E2 NOT EQUALS ZEROS
               DISPLAY "DEU ERRO NA ABERTURA " AS-STATUS-E2
           END-IF

           PERFORM UNTIL WS-CLOSE-VEICULO = 'N'
               READ VEICULOS INTO WS-DADOS-VEICULOS
                   AT END
                       MOVE 'N' TO WS-CLOSE-VEICULO
                   NOT AT END
                       UNSTRING WS-DADOS-VEICULOS DELIMITED BY '|' INTO
                                               ARQ-R-PLACA
                                               ARQ-R-MARCA
                                               ARQ-R-MODELO
                                               ARQ-R-ANO
                                               ARQ-R-PROPRIETARIO

                       IF LINDET01-REL-PLACA = ARQ-R-PLACA
                           MOVE ARQ-R-PROPRIETARIO TO WS-VEICULO-CL
                           MOVE 'N' TO WS-CLOSE-VEICULO
                       END-IF
               END-READ
           END-PERFORM

           CLOSE VEICULOS
           IF AS-STATUS-E2 NOT EQUALS ZEROS
               DISPLAY "DEU ERRO NO FECHAMENTO " AS-STATUS-E2
           END-IF
           .
       2600-VEICULO-PROP-FIM.
           EXIT.

      *-----------------------------------------------------------------
      *                        PROCESSAMENTO
      *-----------------------------------------------------------------


      *-----------------------------------------------------------------
      *                        FINALIZACAO
      *-----------------------------------------------------------------

       3000-FINALIZAR                          SECTION.

           CLOSE MAINTENANCE
           IF AS-STATUS-S NOT EQUALS ZEROS
               DISPLAY "DEU ERRO NO FECHAMENTO " AS-STATUS-S
           END-IF

            EXIT PROGRAM
           .
       3000-FINALIZAR-FIM.
           EXIT.

      *-----------------------------------------------------------------
      *                        FINALIZACAO
      *-----------------------------------------------------------------

       END PROGRAM MANUTENCAO.
