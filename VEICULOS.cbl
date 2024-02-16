       IDENTIFICATION DIVISION.
       PROGRAM-ID. VEICULOS.
      ******************************************************************
      * Author: Wallace
      * Date: 01/02/2024 - Inicio do projeto
      * Date 02/02/2024 - Desenvolvimento do create
      * Date:03/02/2024 - Conserto erros relacionados ao create
      * Date:03/02/2024 - Inicio update
      * Date:04/02/2024 - Trabalhando com erros do update
      * Date:04/02/2024 - Conserto erros update, implementação do READ
      * Date:05/02/2024 - Trabalhando com erros em relação ao ID
      * Date:06/02/2024 - Consertado erro em relação ao ID
      * Date: 07/02/2024 - Erros de open do relato / conserto erros open
      *    16/02/2024 - Implemented a change to the general organization
      *    of the sequential files. Now all of them will be generated
      *    and stored in a other folder in order to improve the
      *    project's organization.
      * Purpose: Elaboração projeto final
      * Tectonics: CBL
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT RELATO ASSIGN TO
       'C:/Users/Theo/Desktop/Escola/Volvo/COBOL/Files/Veiculos.txt'
       FILE STATUS IS AS-STATUS-S.


       DATA DIVISION.
       FILE SECTION.


      *-----------------------------------------------------------------
       FD RELATO
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.

       01 ARQ-RELATO1        PIC X(73).

       01 FILLER REDEFINES ARQ-RELATO1.
               05 ARQ-R-PLACA               PIC X(13).
               05 ARQ-R-MARCA               PIC X(15).
               05 ARQ-R-MODELO              PIC X(15).
               05 ARQ-R-ANO                 PIC X(15).
               05 ARQ-R-PROPRIETARIO        PIC X(15).

      *----------------------------------------------------------------

       WORKING-STORAGE SECTION.

       01 AS-STATUS-E1                  PIC 9(02)  VALUE ZEROS.
       01 AS-STATUS-E2                  PIC 9(02)  VALUE ZEROS.
       01 AS-STATUS-S                   PIC 9(02)  VALUE ZEROS.


       01 WS-CABEC-REL1                 PIC X(73) VALUE ALL '='.
      *-----------------------------------------------------------------
       01 WS-CABEC-REL2.
           05 WS-CABEC-REL2-PGM         PIC X(12)   VALUE'FINALPROJECT'.
           05 WS-CABEC-REL2-FL1         PIC X(18)   VALUE SPACES.
           05 WS-CABEC-REL2-DES         PIC X(09)   VALUE 'VOLVO S.A'.
           05 WS-CABEC-REL2-FL2         PIC X(23)   VALUE SPACES.
           05 WS-CABEC-REL2-DT          PIC X(10)   VALUE SPACES.
      *-----------------------------------------------------------------
       01 WS-CABEC-REL3.
           05 WS-CABEC-REL3-HR         PIC X(08)   VALUE SPACES.
           05 WS-CABEC-REL3-FL1        PIC X(10)   VALUE SPACE.
           05 WS-CABEC-REL3-DES        PIC X(37)   VALUE
                                 'PROJETO FINAL - CURSO DE CAPACITACAO'.

      *-----------------------------------------------------------------

       01 LINDET01-REL.
           05 LINDET01-REL-PLACA           PIC X(05) VALUE 'PLACA'.
           05 LINDET01-REL-SPACE2          PIC X(06) VALUE SPACE.
           05 LINDET01-REL-MARCA           PIC X(08) VALUE 'MARCA'.
           05 LINDET01-REL-SPACE3          PIC X(10) VALUE SPACE.
           05 LINDET01-REL-MODELO          PIC X(08) VALUE 'MODELO'.
           05 LINDET01-REL-SPACE4          PIC X(10) VALUE SPACE.
           05 LINDET01-REL-ANO             PIC X(06) VALUE 'ANO'.
           05 LINDET01-REL-SPACE5          PIC X(01) VALUE SPACE.
           05 LINDET01-REL-PROPRIETARIO  PIC X(12) VALUE 'PROPRIETARIO'.
      *-----------------------------------------------------------------

       01 LINDET02-REL.
           05 LINDET02-REL-PLACA-V             PIC X(08) VALUE SPACE.
           05 LINDET02-REL-SPACE-1             PIC X(03)  VALUE ' | '.
           05 LINDET02-REL-MARCA-V             PIC X(15) VALUE SPACE.
           05 LINDET02-REL-SPACE-2             PIC X(03)  VALUE ' | '.
           05 LINDET02-REL-MODELO-V            PIC X(15) VALUE SPACE.
           05 LINDET02-REL-SPACE-3             PIC X(03)  VALUE ' | '.
           05 LINDET02-REL-ANO-V               PIC X(04) VALUE SPACE.
           05 LINDET02-REL-SPACE-4             PIC X(03)  VALUE ' | '.
           05 LINDET02-REL-PROPRIETARIO-V      PIC X(24) VALUE SPACE.

       01 AS-FIM                       PIC X(04)    VALUE 'NAO'.
       01 AS-FIM-LOOP-MAIN             PIC X(04)    VALUE 'NAO'.
       01 WS-LIDOS                     PIC 9(13)    VALUE ZERO.
       01 AS-DATA                      PIC X(08)    VALUE ZEROS.
       01 AS-HORA                      PIC X(06)    VALUE ZEROS.
       01 OPERACAO                     PIC 9(02)    VALUE ZEROS.
       01 BUSCAR                       PIC X(08)    VALUE ZEROS.
       01 ARMAZENA-RELATO              PIC X(73)    VALUE ZEROS.
       01 ARMAZENA-PLACA               PIC X(08)    VALUE ZEROS.
       01 ARMAZENA-MARCA               PIC X(13)    VALUE ZEROS.
       01 ARMAZENA-MODELO              PIC X(13)    VALUE ZEROS.
       01 ARMAZENA-ANO                 PIC X(04)    VALUE ZEROS.
       01 ARMAZENA-PROPRIETARIO        PIC X(73)    VALUE ZEROS.
       01 OPERACAO-UPDATE              PIC X(02)    VALUE ZEROS.
       01 WS-EXISTE-ARQUIVO            PIC X(01)  VALUE 'N'.


       LINKAGE SECTION.

       01 WS-SUB-OPTION                PIC X(01)   VALUE SPACES.


       PROCEDURE DIVISION USING WS-SUB-OPTION.
       MAIN-PROCEDURE.

            PERFORM 1000-INICIALIZAR.
            PERFORM 2000-PROCESSAR  UNTIL AS-FIM-LOOP-MAIN = 'SIM'.
            PERFORM 3000-FINALIZAR.


            STOP RUN.


       1000-INICIALIZAR           SECTION.

           PERFORM 1100-CHECAR-ARQUIVO-EXISTE.

           OPEN I-O RELATO

           IF WS-EXISTE-ARQUIVO = 'N'

               PERFORM 1200-ABRIR-ARQUIVO-OUTPUT

           ELSE

               CLOSE RELATO

               IF AS-STATUS-S NOT EQUAL ZEROS
                   DISPLAY 'ERRO FECHAR'
                END-IF

               PERFORM 1300-ABRIR-ARQUIVO-EXTEND
           END-IF.

      *-----------------------------------------------------------------

       1000-INICIALIZAR-EXIT.
           EXIT.

        1100-CHECAR-ARQUIVO-EXISTE SECTION.
           OPEN INPUT RELATO
               IF AS-STATUS-S = 0
                   MOVE 'S' TO WS-EXISTE-ARQUIVO
               END-IF
           CLOSE RELATO.

       1100-CHECAR-ARQUIVO-EXISTE-EXIT.
           EXIT.

       1200-ABRIR-ARQUIVO-OUTPUT SECTION.
           OPEN OUTPUT RELATO

           IF AS-STATUS-S NOT EQUAL ZEROS
            DISPLAY 'ERRO AO ABRIR RELATO VEICULOS PARA OUTPUT'
            AS-STATUS-S
           END-IF.

       1200-ABRIR-ARQUIVO-OUTPUT-EXIT.
           EXIT.

       1300-ABRIR-ARQUIVO-EXTEND SECTION.
           OPEN EXTEND RELATO

           IF AS-STATUS-S NOT EQUAL ZEROS
            DISPLAY 'ERRO AO ABRIR RELATO VEICULOS PARA EXTEND'
            AS-STATUS-S
           END-IF.

       1300-ABRIR-ARQUIVO-EXTEND-EXIT.
           EXIT.



       2000-PROCESSAR              SECTION.

           DISPLAY 'QUAL OPERACAO DESEJA FAZER?'
           DISPLAY '(1) - CADASTRAR NOVO VEICULO'
           DISPLAY '(2) - ALTERAR CADASTRO DE VEICULO'
           DISPLAY '(3) - APRESENTAR TODOS OS CADASTROS'
           DISPLAY '(4) - ENCERRAR SISTEMA'
           ACCEPT OPERACAO

           EVALUATE OPERACAO

           WHEN 1

             PERFORM 2100-CREATE-VEICULO

           WHEN 2

             CLOSE RELATO

             IF AS-STATUS-S NOT EQUAL ZEROS

               DISPLAY 'DEU ERRO NO FECHAR RELATO VEICULOS'
               AS-STATUS-S

             END-IF

             PERFORM 2200-UPDATE-VEICULO


           WHEN 3

           CLOSE RELATO
           OPEN INPUT RELATO

           IF AS-STATUS-S NOT EQUAL ZEROS
               DISPLAY 'DEU ERRO NA ABERTURA RELATO' AS-STATUS-S
           END-IF

           PERFORM UNTIL AS-FIM = 'SIM'
               READ RELATO INTO ARMAZENA-RELATO
                   AT END
                          MOVE 'SIM' TO AS-FIM
                          MOVE 'SIM' TO AS-FIM-LOOP-MAIN
                          DISPLAY ''
                          DISPLAY 'PROGRAMA ENCERRADO'
                   NOT AT END
                       DISPLAY ARMAZENA-RELATO
               END-READ
           END-PERFORM

           CLOSE RELATO

           WHEN 4

               CLOSE RELATO
               DISPLAY 'PROGRAMA ENCERRADO'
               MOVE 'SIM' TO AS-FIM-LOOP-MAIN

           WHEN OTHER

               CLOSE RELATO
               DISPLAY 'CARACTERE INVALIDO'
               DISPLAY 'PROGRAMA ENCERRADO'
               MOVE 'SIM' TO AS-FIM-LOOP-MAIN

           .

       2100-CREATE-VEICULO         SECTION.
      *CREATE DE VEICULO
      *----------------------------------------------------------------
           CLOSE RELATO
           IF AS-STATUS-S NOT EQUAL ZEROS
               DISPLAY 'ERRO FECHAR RELATO CREATE'
           END-IF

           OPEN EXTEND RELATO
           IF AS-STATUS-S  NOT EQUAL ZEROS
               DISPLAY 'ERRO AO ABIR RELATO CREATE'
           END-IF

           DISPLAY 'INFORME A PLACA DO VEICULO: '
           ACCEPT LINDET02-REL-PLACA-V
           DISPLAY 'INFORME A MARCA DO VEICULO: '
           ACCEPT LINDET02-REL-MARCA-V
           DISPLAY 'INFORME O MODELO DO VEICULO: '
           ACCEPT LINDET02-REL-MODELO-V
           DISPLAY 'INFORME ANO DO VEICULO: '
           ACCEPT LINDET02-REL-ANO-V
           DISPLAY 'INFORME O PROPRIETARIO DO VEICULO: '
           ACCEPT LINDET02-REL-PROPRIETARIO-V


           MOVE LINDET02-REL TO ARQ-RELATO1
           WRITE ARQ-RELATO1


           DISPLAY ' CADASTRO REALIZADO COM SUCESSO '


             .
       2100-CREATE-VEICULO-EXIT.
           EXIT.
       2200-UPDATE-VEICULO     SECTION.
      *-----------------------------------------------------------------
      * UPDATE DE VEICULO
      *-----------------------------------------------------------------

           DISPLAY 'INFORME A PLACA DO VEICULO: '
           ACCEPT BUSCAR


           MOVE 'NAO' TO AS-FIM
           OPEN I-O RELATO

           IF AS-STATUS-S NOT EQUAL ZEROS

               DISPLAY 'ERRO NA ABERTURA'

           END-IF

           PERFORM UNTIL AS-FIM = 'SIM'


               READ RELATO INTO ARMAZENA-RELATO
                   AT END
                       MOVE 'SIM' TO AS-FIM
                       DISPLAY 'PLACA NAO ENCONTRADA'
                       CLOSE RELATO
                       PERFORM 2200-UPDATE-VEICULO
                   NOT AT END


                       UNSTRING ARMAZENA-RELATO DELIMITED BY SPACES INTO
                       ARMAZENA-PLACA
                       ARMAZENA-MARCA
                       ARMAZENA-MODELO
                       ARMAZENA-ANO
                       ARMAZENA-PROPRIETARIO

                       IF ARMAZENA-PLACA = BUSCAR
                               DISPLAY 'NOVOS DADOS DO VEICULO: '
                               DISPLAY 'PLACA:'
                               ACCEPT ARMAZENA-PLACA
                               DISPLAY 'MARCA:'
                               ACCEPT ARMAZENA-MARCA
                               DISPLAY 'MODELO:'
                               ACCEPT ARMAZENA-MODELO
                               DISPLAY 'ANO:'
                               ACCEPT ARMAZENA-ANO
                               DISPLAY 'PROPRIETARIO:'
                               ACCEPT ARMAZENA-PROPRIETARIO

                               MOVE ARMAZENA-PLACA TO
                               LINDET02-REL-PLACA-V
                               MOVE ARMAZENA-MARCA TO
                               LINDET02-REL-MARCA-V

                               MOVE ARMAZENA-MODELO TO
                               LINDET02-REL-MODELO-V

                               MOVE ARMAZENA-ANO TO
                               LINDET02-REL-ANO-V

                               MOVE ARMAZENA-PROPRIETARIO TO
                               LINDET02-REL-PROPRIETARIO-V


                               MOVE LINDET02-REL TO ARQ-RELATO1
                               REWRITE ARQ-RELATO1
                               DISPLAY 'ALTERACAO FEITA COM SUCESSO'
                               DISPLAY 'DESEJA FAZER MAIS ALTERACOES?'
                               DISPLAY 'DIGITE: '
                               DISPLAY '(1) SIM, FAZER MAIS ALTERACOES'
                               DISPLAY '(2) NAO, ENCERRAR PROGRAMA'
                               ACCEPT OPERACAO-UPDATE

                               EVALUATE OPERACAO-UPDATE

                               WHEN 1
                                   CLOSE RELATO
                                   PERFORM 2200-UPDATE-VEICULO
                               WHEN 2
                                   MOVE 'SIM' TO AS-FIM-LOOP-MAIN
                                   DISPLAY 'PROGRAMA ENCERRADO'
                                   MOVE 'SIM' TO AS-FIM
                                   CLOSE RELATO
                       END-IF
                END-READ
           END-PERFORM


           .
       2200-UPDATE-VEICULO-EXIT.
           EXIT.

       2000-PROCESSAR-EXIT.
           EXIT.


       3000-FINALIZAR              SECTION.


       3000-FINALIZAR-EXIT.
           EXIT.
           EXIT PROGRAM.

       END PROGRAM VEICULOS.
