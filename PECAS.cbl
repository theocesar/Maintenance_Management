       IDENTIFICATION DIVISION.
       PROGRAM-ID. PECAS.
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
      * Date:07/02/2024 - Erros de open do relato
      *    16/02/2024 - Implemented a change to the general organization
      *    of the sequential files. Now all of them will be generated
      *    and stored in a other folder in order to improve the
      *    project's organization.
      *
      * Tectonics: CBL
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT RELATO-PECAS ASSIGN TO
       'ADDRESS'
       FILE STATUS IS AS-STATUS-S.


       DATA DIVISION.
       FILE SECTION.


      *-----------------------------------------------------------------
       FD RELATO-PECAS
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.

       01 ARQ-RELATO1-PECAS        PIC X(73).

      *----------------------------------------------------------------

       WORKING-STORAGE SECTION.

       01 AS-STATUS-S                   PIC 9(02)  VALUE ZEROS.


      *-----------------------------------------------------------------

       01 LINDET02-REL.
           05 LINDET02-REL-ID-P                PIC X(04) VALUE SPACE.
           05 LINDET02-REL-SPACE1              PIC X(03) VALUE ' | '.
           05 LINDET02-REL-MARCA-P             PIC X(11) VALUE SPACE.
           05 LINDET02-REL-SPACE2              PIC X(03) VALUE ' | '.
           05 LINDET02-REL-DESC-P              PIC X(24) VALUE SPACE.
           05 LINDET02-REL-SPACE3              PIC X(03) VALUE ' | '.
           05 LINDET02-REL-QTD-P               PIC 9(04) VALUE ZEROS.
           05 LINDET02-REL-SPACE4              PIC X(03) VALUE ' | '.
           05 LINDET02-REL-VALOR-P             PIC 9(06) VALUE ZEROS.
      *-----------------------------------------------------------------

       01 AS-FIM                       PIC X(04)    VALUE 'NAO'.
       01 AS-FIM-LOOP-MAIN             PIC X(04)    VALUE 'NAO'.
       01 AS-DATA                      PIC X(08)    VALUE ZEROS.
       01 AS-HORA                      PIC X(06)    VALUE ZEROS.
       01 OPERACAO                     PIC 9(02)    VALUE ZEROS.
       01 BUSCAR                       PIC X(08)    VALUE ZEROS.
       01 ARMAZENA-RELATO              PIC X(73)    VALUE ZEROS.
       01 ARMAZENA-ID                  PIC 9(04)    VALUE ZEROS.
       01 ARMAZENA-MARCA               PIC X(15)    VALUE ZEROS.
       01 ARMAZENA-DESC                PIC X(30)    VALUE ZEROS.
       01 ARMAZENA-QTD                 PIC X(04)    VALUE ZEROS.
       01 ARMAZENA-VALOR               PIC X(06)    VALUE ZEROS.
       01 WS-PROXIMO-ID                PIC 9(4)     VALUE 0000.
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


           ACCEPT AS-DATA          FROM  DATE  YYYYMMDD.
           ACCEPT AS-HORA          FROM  TIME.


      *-----------------------------------------------------------------

           PERFORM 1100-CHECAR-ARQUIVO-EXISTE.

           OPEN I-O RELATO-PECAS.

           IF WS-EXISTE-ARQUIVO = 'N'

               PERFORM 1200-ABRIR-ARQUIVO-OUTPUT

               CLOSE RELATO-PECAS
           ELSE

               CLOSE RELATO-PECAS

               IF AS-STATUS-S NOT EQUAL ZEROS
                   DISPLAY 'ERRO FECHAR'
                END-IF

               PERFORM 1300-ABRIR-ARQUIVO-EXTEND
           END-IF.



      *-----------------------------------------------------------------

       1000-INICIALIZAR-EXIT.
           EXIT.

       1100-CHECAR-ARQUIVO-EXISTE SECTION.
           OPEN INPUT RELATO-PECAS
               IF AS-STATUS-S = 0
                   MOVE 'S' TO WS-EXISTE-ARQUIVO
               END-IF
           CLOSE RELATO-PECAS.

       1100-CHECAR-ARQUIVO-EXISTE-EXIT.
           EXIT.

       1200-ABRIR-ARQUIVO-OUTPUT SECTION.
           OPEN OUTPUT RELATO-PECAS.

           IF AS-STATUS-S NOT EQUAL ZEROS
            DISPLAY 'ERRO AO ABRIR RELATO PECAS PARA OUTPUT' AS-STATUS-S
           END-IF.

       1200-ABRIR-ARQUIVO-OUTPUT-EXIT.
           EXIT.

       1300-ABRIR-ARQUIVO-EXTEND SECTION.
           OPEN EXTEND RELATO-PECAS.

           IF AS-STATUS-S NOT EQUAL ZEROS
            DISPLAY 'ERRO AO ABRIR RELATO PECAS PARA EXTEND' AS-STATUS-S
           END-IF.

       1300-ABRIR-ARQUIVO-EXTEND-EXIT.
           EXIT.


       2000-PROCESSAR              SECTION.


           DISPLAY 'QUAL OPERACAO DESEJA FAZER?'
           DISPLAY '(1) - CADASTRAR NOVA PECA'
           DISPLAY '(2) - ALTERAR CADASTRO DE PECA'
           DISPLAY '(3) - APRESENTAR TODOS OS CADASTROS'
           DISPLAY '(4) - ENCERRAR SISTEMA'
           ACCEPT OPERACAO

           EVALUATE OPERACAO
      *-----------------------------------------------------------------
           WHEN 1

             PERFORM 2100-CREATE-PECA
      *-----------------------------------------------------------------
           WHEN 2

             CLOSE RELATO-PECAS

             IF AS-STATUS-S NOT EQUAL ZEROS

               DISPLAY 'DEU ERRO NO FECHAR RELATO PECAS' AS-STATUS-S

             END-IF

             PERFORM 2200-UPDATE-PECA
      *-----------------------------------------------------------------
           WHEN 3

               PERFORM 2300-APRESENTAR-LINHAS


      *-----------------------------------------------------------------
           WHEN 4

            CLOSE RELATO-PECAS

            DISPLAY 'PROGRAMA ENCERRADO'

            MOVE 'SIM' TO AS-FIM-LOOP-MAIN


           WHEN OTHER

               CLOSE RELATO-PECAS

               DISPLAY 'CARACTERE INVALIDO, PROGRAMA ENCERRADO'

               MOVE 'SIM' TO AS-FIM-LOOP-MAIN

           .
      *-----------------------------------------------------------------
       2100-CREATE-PECA            SECTION.

           CLOSE RELATO-PECAS

           IF AS-STATUS-S NOT EQUAL ZEROS

               DISPLAY 'ERRO FECHAR RELATO CREATE'

           END-IF

           OPEN INPUT RELATO-PECAS

           IF AS-STATUS-S  NOT EQUAL ZEROS

               DISPLAY 'ERRO AO ABRIR RELATO CREATE'

           END-IF



           PERFORM UNTIL AS-FIM = 'SIM'
               READ RELATO-PECAS INTO ARMAZENA-RELATO
                   AT END
                       MOVE 'SIM' TO AS-FIM
                   NOT AT END
                       UNSTRING ARMAZENA-RELATO DELIMITED BY SPACES INTO
                           ARMAZENA-ID
                       IF ARMAZENA-ID > WS-PROXIMO-ID
                           MOVE ARMAZENA-ID TO WS-PROXIMO-ID
                       END-IF
               END-READ
           END-PERFORM

           ADD 1 TO WS-PROXIMO-ID

           CLOSE RELATO-PECAS

           IF AS-STATUS-S NOT EQUAL ZEROS

               DISPLAY 'ERRO FECHAR RELATO CREATE'

           END-IF

           OPEN EXTEND RELATO-PECAS

           IF AS-STATUS-S  NOT EQUAL ZEROS

               DISPLAY 'ERRO AO ABIR RELATO CREATE'

           END-IF

      *     DISPLAY WS-PROXIMO-ID

           MOVE WS-PROXIMO-ID TO LINDET02-REL-ID-P

           DISPLAY 'INFORME A MARCA DA PECA: '
           ACCEPT LINDET02-REL-MARCA-P
           DISPLAY 'INFORME A DESCRICAO DA PECA: '
           ACCEPT LINDET02-REL-DESC-P
           DISPLAY 'INFORME A QUANTIDADE: '
           ACCEPT LINDET02-REL-QTD-P
           DISPLAY 'INFORME O VALOR: '
           ACCEPT LINDET02-REL-VALOR-P
           DISPLAY ''


           MOVE LINDET02-REL TO ARQ-RELATO1-PECAS
           WRITE ARQ-RELATO1-PECAS


           DISPLAY ' CADASTRO REALIZADO COM SUCESSO!!'
           DISPLAY ''


            .

       2100-CREATE-PECA-EXIT.
           EXIT.
      *-----------------------------------------------------------------
       2200-UPDATE-PECA            SECTION.

           DISPLAY 'INFORME O ID DA PECA: '

           ACCEPT BUSCAR

           MOVE 'NAO' TO AS-FIM

           OPEN I-O RELATO-PECAS

           IF AS-STATUS-S NOT EQUAL ZEROS

               DISPLAY 'ERRO NA ABERTURA DO RELATO'

           END-IF

           PERFORM UNTIL AS-FIM = 'SIM'


               READ RELATO-PECAS INTO ARMAZENA-RELATO
                   AT END
                       MOVE 'SIM' TO AS-FIM
                       DISPLAY 'ID NAO ENCONTRADO, INFORME UM NOVO ID'
                       CLOSE RELATO-PECAS
                       PERFORM 2200-UPDATE-PECA
                   NOT AT END


                       UNSTRING ARMAZENA-RELATO DELIMITED BY SPACES INTO
                       ARMAZENA-ID
                       ARMAZENA-MARCA
                       ARMAZENA-DESC
                       ARMAZENA-QTD
                       ARMAZENA-VALOR


                       IF ARMAZENA-ID = BUSCAR
                               DISPLAY 'NOVOS DADOS DA PECA: '
                               DISPLAY 'MARCA:'
                               ACCEPT ARMAZENA-MARCA
                               DISPLAY 'DESCRICAO:'
                               ACCEPT ARMAZENA-DESC
                               DISPLAY 'QUANTIDADE:'
                               ACCEPT ARMAZENA-QTD
                               DISPLAY 'VALOR:'
                               ACCEPT ARMAZENA-VALOR

                               MOVE ARMAZENA-ID TO
                               LINDET02-REL-ID-P

                               MOVE ARMAZENA-MARCA TO
                               LINDET02-REL-MARCA-P

                               MOVE ARMAZENA-DESC TO
                               LINDET02-REL-DESC-P

                               MOVE ARMAZENA-QTD TO
                               LINDET02-REL-QTD-P

                               MOVE ARMAZENA-VALOR TO
                               LINDET02-REL-VALOR-P


                               MOVE LINDET02-REL TO ARQ-RELATO1-PECAS
                               REWRITE ARQ-RELATO1-PECAS
                               DISPLAY 'ALTERACAO FEITA COM SUCESSO'
                               DISPLAY 'DESEJA FAZER MAIS ALTERACOES?'
                               DISPLAY 'DIGITE: '
                               DISPLAY '(1) SIM, FAZER MAIS ALTERACOES'
                               DISPLAY '(2) NAO, ENCERRAR PROGRAMA'
                               ACCEPT OPERACAO-UPDATE

                               EVALUATE OPERACAO-UPDATE

                               WHEN 1
                                   CLOSE RELATO-PECAS
                                   PERFORM 2200-UPDATE-PECA
                               WHEN 2
                                   MOVE 'SIM' TO AS-FIM-LOOP-MAIN
                                   DISPLAY 'PROGRAMA ENCERRADO'
                                   MOVE 'SIM' TO AS-FIM
                                   CLOSE RELATO-PECAS
                               WHEN OTHER
                                   MOVE 'SIM' TO AS-FIM-LOOP-MAIN
                                   DISPLAY 'CARACTERE INVALIDO'
                                   DISPLAY 'PROGRAMA ENCERRADO'
                                   MOVE 'SIM' TO AS-FIM
                                   CLOSE RELATO-PECAS
                       END-IF
                END-READ
           END-PERFORM


           .
       2200-UPDATE-PECA-EXIT.
           EXIT.

       2300-APRESENTAR-LINHAS      SECTION.

           MOVE 'NAO' TO AS-FIM

           CLOSE RELATO-PECAS

           OPEN INPUT RELATO-PECAS

           IF AS-STATUS-S NOT EQUAL ZEROS

               DISPLAY 'DEU ERRO NA ABERTURA RELATO' AS-STATUS-S

           END-IF

           PERFORM UNTIL AS-FIM = 'SIM'

               READ RELATO-PECAS INTO ARMAZENA-RELATO

                   AT END
                          MOVE 'SIM' TO AS-FIM

                          MOVE 'SIM' TO AS-FIM-LOOP-MAIN

                          DISPLAY 'PROGRAMA ENCERRADO'

                   NOT AT END

                       DISPLAY ARMAZENA-RELATO

               END-READ

           END-PERFORM

           CLOSE RELATO-PECAS


           .
       2300-APRESENTAR-LINHAS-EXIT.
           EXIT.

       2000-PROCESSAR-EXIT.
           EXIT.


       3000-FINALIZAR              SECTION.



       3000-FINALIZAR-EXIT.
           EXIT.
               EXIT PROGRAM.


       END PROGRAM PECAS.
