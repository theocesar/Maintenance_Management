       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLIENTE.
      ******************************************************************
      * Author: TH�O C�SAR
      * Date: 01/02/2024
      * Purpose: CRUD de Clientes
      * Modifications:
      *    01/02/2024 - CREATE done.
      *    02/02/2024 - READ done.
      *    02/02/2024 - UPDATE done.
      *    02/02/2024 - Having problems with DELETE. Don't know how to
      *    fix it.
      *    02/02/2024 - Succeed in writing the remaining clients to
      *    another file. Still unable to override the content of the
      *    main file.
      *    03/02/2024 - Gave up on overriding the main file. The
      *    solution now is manually switch between the two files.
      *    03/02/2024 - Implementing additional checks after closing or
      *    opening a file.
      *    04/02/2024 - Adding the possibility of updating and deleting
      *    more than one client.
      *    04/02/2024 - Fixed a file closing bug and implemented the
      *    file opening verification for RELATO-NOVO.
      *    05/02/2024 - Fixed a file opening bug in the delete section.
      *    05/02/2024 - Fixed a bug on the initial data checking.
      *    05/02/2024 - Successfully placed the menu in a loop.
      *    05/02/2024 - Adding comments to sections in INITIALIZATION.
      *    05/02/2024 - Don't make the menu inside a loop. Major bug as
      *    a result of it.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT RELATO ASSIGN TO
       'C:/Users/Theo/Desktop/Escola/Volvo/COBOL/Clientes.txt'
       ORGANIZATION IS SEQUENTIAL
       FILE STATUS IS AS-STATUS-S.

       SELECT RELATO-NOVO ASSIGN TO
       'C:/Users/Theo/Desktop/Escola/Volvo/COBOL/Clientes1.txt'
       ORGANIZATION IS SEQUENTIAL
       FILE STATUS IS AS-STATUS-S1.

       DATA DIVISION.
       FILE SECTION.

       FD RELATO
          RECORDING MODE IS F
          BLOCK CONTAINS 0 RECORDS.

       01 ARQ-RELATO-LINHA                 PIC X(62).

       FD RELATO-NOVO
          RECORDING MODE IS F
          BLOCK CONTAINS 0 RECORDS.

       01 ARQ-RELATO-NOVO                  PIC X(62).

       WORKING-STORAGE SECTION.

       01 AS-STATUS-S                      PIC 9(02)   VALUE ZEROS.
       01 AS-STATUS-S1                     PIC 9(02)   VALUE ZEROS.

      *-----------------------------------------------------------------
      *                    DEFINI��O DE CABE�ALHO
      *-----------------------------------------------------------------

       01 WS-CABEC-REL1                    PIC X(60) VALUE ALL '='.

       01 WS-CABEC-REL2.
           05 WS-CABEC-REL2-NOME           PIC X(05) VALUE 'NOME'.
           05 WS-CABEC-REL2-SPACE1         PIC X(10) VALUE SPACES.
           05 WS-CABEC-REL2-CPF            PIC X(03) VALUE 'CPF'.
           05 WS-CABEC-REL2-SPACE2         PIC X(10) VALUE SPACES.
           05 WS-CABEC-REL2-ENDERECO       PIC X(08) VALUE 'ENDERECO'.
           05 WS-CABEC-REL2-SPACE3         PIC X(10) VALUE SPACES.
           05 WS-CABEC-REL2-TELEFONE       PIC X(08) VALUE 'TELEFONE'.

      *-----------------------------------------------------------------
      *                    DEFINI��O DE CABE�ALHO
      *-----------------------------------------------------------------


      *-----------------------------------------------------------------
      *                    DEFINI��O DE DETALHE
      *-----------------------------------------------------------------
       01 LINDET01-REL.
           05 LINDET01-REL-NOME            PIC X(10)   VALUE SPACES.
           05 LINDET01-REL-SPACE1          PIC X(03)   VALUE ' | '.
           05 LINDET01-REL-CPF             PIC 9(11)   VALUE ZEROS.
           05 LINDET01-REL-SPACE2          PIC X(03)   VALUE ' | '.
           05 LINDET01-REL-ENDERECO        PIC X(15)   VALUE SPACES.
           05 LINDET01-REL-SPACE3          PIC X(02)   VALUE '| '.
           05 LINDET01-REL-TELEFONE        PIC X(13)   VALUE SPACES.


       01 WS-LINDET-SPACES                 PIC X(60)   VALUE ALL SPACES.
       01 WS-LINDET-STATUS                 PIC X(01)   VALUE SPACES.

      *-----------------------------------------------------------------
      *                    DEFINI��O DE DETALHE
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
      *                    DEFINI��O DAS VARIAVEIS
      *-----------------------------------------------------------------

       01 AS-FIM                           PIC X(01)   VALUE 'N'.
       01 WS-FIM                           PIC X(01)   VALUE 'N'.
       01 WS-OPCAO                         PIC 9(01)   VALUE ZEROS.
       01 WS-AUX                           PIC X(10)   VALUE ZEROS.
       01 WS-EXISTE-DADOS                  PIC X(01)   VALUE 'N'.
       01 WS-CLOSE-FILE                    PIC X(01)   VALUE 'S'.
       01 WS-DADOS                         PIC X(62)   VALUE SPACES.
       01 WS-CPF-BUSCA                     PIC 9(11)   VALUE ZEROS.
       01 WS-CONTINUE-UPDATE               PIC X(01)   VALUE 'S'.


       LINKAGE SECTION.

       01 WS-SUB-OPTION                        PIC 9(01).
      *-----------------------------------------------------------------
      *                    DEFINI��O DAS VARIAVEIS
      *-----------------------------------------------------------------


      *-----------------------------------------------------------------
      *                        MAIN PROCEDURE
      *-----------------------------------------------------------------

       PROCEDURE DIVISION USING WS-SUB-OPTION.

           PERFORM 1000-INICIALIZAR
           PERFORM 3000-FINALIZAR
           .

      *-----------------------------------------------------------------
      *                        MAIN PROCEDURE
      *-----------------------------------------------------------------


      *-----------------------------------------------------------------
      *                        INICIALIZA��O
      *-----------------------------------------------------------------

       1000-INICIALIZAR                    SECTION.

           PERFORM 1050-VERIFICAR-DADOS

           IF WS-EXISTE-DADOS = 'S'
               OPEN EXTEND RELATO
               PERFORM 1125-VERIFICAR-ABERTURA

               OPEN OUTPUT RELATO-NOVO
               PERFORM 1200-ABERTURA-SEC
           ELSE
               OPEN OUTPUT RELATO
               PERFORM 1125-VERIFICAR-ABERTURA

               OPEN OUTPUT RELATO-NOVO
               PERFORM 1200-ABERTURA-SEC

           END-IF

           PERFORM 1100-DISPLAY-MENU

            .
       1000-INICIALIZAR-FIM.
            EXIT.


      *    SE��O PARA VERIFICAR SE O ARQUIVO RELATO EXISTE OU N�O
      *    SE ELE EXISTIR, RETORNA SE ELE ESTA VAZIO OU N�O
       1050-VERIFICAR-DADOS        SECTION.

           OPEN INPUT RELATO
           IF AS-STATUS-S <> 35
               CLOSE RELATO
               IF AS-STATUS-S NOT EQUALS ZEROS
                    DISPLAY 'DEU ERRO NO FECHAMENTO ' AS-STATUS-S
               END-IF

               OPEN INPUT RELATO
               PERFORM 1125-VERIFICAR-ABERTURA

               READ RELATO
                   AT END
                       MOVE 'N' TO WS-EXISTE-DADOS
                   NOT AT END
                       MOVE 'S' TO WS-EXISTE-DADOS
               END-READ

               CLOSE RELATO
               IF AS-STATUS-S NOT EQUALS ZEROS
                    DISPLAY 'DEU ERRO NO FECHAMENTO ' AS-STATUS-S
               END-IF

           ELSE
               OPEN OUTPUT RELATO
               PERFORM 1125-VERIFICAR-ABERTURA

               CLOSE RELATO
               PERFORM 1150-VERIFICAR-FECHAMENTO

               OPEN INPUT RELATO
               PERFORM 1125-VERIFICAR-ABERTURA

                   READ RELATO
                   AT END
                       MOVE 'N' TO WS-EXISTE-DADOS
                   NOT AT END
                       MOVE 'S' TO WS-EXISTE-DADOS
               END-READ

               CLOSE RELATO
               IF AS-STATUS-S NOT EQUALS ZEROS
                    DISPLAY 'DEU ERRO NO FECHAMENTO ' AS-STATUS-S
               END-IF
           END-IF
           .
       1050-VERIFICAR-DADOS-FIM.
           EXIT.


      * SE��O PARA VERIFICAR SE O ARQUIVO RELATO-NOVO ESTA VAZIO OU N�O.
       1075-EXISTE-DADOS-NOVO              SECTION.

           CLOSE RELATO-NOVO
           IF AS-STATUS-S1 NOT EQUALS ZEROS
               DISPLAY 'DEU ERRO NO FECHAMENTO: ' AS-STATUS-S1
           END-IF

           OPEN INPUT RELATO-NOVO
           PERFORM 1200-ABERTURA-SEC

           READ RELATO-NOVO
               AT END
                   MOVE 'N' TO WS-EXISTE-DADOS
               NOT AT END
                   MOVE 'S' TO WS-EXISTE-DADOS
           END-READ

           CLOSE RELATO-NOVO
           IF AS-STATUS-S1 NOT EQUALS ZEROS
               DISPLAY 'DEU ERRO NO FECHAMENTO: ' AS-STATUS-S1
           END-IF
           .
       1075-EXISTE-DADOS-NOVO-FIM.
           EXIT.

       1100-DISPLAY-MENU                   SECTION.

           DISPLAY "|---------------------------------------|"
           DISPLAY "|            MENU DE CLIENTES           |"
           DISPLAY "|---------------------------------------|"
           DISPLAY "| 1. Cadastrar Cliente                  |"
           DISPLAY "| 2. Consultar Cliente                  |"
           DISPLAY "| 3. Atualizar Cliente                  |"
           DISPLAY "| 4. Excluir Cliente                    |"
           DISPLAY "| 5. Sair                               |"
           DISPLAY "|---------------------------------------|"
           DISPLAY "Escolha uma opcao:"
           ACCEPT WS-OPCAO

               EVALUATE TRUE
           WHEN WS-OPCAO = 1
               PERFORM 2100-CADASTRAR-CLIENTE UNTIL WS-AUX = 'N'
           WHEN WS-OPCAO = 2
               PERFORM 2200-CONSULTAR-CLIENTE
           WHEN WS-OPCAO = 3
               PERFORM 2300-ATUALIZAR-CLIENTE UNTIL WS-AUX = 'N'
           WHEN WS-OPCAO = 4
               PERFORM 2400-EXCLUIR-CLIENTE UNTIL WS-AUX = 'N'
               PERFORM 2500-SOBESCREVER-ARQ
           WHEN WS-OPCAO = 5
               PERFORM 3000-FINALIZAR
           WHEN OTHER
               DISPLAY 'ESCOLHA INVALIDA'
               PERFORM 3000-FINALIZAR
           END-EVALUATE
           .
       1100-DISPLAY-MENU-FIM.
           EXIT.


      * SE��O PARA VERIFICAR O STATUS DE ABERTURA DO ARQUIVO RELATO
       1125-VERIFICAR-ABERTURA             SECTION.

           IF AS-STATUS-S NOT EQUALS ZEROS
               DISPLAY 'DEU ERRO NA ABERTURA ' AS-STATUS-S
           END-IF

           .
       1125-VERIFICAR-ABERTURA-FIM.
           EXIT.


      * SE��O PARA VERIFICAR O STATUS DE FECHAMENTO DO ARQUIVO RELATO
       1150-VERIFICAR-FECHAMENTO           SECTION.

           IF AS-STATUS-S NOT EQUALS ZEROS
               DISPLAY 'DEU ERRO NO FECHAMENTO ' AS-STATUS-S
           END-IF

           .
       1150-VERIFICAR-FECHAMENTO-FIM.
           EXIT.


      * SE��O PARA VERIFICAR O STATUS DE ABERTURA DO ARQUIVO RELATO-NOVO
       1200-ABERTURA-SEC                   SECTION.

           IF AS-STATUS-S1 NOT EQUALS ZEROS
               DISPLAY 'DEU ERRO NA ABERTURA ' AS-STATUS-S1
           END-IF

           .
       1200-ABERTURA-SEC-FIM.
           EXIT.


      *-----------------------------------------------------------------
      *                            INICIALIZA��O
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
      *                            PROCESSAMENTO
      *-----------------------------------------------------------------

       2100-CADASTRAR-CLIENTE              SECTION.

           DISPLAY "NOME: "
           ACCEPT LINDET01-REL-NOME
           DISPLAY "CPF: "
           ACCEPT LINDET01-REL-CPF
           DISPLAY "ENDERECO: "
           ACCEPT LINDET01-REL-ENDERECO
           DISPLAY "TELEFONE: "
           ACCEPT LINDET01-REL-TELEFONE

           MOVE LINDET01-REL TO ARQ-RELATO-LINHA
           WRITE ARQ-RELATO-LINHA

           MOVE WS-LINDET-SPACES TO ARQ-RELATO-LINHA
           WRITE ARQ-RELATO-LINHA

           DISPLAY "Deseja cadastrar outro cliente? (S/N)"
           ACCEPT WS-AUX
           IF WS-AUX = 'N'
               DISPLAY 'CLIENTE(s) CADASTRADO(s) COM SUCESSO'
               MOVE 'S' TO AS-FIM
           END-IF
            .
       2100-CADASTRAR-CLIENTE-FIM.
           EXIT.

       2200-CONSULTAR-CLIENTE              SECTION.

           PERFORM 1050-VERIFICAR-DADOS

           IF WS-EXISTE-DADOS = 'S'

               OPEN INPUT RELATO
               PERFORM 1125-VERIFICAR-ABERTURA

               PERFORM UNTIL WS-CLOSE-FILE = 'N'
                   READ RELATO INTO WS-DADOS
                       AT END
                           MOVE 'N' TO WS-CLOSE-FILE
                       NOT AT END
                           DISPLAY WS-DADOS
                   END-READ
               END-PERFORM
           ELSE
               DISPLAY 'NAO HA CLIENTES CADASTRADOS'
           END-IF

           MOVE 'S' TO AS-FIM
           .
       2200-CONSULTAR-CLIENTE-FIM.
       EXIT.

       2300-ATUALIZAR-CLIENTE              SECTION.

           PERFORM 1050-VERIFICAR-DADOS
           IF WS-EXISTE-DADOS = 'S'
               OPEN I-O RELATO
               PERFORM 1125-VERIFICAR-ABERTURA
               PERFORM UNTIL WS-CONTINUE-UPDATE = 'N'

                   DISPLAY "Informe o CPF do cliente a ser atualizado: "
                   ACCEPT WS-CPF-BUSCA

                   PERFORM UNTIL WS-CLOSE-FILE = 'N'

                       READ RELATO INTO WS-DADOS
                           AT END
                               MOVE 'N' TO WS-CLOSE-FILE
                           NOT AT END
                               UNSTRING WS-DADOS DELIMITED BY '|' INTO
                                                   LINDET01-REL-NOME
                                                   LINDET01-REL-CPF
                                                   LINDET01-REL-ENDERECO
                                                   LINDET01-REL-TELEFONE


                               IF LINDET01-REL-CPF = WS-CPF-BUSCA
                                   DISPLAY "CLIENTE ENCONTRADO!"
                                   DISPLAY
                                   "-------------------------------"
                                   DISPLAY "Novos dados do cliente:"
                                   DISPLAY "Nome: "
                                   ACCEPT LINDET01-REL-NOME
                                   DISPLAY "CPF: "
                                   ACCEPT LINDET01-REL-CPF
                                   DISPLAY "Endereco: "
                                   ACCEPT LINDET01-REL-ENDERECO
                                   DISPLAY "Telefone: "
                                   ACCEPT LINDET01-REL-TELEFONE

                                   STRING LINDET01-REL-NOME DELIMITED
                                   BY SIZE
                                   ' | ' DELIMITED BY SIZE
                                   LINDET01-REL-CPF DELIMITED BY SIZE
                                   ' | ' DELIMITED BY SIZE
                                   LINDET01-REL-ENDERECO DELIMITED BY
                                   SIZE
                                   '| ' DELIMITED BY SIZE
                                   LINDET01-REL-TELEFONE DELIMITED BY
                                   SIZE
                                   INTO LINDET01-REL

                                   MOVE LINDET01-REL TO ARQ-RELATO-LINHA
                                   REWRITE ARQ-RELATO-LINHA
                                   EXIT PERFORM
                               END-IF
                       END-READ
                   END-PERFORM

                   DISPLAY "Deseja atualizar outro cliente? (S/N)"
                   ACCEPT WS-CONTINUE-UPDATE
               END-PERFORM
           ELSE
               DISPLAY 'NAO HA CLIENTES CADASTRADOS'
           END-IF

           MOVE 'N' TO WS-AUX
           DISPLAY 'CLIENTE(s) ATUALIZADO(s) COM SUCESSO'

           .
       2300-ATUALIZAR-CLIENTE-FIM.
           EXIT.


       2400-EXCLUIR-CLIENTE SECTION.

           DISPLAY "Informe o CPF do cliente a ser excluido: "
           ACCEPT WS-CPF-BUSCA

           PERFORM 1050-VERIFICAR-DADOS
           IF WS-EXISTE-DADOS = 'S'
               OPEN INPUT RELATO
               PERFORM 1125-VERIFICAR-ABERTURA

               PERFORM UNTIL WS-CLOSE-FILE = 'N'
                   READ RELATO INTO WS-DADOS
                       AT END
                           MOVE 'N' TO WS-CLOSE-FILE
                       NOT AT END
                           UNSTRING WS-DADOS DELIMITED BY '|' INTO
                                               LINDET01-REL-NOME
                                               LINDET01-REL-CPF
                                               LINDET01-REL-ENDERECO
                                               LINDET01-REL-TELEFONE
                           IF LINDET01-REL-CPF = WS-CPF-BUSCA
                               CONTINUE
                           ELSE
                               STRING WS-DADOS DELIMITED BY SIZE
                                   INTO ARQ-RELATO-NOVO
                               WRITE ARQ-RELATO-NOVO
                           END-IF
               END-PERFORM
           ELSE
               DISPLAY 'NAO HA CLIENTES CADASTRADOS'
           END-IF

           DISPLAY "Deseja excluir outro cliente? (S/N)"
           ACCEPT WS-AUX
           IF WS-AUX = 'N'
               DISPLAY 'CLIENTE(s) EXCLUIDO(s) COM SUCESSO'
           END-IF
           .

       2400-EXCLUIR-CLIENTE-FIM.
           EXIT.

       2500-SOBESCREVER-ARQ                SECTION.

           CLOSE RELATO
           PERFORM 1150-VERIFICAR-FECHAMENTO

           PERFORM 1075-EXISTE-DADOS-NOVO
           IF WS-EXISTE-DADOS = 'S'
               OPEN INPUT RELATO-NOVO
               PERFORM 1200-ABERTURA-SEC

               OPEN I-O RELATO
               PERFORM 1125-VERIFICAR-ABERTURA

               PERFORM UNTIL WS-CLOSE-FILE = 'N'
                   READ RELATO-NOVO INTO WS-DADOS
                       AT END
                           MOVE 'N' TO WS-CLOSE-FILE
                       NOT AT END
                           MOVE WS-DADOS TO ARQ-RELATO-LINHA
                           REWRITE ARQ-RELATO-LINHA
                   END-READ
               END-PERFORM
           END-IF


           .
       2500-SOBESCREVER-ARQ-FIM.
            EXIT.


      *-----------------------------------------------------------------
      *                            PROCESSAMENTO
      *-----------------------------------------------------------------


      *-----------------------------------------------------------------
      *                            FINALIZACAO
      *-----------------------------------------------------------------

       3000-FINALIZAR                      SECTION.

           CLOSE RELATO
           IF AS-STATUS-S NOT EQUALS ZEROS
                DISPLAY 'DEU ERRO NO FECHAMENTO ' AS-STATUS-S
           END-IF

           CLOSE RELATO-NOVO
           IF AS-STATUS-S1 NOT EQUALS ZEROS
               DISPLAY 'DEU ERRO NO FECHAMENTO: ' AS-STATUS-S1
           END-IF

            EXIT PROGRAM
           .
       3000-FINALIZAR-FIM.
           EXIT.

      *-----------------------------------------------------------------
      *                            FINALIZACAO
      *-----------------------------------------------------------------
       END PROGRAM CLIENTE.