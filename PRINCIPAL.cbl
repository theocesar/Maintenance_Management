       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRINCIPAL.
      ******************************************************************
      * Author: Théo César
      * Date: 04/02/2024
      * Purpose: Main program
      * Modifications:
      *    04/02/2024 - Main menu done.
      *    04/02/2024 - Successfully linked clients menu to main program.
      *    05/02/2024 - Tried to put the main menu in a loop, but it
      *    was a failed attempt.
      *    05/02/2024 - Forget the menu inside the loop.
      *    07/02/2024 - Successfully linked all classes to PRINCIPAL.
      *    07/02/2024 - Tried once more to put the main menu in a loop,
      *    but it failed again.
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01 WS-OPCAO                     PIC X(01)   VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           DISPLAY "|---------------------------------------|"
           DISPLAY "|            MENU PRINCIPAL             |"
           DISPLAY "|---------------------------------------|"
           DISPLAY "| 1. Menu de Clientes                   |"
           DISPLAY "| 2. Menu de Veiculos                   |"
           DISPLAY "| 3. Menu de Pecas                      |"
           DISPLAY "| 4. Menu de Manutencoes                |"
           DISPLAY "|                                       |"
           DISPLAY "| PRESS 'S' TO EXIT                     |"
           DISPLAY "|---------------------------------------|"
           DISPLAY "Escolha uma opcao: "
           ACCEPT WS-OPCAO

               EVALUATE WS-OPCAO
           WHEN '1'
               CALL 'CLIENTE' USING WS-OPCAO
           WHEN '2'
               CALL 'VEICULOS' USING WS-OPCAO
           WHEN '3'
               CALL 'PECAS' USING WS-OPCAO
           WHEN '4'
               CALL 'MANUTENCAO' USING WS-OPCAO
           WHEN 'S'
               STOP RUN
           WHEN OTHER
               STOP RUN
           END-EVALUATE

           .
       END PROGRAM PRINCIPAL.
