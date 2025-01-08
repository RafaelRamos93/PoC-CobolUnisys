       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJEMPLO-UNISYS.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       * Definición de variables
       01 VARIABLE-GRANDE PIC X(10) VALUE 'ABCDEFGHIJ'.
       01 VARIABLE-PEQUENA PIC X(5).
       01 VARIABLE-ENTERA PIC 9(4) VALUE 1234.
       01 VARIABLE-FLOTANTE PIC 9V99 VALUE 3.14.

       PROCEDURE DIVISION.
       BEGIN-PROGRAM.
           DISPLAY "Valor inicial de VARIABLE-GRANDE: " VARIABLE-GRANDE.
           DISPLAY "Valor inicial de VARIABLE-ENTERA: " VARIABLE-ENTERA.
           DISPLAY "Valor inicial de VARIABLE-FLOTANTE: " VARIABLE-FLOTANTE.

           * Asignar valor de VARIABLE-GRANDE a VARIABLE-PEQUENA (pérdida de datos)
           MOVE VARIABLE-GRANDE TO VARIABLE-PEQUENA.
           DISPLAY "Valor asignado a VARIABLE-PEQUENA: " VARIABLE-PEQUENA.

           * Modificar el tamaño de VARIABLE-GRANDE (truncamiento)
           MOVE '12345' TO VARIABLE-GRANDE(1:5).
           DISPLAY "Nuevo valor de VARIABLE-GRANDE: " VARIABLE-GRANDE.

           * Modificar valores enteros y flotantes
           ADD 1 TO VARIABLE-ENTERA.
           MULTIPLY 2 BY VARIABLE-FLOTANTE.
           DISPLAY "Nuevo valor de VARIABLE-ENTERA: " VARIABLE-ENTERA.
           DISPLAY "Nuevo valor de VARIABLE-FLOTANTE: " VARIABLE-FLOTANTE.

           STOP RUN.
