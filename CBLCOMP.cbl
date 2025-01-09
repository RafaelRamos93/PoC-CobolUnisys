      **********************************************************
      * COBOL-SQL-ESTANDARD                                    *
      *                                                        *
      * LECTURA DE ARCHIVO SAMPLE-FILE   -COPYBOOK SAM-FILE-   *
      *                               PARA FILE DESCRIPCION    *
      *                                                        *
      * ACCESO A LA TABLA SAM-TAB        - INCLUDE DCLSAM -    *
      *                               PARA ESTRUCTURAS SQL     *
      *                                                        *
      * POR CADA REGISTRO LEIDO EN ARCHIVO SAM-FILE            *
      * ACCESA LA TABLA SAM-TAB CON EL ID                      *
      *                                                        *
      **********************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBOLSQL.
       ENVIRONMENT DIVISION.      
       INPUT-OUTPUT SECTION.
000800 FILE-CONTROL.
000900
001000     SELECT SAM-FILE
001100 		  ASSIGN TO "Sample-File"
001200 		  ORGANIZATION IS INDEXED
001300 		  RECORD KEY IS SAM-FILE-KEY-NUMBER
001400 		  ACCESS MODE IS DYNAMIC
                  FILE STATUS WS-SAM-FILE-STATUS.
001500   
001600 DATA DIVISION.
001700 FILE SECTION.
001800
       FD  SAM-FILE
           RECORD NAME SAM-FILE-RECORD
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 96 CHARACTERS.
       01  SAM-FILE-RECORD.
           05 SAM-FILE-KEY-NUMBER 	PIC X(6).
           05 SAM-FILE-NAME    		PIC X(30).
           05 SAM-FILE-ADDRESS   	PIC X(30).
           05 SAM-FILE-CONTACT 		PIC X(30).  

       WORKING-STORAGE SECTION.

       EXEC SQL
            INCLUDE SQLCA
       END-EXEC.
    
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
     
       EXEC SQL
            INCLUDE DCLSAM
       END-EXEC.
 
       EXEC SQL END DECLARE SECTION END-EXEC.
      
       01  WK-AREA.
           05 WK-DATE-CURR. 
              07 WK-DATE-MM           PIC XX   VALUE SPACES.  
              07 FILLER               PIC X    VALUE '-'.
              07 WK-DATE-DD           PIC XX   VALUE SPACES.  
              07 FILLER               PIC X    VALUE '-'.
              07 WK-DATE-YY           PIC XX   VALUE SPACES. 

       01  WX-AREA.
           05 WX-FILE-KEY-NUMBER 	PIC 9(6).
           05 WX-FILE-NAME    		PIC X(30).
           05 WX-FILE-ADDRESS   	PIC X(30).
           05 WX-FILE-CONTACT 		PIC X(30).  

       01  WX-AREA.
           05 WX-KEY-NUMBER             PIC X.
           05 WX-NAME    		PIC X.
           05 WX-ADDRESS                PIC X.
           05 WX-CONTACT 		PIC X.  

       01  WS-AREA.
           05  WS-SAM-FILE-STATUS     PIC XX   VALUE SPACES.
               88 SAM-FILE-EOF                 VALUE '10'.
               88 SAM-FILE-OPEN                VALUE '00'.
 
       PROCEDURE DIVISION.
       0000-PROGRAMA.

           PERFORM 0100-INICIO  THRU 0100-FIN 

           PERFORM 0500-PROCESO THRU 0500-FIN 
                   UNTIL SAM-FILE-EOF.

           PERFORM 0900-TERMINA THRU 0900-FIN.

           STOP RUN.
          
       0100-INICIO.
           MOVE CURRENT-DATE TO WK-DATE-CURR. 
           OPEN INPUT SAM-FILE.
           PERFORM 0120-LEE-SAM-FILE
                   THRU 0120-FIN.
       0100-FIN.
           EXIT.

       0120-LEE-SAM-FILE.
           READ SAM-FILE NEXT AT END
                DISPLAY "PROGRAMA FIN SAM-FILE".
       0120-FIN.
           EXIT.

       0140-SELECT-SAM-TAB.
           MOVE SAM-FILE-KEY-NUMBER TO SAM-ID.
           EXEC SQL
               SELECT SAM_ID, 
                      SAM_NAME,
                      SAM_ADDRESS,
                      SAM_CONTACT
               INTO   :SAM-ID, 
                      :SAM-NAME,
                      :SAM-ADDRESS,
                      :SAM-CONTACT
                 FROM SAM-TAB
                WHERE SAM_ID = :SAM-ID 
           END-EXEC.

           EVALUATE SQLCODE              
               WHEN ZERO           
                    PERFORM 0510-DESPLIEGA THRU 0510-FIN
               WHEN OTHER
                    DISPLAY "PROGRAMA SAME-ID " SAME-ID " NOT FOUND "
           END-EVALUATE.
       0140-FIN.
           EXIT.

       0500-PROCESO.
           MOVE SAM-FILE-ID  TO  SAM-ID.
           PERFORM 0140-SELECT-SAM-TAB THRU 0140-FIN. 
           PERFORM 0120-LEE-SAM-FILE   THRU 0120-FIN.
       0500-FIN.
           EXIT.

       0510-DESPLIEGA.
            MOVE SAM-FILE-KEY-NUMBER  TO WX-FILE-KEY-NUMBER
            MOVE SAM-FILE-NAME        TO WX-FILE-NAME
            MOVE SAM-FILE-ADDRESS     TO WX-FILE-ADDRESS
            MOVE SAM-FILE-CONTACT     TO WX-FILE-CONTACT 

            MOVE SAM-FILE-KEY-NUMBER  TO WX-KEY-NUMBER
            MOVE SAM-FILE-NAME        TO WX-NAME
            MOVE SAM-FILE-ADDRESS     TO WX-ADDRESS
            MOVE SAM-FILE-CONTACT     TO WX-CONTACT 

            DISPLAY SPACES.
            DISPLAY 'CLIENTE : ' SAM-ID ' ' SAM_NAME. 
            DISPLAY 'ADDRESS : ' SAM-ADDRESS. 
            DISPLAY 'CONTACT : ' SAM-CONTACT ' ON ' WK-DATE-CURR. 
            DISPLAY SPACES.
       0510-FIN.
           EXIT.

       0900-TERMINA.
           CLOSE SAM-FILE.
           DISPLAY "PROGRAMA FIN PROCESO"
       0900-FIN.
           EXIT.