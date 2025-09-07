IDENTIFICATION DIVISION.
PROGRAM-ID. PostLoginNav.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
      SELECT INPUT-FILE ASSIGN TO "InCollege-Input.txt"
       ORGANIZATION IS LINE SEQUENTIAL.
      SELECT OUTPUT-FILE ASSIGN TO "InCollege-Output.txt"
       ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD INPUT-FILE.
01 INPUT-REC        PIC X(80).
FD OUTPUT-FILE.
01 OUTPUT-REC       PIC X(120).

WORKING-STORAGE SECTION.
77 CHOICE       PIC 9 VALUE 0.
77 WS-EOF       PIC X VALUE "N".
77 WS-IN        PIC X(80).

PROCEDURE DIVISION.
      OPEN INPUT INPUT-FILE
      OPEN OUTPUT OUTPUT-FILE

      MOVE "You have successfully logged in." TO OUTPUT-REC
      DISPLAY OUTPUT-REC
      WRITE OUTPUT-REC

      MOVE "Welcome, student!" TO OUTPUT-REC
      DISPLAY OUTPUT-REC
      WRITE OUTPUT-REC

      PERFORM UNTIL CHOICE = 9 OR WS-EOF = "Y"


       MOVE "Search for a job   (1)" TO OUTPUT-REC
       DISPLAY OUTPUT-REC
       WRITE OUTPUT-REC

       MOVE "Find someone you know   (2)" TO OUTPUT-REC
       DISPLAY OUTPUT-REC
       WRITE OUTPUT-REC

       MOVE "Exit   (9)" TO OUTPUT-REC
       DISPLAY OUTPUT-REC
       WRITE OUTPUT-REC

       MOVE "Enter your choice:" TO OUTPUT-REC
       DISPLAY OUTPUT-REC
       WRITE OUTPUT-REC

       READ INPUT-FILE INTO WS-IN
           AT END MOVE "Y" TO WS-EOF
       END-READ

       IF WS-EOF NOT = "Y"
           MOVE FUNCTION NUMVAL (WS-IN) TO CHOICE

           IF CHOICE = 1
               MOVE "Job search/internship is under construction." TO OUTPUT-REC
               DISPLAY OUTPUT-REC
               WRITE OUTPUT-REC
           ELSE
               IF CHOICE = 2
                   MOVE "Find someone you know is under construction." TO OUTPUT-REC
                   DISPLAY OUTPUT-REC
                   WRITE OUTPUT-REC
               ELSE
                   IF CHOICE = 9
                       MOVE "Exiting navigation..." TO OUTPUT-REC
                       DISPLAY OUTPUT-REC
                       WRITE OUTPUT-REC
                   ELSE
                       MOVE "Invalid choice, please try again." TO OUTPUT-REC
                       DISPLAY OUTPUT-REC
                       WRITE OUTPUT-REC
                   END-IF
                END-IF
           END-IF
       END-IF
      END-PERFORM



       CLOSE INPUT-FILE
       CLOSE OUTPUT-FILE
       STOP RUN.
