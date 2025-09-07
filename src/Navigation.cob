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
77 SKILLCHOICE  PIC 9 VALUE 0.
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
      *> new line between choices
       MOVE " " TO OUTPUT-REC
       DISPLAY OUTPUT-REC
       WRITE OUTPUT-REC

       MOVE "Search for a job (1)" TO OUTPUT-REC
       DISPLAY OUTPUT-REC
       WRITE OUTPUT-REC

       MOVE "Find someone you know (2)" TO OUTPUT-REC
       DISPLAY OUTPUT-REC
       WRITE OUTPUT-REC

       MOVE "Learn a new skill (3)" TO OUTPUT-REC
       DISPLAY OUTPUT-REC
       WRITE OUTPUT-REC

       MOVE "Exit (9)" TO OUTPUT-REC
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
           *> changed to evaluate to compile (bug)
           EVALUATE CHOICE
               WHEN 1
                   MOVE "Job search/internship is under construction." TO OUTPUT-REC
                   DISPLAY OUTPUT-REC
                   WRITE OUTPUT-REC
               WHEN 2
                   MOVE "Find someone you know is under construction." TO OUTPUT-REC
                   DISPLAY OUTPUT-REC
                   WRITE OUTPUT-REC
               WHEN 3
                   PERFORM SKILL-MENU
               WHEN 9
                   MOVE "Exiting navigation..." TO OUTPUT-REC
                   DISPLAY OUTPUT-REC
                   WRITE OUTPUT-REC
               WHEN OTHER
                   MOVE "Invalid choice, please try again." TO OUTPUT-REC
                   DISPLAY OUTPUT-REC
                   WRITE OUTPUT-REC
           END-EVALUATE
       END-IF
      END-PERFORM
      CLOSE INPUT-FILE
      CLOSE OUTPUT-FILE
      STOP RUN.
SKILL-MENU.
      MOVE 0 TO SKILLCHOICE
      PERFORM UNTIL SKILLCHOICE = 9 OR WS-EOF = "Y"
       MOVE "Learn a New Skill:" TO OUTPUT-REC
       DISPLAY OUTPUT-REC
       WRITE OUTPUT-REC

       MOVE "1. Skill 1" TO OUTPUT-REC
       DISPLAY OUTPUT-REC
       WRITE OUTPUT-REC

       MOVE "2. Skill 2" TO OUTPUT-REC
       DISPLAY OUTPUT-REC
       WRITE OUTPUT-REC

       MOVE "3. Skill 3" TO OUTPUT-REC
       DISPLAY OUTPUT-REC
       WRITE OUTPUT-REC

       MOVE "4. Skill 4" TO OUTPUT-REC
       DISPLAY OUTPUT-REC
       WRITE OUTPUT-REC

       MOVE "5. Skill 5" TO OUTPUT-REC
       DISPLAY OUTPUT-REC
       WRITE OUTPUT-REC

       MOVE "9. Go Back" TO OUTPUT-REC
       DISPLAY OUTPUT-REC
       WRITE OUTPUT-REC

       MOVE "Enter your choice:" TO OUTPUT-REC
       DISPLAY OUTPUT-REC
       WRITE OUTPUT-REC

       READ INPUT-FILE INTO WS-IN
           AT END MOVE "Y" TO WS-EOF
       END-READ

       IF WS-EOF NOT = "Y"
           MOVE FUNCTION NUMVAL (WS-IN) TO SKILLCHOICE
           EVALUATE SKILLCHOICE
               WHEN 1
                   MOVE "This skill is under construction" TO OUTPUT-REC
                   DISPLAY OUTPUT-REC
                   WRITE OUTPUT-REC
               WHEN 2
                   MOVE "This skill is under construction" TO OUTPUT-REC
                   DISPLAY OUTPUT-REC
                   WRITE OUTPUT-REC
               WHEN 3
                   MOVE "This skill is under construction" TO OUTPUT-REC
                   DISPLAY OUTPUT-REC
                   WRITE OUTPUT-REC
                WHEN 3
                   MOVE "This skill is under construction" TO OUTPUT-REC
                   DISPLAY OUTPUT-REC
                   WRITE OUTPUT-REC
                WHEN 4
                   MOVE "This skill is under construction" TO OUTPUT-REC
                   DISPLAY OUTPUT-REC
                   WRITE OUTPUT-REC
                WHEN 5
                   MOVE "This skill is under construction" TO OUTPUT-REC
                   DISPLAY OUTPUT-REC
                   WRITE OUTPUT-REC
               WHEN 9
                   CONTINUE
               WHEN OTHER
                   MOVE "Invalid choice, please try again." TO OUTPUT-REC
                   DISPLAY OUTPUT-REC
                   WRITE OUTPUT-REC
           END-EVALUATE
       END-IF
      END-PERFORM.
