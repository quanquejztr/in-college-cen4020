*> This is free-form
IDENTIFICATION DIVISION.
PROGRAM-ID. AUTH.
AUTHOR. Washington.
DATE-WRITTEN. 09/06/2025.

ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT USERINFO ASSIGN TO "userinfo.dat"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS UINFO-FILE-STATUS.

               SELECT USERACTIONS ASSIGN TO "useractions.dat"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS UACTIONS-FILE-STATUS.

               SELECT APPLOG ASSIGN TO "InCollege-Output.txt"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS APPLOG-FILE-STATUS.

DATA DIVISION.
       FILE SECTION.
           FD USERINFO.
               01 USER-REC.
                   05 IN-USERNAME PIC X(20).
                   05 IN-PASSWORD PIC X(20).

           FD USERACTIONS.
               01 ACTION-RECORD.
                   05 ACTION-TEXT PIC A(32).

           FD  APPLOG.
               01  SAVE-RECORD.
                   05  SAVE-TEXT PIC A(32).


       WORKING-STORAGE SECTION.
           01 UINFO-FILE-STATUS PIC XX.
           01 UACTIONS-FILE-STATUS PIC XX.
           01 APPLOG-FILE-STATUS PIC XX.
           01 INFOEOF PIC A(1).
           01 ACTIONSEOF PIC A(1).

       01     CURRENT-ACTION Pic X(20).
       01     INFO Pic X(20).
       01     WS-LOGIN Pic A(5) VALUE 'LOGIN'.
       01     WS-NEW Pic A(18) VALUE 'CREATE NEW ACCOUNT'.
       01     WS-NAME PIC A(15).
       01     WS-PASSWORD PIC A(15).
       01     WS-STATUS PIC A(1) VALUE 'N'.   *> Y = logged in, N = not
       01     WS-HASCAPITAL PIC A(1) VALUE 'N'.
       01     WS-HASDIGIT PIC A(1) VALUE 'N'.
       01     WS-HASSPECIAL PIC A(1) VALUE 'N'.
       01     WS-CHARCOUNT PIC 9(2) VALUE 0.
       01     WS-MINPASSWORDCOUNT PIC 9(1) VALUE 8.
       01     WS-MAXPASSWORDCOUNT PIC 9(2) VALUE 12.
       01     WS-INSPECTEDCHAR PIC X(1).
       01     WS-NUMACCOUNTS PIC 9(1) VALUE 0.
       01     I PIC 9(2) VALUE 1.  *>Iterator I variable


PROCEDURE DIVISION.

    OPEN INPUT USERACTIONS.
    OPEN EXTEND APPLOG.

    MOVE "Welcome to InCollege!" TO SAVE-TEXT.
    PERFORM SHOW.
    MOVE "Log In" TO SAVE-TEXT.
    PERFORM SHOW.
    MOVE "Create New Account" TO SAVE-TEXT.
    PERFORM SHOW.

    *>PERFORM PARSEACTION UNTIL ACTIONSEOF='Y'
    OPEN INPUT USERINFO.
           PERFORM UNTIL INFOEOF='Y'
              READ USERINFO INTO USER-REC
                  AT END MOVE 'Y' TO INFOEOF
                  NOT AT END
                      ADD 1 TO WS-NUMACCOUNTS
              END-READ
           END-PERFORM
    CLOSE USERINFO.

    PERFORM UNTIL ACTIONSEOF='Y'
       READ USERACTIONS INTO ACTION-RECORD
           AT END MOVE 'Y' TO ACTIONSEOF
           NOT AT END
               PERFORM PARSEACTION
       END-READ
    END-PERFORM

    CLOSE USERACTIONS.
    CLOSE APPLOG.
    STOP RUN.

SHOW.
DISPLAY SAVE-TEXT.
WRITE SAVE-RECORD.

LOGIN.

SIGNIN.

CHECKPASSWORD.
MOVE 'N' TO WS-HASDIGIT
MOVE 'N' TO WS-HASCAPITAL
MOVE 'N' TO WS-HASSPECIAL

INSPECT FUNCTION TRIM(IN-PASSWORD) TALLYING WS-CHARCOUNT FOR ALL CHARACTERS.
IF WS-CHARCOUNT >= WS-MINPASSWORDCOUNT THEN
       IF WS-CHARCOUNT <= WS-MAXPASSWORDCOUNT THEN
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH OF FUNCTION TRIM(IN-PASSWORD)
               MOVE FUNCTION TRIM(IN-PASSWORD)(I:1) TO WS-INSPECTEDCHAR
               IF WS-INSPECTEDCHAR >= 'A' AND WS-INSPECTEDCHAR <= 'Z' THEN
                   MOVE 'Y' TO WS-HASCAPITAL

               ELSE IF WS-INSPECTEDCHAR >= '0' AND WS-INSPECTEDCHAR <= '9' THEN
                   MOVE 'Y' TO WS-HASDIGIT

               ELSE IF WS-INSPECTEDCHAR >= '!' AND WS-INSPECTEDCHAR <= '/' THEN
                   MOVE 'Y' TO WS-HASSPECIAL

               END-IF
           END-PERFORM
           IF WS-HASCAPITAL IS EQUAL TO 'Y' AND WS-HASDIGIT IS EQUAL TO 'Y' AND WS-HASSPECIAL IS EQUAL TO 'Y' THEN
               MOVE 'Y' TO WS-STATUS
               MOVE "Account created successfully." TO SAVE-TEXT
               PERFORM SHOW
               STRING "Welcome, " DELIMITED BY SIZE IN-USERNAME DELIMITED BY SIZE INTO SAVE-TEXT
               PERFORM SHOW
               OPEN EXTEND USERINFO    *> Write new user info.
               WRITE USER-REC
               END-WRITE
               CLOSE USERINFO
               DISPLAY 'DATA WRITTEN'
           ELSE
               MOVE "Password requirements not met!" TO SAVE-TEXT
               PERFORM SHOW
           END-IF
       ELSE
           MOVE "Password requirements not met!" TO SAVE-TEXT
           PERFORM SHOW
       END-IF
ELSE
       MOVE "Password requirements not met!" TO SAVE-TEXT
       PERFORM SHOW
END-IF.

AUTH-USER.
IF IN-USERNAME IS EQUAL TO WS-NAME THEN
    IF IN-PASSWORD IS EQUAL TO WS-PASSWORD THEN
        MOVE 'Y' TO WS-STATUS
        MOVE "You have successfully logged in." TO SAVE-TEXT
        PERFORM SHOW
        STRING "Welcome, " DELIMITED BY SIZE WS-NAME DELIMITED BY SIZE INTO SAVE-TEXT
        PERFORM SHOW
    ELSE
        MOVE "Wrong credentials. Try again." TO SAVE-TEXT
        PERFORM SHOW
    END-IF
ELSE
    MOVE "Wrong credentials. Try again." TO SAVE-TEXT
    PERFORM SHOW
END-IF.

PARSEACTION.
IF ACTION-TEXT IS EQUAL TO WS-LOGIN THEN
       PERFORM UNTIL INFOEOF='Y'
              IF WS-STATUS = 'Y' THEN
                   MOVE 'Y' TO INFOEOF
              ELSE
                   OPEN INPUT USERINFO
                   READ USERINFO INTO USER-REC
                   AT END MOVE 'Y' TO INFOEOF
                   NOT AT END
                        MOVE "Please enter your username:" TO SAVE-TEXT
                        PERFORM SHOW
                        READ USERACTIONS INTO ACTION-RECORD
                        END-READ
                        MOVE ACTION-TEXT TO WS-NAME
                        MOVE "Please enter your password:" TO SAVE-TEXT
                        PERFORM SHOW
                        READ USERACTIONS INTO ACTION-RECORD
                        END-READ
                        MOVE ACTION-TEXT TO WS-PASSWORD
                        PERFORM AUTH-USER
                   END-READ
                   CLOSE USERINFO
       END-PERFORM

ELSE IF ACTION-TEXT IS EQUAL TO WS-NEW THEN
       IF WS-NUMACCOUNTS < 5 THEN
           READ USERACTIONS INTO ACTION-RECORD
           END-READ
           MOVE ACTION-TEXT TO IN-USERNAME
           READ USERACTIONS INTO ACTION-RECORD
           END-READ
           MOVE ACTION-TEXT TO IN-PASSWORD
           PERFORM CHECKPASSWORD
       ELSE
           MOVE "Account limit reached!" TO SAVE-TEXT
           PERFORM SHOW
       END-IF
END-IF.

