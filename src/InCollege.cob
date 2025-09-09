IDENTIFICATION DIVISION.
PROGRAM-ID. InCollege.
AUTHOR. Washington.
DATE-WRITTEN. 09/06/2025.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
    FILE-CONTROL.
        SELECT USERINFO ASSIGN TO "userinfo.txt"
            ORGANIZATION IS SEQUENTIAL
            ACCESS MODE IS SEQUENTIAL
            FILE STATUS IS UINFO-FILE-STATUS.

        SELECT INPUT-FILE ASSIGN TO "InCollege-Input.txt"
            ORGANIZATION IS LINE SEQUENTIAL
            ACCESS MODE IS SEQUENTIAL
            FILE STATUS IS INPUT-FILE-STATUS.

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

    FD INPUT-FILE.
        01 INPUT-REC.
            05 INPUT-TEXT PIC A(32).

    FD APPLOG.
        01 SAVE-RECORD.
            05 SAVE-TEXT PIC A(32).

WORKING-STORAGE SECTION.
    01 UINFO-FILE-STATUS PIC XX.
    01 INPUT-FILE-STATUS PIC XX.
    01 APPLOG-FILE-STATUS PIC XX.
    01 INFOEOF PIC A(1) VALUE 'N'.
    01 ACTIONSEOF PIC A(1) VALUE 'N'.

    01 CURRENT-ACTION Pic X(20).
    01 INFO Pic X(20).
    01 WS-LOGIN Pic A(5) VALUE 'LOGIN'.
    01 WS-NEW Pic A(18) VALUE 'CREATE NEW ACCOUNT'.
    01 WS-NAME PIC A(15).
    01 WS-PASSWORD PIC A(15).
    01 WS-STATUS PIC A(1) VALUE 'N'.   *> Y = logged in, N = not
    01 WS-HASCAPITAL PIC A(1) VALUE 'N'.
    01 WS-HASDIGIT PIC A(1) VALUE 'N'.
    01 WS-HASSPECIAL PIC A(1) VALUE 'N'.
    01 WS-CHARCOUNT PIC 9(2) VALUE 0.
    01 WS-MINPASSWORDCOUNT PIC 9(1) VALUE 8.
    01 WS-MAXPASSWORDCOUNT PIC 9(2) VALUE 12.
    01 WS-INSPECTEDCHAR PIC X(1).
    01 WS-NUMACCOUNTS PIC 9(1) VALUE 0.
    01 I PIC 9(2) VALUE 1.  *>Iterator I variable

    77 CHOICE       PIC 9 VALUE 0.
    77 SKILLCHOICE  PIC 9 VALUE 0.

PROCEDURE DIVISION.

    OPEN INPUT INPUT-FILE.
    OPEN EXTEND APPLOG.

    MOVE "Welcome to InCollege!" TO SAVE-TEXT.
    PERFORM SHOW.
    MOVE "Log In" TO SAVE-TEXT.
    PERFORM SHOW.
    MOVE "Create New Account" TO SAVE-TEXT.
    PERFORM SHOW.

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
        READ INPUT-FILE INTO INPUT-TEXT
            AT END MOVE 'Y' TO ACTIONSEOF
            NOT AT END
                PERFORM PARSEACTION
        END-READ
    END-PERFORM

    CLOSE INPUT-FILE.
    CLOSE APPLOG.
    STOP RUN.

SHOW.
    DISPLAY SAVE-TEXT.
    WRITE SAVE-RECORD.

CHECKPASSWORD.
    MOVE 'N' TO WS-HASDIGIT
    MOVE 'N' TO WS-HASCAPITAL
    MOVE 'N' TO WS-HASSPECIAL

    INSPECT FUNCTION TRIM(IN-PASSWORD) TALLYING WS-CHARCOUNT FOR ALL CHARACTERS.
    IF WS-CHARCOUNT >= WS-MINPASSWORDCOUNT
       AND WS-CHARCOUNT <= WS-MAXPASSWORDCOUNT
       THEN
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
           IF WS-HASCAPITAL = 'Y' AND WS-HASDIGIT = 'Y' AND WS-HASSPECIAL = 'Y' THEN
               MOVE 'Y' TO WS-STATUS
               MOVE "Account created successfully." TO SAVE-TEXT
               PERFORM SHOW
               STRING "Welcome, " DELIMITED BY SIZE IN-USERNAME DELIMITED BY SIZE
                   INTO SAVE-TEXT
               PERFORM SHOW
               PERFORM NAV-MENU
               OPEN EXTEND USERINFO
               WRITE USER-REC
               CLOSE USERINFO
           ELSE
               MOVE "Password requirements not met!" TO SAVE-TEXT
               PERFORM SHOW
           END-IF
    ELSE
       MOVE "Password requirements not met!" TO SAVE-TEXT
       PERFORM SHOW
    END-IF.

AUTH-USER.
    IF IN-USERNAME = WS-NAME THEN
        IF IN-PASSWORD = WS-PASSWORD THEN
            MOVE 'Y' TO WS-STATUS
            MOVE "You have successfully logged in." TO SAVE-TEXT
            PERFORM SHOW
            STRING "Welcome, " DELIMITED BY SIZE WS-NAME DELIMITED BY SIZE
                INTO SAVE-TEXT
            PERFORM SHOW
            PERFORM NAV-MENU
        ELSE
            MOVE "Wrong credentials. Try again." TO SAVE-TEXT
            PERFORM SHOW
        END-IF
    ELSE
        MOVE "Wrong credentials. Try again." TO SAVE-TEXT
        PERFORM SHOW
    END-IF.

PARSEACTION.
    IF INPUT-TEXT = WS-LOGIN THEN
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
                        READ INPUT-FILE INTO INPUT-TEXT
                        MOVE INPUT-TEXT TO WS-NAME
                        MOVE "Please enter your password:" TO SAVE-TEXT
                        PERFORM SHOW
                        READ INPUT-FILE INTO INPUT-TEXT
                        MOVE INPUT-TEXT TO WS-PASSWORD
                        PERFORM AUTH-USER
                END-READ
                CLOSE USERINFO
        END-PERFORM

    ELSE IF INPUT-TEXT = WS-NEW THEN
        IF WS-NUMACCOUNTS < 5 THEN
            READ INPUT-FILE INTO INPUT-TEXT
            MOVE INPUT-TEXT TO IN-USERNAME
            READ INPUT-FILE INTO INPUT-TEXT
            MOVE INPUT-TEXT TO IN-PASSWORD
            PERFORM CHECKPASSWORD
        ELSE
            MOVE "Account limit reached!" TO SAVE-TEXT
            PERFORM SHOW
        END-IF
    END-IF.

NAV-MENU.
    PERFORM UNTIL CHOICE = 9 OR ACTIONSEOF = "Y"
        MOVE " " TO SAVE-TEXT
        PERFORM SHOW

        MOVE "Search for a job (1)" TO SAVE-TEXT
        PERFORM SHOW

        MOVE "Find someone you know (2)" TO SAVE-TEXT
        PERFORM SHOW
        MOVE "Learn a new skill (3)" TO SAVE-TEXT
        PERFORM SHOW

        MOVE "Exit (9)" TO SAVE-TEXT
        PERFORM SHOW

        MOVE "Enter your choice:" TO SAVE-TEXT
        PERFORM SHOW

        READ INPUT-FILE INTO INPUT-TEXT
            AT END MOVE "Y" TO ACTIONSEOF
        END-READ
        IF ACTIONSEOF NOT = "Y"
            MOVE FUNCTION NUMVAL(INPUT-TEXT) TO CHOICE
            EVALUATE CHOICE
                WHEN 1
                    MOVE "Job search/internship is under construction." TO SAVE-TEXT
                    PERFORM SHOW
                WHEN 2
                    MOVE "Find someone you know is under construction." TO SAVE-TEXT
                    PERFORM SHOW
                WHEN 3
                    PERFORM SKILL-MENU
                WHEN 9
                    MOVE "Exiting navigation..." TO SAVE-TEXT
                    PERFORM SHOW
                WHEN OTHER
                    MOVE "Invalid choice, please try again." TO SAVE-TEXT
                    PERFORM SHOW
            END-EVALUATE
        END-IF
    END-PERFORM
    CLOSE INPUT-FILE
    CLOSE APPLOG
    STOP RUN.

SKILL-MENU.
    MOVE 0 TO SKILLCHOICE
    PERFORM UNTIL SKILLCHOICE = 9 OR ACTIONSEOF= "Y"
        MOVE "Learn a New Skill:" TO SAVE-TEXT
        PERFORM SHOW

        MOVE "1. Skill 1" TO SAVE-TEXT
        PERFORM SHOW

        MOVE "2. Skill 2" TO SAVE-TEXT
        PERFORM SHOW

        MOVE "3. Skill 3" TO SAVE-TEXT
        PERFORM SHOW

        MOVE "4. Skill 4" TO SAVE-TEXT
        PERFORM SHOW
        MOVE "5. Skill 5" TO SAVE-TEXT
        PERFORM SHOW

        MOVE "9. Go Back" TO SAVE-TEXT
        PERFORM SHOW
        MOVE "Enter your choice:" TO SAVE-TEXT
        PERFORM SHOW

        READ INPUT-FILE INTO INPUT-TEXT
            AT END MOVE "Y" TO ACTIONSEOF
        END-READ

        IF ACTIONSEOF NOT = "Y"
            MOVE FUNCTION NUMVAL(INPUT-TEXT) TO SKILLCHOICE
            EVALUATE SKILLCHOICE
                WHEN 1
                    MOVE "This skill is under construction" TO SAVE-TEXT
                    PERFORM SHOW
                WHEN 2
                    MOVE "This skill is under construction" TO SAVE-TEXT
                    PERFORM SHOW
                WHEN 3
                    MOVE "This skill is under construction" TO SAVE-TEXT
                    PERFORM SHOW
                WHEN 4
                    MOVE "This skill is under construction" TO SAVE-TEXT
                    PERFORM SHOW
                WHEN 5
                    MOVE "This skill is under construction" TO SAVE-TEXT
                    PERFORM SHOW
                WHEN 9
                    CONTINUE
                WHEN OTHER
                    MOVE "Invalid choice, please try again." TO SAVE-TEXT
                    PERFORM SHOW
            END-EVALUATE
        END-IF
    END-PERFORM.
