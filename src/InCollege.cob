>>SOURCE FORMAT FREE
*> NOTES: Min year 1990??
IDENTIFICATION DIVISION.
PROGRAM-ID. InCollege.
*> AUTHOR. Washington.
*> DATE-WRITTEN. 09/06/2025.
*> This is the program header section that identifies the program name and metadata

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
*> Define file for storing user account information (username/password pairs)
    SELECT USERINFO ASSIGN TO "userinfo.txt"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS UINFO-FILE-STATUS.

*> Define input file for reading test commands and user input
    SELECT INPUT-FILE ASSIGN TO "InCollege-Test.txt"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS INPUT-FILE-STATUS.

*> Define output file for logging application messages and responses
    SELECT APPLOG ASSIGN TO "InCollege-Output.txt"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS APPLOG-FILE-STATUS.

    *> Profile persistence files
    SELECT PROFILES ASSIGN TO "profiles.txt"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS PROFILES-FILE-STATUS.

    SELECT TEMP-FILE ASSIGN TO "profiles.tmp"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS TEMP-FILE-STATUS.


DATA DIVISION.
FILE SECTION.
*> File description for user account data
FD USERINFO.
01 USER-REC.
    05 IN-USERNAME PIC X(20).
    05 IN-PASSWORD PIC X(20).

*> File description for input commands
FD INPUT-FILE.
01 INPUT-REC.
    05 INPUT-TEXT PIC X(24).

*> File description for application log output
FD APPLOG.
01 SAVE-RECORD.
    05 SAVE-TEXT PIC X(64).

*> Profile data file (one line per record, simple key=value style later)
FD PROFILES.
01 PROFILES-LINE PIC X(256).

*> Temporary file for updates (used when editing existing profiles)
FD TEMP-FILE.
01 TEMP-LINE PIC X(256).


WORKING-STORAGE SECTION.
*> File status indicators
01 UINFO-FILE-STATUS PIC XX.
01 INPUT-FILE-STATUS PIC XX.
01 APPLOG-FILE-STATUS PIC XX.

*> End-of-file flags
01 INFOEOF PIC A(1) VALUE 'N'.
01 INPUTSEOF PIC A(1) VALUE 'N'.

*> Action tracking and command constants
01 CURRENT-ACTION PIC X(20).
01 WS-LOGIN PIC X(5) VALUE 'LOGIN'.
01 WS-NEW   PIC X(18) VALUE 'CREATE NEW ACCOUNT'.

*> User authentication variables
01 WS-NAME PIC X(20).
01 WS-PASSWORD PIC X(20).
01 WS-LOGGEDIN PIC A(1) VALUE 'N'.

01 PROFILES-FILE-STATUS PIC XX.
01 TEMP-FILE-STATUS     PIC XX.

*> Buffers for parsing profile lines (key=value pairs later)
01 LINE-K PIC X(32).
01 LINE-V PIC X(224).
01 WS-BUF PIC X(256).

*> Flag to mark if a profile exists for current user
01 PROFILE-FOUND PIC A(1) VALUE 'N'.


*> Password validation flags
01 WS-HASCAPITAL PIC A(1) VALUE 'N'.
01 WS-HASDIGIT   PIC A(1) VALUE 'N'.
01 WS-HASSPECIAL PIC A(1) VALUE 'N'.

*> Password validation counters and limits
01 WS-CHARCOUNT  PIC 9(2) VALUE 0.
01 WS-MINPASSWORDCOUNT PIC 9(2) VALUE 8.
01 WS-MAXPASSWORDCOUNT PIC 9(2) VALUE 12.
01 WS-INSPECTEDCHAR PIC X(1).

*> Account management variables
01 WS-NUMACCOUNTS PIC 9(1) VALUE 0.
01 WS-NEWUSERNAME PIC X(20).
01 WS-UNIQUEUSERSTATUS PIC A(1) VALUE 'N'.
01 I PIC 9(2) VALUE 1.

*> Menu choice variables
77 CHOICE       PIC 9 VALUE 0.
77 SKILLCHOICE  PIC 9 VALUE 0.
01 P-REC.
   05 P-USERNAME      PIC X(20).      *> set after login (WS-NAME)
   05 P-FIRST-NAME    PIC X(30).
   05 P-LAST-NAME     PIC X(30).
   05 P-UNIVERSITY    PIC X(60).
   05 P-MAJOR         PIC X(40).
   05 P-GRAD-YEAR     PIC 9(4).
   05 P-ABOUT         PIC X(200).

   05 P-EXP-COUNT     PIC 9 VALUE 0.
   05 P-EXPERIENCE OCCURS 3.
      10 P-EXP-TITLE     PIC X(40).
      10 P-EXP-COMPANY   PIC X(40).
      10 P-EXP-DATES     PIC X(30).
      10 P-EXP-DESC      PIC X(100).

   05 P-EDU-COUNT     PIC 9 VALUE 0.
   05 P-EDU OCCURS 3.
      10 P-EDU-DEGREE    PIC X(40).
      10 P-EDU-SCHOOL    PIC X(60).
      10 P-EDU-YEARS     PIC X(20).

01 VALID-YEAR PIC A(1) VALUE 'N'.
01 MIN-YEAR   PIC 9(4) VALUE 1900.
01 MAX-YEAR   PIC 9(4) VALUE 2100.
01 P-I        PIC 9 VALUE 0.

PROCEDURE DIVISION.
*> Main program execution
    OPEN INPUT PROFILES
    IF PROFILES-FILE-STATUS = "00"
        CLOSE PROFILES
    ELSE
        *> If file missing (35), create an empty one. Any other code: warn.
        IF PROFILES-FILE-STATUS = "35"
            OPEN OUTPUT PROFILES
            CLOSE PROFILES
        ELSE
            MOVE "Warning: Could not open profiles file." TO SAVE-TEXT
            PERFORM SHOW
        END-IF
    END-IF

    CLOSE PROFILES

    MOVE "Welcome to InCollege!" TO SAVE-TEXT
    PERFORM SHOW
    MOVE "Log In" TO SAVE-TEXT
    PERFORM SHOW
    MOVE "Create New Account" TO SAVE-TEXT
    PERFORM SHOW

*> Count existing accounts
    OPEN INPUT USERINFO
        PERFORM UNTIL INFOEOF='Y'
            READ USERINFO INTO USER-REC
                AT END MOVE 'Y' TO INFOEOF
                NOT AT END ADD 1 TO WS-NUMACCOUNTS
            END-READ
        END-PERFORM
    CLOSE USERINFO

*> Process input commands
    MOVE 'N' TO INFOEOF
    PERFORM UNTIL INPUTSEOF='Y'
        READ INPUT-FILE INTO INPUT-TEXT
            AT END MOVE 'Y' TO INPUTSEOF
            NOT AT END PERFORM PARSEINPUT
        END-READ
    END-PERFORM

*> Clean up and exit
    CLOSE INPUT-FILE
    CLOSE APPLOG
    STOP RUN.

SHOW.
    DISPLAY SAVE-TEXT
    WRITE SAVE-RECORD.

CHECKPASSWORD.
    MOVE 'N' TO WS-HASDIGIT
    MOVE 'N' TO WS-HASCAPITAL
    MOVE 'N' TO WS-HASSPECIAL

    INSPECT FUNCTION TRIM(IN-PASSWORD)
        TALLYING WS-CHARCOUNT FOR ALL CHARACTERS

    IF WS-CHARCOUNT >= WS-MINPASSWORDCOUNT
       AND WS-CHARCOUNT <= WS-MAXPASSWORDCOUNT
       THEN
           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > LENGTH OF FUNCTION TRIM(IN-PASSWORD)
               MOVE FUNCTION TRIM(IN-PASSWORD)(I:1)
                   TO WS-INSPECTEDCHAR
               IF WS-INSPECTEDCHAR >= 'A'
                  AND WS-INSPECTEDCHAR <= 'Z'
                   MOVE 'Y' TO WS-HASCAPITAL
               ELSE IF WS-INSPECTEDCHAR >= '0'
                       AND WS-INSPECTEDCHAR <= '9'
                   MOVE 'Y' TO WS-HASDIGIT
               ELSE IF WS-INSPECTEDCHAR >= '!'
                       AND WS-INSPECTEDCHAR <= '/'
                   MOVE 'Y' TO WS-HASSPECIAL
               END-IF
           END-PERFORM

           IF WS-HASCAPITAL = 'Y'
              AND WS-HASDIGIT = 'Y'
              AND WS-HASSPECIAL = 'Y'
               MOVE 'Y' TO WS-LOGGEDIN
               MOVE "Account created successfully." TO SAVE-TEXT
               PERFORM SHOW
               STRING "Welcome, " DELIMITED BY SIZE
                      IN-USERNAME DELIMITED BY SIZE
                      INTO SAVE-TEXT
               PERFORM SHOW
               OPEN EXTEND USERINFO
               WRITE USER-REC
               CLOSE USERINFO
               PERFORM NAV-MENU
           ELSE
               MOVE "Password requirements not met!" TO SAVE-TEXT
               PERFORM SHOW
           END-IF
    ELSE
        MOVE "Password requirements not met!" TO SAVE-TEXT
        PERFORM SHOW
    END-IF.

AUTH-USER.
    IF IN-USERNAME = WS-NAME
        IF IN-PASSWORD = WS-PASSWORD
            MOVE 'Y' TO WS-LOGGEDIN
            MOVE "You have successfully logged in." TO SAVE-TEXT
            PERFORM SHOW
            STRING "Welcome, " DELIMITED BY SIZE
                   WS-NAME DELIMITED BY SIZE
                   INTO SAVE-TEXT
            PERFORM SHOW
        ELSE
            MOVE "Wrong credentials. Try again." TO SAVE-TEXT
            PERFORM SHOW
        END-IF
    ELSE
        MOVE "Wrong credentials. Try again." TO SAVE-TEXT
        PERFORM SHOW
        CLOSE INPUT-FILE
        CLOSE APPLOG
        STOP RUN
    END-IF.

PARSEINPUT.
    IF INPUT-TEXT = WS-LOGIN
        IF WS-LOGGEDIN = 'Y'
            MOVE "You are already logged in." TO SAVE-TEXT
            PERFORM SHOW
        ELSE
            PERFORM UNTIL INFOEOF='Y'
                MOVE "Please enter your username:" TO SAVE-TEXT
                PERFORM SHOW
                MOVE "Please enter your password:" TO SAVE-TEXT
                PERFORM SHOW
                READ INPUT-FILE INTO INPUT-TEXT
                MOVE INPUT-TEXT TO WS-NAME
                READ INPUT-FILE INTO INPUT-TEXT
                MOVE INPUT-TEXT TO WS-PASSWORD
                OPEN INPUT USERINFO
                    READ USERINFO INTO USER-REC
                        AT END MOVE 'Y' TO INFOEOF
                        NOT AT END PERFORM AUTH-USER
                    END-READ
                CLOSE USERINFO
                IF WS-LOGGEDIN = 'Y'
                    PERFORM NAV-MENU
                END-IF
            END-PERFORM
        END-IF
    ELSE IF INPUT-TEXT = WS-NEW
        IF WS-LOGGEDIN = 'Y'
            MOVE "You are already logged in." TO SAVE-TEXT
            PERFORM SHOW
        ELSE IF WS-NUMACCOUNTS < 5
            READ INPUT-FILE INTO INPUT-TEXT
            MOVE INPUT-TEXT TO WS-NEWUSERNAME
            READ INPUT-FILE INTO INPUT-TEXT
            MOVE INPUT-TEXT TO IN-PASSWORD
            OPEN INPUT USERINFO
            MOVE 'N' TO INFOEOF
            MOVE 'Y' TO WS-UNIQUEUSERSTATUS
            PERFORM UNTIL INFOEOF='Y'
                READ USERINFO INTO USER-REC
                    AT END MOVE 'Y' TO INFOEOF
                    NOT AT END
                        IF WS-NEWUSERNAME = IN-USERNAME
                            MOVE "Username already exists, please try again." TO SAVE-TEXT
                            PERFORM SHOW
                            MOVE 'Y' TO INFOEOF
                            MOVE 'N' TO WS-UNIQUEUSERSTATUS
                        END-IF
                END-READ
            END-PERFORM
            CLOSE USERINFO
            IF WS-UNIQUEUSERSTATUS = 'Y'
                MOVE WS-NEWUSERNAME TO IN-USERNAME
                PERFORM CHECKPASSWORD
            END-IF
        ELSE
            MOVE "All permitted accounts have been created, please come back later" TO SAVE-TEXT
            PERFORM SHOW
        END-IF
    ELSE
        MOVE "Invalid action, please try again." TO SAVE-TEXT
        PERFORM SHOW
    END-IF.

*> To test, userinfo.txt must be empty
NAV-MENU.
    PERFORM UNTIL CHOICE = 9 OR INPUTSEOF = "Y"
        MOVE "1. Create/Edit My Profile" TO SAVE-TEXT
        PERFORM SHOW
        MOVE "2. View My Profile" TO SAVE-TEXT
        PERFORM SHOW
        MOVE "3. Search for a job" TO SAVE-TEXT
        PERFORM SHOW
        MOVE "4. Find someone you know" TO SAVE-TEXT
        PERFORM SHOW
        MOVE "5. Learn a New Skill" TO SAVE-TEXT
        PERFORM SHOW
        MOVE "9. Exit" TO SAVE-TEXT
        PERFORM SHOW
        MOVE "Enter your choice:" TO SAVE-TEXT
        PERFORM SHOW

        READ INPUT-FILE INTO INPUT-TEXT
            AT END MOVE "Y" TO INPUTSEOF
        END-READ

        IF INPUTSEOF NOT = "Y"
            MOVE FUNCTION NUMVAL(INPUT-TEXT) TO CHOICE
            EVALUATE CHOICE
                WHEN 1
                    MOVE "Profile editor not implemented yet." TO SAVE-TEXT
                    PERFORM SHOW
                WHEN 2
                    MOVE "No profile found (viewer not implemented yet)." TO SAVE-TEXT
                    PERFORM SHOW
                WHEN 3
                    MOVE "Job search/internship is under construction." TO SAVE-TEXT
                    PERFORM SHOW
                WHEN 4
                    MOVE "Find someone you know is under construction." TO SAVE-TEXT
                    PERFORM SHOW
                WHEN 5
                    PERFORM SKILL-MENU
                WHEN 9
                    MOVE "Exiting navigation..." TO SAVE-TEXT
                    PERFORM SHOW
                    CLOSE INPUT-FILE
                    CLOSE APPLOG
                    STOP RUN
                WHEN OTHER
                    MOVE "Invalid choice, please try again." TO SAVE-TEXT
                    PERFORM SHOW
            END-EVALUATE
        END-IF
    END-PERFORM.

SKILL-MENU.
    MOVE 0 TO SKILLCHOICE
    PERFORM UNTIL SKILLCHOICE = 9 OR INPUTSEOF = "Y"
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
            AT END MOVE "Y" TO INPUTSEOF
        END-READ

        IF INPUTSEOF NOT = "Y"
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
