>>SOURCE FORMAT FREE
*> Program identity
IDENTIFICATION DIVISION.
PROGRAM-ID. InCollege.
*> AUTHOR. Washington.
*> DATE-WRITTEN. 09/06/2025.
*> Simple I/O setup for scripted runs

*> Environment and file assignments
ENVIRONMENT DIVISION.
*> I/O configuration
INPUT-OUTPUT SECTION.
*> Files used by this program
FILE-CONTROL.
    *> Where we store usernames/passwords
    SELECT USERINFO ASSIGN TO "src/userinfo.txt"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS UINFO-FILE-STATUS.

    *> Test input file (can make dynamic later)
    SELECT INPUT-FILE ASSIGN TO "src/InCollege-Input.txt"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS INPUT-FILE-STATUS.

    *> Output transcript (path comes from WS-OUTFILE)
    SELECT APPLOG ASSIGN TO DYNAMIC WS-OUTFILE
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS APPLOG-FILE-STATUS.

    *> Profile data files
    SELECT PROFILES-FILE    ASSIGN TO "src/profiles.txt"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS PROFILES-FILE-STATUS.
    SELECT PROFILES-TEMP-FILE   ASSIGN TO "src/profiles.tmp"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS PROFILES-TEMP-FILE-STATUS.
    SELECT PROFILE-NEW-FILE    ASSIGN TO "src/profiles.new"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS NEW-FILE-STATUS.

    *> Connections file (pending requests)
    SELECT CONNECTIONS-FILE ASSIGN TO "src/connections.txt"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS CONNECTIONS-FILE-STATUS.

    *> Accepted connections (friends) file
    SELECT FRIENDS-FILE ASSIGN TO "src/friends.txt"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS FRIENDS-FILE-STATUS.

    *> Jobs postings file
    SELECT JOBS-FILE ASSIGN TO "src/jobs.txt"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS JOBS-FILE-STATUS.

    SELECT JOBS-TEMP-FILE ASSIGN TO "src/jobs.tmp"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS JOBS-TEMP-FILE-STATUS.

    SELECT JOBS-NEW-FILE ASSIGN TO "src/jobs.new"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS JOBS-NEW-FILE-STATUS.

    *> Job applications file
    SELECT APPLICATIONS-FILE ASSIGN TO "src/applications.txt"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS APPLICATIONS-FILE-STATUS.

    *> Messages file
    SELECT MESSAGES-FILE ASSIGN TO "src/messages.txt"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS MESSAGES-FILE-STATUS.

    *> Temp files for rewriting connections on accept
    SELECT CONN-PROFILES-TEMP-FILE ASSIGN TO "src/connections.tmp"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS CONN-PROFILES-TEMP-FILE-STATUS.
    SELECT CONN-NEW-FILE ASSIGN TO "src/connections.new"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS CONN-NEW-FILE-STATUS.

*> Data descriptions
DATA DIVISION.
*> File record layouts
FILE SECTION.
*> Accounts file: username/password pairs
FD USERINFO.
01 USER-REC.
    05 IN-USERNAME PIC X(20).
    05 IN-PASSWORD PIC X(20).

*> Scripted input file for automation
FD INPUT-FILE.
01 INPUT-REC.
    05 INPUT-TEXT PIC X(256).

*> Transcript/output log
FD APPLOG.
01 SAVE-RECORD.
    05 SAVE-TEXT PIC X(200).

*> Persisted user profiles
FD PROFILES-FILE.
01 PROFILES-LINE PIC X(256).

*> Temp file used while rewriting profiles
FD PROFILES-TEMP-FILE.
01 TEMP-LINE PIC X(256).

*> New file target for atomic replace
FD PROFILE-NEW-FILE.
01 NEW-LINE PIC X(256).

*> Connections (pending requests)
FD CONNECTIONS-FILE.
01 CONNECTION-REC.
    05 CONN-SENDER    PIC X(20).
    05 CONN-RECIPIENT PIC X(20).

FD FRIENDS-FILE.
01 FRIEND-REC.
    05 FR-USER    PIC X(20).
    05 FR-FRIEND  PIC X(20).

FD CONN-PROFILES-TEMP-FILE.
01 CONN-TEMP-REC.
    05 CONN-TEMP-SENDER    PIC X(20).
    05 CONN-TEMP-RECIPIENT PIC X(20).

FD CONN-NEW-FILE.
01 CONN-NEW-REC.
    05 CONN-NEW-SENDER    PIC X(20).
    05 CONN-NEW-RECIPIENT PIC X(20).

FD JOBS-FILE.
 01 JOBS-LINE PIC X(256).

 FD JOBS-TEMP-FILE.
 01 JOBS-TEMP-LINE PIC X(256).

FD JOBS-NEW-FILE.
01 JOBS-NEW-LINE PIC X(256).

FD APPLICATIONS-FILE.
01 APPLICATIONS-LINE PIC X(400).

FD MESSAGES-FILE.
01 MESSAGES-LINE PIC X(512).


*> Variables, flags, and helpers
WORKING-STORAGE SECTION.
   77 WS-OUTFILE PIC X(256) VALUE "src/InCollege-Output.txt".
*> File status codes
01 UINFO-FILE-STATUS PIC XX.
01 INPUT-FILE-STATUS PIC XX.
01 APPLOG-FILE-STATUS PIC XX.
01 PROFILES-FILE-STATUS PIC XX.
01 PROFILES-TEMP-FILE-STATUS PIC XX.
01 NEW-FILE-STATUS PIC XX.
01 JOBS-NEW-FILE-STATUS PIC XX.
01 JOBS-TEMP-FILE-STATUS PIC XX.
01 APPLICATIONS-FILE-STATUS PIC XX.
01 MESSAGES-FILE-STATUS PIC XX.

01 CONNECTIONS-FILE-STATUS PIC XX.

01 FRIENDS-FILE-STATUS     PIC XX.
01 CONN-PROFILES-TEMP-FILE-STATUS   PIC XX.
01 CONN-NEW-FILE-STATUS    PIC XX.
01 JOBS-FILE-STATUS     PIC XX.

01 WS-CONN-SENDER    PIC X(20).
01 WS-CONN-RECIPIENT PIC X(20).
01 WS-CONN-FOUND     PIC A(1) VALUE 'N'.

01 WS-MESSAGE-RECIPIENT PIC X(20).
01 WS-MESSAGE-TEXT      PIC X(300).
01 WS-MESSAGE-CONN-COUNT PIC 99 VALUE 0.
01 WS-MESSAGE-CONNECTIONS.
   05 WS-MESSAGE-CONNECTION OCCURS 20 PIC X(20).
01 WS-MESSAGE-INDEX PIC 99 VALUE 0.
01 WS-MESSAGE-VALID PIC A(1) VALUE 'N'.
01 WS-MESSAGE-USER-FOUND PIC A(1) VALUE 'N'.

01 WS-ACCEPT-NAME    PIC X(20).
01 WS-PENDING-MATCH  PIC A(1) VALUE 'N'.
01 WS-NEED-A-TO-B    PIC A(1) VALUE 'Y'.
01 WS-NEED-B-TO-A    PIC A(1) VALUE 'Y'.

01 WS-PENDING-COUNT   PIC 99   VALUE 0.
01 WS-PENDING-SENDERS OCCURS 20 PIC X(20).
01 WS-PEND-I          PIC 99   VALUE 0.
01 WS-REQ-CHOICE      PIC 9    VALUE 0.
01 WS-REQ-INVALID-COUNT PIC 9  VALUE 0.


*> EOF flags
01 INFOEOF   PIC A(1) VALUE 'N'.
01 WS-INPUT-EOF PIC A(1) VALUE 'N'.

*> Menu choices
01 CURRENT-ACTION PIC X(20).
01 WS-LOGIN PIC X(5)  VALUE 'LOGIN'.
01 WS-NEW   PIC X(18) VALUE 'CREATE NEW ACCOUNT'.

*> Login fields
01 WS-NAME      PIC X(20).
01 WS-PASSWORD  PIC X(20).
01 WS-LOGGEDIN  PIC A(1) VALUE 'N'.

*> Scratch strings
01 LINE-K     PIC X(32).
01 LINE-V     PIC X(224).
01 WS-BUF     PIC X(256).

*> Profile state
01 PROFILE-FOUND PIC A(1) VALUE 'N'.
01 JOB-FOUND PIC A(1) VALUE 'N'.

*> Print helpers
01 WS-GRAD-YEAR-DISPLAY PIC X(4).
01 WS-IND1              PIC X(4)  VALUE "    ".
01 WS-IND2              PIC X(8)  VALUE "        ".
01 WS-IND3              PIC X(12) VALUE "            ".
01 WS-HEADER            PIC X(60).
01 WS-MIN-YEAR-TXT      PIC X(4).
01 WS-MAX-YEAR-TXT      PIC X(4).
01 WS-IDX-TXT           PIC 99.
01 WS-SECTION           PIC X(1) VALUE SPACE.
01 CUR-EXP-IDX          PIC 9 VALUE 0.
01 CUR-EDU-IDX          PIC 9 VALUE 0.
01 WS-YEAR-VALID        PIC A(1) VALUE 'N'.
01 WS-YEAR-NUM          PIC 9(4) VALUE 0.
01 WS-YEAR-TRIES        PIC 9   VALUE 0.
01 WS-YEAR-MAX-TRIES    PIC 9   VALUE 3.

*> Password rule tracking
01 WS-HASCAPITAL PIC A(1) VALUE 'N'.
01 WS-HASDIGIT   PIC A(1) VALUE 'N'.
01 WS-HASSPECIAL PIC A(1) VALUE 'N'.
01 WS-CHARCOUNT  PIC 9(2) VALUE 0.
01 WS-MINPASSWORDCOUNT PIC 9(2) VALUE 8.
01 WS-MAXPASSWORDCOUNT PIC 9(2) VALUE 12.
01 WS-INSPECTEDCHAR PIC X(1).
01 I PIC 9(2) VALUE 1.

*> Account creation helpers
01 WS-NUMACCOUNTS      PIC 9(1) VALUE 0.
01 WS-NEWUSERNAME      PIC X(20).
01 WS-UNIQUEUSERSTATUS PIC A(1) VALUE 'N'.
01 WS-ABORT-CREATE     PIC A(1) VALUE 'N'.

*> Search helpers
01 WS-DONE            PIC A(1)  VALUE 'N'.
01 WS-BLOCK-LINES     PIC 9(4)  VALUE 0.
01 WS-CANDIDATE-NAME  PIC X(128).
01 WS-SEARCH-NAME     PIC X(128).

*> Menus
77 CHOICE      PIC 9 VALUE 0.
77 SKILLCHOICE PIC 9 VALUE 0.
77 WS-JOB-MENU-CHOICE PIC 9 VALUE 0.
77 WS-MESSAGE-CHOICE PIC 9 VALUE 0.
77 WS-JOB-COUNT        PIC 9(4) VALUE 0.
77 WS-JOBS-FILE-READY  PIC A    VALUE 'N'.
77 WS-JOB-CURRENT-INDEX PIC 9(4) VALUE 0.
77 WS-JOB-MAX-SLOTS      PIC 9(4) VALUE 100.
77 WS-JOB-LOOP-INDEX     PIC 9(4) VALUE 0.
77 WS-JOB-SELECTION-NUM  PIC 9(4) VALUE 0.
77 WS-JOB-DETAIL-INDEX   PIC 9(4) VALUE 0.
77 WS-APP-COUNT          PIC 9(4) VALUE 0.
77 WS-JOB-MAX-ID         PIC 9(4) VALUE 0.
77 WS-JOB-JOIN-ID        PIC 9(4) VALUE 0.
77 WS-JOB-JOIN-LOOP      PIC 9(4) VALUE 0.
77 WS-JOB-JOIN-FOUND     PIC A    VALUE 'N'.
77 WS-APP-FILE-MODE      PIC X    VALUE 'E'.

01 WS-JOB-EXIT        PIC A(1) VALUE 'N'.
01 WS-JOB-DETAIL-EXIT PIC A(1) VALUE 'N'.
01 WS-JOB-IN-PROGRESS PIC A(1) VALUE 'N'.
01 WS-JOB-SELECTION       PIC X(40).
01 WS-JOB-SELECTION-UPPER PIC X(40).
01 WS-JOB-SELECTION-CHECK PIC X(40).
01 WS-JOB-SELECTION-TAIL  PIC X(40).
01 WS-JOB-INDEX-DISPLAY   PIC Z(3)9.
01 WS-JOB-ID-DISPLAY      PIC Z(3)9.
01 WS-JOB-DETAIL-ID       PIC 9(4) VALUE 0.
01 WS-JOB-DETAIL-FOUND    PIC A(1) VALUE 'N'.

01 WS-JOB-TABLE.
   05 WS-JOB-ENTRY OCCURS 100 TIMES.
      10 WS-JOB-ID-NUM      PIC 9(4).
      10 WS-JOB-TITLE-TEXT  PIC X(60).
      10 WS-JOB-DESC-TEXT   PIC X(256).
      10 WS-JOB-EMP-TEXT    PIC X(60).
      10 WS-JOB-LOC-TEXT    PIC X(60).
      10 WS-JOB-SALARY-TEXT PIC X(32).

01 WS-APPLICATION-LINE    PIC X(400).
01 WS-APP-USER            PIC X(20).
01 WS-APP-JOBID           PIC X(10).
01 WS-APP-TITLE           PIC X(60).
01 WS-APP-EMPLOYER        PIC X(60).
01 WS-APP-LOCATION        PIC X(60).
01 WS-APP-SALARY          PIC X(32).
01 WS-APP-TIMESTAMP       PIC X(32).
01 WS-APP-DISPLAY-TITLE     PIC X(60).
01 WS-APP-DISPLAY-EMPLOYER  PIC X(60).
01 WS-APP-DISPLAY-LOCATION  PIC X(60).
01 WS-APPLY-DATE          PIC 9(8).
01 WS-APPLY-TIME          PIC 9(6).
01 WS-APPLY-TIMESTAMP     PIC X(19).
01 WS-APPLY-ALREADY       PIC A(1) VALUE 'N'.
01 WS-APPLY-JOBID-TXT     PIC X(10).
01 WS-UNSTRING-PTR        PIC 9(4) VALUE 1.

*> In-memory profile
01 P-REC.
   05 P-USERNAME      PIC X(20).
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

*> In-memory profile
01 JOB-REC.
   05 JOB-ID       PIC 9(4).
   05 JOB-TITLE      PIC X(30).
   05 JOB-DESCRIPTION    PIC X(120).
   05 JOB-EMPLOYER     PIC X(20).
   05 JOB-LOCATION    PIC X(20).
   05 JOB-SALARY         PIC X(16).

01 VALID-YEAR PIC A(1) VALUE 'N'.
01 MIN-YEAR   PIC 9(4) VALUE 1950.
01 MAX-YEAR   PIC 9(4) VALUE 2060.
01 P-I        PIC 9 VALUE 0.

*> Program logic starts here
PROCEDURE DIVISION.
*> Entry point: init files, then menu
MAIN.
    CALL "SYSTEM" USING BY CONTENT "cmd /c if not exist src mkdir src"

    OPEN INPUT  INPUT-FILE

    *> Output path is fixed here

    *> Start with a fresh output file
    OPEN OUTPUT APPLOG
    EVALUATE APPLOG-FILE-STATUS
        WHEN "00"
            CONTINUE
        WHEN "61"
            *> If OUTPUT refuses to open, remove the file and retry
            CALL "SYSTEM" USING BY CONTENT "rm -f src/InCollege-Output.txt"
            OPEN OUTPUT APPLOG
            IF APPLOG-FILE-STATUS NOT = "00"
                DISPLAY "APPLOG OPEN FAILED: " APPLOG-FILE-STATUS " (src/InCollege-Output.txt)"
            END-IF
        WHEN OTHER
            DISPLAY "APPLOG OPEN FAILED: " APPLOG-FILE-STATUS " (src/InCollege-Output.txt)"
    END-EVALUATE


    *> Make sure profiles file exists
    OPEN INPUT PROFILES-FILE
    IF PROFILES-FILE-STATUS = "00"
        CLOSE PROFILES-FILE
    ELSE
        IF PROFILES-FILE-STATUS = "35"
            OPEN OUTPUT PROFILES-FILE
            CLOSE PROFILES-FILE
        END-IF
    END-IF

    *> Make sure connections file exists
    OPEN INPUT CONNECTIONS-FILE
    IF CONNECTIONS-FILE-STATUS = "00"
        CLOSE CONNECTIONS-FILE
    ELSE
        IF CONNECTIONS-FILE-STATUS = "35"
            OPEN OUTPUT CONNECTIONS-FILE
            CLOSE CONNECTIONS-FILE
        END-IF
    END-IF

    *> Make sure friends file exists
    OPEN INPUT FRIENDS-FILE
    IF FRIENDS-FILE-STATUS = "00"
        CLOSE FRIENDS-FILE
    ELSE
        IF FRIENDS-FILE-STATUS = "35"
            OPEN OUTPUT FRIENDS-FILE
            CLOSE FRIENDS-FILE
        END-IF
    END-IF

    *> Make sure jobs file exists
    OPEN INPUT JOBS-FILE
    IF JOBS-FILE-STATUS = "00"
        CLOSE JOBS-FILE
    ELSE
        IF JOBS-FILE-STATUS = "35"
            OPEN OUTPUT JOBS-FILE
            CLOSE JOBS-FILE
        END-IF
    END-IF

    *> Make sure applications file exists
    OPEN INPUT APPLICATIONS-FILE
    IF APPLICATIONS-FILE-STATUS = "00"
        CLOSE APPLICATIONS-FILE
    ELSE
        IF APPLICATIONS-FILE-STATUS = "35"
            OPEN OUTPUT APPLICATIONS-FILE
            CLOSE APPLICATIONS-FILE
        END-IF
    END-IF


*> Count existing accounts
    MOVE 0 TO WS-NUMACCOUNTS
    OPEN INPUT USERINFO
    IF UINFO-FILE-STATUS = "00"
        MOVE 'N' TO INFOEOF
        PERFORM UNTIL INFOEOF='Y'
            READ USERINFO INTO USER-REC
                AT END MOVE 'Y' TO INFOEOF
                NOT AT END ADD 1 TO WS-NUMACCOUNTS
            END-READ
        END-PERFORM
        CLOSE USERINFO
    END-IF

    PERFORM SHOW-MAIN-MENU

    *> All done — wrap up
    MOVE "--- END_OF_PROGRAM_EXECUTION ---" TO SAVE-TEXT PERFORM SHOW
    CLOSE INPUT-FILE
    CLOSE APPLOG
    STOP RUN.

SHOW-MAIN-MENU.
    *> Show the main menu and route to actions
    MOVE "Welcome to InCollege!" TO SAVE-TEXT PERFORM SHOW
    MOVE "--------------------------" TO SAVE-TEXT PERFORM SHOW
    MOVE "  1. Log In"            TO SAVE-TEXT PERFORM SHOW
    MOVE "  2. Create New Account" TO SAVE-TEXT PERFORM SHOW
    MOVE "--------------------------" TO SAVE-TEXT PERFORM SHOW
    MOVE "Enter your choice:"   TO SAVE-TEXT PERFORM SHOW

    READ INPUT-FILE INTO INPUT-TEXT
        AT END
            MOVE "No input. Exiting." TO SAVE-TEXT
            PERFORM SHOW
            EXIT PARAGRAPH
        NOT AT END
            EVALUATE FUNCTION TRIM(INPUT-TEXT)
                WHEN "1"
                    PERFORM LOGIN-PROCESS
                WHEN "Log In"
                    PERFORM LOGIN-PROCESS
                WHEN "2"
                    PERFORM CREATE-ACCOUNT-PROCESS
                WHEN "Create New Account"
                    PERFORM CREATE-ACCOUNT-PROCESS
                WHEN OTHER
                    MOVE "Invalid choice." TO SAVE-TEXT PERFORM SHOW
            END-EVALUATE
    END-READ.

LOGIN-PROCESS.
    *> Handle user login prompts and auth
    IF WS-LOGGEDIN = 'Y'
        MOVE "You are already logged in." TO SAVE-TEXT PERFORM SHOW
    ELSE
        MOVE "Please enter your username:" TO SAVE-TEXT PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
        MOVE FUNCTION TRIM(INPUT-TEXT) TO WS-NAME

        MOVE "Please enter your password:" TO SAVE-TEXT PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
        MOVE FUNCTION TRIM(INPUT-TEXT) TO WS-PASSWORD

        PERFORM AUTH-USER
        IF WS-LOGGEDIN = 'Y'
            PERFORM NAV-MENU
        END-IF
    END-IF.

CREATE-ACCOUNT-PROCESS.
    *> Create a new account with basic validation
    IF WS-LOGGEDIN = 'Y'
        MOVE "You are already logged in." TO SAVE-TEXT PERFORM SHOW
        EXIT PARAGRAPH
    ELSE IF WS-NUMACCOUNTS >= 5
        MOVE "All permitted accounts have been created, please come back later" TO SAVE-TEXT PERFORM SHOW
        EXIT PARAGRAPH
    END-IF

    MOVE 'N' TO WS-UNIQUEUSERSTATUS
    MOVE 'N' TO WS-ABORT-CREATE
    *> Keep asking until the username is unique
    PERFORM UNTIL WS-UNIQUEUSERSTATUS = 'Y'
        MOVE "Enter new username:" TO SAVE-TEXT PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
            AT END MOVE "No more input while creating account." TO SAVE-TEXT PERFORM SHOW
                 EXIT PARAGRAPH
        END-READ
        MOVE FUNCTION TRIM(INPUT-TEXT) TO WS-NEWUSERNAME

        MOVE 'Y' TO WS-UNIQUEUSERSTATUS
        OPEN INPUT USERINFO
        IF UINFO-FILE-STATUS = "35"
            CONTINUE
        ELSE
            IF UINFO-FILE-STATUS = "00"
                MOVE 'N' TO INFOEOF
                PERFORM UNTIL INFOEOF='Y'
                    READ USERINFO INTO USER-REC
                        AT END MOVE 'Y' TO INFOEOF
                        NOT AT END
                            IF FUNCTION TRIM(WS-NEWUSERNAME) = FUNCTION TRIM(IN-USERNAME)
                                MOVE "Username already exists, please try again." TO SAVE-TEXT PERFORM SHOW
                                MOVE 'Y' TO WS-ABORT-CREATE
                                MOVE 'Y' TO WS-UNIQUEUSERSTATUS
                                MOVE 'Y' TO INFOEOF
                            END-IF
                    END-READ
                END-PERFORM
            END-IF
        END-IF
        CLOSE USERINFO
        IF WS-ABORT-CREATE = 'Y'
            PERFORM SHOW-MAIN-MENU
            EXIT PARAGRAPH
        END-IF
    END-PERFORM

    *> Ask for a valid password
    PERFORM UNTIL WS-LOGGEDIN = 'Y'
        MOVE "Enter password (8-12 chars, 1 capital, 1 digit, 1 special):" TO SAVE-TEXT PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
            AT END MOVE "No more input while creating account." TO SAVE-TEXT PERFORM SHOW
                 EXIT PARAGRAPH
        END-READ
        MOVE FUNCTION TRIM(INPUT-TEXT) TO IN-PASSWORD

        MOVE WS-NEWUSERNAME TO IN-USERNAME
        PERFORM CHECKPASSWORD
        *> On success: log in, save user, and go to the menu
    END-PERFORM.

SHOW.
    *> Print to screen and append to the output file
    DISPLAY SAVE-TEXT
    MOVE SAVE-TEXT TO SAVE-RECORD
    WRITE SAVE-RECORD
    IF APPLOG-FILE-STATUS NOT = "00"
        DISPLAY "APPLOG WRITE FAILED: " APPLOG-FILE-STATUS
    END-IF.


CHECKPASSWORD.
    *> Reset rule flags and counters
    MOVE 0  TO WS-CHARCOUNT
    MOVE 'N' TO WS-HASDIGIT
    MOVE 'N' TO WS-HASCAPITAL
    MOVE 'N' TO WS-HASSPECIAL

    *> Clean up: trim and strip CR/TAB if present
    MOVE FUNCTION TRIM(IN-PASSWORD) TO WS-BUF
    INSPECT WS-BUF REPLACING ALL X"0D" BY SPACE
    INSPECT WS-BUF REPLACING ALL X"09" BY SPACE
    MOVE FUNCTION TRIM(WS-BUF) TO WS-BUF

    COMPUTE WS-CHARCOUNT = FUNCTION LENGTH(FUNCTION TRIM(WS-BUF))

    IF WS-CHARCOUNT >= WS-MINPASSWORDCOUNT
       AND WS-CHARCOUNT <= WS-MAXPASSWORDCOUNT
        PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-CHARCOUNT
            MOVE WS-BUF(I:1) TO WS-INSPECTEDCHAR

            IF WS-INSPECTEDCHAR >= 'A' AND WS-INSPECTEDCHAR <= 'Z'
                MOVE 'Y' TO WS-HASCAPITAL
            ELSE
                IF WS-INSPECTEDCHAR >= '0' AND WS-INSPECTEDCHAR <= '9'
                    MOVE 'Y' TO WS-HASDIGIT
                ELSE
                    *> Count ASCII punctuation as "special" (excludes space)
                    IF (WS-INSPECTEDCHAR >= '!' AND WS-INSPECTEDCHAR <= '/')
                     OR (WS-INSPECTEDCHAR >= ':' AND WS-INSPECTEDCHAR <= '@')
                     OR (WS-INSPECTEDCHAR >= '[' AND WS-INSPECTEDCHAR <= '`')
                     OR (WS-INSPECTEDCHAR >= '{' AND WS-INSPECTEDCHAR <= '~')
                        MOVE 'Y' TO WS-HASSPECIAL
                    END-IF
                END-IF
            END-IF
        END-PERFORM

        IF WS-HASCAPITAL = 'Y'
           AND WS-HASDIGIT   = 'Y'
           AND WS-HASSPECIAL = 'Y'
            MOVE 'Y' TO WS-LOGGEDIN
            MOVE "Account created successfully." TO SAVE-TEXT
            PERFORM SHOW

            MOVE SPACES TO SAVE-TEXT
            STRING "Welcome, " DELIMITED BY SIZE
                   FUNCTION TRIM(IN-USERNAME) DELIMITED BY SIZE
                   "!" DELIMITED BY SIZE
                   INTO SAVE-TEXT
            END-STRING
            PERFORM SHOW
            MOVE SPACES TO SAVE-TEXT PERFORM SHOW

            MOVE IN-USERNAME TO WS-NAME

            *> Make sure USERINFO exists before appending
            OPEN INPUT USERINFO
            IF UINFO-FILE-STATUS = "35"
                OPEN OUTPUT USERINFO
                CLOSE USERINFO
            END-IF
            CLOSE USERINFO

            OPEN EXTEND USERINFO
            WRITE USER-REC
            CLOSE USERINFO

            ADD 1 TO WS-NUMACCOUNTS

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
    *> Verify username/password against USERINFO
    MOVE 'N' TO WS-LOGGEDIN
    OPEN INPUT USERINFO
    IF UINFO-FILE-STATUS = "00"
        MOVE 'N' TO INFOEOF
        PERFORM UNTIL INFOEOF = 'Y' OR WS-LOGGEDIN = 'Y'
            READ USERINFO INTO USER-REC
                AT END MOVE 'Y' TO INFOEOF
                NOT AT END
                    IF FUNCTION TRIM(IN-USERNAME) = FUNCTION TRIM(WS-NAME)
                       AND FUNCTION TRIM(IN-PASSWORD) = FUNCTION TRIM(WS-PASSWORD)
                        MOVE 'Y' TO WS-LOGGEDIN
                    END-IF
            END-READ
        END-PERFORM
        CLOSE USERINFO
    END-IF

    IF WS-LOGGEDIN = 'Y'
        MOVE "You have successfully logged in." TO SAVE-TEXT PERFORM SHOW
        MOVE SPACES TO SAVE-TEXT
        STRING "Welcome, " DELIMITED BY SIZE
               FUNCTION TRIM(WS-NAME) DELIMITED BY SIZE
               "!" DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
    ELSE
        MOVE "Wrong credentials. Try again." TO SAVE-TEXT PERFORM SHOW
    END-IF.

EDIT-PROFILE.
    *> Collect and validate profile fields
    MOVE "     Create/Edit Profile     " TO SAVE-TEXT PERFORM SHOW
    MOVE "--------------------------" TO SAVE-TEXT PERFORM SHOW

    *> First Name (required)
    PERFORM UNTIL FUNCTION LENGTH(FUNCTION TRIM(P-FIRST-NAME)) > 0
        MOVE "  Enter First Name:" TO SAVE-TEXT PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
            AT END
                MOVE 'Y' TO WS-INPUT-EOF
                MOVE 9 TO CHOICE
                MOVE "Ran out of input during profile setup." TO SAVE-TEXT PERFORM SHOW
                EXIT PARAGRAPH
        END-READ
        MOVE FUNCTION TRIM(INPUT-TEXT) TO P-FIRST-NAME
        IF FUNCTION LENGTH(FUNCTION TRIM(P-FIRST-NAME)) = 0
            MOVE "First Name is required. Please try again." TO SAVE-TEXT PERFORM SHOW
        END-IF
    END-PERFORM

    *> Last Name (required)
    PERFORM UNTIL FUNCTION LENGTH(FUNCTION TRIM(P-LAST-NAME)) > 0
        MOVE "  Enter Last Name:" TO SAVE-TEXT PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
            AT END
                MOVE 'Y' TO WS-INPUT-EOF
                MOVE 9 TO CHOICE
                MOVE "Ran out of input while setting up the profile." TO SAVE-TEXT PERFORM SHOW
                EXIT PARAGRAPH
        END-READ
        MOVE FUNCTION TRIM(INPUT-TEXT) TO P-LAST-NAME
        IF FUNCTION LENGTH(FUNCTION TRIM(P-LAST-NAME)) = 0
            MOVE "Last Name is required. Please try again." TO SAVE-TEXT PERFORM SHOW
        END-IF
    END-PERFORM

    *> University (required)
    MOVE "  Enter University/College Attended:" TO SAVE-TEXT PERFORM SHOW
    READ INPUT-FILE INTO INPUT-TEXT
        AT END
            MOVE 'Y' TO WS-INPUT-EOF
            MOVE 9 TO CHOICE
            MOVE "Ran out of input while setting up the profile." TO SAVE-TEXT PERFORM SHOW
            EXIT PARAGRAPH
    END-READ
    MOVE FUNCTION TRIM(INPUT-TEXT) TO P-UNIVERSITY

    *> Major (required)
    MOVE "  Enter Major:" TO SAVE-TEXT PERFORM SHOW
    READ INPUT-FILE INTO INPUT-TEXT
        AT END
            MOVE 'Y' TO WS-INPUT-EOF
            MOVE 9 TO CHOICE
            MOVE "Ran out of input while setting up the profile." TO SAVE-TEXT PERFORM SHOW
            EXIT PARAGRAPH
    END-READ
    MOVE FUNCTION TRIM(INPUT-TEXT) TO P-MAJOR

    *> Graduation Year (required, exactly 4 digits)
    MOVE 'N' TO WS-YEAR-VALID
    MOVE 0   TO WS-YEAR-TRIES
    PERFORM UNTIL WS-YEAR-VALID = 'Y'
        MOVE MIN-YEAR TO WS-MIN-YEAR-TXT
        MOVE MAX-YEAR TO WS-MAX-YEAR-TXT
        MOVE SPACES TO SAVE-TEXT
        STRING "  Enter Graduation Year (" DELIMITED BY SIZE
               WS-MIN-YEAR-TXT           DELIMITED BY SIZE
               "-"                       DELIMITED BY SIZE
               WS-MAX-YEAR-TXT           DELIMITED BY SIZE
               "):"                      DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
            AT END
                MOVE 'Y' TO WS-INPUT-EOF
                MOVE 9 TO CHOICE
                MOVE "Ran out of input while setting up the profile." TO SAVE-TEXT PERFORM SHOW
                EXIT PARAGRAPH
        END-READ

        MOVE FUNCTION TRIM(INPUT-TEXT) TO WS-BUF
        IF FUNCTION UPPER-CASE(WS-BUF) = "BACK"
            MOVE "Cancelled editing profile. Returning to menu." TO SAVE-TEXT PERFORM SHOW
            EXIT PARAGRAPH
        END-IF
        *> Clean up CR/TAB just in case
        INSPECT WS-BUF REPLACING ALL X"0D" BY SPACE
        INSPECT WS-BUF REPLACING ALL X"09" BY SPACE
        MOVE FUNCTION TRIM(WS-BUF) TO WS-BUF
        IF FUNCTION LENGTH(FUNCTION TRIM(WS-BUF)) = 4
            MOVE 'Y' TO WS-YEAR-VALID
            PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4 OR WS-YEAR-VALID = 'N'
                MOVE WS-BUF(I:1) TO WS-INSPECTEDCHAR
                IF WS-INSPECTEDCHAR < '0' OR WS-INSPECTEDCHAR > '9'
                    MOVE 'N' TO WS-YEAR-VALID
                END-IF
            END-PERFORM
        ELSE
            MOVE 'N' TO WS-YEAR-VALID
        END-IF

        IF WS-YEAR-VALID = 'Y'
            MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-BUF)) TO WS-YEAR-NUM
            IF WS-YEAR-NUM >= MIN-YEAR AND WS-YEAR-NUM <= MAX-YEAR
                MOVE WS-YEAR-NUM TO P-GRAD-YEAR
            ELSE
                MOVE 'N' TO WS-YEAR-VALID
                MOVE "Invalid year. Please enter 1950-2060." TO SAVE-TEXT PERFORM SHOW
            END-IF
        ELSE
            MOVE "Invalid year. Please enter 4 digits (YYYY)." TO SAVE-TEXT PERFORM SHOW
            ADD 1 TO WS-YEAR-TRIES
            IF WS-YEAR-TRIES >= WS-YEAR-MAX-TRIES
                MOVE "Too many invalid attempts. Returning to main menu." TO SAVE-TEXT PERFORM SHOW
                EXIT PARAGRAPH
            END-IF
        END-IF
    END-PERFORM

    *> About (optional)
    MOVE "  Enter About Me (optional, max 200 chars, enter blank line to skip):" TO SAVE-TEXT PERFORM SHOW
    READ INPUT-FILE INTO INPUT-TEXT
    IF INPUT-FILE-STATUS NOT = "00"
        MOVE 'Y' TO WS-INPUT-EOF
        MOVE 9 TO CHOICE
        MOVE "Ran out of input while setting up the profile." TO SAVE-TEXT PERFORM SHOW
        EXIT PARAGRAPH
    END-IF
    IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-TEXT)) = 0
        MOVE SPACES TO P-ABOUT
    ELSE
        MOVE FUNCTION TRIM(INPUT-TEXT) TO P-ABOUT
    END-IF

    *> Experience entries (prompt order adjusted to include Title prompt)
    MOVE 0 TO P-EXP-COUNT

    PERFORM UNTIL P-EXP-COUNT >= 3
        COMPUTE P-I = P-EXP-COUNT + 1
        MOVE P-I TO WS-IDX-TXT
        IF WS-IDX-TXT(1:1) = '0'
            MOVE WS-IDX-TXT(2:1) TO WS-IDX-TXT(1:1)
            MOVE SPACE TO WS-IDX-TXT(2:1)
        END-IF
        MOVE SPACES TO SAVE-TEXT
        STRING "  Add Experience #" DELIMITED BY SIZE
               FUNCTION TRIM(WS-IDX-TXT) DELIMITED BY SIZE
               " (optional, max 3 entries. Enter 'DONE' to finish):" DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
        MOVE SPACES TO SAVE-TEXT
        STRING "    Experience #" DELIMITED BY SIZE
               FUNCTION TRIM(WS-IDX-TXT) DELIMITED BY SIZE
               " - Title:" DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
        IF INPUT-FILE-STATUS NOT = "00"
            MOVE 'Y' TO WS-INPUT-EOF
            MOVE 9 TO CHOICE
            MOVE "No more input while editing profile." TO SAVE-TEXT PERFORM SHOW
            EXIT PARAGRAPH
        END-IF
        IF FUNCTION UPPER-CASE(FUNCTION TRIM(INPUT-TEXT)) = "DONE"
            EXIT PERFORM
        END-IF

        ADD 1 TO P-EXP-COUNT
        MOVE P-EXP-COUNT TO WS-IDX-TXT
        IF WS-IDX-TXT(1:1) = '0'
            MOVE WS-IDX-TXT(2:1) TO WS-IDX-TXT(1:1)
            MOVE SPACE TO WS-IDX-TXT(2:1)
        END-IF
        MOVE FUNCTION TRIM(INPUT-TEXT) TO P-EXP-TITLE(P-EXP-COUNT)

        MOVE SPACES TO SAVE-TEXT
        STRING "    Experience #" DELIMITED BY SIZE
               FUNCTION TRIM(WS-IDX-TXT) DELIMITED BY SIZE
               " - Company/Organization:" DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
        IF INPUT-FILE-STATUS NOT = "00"
            MOVE 'Y' TO WS-INPUT-EOF
            MOVE 9 TO CHOICE
            MOVE "No more input while editing profile." TO SAVE-TEXT PERFORM SHOW
            EXIT PARAGRAPH
        END-IF
        MOVE FUNCTION TRIM(INPUT-TEXT) TO P-EXP-COMPANY(P-EXP-COUNT)

        MOVE SPACES TO SAVE-TEXT
        STRING "    Experience #" DELIMITED BY SIZE
               FUNCTION TRIM(WS-IDX-TXT) DELIMITED BY SIZE
               " - Dates (e.g., Summer 2024):" DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
        IF INPUT-FILE-STATUS NOT = "00"
            MOVE 'Y' TO WS-INPUT-EOF
            MOVE 9 TO CHOICE
            MOVE "No more input while editing profile." TO SAVE-TEXT PERFORM SHOW
            EXIT PARAGRAPH
        END-IF
        MOVE FUNCTION TRIM(INPUT-TEXT) TO P-EXP-DATES(P-EXP-COUNT)

        MOVE SPACES TO SAVE-TEXT
        STRING "    Experience #" DELIMITED BY SIZE
               FUNCTION TRIM(WS-IDX-TXT) DELIMITED BY SIZE
               " - Description (optional, max 100 chars, blank to skip):" DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
        IF INPUT-FILE-STATUS NOT = "00"
            MOVE 'Y' TO WS-INPUT-EOF
            MOVE 9 TO CHOICE
            MOVE "No more input while editing profile." TO SAVE-TEXT PERFORM SHOW
            EXIT PARAGRAPH
        END-IF
        IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-TEXT)) > 0
            MOVE FUNCTION TRIM(INPUT-TEXT) TO P-EXP-DESC(P-EXP-COUNT)
        ELSE
            MOVE SPACES TO P-EXP-DESC(P-EXP-COUNT)
        END-IF

        *> Do not print the next header here to avoid duplicates
        END-PERFORM

    *> Education entries (prompt order adjusted to include Degree prompt)
    MOVE 0 TO P-EDU-COUNT

    PERFORM UNTIL P-EDU-COUNT >= 3
        COMPUTE P-I = P-EDU-COUNT + 1
        MOVE P-I TO WS-IDX-TXT
        IF WS-IDX-TXT(1:1) = '0'
            MOVE WS-IDX-TXT(2:1) TO WS-IDX-TXT(1:1)
            MOVE SPACE TO WS-IDX-TXT(2:1)
        END-IF
        MOVE SPACES TO SAVE-TEXT
        STRING "  Add Education #" DELIMITED BY SIZE
               FUNCTION TRIM(WS-IDX-TXT) DELIMITED BY SIZE
               " (optional, max 3 entries. Enter 'DONE' to finish):" DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
        MOVE SPACES TO SAVE-TEXT
        STRING "    Education #" DELIMITED BY SIZE
               FUNCTION TRIM(WS-IDX-TXT) DELIMITED BY SIZE
               " - Degree:" DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
        IF INPUT-FILE-STATUS NOT = "00"
            MOVE 'Y' TO WS-INPUT-EOF
            MOVE 9 TO CHOICE
            MOVE "No more input while editing profile." TO SAVE-TEXT PERFORM SHOW
            EXIT PARAGRAPH
        END-IF
        IF FUNCTION UPPER-CASE(FUNCTION TRIM(INPUT-TEXT)) = "DONE"
            EXIT PERFORM
        END-IF

        ADD 1 TO P-EDU-COUNT
        MOVE P-EDU-COUNT TO WS-IDX-TXT
        IF WS-IDX-TXT(1:1) = '0'
            MOVE WS-IDX-TXT(2:1) TO WS-IDX-TXT(1:1)
            MOVE SPACE TO WS-IDX-TXT(2:1)
        END-IF
        MOVE FUNCTION TRIM(INPUT-TEXT) TO P-EDU-DEGREE(P-EDU-COUNT)

        MOVE SPACES TO SAVE-TEXT
        STRING "    Education #" DELIMITED BY SIZE
               FUNCTION TRIM(WS-IDX-TXT) DELIMITED BY SIZE
               " - University/College:" DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
        IF INPUT-FILE-STATUS NOT = "00"
            MOVE 'Y' TO WS-INPUT-EOF
            MOVE 9 TO CHOICE
            MOVE "No more input while editing profile." TO SAVE-TEXT PERFORM SHOW
            EXIT PARAGRAPH
        END-IF
        MOVE FUNCTION TRIM(INPUT-TEXT) TO P-EDU-SCHOOL(P-EDU-COUNT)

        MOVE SPACES TO SAVE-TEXT
        STRING "    Education #" DELIMITED BY SIZE
               FUNCTION TRIM(WS-IDX-TXT) DELIMITED BY SIZE
               " - Years Attended (e.g., 2023-2025):" DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
        IF INPUT-FILE-STATUS NOT = "00"
            MOVE 'Y' TO WS-INPUT-EOF
            MOVE 9 TO CHOICE
            MOVE "No more input while editing profile." TO SAVE-TEXT PERFORM SHOW
            EXIT PARAGRAPH
        END-IF
        MOVE FUNCTION TRIM(INPUT-TEXT) TO P-EDU-YEARS(P-EDU-COUNT)

        *> Do not print the next header here to avoid duplicates
        END-PERFORM

    *> Save profile
    MOVE WS-NAME TO P-USERNAME
    PERFORM SAVE-PROFILE

    MOVE "Profile saved successfully!" TO SAVE-TEXT PERFORM SHOW

    *> Show the freshly saved profile using the reusable printer
    MOVE "--- Your Profile ---" TO WS-HEADER
    PERFORM PRINT-PROFILE-FRIENDLY.
NAV-MENU.
    *> Main navigation after login
    MOVE 0 TO CHOICE
    PERFORM UNTIL CHOICE = 9
        IF WS-INPUT-EOF = 'Y'
            MOVE 9 TO CHOICE
            EXIT PERFORM
        END-IF
        MOVE "--------------------------" TO SAVE-TEXT PERFORM SHOW
        MOVE "        Menu        " TO SAVE-TEXT PERFORM SHOW
        MOVE "--------------------------" TO SAVE-TEXT PERFORM SHOW

        MOVE "  1. Create/Edit My Profile" TO SAVE-TEXT PERFORM SHOW
        MOVE "  2. View My Profile"        TO SAVE-TEXT PERFORM SHOW
        MOVE "  3. Search for a job"       TO SAVE-TEXT PERFORM SHOW
        MOVE "  4. Find someone you know"  TO SAVE-TEXT PERFORM SHOW
        MOVE "  5. Learn a New Skill"      TO SAVE-TEXT PERFORM SHOW
        MOVE "  6. View My Pending Connection Requests" TO SAVE-TEXT PERFORM SHOW
        MOVE "  7. View My Network"        TO SAVE-TEXT PERFORM SHOW
        MOVE "  8. Messages"               TO SAVE-TEXT PERFORM SHOW
        MOVE "  9. Log Out / Exit"         TO SAVE-TEXT PERFORM SHOW
        MOVE "--------------------------"  TO SAVE-TEXT PERFORM SHOW
        MOVE "  Enter your choice:"        TO SAVE-TEXT PERFORM SHOW

        READ INPUT-FILE INTO INPUT-TEXT
            AT END
                MOVE 9 TO CHOICE
                MOVE "No more input. Exiting." TO SAVE-TEXT
                PERFORM SHOW
            NOT AT END
                MOVE FUNCTION NUMVAL(FUNCTION TRIM(INPUT-TEXT)) TO CHOICE
        END-READ

        EVALUATE TRUE
            WHEN CHOICE = 1
                PERFORM EDIT-PROFILE
            WHEN CHOICE = 2
                PERFORM VIEW-PROFILE
            WHEN CHOICE = 3
                PERFORM VIEW-JOBS
            WHEN CHOICE = 4
                PERFORM FIND-SOMEONE-YOU-KNOW
            WHEN CHOICE = 5
                PERFORM SKILL-MENU
            WHEN CHOICE = 6
                PERFORM VIEW-PENDING-REQUESTS
            WHEN CHOICE = 7
                PERFORM VIEW-MY-NETWORK
            WHEN CHOICE = 8
                PERFORM MESSAGES-MENU
            WHEN CHOICE = 9
                CONTINUE
            WHEN OTHER
                MOVE "Invalid choice, please try again." TO SAVE-TEXT PERFORM SHOW
                MOVE 0 TO CHOICE
        END-EVALUATE
    END-PERFORM.

MESSAGES-MENU.
    *> Provides messaging related options
    MOVE 0 TO WS-MESSAGE-CHOICE
    PERFORM UNTIL WS-MESSAGE-CHOICE = 3 OR WS-INPUT-EOF = 'Y'
        MOVE "--------------------------" TO SAVE-TEXT PERFORM SHOW
        MOVE "      Messages Menu      " TO SAVE-TEXT PERFORM SHOW
        MOVE "--------------------------" TO SAVE-TEXT PERFORM SHOW
        MOVE "  1. Send a New Message" TO SAVE-TEXT PERFORM SHOW
        MOVE "  2. View My Messages"   TO SAVE-TEXT PERFORM SHOW
        MOVE "  3. Back to Main Menu"  TO SAVE-TEXT PERFORM SHOW
        MOVE "--------------------------" TO SAVE-TEXT PERFORM SHOW
        MOVE "  Enter your choice:"    TO SAVE-TEXT PERFORM SHOW

        READ INPUT-FILE INTO INPUT-TEXT
            AT END
                MOVE 'Y' TO WS-INPUT-EOF
                MOVE 3 TO WS-MESSAGE-CHOICE
                MOVE "No more input while in Messages menu." TO SAVE-TEXT PERFORM SHOW
            NOT AT END
                MOVE FUNCTION NUMVAL(FUNCTION TRIM(INPUT-TEXT)) TO WS-MESSAGE-CHOICE
        END-READ

        EVALUATE WS-MESSAGE-CHOICE
            WHEN 1
                PERFORM SEND-NEW-MESSAGE
                MOVE 0 TO WS-MESSAGE-CHOICE
            WHEN 2
                PERFORM VIEW-MY-MESSAGES
                MOVE 0 TO WS-MESSAGE-CHOICE
            WHEN 3
                CONTINUE
            WHEN OTHER
                MOVE "Invalid choice, please try again." TO SAVE-TEXT PERFORM SHOW
                MOVE 0 TO WS-MESSAGE-CHOICE
        END-EVALUATE
    END-PERFORM.

LOAD-MESSAGE-CONNECTIONS.
    *> Collect usernames of accepted connections for messaging
    MOVE 0 TO WS-MESSAGE-CONN-COUNT
    MOVE SPACES TO WS-MESSAGE-CONNECTIONS

    OPEN INPUT FRIENDS-FILE
    IF FRIENDS-FILE-STATUS = "00"
        PERFORM UNTIL FRIENDS-FILE-STATUS = "10"
            READ FRIENDS-FILE INTO FRIEND-REC
                AT END EXIT PERFORM
            END-READ
            IF FUNCTION TRIM(FR-USER) = FUNCTION TRIM(WS-NAME)
                IF WS-MESSAGE-CONN-COUNT < 20
                    ADD 1 TO WS-MESSAGE-CONN-COUNT
                    MOVE FR-FRIEND TO WS-MESSAGE-CONNECTION(WS-MESSAGE-CONN-COUNT)
                END-IF
            END-IF
        END-PERFORM
        CLOSE FRIENDS-FILE
    ELSE IF FRIENDS-FILE-STATUS = "35"
        *> No friends file yet means no connections
        CONTINUE
    ELSE
        MOVE SPACES TO SAVE-TEXT
        STRING "Unable to read network data (status " DELIMITED BY SIZE
               FRIENDS-FILE-STATUS DELIMITED BY SIZE
               ")." DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
    END-IF.

VERIFY-MESSAGE-RECIPIENT-EXISTS.
    *> Determine whether the intended recipient exists in USERINFO
    MOVE 'N' TO WS-MESSAGE-USER-FOUND

    OPEN INPUT USERINFO
    EVALUATE UINFO-FILE-STATUS
        WHEN "00"
            PERFORM UNTIL UINFO-FILE-STATUS = "10" OR WS-MESSAGE-USER-FOUND = 'Y'
                READ USERINFO INTO USER-REC
                    AT END EXIT PERFORM
                    NOT AT END
                        IF FUNCTION TRIM(IN-USERNAME) = FUNCTION TRIM(WS-MESSAGE-RECIPIENT)
                            MOVE 'Y' TO WS-MESSAGE-USER-FOUND
                            EXIT PERFORM
                        END-IF
                END-READ
            END-PERFORM
            CLOSE USERINFO
        WHEN "35"
            *> File does not exist yet; no users recorded
            CONTINUE
        WHEN OTHER
            MOVE SPACES TO SAVE-TEXT
            STRING "Unable to access user records (status " DELIMITED BY SIZE
                   UINFO-FILE-STATUS DELIMITED BY SIZE
                   ")." DELIMITED BY SIZE
                   INTO SAVE-TEXT
            END-STRING
            PERFORM SHOW
    END-EVALUATE.

SEND-NEW-MESSAGE.
    *> Prompt for recipient and persist message when valid
    PERFORM LOAD-MESSAGE-CONNECTIONS

    IF WS-MESSAGE-CONN-COUNT = 0
        MOVE "You are not connected with anyone yet. Connect with other users to send messages." TO SAVE-TEXT PERFORM SHOW
        EXIT PARAGRAPH
    END-IF

    MOVE "Enter the username of the recipient:" TO SAVE-TEXT PERFORM SHOW
    READ INPUT-FILE INTO INPUT-TEXT
        AT END
            MOVE 'Y' TO WS-INPUT-EOF
            MOVE "No more input while attempting to send a message." TO SAVE-TEXT PERFORM SHOW
            EXIT PARAGRAPH
        NOT AT END
            MOVE FUNCTION TRIM(INPUT-TEXT) TO WS-MESSAGE-RECIPIENT
    END-READ

    IF FUNCTION LENGTH(FUNCTION TRIM(WS-MESSAGE-RECIPIENT)) = 0
        MOVE "Recipient username cannot be empty." TO SAVE-TEXT PERFORM SHOW
        EXIT PARAGRAPH
    END-IF

    PERFORM VERIFY-MESSAGE-RECIPIENT-EXISTS

    IF WS-MESSAGE-USER-FOUND NOT = 'Y'
        MOVE "User not found." TO SAVE-TEXT PERFORM SHOW
        EXIT PARAGRAPH
    END-IF

    MOVE 'N' TO WS-MESSAGE-VALID
    PERFORM VARYING WS-MESSAGE-INDEX FROM 1 BY 1
            UNTIL WS-MESSAGE-INDEX > WS-MESSAGE-CONN-COUNT
        IF FUNCTION TRIM(WS-MESSAGE-CONNECTION(WS-MESSAGE-INDEX))
           = FUNCTION TRIM(WS-MESSAGE-RECIPIENT)
            MOVE 'Y' TO WS-MESSAGE-VALID
            EXIT PERFORM
        END-IF
    END-PERFORM

    IF WS-MESSAGE-VALID NOT = 'Y'
        MOVE "You can only message users you are connected with." TO SAVE-TEXT PERFORM SHOW
        EXIT PARAGRAPH
    END-IF

    MOVE "Enter your message:" TO SAVE-TEXT PERFORM SHOW
    READ INPUT-FILE INTO INPUT-TEXT
        AT END
            MOVE 'Y' TO WS-INPUT-EOF
            MOVE "No more input while attempting to send a message." TO SAVE-TEXT PERFORM SHOW
            EXIT PARAGRAPH
        NOT AT END
            MOVE FUNCTION TRIM(INPUT-TEXT) TO WS-MESSAGE-TEXT
    END-READ

    IF FUNCTION LENGTH(FUNCTION TRIM(WS-MESSAGE-TEXT)) = 0
        MOVE "Message text cannot be empty." TO SAVE-TEXT PERFORM SHOW
        EXIT PARAGRAPH
    END-IF

    OPEN EXTEND MESSAGES-FILE
    IF MESSAGES-FILE-STATUS = "35"
        OPEN OUTPUT MESSAGES-FILE
        CLOSE MESSAGES-FILE
        OPEN EXTEND MESSAGES-FILE
    END-IF

    IF MESSAGES-FILE-STATUS NOT = "00"
        MOVE SPACES TO SAVE-TEXT
        STRING "Unable to save your message (status " DELIMITED BY SIZE
               MESSAGES-FILE-STATUS DELIMITED BY SIZE
               ")." DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
        EXIT PARAGRAPH
    END-IF

    MOVE SPACES TO MESSAGES-LINE
    STRING FUNCTION TRIM(WS-NAME) DELIMITED BY SIZE
           "|" DELIMITED BY SIZE
           FUNCTION TRIM(WS-MESSAGE-RECIPIENT) DELIMITED BY SIZE
           "|" DELIMITED BY SIZE
           FUNCTION TRIM(WS-MESSAGE-TEXT) DELIMITED BY SIZE
           INTO MESSAGES-LINE
    END-STRING
    WRITE MESSAGES-LINE

    IF MESSAGES-FILE-STATUS NOT = "00"
        MOVE SPACES TO SAVE-TEXT
        STRING "Failed to write message (status " DELIMITED BY SIZE
               MESSAGES-FILE-STATUS DELIMITED BY SIZE
               ")." DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
        CLOSE MESSAGES-FILE
        EXIT PARAGRAPH
    END-IF

    CLOSE MESSAGES-FILE

    MOVE "Message sent successfully!" TO SAVE-TEXT PERFORM SHOW.

VIEW-MY-MESSAGES.
    *> Under construction per current requirements
    MOVE "View My Messages is currently under construction." TO SAVE-TEXT PERFORM SHOW.


WRITE-PROFILE-BLOCK.
    *> Persist the in-memory profile (P-REC) as text
    MOVE SPACES TO TEMP-LINE
    STRING "USER: "  P-USERNAME   INTO TEMP-LINE END-STRING
    WRITE TEMP-LINE

    MOVE SPACES TO TEMP-LINE
    STRING "FN: "    P-FIRST-NAME INTO TEMP-LINE END-STRING
    WRITE TEMP-LINE

    MOVE SPACES TO TEMP-LINE
    STRING "LN: "    P-LAST-NAME  INTO TEMP-LINE END-STRING
    WRITE TEMP-LINE

    MOVE SPACES TO TEMP-LINE
    STRING "UNIV: "  P-UNIVERSITY INTO TEMP-LINE END-STRING
    WRITE TEMP-LINE

    MOVE SPACES TO TEMP-LINE
    STRING "MAJOR: " P-MAJOR      INTO TEMP-LINE END-STRING
    WRITE TEMP-LINE

    MOVE P-GRAD-YEAR TO WS-GRAD-YEAR-DISPLAY
    MOVE SPACES TO TEMP-LINE
    STRING "GRAD: "  WS-GRAD-YEAR-DISPLAY INTO TEMP-LINE END-STRING
    WRITE TEMP-LINE

    *> Only write About if it’s not empty
    IF FUNCTION LENGTH(FUNCTION TRIM(P-ABOUT)) > 0
        MOVE SPACES TO TEMP-LINE
        STRING "ABOUT: " P-ABOUT      INTO TEMP-LINE END-STRING
        WRITE TEMP-LINE
    END-IF

    IF P-EXP-COUNT > 0
        MOVE "Experience:" TO TEMP-LINE
        WRITE TEMP-LINE
        PERFORM VARYING P-I FROM 1 BY 1 UNTIL P-I > P-EXP-COUNT
            MOVE SPACES TO TEMP-LINE
            STRING "Title: " P-EXP-TITLE(P-I) INTO TEMP-LINE END-STRING
            WRITE TEMP-LINE

            MOVE SPACES TO TEMP-LINE
            STRING "Company: " P-EXP-COMPANY(P-I) INTO TEMP-LINE END-STRING
            WRITE TEMP-LINE

            MOVE SPACES TO TEMP-LINE
            STRING "Dates: " P-EXP-DATES(P-I) INTO TEMP-LINE END-STRING
            WRITE TEMP-LINE

            IF FUNCTION LENGTH(FUNCTION TRIM(P-EXP-DESC(P-I))) > 0
                MOVE SPACES TO TEMP-LINE
                STRING "Description: " P-EXP-DESC(P-I) INTO TEMP-LINE END-STRING
                WRITE TEMP-LINE
            END-IF
        END-PERFORM
    END-IF

    IF P-EDU-COUNT > 0
        MOVE "Education:" TO TEMP-LINE
        WRITE TEMP-LINE
        PERFORM VARYING P-I FROM 1 BY 1 UNTIL P-I > P-EDU-COUNT
            MOVE SPACES TO TEMP-LINE
            STRING "Degree: " P-EDU-DEGREE(P-I) INTO TEMP-LINE END-STRING
            WRITE TEMP-LINE

            MOVE SPACES TO TEMP-LINE
            STRING "University: " P-EDU-SCHOOL(P-I) INTO TEMP-LINE END-STRING
            WRITE TEMP-LINE

            MOVE SPACES TO TEMP-LINE
            STRING "Years: " P-EDU-YEARS(P-I) INTO TEMP-LINE END-STRING
            WRITE TEMP-LINE
        END-PERFORM
    ELSE
        MOVE "Education: None" TO SAVE-TEXT PERFORM SHOW
    END-IF


    MOVE "-----END-----" TO TEMP-LINE
    WRITE TEMP-LINE.

SAVE-PROFILE.
    *> Save current P-REC into profiles.txt (replace or add)
    MOVE "N" TO PROFILE-FOUND

    *> Make sure the profiles file exists
    OPEN INPUT PROFILES-FILE
    IF PROFILES-FILE-STATUS NOT = "00"
        OPEN OUTPUT PROFILES-FILE
        CLOSE PROFILES-FILE
        OPEN INPUT PROFILES-FILE
    END-IF

    *> Rewrite via temp: copy everything, replacing just this user’s block
    OPEN OUTPUT PROFILES-TEMP-FILE

    PERFORM UNTIL PROFILES-FILE-STATUS = "10"
        READ PROFILES-FILE INTO PROFILES-LINE
            AT END EXIT PERFORM
        END-READ

        IF PROFILES-LINE(1:6) = "USER: "
            MOVE PROFILES-LINE(7:) TO WS-BUF
            IF FUNCTION TRIM(WS-BUF) = FUNCTION TRIM(P-USERNAME)
                *> Skip the old block for this user
                PERFORM UNTIL PROFILES-LINE = "END" OR PROFILES-LINE = "-----END-----"
                    READ PROFILES-FILE INTO PROFILES-LINE
                        AT END EXIT PERFORM
                    END-READ
                END-PERFORM
                *> Write the updated block
                PERFORM WRITE-PROFILE-BLOCK
                MOVE "Y" TO PROFILE-FOUND
            ELSE
                *> Copy other users as-is
                MOVE PROFILES-LINE TO TEMP-LINE
                WRITE TEMP-LINE
                PERFORM UNTIL PROFILES-LINE = "END" OR PROFILES-LINE = "-----END-----"
                    READ PROFILES-FILE INTO PROFILES-LINE
                        AT END EXIT PERFORM
                    END-READ
                    IF PROFILES-LINE = "END" OR PROFILES-LINE = "-----END-----"
                        MOVE "-----END-----" TO TEMP-LINE
                    ELSE
                        MOVE PROFILES-LINE TO TEMP-LINE
                    END-IF
                    WRITE TEMP-LINE
                END-PERFORM
            END-IF
        ELSE
            MOVE PROFILES-LINE TO TEMP-LINE
            WRITE TEMP-LINE
        END-IF
    END-PERFORM

    CLOSE PROFILES-FILE

    IF PROFILE-FOUND NOT = "Y"
        PERFORM WRITE-PROFILE-BLOCK
    END-IF

    CLOSE PROFILES-TEMP-FILE

    *> Swap in the new file (atomic replace)
    OPEN INPUT  PROFILES-TEMP-FILE
    OPEN OUTPUT PROFILE-NEW-FILE
    PERFORM UNTIL PROFILES-TEMP-FILE-STATUS = "10"
        READ PROFILES-TEMP-FILE INTO TEMP-LINE
            AT END EXIT PERFORM
        END-READ
        MOVE TEMP-LINE TO NEW-LINE
        WRITE NEW-LINE
    END-PERFORM
    CLOSE PROFILES-TEMP-FILE
    CLOSE PROFILE-NEW-FILE

    CALL "SYSTEM" USING BY CONTENT "mv -f src/profiles.new src/profiles.txt".

VIEW-PROFILE.
    *> Load and show the current user's profile
    *> Load and show the current user's profile
    MOVE 'N' TO PROFILE-FOUND
    OPEN INPUT PROFILES-FILE
    MOVE SPACES TO PROFILES-LINE
    MOVE SPACE TO WS-SECTION
    MOVE 0 TO CUR-EXP-IDX CUR-EDU-IDX

    PERFORM UNTIL 1 = 2
        READ PROFILES-FILE INTO PROFILES-LINE
            AT END EXIT PERFORM
        END-READ

        IF PROFILES-LINE(1:6) = "USER: "
            MOVE PROFILES-LINE(7:) TO WS-BUF
            IF FUNCTION TRIM(WS-BUF) = FUNCTION TRIM(WS-NAME)
                MOVE 'Y' TO PROFILE-FOUND

                MOVE SPACES TO P-FIRST-NAME P-LAST-NAME P-UNIVERSITY P-MAJOR P-ABOUT
                MOVE 0 TO P-GRAD-YEAR P-EXP-COUNT P-EDU-COUNT CUR-EXP-IDX CUR-EDU-IDX
                MOVE SPACE TO WS-SECTION

                PERFORM UNTIL PROFILES-LINE = "END" OR PROFILES-LINE = "-----END-----"
                    READ PROFILES-FILE INTO PROFILES-LINE
                        AT END EXIT PERFORM
                    END-READ

                    IF PROFILES-LINE = "END" OR PROFILES-LINE = "-----END-----"
                        EXIT PERFORM
                    ELSE IF PROFILES-LINE = "Experience:"
                        MOVE 'X' TO WS-SECTION
                    ELSE IF PROFILES-LINE = "Education:"
                        MOVE 'U' TO WS-SECTION
                    ELSE IF PROFILES-LINE(1:4) = "FN: "
                        MOVE PROFILES-LINE(5:) TO WS-BUF
                        MOVE FUNCTION TRIM(WS-BUF) TO P-FIRST-NAME
                    ELSE IF PROFILES-LINE(1:4) = "LN: "
                        MOVE PROFILES-LINE(5:) TO WS-BUF
                        MOVE FUNCTION TRIM(WS-BUF) TO P-LAST-NAME
                    ELSE IF PROFILES-LINE(1:6) = "UNIV: "
                        MOVE PROFILES-LINE(7:) TO WS-BUF
                        MOVE FUNCTION TRIM(WS-BUF) TO P-UNIVERSITY
                    ELSE IF PROFILES-LINE(1:7) = "MAJOR: "
                        MOVE PROFILES-LINE(8:) TO WS-BUF
                        MOVE FUNCTION TRIM(WS-BUF) TO P-MAJOR
                    ELSE IF PROFILES-LINE(1:6) = "GRAD: "
                        MOVE PROFILES-LINE(7:) TO WS-BUF
                        MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-BUF)) TO P-GRAD-YEAR
                    ELSE IF PROFILES-LINE(1:7) = "ABOUT: "
                        MOVE PROFILES-LINE(8:) TO WS-BUF
                        MOVE FUNCTION TRIM(WS-BUF) TO P-ABOUT
                    ELSE
                        EVALUATE WS-SECTION
                            WHEN 'X'
                                IF PROFILES-LINE(1:7) = "Title: "
                                    ADD 1 TO CUR-EXP-IDX
                                    MOVE PROFILES-LINE(8:) TO WS-BUF
                                    MOVE FUNCTION TRIM(WS-BUF) TO P-EXP-TITLE(CUR-EXP-IDX)
                                    MOVE CUR-EXP-IDX TO P-EXP-COUNT
                                ELSE IF PROFILES-LINE(1:9) = "Company: "
                                    MOVE PROFILES-LINE(10:) TO WS-BUF
                                    MOVE FUNCTION TRIM(WS-BUF) TO P-EXP-COMPANY(CUR-EXP-IDX)
                                ELSE IF PROFILES-LINE(1:7) = "Dates: "
                                    MOVE PROFILES-LINE(8:) TO WS-BUF
                                    MOVE FUNCTION TRIM(WS-BUF) TO P-EXP-DATES(CUR-EXP-IDX)
                                ELSE IF PROFILES-LINE(1:13) = "Description: "
                                    MOVE PROFILES-LINE(14:) TO WS-BUF
                                    MOVE FUNCTION TRIM(WS-BUF) TO P-EXP-DESC(CUR-EXP-IDX)
                                END-IF
                            WHEN 'U'
                                IF PROFILES-LINE(1:8) = "Degree: "
                                    ADD 1 TO CUR-EDU-IDX
                                    MOVE PROFILES-LINE(9:) TO WS-BUF
                                    MOVE FUNCTION TRIM(WS-BUF) TO P-EDU-DEGREE(CUR-EDU-IDX)
                                    MOVE CUR-EDU-IDX TO P-EDU-COUNT
                                ELSE IF PROFILES-LINE(1:12) = "University: "
                                    MOVE PROFILES-LINE(13:) TO WS-BUF
                                    MOVE FUNCTION TRIM(WS-BUF) TO P-EDU-SCHOOL(CUR-EDU-IDX)
                                ELSE IF PROFILES-LINE(1:7) = "Years: "
                                    MOVE PROFILES-LINE(8:) TO WS-BUF
                                    MOVE FUNCTION TRIM(WS-BUF) TO P-EDU-YEARS(CUR-EDU-IDX)
                                END-IF
                            WHEN OTHER
                                CONTINUE
                        END-EVALUATE
                    END-IF
                END-PERFORM

                *> Print a friendly view
                MOVE "--- Your Profile ---" TO WS-HEADER
                PERFORM PRINT-PROFILE-FRIENDLY

                EXIT PERFORM
            END-IF
        END-IF
    END-PERFORM

    CLOSE PROFILES-FILE

    IF PROFILE-FOUND = 'Y'
        CONTINUE
    ELSE
        MOVE SPACES TO SAVE-TEXT
        STRING WS-IND1 DELIMITED BY SIZE
               "No profile found." DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
    END-IF.

VIEW-JOBS.
    *> Job search / internship menu loop
    MOVE 0 TO WS-JOB-MENU-CHOICE

    PERFORM UNTIL WS-JOB-MENU-CHOICE = 4 OR WS-INPUT-EOF = 'Y'
        MOVE "Enter your choice:"           TO SAVE-TEXT PERFORM SHOW
        MOVE "  1. Post a Job/Internships" TO SAVE-TEXT PERFORM SHOW
        MOVE "  2. Browse Jobs/Internships"        TO SAVE-TEXT PERFORM SHOW
        MOVE "  3. View My Applications"   TO SAVE-TEXT PERFORM SHOW
        MOVE "  4. Back to Main Menu"       TO SAVE-TEXT PERFORM SHOW

        READ INPUT-FILE INTO INPUT-TEXT
            AT END
                MOVE "No input. Exiting." TO SAVE-TEXT
                PERFORM SHOW
                MOVE 4 TO WS-JOB-MENU-CHOICE
                EXIT PERFORM
            NOT AT END
                IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-TEXT)) = 0
                    MOVE "Invalid choice." TO SAVE-TEXT PERFORM SHOW
                ELSE
                    MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(INPUT-TEXT)) TO WS-BUF
                    EVALUATE TRUE
                        WHEN WS-BUF = "1"
                             OR WS-BUF = "POST"
                             OR WS-BUF = "POST JOBS"
                             OR WS-BUF = "POST A JOB"
                             OR WS-BUF = "POST A JOB/INTERNSHIPS"
                            PERFORM POST-JOBS
                            MOVE 4 TO WS-JOB-MENU-CHOICE
                        WHEN WS-BUF = "2"
                             OR WS-BUF = "BROWSE"
                             OR WS-BUF = "BROWSE JOBS"
                             OR WS-BUF = "BROWSE JOBS/INTERNSHIPS"
                            PERFORM BROWSE-JOBS
                            MOVE 4 TO WS-JOB-MENU-CHOICE
                        WHEN WS-BUF = "3"
                             OR WS-BUF = "VIEW"
                             OR WS-BUF = "VIEW APPLICATIONS"
                             OR WS-BUF = "VIEW MY APPLICATIONS"
                             OR WS-BUF = "MY APPLICATIONS"
                             OR WS-BUF = "APPLICATIONS"
                            PERFORM VIEW-MY-APPLICATIONS
                            MOVE 4 TO WS-JOB-MENU-CHOICE
                        WHEN WS-BUF = "4"
                             OR WS-BUF = "BACK"
                             OR WS-BUF = "BACK TO MAIN MENU"
                             OR WS-BUF = "MAIN MENU"
                            MOVE 4 TO WS-JOB-MENU-CHOICE
                        WHEN OTHER
                            MOVE "Invalid choice." TO SAVE-TEXT PERFORM SHOW
                    END-EVALUATE
                END-IF
        END-READ
    END-PERFORM.

POST-JOBS.
*> Collect and validate profile fields
    MOVE "     Post a Job/Internship     " TO SAVE-TEXT PERFORM SHOW
    MOVE "--------------------------" TO SAVE-TEXT PERFORM SHOW

    *> Generate a unique job ID
    MOVE 0 TO JOB-ID
    OPEN INPUT JOBS-FILE
    IF JOBS-FILE-STATUS = "00"
        MOVE "00" TO JOBS-FILE-STATUS
        PERFORM UNTIL JOBS-FILE-STATUS = "10"
            READ JOBS-FILE INTO JOBS-LINE
                AT END EXIT PERFORM
            END-READ
            IF JOBS-LINE(1:4) = "ID: "
                MOVE JOBS-LINE(5:) TO WS-BUF
                IF FUNCTION LENGTH(FUNCTION TRIM(WS-BUF)) > 0
                    IF JOB-ID < FUNCTION NUMVAL(FUNCTION TRIM(WS-BUF))
                        MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-BUF)) TO JOB-ID
                    END-IF
                END-IF
            END-IF
        END-PERFORM
        CLOSE JOBS-FILE
    END-IF
    ADD 1 TO JOB-ID

    MOVE SPACES TO JOB-TITLE
    MOVE SPACES TO JOB-DESCRIPTION
    MOVE SPACES TO JOB-EMPLOYER
    MOVE SPACES TO JOB-LOCATION
    MOVE SPACES TO JOB-SALARY    *> Safely clearing adjacent field
    MOVE SPACES TO INPUT-TEXT

    *> Job Title (required)
    PERFORM UNTIL FUNCTION LENGTH(FUNCTION TRIM(JOB-TITLE)) > 0
       MOVE "  Enter Job Title:" TO SAVE-TEXT PERFORM SHOW
       READ INPUT-FILE INTO INPUT-TEXT
           AT END
               MOVE 'Y' TO WS-INPUT-EOF
               MOVE 9 TO CHOICE
               MOVE "Ran out of input while detailing job." TO SAVE-TEXT PERFORM SHOW
               EXIT PARAGRAPH
       END-READ
       MOVE FUNCTION TRIM(INPUT-TEXT) TO JOB-TITLE
       IF FUNCTION LENGTH(FUNCTION TRIM(JOB-TITLE)) = 0
            MOVE "Job Title cannot be empty. Job Posting Failed, returning to Main Menu" TO SAVE-TEXT PERFORM SHOW
            EXIT PARAGRAPH
       END-IF
    END-PERFORM

    *> Job Description (required)
    PERFORM UNTIL FUNCTION LENGTH(FUNCTION TRIM(JOB-DESCRIPTION)) > 0
       MOVE "  Enter Job Description:" TO SAVE-TEXT PERFORM SHOW
       READ INPUT-FILE INTO INPUT-TEXT
           AT END
               MOVE 'Y' TO WS-INPUT-EOF
               MOVE 9 TO CHOICE
               MOVE "Ran out of input while detailing job." TO SAVE-TEXT PERFORM SHOW
               EXIT PARAGRAPH
       END-READ
       MOVE FUNCTION TRIM(INPUT-TEXT) TO JOB-DESCRIPTION
       IF FUNCTION LENGTH(FUNCTION TRIM(JOB-DESCRIPTION)) = 0
            MOVE "Job Description cannot be empty. Job Posting Failed, returning to Main Menu" TO SAVE-TEXT PERFORM SHOW
            EXIT PARAGRAPH
       END-IF
    END-PERFORM

    *> Employer (required)
    PERFORM UNTIL FUNCTION LENGTH(FUNCTION TRIM(JOB-EMPLOYER)) > 0
       MOVE "  Enter Employer:" TO SAVE-TEXT PERFORM SHOW
       READ INPUT-FILE INTO INPUT-TEXT
           AT END
               MOVE 'Y' TO WS-INPUT-EOF
               MOVE 9 TO CHOICE
               MOVE "Ran out of input while detailing job." TO SAVE-TEXT PERFORM SHOW
               EXIT PARAGRAPH
       END-READ
       MOVE FUNCTION TRIM(INPUT-TEXT) TO JOB-EMPLOYER
       IF FUNCTION LENGTH(FUNCTION TRIM(JOB-DESCRIPTION)) = 0
            MOVE "Employer cannot be empty. Job Posting Failed, returning to Main Menu" TO SAVE-TEXT PERFORM SHOW
            EXIT PARAGRAPH
       END-IF
    END-PERFORM

    *> Location (required)
    PERFORM UNTIL FUNCTION LENGTH(FUNCTION TRIM(JOB-LOCATION)) > 0
       MOVE "  Enter Location:" TO SAVE-TEXT PERFORM SHOW
       READ INPUT-FILE INTO INPUT-TEXT
           AT END
               MOVE 'Y' TO WS-INPUT-EOF
               MOVE 9 TO CHOICE
               MOVE "Ran out of input while detailing job." TO SAVE-TEXT PERFORM SHOW
               EXIT PARAGRAPH
       END-READ
       MOVE FUNCTION TRIM(INPUT-TEXT) TO JOB-LOCATION
       IF FUNCTION LENGTH(FUNCTION TRIM(JOB-LOCATION)) = 0
            MOVE "Job Location cannot be empty. Job Posting Failed, returning to Main Menu." TO SAVE-TEXT PERFORM SHOW
            EXIT PARAGRAPH
       END-IF
    END-PERFORM
    *> Salary (optional)
    MOVE "  Enter Salary (optional, max 16 chars, enter blank line to skip):" TO SAVE-TEXT PERFORM SHOW
    READ INPUT-FILE INTO INPUT-TEXT
    IF INPUT-FILE-STATUS NOT = "00"
        MOVE 'Y' TO WS-INPUT-EOF
        MOVE 9 TO CHOICE
        MOVE "Ran out of input while detailing job." TO SAVE-TEXT PERFORM SHOW
        EXIT PARAGRAPH
    END-IF
    IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-TEXT)) = 0
        MOVE SPACES TO JOB-SALARY
    ELSE
        MOVE FUNCTION TRIM(INPUT-TEXT) TO JOB-SALARY
    END-IF


    *> Rewrite via temp: copy everything, then add new job
    OPEN INPUT JOBS-FILE
    OPEN OUTPUT JOBS-TEMP-FILE

    *> Copy all existing jobs
    PERFORM UNTIL JOBS-FILE-STATUS = "10"
        READ JOBS-FILE INTO JOBS-LINE
            AT END EXIT PERFORM
        END-READ
        MOVE JOBS-LINE TO JOBS-TEMP-LINE
        WRITE JOBS-TEMP-LINE
    END-PERFORM

    CLOSE JOBS-FILE

    *> Add the new job
    PERFORM WRITE-JOB-BLOCK

    CLOSE JOBS-TEMP-FILE

    *> Rewrite jobs.txt from the staged temp file
    MOVE "00" TO JOBS-TEMP-FILE-STATUS
    OPEN INPUT JOBS-TEMP-FILE
    OPEN OUTPUT JOBS-FILE
    PERFORM UNTIL JOBS-TEMP-FILE-STATUS = "10"
        READ JOBS-TEMP-FILE INTO JOBS-TEMP-LINE
            AT END EXIT PERFORM
        END-READ
        MOVE JOBS-TEMP-LINE TO JOBS-LINE
        WRITE JOBS-LINE
    END-PERFORM
    CLOSE JOBS-TEMP-FILE
    CLOSE JOBS-FILE

    MOVE "Job posted successfully!" TO SAVE-TEXT PERFORM SHOW.


BROWSE-JOBS.
    MOVE 'N' TO WS-JOBS-FILE-READY
    MOVE 0 TO WS-JOB-COUNT
    OPEN INPUT JOBS-FILE
    EVALUATE JOBS-FILE-STATUS
        WHEN "00"
            MOVE 'Y' TO WS-JOBS-FILE-READY
        WHEN "41"
            MOVE "00" TO JOBS-FILE-STATUS
            MOVE 'Y' TO WS-JOBS-FILE-READY
        WHEN "35"
            PERFORM SHOW-NO-JOBS-MESSAGE
            PERFORM CLOSE-JOBS-FILE-SAFE
            EXIT PARAGRAPH
        WHEN OTHER
            MOVE "Unable to read job postings right now. Please try again later." TO SAVE-TEXT PERFORM SHOW
            PERFORM CLOSE-JOBS-FILE-SAFE
            EXIT PARAGRAPH
    END-EVALUATE

    IF WS-JOBS-FILE-READY = 'Y'
        PERFORM LOAD-JOBS-FROM-FILE
        IF WS-JOB-COUNT = 0
            PERFORM CLOSE-JOBS-FILE-SAFE
            PERFORM SHOW-NO-JOBS-MESSAGE
            EXIT PARAGRAPH
        END-IF
        PERFORM CLOSE-JOBS-FILE-SAFE
    END-IF

    MOVE 'N' TO WS-JOB-EXIT

    PERFORM UNTIL WS-JOB-EXIT = 'Y' OR WS-INPUT-EOF = 'Y'
        PERFORM DISPLAY-JOB-LIST
        PERFORM PROMPT-JOB-SELECTION
    END-PERFORM.

SHOW-NO-JOBS-MESSAGE.
    MOVE "--------------------------------" TO SAVE-TEXT PERFORM SHOW
    MOVE "     Browse Jobs/Internships     " TO SAVE-TEXT PERFORM SHOW
    MOVE "Title | Employer | Location | ID" TO SAVE-TEXT PERFORM SHOW
    MOVE "--------------------------------" TO SAVE-TEXT PERFORM SHOW
    MOVE "No Jobs To View!" TO SAVE-TEXT PERFORM SHOW
    MOVE "--------------------------------" TO SAVE-TEXT PERFORM SHOW.


CLOSE-JOBS-FILE-SAFE.
    IF WS-JOBS-FILE-READY = 'Y'
        CLOSE JOBS-FILE
        MOVE 'N' TO WS-JOBS-FILE-READY
    END-IF.

RESET-JOB-TABLE.
    PERFORM VARYING WS-JOB-LOOP-INDEX FROM 1 BY 1
            UNTIL WS-JOB-LOOP-INDEX > WS-JOB-MAX-SLOTS
        MOVE 0 TO WS-JOB-ID-NUM (WS-JOB-LOOP-INDEX)
        MOVE SPACES TO WS-JOB-TITLE-TEXT (WS-JOB-LOOP-INDEX)
        MOVE SPACES TO WS-JOB-DESC-TEXT (WS-JOB-LOOP-INDEX)
        MOVE SPACES TO WS-JOB-EMP-TEXT (WS-JOB-LOOP-INDEX)
        MOVE SPACES TO WS-JOB-LOC-TEXT (WS-JOB-LOOP-INDEX)
        MOVE SPACES TO WS-JOB-SALARY-TEXT (WS-JOB-LOOP-INDEX)
    END-PERFORM.

LOAD-JOBS-FROM-FILE.
    PERFORM RESET-JOB-TABLE
    MOVE 0 TO WS-JOB-COUNT
    MOVE 0 TO WS-JOB-CURRENT-INDEX
    MOVE 'N' TO WS-JOB-IN-PROGRESS
    MOVE 0 TO WS-JOB-MAX-ID
    MOVE "00" TO JOBS-FILE-STATUS

    PERFORM UNTIL JOBS-FILE-STATUS = "10"
        READ JOBS-FILE INTO JOBS-LINE
            AT END EXIT PERFORM
        END-READ

        EVALUATE TRUE
            WHEN JOBS-LINE(1:4) = "ID: "
                IF WS-JOB-COUNT < WS-JOB-MAX-SLOTS
                    COMPUTE WS-JOB-CURRENT-INDEX = WS-JOB-COUNT + 1
                    MOVE 'Y' TO WS-JOB-IN-PROGRESS
                    MOVE 0 TO WS-JOB-ID-NUM (WS-JOB-CURRENT-INDEX)
                    MOVE SPACES TO WS-JOB-TITLE-TEXT (WS-JOB-CURRENT-INDEX)
                    MOVE SPACES TO WS-JOB-DESC-TEXT (WS-JOB-CURRENT-INDEX)
                    MOVE SPACES TO WS-JOB-EMP-TEXT (WS-JOB-CURRENT-INDEX)
                    MOVE SPACES TO WS-JOB-LOC-TEXT (WS-JOB-CURRENT-INDEX)
                    MOVE SPACES TO WS-JOB-SALARY-TEXT (WS-JOB-CURRENT-INDEX)
                    MOVE JOBS-LINE(5:) TO WS-BUF
                    IF FUNCTION LENGTH(FUNCTION TRIM(WS-BUF)) > 0
                        MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-BUF))
                            TO WS-JOB-ID-NUM (WS-JOB-CURRENT-INDEX)
                        IF WS-JOB-MAX-ID < WS-JOB-ID-NUM (WS-JOB-CURRENT-INDEX)
                            MOVE WS-JOB-ID-NUM (WS-JOB-CURRENT-INDEX) TO WS-JOB-MAX-ID
                        END-IF
                    END-IF
                ELSE
                    MOVE 0 TO WS-JOB-CURRENT-INDEX
                    MOVE 'N' TO WS-JOB-IN-PROGRESS
                END-IF
            WHEN JOBS-LINE(1:7) = "Title: "
                IF WS-JOB-IN-PROGRESS = 'Y' AND WS-JOB-CURRENT-INDEX > 0
                    MOVE JOBS-LINE(8:) TO WS-BUF
                    MOVE FUNCTION TRIM(WS-BUF)
                        TO WS-JOB-TITLE-TEXT (WS-JOB-CURRENT-INDEX)
                END-IF
            WHEN JOBS-LINE(1:13) = "Description: "
                IF WS-JOB-IN-PROGRESS = 'Y' AND WS-JOB-CURRENT-INDEX > 0
                    MOVE JOBS-LINE(14:) TO WS-BUF
                    MOVE FUNCTION TRIM(WS-BUF)
                        TO WS-JOB-DESC-TEXT (WS-JOB-CURRENT-INDEX)
                END-IF
            WHEN JOBS-LINE(1:10) = "Employer: "
                IF WS-JOB-IN-PROGRESS = 'Y' AND WS-JOB-CURRENT-INDEX > 0
                    MOVE JOBS-LINE(11:) TO WS-BUF
                    MOVE FUNCTION TRIM(WS-BUF)
                        TO WS-JOB-EMP-TEXT (WS-JOB-CURRENT-INDEX)
                END-IF
            WHEN JOBS-LINE(1:10) = "Location: "
                IF WS-JOB-IN-PROGRESS = 'Y' AND WS-JOB-CURRENT-INDEX > 0
                    MOVE JOBS-LINE(11:) TO WS-BUF
                    MOVE FUNCTION TRIM(WS-BUF)
                        TO WS-JOB-LOC-TEXT (WS-JOB-CURRENT-INDEX)
                END-IF
            WHEN JOBS-LINE(1:8) = "Salary: "
                IF WS-JOB-IN-PROGRESS = 'Y' AND WS-JOB-CURRENT-INDEX > 0
                    MOVE JOBS-LINE(9:) TO WS-BUF
                    MOVE FUNCTION TRIM(WS-BUF)
                        TO WS-JOB-SALARY-TEXT (WS-JOB-CURRENT-INDEX)
                END-IF
            WHEN JOBS-LINE = "-----END-----"
                IF WS-JOB-IN-PROGRESS = 'Y' AND WS-JOB-CURRENT-INDEX > 0
                    IF FUNCTION LENGTH(
                           FUNCTION TRIM(WS-JOB-TITLE-TEXT (WS-JOB-CURRENT-INDEX))
                       ) > 0
                        IF WS-JOB-ID-NUM (WS-JOB-CURRENT-INDEX) = 0
                            ADD 1 TO WS-JOB-MAX-ID
                            MOVE WS-JOB-MAX-ID TO WS-JOB-ID-NUM (WS-JOB-CURRENT-INDEX)
                        END-IF
                        ADD 1 TO WS-JOB-COUNT
                    END-IF
                END-IF
                MOVE 0 TO WS-JOB-CURRENT-INDEX
                MOVE 'N' TO WS-JOB-IN-PROGRESS
            WHEN OTHER
                CONTINUE
        END-EVALUATE
    END-PERFORM

    MOVE 0 TO WS-JOB-CURRENT-INDEX
    MOVE 'N' TO WS-JOB-IN-PROGRESS.

DISPLAY-JOB-LIST.
    MOVE "--------------------------------" TO SAVE-TEXT PERFORM SHOW
    MOVE "     Browse Jobs/Internships     " TO SAVE-TEXT PERFORM SHOW
    MOVE "Title | Employer | Location | ID" TO SAVE-TEXT PERFORM SHOW
    MOVE "--------------------------------" TO SAVE-TEXT PERFORM SHOW

    PERFORM VARYING WS-JOB-LOOP-INDEX FROM 1 BY 1
            UNTIL WS-JOB-LOOP-INDEX > WS-JOB-COUNT
        MOVE WS-JOB-LOOP-INDEX TO WS-JOB-INDEX-DISPLAY
        MOVE WS-JOB-ID-NUM (WS-JOB-LOOP-INDEX) TO WS-JOB-ID-DISPLAY
        MOVE SPACES TO SAVE-TEXT
        STRING WS-IND1                                DELIMITED BY SIZE
               FUNCTION TRIM(WS-JOB-INDEX-DISPLAY)    DELIMITED BY SIZE
               ". "                                   DELIMITED BY SIZE
               FUNCTION TRIM(WS-JOB-TITLE-TEXT (WS-JOB-LOOP-INDEX))
                                                      DELIMITED BY SIZE
               " | "                                 DELIMITED BY SIZE
               FUNCTION TRIM(WS-JOB-EMP-TEXT (WS-JOB-LOOP-INDEX))
                                                      DELIMITED BY SIZE
               " | "                                 DELIMITED BY SIZE
               FUNCTION TRIM(WS-JOB-LOC-TEXT (WS-JOB-LOOP-INDEX))
                                                      DELIMITED BY SIZE
               " (ID: "                              DELIMITED BY SIZE
               FUNCTION TRIM(WS-JOB-ID-DISPLAY)      DELIMITED BY SIZE
               ")"                                   DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
    END-PERFORM

    MOVE "----------------------------------------------------------------" TO SAVE-TEXT PERFORM SHOW.

PROMPT-JOB-SELECTION.
    MOVE "Enter job number or ID to view details (or BACK to Job Menu):" TO SAVE-TEXT
    PERFORM SHOW

    READ INPUT-FILE INTO INPUT-TEXT
        AT END
            MOVE 'Y' TO WS-INPUT-EOF
            MOVE 'Y' TO WS-JOB-EXIT
            EXIT PARAGRAPH
        NOT AT END
            MOVE FUNCTION TRIM(INPUT-TEXT) TO WS-JOB-SELECTION
    END-READ

    IF FUNCTION LENGTH(WS-JOB-SELECTION) = 0
        MOVE "Invalid choice." TO SAVE-TEXT PERFORM SHOW
        EXIT PARAGRAPH
    END-IF

    MOVE FUNCTION UPPER-CASE(WS-JOB-SELECTION) TO WS-JOB-SELECTION-UPPER

    IF WS-JOB-SELECTION-UPPER = "BACK"
        MOVE 'Y' TO WS-JOB-EXIT
        EXIT PARAGRAPH
    END-IF

    IF WS-JOB-SELECTION-UPPER = "B"
        MOVE 'Y' TO WS-JOB-EXIT
        EXIT PARAGRAPH
    END-IF

    MOVE WS-JOB-SELECTION TO WS-JOB-SELECTION-CHECK
    INSPECT WS-JOB-SELECTION-CHECK REPLACING ALL ":" BY SPACE
    MOVE FUNCTION TRIM(WS-JOB-SELECTION-CHECK) TO WS-JOB-SELECTION-CHECK

    MOVE WS-JOB-SELECTION-CHECK TO WS-BUF
    INSPECT WS-BUF CONVERTING "0123456789" TO SPACES

    IF FUNCTION LENGTH(FUNCTION TRIM(WS-BUF)) = 0
        MOVE FUNCTION NUMVAL(WS-JOB-SELECTION-CHECK) TO WS-JOB-SELECTION-NUM
        IF WS-JOB-SELECTION-NUM >= 1 AND WS-JOB-SELECTION-NUM <= WS-JOB-COUNT
            MOVE WS-JOB-SELECTION-NUM TO WS-JOB-DETAIL-INDEX
            PERFORM SHOW-JOB-DETAILS
            EXIT PARAGRAPH
        END-IF
        MOVE WS-JOB-SELECTION-NUM TO WS-JOB-DETAIL-ID
        PERFORM FIND-JOB-BY-ID
        IF WS-JOB-DETAIL-FOUND = 'Y'
            PERFORM SHOW-JOB-DETAILS
        ELSE
            MOVE "No job found with that number or ID." TO SAVE-TEXT PERFORM SHOW
        END-IF
        EXIT PARAGRAPH
    END-IF

    IF WS-JOB-SELECTION-UPPER(1:2) = "ID"
        MOVE SPACES TO WS-JOB-SELECTION-TAIL
        IF FUNCTION LENGTH(WS-JOB-SELECTION) > 2
            MOVE WS-JOB-SELECTION(3:) TO WS-JOB-SELECTION-TAIL
        END-IF
        INSPECT WS-JOB-SELECTION-TAIL REPLACING ALL ":" BY SPACE
        MOVE FUNCTION TRIM(WS-JOB-SELECTION-TAIL) TO WS-JOB-SELECTION-TAIL
        MOVE WS-JOB-SELECTION-TAIL TO WS-BUF
        INSPECT WS-BUF CONVERTING "0123456789" TO SPACES
        IF FUNCTION LENGTH(FUNCTION TRIM(WS-BUF)) = 0
            MOVE FUNCTION NUMVAL(WS-JOB-SELECTION-TAIL) TO WS-JOB-DETAIL-ID
            PERFORM FIND-JOB-BY-ID
            IF WS-JOB-DETAIL-FOUND = 'Y'
                PERFORM SHOW-JOB-DETAILS
            ELSE
                MOVE "No job found with that number or ID." TO SAVE-TEXT PERFORM SHOW
            END-IF
            EXIT PARAGRAPH
        END-IF
    END-IF

    MOVE "Invalid choice." TO SAVE-TEXT PERFORM SHOW.

FIND-JOB-BY-ID.
    MOVE 'N' TO WS-JOB-DETAIL-FOUND
    MOVE 0 TO WS-JOB-DETAIL-INDEX
    PERFORM VARYING WS-JOB-LOOP-INDEX FROM 1 BY 1
            UNTIL WS-JOB-LOOP-INDEX > WS-JOB-COUNT
        IF WS-JOB-ID-NUM (WS-JOB-LOOP-INDEX) = WS-JOB-DETAIL-ID
            MOVE WS-JOB-LOOP-INDEX TO WS-JOB-DETAIL-INDEX
            MOVE 'Y' TO WS-JOB-DETAIL-FOUND
            EXIT PERFORM
        END-IF
    END-PERFORM.

SHOW-JOB-DETAILS.
    IF WS-JOB-DETAIL-INDEX < 1 OR WS-JOB-DETAIL-INDEX > WS-JOB-COUNT
        MOVE "Unable to show job details right now." TO SAVE-TEXT PERFORM SHOW
        EXIT PARAGRAPH
    END-IF

    MOVE WS-JOB-ID-NUM (WS-JOB-DETAIL-INDEX) TO WS-JOB-DETAIL-ID
    MOVE WS-JOB-ID-NUM (WS-JOB-DETAIL-INDEX) TO WS-JOB-ID-DISPLAY
    MOVE WS-JOB-DETAIL-INDEX TO WS-JOB-INDEX-DISPLAY

    MOVE "----------------------------------------------------------------" TO SAVE-TEXT PERFORM SHOW
    MOVE SPACES TO SAVE-TEXT
    STRING WS-IND1                      DELIMITED BY SIZE
           "Job "                       DELIMITED BY SIZE
           FUNCTION TRIM(WS-JOB-INDEX-DISPLAY)
                                         DELIMITED BY SIZE
           " (ID: "                     DELIMITED BY SIZE
           FUNCTION TRIM(WS-JOB-ID-DISPLAY)
                                         DELIMITED BY SIZE
           ")"                          DELIMITED BY SIZE
           INTO SAVE-TEXT
    END-STRING
    PERFORM SHOW

    MOVE SPACES TO SAVE-TEXT
    STRING WS-IND1 "Title: " DELIMITED BY SIZE
           FUNCTION TRIM(WS-JOB-TITLE-TEXT (WS-JOB-DETAIL-INDEX))
                               DELIMITED BY SIZE
           INTO SAVE-TEXT
    END-STRING
    PERFORM SHOW

    MOVE SPACES TO SAVE-TEXT
    STRING WS-IND1 "Employer: " DELIMITED BY SIZE
           FUNCTION TRIM(WS-JOB-EMP-TEXT (WS-JOB-DETAIL-INDEX))
                               DELIMITED BY SIZE
           INTO SAVE-TEXT
    END-STRING
    PERFORM SHOW

    MOVE SPACES TO SAVE-TEXT
    STRING WS-IND1 "Location: " DELIMITED BY SIZE
           FUNCTION TRIM(WS-JOB-LOC-TEXT (WS-JOB-DETAIL-INDEX))
                               DELIMITED BY SIZE
           INTO SAVE-TEXT
    END-STRING
    PERFORM SHOW

    MOVE SPACES TO SAVE-TEXT
    STRING WS-IND1 "Description: " DELIMITED BY SIZE
           FUNCTION TRIM(WS-JOB-DESC-TEXT (WS-JOB-DETAIL-INDEX))
                                   DELIMITED BY SIZE
           INTO SAVE-TEXT
    END-STRING
    PERFORM SHOW

    IF FUNCTION LENGTH(
           FUNCTION TRIM(WS-JOB-SALARY-TEXT (WS-JOB-DETAIL-INDEX))
       ) > 0
        MOVE SPACES TO SAVE-TEXT
        STRING WS-IND1 "Salary: " DELIMITED BY SIZE
               FUNCTION TRIM(WS-JOB-SALARY-TEXT (WS-JOB-DETAIL-INDEX))
                                       DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
    ELSE
        MOVE SPACES TO SAVE-TEXT
        STRING WS-IND1 "Salary: Not provided" DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
    END-IF

    MOVE 'N' TO WS-JOB-DETAIL-EXIT
    PERFORM UNTIL WS-JOB-DETAIL-EXIT = 'Y' OR WS-INPUT-EOF = 'Y'
        MOVE SPACES TO SAVE-TEXT
        STRING WS-IND2 "1. Apply for this Job" INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
        MOVE SPACES TO SAVE-TEXT
        STRING WS-IND2 "2. Back to Job List" INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW

        READ INPUT-FILE INTO INPUT-TEXT
            AT END
                MOVE 'Y' TO WS-INPUT-EOF
                MOVE 'Y' TO WS-JOB-DETAIL-EXIT
                EXIT PERFORM
            NOT AT END
                MOVE FUNCTION TRIM(INPUT-TEXT) TO WS-JOB-SELECTION
        END-READ

        IF WS-JOB-DETAIL-EXIT = 'Y'
            EXIT PERFORM
        END-IF

        MOVE FUNCTION UPPER-CASE(WS-JOB-SELECTION) TO WS-JOB-SELECTION-UPPER

        EVALUATE TRUE
            WHEN WS-JOB-SELECTION-UPPER = "1"
                 OR WS-JOB-SELECTION-UPPER = "APPLY"
                 OR WS-JOB-SELECTION-UPPER = "APPLY FOR THIS JOB"
                PERFORM APPLY-TO-JOB
            WHEN WS-JOB-SELECTION-UPPER = "2"
                 OR WS-JOB-SELECTION-UPPER = "BACK"
                 OR WS-JOB-SELECTION-UPPER = "BACK TO JOB LIST"
                MOVE 'Y' TO WS-JOB-DETAIL-EXIT
            WHEN OTHER
                MOVE "Invalid choice." TO SAVE-TEXT PERFORM SHOW
        END-EVALUATE
    END-PERFORM

    MOVE " " TO SAVE-TEXT
    PERFORM SHOW.

VIEW-MY-APPLICATIONS.
    IF WS-LOGGEDIN NOT = 'Y'
        MOVE "Please log in before viewing applications." TO SAVE-TEXT PERFORM SHOW
        EXIT PARAGRAPH
    END-IF

    MOVE 0 TO WS-APP-COUNT

    OPEN INPUT APPLICATIONS-FILE
    EVALUATE APPLICATIONS-FILE-STATUS
        WHEN "00"
            CONTINUE
        WHEN "35"
            PERFORM PRINT-APPLICATIONS-HEADER
            MOVE "No applications submitted yet." TO SAVE-TEXT PERFORM SHOW
            MOVE WS-APP-COUNT TO WS-JOB-INDEX-DISPLAY
            IF WS-APP-COUNT = 0
                MOVE "0" TO WS-JOB-INDEX-DISPLAY
            END-IF
            MOVE SPACES TO SAVE-TEXT
            STRING WS-IND1 "Total Applications: "
                   FUNCTION TRIM(WS-JOB-INDEX-DISPLAY)
                   INTO SAVE-TEXT
            END-STRING
            PERFORM SHOW
            MOVE " " TO SAVE-TEXT
            PERFORM SHOW
            EXIT PARAGRAPH
        WHEN OTHER
            MOVE "Unable to read applications right now. Please try again later." TO SAVE-TEXT PERFORM SHOW
            EXIT PARAGRAPH
    END-EVALUATE

    PERFORM PRINT-APPLICATIONS-HEADER

    MOVE "00" TO APPLICATIONS-FILE-STATUS
    PERFORM UNTIL APPLICATIONS-FILE-STATUS = "10"
        READ APPLICATIONS-FILE INTO APPLICATIONS-LINE
            AT END EXIT PERFORM
        END-READ

        IF FUNCTION LENGTH(FUNCTION TRIM(APPLICATIONS-LINE)) = 0
            CONTINUE
        END-IF

        MOVE 1 TO WS-UNSTRING-PTR
        MOVE SPACES TO WS-APP-USER
        MOVE SPACES TO WS-APP-JOBID
        MOVE SPACES TO WS-APP-TITLE
        MOVE SPACES TO WS-APP-EMPLOYER
        MOVE SPACES TO WS-APP-LOCATION
        MOVE SPACES TO WS-APP-SALARY
        MOVE SPACES TO WS-APP-TIMESTAMP

        UNSTRING APPLICATIONS-LINE DELIMITED BY "|"
            INTO WS-APP-USER
                 WS-APP-JOBID
                 WS-APP-TITLE
                 WS-APP-EMPLOYER
                 WS-APP-LOCATION
                 WS-APP-SALARY
                 WS-APP-TIMESTAMP
            WITH POINTER WS-UNSTRING-PTR
        END-UNSTRING

        MOVE FUNCTION TRIM(WS-APP-USER) TO WS-APP-USER
        MOVE FUNCTION TRIM(WS-APP-JOBID) TO WS-APP-JOBID
        MOVE FUNCTION TRIM(WS-APP-TITLE) TO WS-APP-TITLE
        MOVE FUNCTION TRIM(WS-APP-EMPLOYER) TO WS-APP-EMPLOYER
        MOVE FUNCTION TRIM(WS-APP-LOCATION) TO WS-APP-LOCATION
        MOVE FUNCTION TRIM(WS-APP-SALARY) TO WS-APP-SALARY

        MOVE WS-APP-TITLE TO WS-APP-DISPLAY-TITLE
        MOVE WS-APP-EMPLOYER TO WS-APP-DISPLAY-EMPLOYER
        MOVE WS-APP-LOCATION TO WS-APP-DISPLAY-LOCATION

        PERFORM POPULATE-APP-DISPLAY-FROM-JOBS

        IF FUNCTION TRIM(WS-APP-USER) = FUNCTION TRIM(WS-NAME)
            ADD 1 TO WS-APP-COUNT
            MOVE SPACES TO SAVE-TEXT
            STRING WS-IND1 DELIMITED BY SIZE
                   "- " DELIMITED BY SIZE
                   FUNCTION TRIM(WS-APP-DISPLAY-TITLE) DELIMITED BY SIZE
                   " | " DELIMITED BY SIZE
                   FUNCTION TRIM(WS-APP-DISPLAY-EMPLOYER) DELIMITED BY SIZE
                   " | " DELIMITED BY SIZE
                   FUNCTION TRIM(WS-APP-DISPLAY-LOCATION) DELIMITED BY SIZE
                   INTO SAVE-TEXT
            END-STRING

            IF FUNCTION LENGTH(FUNCTION TRIM(WS-APP-JOBID)) > 0
                MOVE SPACES TO WS-BUF
                STRING SAVE-TEXT DELIMITED BY SIZE
                       " (Job ID: " DELIMITED BY SIZE
                       FUNCTION TRIM(WS-APP-JOBID) DELIMITED BY SIZE
                       ")" DELIMITED BY SIZE
                       INTO WS-BUF
                END-STRING
                MOVE WS-BUF TO SAVE-TEXT
            END-IF

            PERFORM SHOW
        END-IF
    END-PERFORM

    CLOSE APPLICATIONS-FILE

    IF WS-APP-COUNT = 0
        MOVE "No applications submitted yet." TO SAVE-TEXT PERFORM SHOW
    ELSE
        MOVE "----------------------------------------------------------------" TO SAVE-TEXT PERFORM SHOW
    END-IF

    MOVE WS-APP-COUNT TO WS-JOB-INDEX-DISPLAY
    IF WS-APP-COUNT = 0
        MOVE "0" TO WS-JOB-INDEX-DISPLAY
    END-IF
    MOVE SPACES TO SAVE-TEXT
    STRING WS-IND1 "Total Applications: "
           FUNCTION TRIM(WS-JOB-INDEX-DISPLAY)
           INTO SAVE-TEXT
    END-STRING
    PERFORM SHOW.

POPULATE-APP-DISPLAY-FROM-JOBS.
    MOVE 'N' TO WS-JOB-JOIN-FOUND
    MOVE FUNCTION TRIM(WS-APP-JOBID) TO WS-BUF

    IF FUNCTION LENGTH(WS-BUF) = 0
        EXIT PARAGRAPH
    END-IF

    MOVE 0 TO WS-JOB-JOIN-ID
    COMPUTE WS-JOB-JOIN-ID = FUNCTION NUMVAL(WS-BUF)

    PERFORM VARYING WS-JOB-JOIN-LOOP FROM 1 BY 1
            UNTIL WS-JOB-JOIN-LOOP > WS-JOB-COUNT
               OR WS-JOB-JOIN-FOUND = 'Y'
        IF WS-JOB-ID-NUM (WS-JOB-JOIN-LOOP) = WS-JOB-JOIN-ID
            MOVE FUNCTION TRIM(WS-JOB-TITLE-TEXT (WS-JOB-JOIN-LOOP))
                 TO WS-APP-DISPLAY-TITLE
            MOVE FUNCTION TRIM(WS-JOB-EMP-TEXT (WS-JOB-JOIN-LOOP))
                 TO WS-APP-DISPLAY-EMPLOYER
            MOVE FUNCTION TRIM(WS-JOB-LOC-TEXT (WS-JOB-JOIN-LOOP))
                 TO WS-APP-DISPLAY-LOCATION
            MOVE 'Y' TO WS-JOB-JOIN-FOUND
        END-IF
    END-PERFORM.

PRINT-APPLICATIONS-HEADER.
    MOVE "--------------------------------" TO SAVE-TEXT PERFORM SHOW
    MOVE "     My Applications     " TO SAVE-TEXT PERFORM SHOW
    MOVE "Title | Employer | Location | Job ID" TO SAVE-TEXT PERFORM SHOW.

APPLY-TO-JOB.
    IF WS-LOGGEDIN NOT = 'Y'
        MOVE "Please log in before applying to a job." TO SAVE-TEXT PERFORM SHOW
        MOVE 'Y' TO WS-JOB-DETAIL-EXIT
        EXIT PARAGRAPH
    END-IF

    IF WS-JOB-DETAIL-INDEX < 1 OR WS-JOB-DETAIL-INDEX > WS-JOB-COUNT
        MOVE "Unable to submit application right now." TO SAVE-TEXT PERFORM SHOW
        MOVE 'Y' TO WS-JOB-DETAIL-EXIT
        EXIT PARAGRAPH
    END-IF

    MOVE FUNCTION TRIM(WS-NAME) TO WS-APP-USER
    MOVE WS-JOB-ID-NUM (WS-JOB-DETAIL-INDEX) TO WS-APPLY-JOBID-TXT
    MOVE FUNCTION TRIM(WS-JOB-TITLE-TEXT (WS-JOB-DETAIL-INDEX)) TO WS-APP-TITLE
    MOVE FUNCTION TRIM(WS-JOB-EMP-TEXT (WS-JOB-DETAIL-INDEX)) TO WS-APP-EMPLOYER
    MOVE FUNCTION TRIM(WS-JOB-LOC-TEXT (WS-JOB-DETAIL-INDEX)) TO WS-APP-LOCATION

    IF FUNCTION LENGTH(FUNCTION TRIM(WS-JOB-SALARY-TEXT (WS-JOB-DETAIL-INDEX))) > 0
        MOVE FUNCTION TRIM(WS-JOB-SALARY-TEXT (WS-JOB-DETAIL-INDEX)) TO WS-APP-SALARY
    ELSE
        MOVE "Not provided" TO WS-APP-SALARY
    END-IF

    MOVE 'N' TO WS-APPLY-ALREADY

    OPEN INPUT APPLICATIONS-FILE
    EVALUATE APPLICATIONS-FILE-STATUS
        WHEN "00"
            PERFORM UNTIL APPLICATIONS-FILE-STATUS = "10" OR WS-APPLY-ALREADY = 'Y'
                READ APPLICATIONS-FILE INTO APPLICATIONS-LINE
                    AT END EXIT PERFORM
                END-READ
                MOVE 1 TO WS-UNSTRING-PTR
                MOVE SPACES TO WS-APP-USER
                MOVE SPACES TO WS-APP-JOBID
                MOVE SPACES TO WS-APP-TITLE
                MOVE SPACES TO WS-APP-EMPLOYER
                MOVE SPACES TO WS-APP-LOCATION
                MOVE SPACES TO WS-APP-SALARY
                MOVE SPACES TO WS-APP-TIMESTAMP
                UNSTRING APPLICATIONS-LINE DELIMITED BY "|"
                    INTO WS-APP-USER
                         WS-APP-JOBID
                         WS-APP-TITLE
                         WS-APP-EMPLOYER
                         WS-APP-LOCATION
                         WS-APP-SALARY
                         WS-APP-TIMESTAMP
                    WITH POINTER WS-UNSTRING-PTR
                END-UNSTRING
                MOVE FUNCTION TRIM(WS-APP-USER) TO WS-APP-USER
                MOVE FUNCTION TRIM(WS-APP-JOBID) TO WS-APP-JOBID
                IF FUNCTION LENGTH(FUNCTION TRIM(WS-APP-JOBID)) > 0
                   AND FUNCTION LENGTH(FUNCTION TRIM(WS-APP-USER)) > 0
                   AND FUNCTION TRIM(WS-APP-USER) = FUNCTION TRIM(WS-NAME)
                   AND FUNCTION NUMVAL(FUNCTION TRIM(WS-APP-JOBID))
                       = WS-JOB-ID-NUM (WS-JOB-DETAIL-INDEX)
                    MOVE 'Y' TO WS-APPLY-ALREADY
                END-IF
            END-PERFORM
            CLOSE APPLICATIONS-FILE
        WHEN "35"
            MOVE "00" TO APPLICATIONS-FILE-STATUS
        WHEN OTHER
            MOVE "Unable to read applications right now. Please try again later." TO SAVE-TEXT PERFORM SHOW
            MOVE 'Y' TO WS-JOB-DETAIL-EXIT
            EXIT PARAGRAPH
    END-EVALUATE

    IF WS-APPLY-ALREADY = 'Y'
        MOVE "----------------------------------------------------------------" TO SAVE-TEXT PERFORM SHOW
        MOVE SPACES TO SAVE-TEXT
        STRING "You have already applied to "
               FUNCTION TRIM(WS-JOB-TITLE-TEXT (WS-JOB-DETAIL-INDEX))
               " at "
               FUNCTION TRIM(WS-JOB-EMP-TEXT (WS-JOB-DETAIL-INDEX))
               "." INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
        MOVE "----------------------------------------------------------------" TO SAVE-TEXT PERFORM SHOW
        MOVE 'Y' TO WS-JOB-DETAIL-EXIT
        EXIT PARAGRAPH
    END-IF


    MOVE FUNCTION TRIM(WS-NAME) TO WS-APP-USER
    MOVE WS-JOB-ID-NUM (WS-JOB-DETAIL-INDEX) TO WS-APPLY-JOBID-TXT
    MOVE FUNCTION TRIM(WS-JOB-TITLE-TEXT (WS-JOB-DETAIL-INDEX)) TO WS-APP-TITLE
    MOVE FUNCTION TRIM(WS-JOB-EMP-TEXT (WS-JOB-DETAIL-INDEX)) TO WS-APP-EMPLOYER
    MOVE FUNCTION TRIM(WS-JOB-LOC-TEXT (WS-JOB-DETAIL-INDEX)) TO WS-APP-LOCATION

    IF FUNCTION LENGTH(FUNCTION TRIM(WS-JOB-SALARY-TEXT (WS-JOB-DETAIL-INDEX))) > 0
        MOVE FUNCTION TRIM(WS-JOB-SALARY-TEXT (WS-JOB-DETAIL-INDEX)) TO WS-APP-SALARY
    ELSE
        MOVE "Not provided" TO WS-APP-SALARY
    END-IF

    MOVE 'E' TO WS-APP-FILE-MODE

    OPEN EXTEND APPLICATIONS-FILE
    DISPLAY "DBG APP OPEN STATUS=" APPLICATIONS-FILE-STATUS
    EVALUATE APPLICATIONS-FILE-STATUS
        WHEN "00"
            CONTINUE
        WHEN "30"
            OPEN OUTPUT APPLICATIONS-FILE
            DISPLAY "DBG APP CREATE STATUS=" APPLICATIONS-FILE-STATUS
            IF APPLICATIONS-FILE-STATUS = "00"
                MOVE 'O' TO WS-APP-FILE-MODE
            END-IF
        WHEN "35"
            OPEN OUTPUT APPLICATIONS-FILE
            DISPLAY "DBG APP CREATE STATUS=" APPLICATIONS-FILE-STATUS
            IF APPLICATIONS-FILE-STATUS = "00"
                MOVE 'O' TO WS-APP-FILE-MODE
            END-IF
        WHEN OTHER
            CONTINUE
    END-EVALUATE

    IF APPLICATIONS-FILE-STATUS NOT = "00"
        MOVE SPACES TO SAVE-TEXT
        STRING "Unable to save your application. FILE STATUS "
               APPLICATIONS-FILE-STATUS
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
        MOVE 'Y' TO WS-JOB-DETAIL-EXIT
        EXIT PARAGRAPH
    END-IF

    ACCEPT WS-APPLY-DATE FROM DATE YYYYMMDD
    ACCEPT WS-APPLY-TIME FROM TIME

    MOVE SPACES TO WS-APPLY-TIMESTAMP
    STRING WS-APPLY-DATE(1:4) DELIMITED BY SIZE
           "-"                 DELIMITED BY SIZE
           WS-APPLY-DATE(5:2)  DELIMITED BY SIZE
           "-"                 DELIMITED BY SIZE
           WS-APPLY-DATE(7:2)  DELIMITED BY SIZE
           " "                 DELIMITED BY SIZE
           WS-APPLY-TIME(1:2)  DELIMITED BY SIZE
           ":"                 DELIMITED BY SIZE
           WS-APPLY-TIME(3:2)  DELIMITED BY SIZE
           ":"                 DELIMITED BY SIZE
           WS-APPLY-TIME(5:2)  DELIMITED BY SIZE
           INTO WS-APPLY-TIMESTAMP
    END-STRING

    MOVE SPACES TO WS-APPLICATION-LINE
    STRING FUNCTION TRIM(WS-NAME)                DELIMITED BY SIZE
           "|"                                   DELIMITED BY SIZE
           FUNCTION TRIM(WS-APPLY-JOBID-TXT)     DELIMITED BY SIZE
           "|"                                   DELIMITED BY SIZE
           FUNCTION TRIM(WS-JOB-TITLE-TEXT (WS-JOB-DETAIL-INDEX))
                                                 DELIMITED BY SIZE
           "|"                                   DELIMITED BY SIZE
           FUNCTION TRIM(WS-JOB-EMP-TEXT (WS-JOB-DETAIL-INDEX))
                                                 DELIMITED BY SIZE
           "|"                                   DELIMITED BY SIZE
           FUNCTION TRIM(WS-JOB-LOC-TEXT (WS-JOB-DETAIL-INDEX))
                                                 DELIMITED BY SIZE
           "|"                                   DELIMITED BY SIZE
           FUNCTION TRIM(WS-APP-SALARY)          DELIMITED BY SIZE
           "|"                                   DELIMITED BY SIZE
           FUNCTION TRIM(WS-APPLY-TIMESTAMP)     DELIMITED BY SIZE
           INTO WS-APPLICATION-LINE
    END-STRING

    MOVE WS-APPLICATION-LINE TO APPLICATIONS-LINE
    WRITE APPLICATIONS-LINE
    DISPLAY "DBG APP WRITE STATUS=" APPLICATIONS-FILE-STATUS

    CLOSE APPLICATIONS-FILE

    MOVE "----------------------------------------------------------------" TO SAVE-TEXT PERFORM SHOW
    MOVE SPACES TO SAVE-TEXT
    STRING "Application submitted for "
           FUNCTION TRIM(WS-JOB-TITLE-TEXT (WS-JOB-DETAIL-INDEX))
           " at "
           FUNCTION TRIM(WS-JOB-EMP-TEXT (WS-JOB-DETAIL-INDEX))
           "." INTO SAVE-TEXT
    END-STRING
    PERFORM SHOW

    MOVE 'Y' TO WS-JOB-DETAIL-EXIT.

WRITE-JOB-BLOCK.
    *> Persist the in-memory job (JOB-REC) as text
    MOVE SPACES TO JOBS-TEMP-LINE
    STRING "ID: "  JOB-ID   INTO JOBS-TEMP-LINE END-STRING
    WRITE JOBS-TEMP-LINE

    MOVE SPACES TO JOBS-TEMP-LINE
    STRING "Title: "  JOB-TITLE   INTO JOBS-TEMP-LINE END-STRING
    WRITE JOBS-TEMP-LINE

    MOVE SPACES TO JOBS-TEMP-LINE
    STRING "Description: "    JOB-DESCRIPTION INTO JOBS-TEMP-LINE END-STRING
    WRITE JOBS-TEMP-LINE

    MOVE SPACES TO JOBS-TEMP-LINE
    STRING "Employer: "    JOB-EMPLOYER  INTO JOBS-TEMP-LINE END-STRING
    WRITE JOBS-TEMP-LINE

    MOVE SPACES TO JOBS-TEMP-LINE
    STRING "Location: "  JOB-LOCATION INTO JOBS-TEMP-LINE END-STRING
    WRITE JOBS-TEMP-LINE

    *> Only write salary if it's not empty
    IF FUNCTION LENGTH(FUNCTION TRIM(JOB-SALARY)) > 0
           MOVE SPACES TO JOBS-TEMP-LINE
           STRING "Salary: " JOB-SALARY      INTO JOBS-TEMP-LINE END-STRING
           WRITE JOBS-TEMP-LINE
    END-IF
    MOVE "-----END-----" TO JOBS-TEMP-LINE
    WRITE JOBS-TEMP-LINE.

SKILL-MENU.
    *> Stub skills menu (under construction)
    MOVE 0 TO SKILLCHOICE
    PERFORM UNTIL SKILLCHOICE = 9
        MOVE "Learn a New Skill:" TO SAVE-TEXT PERFORM SHOW
        MOVE "1. Skill 1"          TO SAVE-TEXT PERFORM SHOW
        MOVE "2. Skill 2"          TO SAVE-TEXT PERFORM SHOW
        MOVE "3. Skill 3"          TO SAVE-TEXT PERFORM SHOW
        MOVE "4. Skill 4"          TO SAVE-TEXT PERFORM SHOW
        MOVE "5. Skill 5"          TO SAVE-TEXT PERFORM SHOW
        MOVE "9. Go Back"          TO SAVE-TEXT PERFORM SHOW
        MOVE "Enter your choice:"  TO SAVE-TEXT PERFORM SHOW

        READ INPUT-FILE INTO INPUT-TEXT
            AT END
                MOVE 9 TO SKILLCHOICE
                MOVE "No more input. Returning to main menu." TO SAVE-TEXT
                PERFORM SHOW
            NOT AT END
                MOVE FUNCTION NUMVAL(INPUT-TEXT) TO SKILLCHOICE
        END-READ

        EVALUATE SKILLCHOICE
            WHEN 1
                MOVE "This skill is under construction" TO SAVE-TEXT PERFORM SHOW
            WHEN 2
                MOVE "This skill is under construction" TO SAVE-TEXT PERFORM SHOW
            WHEN 3
                MOVE "This skill is under construction" TO SAVE-TEXT PERFORM SHOW
            WHEN 4
                MOVE "This skill is under construction" TO SAVE-TEXT PERFORM SHOW
            WHEN 5
                MOVE "This skill is under construction" TO SAVE-TEXT PERFORM SHOW
            WHEN 9
                CONTINUE
            WHEN OTHER
                MOVE "Invalid choice, please try again." TO SAVE-TEXT PERFORM SHOW
                MOVE 0 TO SKILLCHOICE
        END-EVALUATE
    END-PERFORM.
PRINT-PROFILE-FRIENDLY.
    *> readable printer for the profile in P-REC
    MOVE WS-HEADER TO SAVE-TEXT PERFORM SHOW
    MOVE "--------------------------" TO SAVE-TEXT PERFORM SHOW

    MOVE SPACES TO SAVE-TEXT
    STRING "Name: " DELIMITED BY SIZE
           FUNCTION TRIM(P-FIRST-NAME) DELIMITED BY SIZE
           " " DELIMITED BY SIZE
           FUNCTION TRIM(P-LAST-NAME)  DELIMITED BY SIZE
           INTO SAVE-TEXT
    END-STRING
    PERFORM SHOW

    MOVE SPACES TO SAVE-TEXT
    STRING "University: " DELIMITED BY SIZE
           FUNCTION TRIM(P-UNIVERSITY) DELIMITED BY SIZE
           INTO SAVE-TEXT
    END-STRING
    PERFORM SHOW

    MOVE SPACES TO SAVE-TEXT
    STRING "Major: " DELIMITED BY SIZE
           FUNCTION TRIM(P-MAJOR) DELIMITED BY SIZE
           INTO SAVE-TEXT
    END-STRING
    PERFORM SHOW

    MOVE P-GRAD-YEAR TO WS-GRAD-YEAR-DISPLAY
    MOVE SPACES TO SAVE-TEXT
    STRING "Graduation Year: " DELIMITED BY SIZE
           WS-GRAD-YEAR-DISPLAY DELIMITED BY SIZE
           INTO SAVE-TEXT
    END-STRING
    PERFORM SHOW

    *> About (print value or "None")
    IF FUNCTION LENGTH(FUNCTION TRIM(P-ABOUT)) > 0
        MOVE SPACES TO SAVE-TEXT
        STRING "About Me: " DELIMITED BY SIZE
               FUNCTION TRIM(P-ABOUT) DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
    ELSE
        MOVE "About Me: None" TO SAVE-TEXT PERFORM SHOW
    END-IF

    *> Experience (print items or "None")
    IF P-EXP-COUNT > 0
        MOVE "Experience:" TO SAVE-TEXT PERFORM SHOW
        PERFORM VARYING P-I FROM 1 BY 1 UNTIL P-I > P-EXP-COUNT
            MOVE SPACES TO SAVE-TEXT
            STRING WS-IND1 DELIMITED BY SIZE
                   "Title: " DELIMITED BY SIZE
                   P-EXP-TITLE(P-I) DELIMITED BY SIZE
                   INTO SAVE-TEXT
            END-STRING
            PERFORM SHOW

            MOVE SPACES TO SAVE-TEXT
            STRING WS-IND2 DELIMITED BY SIZE
                   "Company: " DELIMITED BY SIZE
                   P-EXP-COMPANY(P-I) DELIMITED BY SIZE
                   INTO SAVE-TEXT
            END-STRING
            PERFORM SHOW

            MOVE SPACES TO SAVE-TEXT
            STRING WS-IND2 DELIMITED BY SIZE
                   "Dates: " DELIMITED BY SIZE
                   P-EXP-DATES(P-I) DELIMITED BY SIZE
                   INTO SAVE-TEXT
            END-STRING
            PERFORM SHOW

            IF FUNCTION LENGTH(FUNCTION TRIM(P-EXP-DESC(P-I))) > 0
                MOVE SPACES TO SAVE-TEXT
                STRING WS-IND2 DELIMITED BY SIZE
                       "Description: " DELIMITED BY SIZE
                       P-EXP-DESC(P-I) DELIMITED BY SIZE
                       INTO SAVE-TEXT
                END-STRING
                PERFORM SHOW
            END-IF
        END-PERFORM
    ELSE
        MOVE "Experience: None" TO SAVE-TEXT PERFORM SHOW
    END-IF

    *> Education (print items or "None")
    IF P-EDU-COUNT > 0
        MOVE "Education:" TO SAVE-TEXT PERFORM SHOW
        PERFORM VARYING P-I FROM 1 BY 1 UNTIL P-I > P-EDU-COUNT
            MOVE SPACES TO SAVE-TEXT
            STRING WS-IND1 DELIMITED BY SIZE
                   "Degree: " DELIMITED BY SIZE
                   P-EDU-DEGREE(P-I) DELIMITED BY SIZE
                   INTO SAVE-TEXT
            END-STRING
            PERFORM SHOW

            MOVE SPACES TO SAVE-TEXT
            STRING WS-IND2 DELIMITED BY SIZE
                   "University: " DELIMITED BY SIZE
                   P-EDU-SCHOOL(P-I) DELIMITED BY SIZE
                   INTO SAVE-TEXT
            END-STRING
            PERFORM SHOW

            MOVE SPACES TO SAVE-TEXT
            STRING WS-IND2 DELIMITED BY SIZE
                   "Years: " DELIMITED BY SIZE
                   P-EDU-YEARS(P-I) DELIMITED BY SIZE
                   INTO SAVE-TEXT
            END-STRING
            PERFORM SHOW
        END-PERFORM
    ELSE
        MOVE "Education: None" TO SAVE-TEXT PERFORM SHOW
    END-IF

    *> No trailing separator
    CONTINUE.

*> rejects blank/whitespace, opens src/profiles.txt and reads line-by-line until end of file
*> Detects the start of a block by USER: , then parses that block into P-REC fields. If match, collects all fields(experience, education, etc)
*> if it can’t open prints “No profiles on file", ,
FIND-SOMEONE-YOU-KNOW.
    *> Search profiles by full name (case-insensitive)
    MOVE "Enter the full name of the person you are looking for:" TO SAVE-TEXT
    PERFORM SHOW

    READ INPUT-FILE INTO INPUT-TEXT
        AT END
            MOVE "Missing search input." TO SAVE-TEXT PERFORM SHOW
            EXIT PARAGRAPH
    END-READ

    *> Clean up the search string
    MOVE FUNCTION TRIM(INPUT-TEXT) TO WS-SEARCH-NAME
    INSPECT WS-SEARCH-NAME REPLACING ALL X"0D" BY SPACE
    INSPECT WS-SEARCH-NAME REPLACING ALL X"09" BY SPACE
    MOVE FUNCTION TRIM(WS-SEARCH-NAME) TO WS-SEARCH-NAME
    *> validation for if user enters ' ' for name
    IF FUNCTION LENGTH(FUNCTION TRIM(WS-SEARCH-NAME)) = 0
        MOVE "Name cannot be empty." TO SAVE-TEXT PERFORM SHOW
        EXIT PARAGRAPH
    END-IF

    OPEN INPUT PROFILES-FILE
    IF PROFILES-FILE-STATUS NOT = "00"
        MOVE "No profiles on file." TO SAVE-TEXT PERFORM SHOW
        EXIT PARAGRAPH
    END-IF

    MOVE 'N' TO WS-DONE
    PERFORM UNTIL WS-DONE = 'Y'
        READ PROFILES-FILE INTO PROFILES-LINE
            AT END MOVE 'Y' TO WS-DONE
            NOT AT END
                INSPECT PROFILES-LINE REPLACING ALL X"0D" BY SPACE
                INSPECT PROFILES-LINE REPLACING ALL X"09" BY SPACE

                IF PROFILES-LINE(1:6) = "USER: "
                    MOVE PROFILES-LINE(7:) TO WS-BUF
                    MOVE FUNCTION TRIM(WS-BUF) TO P-USERNAME
                    *> Reset record state
                    MOVE 0 TO WS-BLOCK-LINES
                    MOVE SPACE TO WS-SECTION
                    MOVE 0 TO P-EXP-COUNT P-EDU-COUNT
                    MOVE SPACES TO P-FIRST-NAME P-LAST-NAME P-UNIVERSITY P-MAJOR P-ABOUT
                    MOVE 0 TO P-GRAD-YEAR

                    *> Parse this profile block into P-REC
                    PERFORM UNTIL PROFILES-LINE = "-----END-----"
                        READ PROFILES-FILE INTO PROFILES-LINE
                            AT END EXIT PERFORM
                        END-READ
                        ADD 1 TO WS-BLOCK-LINES
                        IF WS-BLOCK-LINES > 500
                            EXIT PERFORM
                        END-IF

                        INSPECT PROFILES-LINE REPLACING ALL X"0D" BY SPACE
                        INSPECT PROFILES-LINE REPLACING ALL X"09" BY SPACE

                        IF PROFILES-LINE = "-----END-----"
                            EXIT PERFORM
                        ELSE IF PROFILES-LINE = "Experience:"
                            MOVE 'X' TO WS-SECTION
                        ELSE IF PROFILES-LINE = "Education:"
                            MOVE 'U' TO WS-SECTION
                        ELSE IF PROFILES-LINE(1:4) = "FN: "
                            MOVE PROFILES-LINE(5:) TO WS-BUF
                            MOVE FUNCTION TRIM(WS-BUF) TO P-FIRST-NAME
                        ELSE IF PROFILES-LINE(1:4) = "LN: "
                            MOVE PROFILES-LINE(5:) TO WS-BUF
                            MOVE FUNCTION TRIM(WS-BUF) TO P-LAST-NAME
                        ELSE IF PROFILES-LINE(1:6) = "UNIV: "
                            MOVE PROFILES-LINE(7:) TO WS-BUF
                            MOVE FUNCTION TRIM(WS-BUF) TO P-UNIVERSITY
                        ELSE IF PROFILES-LINE(1:7) = "MAJOR: "
                            MOVE PROFILES-LINE(8:) TO WS-BUF
                            MOVE FUNCTION TRIM(WS-BUF) TO P-MAJOR
                        ELSE IF PROFILES-LINE(1:6) = "GRAD: "
                            MOVE PROFILES-LINE(7:) TO WS-BUF
                            MOVE FUNCTION NUMVAL(FUNCTION TRIM(WS-BUF)) TO P-GRAD-YEAR
                        ELSE IF PROFILES-LINE(1:7) = "ABOUT: "
                            MOVE PROFILES-LINE(8:) TO WS-BUF
                            MOVE FUNCTION TRIM(WS-BUF) TO P-ABOUT
                        ELSE
                            EVALUATE WS-SECTION
                                WHEN 'X'
                                    IF PROFILES-LINE(1:7) = "Title: "
                                        IF P-EXP-COUNT < 3
                                            ADD 1 TO P-EXP-COUNT
                                            MOVE PROFILES-LINE(8:) TO WS-BUF
                                            MOVE FUNCTION TRIM(WS-BUF) TO P-EXP-TITLE(P-EXP-COUNT)
                                        END-IF
                                    ELSE IF PROFILES-LINE(1:9) = "Company: "
                                        IF P-EXP-COUNT > 0
                                            MOVE PROFILES-LINE(10:) TO WS-BUF
                                            MOVE FUNCTION TRIM(WS-BUF) TO P-EXP-COMPANY(P-EXP-COUNT)
                                        END-IF
                                    ELSE IF PROFILES-LINE(1:7) = "Dates: "
                                        IF P-EXP-COUNT > 0
                                            MOVE PROFILES-LINE(8:) TO WS-BUF
                                            MOVE FUNCTION TRIM(WS-BUF) TO P-EXP-DATES(P-EXP-COUNT)
                                        END-IF
                                    ELSE IF PROFILES-LINE(1:13) = "Description: "
                                        IF P-EXP-COUNT > 0
                                            MOVE PROFILES-LINE(14:) TO WS-BUF
                                            MOVE FUNCTION TRIM(WS-BUF) TO P-EXP-DESC(P-EXP-COUNT)
                                        END-IF
                                    END-IF
                                WHEN 'U'
                                    IF PROFILES-LINE(1:8) = "Degree: "
                                        IF P-EDU-COUNT < 3
                                            ADD 1 TO P-EDU-COUNT
                                            MOVE PROFILES-LINE(9:) TO WS-BUF
                                            MOVE FUNCTION TRIM(WS-BUF) TO P-EDU-DEGREE(P-EDU-COUNT)
                                        END-IF
                                    ELSE IF PROFILES-LINE(1:12) = "University: "
                                        IF P-EDU-COUNT > 0
                                            MOVE PROFILES-LINE(13:) TO WS-BUF
                                            MOVE FUNCTION TRIM(WS-BUF) TO P-EDU-SCHOOL(P-EDU-COUNT)
                                        END-IF
                                    ELSE IF PROFILES-LINE(1:7) = "Years: "
                                        IF P-EDU-COUNT > 0
                                            MOVE PROFILES-LINE(8:) TO WS-BUF
                                            MOVE FUNCTION TRIM(WS-BUF) TO P-EDU-YEARS(P-EDU-COUNT)
                                        END-IF
                                    END-IF
                                WHEN OTHER
                                    CONTINUE
                            END-EVALUATE
                        END-IF
                    END-PERFORM

                    *> Compare full name if we have both parts
                    IF FUNCTION LENGTH(FUNCTION TRIM(P-FIRST-NAME)) > 0
                       AND FUNCTION LENGTH(FUNCTION TRIM(P-LAST-NAME))  > 0
                        MOVE SPACES TO WS-CANDIDATE-NAME
                        STRING FUNCTION TRIM(P-FIRST-NAME) DELIMITED BY SIZE
                               " " DELIMITED BY SIZE
                               FUNCTION TRIM(P-LAST-NAME)  DELIMITED BY SIZE
                               INTO WS-CANDIDATE-NAME
                        END-STRING

                        IF FUNCTION UPPER-CASE(FUNCTION TRIM(WS-CANDIDATE-NAME))
                           = FUNCTION UPPER-CASE(FUNCTION TRIM(WS-SEARCH-NAME))
                            CLOSE PROFILES-FILE
                            MOVE "--- Found User Profile ---" TO WS-HEADER
                            PERFORM PRINT-PROFILE-FRIENDLY

                            *> Offer to send a connection request
                            MOVE "-------------------------" TO SAVE-TEXT PERFORM SHOW
                            MOVE "  1. Send Connection Request" TO SAVE-TEXT PERFORM SHOW
                            MOVE "  2. Back to Main Menu" TO SAVE-TEXT PERFORM SHOW
                            MOVE "Enter your choice:" TO SAVE-TEXT PERFORM SHOW

                            READ INPUT-FILE INTO INPUT-TEXT
                                AT END
                                    MOVE "No input. Returning to main menu." TO SAVE-TEXT PERFORM SHOW
                                    EXIT PARAGRAPH
                                NOT AT END
                                    EVALUATE FUNCTION TRIM(INPUT-TEXT)
                                        WHEN "1"
                                            *> logged-in user is WS-NAME, recipient is P-USERNAME
                                            MOVE FUNCTION TRIM(WS-NAME) TO WS-CONN-SENDER
                                            MOVE FUNCTION TRIM(P-USERNAME) TO WS-CONN-RECIPIENT
                                            PERFORM SEND-CONNECTION-REQUEST
                                        WHEN OTHER
                                            CONTINUE
                                    END-EVALUATE
                            END-READ

                            EXIT PARAGRAPH

                        END-IF
                    END-IF
                END-IF
        END-READ
    END-PERFORM
    CLOSE PROFILES-FILE

    MOVE "No one by that name could be found." TO SAVE-TEXT PERFORM SHOW.

SEND-CONNECTION-REQUEST.
    *> First check: prevent self-connection requests
    IF FUNCTION TRIM(WS-CONN-SENDER) = FUNCTION TRIM(WS-CONN-RECIPIENT)
        MOVE "You cannot send a connection request to yourself." TO SAVE-TEXT PERFORM SHOW
        EXIT PARAGRAPH
    END-IF

    *> Validate that there is no duplicate or reverse pending request
    MOVE 'N' TO WS-CONN-FOUND

    OPEN INPUT CONNECTIONS-FILE
    IF CONNECTIONS-FILE-STATUS = "00"
        PERFORM UNTIL CONNECTIONS-FILE-STATUS = "10"
            READ CONNECTIONS-FILE INTO CONNECTION-REC
                AT END EXIT PERFORM
            END-READ
            IF FUNCTION TRIM(CONN-SENDER) = FUNCTION TRIM(WS-CONN-SENDER)
               AND FUNCTION TRIM(CONN-RECIPIENT) = FUNCTION TRIM(WS-CONN-RECIPIENT)
                MOVE 'Y' TO WS-CONN-FOUND

            ELSE IF FUNCTION TRIM(CONN-SENDER) = FUNCTION TRIM(WS-CONN-RECIPIENT)
               AND FUNCTION TRIM(CONN-RECIPIENT) = FUNCTION TRIM(WS-CONN-SENDER)
                *> recipient already sent you a request (reverse pending)
                MOVE 'Y' TO WS-CONN-FOUND
            END-IF
        END-PERFORM
        CLOSE CONNECTIONS-FILE
    END-IF

    IF WS-CONN-FOUND = 'Y'
        MOVE "You are already connected with this user or a connection request is pending." TO SAVE-TEXT PERFORM SHOW
        EXIT PARAGRAPH
    END-IF

    *> Append the new pending request (create file if necessary)
    OPEN EXTEND CONNECTIONS-FILE
    IF CONNECTIONS-FILE-STATUS = "35"
        OPEN OUTPUT CONNECTIONS-FILE
        CLOSE CONNECTIONS-FILE
        OPEN EXTEND CONNECTIONS-FILE
    END-IF

    MOVE WS-CONN-SENDER    TO CONN-SENDER
    MOVE WS-CONN-RECIPIENT TO CONN-RECIPIENT
    WRITE CONNECTION-REC
    CLOSE CONNECTIONS-FILE

    MOVE SPACES TO SAVE-TEXT
    STRING "Connection request sent to " DELIMITED BY SIZE
           FUNCTION TRIM(WS-CONN-RECIPIENT) DELIMITED BY SIZE
           "." DELIMITED BY SIZE
           INTO SAVE-TEXT
    END-STRING
    PERFORM SHOW.

VIEW-PENDING-REQUESTS.
    MOVE "--- Pending Connection Requests ---" TO SAVE-TEXT PERFORM SHOW
    MOVE 'N' TO WS-CONN-FOUND
    MOVE 0 TO WS-PENDING-COUNT

    OPEN INPUT CONNECTIONS-FILE
    IF CONNECTIONS-FILE-STATUS = "00"
        PERFORM UNTIL CONNECTIONS-FILE-STATUS = "10"
            READ CONNECTIONS-FILE INTO CONNECTION-REC
                AT END EXIT PERFORM
            END-READ
            IF FUNCTION TRIM(CONN-RECIPIENT) = FUNCTION TRIM(WS-NAME)
                MOVE 'Y' TO WS-CONN-FOUND
                IF WS-PENDING-COUNT < 20
                    ADD 1 TO WS-PENDING-COUNT
                    MOVE CONN-SENDER TO WS-PENDING-SENDERS(WS-PENDING-COUNT)
                END-IF
            END-IF
        END-PERFORM
        CLOSE CONNECTIONS-FILE
    ELSE
        MOVE "You have no pending connection requests at this time." TO SAVE-TEXT PERFORM SHOW
    END-IF

    IF WS-CONN-FOUND = 'N' OR WS-PENDING-COUNT = 0
        MOVE "You have no pending connection requests at this time." TO SAVE-TEXT PERFORM SHOW
    ELSE
        PERFORM VARYING WS-PEND-I FROM 1 BY 1 UNTIL WS-PEND-I > WS-PENDING-COUNT
            MOVE SPACES TO SAVE-TEXT
            STRING "[" DELIMITED BY SIZE
                   FUNCTION TRIM(WS-PEND-I) DELIMITED BY SIZE
                   "] Request from: " DELIMITED BY SIZE
                   FUNCTION TRIM(WS-PENDING-SENDERS(WS-PEND-I)) DELIMITED BY SIZE
                   INTO SAVE-TEXT
            END-STRING
            PERFORM SHOW
        END-PERFORM

        PERFORM VARYING WS-PEND-I FROM 1 BY 1 UNTIL WS-PEND-I > WS-PENDING-COUNT
            MOVE SPACES TO SAVE-TEXT
            STRING "For request #" DELIMITED BY SIZE
                   FUNCTION TRIM(WS-PEND-I) DELIMITED BY SIZE
                   ": (1) Accept  (2) Reject" DELIMITED BY SIZE
                   INTO SAVE-TEXT
            END-STRING
            PERFORM SHOW

            MOVE 0 TO WS-REQ-CHOICE
            MOVE 0 TO WS-REQ-INVALID-COUNT
            PERFORM UNTIL WS-REQ-CHOICE = 1 OR WS-REQ-CHOICE = 2
                     OR WS-REQ-INVALID-COUNT >= 3
                READ INPUT-FILE INTO INPUT-TEXT
                    AT END
                        MOVE 2 TO WS-REQ-CHOICE
                    NOT AT END
                        MOVE FUNCTION NUMVAL(FUNCTION TRIM(INPUT-TEXT)) TO WS-REQ-CHOICE
                END-READ
                IF WS-REQ-CHOICE NOT = 1 AND WS-REQ-CHOICE NOT = 2
                    ADD 1 TO WS-REQ-INVALID-COUNT
                    IF WS-REQ-INVALID-COUNT >= 3
                        MOVE "Too many invalid attempts. Returning to menu." TO SAVE-TEXT
                        PERFORM SHOW
                    ELSE
                        MOVE "Invalid choice. Please enter 1 or 2 to proceed." TO SAVE-TEXT
                        PERFORM SHOW
                    END-IF
                END-IF
            END-PERFORM

            IF WS-REQ-INVALID-COUNT >= 3
                EXIT PERFORM
            END-IF

            EVALUATE WS-REQ-CHOICE
                WHEN 1
                    MOVE WS-PENDING-SENDERS(WS-PEND-I) TO WS-ACCEPT-NAME
                    PERFORM ACCEPT-CONNECTION-BY-USERNAME
                WHEN 2
                    MOVE WS-PENDING-SENDERS(WS-PEND-I) TO WS-ACCEPT-NAME
                    PERFORM REJECT-PENDING-BY-USERNAME
                WHEN OTHER
                    CONTINUE
            END-EVALUATE
        END-PERFORM
    END-IF

    MOVE "-----------------------------------" TO SAVE-TEXT PERFORM SHOW.

ACCEPT-CONNECTION-BY-USERNAME.
    *> Verify that a pending request exists for WS-ACCEPT-NAME -> WS-NAME
    MOVE 'N' TO WS-PENDING-MATCH
    OPEN INPUT CONNECTIONS-FILE
    IF CONNECTIONS-FILE-STATUS = "00"
        PERFORM UNTIL CONNECTIONS-FILE-STATUS = "10"
            READ CONNECTIONS-FILE INTO CONNECTION-REC
                AT END EXIT PERFORM
            END-READ
            IF FUNCTION TRIM(CONN-SENDER) = FUNCTION TRIM(WS-ACCEPT-NAME)
               AND FUNCTION TRIM(CONN-RECIPIENT) = FUNCTION TRIM(WS-NAME)
                MOVE 'Y' TO WS-PENDING-MATCH
                EXIT PERFORM
            END-IF
        END-PERFORM
        CLOSE CONNECTIONS-FILE
    END-IF

    IF WS-PENDING-MATCH = 'Y'
        PERFORM ADD-FRIEND-BIDIRECTIONAL
        PERFORM REMOVE-PENDING-PAIR
        MOVE SPACES TO SAVE-TEXT
        STRING "You are now connected with " DELIMITED BY SIZE
               FUNCTION TRIM(WS-ACCEPT-NAME) DELIMITED BY SIZE
               "." DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
    ELSE
        MOVE "No pending request from that user." TO SAVE-TEXT PERFORM SHOW
    END-IF.

ADD-FRIEND-BIDIRECTIONAL.
    *> Add both directions to FRIENDS-FILE if not already present
    MOVE 'Y' TO WS-NEED-A-TO-B
    MOVE 'Y' TO WS-NEED-B-TO-A

    OPEN INPUT FRIENDS-FILE
    IF FRIENDS-FILE-STATUS = "00"
        PERFORM UNTIL FRIENDS-FILE-STATUS = "10"
            READ FRIENDS-FILE INTO FRIEND-REC
                AT END EXIT PERFORM
            END-READ
            IF FUNCTION TRIM(FR-USER) = FUNCTION TRIM(WS-NAME)
               AND FUNCTION TRIM(FR-FRIEND) = FUNCTION TRIM(WS-ACCEPT-NAME)
                MOVE 'N' TO WS-NEED-A-TO-B
            ELSE IF FUNCTION TRIM(FR-USER) = FUNCTION TRIM(WS-ACCEPT-NAME)
               AND FUNCTION TRIM(FR-FRIEND) = FUNCTION TRIM(WS-NAME)
                MOVE 'N' TO WS-NEED-B-TO-A
            END-IF
        END-PERFORM
        CLOSE FRIENDS-FILE
    END-IF

    OPEN EXTEND FRIENDS-FILE
    IF FRIENDS-FILE-STATUS = "35"
        OPEN OUTPUT FRIENDS-FILE
        CLOSE FRIENDS-FILE
        OPEN EXTEND FRIENDS-FILE
    END-IF

    IF WS-NEED-A-TO-B = 'Y'
        MOVE WS-NAME        TO FR-USER
        MOVE WS-ACCEPT-NAME TO FR-FRIEND
        WRITE FRIEND-REC
    END-IF
    IF WS-NEED-B-TO-A = 'Y'
        MOVE WS-ACCEPT-NAME TO FR-USER
        MOVE WS-NAME        TO FR-FRIEND
        WRITE FRIEND-REC
    END-IF
    CLOSE FRIENDS-FILE.

REMOVE-PENDING-PAIR.
    *> Remove the accepted pending request from CONNECTIONS-FILE
    OPEN INPUT CONNECTIONS-FILE
    IF CONNECTIONS-FILE-STATUS NOT = "00"
        EXIT PARAGRAPH
    END-IF

    OPEN OUTPUT CONN-PROFILES-TEMP-FILE

    PERFORM UNTIL CONNECTIONS-FILE-STATUS = "10"
        READ CONNECTIONS-FILE INTO CONNECTION-REC
            AT END EXIT PERFORM
        END-READ
        IF FUNCTION TRIM(CONN-SENDER) = FUNCTION TRIM(WS-ACCEPT-NAME)
           AND FUNCTION TRIM(CONN-RECIPIENT) = FUNCTION TRIM(WS-NAME)
            CONTINUE
        ELSE
            MOVE CONN-SENDER    TO CONN-TEMP-SENDER
            MOVE CONN-RECIPIENT TO CONN-TEMP-RECIPIENT
            WRITE CONN-TEMP-REC
        END-IF
    END-PERFORM

    CLOSE CONNECTIONS-FILE
    CLOSE CONN-PROFILES-TEMP-FILE

    *> Copy temp to new file, then replace original
    OPEN INPUT CONN-PROFILES-TEMP-FILE
    OPEN OUTPUT CONN-NEW-FILE
    PERFORM UNTIL CONN-PROFILES-TEMP-FILE-STATUS = "10"
        READ CONN-PROFILES-TEMP-FILE INTO CONN-TEMP-REC
            AT END EXIT PERFORM
        END-READ
        MOVE CONN-TEMP-SENDER    TO CONN-NEW-SENDER
        MOVE CONN-TEMP-RECIPIENT TO CONN-NEW-RECIPIENT
        WRITE CONN-NEW-REC
    END-PERFORM
    CLOSE CONN-PROFILES-TEMP-FILE
    CLOSE CONN-NEW-FILE

    CALL "SYSTEM" USING BY CONTENT "mv -f src/connections.new src/connections.txt".

VIEW-MY-NETWORK.
    *> List all users connected to WS-NAME using friends.txt, with full names
    MOVE "--- My Network ---" TO SAVE-TEXT PERFORM SHOW

    MOVE 0 TO WS-PENDING-COUNT

    OPEN INPUT FRIENDS-FILE
    IF FRIENDS-FILE-STATUS = "00"
        PERFORM UNTIL FRIENDS-FILE-STATUS = "10"
            READ FRIENDS-FILE INTO FRIEND-REC
                AT END EXIT PERFORM
            END-READ
            IF FUNCTION TRIM(FR-USER) = FUNCTION TRIM(WS-NAME)
                IF WS-PENDING-COUNT < 20
                    ADD 1 TO WS-PENDING-COUNT
                    MOVE FR-FRIEND TO WS-PENDING-SENDERS(WS-PENDING-COUNT)
                END-IF
            END-IF
        END-PERFORM
        CLOSE FRIENDS-FILE
    END-IF

    IF WS-PENDING-COUNT = 0
        MOVE "You are not connected with anyone yet." TO SAVE-TEXT PERFORM SHOW
        EXIT PARAGRAPH
    END-IF

    *> For each friend username, look up full name (and optionally school/major)
    PERFORM VARYING WS-PEND-I FROM 1 BY 1 UNTIL WS-PEND-I > WS-PENDING-COUNT
        PERFORM LOOKUP-USER-DETAILS
    END-PERFORM.

LOOKUP-USER-DETAILS.
    *> Loads name/university/major from profiles.txt for WS-PENDING-SENDERS(WS-PEND-I)
    MOVE SPACES TO P-FIRST-NAME P-LAST-NAME P-UNIVERSITY P-MAJOR

    OPEN INPUT PROFILES-FILE
    IF PROFILES-FILE-STATUS = "00"
        PERFORM UNTIL PROFILES-FILE-STATUS = "10"
            READ PROFILES-FILE INTO PROFILES-LINE
                AT END EXIT PERFORM
            END-READ
            IF PROFILES-LINE(1:6) = "USER: "
                MOVE PROFILES-LINE(7:) TO WS-BUF
                IF FUNCTION TRIM(WS-BUF) = FUNCTION TRIM(WS-PENDING-SENDERS(WS-PEND-I))
                    *> Within this block, gather fields then print
                    PERFORM UNTIL PROFILES-LINE = "-----END-----"
                        READ PROFILES-FILE INTO PROFILES-LINE
                            AT END EXIT PERFORM
                        END-READ
                        IF PROFILES-LINE = "-----END-----"
                            EXIT PERFORM
                        ELSE IF PROFILES-LINE(1:4) = "FN: "
                            MOVE PROFILES-LINE(5:) TO WS-BUF
                            MOVE FUNCTION TRIM(WS-BUF) TO P-FIRST-NAME
                        ELSE IF PROFILES-LINE(1:4) = "LN: "
                            MOVE PROFILES-LINE(5:) TO WS-BUF
                            MOVE FUNCTION TRIM(WS-BUF) TO P-LAST-NAME
                        ELSE IF PROFILES-LINE(1:6) = "UNIV: "
                            MOVE PROFILES-LINE(7:) TO WS-BUF
                            MOVE FUNCTION TRIM(WS-BUF) TO P-UNIVERSITY
                        ELSE IF PROFILES-LINE(1:7) = "MAJOR: "
                            MOVE PROFILES-LINE(8:) TO WS-BUF
                            MOVE FUNCTION TRIM(WS-BUF) TO P-MAJOR
                        END-IF
                    END-PERFORM

                    MOVE SPACES TO SAVE-TEXT
                    STRING " - " DELIMITED BY SIZE
                           FUNCTION TRIM(P-FIRST-NAME) DELIMITED BY SIZE
                           " " DELIMITED BY SIZE
                           FUNCTION TRIM(P-LAST-NAME)  DELIMITED BY SIZE
                           INTO SAVE-TEXT
                    END-STRING
                    PERFORM SHOW

                    IF FUNCTION LENGTH(FUNCTION TRIM(P-UNIVERSITY)) > 0
                        MOVE SPACES TO SAVE-TEXT
                        STRING WS-IND2 DELIMITED BY SIZE
                               "University: " DELIMITED BY SIZE
                               FUNCTION TRIM(P-UNIVERSITY) DELIMITED BY SIZE
                               INTO SAVE-TEXT
                        END-STRING
                        PERFORM SHOW
                    END-IF

                    IF FUNCTION LENGTH(FUNCTION TRIM(P-MAJOR)) > 0
                        MOVE SPACES TO SAVE-TEXT
                        STRING WS-IND2 DELIMITED BY SIZE
                               "Major: " DELIMITED BY SIZE
                               FUNCTION TRIM(P-MAJOR) DELIMITED BY SIZE
                               INTO SAVE-TEXT
                        END-STRING
                        PERFORM SHOW
                    END-IF
                    CLOSE PROFILES-FILE

                    EXIT PARAGRAPH
                END-IF
            END-IF
        END-PERFORM
        CLOSE PROFILES-FILE
    END-IF

    *> Fallback if profile not found: print username
    MOVE SPACES TO SAVE-TEXT
    STRING " - " DELIMITED BY SIZE
           FUNCTION TRIM(WS-PENDING-SENDERS(WS-PEND-I)) DELIMITED BY SIZE
           INTO SAVE-TEXT
    END-STRING
    PERFORM SHOW.

REJECT-PENDING-BY-USERNAME.
    *> Remove a pending request from WS-ACCEPT-NAME -> WS-NAME (no friends added)
    OPEN INPUT CONNECTIONS-FILE
    IF CONNECTIONS-FILE-STATUS NOT = "00"
        MOVE "You have no pending connection requests at this time." TO SAVE-TEXT PERFORM SHOW
        EXIT PARAGRAPH
    END-IF

    OPEN OUTPUT CONN-PROFILES-TEMP-FILE

    PERFORM UNTIL CONNECTIONS-FILE-STATUS = "10"
        READ CONNECTIONS-FILE INTO CONNECTION-REC
            AT END EXIT PERFORM
        END-READ
        IF FUNCTION TRIM(CONN-SENDER) = FUNCTION TRIM(WS-ACCEPT-NAME)
           AND FUNCTION TRIM(CONN-RECIPIENT) = FUNCTION TRIM(WS-NAME)
            CONTINUE
        ELSE
            MOVE CONN-SENDER    TO CONN-TEMP-SENDER
            MOVE CONN-RECIPIENT TO CONN-TEMP-RECIPIENT
            WRITE CONN-TEMP-REC
        END-IF
    END-PERFORM

    CLOSE CONNECTIONS-FILE
    CLOSE CONN-PROFILES-TEMP-FILE

    *> Copy temp to new file, then replace original
    OPEN INPUT CONN-PROFILES-TEMP-FILE
    OPEN OUTPUT CONN-NEW-FILE
    PERFORM UNTIL CONN-PROFILES-TEMP-FILE-STATUS = "10"
        READ CONN-PROFILES-TEMP-FILE INTO CONN-TEMP-REC
            AT END EXIT PERFORM
        END-READ
        MOVE CONN-TEMP-SENDER    TO CONN-NEW-SENDER
        MOVE CONN-TEMP-RECIPIENT TO CONN-NEW-RECIPIENT
        WRITE CONN-NEW-REC
    END-PERFORM
    CLOSE CONN-PROFILES-TEMP-FILE
    CLOSE CONN-NEW-FILE

    CALL "SYSTEM" USING BY CONTENT "mv -f src/connections.new src/connections.txt"
    MOVE SPACES TO SAVE-TEXT
    STRING "Request from " DELIMITED BY SIZE
           FUNCTION TRIM(WS-ACCEPT-NAME) DELIMITED BY SIZE
           " rejected." DELIMITED BY SIZE
           INTO SAVE-TEXT
    END-STRING
    PERFORM SHOW.
