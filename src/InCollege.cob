>>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. InCollege.
*> AUTHOR. Washington.
*> DATE-WRITTEN. 09/06/2025.
*> Rewritten I/O flow for predictable scripted input

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    *> User accounts (username/password)
    SELECT USERINFO ASSIGN TO "src/userinfo.txt"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS UINFO-FILE-STATUS.

    *> Scripted input (keep static or make dynamic later)
    SELECT INPUT-FILE ASSIGN TO "src/InCollege-Input.txt"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS INPUT-FILE-STATUS.

    *> Human/log transcript (dynamic path via WS-OUTFILE default)
    SELECT APPLOG ASSIGN TO DYNAMIC WS-OUTFILE
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS APPLOG-FILE-STATUS.

    *> Profile persistence
    SELECT PROFILES    ASSIGN TO "src/profiles.txt"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS PROFILES-FILE-STATUS.
    SELECT TEMP-FILE   ASSIGN TO "src/profiles.tmp"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS TEMP-FILE-STATUS.
    SELECT NEW-FILE    ASSIGN TO "src/profiles.new"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS NEW-FILE-STATUS.

DATA DIVISION.
FILE SECTION.
FD USERINFO.
01 USER-REC.
    05 IN-USERNAME PIC X(20).
    05 IN-PASSWORD PIC X(20).

FD INPUT-FILE.
01 INPUT-REC.
    05 INPUT-TEXT PIC X(256).

FD APPLOG.
01 SAVE-RECORD.
    05 SAVE-TEXT PIC X(200).

FD PROFILES.
01 PROFILES-LINE PIC X(256).

FD TEMP-FILE.
01 TEMP-LINE PIC X(256).

FD NEW-FILE.
01 NEW-LINE PIC X(256).

WORKING-STORAGE SECTION.
   77 WS-OUTFILE PIC X(256) VALUE "src/InCollege-Output.txt".
*> File status
01 UINFO-FILE-STATUS PIC XX.
01 INPUT-FILE-STATUS PIC XX.
01 APPLOG-FILE-STATUS PIC XX.
01 PROFILES-FILE-STATUS PIC XX.
01 TEMP-FILE-STATUS     PIC XX.
01 NEW-FILE-STATUS      PIC XX.

*> EOF flags
01 INFOEOF   PIC A(1) VALUE 'N'.

*> Flow / actions
01 CURRENT-ACTION PIC X(20).
01 WS-LOGIN PIC X(5)  VALUE 'LOGIN'.
01 WS-NEW   PIC X(18) VALUE 'CREATE NEW ACCOUNT'.

*> Auth
01 WS-NAME      PIC X(20).
01 WS-PASSWORD  PIC X(20).
01 WS-LOGGEDIN  PIC A(1) VALUE 'N'.

*> Scratch buffers
01 LINE-K     PIC X(32).
01 LINE-V     PIC X(224).
01 WS-BUF     PIC X(256).

*> Profile flags
01 PROFILE-FOUND PIC A(1) VALUE 'N'.

*> Display helpers
01 WS-GRAD-YEAR-DISPLAY PIC X(4).
01 WS-IDX-TXT           PIC 99.
01 WS-SECTION           PIC X(1) VALUE SPACE.
01 CUR-EXP-IDX          PIC 9 VALUE 0.
01 CUR-EDU-IDX          PIC 9 VALUE 0.

*> Password validation
01 WS-HASCAPITAL PIC A(1) VALUE 'N'.
01 WS-HASDIGIT   PIC A(1) VALUE 'N'.
01 WS-HASSPECIAL PIC A(1) VALUE 'N'.
01 WS-CHARCOUNT  PIC 9(2) VALUE 0.
01 WS-MINPASSWORDCOUNT PIC 9(2) VALUE 8.
01 WS-MAXPASSWORDCOUNT PIC 9(2) VALUE 12.
01 WS-INSPECTEDCHAR PIC X(1).
01 I PIC 9(2) VALUE 1.

*> Account management
01 WS-NUMACCOUNTS      PIC 9(1) VALUE 0.
01 WS-NEWUSERNAME      PIC X(20).
01 WS-UNIQUEUSERSTATUS PIC A(1) VALUE 'N'.
01 WS-ABORT-CREATE     PIC A(1) VALUE 'N'.

*> Menus
77 CHOICE      PIC 9 VALUE 0.
77 SKILLCHOICE PIC 9 VALUE 0.

*> Profile record in memory
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

01 VALID-YEAR PIC A(1) VALUE 'N'.
01 MIN-YEAR   PIC 9(4) VALUE 1980.
01 MAX-YEAR   PIC 9(4) VALUE 2100.
01 P-I        PIC 9 VALUE 0.

PROCEDURE DIVISION.
MAIN.
    OPEN INPUT  INPUT-FILE

    *> Static output path; no env override

    *> Open APPLOG fresh (truncate/create)
    OPEN OUTPUT APPLOG
    EVALUATE APPLOG-FILE-STATUS
        WHEN "00"
            CONTINUE
        WHEN "61"
            *> File exists and cannot be opened with OUTPUT; remove and retry
            CALL "SYSTEM" USING BY CONTENT "rm -f src/InCollege-Output.txt"
            OPEN OUTPUT APPLOG
            IF APPLOG-FILE-STATUS NOT = "00"
                DISPLAY "APPLOG OPEN FAILED: " APPLOG-FILE-STATUS " (src/InCollege-Output.txt)"
            END-IF
        WHEN OTHER
            DISPLAY "APPLOG OPEN FAILED: " APPLOG-FILE-STATUS " (src/InCollege-Output.txt)"
    END-EVALUATE


    *> Ensure profiles file exists
    OPEN INPUT PROFILES
    IF PROFILES-FILE-STATUS = "00"
        CLOSE PROFILES
    ELSE
        IF PROFILES-FILE-STATUS = "35"
            OPEN OUTPUT PROFILES
            CLOSE PROFILES
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

    *> Done after the chosen flow returns
    MOVE "--- END_OF_PROGRAM_EXECUTION ---" TO SAVE-TEXT PERFORM SHOW
    CLOSE INPUT-FILE
    CLOSE APPLOG
    STOP RUN.

SHOW-MAIN-MENU.
    *> Welcome + first choice (reusable)
    MOVE "Welcome to InCollege!" TO SAVE-TEXT PERFORM SHOW
    MOVE "1. Log In"            TO SAVE-TEXT PERFORM SHOW
    MOVE "2. Create New Account" TO SAVE-TEXT PERFORM SHOW
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
    IF WS-LOGGEDIN = 'Y'
        MOVE "You are already logged in." TO SAVE-TEXT PERFORM SHOW
        EXIT PARAGRAPH
    ELSE IF WS-NUMACCOUNTS >= 5
        MOVE "All permitted accounts have been created, please come back later" TO SAVE-TEXT PERFORM SHOW
        EXIT PARAGRAPH
    END-IF

    MOVE 'N' TO WS-UNIQUEUSERSTATUS
    MOVE 'N' TO WS-ABORT-CREATE
    *> Ask until username is unique
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

    *> Ask for password until valid
    PERFORM UNTIL WS-LOGGEDIN = 'Y'
        MOVE "Enter password (8-12 chars, 1 capital, 1 digit, 1 special):" TO SAVE-TEXT PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
            AT END MOVE "No more input while creating account." TO SAVE-TEXT PERFORM SHOW
                 EXIT PARAGRAPH
        END-READ
        MOVE FUNCTION TRIM(INPUT-TEXT) TO IN-PASSWORD

        MOVE WS-NEWUSERNAME TO IN-USERNAME
        PERFORM CHECKPASSWORD
        *> On success, CHECKPASSWORD sets WS-LOGGEDIN='Y', writes USERINFO, and jumps to NAV-MENU.
    END-PERFORM.

SHOW.
    DISPLAY SAVE-TEXT
    MOVE SAVE-TEXT TO SAVE-RECORD
    WRITE SAVE-RECORD
    IF APPLOG-FILE-STATUS NOT = "00"
        DISPLAY "APPLOG WRITE FAILED: " APPLOG-FILE-STATUS
    END-IF.


CHECKPASSWORD.
    *> Reset flags/counters
    MOVE 0  TO WS-CHARCOUNT
    MOVE 'N' TO WS-HASDIGIT
    MOVE 'N' TO WS-HASCAPITAL
    MOVE 'N' TO WS-HASSPECIAL

    *> Normalize: trim spaces, remove Windows CR (\r) and tabs if any
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

            MOVE IN-USERNAME TO WS-NAME

            *> Ensure file exists before appending
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
    MOVE "--- Create/Edit Profile ---" TO SAVE-TEXT PERFORM SHOW

    *> First Name (required)
    MOVE "Enter First Name:" TO SAVE-TEXT PERFORM SHOW
    READ INPUT-FILE INTO INPUT-TEXT
    MOVE FUNCTION TRIM(INPUT-TEXT) TO P-FIRST-NAME

    *> Last Name (required)
    MOVE "Enter Last Name:" TO SAVE-TEXT PERFORM SHOW
    READ INPUT-FILE INTO INPUT-TEXT
    MOVE FUNCTION TRIM(INPUT-TEXT) TO P-LAST-NAME

    *> University (required)
    MOVE "Enter University/College Attended:" TO SAVE-TEXT PERFORM SHOW
    READ INPUT-FILE INTO INPUT-TEXT
    MOVE FUNCTION TRIM(INPUT-TEXT) TO P-UNIVERSITY

    *> Major (required)
    MOVE "Enter Major:" TO SAVE-TEXT PERFORM SHOW
    READ INPUT-FILE INTO INPUT-TEXT
    MOVE FUNCTION TRIM(INPUT-TEXT) TO P-MAJOR

    *> Graduation Year (required)
    MOVE "Enter Graduation Year (YYYY):" TO SAVE-TEXT PERFORM SHOW
    READ INPUT-FILE INTO INPUT-TEXT
    MOVE FUNCTION NUMVAL(FUNCTION TRIM(INPUT-TEXT)) TO P-GRAD-YEAR

    *> About (optional)
    MOVE "Enter About Me (optional, max 200 chars, enter blank line to skip):" TO SAVE-TEXT PERFORM SHOW
    READ INPUT-FILE INTO INPUT-TEXT
    IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-TEXT)) = 0
        MOVE SPACES TO P-ABOUT
    ELSE
        MOVE FUNCTION TRIM(INPUT-TEXT) TO P-ABOUT
    END-IF

    *> Experience entries (prompt order adjusted to include Title prompt)
    MOVE 0 TO P-EXP-COUNT
    MOVE "Add Experience (optional, max 3 entries. Enter 'DONE' to finish):" TO SAVE-TEXT PERFORM SHOW

    PERFORM UNTIL P-EXP-COUNT >= 3
        COMPUTE P-I = P-EXP-COUNT + 1
        MOVE P-I TO WS-IDX-TXT
        IF WS-IDX-TXT(1:1) = '0'
            MOVE WS-IDX-TXT(2:1) TO WS-IDX-TXT(1:1)
            MOVE SPACE TO WS-IDX-TXT(2:1)
        END-IF
        MOVE SPACES TO SAVE-TEXT
        STRING "Experience #" DELIMITED BY SIZE
               FUNCTION TRIM(WS-IDX-TXT) DELIMITED BY SIZE
               " - Title:" DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
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
        STRING "Experience #" DELIMITED BY SIZE
               FUNCTION TRIM(WS-IDX-TXT) DELIMITED BY SIZE
               " - Company/Organization:" DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
        MOVE FUNCTION TRIM(INPUT-TEXT) TO P-EXP-COMPANY(P-EXP-COUNT)

        MOVE SPACES TO SAVE-TEXT
        STRING "Experience #" DELIMITED BY SIZE
               FUNCTION TRIM(WS-IDX-TXT) DELIMITED BY SIZE
               " - Dates (e.g., Summer 2024):" DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
        MOVE FUNCTION TRIM(INPUT-TEXT) TO P-EXP-DATES(P-EXP-COUNT)

        MOVE SPACES TO SAVE-TEXT
        STRING "Experience #" DELIMITED BY SIZE
               FUNCTION TRIM(WS-IDX-TXT) DELIMITED BY SIZE
               " - Description (optional, max 100 chars, blank to skip):" DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
        IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-TEXT)) > 0
            MOVE FUNCTION TRIM(INPUT-TEXT) TO P-EXP-DESC(P-EXP-COUNT)
        ELSE
            MOVE SPACES TO P-EXP-DESC(P-EXP-COUNT)
        END-IF

        IF P-EXP-COUNT < 3
            MOVE "Add Experience (optional, max 3 entries. Enter 'DONE' to finish):" TO SAVE-TEXT PERFORM SHOW
        END-IF
    END-PERFORM

    *> Education entries (prompt order adjusted to include Degree prompt)
    MOVE 0 TO P-EDU-COUNT
    MOVE "Add Education (optional, max 3 entries. Enter 'DONE' to finish):" TO SAVE-TEXT PERFORM SHOW

    PERFORM UNTIL P-EDU-COUNT >= 3
        COMPUTE P-I = P-EDU-COUNT + 1
        MOVE P-I TO WS-IDX-TXT
        IF WS-IDX-TXT(1:1) = '0'
            MOVE WS-IDX-TXT(2:1) TO WS-IDX-TXT(1:1)
            MOVE SPACE TO WS-IDX-TXT(2:1)
        END-IF
        MOVE SPACES TO SAVE-TEXT
        STRING "Education #" DELIMITED BY SIZE
               FUNCTION TRIM(WS-IDX-TXT) DELIMITED BY SIZE
               " - Degree:" DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
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
        STRING "Education #" DELIMITED BY SIZE
               FUNCTION TRIM(WS-IDX-TXT) DELIMITED BY SIZE
               " - University/College:" DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
        MOVE FUNCTION TRIM(INPUT-TEXT) TO P-EDU-SCHOOL(P-EDU-COUNT)

        MOVE SPACES TO SAVE-TEXT
        STRING "Education #" DELIMITED BY SIZE
               FUNCTION TRIM(WS-IDX-TXT) DELIMITED BY SIZE
               " - Years Attended (e.g., 2023-2025):" DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
        MOVE FUNCTION TRIM(INPUT-TEXT) TO P-EDU-YEARS(P-EDU-COUNT)

        IF P-EDU-COUNT < 3
            MOVE "Add Education (optional, max 3 entries. Enter 'DONE' to finish):" TO SAVE-TEXT PERFORM SHOW
        END-IF
    END-PERFORM

    *> Save profile
    MOVE WS-NAME TO P-USERNAME
    PERFORM SAVE-PROFILE

    MOVE "Profile saved successfully!" TO SAVE-TEXT PERFORM SHOW.
NAV-MENU.
    MOVE 0 TO CHOICE
    PERFORM UNTIL CHOICE = 9
        MOVE "1. Create/Edit My Profile" TO SAVE-TEXT PERFORM SHOW
        MOVE "2. View My Profile"        TO SAVE-TEXT PERFORM SHOW
        MOVE "3. Search for User"        TO SAVE-TEXT PERFORM SHOW
        MOVE "4. Learn a New Skill"      TO SAVE-TEXT PERFORM SHOW
        MOVE "9. Exit" TO SAVE-TEXT PERFORM SHOW
        MOVE "Enter your choice:"        TO SAVE-TEXT PERFORM SHOW

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
                MOVE "Search for User is under construction." TO SAVE-TEXT PERFORM SHOW
            WHEN CHOICE = 4
                PERFORM SKILL-MENU
            WHEN CHOICE = 9
                CONTINUE
            WHEN OTHER
                MOVE "Invalid choice, please try again." TO SAVE-TEXT PERFORM SHOW
                MOVE 0 TO CHOICE
        END-EVALUATE
    END-PERFORM.


WRITE-PROFILE-BLOCK.
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

    *> Only persist About when non-empty
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
    END-IF

    MOVE "-----END-----" TO TEMP-LINE
    WRITE TEMP-LINE.

SAVE-PROFILE.
    MOVE "N" TO PROFILE-FOUND

    *> Ensure profiles exists
    OPEN INPUT PROFILES
    IF PROFILES-FILE-STATUS NOT = "00"
        OPEN OUTPUT PROFILES
        CLOSE PROFILES
        OPEN INPUT PROFILES
    END-IF

    *> Truncate temp and start copying
    OPEN OUTPUT TEMP-FILE

    PERFORM UNTIL PROFILES-FILE-STATUS = "10"
        READ PROFILES INTO PROFILES-LINE
            AT END EXIT PERFORM
        END-READ

        IF PROFILES-LINE(1:6) = "USER: "
            MOVE PROFILES-LINE(7:) TO WS-BUF
            IF FUNCTION TRIM(WS-BUF) = FUNCTION TRIM(P-USERNAME)
                *> Skip this block
                PERFORM UNTIL PROFILES-LINE = "END" OR PROFILES-LINE = "-----END-----"
                    READ PROFILES INTO PROFILES-LINE
                        AT END EXIT PERFORM
                    END-READ
                END-PERFORM
                *> Write new block
                PERFORM WRITE-PROFILE-BLOCK
                MOVE "Y" TO PROFILE-FOUND
            ELSE
                *> Copy other user's block
                MOVE PROFILES-LINE TO TEMP-LINE
                WRITE TEMP-LINE
                PERFORM UNTIL PROFILES-LINE = "END" OR PROFILES-LINE = "-----END-----"
                    READ PROFILES INTO PROFILES-LINE
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

    CLOSE PROFILES

    IF PROFILE-FOUND NOT = "Y"
        PERFORM WRITE-PROFILE-BLOCK
    END-IF

    CLOSE TEMP-FILE

    *> Atomic replace
    OPEN INPUT  TEMP-FILE
    OPEN OUTPUT NEW-FILE
    PERFORM UNTIL TEMP-FILE-STATUS = "10"
        READ TEMP-FILE INTO TEMP-LINE
            AT END EXIT PERFORM
        END-READ
        MOVE TEMP-LINE TO NEW-LINE
        WRITE NEW-LINE
    END-PERFORM
    CLOSE TEMP-FILE
    CLOSE NEW-FILE

    CALL "SYSTEM" USING BY CONTENT "mv -f src/profiles.new src/profiles.txt".

VIEW-PROFILE.
    MOVE "--- Your Profile ---" TO SAVE-TEXT PERFORM SHOW

    MOVE 'N' TO PROFILE-FOUND
    OPEN INPUT PROFILES
    MOVE SPACES TO PROFILES-LINE
    MOVE SPACE TO WS-SECTION
    MOVE 0 TO CUR-EXP-IDX CUR-EDU-IDX

    PERFORM UNTIL 1 = 2
        READ PROFILES INTO PROFILES-LINE
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
                    READ PROFILES INTO PROFILES-LINE
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

                *> Friendly view
                MOVE SPACES TO SAVE-TEXT
                STRING "Name: " DELIMITED BY SIZE
                       FUNCTION TRIM(P-FIRST-NAME) DELIMITED BY SIZE
                       " " DELIMITED BY SIZE
                       FUNCTION TRIM(P-LAST-NAME) DELIMITED BY SIZE
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

                *> Only display About when non-empty
                IF FUNCTION LENGTH(FUNCTION TRIM(P-ABOUT)) > 0
                    MOVE SPACES TO SAVE-TEXT
                    STRING "About Me: " DELIMITED BY SIZE
                           FUNCTION TRIM(P-ABOUT) DELIMITED BY SIZE
                           INTO SAVE-TEXT
                    END-STRING
                    PERFORM SHOW
                END-IF

                IF P-EXP-COUNT > 0
                    MOVE "Experience:" TO SAVE-TEXT PERFORM SHOW
                    PERFORM VARYING P-I FROM 1 BY 1 UNTIL P-I > P-EXP-COUNT
                        MOVE SPACES TO SAVE-TEXT
                        STRING "Title: " P-EXP-TITLE(P-I) INTO SAVE-TEXT END-STRING
                        PERFORM SHOW
                        MOVE SPACES TO SAVE-TEXT
                        STRING "Company: " P-EXP-COMPANY(P-I) INTO SAVE-TEXT END-STRING
                        PERFORM SHOW
                        MOVE SPACES TO SAVE-TEXT
                        STRING "Dates: " P-EXP-DATES(P-I) INTO SAVE-TEXT END-STRING
                        PERFORM SHOW
                        IF FUNCTION LENGTH(FUNCTION TRIM(P-EXP-DESC(P-I))) > 0
                            MOVE SPACES TO SAVE-TEXT
                            STRING "Description: " P-EXP-DESC(P-I) INTO SAVE-TEXT END-STRING
                            PERFORM SHOW
                        END-IF
                    END-PERFORM
                END-IF

                IF P-EDU-COUNT > 0
                    MOVE "Education:" TO SAVE-TEXT PERFORM SHOW
                    PERFORM VARYING P-I FROM 1 BY 1 UNTIL P-I > P-EDU-COUNT
                        MOVE SPACES TO SAVE-TEXT
                        STRING "Degree: " P-EDU-DEGREE(P-I) INTO SAVE-TEXT END-STRING
                        PERFORM SHOW
                        MOVE SPACES TO SAVE-TEXT
                        STRING "University: " P-EDU-SCHOOL(P-I) INTO SAVE-TEXT END-STRING
                        PERFORM SHOW
                        MOVE SPACES TO SAVE-TEXT
                        STRING "Years: " P-EDU-YEARS(P-I) INTO SAVE-TEXT END-STRING
                        PERFORM SHOW
                    END-PERFORM
                END-IF

                EXIT PERFORM
            END-IF
        END-IF
    END-PERFORM

    CLOSE PROFILES

    IF PROFILE-FOUND = 'Y'
        MOVE "--------------------" TO SAVE-TEXT PERFORM SHOW
    ELSE
        MOVE "No profile found." TO SAVE-TEXT PERFORM SHOW
    END-IF.

SKILL-MENU.
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
