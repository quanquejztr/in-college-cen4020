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
    SELECT USERINFO ASSIGN TO "src/userinfo.txt"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS UINFO-FILE-STATUS.

*> Define input file for reading test commands and user input
    SELECT INPUT-FILE ASSIGN TO "src/InCollege-Test.txt"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS INPUT-FILE-STATUS.

*> Define output file for logging application messages and responses
    SELECT APPLOG ASSIGN TO "src/InCollege-Output.txt"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS APPLOG-FILE-STATUS.

    *> Profile persistence files
    SELECT PROFILES ASSIGN TO "src/profiles.txt"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS PROFILES-FILE-STATUS.

    SELECT TEMP-FILE ASSIGN TO "src/profiles.tmp"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS TEMP-FILE-STATUS.

    *> New file for atomic replace
    SELECT NEW-FILE ASSIGN TO "src/profiles.new"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS NEW-FILE-STATUS.


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
    05 INPUT-TEXT PIC X(256).

*> File description for application log output
FD APPLOG.
01 SAVE-RECORD.
    05 SAVE-TEXT PIC X(200).

*> Profile data file (one line per record, simple key=value style later)
FD PROFILES.
01 PROFILES-LINE PIC X(256).

*> Temporary file for updates (used when editing existing profiles)
FD TEMP-FILE.
01 TEMP-LINE PIC X(256).

*> New output file used for atomic replacement of profiles.txt
FD NEW-FILE.
01 NEW-LINE PIC X(256).


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
01 NEW-FILE-STATUS      PIC XX.

*> Buffers for parsing profile lines (key=value pairs later)
01 LINE-K PIC X(32).
01 LINE-V PIC X(224).
01 WS-BUF PIC X(256).

*> Flag to mark if a profile exists for current user
01 PROFILE-FOUND PIC A(1) VALUE 'N'.

*> Display buffer for writing graduation year in text
01 WS-GRAD-YEAR-DISPLAY PIC X(4).

*> Display buffer for numbering prompts (1..3)
01 WS-IDX-TXT PIC X(2).
01 WS-SECTION PIC X(1) VALUE SPACE.
01 CUR-EXP-IDX PIC 9 VALUE 0.
01 CUR-EDU-IDX PIC 9 VALUE 0.

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
01 FIELD-OK PIC A(1) VALUE "N".

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
01 MIN-YEAR   PIC 9(4) VALUE 1980.
01 MAX-YEAR   PIC 9(4) VALUE 2100.
01 P-I        PIC 9 VALUE 0.

PROCEDURE DIVISION.
*> Main program execution
    OPEN INPUT  INPUT-FILE
    OPEN OUTPUT APPLOG
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
    MOVE 0  TO WS-CHARCOUNT
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
               ELSE
                   IF (WS-INSPECTEDCHAR >= '!'
                        AND WS-INSPECTEDCHAR <= '/')
                    OR (WS-INSPECTEDCHAR >= ':'
                        AND WS-INSPECTEDCHAR <= '@')
                    OR (WS-INSPECTEDCHAR >= '['
                        AND WS-INSPECTEDCHAR <= '`')
                    OR (WS-INSPECTEDCHAR >= '{'
                        AND WS-INSPECTEDCHAR <= '~')
                        MOVE 'Y' TO WS-HASSPECIAL
                   END-IF
               END-IF
           END-PERFORM

           IF WS-HASCAPITAL = 'Y'
              AND WS-HASDIGIT = 'Y'
              AND WS-HASSPECIAL = 'Y'
               MOVE 'Y' TO WS-LOGGEDIN
               MOVE "Account created successfully." TO SAVE-TEXT
               PERFORM SHOW
               MOVE SPACES TO SAVE-TEXT
               STRING "Welcome, " DELIMITED BY SIZE
                      FUNCTION TRIM(IN-USERNAME) DELIMITED BY SIZE
                      INTO SAVE-TEXT
               END-STRING
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
    MOVE 'N' TO WS-LOGGEDIN
    OPEN INPUT USERINFO
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

    IF WS-LOGGEDIN = 'Y'
        MOVE "You have successfully logged in." TO SAVE-TEXT
        PERFORM SHOW
        MOVE SPACES TO SAVE-TEXT
        STRING "Welcome, " DELIMITED BY SIZE
               FUNCTION TRIM(WS-NAME) DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
    ELSE
        MOVE "Wrong credentials. Try again." TO SAVE-TEXT
        PERFORM SHOW
    END-IF.

PARSEINPUT.
    IF INPUT-TEXT = WS-LOGIN
        IF WS-LOGGEDIN = 'Y'
            MOVE "You are already logged in." TO SAVE-TEXT
            PERFORM SHOW
        ELSE
            *> Reset file-EOF flag before using it as a loop sentinel for login attempts
            MOVE 'N' TO INFOEOF
            PERFORM UNTIL INFOEOF='Y'
                MOVE "Please enter your username:" TO SAVE-TEXT
                PERFORM SHOW
                MOVE "Please enter your password:" TO SAVE-TEXT
                PERFORM SHOW
                READ INPUT-FILE INTO INPUT-TEXT
                MOVE INPUT-TEXT TO WS-NAME
                READ INPUT-FILE INTO INPUT-TEXT
                MOVE INPUT-TEXT TO WS-PASSWORD
                PERFORM AUTH-USER
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

*>
EDIT-PROFILE.
    MOVE "--- Create/Edit Profile ---" TO SAVE-TEXT
    PERFORM SHOW

    *> First Name (required)
    MOVE "N" TO FIELD-OK
    PERFORM UNTIL FIELD-OK = "Y"
        MOVE "Enter First Name:" TO SAVE-TEXT
        PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
        IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-TEXT)) > 0
            MOVE FUNCTION TRIM(INPUT-TEXT) TO P-FIRST-NAME
            MOVE "Y" TO FIELD-OK
        ELSE
            MOVE "This field is required. Please enter a non-blank value." TO SAVE-TEXT
            PERFORM SHOW
        END-IF
    END-PERFORM

    *> Last Name (required)
    MOVE "N" TO FIELD-OK
    PERFORM UNTIL FIELD-OK = "Y"
        MOVE "Enter Last Name:" TO SAVE-TEXT
        PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
        IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-TEXT)) > 0
            MOVE FUNCTION TRIM(INPUT-TEXT) TO P-LAST-NAME
            MOVE "Y" TO FIELD-OK
        ELSE
            MOVE "This field is required. Please enter a non-blank value." TO SAVE-TEXT
            PERFORM SHOW
        END-IF
    END-PERFORM

    *> University (required)
    MOVE "N" TO FIELD-OK
    PERFORM UNTIL FIELD-OK = "Y"
        MOVE "Enter University/College Attended:" TO SAVE-TEXT
        PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
        IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-TEXT)) > 0
            MOVE FUNCTION TRIM(INPUT-TEXT) TO P-UNIVERSITY
            MOVE "Y" TO FIELD-OK
        ELSE
            MOVE "This field is required. Please enter a non-blank value." TO SAVE-TEXT
            PERFORM SHOW
        END-IF
    END-PERFORM

    *> Major (required)
    MOVE "N" TO FIELD-OK
    PERFORM UNTIL FIELD-OK = "Y"
        MOVE "Enter Major:" TO SAVE-TEXT
        PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
        IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-TEXT)) > 0
            MOVE FUNCTION TRIM(INPUT-TEXT) TO P-MAJOR
            MOVE "Y" TO FIELD-OK
        ELSE
            MOVE "This field is required. Please enter a non-blank value." TO SAVE-TEXT
            PERFORM SHOW
        END-IF
    END-PERFORM

    *> Graduation Year (required with numeric/range check)
    MOVE "N" TO VALID-YEAR
    PERFORM UNTIL VALID-YEAR = "Y"
        MOVE "Enter Graduation Year (YYYY):" TO SAVE-TEXT
        PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
        IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-TEXT)) = 4
           AND FUNCTION NUMVAL(FUNCTION TRIM(INPUT-TEXT)) >= MIN-YEAR
           AND FUNCTION NUMVAL(FUNCTION TRIM(INPUT-TEXT)) <= MAX-YEAR
            MOVE FUNCTION NUMVAL(FUNCTION TRIM(INPUT-TEXT)) TO P-GRAD-YEAR
            MOVE "Y" TO VALID-YEAR
        ELSE
            MOVE "Invalid graduation year. Please enter a 4-digit year between 1980 and 2100." TO SAVE-TEXT
            PERFORM SHOW
        END-IF
    END-PERFORM

    *> Optional About (single line; blank skips)
    MOVE "Enter About Me (optional, max 200 chars, enter blank line to skip):" TO SAVE-TEXT
    PERFORM SHOW
    READ INPUT-FILE INTO INPUT-TEXT
    IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-TEXT)) = 0
        MOVE SPACES TO P-ABOUT
    ELSE
        MOVE FUNCTION TRIM(INPUT-TEXT) TO P-ABOUT
    END-IF

    *> Optional Experience entries (up to 3)
    MOVE 0 TO P-EXP-COUNT
    MOVE 1 TO P-I
    PERFORM UNTIL P-I > 3
        MOVE "Add Experience (optional, max 3 entries. Enter 'DONE' to finish):" TO SAVE-TEXT
        PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
        IF FUNCTION TRIM(INPUT-TEXT) = "DONE"
           OR FUNCTION LENGTH(FUNCTION TRIM(INPUT-TEXT)) = 0
            EXIT PERFORM
        END-IF

        *> Title (required for an entry)
        MOVE "N" TO FIELD-OK
        PERFORM UNTIL FIELD-OK = "Y"
            MOVE P-I TO WS-IDX-TXT
            MOVE SPACES TO SAVE-TEXT
            STRING "Experience #" DELIMITED BY SIZE
                   FUNCTION TRIM(WS-IDX-TXT) DELIMITED BY SIZE
                   " - Title:" DELIMITED BY SIZE
                   INTO SAVE-TEXT
            END-STRING
            PERFORM SHOW
            READ INPUT-FILE INTO INPUT-TEXT
            IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-TEXT)) > 0
                MOVE FUNCTION TRIM(INPUT-TEXT) TO P-EXP-TITLE(P-I)
                MOVE "Y" TO FIELD-OK
            ELSE
                MOVE "This field is required. Please enter a non-blank value." TO SAVE-TEXT
                PERFORM SHOW
            END-IF
        END-PERFORM

        *> Company (required)
        MOVE "N" TO FIELD-OK
        PERFORM UNTIL FIELD-OK = "Y"
            MOVE P-I TO WS-IDX-TXT
            MOVE SPACES TO SAVE-TEXT
            STRING "Experience #" DELIMITED BY SIZE
                   FUNCTION TRIM(WS-IDX-TXT) DELIMITED BY SIZE
                   " - Company/Organization:" DELIMITED BY SIZE
                   INTO SAVE-TEXT
            END-STRING
            PERFORM SHOW
            READ INPUT-FILE INTO INPUT-TEXT
            IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-TEXT)) > 0
                MOVE FUNCTION TRIM(INPUT-TEXT) TO P-EXP-COMPANY(P-I)
                MOVE "Y" TO FIELD-OK
            ELSE
                MOVE "This field is required. Please enter a non-blank value." TO SAVE-TEXT
                PERFORM SHOW
            END-IF
        END-PERFORM

        *> Dates (required)
        MOVE P-I TO WS-IDX-TXT
        MOVE SPACES TO SAVE-TEXT
        STRING "Experience #" DELIMITED BY SIZE
               FUNCTION TRIM(WS-IDX-TXT) DELIMITED BY SIZE
               " - Dates (e.g., Summer 2024):" DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
        IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-TEXT)) > 0
            MOVE FUNCTION TRIM(INPUT-TEXT) TO P-EXP-DATES(P-I)
        ELSE
            MOVE SPACES TO P-EXP-DATES(P-I)
        END-IF

        *> Description (optional)
        MOVE P-I TO WS-IDX-TXT
        MOVE SPACES TO SAVE-TEXT
        STRING "Experience #" DELIMITED BY SIZE
               FUNCTION TRIM(WS-IDX-TXT) DELIMITED BY SIZE
               " - Description (optional, max 100 chars, blank to skip):" DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
        IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-TEXT)) > 0
            MOVE FUNCTION TRIM(INPUT-TEXT) TO P-EXP-DESC(P-I)
        ELSE
            MOVE SPACES TO P-EXP-DESC(P-I)
        END-IF

        ADD 1 TO P-EXP-COUNT
        ADD 1 TO P-I
    END-PERFORM

    *> Optional Education entries (up to 3)
    MOVE 0 TO P-EDU-COUNT
    MOVE 1 TO P-I
    PERFORM UNTIL P-I > 3
        MOVE "Add Education (optional, max 3 entries. Enter 'DONE' to finish):" TO SAVE-TEXT
        PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
        IF FUNCTION TRIM(INPUT-TEXT) = "DONE"
           OR FUNCTION LENGTH(FUNCTION TRIM(INPUT-TEXT)) = 0
            EXIT PERFORM
        END-IF

        *> Degree (required for an entry)
        MOVE "N" TO FIELD-OK
        PERFORM UNTIL FIELD-OK = "Y"
            MOVE P-I TO WS-IDX-TXT
            MOVE SPACES TO SAVE-TEXT
            STRING "Education #" DELIMITED BY SIZE
                   FUNCTION TRIM(WS-IDX-TXT) DELIMITED BY SIZE
                   " - Degree:" DELIMITED BY SIZE
                   INTO SAVE-TEXT
            END-STRING
            PERFORM SHOW
            READ INPUT-FILE INTO INPUT-TEXT
            IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-TEXT)) > 0
                MOVE FUNCTION TRIM(INPUT-TEXT) TO P-EDU-DEGREE(P-I)
                MOVE "Y" TO FIELD-OK
            ELSE
                MOVE "This field is required. Please enter a non-blank value." TO SAVE-TEXT
                PERFORM SHOW
            END-IF
        END-PERFORM

        *> University/College (required)
        MOVE "N" TO FIELD-OK
        PERFORM UNTIL FIELD-OK = "Y"
            MOVE P-I TO WS-IDX-TXT
            MOVE SPACES TO SAVE-TEXT
            STRING "Education #" DELIMITED BY SIZE
                   FUNCTION TRIM(WS-IDX-TXT) DELIMITED BY SIZE
                   " - University/College:" DELIMITED BY SIZE
                   INTO SAVE-TEXT
            END-STRING
            PERFORM SHOW
            READ INPUT-FILE INTO INPUT-TEXT
            IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-TEXT)) > 0
                MOVE FUNCTION TRIM(INPUT-TEXT) TO P-EDU-SCHOOL(P-I)
                MOVE "Y" TO FIELD-OK
            ELSE
                MOVE "This field is required. Please enter a non-blank value." TO SAVE-TEXT
                PERFORM SHOW
            END-IF
        END-PERFORM

        *> Years Attended (required)
        MOVE P-I TO WS-IDX-TXT
        MOVE SPACES TO SAVE-TEXT
        STRING "Education #" DELIMITED BY SIZE
               FUNCTION TRIM(WS-IDX-TXT) DELIMITED BY SIZE
               " - Years Attended (e.g., 2023-2025):" DELIMITED BY SIZE
               INTO SAVE-TEXT
        END-STRING
        PERFORM SHOW
        READ INPUT-FILE INTO INPUT-TEXT
        IF FUNCTION LENGTH(FUNCTION TRIM(INPUT-TEXT)) > 0
            MOVE FUNCTION TRIM(INPUT-TEXT) TO P-EDU-YEARS(P-I)
        ELSE
            MOVE SPACES TO P-EDU-YEARS(P-I)
        END-IF

        ADD 1 TO P-EDU-COUNT
        ADD 1 TO P-I
    END-PERFORM

    *> Save to disk (FC4-71) and confirm (FC4-72)
    MOVE WS-NAME TO P-USERNAME
    PERFORM SAVE-PROFILE

    MOVE "Profile saved successfully!" TO SAVE-TEXT
    PERFORM SHOW

    EXIT PARAGRAPH.



*> To test, userinfo.txt must be empty
NAV-MENU.
    PERFORM UNTIL CHOICE = 9 OR INPUTSEOF = "Y"
        MOVE "1. Create/Edit My Profile" TO SAVE-TEXT
        PERFORM SHOW
        MOVE "2. View My Profile" TO SAVE-TEXT
        PERFORM SHOW
        MOVE "3. Search for User" TO SAVE-TEXT
        PERFORM SHOW
        MOVE "4. Learn a New Skill" TO SAVE-TEXT
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
    PERFORM         EDIT-PROFILE
                WHEN 2
                    PERFORM VIEW-PROFILE
                WHEN 3
                    MOVE "Search for User is under construction." TO SAVE-TEXT
                    PERFORM SHOW
                WHEN 4
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

    MOVE SPACES TO TEMP-LINE
    MOVE P-GRAD-YEAR TO WS-GRAD-YEAR-DISPLAY
    STRING "GRAD: "  WS-GRAD-YEAR-DISPLAY
           INTO TEMP-LINE END-STRING
    WRITE TEMP-LINE

    MOVE SPACES TO TEMP-LINE
    STRING "ABOUT: " P-ABOUT      INTO TEMP-LINE END-STRING
    WRITE TEMP-LINE

    *> Experience section
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

    *> Education section
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

    MOVE "END" TO TEMP-LINE
    WRITE TEMP-LINE.


SAVE-PROFILE.
    MOVE "N" TO PROFILE-FOUND

    *> Step 1: Make sure profiles.txt exists
    OPEN INPUT PROFILES
    IF PROFILES-FILE-STATUS NOT = "00"
        OPEN OUTPUT PROFILES
        CLOSE PROFILES
        OPEN INPUT PROFILES
    END-IF

    *> Step 2: Open temp file (this will truncate it)
    OPEN OUTPUT TEMP-FILE

    *> Step 3: Copy over all profiles, skipping/replacing current user
    PERFORM UNTIL PROFILES-FILE-STATUS = "10"
        READ PROFILES INTO PROFILES-LINE
            AT END EXIT PERFORM
        END-READ

        IF PROFILES-LINE(1:6) = "USER: "
            MOVE PROFILES-LINE(7:) TO WS-BUF
            IF FUNCTION TRIM(WS-BUF) = FUNCTION TRIM(P-USERNAME)
                *> Skip old profile block
                PERFORM UNTIL PROFILES-LINE = "END"
                    READ PROFILES INTO PROFILES-LINE
                        AT END EXIT PERFORM
                    END-READ
                END-PERFORM
                *> Write the new block instead
                PERFORM WRITE-PROFILE-BLOCK
                MOVE "Y" TO PROFILE-FOUND
            ELSE
                *> Copy this other user’s block
                MOVE PROFILES-LINE TO TEMP-LINE
                WRITE TEMP-LINE
                PERFORM UNTIL PROFILES-LINE = "END"
                    READ PROFILES INTO PROFILES-LINE
                        AT END EXIT PERFORM
                    END-READ
                    MOVE PROFILES-LINE TO TEMP-LINE
                    WRITE TEMP-LINE
                END-PERFORM
            END-IF
        ELSE
            MOVE PROFILES-LINE TO TEMP-LINE
            WRITE TEMP-LINE
        END-IF
    END-PERFORM

    CLOSE PROFILES

    *> Step 4: If no profile existed, add a new one
    IF PROFILE-FOUND NOT = "Y"
        PERFORM WRITE-PROFILE-BLOCK
    END-IF

    CLOSE TEMP-FILE

    *> Step 5: Atomic replace via profiles.new then rename
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

    *> Attempt to rename profiles.new -> profiles.txt
    *> Prefer system mv for atomic replace behavior
    CALL "SYSTEM" USING BY CONTENT "mv -f src/profiles.new src/profiles.txt".


VIEW-PROFILE.
    MOVE "--- Your Profile ---" TO SAVE-TEXT
    PERFORM SHOW

    MOVE 'N' TO PROFILE-FOUND
    OPEN INPUT PROFILES
    MOVE SPACES TO PROFILES-LINE

    PERFORM UNTIL 1 = 2
        READ PROFILES INTO PROFILES-LINE
            AT END EXIT PERFORM
        END-READ

        IF PROFILES-LINE(1:6) = "USER: "
            MOVE PROFILES-LINE(7:) TO WS-BUF
            IF FUNCTION TRIM(WS-BUF) = FUNCTION TRIM(WS-NAME)
                MOVE 'Y' TO PROFILE-FOUND

                *> Reset in-memory record
                MOVE SPACES TO P-FIRST-NAME P-LAST-NAME P-UNIVERSITY P-MAJOR P-ABOUT
                MOVE 0 TO P-GRAD-YEAR P-EXP-COUNT P-EDU-COUNT CUR-EXP-IDX CUR-EDU-IDX
                MOVE SPACE TO WS-SECTION

                *> Read block and parse until END
                PERFORM UNTIL PROFILES-LINE = "END"
                    READ PROFILES INTO PROFILES-LINE
                        AT END EXIT PERFORM
                    END-READ

                    IF PROFILES-LINE = "END"
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
                        *> Section-specific entries
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

                *> Now format and display the friendly view
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

                MOVE SPACES TO SAVE-TEXT
                STRING "About Me: " DELIMITED BY SIZE
                       FUNCTION TRIM(P-ABOUT) DELIMITED BY SIZE
                       INTO SAVE-TEXT
                END-STRING
                PERFORM SHOW

                IF P-EXP-COUNT > 0
                    MOVE "Experience:" TO SAVE-TEXT
                    PERFORM SHOW
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
                    MOVE "Education:" TO SAVE-TEXT
                    PERFORM SHOW
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

    IF PROFILE-FOUND NOT = 'Y'
        MOVE "No profile found." TO SAVE-TEXT
        PERFORM SHOW
    END-IF.


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
