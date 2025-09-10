IDENTIFICATION DIVISION.
PROGRAM-ID. InCollege.
*>AUTHOR. Washington.
*>DATE-WRITTEN. 09/06/2025.
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

DATA DIVISION.
FILE SECTION.
*> File description for user account data
FD USERINFO.
       01 USER-REC.
*> Username field - 20 characters maximum
           05 IN-USERNAME PIC X(20).
*> Password field - 20 characters maximum
           05 IN-PASSWORD PIC X(20).

*> File description for input commands
FD INPUT-FILE.
       01 INPUT-REC.
*> Input text field - 24 characters maximum for commands
           05 INPUT-TEXT PIC X(24).

*> File description for application log output
FD APPLOG.
       01 SAVE-RECORD.
*> Output text field - 64 characters maximum for log messages
           05 SAVE-TEXT PIC X(64).

WORKING-STORAGE SECTION.
*> File status indicators for error handling
       01 UINFO-FILE-STATUS PIC XX.
       01 INPUT-FILE-STATUS PIC XX.
       01 APPLOG-FILE-STATUS PIC XX.
*> End-of-file flags
       01 INFOEOF PIC A(1) VALUE 'N'.    *> End of user info file
       01 INPUTSEOF PIC A(1) VALUE 'N'.  *> End of input file

*> Action tracking and command constants
       01 CURRENT-ACTION PIC X(20).
       01 WS-LOGIN PIC X(5) VALUE 'LOGIN'.
       01 WS-NEW   PIC X(18) VALUE 'CREATE NEW ACCOUNT'.
*> User authentication variables
       01 WS-NAME PIC X(20).             *> Current username
       01 WS-PASSWORD PIC X(20).         *> Current password
       01 WS-LOGGEDIN PIC A(1) VALUE 'N'.   *> Y = logged in, N = not
*> Password validation flags
       01 WS-HASCAPITAL PIC A(1) VALUE 'N'.   *> Has uppercase letter
       01 WS-HASDIGIT   PIC A(1) VALUE 'N'.   *> Has digit
       01 WS-HASSPECIAL PIC A(1) VALUE 'N'.   *> Has special character
*> Password validation counters and limits
       01 WS-CHARCOUNT  PIC 9(2) VALUE 0.     *> Character count in password
       01 WS-MINPASSWORDCOUNT PIC 9(2) VALUE 8.  *> Minimum password length
       01 WS-MAXPASSWORDCOUNT PIC 9(2) VALUE 12. *> Maximum password length
       01 WS-INSPECTEDCHAR PIC X(1).          *> Current character being inspected
*> Account management variables
       01 WS-NUMACCOUNTS PIC 9(1) VALUE 0.    *> Number of existing accounts
       01 WS-NEWUSERNAME PIC X(20).           *> New username for account creation
       01 WS-UNIQUEUSERSTATUS PIC A(1) VALUE 'N'. *> Username uniqueness check
       01 I PIC 9(2) VALUE 1.  *>Iterator I variable for loops

*> Menu choice variables
       77 CHOICE       PIC 9 VALUE 0.         *> Main menu choice
       77 SKILLCHOICE  PIC 9 VALUE 0.         *> Skill menu choice

PROCEDURE DIVISION.
*> Main program execution starts here

*> Open input file for reading commands and output file for logging
       OPEN INPUT INPUT-FILE
       OPEN OUTPUT APPLOG

*> Display welcome message and main menu options
       MOVE "Welcome to InCollege!" TO SAVE-TEXT
       PERFORM SHOW
       MOVE "Log In" TO SAVE-TEXT
       PERFORM SHOW
       MOVE "Create New Account" TO SAVE-TEXT
       PERFORM SHOW

       *> Count existing accounts to enforce 5-account limit
       OPEN INPUT USERINFO
           PERFORM UNTIL INFOEOF='Y'
               READ USERINFO INTO USER-REC
                   AT END MOVE 'Y' TO INFOEOF
                   NOT AT END
                       ADD 1 TO WS-NUMACCOUNTS
               END-READ
           END-PERFORM
       CLOSE USERINFO

*> Process input commands from the test file
       MOVE 'N' TO INFOEOF.
       PERFORM UNTIL INPUTSEOF='Y'
           READ INPUT-FILE INTO INPUT-TEXT
               AT END MOVE 'Y' TO INPUTSEOF
                   NOT AT END
                     PERFORM PARSEINPUT
            END-READ
       END-PERFORM

*> Clean up and exit
       CLOSE INPUT-FILE
       CLOSE APPLOG
       STOP RUN.

SHOW.
*> Display text to console and write to log file
       DISPLAY SAVE-TEXT
       WRITE SAVE-RECORD.
CHECKPASSWORD.
*> Password validation routine - checks for length and character requirements
*> Reset all password validation flags
           MOVE 'N' TO WS-HASDIGIT
           MOVE 'N' TO WS-HASCAPITAL
           MOVE 'N' TO WS-HASSPECIAL

*> Count characters in the password (trimmed of spaces)
           INSPECT FUNCTION TRIM(IN-PASSWORD)
              TALLYING WS-CHARCOUNT FOR ALL CHARACTERS

*> Check if password length is within acceptable range (8-12 characters)
           IF WS-CHARCOUNT >= WS-MINPASSWORDCOUNT
              AND WS-CHARCOUNT <= WS-MAXPASSWORDCOUNT
              THEN
*> Loop through each character in the password to check requirements
                  PERFORM VARYING I FROM 1 BY 1
                      UNTIL I > LENGTH OF FUNCTION TRIM(IN-PASSWORD)
                      MOVE FUNCTION TRIM(IN-PASSWORD)(I:1)
                          TO WS-INSPECTEDCHAR
*> Check for uppercase letter (A-Z)
                      IF WS-INSPECTEDCHAR >= 'A'
                         AND WS-INSPECTEDCHAR <= 'Z'
                          MOVE 'Y' TO WS-HASCAPITAL
*> Check for digit (0-9)
                      ELSE IF WS-INSPECTEDCHAR >= '0'
                              AND WS-INSPECTEDCHAR <= '9'
                          MOVE 'Y' TO WS-HASDIGIT
*> Check for special character (! to /)
                      ELSE IF WS-INSPECTEDCHAR >= '!'
                              AND WS-INSPECTEDCHAR <= '/'
                          MOVE 'Y' TO WS-HASSPECIAL
                      END-IF
                  END-PERFORM

*> Verify all password requirements are met
                  IF WS-HASCAPITAL = 'Y'
                     AND WS-HASDIGIT = 'Y'
                     AND WS-HASSPECIAL = 'Y'
*> Password is valid - create account and log in user
                      MOVE 'Y' TO WS-LOGGEDIN
                      MOVE "Account created successfully." TO SAVE-TEXT
                      PERFORM SHOW
                      STRING "Welcome, " DELIMITED BY SIZE
                             IN-USERNAME DELIMITED BY SIZE
                             INTO SAVE-TEXT
                      PERFORM SHOW
*> Save new account to user info file
                      OPEN EXTEND USERINFO
                      WRITE USER-REC
                      CLOSE USERINFO
*> Navigate to main menu
                      PERFORM NAV-MENU
                  ELSE
*> Password missing required character types
                      MOVE "Password requirements not met!" TO SAVE-TEXT
                      PERFORM SHOW
                  END-IF
           ELSE
*> Password length is invalid
               MOVE "Password requirements not met!" TO SAVE-TEXT
               PERFORM SHOW
           END-IF.

AUTH-USER.
*> User authentication routine - validates login credentials
*>DISPLAY WS-NAME
*>DISPLAY WS-PASSWORD
*> Check if username matches
       IF IN-USERNAME = WS-NAME
*> Check if password matches
           IF IN-PASSWORD = WS-PASSWORD
*> Login successful
                   MOVE 'Y' TO WS-LOGGEDIN
                   MOVE "You have successfully logged in." TO SAVE-TEXT
                   PERFORM SHOW
                   MOVE SPACES TO SAVE-TEXT
                   STRING "Welcome, " DELIMITED BY SIZE
                          WS-NAME DELIMITED BY SIZE
                          INTO SAVE-TEXT
                   PERFORM SHOW
               ELSE
*> Wrong password
                   MOVE "Wrong credentials. Try again." TO SAVE-TEXT
                   PERFORM SHOW
           END-IF
       ELSE
*> Wrong username - terminate program
           MOVE "Wrong credentials. Try again." TO SAVE-TEXT
           PERFORM SHOW
           CLOSE INPUT-FILE
           CLOSE APPLOG
           STOP RUN
       END-IF.
PARSEINPUT.
*> Input parsing routine - processes commands from input file
*>DISPLAY INPUT-TEXT
*> Check if command is LOGIN
       IF INPUT-TEXT = WS-LOGIN
              IF WS-LOGGEDIN = 'Y' THEN
*> User already logged in
                  MOVE "You are already logged in." TO SAVE-TEXT
                  PERFORM SHOW
              ELSE
*> Process login - read username and password from input
                  PERFORM UNTIL INFOEOF='Y'
                      MOVE "Please enter your username:" TO SAVE-TEXT
                      PERFORM SHOW
                      MOVE "Please enter your password:" TO SAVE-TEXT
                      PERFORM SHOW
                      READ INPUT-FILE INTO INPUT-TEXT
                      MOVE INPUT-TEXT TO WS-NAME
                      READ INPUT-FILE INTO INPUT-TEXT
                      MOVE INPUT-TEXT TO WS-PASSWORD
*> Check credentials against user info file
                      OPEN INPUT USERINFO
                         READ USERINFO INTO USER-REC
                            AT END MOVE 'Y' TO INFOEOF
                              NOT AT END
                                  PERFORM AUTH-USER
                         END-READ
                      CLOSE USERINFO
                      IF WS-LOGGEDIN = 'Y' THEN
                          PERFORM NAV-MENU
                      END-IF
                  END-PERFORM
              END-IF
*> Check if command is CREATE NEW ACCOUNT
       ELSE IF INPUT-TEXT = WS-NEW
               IF WS-LOGGEDIN = 'Y' THEN
*> User already logged in
                   MOVE "You are already logged in." TO SAVE-TEXT
                   PERFORM SHOW
               ELSE IF WS-NUMACCOUNTS < 5
*> Account limit not reached - proceed with account creation
                   READ INPUT-FILE INTO INPUT-TEXT
                   MOVE INPUT-TEXT TO WS-NEWUSERNAME
                   READ INPUT-FILE INTO INPUT-TEXT
                   MOVE INPUT-TEXT TO IN-PASSWORD
*> Check if username already exists
                   OPEN INPUT USERINFO
                   MOVE 'N' TO INFOEOF
                   MOVE 'Y' TO WS-UNIQUEUSERSTATUS
                   PERFORM UNTIL INFOEOF='Y'
                       READ USERINFO INTO USER-REC
                          AT END MOVE 'Y' TO INFOEOF
                          NOT AT END
                               IF WS-NEWUSERNAME = IN-USERNAME THEN
                                   MOVE "Username already exists, please try again." TO SAVE-TEXT
                                   PERFORM SHOW
                                   MOVE 'Y' TO INFOEOF
                                   MOVE 'N' TO WS-UNIQUEUSERSTATUS
                               END-IF
                       END-READ
                   END-PERFORM
                   CLOSE USERINFO

*> If username is unique, validate password and create account
                   IF WS-UNIQUEUSERSTATUS = 'Y' THEN
                       MOVE WS-NEWUSERNAME TO IN-USERNAME
                       PERFORM CHECKPASSWORD
                   END-IF
               ELSE
*> Account limit reached (5 accounts maximum)
                   MOVE "All permitted accounts have been created, please come back later" TO SAVE-TEXT
                   PERFORM SHOW
               END-IF
       ELSE
*> Invalid command
           MOVE "Invalid action, please try again." TO SAVE-TEXT
           PERFORM SHOW
       END-IF.
NAV-MENU.
*> Main navigation menu - displayed after successful login
       PERFORM UNTIL CHOICE = 9 OR INPUTSEOF = "Y"

*> Display main menu options
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

*> Read user's menu choice from input file
               READ INPUT-FILE INTO INPUT-TEXT
                   AT END MOVE "Y" TO INPUTSEOF
               END-READ

*> Process menu choice if not end of file
               IF INPUTSEOF NOT = "Y"
                   MOVE FUNCTION NUMVAL(INPUT-TEXT) TO CHOICE
                   EVALUATE CHOICE
                       WHEN 1
*> Job search feature (not implemented)
                           MOVE "Job search/internship is under construction." TO SAVE-TEXT
                           PERFORM SHOW
                       WHEN 2
*> Find someone feature (not implemented)
                           MOVE "Find someone you know is under construction." TO SAVE-TEXT
                           PERFORM SHOW
                       WHEN 3
*> Navigate to skill learning menu
                           PERFORM SKILL-MENU
                       WHEN 9
*> Exit program
                           MOVE "Exiting navigation..." TO SAVE-TEXT
                           PERFORM SHOW
                           CLOSE INPUT-FILE
                           CLOSE APPLOG
                           STOP RUN
                       WHEN OTHER
*> Invalid menu choice
                           MOVE "Invalid choice, please try again." TO SAVE-TEXT
                           PERFORM SHOW
                   END-EVALUATE
               END-IF

           END-PERFORM.
SKILL-MENU.
*> Skill learning submenu - allows users to select skills to learn
           MOVE 0 TO SKILLCHOICE
           PERFORM UNTIL SKILLCHOICE = 9 OR INPUTSEOF = "Y"
*> Display skill menu header
               MOVE "Learn a New Skill:" TO SAVE-TEXT
               PERFORM SHOW

*> Display available skills (all currently under construction)
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

*> Read user's skill choice from input file
               READ INPUT-FILE INTO INPUT-TEXT
                   AT END MOVE "Y" TO INPUTSEOF
               END-READ

*> Process skill choice if not end of file
               IF INPUTSEOF NOT = "Y"
                   MOVE FUNCTION NUMVAL(INPUT-TEXT) TO SKILLCHOICE
                   EVALUATE SKILLCHOICE
                       WHEN 1
*> Skill 1 (not implemented)
                           MOVE "This skill is under construction" TO SAVE-TEXT
                           PERFORM SHOW
                       WHEN 2
*> Skill 2 (not implemented)
                           MOVE "This skill is under construction" TO SAVE-TEXT
                           PERFORM SHOW
                       WHEN 3
*> Skill 3 (not implemented)
                           MOVE "This skill is under construction" TO SAVE-TEXT
                           PERFORM SHOW
                       WHEN 4
*> Skill 4 (not implemented)
                           MOVE "This skill is under construction" TO SAVE-TEXT
                           PERFORM SHOW
                       WHEN 5
*> Skill 5 (not implemented)
                           MOVE "This skill is under construction" TO SAVE-TEXT
                           PERFORM SHOW
                       WHEN 9
*> Return to main menu
                           CONTINUE
                       WHEN OTHER
*> Invalid skill choice
                           MOVE "Invalid choice, please try again." TO SAVE-TEXT
                           PERFORM SHOW
                   END-EVALUATE
               END-IF
           END-PERFORM.
