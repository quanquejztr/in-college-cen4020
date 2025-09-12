InCollege Week 2 — Profile Management Testing

Build
- GnuCOBOL: cobc -x -free -o InCollege src/InCollege.cob
------------------------------------------------
FILES FROM EPIC1
./src/InCollege.exe: executable to run (run from repo root).
src/InCollege-Test.txt: input script the app reads.
src/InCollege-Output.txt: app log output. What the user sees
src/userinfo.txt: credential store. Read to count existing accounts and to authenticate; appended to on successful “Create New Account"
------------------------------------------------
NEW FILES
src/profiles.txt: profiles database. Each profile is a block of tagged lines (USER, FN, LN, UNIV, MAJOR, GRAD, ABOUT, END). Created if missing at startup; read to view profile.
src/profiles.tmp: temp file used during “Save Profile” to build the next full contents (truncated each save) .
src/profiles.new: staging file used for atomic replace; after writing, it is moved over profiles.txt with mv -f
How they interact
------------------------------------------------
INPUT FLOW
Input flows from src/InCollege-Test.txt → actions (create user, login, edit/view profile).
Output flows to src/InCollege-Output.txt via SHOW for every prompt/message.
Creating an account appends a fixed-width record to src/userinfo.txt. later logins read from it to authenticate.
Creating/editing a profile... (1) writes a new full file image into src/profiles.tmp for building, (2) copies it to src/profiles.new for staging, then renames to src/profiles.txt for viewing
Viewing a profile scans src/profiles.txt for USER: <username> and prints that block until END (src/InCollege.cob:633, 646, 655).
------------------------------------------------
TESTING
FC4-95 – Profile linked to correct username
- Verify: Each user views only their own data; `src/profiles.txt` contains distinct `USER:` blocks per user.

FC4-97 – Saved profile across multiple runs
- Verify: No data loss, no duplicate blocks; latest edits persist after multiple runs.

FC4-98 – Prompts display while reading from file
- Verify: Prompts (e.g., `Enter Major:`) appear on screen and in `src/InCollege-Output.txt` even though input comes from `src/InCollege-Test.txt`.

FC4-99 – Sample input files (positive/negative/edge)
    - Positive Test Cases: Scenarios for successful profile creation/editing with all
        required fields, and viewing a complete profile. Include scenarios with optional
        fields both present and omitted.
    - Negative Test Cases: Scenarios for invalid input (e.g., non-numeric graduation year, invalid year range, blank required fields).
    - Edge Cases: Test boundaries, like exactly 3 experience/education entries, or omitting all optional fields.

FC4-100 – Output file matches console
- Verify: `src/InCollege-Output.txt` matches console output line-for-line.
- Helper: `bin/record_run.sh <scenario>` captures console to `tmp/console.txt` and prints a diff vs file output.

FC4-101 – Golden outputs
- Guidance: Save expected `src/InCollege-Output.txt` as goldens (e.g., `golden/positive.out`) and diff against future runs.

View/Edit consistency checks
- FC4-80: After save, compare `View My Profile` output to the corresponding block in `src/profiles.txt` (labels/order/counts).
- FC4-76: Edit only UserA; confirm only UserA’s block changes in `src/profiles.txt`.

