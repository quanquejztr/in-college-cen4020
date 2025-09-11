InCollege Week 2 — Profile Management Testing

Build
- GnuCOBOL: cobc -x -free -o InCollege src/InCollege.cob

FILES FROM EPIC1
./src/InCollege.exe: executable to run (run from repo root).
src/InCollege-Test.txt: input script the app reads.
src/InCollege-Output.txt: app log output. What the user sees

NEW FILES
src/userinfo.txt: credential store. Read to count existing accounts and to authenticate; appended to on successful “Create New Account"
src/profiles.txt: profiles database. Each profile is a block of tagged lines (USER, FN, LN, UNIV, MAJOR, GRAD, ABOUT, END). Created if missing at startup; read to view profile; updated via a safe replace flow (src/InCollege.cob:31, 168–175, 633, 646, 655, 672).
src/profiles.tmp: temp file used during “Save Profile” to build the next full contents (truncated each save) (src/InCollege.cob:36, 564).
src/profiles.new: staging file used for atomic replace; after writing, it is moved over profiles.txt with mv -f (src/InCollege.cob:42, 615–626, 630).
How they interact

INPUT FLOW
Input flows from src/InCollege-Test.txt → actions (create user, login, edit/view profile).
Output flows to src/InCollege-Output.txt via SHOW for every prompt/message.
Creating an account appends a fixed-width record to src/userinfo.txt; later logins read from it to authenticate.
Creating/editing a profile writes a new full file image into src/profiles.tmp, copies it to src/profiles.new, then renames to src/profiles.txt for an atomic update (src/InCollege.cob:556–631).
Viewing a profile scans src/profiles.txt for USER: <username> and prints that block until END (src/InCollege.cob:633, 646, 655).
