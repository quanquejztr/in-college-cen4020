# InCollege COBOL Project CEN4020

## Overview
This project implements the **core authentication system** for the InCollege COBOL application.  

### Features
- Create new student accounts (up to 5)  
- Log in with existing accounts
- Validate passwords against security rules (8-12 characters with at least one capital letter, one digit, and one special character) 
- Display navigation options after login  
- Write all output to both the screen and an output file for verification  

---

## How to Compile and Run
From the project root directory, run:

```sh
cobc -x InCollege.cob -o InCollege.exe
```

The program reads user input from a file and produces identical output to both:
- The screen (console)
- InCollege-Output.txt (log file)

Run the program with:

```sh
./bin/InCollege
```

## Testing
Place test inputs into **InCollege-Test.txt**.
Run the program and capture output:
Output Files:
**InCollege-Output.txt** → program’s actual output file produced
**SampleOutput.txt** → given sample file with expected output for comparison
