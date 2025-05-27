       IDENTIFICATION DIVISION.
       PROGRAM-ID. CybersecurityManagement.


      * Program to manage user login and track security incidents.
      * Compatible with COBOL 2014 (ISO/IEC 1989:2014).

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * Define UserFile for storing user credentials and login status.
           SELECT UserFile ASSIGN TO "users.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS UserFile-Status.
      * Define IncidentFile for logging security incidents.
           SELECT IncidentFile ASSIGN TO "incidents.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS IncidentFile-Status.

       DATA DIVISION.
       FILE SECTION.
      * File descriptor for UserFile.
       FD  UserFile.
       01  UserRecord.
           05  Username           PIC A(30).    *> Store username (alphabetic).
           05  Password           PIC A(30).    *> Store password (plain text, not secure).
           05  FailedAttempts     PIC 9(2) VALUE 0.  *> Track failed login attempts.
           05  IsLocked           PIC X VALUE 'N'.   *> Indicate if account is locked ('Y'/'N').

      * File descriptor for IncidentFile.
       FD  IncidentFile.
       01  IncidentRecord.
           05  IncidentDate       PIC 9(8).     *> Date of incident (YYYYMMDD).
           05  IncidentType       PIC X(20).    *> Type of incident (e.g., "Account Locked").
           05  IncidentDetails    PIC X(100).   *> Detailed description of incident.

       WORKING-STORAGE SECTION.
      * Variables for user input and program control.
       01  InputUsername        PIC A(30).      *> Store user-entered username.
       01  InputPassword        PIC A(30).      *> Store user-entered password.
       01  MenuOption           PIC X(1).       *> Store menu choice (1 or 2).
       01  UserFound            PIC X VALUE 'N'. *> Flag to indicate if user is found.
       01  MaxAttempts          PIC 9(2) VALUE 3. *> Maximum allowed login attempts.
       01  ContinueFlag         PIC X VALUE 'Y'.  *> Control main loop ('Y' to continue).
       01  CurrentDate          PIC 9(8).       *> Store current date (YYYYMMDD).
       01  UserFile-Status      PIC X(2).       *> Status code for UserFile operations.
       01  IncidentFile-Status  PIC X(2).       *> Status code for IncidentFile operations.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
      * Main program logic to display menu and handle user choices.
           MOVE FUNCTION CURRENT-DATE (1:8) TO CurrentDate  *> Get current date.
           OPEN I-O UserFile                   *> Open UserFile for read/write.
           IF UserFile-Status NOT = "00"
               DISPLAY "Error opening UserFile: " UserFile-Status
               STOP RUN                        *> Terminate on file error.
           END-IF
           OPEN OUTPUT IncidentFile            *> Open IncidentFile for writing.
           IF IncidentFile-Status NOT = "00"
               DISPLAY "Error opening IncidentFile: " IncidentFile-Status
               STOP RUN                        *> Terminate on file error.
           END-IF
      * Main loop to display menu and process user input.
           PERFORM UNTIL ContinueFlag = 'N'
               DISPLAY "Cybersecurity Management System"
               DISPLAY "==============================="
               DISPLAY "1. User Login"
               DISPLAY "2. Exit"
               DISPLAY "==============================="
               DISPLAY "Please choose an option (1-2): "
               ACCEPT MenuOption               *> Get menu choice.

               EVALUATE MenuOption             *> Process menu selection.
                   WHEN '1'
                       PERFORM USER-LOGIN      *> Handle login process.
                   WHEN '2'
                       MOVE 'N' TO ContinueFlag  *> Exit the program.
                   WHEN OTHER
                       DISPLAY "Invalid choice. Please try again."
               END-EVALUATE
           END-PERFORM

      * Close files and check for errors.
           CLOSE UserFile
           IF UserFile-Status NOT = "00"
               DISPLAY "Error closing UserFile: " UserFile-Status
           END-IF
           CLOSE IncidentFile
           IF IncidentFile-Status NOT = "00"
               DISPLAY "Error closing IncidentFile: " IncidentFile-Status
           END-IF
           DISPLAY "Thank you for using the cybersecurity management system!"
           STOP RUN.                       *> End program.

       USER-LOGIN.
      * Handle user login by accepting credentials and searching UserFile.
           DISPLAY "Enter Username: "
           ACCEPT InputUsername                *> Get username input.
           DISPLAY "Enter Password: "
           ACCEPT InputPassword                *> Get password input.
           MOVE 'N' TO UserFound               *> Initialize user found flag.
      * Search UserFile for matching username.
           PERFORM UNTIL UserFound = 'Y' OR AT END
               READ UserFile INTO UserRecord
                   AT END
                       DISPLAY "User not found."
                       EXIT PERFORM           *> Exit if no more records.
                   NOT AT END
                       IF Username = InputUsername
                           MOVE 'Y' TO UserFound  *> Mark user as found.
                           PERFORM CHECK-LOGIN    *> Verify credentials.
                       END-IF
               END-READ
           END-PERFORM.

       CHECK-LOGIN.
      * Verify user credentials and handle login logic.
           IF IsLocked = 'Y' THEN
               DISPLAY "Account is locked due to multiple failed login attempts."
               EXIT PERFORM                   *> Exit if account is locked.
           END-IF

           IF InputPassword = Password THEN
               DISPLAY "Login successful!"
               MOVE 0 TO FailedAttempts       *> Reset failed attempts.
               MOVE 'N' TO IsLocked           *> Unlock account if previously locked.
               REWRITE UserRecord             *> Update UserFile.
               IF UserFile-Status NOT = "00"
                   DISPLAY "Error updating UserFile: " UserFile-Status
               END-IF
           ELSE
               ADD 1 TO FailedAttempts        *> Increment failed attempts.
               DISPLAY "Login failed. Attempt: ", FailedAttempts
               IF FailedAttempts >= MaxAttempts THEN
                   MOVE 'Y' TO IsLocked       *> Lock account after max attempts.
                   CALL "LogIncident" USING CurrentDate
                                          "Account Locked"
                                          "Account locked due to multiple failed attempts."
                   DISPLAY "Account has been locked due to multiple failed login attempts."
               END-IF
               REWRITE UserRecord             *> Update UserFile.
               IF UserFile-Status NOT = "00"
                   DISPLAY "Error updating UserFile: " UserFile-Status
               END-IF
           END-IF.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. LogIncident.
      * Subprogram to log security incidents to IncidentFile.
      * Compatible with COBOL 2014.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IncidentFile ASSIGN TO "incidents.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS IncidentFile-Status.

       DATA DIVISION.
       FILE SECTION.
       FD  IncidentFile.
       01  IncidentRecord.
           05  IncidentDate       PIC 9(8).     *> Date of incident.
           05  IncidentType       PIC X(20).    *> Type of incident.
           05  IncidentDetails    PIC X(100).   *> Incident details.

       WORKING-STORAGE SECTION.
       01  IncidentFile-Status  PIC X(2).       *> Status code for IncidentFile.

       LINKAGE SECTION.
      * Parameters passed from calling program.
       01  LS-IncidentDate      PIC 9(8).
       01  LS-IncidentType      PIC X(20).
       01  LS-IncidentDetails   PIC X(100).

       PROCEDURE DIVISION USING LS-IncidentDate
                               LS-IncidentType
                               LS-IncidentDetails.
      * Log incident to IncidentFile.
           OPEN EXTEND IncidentFile            *> Open file in append mode.
           IF IncidentFile-Status NOT = "00"
               DISPLAY "Error opening IncidentFile: " IncidentFile-Status
               GOBACK                         *> Return on error.
           END-IF
           MOVE LS-IncidentDate TO IncidentDate    *> Populate record fields.
           MOVE LS-IncientType TO IncidentType
           MOVE LS-IncidentDetails TO IncidentDetails
           WRITE IncidentRecord                *> Write incident to file.
           IF IncidentFile-Status NOT = "00"
               DISPLAY "Error writing to IncidentFile: " IncidentFile-Status
           END-IF
           DISPLAY "Incident logged: ", LS-IncidentDetails
           CLOSE IncidentFile                  *> Close file.
           IF IncidentFile-Status NOT = "00"
               DISPLAY "Error closing IncidentFile: " IncidentFile-Status
           END-IF
           GOBACK.                             *> Return to calling program.

       END PROGRAM LogIncident.
       END PROGRAM CybersecurityManagement.
