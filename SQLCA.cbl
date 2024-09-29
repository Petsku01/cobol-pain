       IDENTIFICATION DIVISION.
       PROGRAM-ID. CybersecurityManagement.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT UserFile ASSIGN TO "users.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT IncidentFile ASSIGN TO "incidents.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  UserFile.
       01  UserRecord.
           05  Username           PIC A(30).
           05  Password           PIC A(30).
           05  FailedAttempts     PIC 9(2) VALUE 0.
           05  IsLocked           PIC X VALUE 'N'.

       FD  IncidentFile.
       01  IncidentRecord.
           05  IncidentDate       PIC 9(8).
           05  IncidentType       PIC X(20).
           05  IncidentDetails    PIC X(100).

       WORKING-STORAGE SECTION.
       01  InputUsername        PIC A(30).
       01  InputPassword        PIC A(30).
       01  UserFound            PIC X VALUE 'N'.
       01  MaxAttempts          PIC 9(2) VALUE 3.
       01  ContinueFlag         PIC X VALUE 'Y'.
       01  CurrentDate          PIC 9(8) VALUE "20230929".  *> Placeholder for date.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN I-O UserFile
           OPEN OUTPUT IncidentFile
           PERFORM UNTIL ContinueFlag = 'N'
               DISPLAY "Cybersecurity Management System"
               DISPLAY "==============================="
               DISPLAY "1. User Login"
               DISPLAY "2. Exit"
               DISPLAY "==============================="
               DISPLAY "Please choose an option (1-2): "
               ACCEPT InputUsername

               EVALUATE InputUsername
                   WHEN '1'
                       PERFORM USER-LOGIN
                   WHEN '2'
                       MOVE 'N' TO ContinueFlag
                   WHEN OTHER
                       DISPLAY "Invalid choice. Please try again."
               END-EVALUATE
           END-PERFORM.

           CLOSE UserFile
           CLOSE IncidentFile
           DISPLAY "Thank you for using the cybersecurity management system!"
           STOP RUN.

       USER-LOGIN.
           DISPLAY "Enter Username: "
           ACCEPT InputUsername
           DISPLAY "Enter Password: "
           ACCEPT InputPassword
           READ UserFile INTO UserRecord
               AT END
                   DISPLAY "User not found."
                   EXIT PERFORM
               NOT AT END
                   PERFORM CHECK-LOGIN.

       CHECK-LOGIN.
           IF IsLocked = 'Y' THEN
               DISPLAY "Account is locked due to multiple failed login attempts."
               EXIT PERFORM
           END-IF.

           IF InputPassword = Password THEN
               DISPLAY "Login successful!"
               MOVE 0 TO FailedAttempts
           ELSE
               ADD 1 TO FailedAttempts
               DISPLAY "Login failed. Attempt: ", FailedAttempts
               IF FailedAttempts >= MaxAttempts THEN
                   MOVE 'Y' TO IsLocked
                   CALL "LogIncident" USING CurrentDate "Account Locked" "Account locked due to multiple failed attempts."
                   DISPLAY "Account has been locked due to multiple failed login attempts."
               END-IF.
           END-IF.

       LogIncident.
           LINKAGE SECTION.
           01  IncidentDate      PIC 9(8).
           01  IncidentType      PIC X(20).
           01  IncidentDetails   PIC X(100).
           WRITE IncidentRecord
               FROM IncidentDate
           WITH NO ADVANCING.
           MOVE IncidentType TO IncidentRecord(6:20)
           MOVE IncidentDetails TO IncidentRecord(26:100)
           DISPLAY "Incident logged: ", IncidentDetails.
           EXIT PROGRAM.

       END PROGRAM CybersecurityManagement.
