bank-account-processing-system/
│
├── Day1/
│   ├── Project_Summary

│   ├── Flowchart
│   └── DB2_Tables.sql
│
├── Day2/
│   ├── AccountCreation.cob
   ENVIRONMENT DIVISION.
   INPUT-OUTPUT SECTION.
   FILE-CONTROL.
       SELECT ACCOUNT-FILE ASSIGN TO "ACCOUNT.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.

   DATA DIVISION.
   FILE SECTION.
   FD ACCOUNT-FILE.
   01 ACCOUNT-REC.
       05 ACC-NO     PIC X(10).
       05 NAME       PIC X(30).
       05 BALANCE    PIC 9(8)V99.
       05 ACC-TYPE   PIC X(1).

   WORKING-STORAGE SECTION.
   01 WS-ACC-NO     PIC X(10).
   01 WS-NAME       PIC X(30).
   01 WS-BALANCE    PIC 9(8)V99.
   01 WS-ACC-TYPE   PIC X(1).

   PROCEDURE DIVISION.
       OPEN OUTPUT ACCOUNT-FILE.

       DISPLAY "Enter Account Number: ".
       ACCEPT WS-ACC-NO.
       DISPLAY "Enter Name: ".
       ACCEPT WS-NAME.
       DISPLAY "Enter Initial Balance: ".
       ACCEPT WS-BALANCE.
       DISPLAY "Enter Account Type (S/C): ".
       ACCEPT WS-ACC-TYPE.

       MOVE WS-ACC-NO TO ACC-NO.
       MOVE WS-NAME TO NAME.
       MOVE WS-BALANCE TO BALANCE.
       MOVE WS-ACC-TYPE TO ACC-TYPE.

       WRITE ACCOUNT-REC.

       DISPLAY "Account Created Successfully!".

       CLOSE ACCOUNT-FILE.
       STOP RUN.
              
│   ├── Deposit.cob
            ENVIRONMENT DIVISION.
   INPUT-OUTPUT SECTION.
   FILE-CONTROL.
       SELECT OLD-FILE ASSIGN TO "ACCOUNT.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.
       SELECT NEW-FILE ASSIGN TO "TEMP.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.

   DATA DIVISION.
   FILE SECTION.
   FD OLD-FILE.
   01 OLD-REC.
       05 O-ACC-NO   PIC X(10).
       05 O-NAME     PIC X(30).
       05 O-BALANCE  PIC 9(8)V99.
       05 O-ACC-TYPE PIC X(1).

   FD NEW-FILE.
   01 NEW-REC.
       05 N-ACC-NO   PIC X(10).
       05 N-NAME     PIC X(30).
       05 N-BALANCE  PIC 9(8)V99.
       05 N-ACC-TYPE PIC X(1).

   WORKING-STORAGE SECTION.
   01 WS-ACC-NO     PIC X(10).
   01 WS-AMOUNT     PIC 9(8)V99.
   01 FOUND-FLAG    PIC X VALUE "N".
   01 EOF-FLAG      PIC X VALUE "N".   *> NEW: flag for end-of-file

   PROCEDURE DIVISION.
       DISPLAY "Enter Account Number: ".
       ACCEPT WS-ACC-NO.
       DISPLAY "Enter Deposit Amount: ".
       ACCEPT WS-AMOUNT.

       OPEN INPUT OLD-FILE.
       OPEN OUTPUT NEW-FILE.

       PERFORM UNTIL EOF-FLAG = "Y"
           READ OLD-FILE
               AT END MOVE "Y" TO EOF-FLAG
               NOT AT END
                   IF O-ACC-NO = WS-ACC-NO
                       ADD WS-AMOUNT TO O-BALANCE
                       MOVE "Y" TO FOUND-FLAG
                       DISPLAY "Deposit Successful!"
                       DISPLAY "New Balance: " O-BALANCE
                   END-IF
                   MOVE O-ACC-NO   TO N-ACC-NO
                   MOVE O-NAME     TO N-NAME
                   MOVE O-BALANCE  TO N-BALANCE
                   MOVE O-ACC-TYPE TO N-ACC-TYPE
                   WRITE NEW-REC
           END-READ
       END-PERFORM.

       CLOSE OLD-FILE.
       CLOSE NEW-FILE.

       IF FOUND-FLAG = "N"
           DISPLAY "Account Not Found."
       END-IF.

       STOP RUN.
│   ├── Withdrawal.cob
            ENVIRONMENT DIVISION.
   INPUT-OUTPUT SECTION.
   FILE-CONTROL.
       SELECT OLD-FILE ASSIGN TO "ACCOUNT.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.
       SELECT NEW-FILE ASSIGN TO "TEMP.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.

   DATA DIVISION.
   FILE SECTION.
   FD OLD-FILE.
   01 OLD-REC.
       05 O-ACC-NO   PIC X(10).
       05 O-NAME     PIC X(30).
       05 O-BALANCE  PIC 9(8)V99.
       05 O-ACC-TYPE PIC X(1).

   FD NEW-FILE.
   01 NEW-REC.
       05 N-ACC-NO   PIC X(10).
       05 N-NAME     PIC X(30).
       05 N-BALANCE  PIC 9(8)V99.
       05 N-ACC-TYPE PIC X(1).

   WORKING-STORAGE SECTION.
   01 WS-ACC-NO     PIC X(10).
   01 WS-AMOUNT     PIC 9(8)V99.
   01 FOUND-FLAG    PIC X VALUE "N".
   01 EOF-FLAG      PIC X VALUE "N".

   PROCEDURE DIVISION.
       DISPLAY "Enter Account Number: ".
       ACCEPT WS-ACC-NO.
       DISPLAY "Enter Withdrawal Amount: ".
       ACCEPT WS-AMOUNT.

       OPEN INPUT OLD-FILE.
       OPEN OUTPUT NEW-FILE.

       PERFORM UNTIL EOF-FLAG = "Y"
           READ OLD-FILE
               AT END MOVE "Y" TO EOF-FLAG
               NOT AT END
                   IF O-ACC-NO = WS-ACC-NO
                       IF O-BALANCE >= WS-AMOUNT
                           SUBTRACT WS-AMOUNT FROM O-BALANCE
                           MOVE "Y" TO FOUND-FLAG
                           DISPLAY "Withdrawal Successful!"
                           DISPLAY "New Balance: " O-BALANCE
                       ELSE
                           DISPLAY "Insufficient Balance!"
                           MOVE "Y" TO FOUND-FLAG
                       END-IF
                   END-IF

                   MOVE O-ACC-NO   TO N-ACC-NO
                   MOVE O-NAME     TO N-NAME
                   MOVE O-BALANCE  TO N-BALANCE
                   MOVE O-ACC-TYPE TO N-ACC-TYPE
                   WRITE NEW-REC
           END-READ
       END-PERFORM.

       CLOSE OLD-FILE.
       CLOSE NEW-FILE.

       IF FOUND-FLAG = "N"
           DISPLAY "Account Not Found."
       END-IF.

       STOP RUN.
│   └── BalanceInquiry.cob
             ENVIRONMENT DIVISION.
   INPUT-OUTPUT SECTION.
   FILE-CONTROL.
       SELECT ACCOUNT-FILE ASSIGN TO "ACCOUNT.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.

   DATA DIVISION.
   FILE SECTION.
   FD ACCOUNT-FILE.
   01 ACCOUNT-REC.
       05 ACC-NO     PIC X(10).
       05 NAME       PIC X(30).
       05 BALANCE    PIC 9(8)V99.
       05 ACC-TYPE   PIC X(1).

   WORKING-STORAGE SECTION.
   01 WS-ACC-NO     PIC X(10).
   01 FOUND-FLAG    PIC X VALUE "N".
   01 EOF-FLAG      PIC X VALUE "N".

   PROCEDURE DIVISION.
       DISPLAY "Enter Account Number: ".
       ACCEPT WS-ACC-NO.

       OPEN INPUT ACCOUNT-FILE.

       PERFORM UNTIL EOF-FLAG = "Y"
           READ ACCOUNT-FILE
               AT END MOVE "Y" TO EOF-FLAG
               NOT AT END
                   IF ACC-NO = WS-ACC-NO
                       DISPLAY "Account Found!"
                       DISPLAY "Name: " NAME
                       DISPLAY "Balance: " BALANCE
                       DISPLAY "Account Type: " ACC-TYPE
                       MOVE "Y" TO FOUND-FLAG
                   END-IF
           END-READ
       END-PERFORM.

       CLOSE ACCOUNT-FILE.

       IF FOUND-FLAG = "N"
           DISPLAY "Account Not Found."
       END-IF.

       STOP RUN.
│
└── README.md
