IDENTIFICATION DIVISION.
PROGRAM-ID. Interactive-Banking-System.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
	SELECT account-file
	ASSIGN TO "accounts.dat"
	ORGANIZATION IS INDEXED
	ACCESS MODE IS DYNAMIC
	RECORD KEY IS account-number.


DATA DIVISION.
FILE SECTION.
FD account-file.
01 account-record.
	02 account-number 	PIC 9(5) VALUE ZEROS.
	02 account-name		PIC X(20) VALUE SPACES.
	02 account-balance	PIC 9(7)V99 VALUE ZEROS.
	*>02 account-history	PIC x(300).

WORKING-STORAGE SECTION.
01 prompter.
	02 question	PIC X(50) VALUE SPACES.
	02 response PIC X(20) VALUE SPACES.

01 user-status 	PIC 9 VALUE 0.
	88 confirm	VALUE 1.

01 menu-selection PIC 9 VALUE 0.
	88 create-account 	VALUE 1.
	88 deposit			VALUE 2.
	88 withdraw 		VALUE 3.
	88 transfer			VALUE 4.
	88 view-account		VALUE 5.
	88 get-reports		VALUE 6.
	88 quit 			VALUE 7.

01 generic-number.
	02 generic-number-raw 		PIC 9(7)V99.
	02 generic-number-formatted PIC Z(6)9.99.

01 temp-account-number PIC 9(5).

PROCEDURE DIVISION.
Main-Logic.
	OPEN I-O account-file.

	PERFORM Display-Menu UNTIL menu-selection <> 0.

	EVALUATE menu-selection
		WHEN 1
			PERFORM Open-Account
		WHEN 2
			PERFORM Make-Deposit
		WHEN 3
			PERFORM Make-Withdrawl
		WHEN 4
			PERFORM Make-Transfer
		WHEN 5
			PERFORM Sign-In
			PERFORM View-Statement
		WHEN 6
			CONTINUE
		WHEN 7
			SET quit TO TRUE
		WHEN OTHER
		SET menu-selection TO 0
	END-EVALUATE.
	
	CLOSE account-file.
STOP RUN.

ACCOUNT-ACTIONS SECTION.
Open-Account.
	MOVE "Please enter account holder's name:" TO question
	PERFORM Ask.

	MOVE response TO account-name.

	MOVE "Please Enter an initial amount:" TO question
	PERFORM Ask-for-Number.

	DISPLAY "Your name is: " account-name.
	DISPLAY "You want to deposit: " generic-number-formatted.

	MOVE generic-number TO account-balance.

	ACCEPT account-number FROM TIME
	
	WRITE account-record.

	PERFORM View-Statement.

Make-Deposit.
	PERFORM Sign-In.

	MOVE "Deposit how much?" TO question
	PERFORM Ask-for-Number.

	ADD generic-number-raw TO account-balance.

	REWRITE account-record.
	PERFORM View-Statement.

Make-Withdrawl.
	PERFORM Sign-In.

	MOVE "Withdraw how much?" TO question
	PERFORM Ask-for-Number.

	IF generic-number-raw > account-balance
		DISPLAY "Insufficient Funds."
	ELSE
		SUBTRACT generic-number-raw FROM account-balance
		REWRITE account-record
	END-IF
	
	PERFORM View-Statement.

Make-Transfer.
	PERFORM Sign-In.

View-Statement.
	DISPLAY "NAME: " account-name
	DISPLAY "ID: " account-number.
	MOVE account-balance TO generic-number-formatted.
	DISPLAY "BALANCE: $" generic-number-formatted.

Sign-In.
	MOVE "Enter your account number: " TO question
	PERFORM Ask.

	MOVE response to account-number.

	READ account-file 
		KEY IS account-number
	END-READ.

USER-INTERFACE SECTION.
Ask.
	DISPLAY FUNCTION TRIM(question) " " WITH NO ADVANCING
	ACCEPT response.

Ask-for-Number.
	PERFORM Ask.
	PERFORM Accept-number.

Ask-for-Selection.
	PERFORM Ask.
	PERFORM Accept-Selection.

Accept-number.
	MOVE response TO generic-number-raw.
	MOVE response TO generic-number-formatted.

Accept-Selection.
	MOVE response TO menu-selection.

Display-Menu.
	DISPLAY "1. Create New Account"
	DISPLAY "2. Deposit Money"
	DISPLAY "3. Withdraw Money"
	DISPLAY "4. Transfer Money"
	DISPLAY "5. View Account Details"
	DISPLAY "6. Generate Reports"
	DISPLAY "7. Exit".
	MOVE "Please make a selection" TO question
	PERFORM Ask-for-Selection.
