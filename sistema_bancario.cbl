       IDENTIFICATION DIVISION.
        PROGRAM-ID. Sistema-Bancario.
        AUTHOR Carlos García.
        DATE-WRITTEN APR.04.2025
        
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
      
      * Archivos físicos    
      
            SELECT OPTIONAL AccountFile 
            ASSIGN TO "cuentas.dat"
    		    ORGANIZATION IS SEQUENTIAL.
	        SELECT OPTIONAL TransactionFile 
	        ASSIGN TO "transacciones.dat"
		        ORGANIZATION IS SEQUENTIAL.
		        
	    DATA DIVISION.
	    FILE SECTION.
	  
	    FD AccountFile.
        01 AccountRecord.
        	05 Account-ID PIC 9(5).
        	05 Account-Holder PIC X(30).
        	05 Account-Balance PIC 9(7)V99.
	  

        FD TransactionFile.
        01 TransactionRecord.
        	05 Trans-Account-ID PIC 9(5).
        	05 Trans-Type PIC X(1).
        		88 Deposit VALUE 'D'.
        		88 Withdraw	VALUE 'W'.
        	05 Trans-Amount PIC 9(7)V99.
        	
        WORKING-STORAGE SECTION.
        01 Prompt-Account-ID PIC X(30) 
        VALUE "Introduce el ID de la cuenta".
        01 Prompt-Holder-Name PIC X(33) 
        VALUE "Introduce el nombre del titular".
        01 Prompt-Amount PIC X(20) 
        VALUE "Introduce el monto".
        01 Invalid-Amount PIC X(20) 
        VALUE "Cantidad no válida".
        01 Insufficient-Funds PIC X(51) 
        VALUE "Fondos insuficientes para realizar la transacción".
        
        01 User-Option PIC X.
        01 Found-Account PIC X VALUE 'N'.
        01 Account-Search-ID PIC 9(5).
        01 Transaction-Amount PIC 9(7)V99.
        PROCEDURE DIVISION.
        
        Main-Logic.
        	PERFORM Display-Menu
        	PERFORM UNTIL User-Option = '5'
        		PERFORM Process-Option
        		PERFORM Display-Menu
        	END-PERFORM.
        	STOP RUN.
	
        Display-Menu.
        	DISPLAY "<----- SISTEMA BANCARIO ------>".
        	DISPLAY "1. Crear cuenta".
        	DISPLAY "2. Depositar dinero".
        	DISPLAY "3. Retirar dinero".
        	DISPLAY "4. Consultar saldo".
        	DISPLAY "5. SALIR".
        	DISPLAY "Seleccione una opción: ".
        	ACCEPT User-Option.
        	
        Process-Option.
        	EVALUATE User-Option
        		WHEN '1'
        			PERFORM Create-Account
        		WHEN '2'
        			PERFORM Deposit-Money
        		WHEN '3'
        			PERFORM Withdraw-Money
        		WHEN '4'
        			PERFORM Check-Balance
        		WHEN OTHER
        			DISPLAY 
        			"Opción no válida, Intennte de nuevo."
        	END-EVALUATE.
        	
        Create-Account.
        	OPEN EXTEND AccountFile.
        	DISPLAY Prompt-Account-ID.
        	ACCEPT Account-ID.
        	DISPLAY Prompt-Holder-Name.
        	ACCEPT Account-Holder.
        	MOVE 0 TO Account-Balance.
        	WRITE AccountRecord.
        	DISPLAY "Cuenta creada exitosamente".
        	CLOSE AccountFile.
        	
        Deposit-Money.
            OPEN I-O AccountFile.
            PERFORM Find-Account.
            IF Found-Account = 'Y'
                DISPLAY Prompt-Amount
                ACCEPT Transaction-Amount
                IF Transaction-Amount > 0
                    ADD Transaction-Amount TO Account-Balance
                    REWRITE AccountRecord
                    MOVE 'D' TO Trans-Type
                    PERFORM Record-Transaction
                    DISPLAY "Depósito exitoso."
                ELSE
                    DISPLAY Invalid-Amount
                END-IF
            ELSE
                DISPLAY "Cuenta no encontrada"
            END-IF.
            CLOSE AccountFile.

            
        Withdraw-Money.
            OPEN I-O AccountFile.
            PERFORM Find-Account.
            IF Found-Account = 'Y'
                DISPLAY Prompt-Amount
                ACCEPT Transaction-Amount
                IF Transaction-Amount > 0
                AND Transaction-Amount <= Account-Balance
                    SUBTRACT Transaction-Amount FROM Account-Balance
                    MOVE 'W' TO Trans-Type
                    PERFORM Record-Transaction
                    REWRITE AccountRecord
                    DISPLAY "Retiro Exitoso"
                ELSE IF Transaction-Amount > Account-Balance
                    DISPLAY Insufficient-Funds
                ELSE
                    DISPLAY Invalid-Amount
                END-IF
            ELSE
                DISPLAY "Cuenta no encontrada"
            END-IF.
            CLOSE AccountFile.
        
        Check-Balance.
            OPEN I-O AccountFile.
            PERFORM Find-Account.
            IF Found-Account = 'Y'
                DISPLAY "Saldo actual de la cuenta: ", Account-Balance
            ELSE
                DISPLAY "Cuenta no encontrada"
            END-IF.
            CLOSE AccountFile.
        
        Find-Account.
            MOVE 'N' TO Found-Account.
            DISPLAY Prompt-Account-ID.
            ACCEPT Account-Search-ID.
            PERFORM UNTIL Found-Account = 'Y'
                READ AccountFile
                    AT END
                        DISPLAY "Cuenta no encontrada."
                        EXIT PERFORM
                    NOT AT END
                        IF Account-ID = Account-Search-ID
                            MOVE 'Y' TO Found-Account
                        END-IF
                END-READ
            END-PERFORM.
        
        Record-Transaction.
            OPEN EXTEND TransactionFile.
            MOVE Account-ID TO Trans-Account-ID.
            MOVE Transaction-Amount TO Trans-Amount.
            WRITE TransactionRecord.
            CLOSE TransactionFile.
            
                
