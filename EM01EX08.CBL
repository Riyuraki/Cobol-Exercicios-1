       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EM01EX08.
       AUTHOR.          MARCELO MIYAZAKI.
       INSTALLATION.    FATEC-SP.
       DATE-WRITTEN.    22/03/2024.
       DATE-COMPILED.
       SECURITY.  APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS. LE ARQUIVOS DE ENTRADA CADFUN(CODIGO, NOME E
      *         SALARIO BRUTO) E GRAVA NO ARQUIVO DE SAIDA
      *         CADSAI (CODIGO, NOME E SALARIO REAJUSTADO).
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. MARCELO-PC.
       OBJECT-COMPUTER. MARCELO-PC.
       SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT     CADFUN  ASSIGN  TO      DISK
           ORGANIZATION IS    LINE    SEQUENTIAL.
           SELECT     CADSAI  ASSIGN  TO      DISK
           ORGANIZATION IS    LINE    SEQUENTIAL.

       DATA     DIVISION.

       FILE SECTION.
       FD CADFUN
           LABEL        RECORD  ARE     STANDARD
           VALUE        OF FILE-ID      IS      "CADFUN.DAT".

       01 REG-ENT.
           02 COD-ENT 	    	PIC 9(05).
           02 NOME-ENT  	PIC X(20).
           02 SALBRT 		PIC 9(05)V99.

       FD CADSAI
           LABEL        RECORD ARE      STANDARD
           VALUE        OF FILE-ID      IS      "CADSAI.DAT".

       01 REG-SAI.
           02 COD-SAI 	    	PIC 9(05).
           02 NOME-SAI  	PIC X(20).
           02 SALAJT 		PIC 9(05)V99.	

       WORKING-STORAGE SECTION.

       77 FIM-ARQ        PIC X(03) 	VALUE "NAO".
       77 AJUSTE	 PIC 9(05)V99   VALUE ZEROS.

       PROCEDURE         DIVISION.

       PERFORM INICIO.
       PERFORM PRINCIPAL
               UNTIL FIM-ARQ EQUAL "SIM".
       PERFORM TERMINO.
       STOP RUN.

       INICIO.
            OPEN INPUT  CADFUN
                 OUTPUT CADSAI.
            PERFORM LEITURA.

       LEITURA.
            READ CADFUN
                AT END
                MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
         PERFORM CALCREAJ.
         PERFORM LEITURA.

       CALCREAJ.

         IF SALBRT NOT> 1000
            MULTIPLY SALBRT BY 0,12 GIVING AJUSTE
            ADD AJUSTE TO SALBRT
	 ELSE
	 	IF SALBRT > 1000 AND NOT> 2000
            		MULTIPLY SALBRT BY 0,11 GIVING AJUSTE
            		ADD AJUSTE TO SALBRT
	    	ELSE
            		MULTIPLY SALBRT BY 0,1 GIVING AJUSTE
            		ADD AJUSTE TO SALBRT.        
         PERFORM GRAVACAO.

       GRAVACAO.
           MOVE COD-ENT TO COD-SAI.
           MOVE NOME-ENT TO NOME-SAI.
   	   MOVE SALBRT TO SALAJT.
           WRITE REG-SAI.

       TERMINO.
           CLOSE CADFUN
                 CADSAI.

