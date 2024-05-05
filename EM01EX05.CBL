       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EM01EX05.
       AUTHOR.          MARCELO MIYAZAKI.
       INSTALLATION.    FATEC-SP.
       DATE-WRITTEN.    22/03/2024.
       DATE-COMPILED.
       SECURITY.  APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS. LE ARQUIVOS DE ENTRADA CADALU(NUMERO, NOME,
      *         NOTA1, NOTA2 E FALTAS) E GRAVA NO ARQUIVO DE SAIDA 
      *         CADAPR (NUMERO, NOME E MEDIA) SOMENTE OS 
      *         REGISTRO QUE TIVEREM APROVAÇÃO POR FALTA E MEDIA.
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. MARCELO-PC.
       OBJECT-COMPUTER. MARCELO-PC.
       SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT     CADALU  ASSIGN  TO      DISK
           ORGANIZATION IS    LINE    SEQUENTIAL.
           SELECT     CADAPR  ASSIGN  TO      DISK
           ORGANIZATION IS    LINE    SEQUENTIAL.

       DATA     DIVISION.

       FILE SECTION.
       FD CADALU
           LABEL        RECORD  ARE     STANDARD
           VALUE        OF FILE-ID      IS      "CADALU.DAT".

       01 REG-ENT.
           02 NUMERO-ENT 	PIC 9(05).
           02 NOME-ENT  	PIC X(20).
           02 NOTA1		PIC 9(02)V99.
           02 NOTA2		PIC 9(02)V99.
	   02 FALTAS    	PIC 9(02).

       FD CADAPR
           LABEL        RECORD ARE      STANDARD
           VALUE        OF FILE-ID      IS      "CADAPR.DAT".

       01 REG-SAI.
           02 NUMERO-SAI   PIC 9(05).
           02 NOME-SAI     PIC X(20).
           02 MEDIA	   PIC 9(02)V99.	

       WORKING-STORAGE SECTION.

       77 FIM-ARQ        PIC X(03) 	VALUE "NAO".
       77 SOMA-NOTA	 PIC 9(02)V99 	VALUE ZEROS.
       77 WMEDIA	 PIC 9(02)V99	VALUE ZEROS.

       PROCEDURE         DIVISION.

       PERFORM INICIO.
       PERFORM PRINCIPAL
               UNTIL FIM-ARQ EQUAL "SIM".
       PERFORM TERMINO.
       STOP RUN.

       INICIO.
            OPEN INPUT  CADALU
                 OUTPUT CADAPR.
            PERFORM LEITURA.

       LEITURA.
            READ CADALU
                AT END
                MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
         PERFORM CALCMEDIA.
         PERFORM APROVADOS.
         PERFORM LEITURA.

       CALCMEDIA.
          
         ADD NOTA1 NOTA2 GIVING SOMA-NOTA.
         DIVIDE SOMA-NOTA BY 2 GIVING WMEDIA.

       APROVADOS.
          IF WMEDIA NOT< 7 
		AND FALTAS NOT> 18
                    
             PERFORM GRAVACAO.

       GRAVACAO.
           MOVE NUMERO-ENT TO NUMERO-SAI.
           MOVE NOME-ENT TO NOME-SAI.
   	   MOVE WMEDIA TO MEDIA.
           WRITE REG-SAI.

       TERMINO.
           CLOSE CADALU
                 CADAPR.
