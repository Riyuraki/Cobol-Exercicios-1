       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EM01EX02.
       AUTHOR.          MARCELO MIYAZAKI.
       INSTALLATION.    FATEC-SP.
       DATE-WRITTEN.    21/03/2024.
       DATE-COMPILED.
       SECURITY.  APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS. LE O ARQUIVO DE ENTRADA CADALU(NUMERO, NOME,
      *         NOTA1, NOTA2 E NOTA3) CALCULA A MEDIA E
      *         GRAVA NO ARQUIVO DE SAIDA CADATU(NUMERO E MEDIA).
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. MARCELO-PC.
       OBJECT-COMPUTER. MARCELO-PC.
       SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT     CADALU  ASSIGN  TO      DISK
           ORGANIZATION IS    LINE    SEQUENTIAL.
           SELECT     CADATU  ASSIGN  TO      DISK
           ORGANIZATION IS    LINE    SEQUENTIAL.

       DATA     DIVISION.

       FILE SECTION.
       FD CADALU
           LABEL        RECORD  ARE     STANDARD
           VALUE        OF FILE-ID      IS      "CADALU.DAT".

       01 REG-ENT.
           02 NUMERO-ENT	PIC 9(05).
           02 NOME		PIC X(20).
           02 NOTA1             PIC 9(02)V99.
           02 NOTA2             PIC 9(02)V99.
           02 NOTA3             PIC 9(02)V99.

       FD CADATU
           LABEL        RECORD ARE      STANDARD
           VALUE        OF FILE-ID      IS      "CADATU.DAT".

       01 REG-SAI.
           02 NUMERO-SAI 	PIC 9(05).
           02 MEDIA		PIC 9(02)V99.

       WORKING-STORAGE SECTION.

       77 FIM-ARQ        PIC X(03) 	VALUE "NAO".
       77 SOMA-NOTA	 PIC 9(02)V99	VALUE ZEROS.
       77 WMEDIA         PIC 9(02)V99   VALUE ZEROS.	

       PROCEDURE         DIVISION.

       PERFORM INICIO.
       PERFORM PRINCIPAL
               UNTIL FIM-ARQ EQUAL "SIM".
       PERFORM TERMINO.
       STOP RUN.

       INICIO.
            OPEN INPUT  CADALU
                 OUTPUT CADATU.
            PERFORM LEITURA.

       LEITURA.
            READ CADALU
                AT END
                MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
	 PERFORM CALCULO.
         PERFORM GRAVACAO.
         PERFORM LEITURA.

       CALCULO.
          ADD NOTA1
              NOTA2
              NOTA3
		 GIVING  SOMA-NOTA. 
          
          DIVIDE SOMA-NOTA BY 3
                 GIVING WMEDIA.
 
       GRAVACAO.
           MOVE NUMERO-ENT TO NUMERO-SAI.
           MOVE WMEDIA TO MEDIA.
           WRITE REG-SAI.

       TERMINO.
           CLOSE CADALU
                 CADATU.

      


