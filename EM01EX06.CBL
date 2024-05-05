       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EM01EX06.
       AUTHOR.          MARCELO MIYAZAKI.
       INSTALLATION.    FATEC-SP.
       DATE-WRITTEN.    22/03/2024.
       DATE-COMPILED.
       SECURITY.  APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS. LE ARQUIVOS DE ENTRADA CADENT(MATRICULA, NOME E
      *         SALARIO BRUTO) E GRAVA NO ARQUIVO DE SAIDA 
      *         CADSAI (MATRICULA, NOME E SALARIO BRUTO) SOMENTE OS 
      *         REGISTRO QUE TIVEREM SALARIO MAIOR QUE 3000.
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. MARCELO-PC.
       OBJECT-COMPUTER. MARCELO-PC.
       SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT     CADENT  ASSIGN  TO      DISK
           ORGANIZATION IS    LINE    SEQUENTIAL.
           SELECT     CADSAI  ASSIGN  TO      DISK
           ORGANIZATION IS    LINE    SEQUENTIAL.

       DATA     DIVISION.

       FILE SECTION.
       FD CADENT
           LABEL        RECORD  ARE     STANDARD
           VALUE        OF FILE-ID      IS      "CADENT.DAT".

       01 REG-ENT.
           02 MATRICULA-ENT 	PIC 9(05).
           02 NOME-ENT  	PIC X(30).
           02 SALARIO-ENT	PIC 9(05)V99.

       FD CADSAI
           LABEL        RECORD ARE      STANDARD
           VALUE        OF FILE-ID      IS      "CADSAI.DAT".

       01 REG-SAI.
           02 MATRICULA-SAI   	  PIC 9(05).
           02 NOME-SAI   	  PIC X(30).
           02 SALARIO-SAI         PIC 9(05)V99. 

       WORKING-STORAGE SECTION.

       77 FIM-ARQ        PIC X(03) 	VALUE "NAO".

       PROCEDURE         DIVISION.

       PERFORM INICIO.
       PERFORM PRINCIPAL
               UNTIL FIM-ARQ EQUAL "SIM".
       PERFORM TERMINO.
       STOP RUN.

       INICIO.
            OPEN INPUT  CADENT
                 OUTPUT CADSAI.
            PERFORM LEITURA.

       LEITURA.
            READ CADENT
                AT END
                MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
         PERFORM SELECAO.
         PERFORM LEITURA.

       SELECAO.
          IF SALARIO-ENT > 3000
             PERFORM GRAVACAO.

       GRAVACAO.
           MOVE MATRICULA-ENT TO MATRICULA-SAI.
           MOVE NOME-ENT TO NOME-SAI.
   	   MOVE SALARIO-ENT TO SALARIO-SAI.
           WRITE REG-SAI.

       TERMINO.
           CLOSE CADENT
                 CADSAI.
