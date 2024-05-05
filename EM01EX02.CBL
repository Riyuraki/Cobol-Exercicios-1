       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EM01EX02.
       AUTHOR.          MARCELO MIYAZAKI.
       INSTALLATION.    FATEC-SP.
       DATE-WRITTEN.    21/03/20204.
       DATE-COMPILED.
       SECURITY.  APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS. LE OS REGISTROS DO ARQUIVO DE ENTRADA
      *         CADALU (NUMERO, NOME, SEXO, DATA NASCIMENTO)
      *         E GRAVA NO ARQUIVO DE SAIDA CADATU(NUMERO, NOME
      *         E DATA NASCIMENTO).
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
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
           02 NUMERO-ENT            PIC 9(05).
           02 NOME-ENT              PIC X(20).
           02 SEXO-ENT              PIC X(01).
           02 DATANASCIMENTO-ENT.
             03 DD-ENT              PIC 9(02).
             03 MM-ENT              PIC 9(02).
             03 AAAA-ENT            PIC 9(04).

       FD CADATU
           LABEL        RECORD ARE      STANDARD
           VALUE        OF FILE-ID      IS      "CADATU.DAT".

       01 REG-SAI.
           02 NUMERO-SAI            PIC 9(05).
           02 NOME-SAI              PIC X(20).
           02 DATANASCIMENTO-SAI.
             03 DD-SAI              PIC 9(02).
             03 MM-SAI              PIC 9(02).
             03 AAAA-SAI            PIC 9(04).    

       WORKING-STORAGE SECTION.

       77 FIM-ARQ        PIC X(03) VALUE "NAO".

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
         PERFORM GRAVACAO.
         PERFORM LEITURA.

       GRAVACAO.
           MOVE NUMERO-ENT TO NUMERO-SAI.
           MOVE NOME-ENT TO NOME-SAI.
           MOVE DD-ENT TO DD-SAI.
           MOVE MM-ENT TO MM-SAI.
           MOVE AAAA-ENT TO AAAA-SAI.
           WRITE REG-SAI.

       TERMINO.
           CLOSE CADALU
                 CADATU.
