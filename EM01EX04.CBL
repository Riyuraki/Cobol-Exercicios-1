       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EM01EX04.
       AUTHOR.          MARCELO MIYAZAKI.
       INSTALLATION.    FATEC-SP.
       DATE-WRITTEN.    21/03/2024.
       DATE-COMPILED.
       SECURITY.  APENAS O AUTOR PODE MODIFICA-LO.
      *REMARKS. LE ARQUIVOS DE ENTRADA CADCLI1(CODIGO, NOME E
      *         SEXO) E GRAVA NO ARQUIVO DE SAIDA CADCLI2 (CODIGO E
      *         NOME) SOMENTE OS REGISTRO DO SEXO MASCULINO.
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER. MARCELO-PC.
       OBJECT-COMPUTER. MARCELO-PC.
       SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT     CADCLI1  ASSIGN  TO      DISK
           ORGANIZATION IS    LINE    SEQUENTIAL.
           SELECT     CADCLI2  ASSIGN  TO      DISK
           ORGANIZATION IS    LINE    SEQUENTIAL.

       DATA     DIVISION.

       FILE SECTION.
       FD CADCLI1
           LABEL        RECORD  ARE     STANDARD
           VALUE        OF FILE-ID      IS      "CADCLI1.DAT".

       01 REG-ENT.
           02 COD-ENT 	PIC 9(05).
           02 NOME-ENT  PIC X(20).
           02 SEXO	PIC X(01).

       FD CADCLI2
           LABEL        RECORD ARE      STANDARD
           VALUE        OF FILE-ID      IS      "CADCLI2.DAT".

       01 REG-SAI.
           02 COD-SAI   PIC 9(05).
           02 NOME-SAI  PIC X(20).

       WORKING-STORAGE SECTION.

       77 FIM-ARQ        PIC X(03) VALUE "NAO".

       PROCEDURE         DIVISION.

       PERFORM INICIO.
       PERFORM PRINCIPAL
               UNTIL FIM-ARQ EQUAL "SIM".
       PERFORM TERMINO.
       STOP RUN.

       INICIO.
            OPEN INPUT  CADCLI1
                 OUTPUT CADCLI2.
            PERFORM LEITURA.

       LEITURA.
            READ CADCLI1
                AT END
                MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
         PERFORM MACHISMO.
         PERFORM LEITURA.

       MACHISMO.
          IF SEXO EQUAL "M" 
                    OR	"m"
             PERFORM GRAVACAO.

       GRAVACAO.
           MOVE COD-ENT TO COD-SAI.
           MOVE NOME-ENT TO NOME-SAI.
           WRITE REG-SAI.

       TERMINO.
           CLOSE CADCLI1
                 CADCLI2.

      


