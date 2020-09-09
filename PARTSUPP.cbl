       IDENTIFICATION DIVISION.
       PROGRAM-ID.  PARTSUPP.
       AUTHOR. Dave, Hartanto, Hugh Stone, Maruca, Kun.
       DATE-WRITTEN. 09/06/20.
       DATE-COMPILED. 09/06/20.
       SECURITY. NON-CONFIDENTIAL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARTSUPPFILE
           ASSIGN TO PARTSUPPFILE
             FILE STATUS IS PSCODE.

           SELECT ZIPFILE
           ASSIGN TO ZIPFILE
             FILE STATUS IS ZPCODE.

           SELECT PARTFILE
           ASSIGN TO PARTFILE
             FILE STATUS IS PTCODE.

           SELECT ADDRFILE
           ASSIGN TO ADDRFILE
             FILE STATUS IS ADCODE.

           SELECT SUPPFILE
           ASSIGN TO SUPPLIERS
             FILE STATUS IS SPCODE.

           SELECT ERRFILE
           ASSIGN TO ERRFILE
             FILE STATUS IS EFCODE.

       DATA DIVISION.
       FILE SECTION.
       FD  PARTSUPPFILE
           RECORD CONTAINS 133 CHARACTERS
           DATA RECORD IS PS-REC.
       01  PS-REC  PIC X(133).

       FD  ZIPFILE
           RECORD CONTAINS 133 CHARACTERS
           DATA RECORD IS ZIP-REC.
       01  ZIP-REC  PIC X(133).

       FD  PARTFILE
           RECORD CONTAINS 100 CHARACTERS
           DATA RECORD IS PART-REC.
       01  PART-REC  PIC X(100).

       FD  SUPPFILE
           RECORD CONTAINS 133 CHARACTERS
           DATA RECORD IS SUP-REC.
       01  SUP-REC  PIC X(133).

       FD  ERRFILE
           RECORD CONTAINS 133 CHARACTERS
           DATA RECORD IS ERR-REC.
       01  ERR-REC  PIC X(133).

       FD  ADDRFILE
           RECORD CONTAINS 133 CHARACTERS
           DATA RECORD IS ADDR-REC.
       01  ADDR-REC PIC X(133).

       WORKING-STORAGE SECTION.

       01  FILE-STATUS-CODES.
           05  PSCODE                  PIC X(2).
               88 PS-VALID      VALUE 0.
               88 NOT-VALID     VALUE 8.
           05  PTCODE                  PIC X(2).
               88 PT-VALID      VALUE 0.
               88 NOT-VALID     VALUE 8.
           05  SPCODE                  PIC X(2).
               88 SP-VALID      VALUE 0.
               88 NOT-VALID     VALUE 8.
           05  EFCODE                  PIC X(2).
               88 CODE-WRITE    VALUE SPACES.
           05  ADCODE                  PIC X(2).
               88 AD-VALID      VALUE 0.
               88 NOT-VALID     VALUE 8.
           05  ZPCODE                  PIC X(2).
               88 AD-VALID      VALUE 0.
               88 NOT-VALID     VALUE 8.

       01  SWITCHES-IN-PROGRAM.
           05  SW-MORE-DATA              PIC X VALUE 'N'.
               88  END-OF-DATA                 VALUE 'Y'.
           05  SW-MORE-RECORDS           PIC X VALUE 'N'.
               88  END-OF-RECORD               VALUE 'Y'.
           05  SW-DATA-FOUND          PIC X VALUE 'N'.
               88  DATA-FOUND               VALUE 'Y'.
      ******************************************************************
      **** THIS CODE SNNIPETS CITE FROM
      **** THE COPYBOOK
      ******************************************************************

      * COPYBOOK PARTSUPP
        01  PART-SUPP-ADDR-PO.
           05 PARTS.
               10  PART-NUMBER       PIC X(23) VALUE SPACES.
               10  PART-NAME         PIC X(14) VALUE SPACES.
               10  SPEC-NUMBER       PIC X(07) VALUE SPACES.
               10  GOVT-COMML-CODE   PIC X(01) VALUE SPACES.
               10  BLUEPRINT-NUMBER  PIC X(10) VALUE SPACES.
               10  UNIT-OF-MEASURE   PIC X(03) VALUE SPACES.
               10  WEEKS-LEAD-TIME   PIC 9(03) VALUE ZERO.
               10  VEHICLE-MAKE      PIC X(03) VALUE SPACES.
                    88 CHRYSLER       VALUE 'CHR'.
                    88 FORD           VALUE 'FOR'.
                    88 GM             VALUE 'GM '.
                    88 VOLKSWAGON     VALUE 'VW '.
                    88 TOYOTA         VALUE 'TOY'.
                    88 JAGUAR         VALUE 'JAG'.
                    88 PEUGEOT        VALUE 'PEU'.
                    88 BMW            VALUE 'BMW'.
               10  VEHICLE-MODEL     PIC X(10) VALUE SPACES.
               10  VEHICLE-YEAR      PIC X(04) VALUE '0000'.
               10  FILLER            PIC X(14) VALUE SPACES.
           05 SUPPLIERS.
               10  SUPPLIER-CODE     PIC X(10) VALUE SPACES.
               10  SUPPLIER-TYPE     PIC X(01) VALUE SPACES.
                    88 SUBCONTRACTOR  VALUE 'S'.
                    88 DISTRIBUTOR    VALUE 'D'.
                    88 MANUFACTURER   VALUE 'M'.
                    88 IMPORTER       VALUE 'I'.
               10  SUPPLIER-NAME     PIC X(15) VALUE SPACES.
               10  SUPPLIER-PERF     PIC 9(03) VALUE ZERO.
               10  SUPPLIER-RATING   PIC X(01) VALUE SPACES.
                    88 HIGHEST-QUALITY VALUE '3'.
                    88 AVERAGE-QUALITY VALUE '2'.
                    88 LOWEST-QUALITY  VALUE '1'.
               10  SUPPLIER-STATUS   PIC X(01) VALUE SPACES.
                    88 GOVT-COMM       VALUE '1'.
                    88 GOVT-ONLY       VALUE '2'.
                    88 COMMERCIAL-ONLY VALUE '3'.
               10  SUPPLIER-ACT-DATE PIC 9(08) VALUE ZERO.
           05 SUPP-ADDRESS OCCURS 3 TIMES INDEXED BY ADDR-IDX.
               10 ADDRESS-TYPE      PIC X(01) VALUE SPACES.
                  88 ORDER-ADDRESS           VALUE '1'.
                  88 SCHED-ADDRESS           VALUE '2'.
                  88 REMIT-ADDRESS           VALUE '3'.
               10 ADDRESS-1         PIC X(15) VALUE SPACES.
               10 ADDRESS-2         PIC X(15) VALUE SPACES.
               10 ADDRESS-3         PIC X(15) VALUE SPACES.
               10 CITY              PIC X(15) VALUE SPACES.
               10 ADDR-STATE        PIC X(02) VALUE SPACES.
               10 ZIP-CODE          PIC 9(10) VALUE ZERO.
           05 PURCHASE-ORDER OCCURS 3 TIMES INDEXED BY PO-IDX.
               10  PO-NUMBER         PIC X(06) VALUE SPACES.
               10  BUYER-CODE        PIC X(03) VALUE SPACES.
               10  QUANTITY          PIC S9(7) VALUE ZERO.
               10  UNIT-PRICE        PIC S9(7)V99 VALUE ZERO.
               10  ORDER-DATE        PIC 9(08) VALUE ZERO.
               10  DELIVERY-DATE     PIC 9(08) VALUE ZERO.

      *
       01  WS-ZIP.
           05  WS-STATE          PIC X(14) VALUE SPACES.
           05  FILLER            PIC X(1)  VALUE SPACES.
           05  WS-ST-ABBR        PIC X(2)  VALUE SPACES.
           05  FILLER            PIC X(2)  VALUE SPACES.
           05  WS-ZIP-LOW        PIC 9(5)  VALUE SPACES.
           05  FILLER            PIC X(3)  VALUE SPACES.
           05  WS-ZIP-HIG        PIC 9(5)  VALUE SPACES.

      *
       01  WS-PARTS.
           05  WS-PART-NUMBER       PIC X(23) VALUE SPACES.
           05  WS-PART-NAME         PIC X(14) VALUE SPACES.
           05  WS-SPEC-NUMBER       PIC X(07) VALUE SPACES.
           05  WS-GOVT-COMML-CODE   PIC X(01) VALUE SPACES.
           05  WS-BLUEPRINT-NUMBER  PIC X(05) VALUE SPACES.
           05  WS-UNIT-OF-MEASURE   PIC X(03) VALUE SPACES.
           05  WS-WEEKS-LEAD-TIME   PIC S9(04) COMP VALUE ZEROS.
           05  WS-VEHICLE-MAKE      PIC X(03) VALUE SPACES.
                88 WS-CHRYSLER       VALUE 'CHR'.
                88 WS-FORD           VALUE 'FOR'.
                88 WS-GM             VALUE 'GM '.
                88 WS-VOLKSWAGON     VALUE 'VW '.
                88 WS-TOYOTA         VALUE 'TOY'.
                88 WS-JAGUAR         VALUE 'JAG'.
                88 WS-PEUGEOT        VALUE 'PEU'.
                88 WS-BMW            VALUE 'BMW'.
           05  WS-VEHICLE-MODEL     PIC X(05) VALUE SPACES.
           05  WS-VEHICLE-YEAR      PIC X(04) VALUE '0000'.

      *
       01 WS-SUPPLIERS.
           05  WS-SUPPLIER-CODE     PIC X(05) VALUE SPACES.
           05  WS-SUPPLIER-TYPE     PIC X(01) VALUE SPACES.
                88 WS-SUBCONTRACTOR  VALUE 'S'.
                88 WS-DISTRIBUTOR    VALUE 'D'.
                88 WS-MANUFACTURER   VALUE 'M'.
                88 WS-IMPORTER       VALUE 'I'.
           05  WS-SUPPLIER-NAME     PIC X(15) VALUE SPACES.
           05  WS-SUPPLIER-PERF     PIC 9(03) COMP VALUE ZERO.
           05  WS-SUPPLIER-RATING   PIC X(01) VALUE SPACES.
                88 WS-HIGHEST-QUALITY VALUE '3'.
                88 WS-AVERAGE-QUALITY VALUE '2'.
                88 WS-LOWEST-QUALITY  VALUE '1'.
           05  WS-SUPPLIER-STATUS   PIC X(01) VALUE SPACES.
                88 WS-GOVT-COMM       VALUE '1'.
                88 WS-GOVT-ONLY       VALUE '2'.
                88 WS-COMMERCIAL-ONLY VALUE '3'.
           05  WS-SUPPLIER-ACT-DATE PIC X(08) VALUE SPACES.
      * A valid data format
           05 WS-VALID-DATE REDEFINES WS-SUPPLIER-ACT-DATE.
              10  WS-VALID-MONTH     PIC 9(02).
                  88   WS-MONTH-TOT  PIC 9(02) VALUE 1 THRU 12.
                  88   WS-MONTH-31   PIC 9(02) VALUE 1 3 5 7 8 10 12.
                  88   WS-MONTH-30      PIC 9(02) VALUE 4 6 9 11.
                  88   WS-MONTH-28-29   PIC 9(02) VALUE 2.
              10  WS-VALID-DAY       PIC 9(02).
                  88   WS-DAY-31     PIC 9(02) VALUE 1 THRU 31.
                  88   WS-DAY-30     PIC 9(02) VALUE 1 THRU 30.
                  88   WS-DAY-28     PIC 9(02) VALUE 1 THRU 28.
                  88   WS-DAY-29     PIC 9(02) VALUE 1 THRU 29.
              10  WS-VALID-YEAR      PIC 9(04).
                  88   WS-REG-YEAR   PIC 9(04) VALUE 1900 THRU 2020.

      *
       01  WS-SUPP-ADDRESS.
           05 WS-ADDRESS-TYPE      PIC X(01) VALUE SPACES.
              88 WS-ORDER-ADDRESS           VALUE '1'.
              88 WS-SCHED-ADDRESS           VALUE '2'.
              88 WS-REMIT-ADDRESS           VALUE '3'.
           05 WS-ADDRESS-1         PIC X(15) VALUE SPACES.
           05 WS-ADDRESS-2         PIC X(15) VALUE SPACES.
           05 WS-ADDRESS-3         PIC X(15) VALUE SPACES.
           05 WS-CITY              PIC X(15) VALUE SPACES.
           05 WS-ADDR-STATE        PIC X(02) VALUE SPACES.
           05 WS-ZIP-CODE          PIC X(05) VALUE SPACES.

      *
       01  WS-PURCHASE-ORDERS.
           05  WS-PO-NUMBER           PIC X(06) VALUE SPACES.
           05  WS-BUYER-CODE          PIC X(03) VALUE SPACES.
           05  WS-QUANTITY            PIC S9(8) COMP VALUE ZERO.
           05  WS-UNIT-PRICE          PIC S9(7)V99 COMP-3 VALUE ZERO.
           05  WS-ORDER-DATE          PIC X(08) VALUE SPACES.
           05  WS-DELIVERY-DATE       PIC X(08) VALUE SPACES.


       PROCEDURE DIVISION.
       000-PARTMAIN.
           PERFORM 100-INITIALIZATION.
           PERFORM 200-PROCESS-DATA THRU 710-EXIT
                    UNTIL SW-MORE-RECORDS.
           GOBACK.

       100-INITIALIZATION.
      *   Open files
           OPEN INPUT PARTSUPPFILE.
           OPEN INPUT ZIPFILE.
           OPEN OUTPUT PARTFILE.
           OPEN OUTPUT SUPPFILE.
           OPEN OUTPUT ADDRFILE.
           OPEN OUTPUT ERRFILE.

           INITIALIZE  PS-REC,
                       PART-REC.
           PERFORM 150-READ-A-RECORD.

       150-READ-A-RECORD.
           READ PARTSUPPFILE
               AT END
               MOVE 'Y' TO SW-MORE-RECORDS
           END-READ.
       200-PROCESS-DATA.
           MOVE PS-REC TO PART-SUPP-ADDR-PO.
           PERFORM 400-SUPPEDIT.
           MOVE ADDR-REC TO WS-SUPP-ADDRESS.
           PERFORM 510-READ-A-ZIP.
           PERFORM 500-ADDREDIT THRU 520-EXIT
                UNTIL SW-MORE-RECORDS.
           PERFORM 600-POEDIT.
      * Required fields
           MOVE SUPPLIER-CODE TO WS-SUPPLIER-CODE.
           MOVE SUPPLIER-NAME TO WS-SUPPLIER-NAME.
           MOVE SUPPLIER-PERF TO WS-SUPPLIER-PERF.
           PERFORM 150-READ-A-RECORD.

       300-PARTEDIT.

       400-SUPPEDIT.
           MOVE SUP-REC TO WS-SUPPLIERS.
      * Evaluate SUPPLIER-TYPE
           EVALUATE TRUE
                WHEN SUBCONTRACTOR
                    IF HIGHEST-QUALITY
                        MOVE SUPPLIER-TYPE TO WS-SUPPLIER-TYPE
                    ELSE
                        GO TO 710-EXIT
                    END-IF
                WHEN DISTRIBUTOR
                WHEN MANUFACTURER
                WHEN IMPORTER
                    MOVE SUPPLIER-TYPE TO WS-SUPPLIER-TYPE
                WHEN OTHER
                    GO TO 710-EXIT
           END-EVALUATE.
      * Evaluate SUPPLIER-RATING
           EVALUATE TRUE
                WHEN HIGHEST-QUALITY
                WHEN AVERAGE-QUALITY
                WHEN LOWEST-QUALITY
                    MOVE SUPPLIER-RATING  TO WS-SUPPLIER-RATING
                WHEN OTHER
                    GO TO 710-EXIT
           END-EVALUATE.
      * Evaluate SUPPLIER-STATUS
           EVALUATE TRUE
                WHEN GOVT-COMM
                WHEN GOVT-ONLY
                WHEN COMMERCIAL-ONLY
                    MOVE SUPPLIER-STATUS  TO WS-SUPPLIER-STATUS
                WHEN OTHER
                    GO TO 710-EXIT
           END-EVALUATE.

      * Evaluate SUPPLIER-ACT-DATE
           EVALUATE TRUE
                WHEN WS-MONTH-31 ALSO WS-DAY-31
                WHEN WS-MONTH-30 ALSO WS-DAY-30
                WHEN WS-MONTH-28-29 ALSO WS-DAY-28
                WHEN WS-MONTH-28-29 ALSO WS-DAY-29
                WHEN WS-VALID-YEAR
                    MOVE SUPPLIER-ACT-DATE  TO WS-SUPPLIER-ACT-DATE
                WHEN OTHER
                    GO TO 710-EXIT
           END-EVALUATE.
       500-ADDREDIT.
           MOVE ZIP-REC TO WS-ZIP.
      * Evaluate ADDRESS-TYPE
           EVALUATE TRUE
                WHEN ORDER-ADDRESS
                WHEN SCHED-ADDRESS
                WHEN REMIT-ADDRESS
                     MOVE ADDRESS-1 TO WS-ADDRESS-1
                     MOVE CITY TO WS-CITY
                WHEN OTHER
                    GO TO 520-EXIT
           END-EVALUATE.
      * Indexed SEARCH code pattern
           PERFORM VARYING ADDR-IDX FROM 1 BY 1
               UNTIL ADDR-IDX > 3 OR DATA-FOUND
           SET ADDR-IDX TO 1
      *  Find the qualified state abbreviation and zipcode
           SEARCH SUPP-ADDRESS
           WHEN ADDR-STATE (ADDR-IDX)  = WS-ST-ABBR
                AND WS-ZIP-CODE > WS-ZIP-LOW
                AND WS-ZIP-CODE < WS-ZIP-HIG
                MOVE 'Y' TO SW-DATA-FOUND
                MOVE ADDR-STATE TO WS-ADDR-STATE
                MOVE ZIP-CODE TO WS-ZIP-CODE
           END-SEARCH
           IF SW-DATA-FOUND = 'N'
               GO TO 520-EXIT
           END-IF
           END-PERFORM.

           PERFORM 510-READ-A-ZIP.

       510-READ-A-ZIP.
           READ ZIPFILE
               AT END
               MOVE 'Y' TO SW-MORE-DATA
           END-READ.

       520-EXIT.
           EXIT.

       600-POEDIT.

       710-EXIT.
           EXIT.
