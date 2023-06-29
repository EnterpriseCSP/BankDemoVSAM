000100***************************************************************** ZBNKPRT1
000200*                                                               * ZBNKPRT1
000300*   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * ZBNKPRT1
000400*   This demonstration program is provided for use by users     * ZBNKPRT1
000500*   of Micro Focus products and may be used, modified and       * ZBNKPRT1
000600*   distributed as part of your application provided that       * ZBNKPRT1
000700*   you properly acknowledge the copyright of Micro Focus       * ZBNKPRT1
000800*   in this material.                                           * ZBNKPRT1
000900*                                                               * ZBNKPRT1
001000***************************************************************** ZBNKPRT1
001100                                                                  ZBNKPRT1
001200***************************************************************** ZBNKPRT1
001300* Program:     ZBNKPRT1.CBL                                     * ZBNKPRT1
001400* Function:    Print the bank statements                        * ZBNKPRT1
001500***************************************************************** ZBNKPRT1
001600 IDENTIFICATION DIVISION.                                         ZBNKPRT1
001700 PROGRAM-ID.                                                      ZBNKPRT1
001800     ZBNKPRT1.                                                    ZBNKPRT1
001900 DATE-WRITTEN.                                                    ZBNKPRT1
002000     September 2002.                                              ZBNKPRT1
002100 DATE-COMPILED.                                                   ZBNKPRT1
002200     Today.                                                       ZBNKPRT1
002300                                                                  ZBNKPRT1
002400 ENVIRONMENT DIVISION.                                            ZBNKPRT1
002500 INPUT-OUTPUT   SECTION.                                          ZBNKPRT1
002600   FILE-CONTROL.                                                  ZBNKPRT1
002700     SELECT EXTRACT-FILE                                          ZBNKPRT1
002800            ASSIGN       TO EXTRACT                               ZBNKPRT1
002900            ORGANIZATION IS SEQUENTIAL                            ZBNKPRT1
003000            ACCESS MODE  IS SEQUENTIAL                            ZBNKPRT1
003100            FILE STATUS  IS WS-EXTRACT-STATUS.                    ZBNKPRT1
003200     SELECT PRINTOUT-FILE                                         ZBNKPRT1
003300            ASSIGN       TO PRINTOUT                              ZBNKPRT1
003400            ORGANIZATION IS SEQUENTIAL                            ZBNKPRT1
003500            ACCESS MODE  IS SEQUENTIAL                            ZBNKPRT1
003600            FILE STATUS  IS WS-PRINTOUT-STATUS.                   ZBNKPRT1
003700                                                                  ZBNKPRT1
003800 DATA DIVISION.                                                   ZBNKPRT1
003900 FILE SECTION.                                                    ZBNKPRT1
004000                                                                  ZBNKPRT1
004100 FD  EXTRACT-FILE                                                 ZBNKPRT1
004200     RECORDING MODE IS V                                          ZBNKPRT1
004300     RECORD CONTAINS 66 TO 95 CHARACTERS.                         ZBNKPRT1
004400 COPY CBANKXT1.                                                   ZBNKPRT1
004500                                                                  ZBNKPRT1
004600 FD  PRINTOUT-FILE.                                               ZBNKPRT1
004700 01  PRINTOUT-REC                            PIC X(121).          ZBNKPRT1
004800                                                                  ZBNKPRT1
004900 WORKING-STORAGE SECTION.                                         ZBNKPRT1
005000 COPY CTIMERD.                                                    ZBNKPRT1
005100                                                                  ZBNKPRT1
005200 01  WS-DATE-WORK-AREA.                                           ZBNKPRT1
005300 COPY CDATED.                                                     ZBNKPRT1
005400                                                                  ZBNKPRT1
005500 01  WS-MISC-STORAGE.                                             ZBNKPRT1
005600   05  WS-PROGRAM-ID                         PIC X(8)             ZBNKPRT1
005700       VALUE 'ZBNKPRT1'.                                          ZBNKPRT1
005800   05  WS-EXTRACT-STATUS.                                         ZBNKPRT1
005900     10  WS-EXTRACT-STAT1                    PIC X(1).            ZBNKPRT1
006000     10  WS-EXTRACT-STAT2                    PIC X(1).            ZBNKPRT1
006100                                                                  ZBNKPRT1
006200   05  WS-PRINTOUT-STATUS.                                        ZBNKPRT1
006300     10  WS-PRINTOUT-STAT1                   PIC X(1).            ZBNKPRT1
006400     10  WS-PRINOUTY-STAT2                   PIC X(1).            ZBNKPRT1
006500                                                                  ZBNKPRT1
006600   05  WS-IO-STATUS.                                              ZBNKPRT1
006700     10  WS-IO-STAT1                         PIC X(1).            ZBNKPRT1
006800     10  WS-IO-STAT2                         PIC X(1).            ZBNKPRT1
006900                                                                  ZBNKPRT1
007000   05  WS-TWO-BYTES.                                              ZBNKPRT1
007100     10  WS-TWO-BYTES-LEFT                   PIC X(1).            ZBNKPRT1
007200     10  WS-TWO-BYTES-RIGHT                  PIC X(1).            ZBNKPRT1
007300   05 WS-TWO-BYTES-BINARY REDEFINES WS-TWO-BYTES                  ZBNKPRT1
007400                                             PIC 9(1) COMP.       ZBNKPRT1
007500                                                                  ZBNKPRT1
007600   05  WS-FIRST-REC                          PIC X(3)             ZBNKPRT1
007700       VALUE 'YES'.                                               ZBNKPRT1
007800                                                                  ZBNKPRT1
007900   05  WS-END-OF-FILE                        PIC X(3)             ZBNKPRT1
008000       VALUE 'NO '.                                               ZBNKPRT1
008100                                                                  ZBNKPRT1
008200   05  WS-RECORDS-READ                       PIC 9(5)             ZBNKPRT1
008300       VALUE ZERO.                                                ZBNKPRT1
008400                                                                  ZBNKPRT1
008500   05  WS-TXNS-FLAG                          PIC X(1).            ZBNKPRT1
008600     88  TXNS-PRINTED                        VALUE '1'.           ZBNKPRT1
008700     88  NO-TXNS-PRINTED                     VALUE '0'.           ZBNKPRT1
008800                                                                  ZBNKPRT1
008900   05  WS-SUB1                               PIC 9(3).            ZBNKPRT1
009000   05  WS-SYS-DATE                           PIC 9(5).            ZBNKPRT1
009100   05  WS-SYS-TIME                           PIC 9(8).            ZBNKPRT1
009200   05  WS-PRINTED.                                                ZBNKPRT1
009300     10  WS-PRINTED-DATE.                                         ZBNKPRT1
009400       15  FILLER                            PIC X(9)             ZBNKPRT1
009500           VALUE 'Printed: '.                                     ZBNKPRT1
009600       15  WS-PRINT-DATE                     PIC X(11)            ZBNKPRT1
009700           VALUE 'dd mmm yyyy'.                                   ZBNKPRT1
009800     10  WS-PRINTED-TIME.                                         ZBNKPRT1
009900       15  FILLER                            PIC X(12)            ZBNKPRT1
010000           VALUE SPACES.                                          ZBNKPRT1
010100       15  WS-PRINT-TIME.                                         ZBNKPRT1
010200         20  WS-PRINT-TIME-HH                PIC X(2).            ZBNKPRT1
010300         20  WS-PRINT-TIME-DOT1              PIC X(1).            ZBNKPRT1
010400         20  WS-PRINT-TIME-MM                PIC X(2).            ZBNKPRT1
010500         20  WS-PRINT-TIME-DOT2              PIC X(1).            ZBNKPRT1
010600         20  WS-PRINT-TIME-SS                PIC X(2).            ZBNKPRT1
010700   05  WS-TOTAL-TXNS                         PIC S9(7)V99 COMP-3. ZBNKPRT1
010800   05  WS-TOTAL-ASSETS                       PIC S9(7)V99 COMP-3. ZBNKPRT1
010900                                                                  ZBNKPRT1
011000                                                                  ZBNKPRT1
011100 01  WS-PRINT-LINES.                                              ZBNKPRT1
011200   05  WS-LINE1.                                                  ZBNKPRT1
011300     10  WS-LINE1-CC                         PIC X(1)             ZBNKPRT1
011400         VALUE '1'.                                               ZBNKPRT1
011500     10  FILLER                              PIC X(40)            ZBNKPRT1
011600         VALUE SPACES.                                            ZBNKPRT1
011700     10  WS-LINE1-HEAD                       PIC X(21)            ZBNKPRT1
011800         VALUE 'Micro Focus Demo Bank'.                           ZBNKPRT1
011900                                                                  ZBNKPRT1
012000   05  WS-LINE2.                                                  ZBNKPRT1
012100     10  WS-LINE2-CC                         PIC X(1)             ZBNKPRT1
012200         VALUE ' '.                                               ZBNKPRT1
012300     10  FILLER                              PIC X(40)            ZBNKPRT1
012400         VALUE SPACES.                                            ZBNKPRT1
012500     10  WS-LINE1-HEAD                       PIC X(20)            ZBNKPRT1
012600         VALUE 'Statement of Account'.                            ZBNKPRT1
012700                                                                  ZBNKPRT1
012800   05  WS-LINE3.                                                  ZBNKPRT1
012900     10  WS-LINE3-CC                         PIC X(1)             ZBNKPRT1
013000         VALUE '0'.                                               ZBNKPRT1
013100     10  WS-LINE3-NAME-ADDR                  PIC X(23)            ZBNKPRT1
013200         VALUE SPACES.                                            ZBNKPRT1
013300     10  FILLER                              PIC X(55)            ZBNKPRT1
013400         VALUE SPACES.                                            ZBNKPRT1
013500     10  WS-LINE3-DATE                       PIC X(20)            ZBNKPRT1
013600         VALUE SPACES.                                            ZBNKPRT1
013700                                                                  ZBNKPRT1
013800   05  WS-LINE4.                                                  ZBNKPRT1
013900     10  WS-LINE4-CC                         PIC X(1)             ZBNKPRT1
014000         VALUE '0'.                                               ZBNKPRT1
014100     10  FILLER                              PIC X(14)            ZBNKPRT1
014200         VALUE 'Account No.'.                                     ZBNKPRT1
014300     10  FILLER                              PIC X(38)            ZBNKPRT1
014400         VALUE 'Description '.                                    ZBNKPRT1
014500     10  FILLER                              PIC X(15)            ZBNKPRT1
014600         VALUE '    Date  '.                                      ZBNKPRT1
014700     10  FILLER                              PIC X(18)            ZBNKPRT1
014800         VALUE '      Amount '.                                   ZBNKPRT1
014900     10  FILLER                              PIC X(18)            ZBNKPRT1
015000         VALUE '     Balance '.                                   ZBNKPRT1
015100                                                                  ZBNKPRT1
015200   05  WS-LINE5.                                                  ZBNKPRT1
015300     10  WS-LINE5-CC                         PIC X(1).            ZBNKPRT1
015400     10  WS-LINE5-ACC-NO                     PIC X(9).            ZBNKPRT1
015500     10  FILLER                              PIC X(5).            ZBNKPRT1
015600     10  WS-LINE5-DESC.                                           ZBNKPRT1
015700       15  WS-LINE5-DESC-PT1                 PIC X(15).           ZBNKPRT1
015800       15  WS-LINE5-DESC-PT2                 PIC X(18).           ZBNKPRT1
015900     10  FILLER                              PIC X(5).            ZBNKPRT1
016000     10  WS-LINE5-DATE                       PIC X(11).           ZBNKPRT1
016100     10  FILLER                              PIC X(4).            ZBNKPRT1
016200     10  WS-LINE5-AMOUNT-DASH                PIC X(13).           ZBNKPRT1
016300     10  WS-LINE5-AMOUNT REDEFINES WS-LINE5-AMOUNT-DASH           ZBNKPRT1
016400                                             PIC Z,ZZZ,ZZ9.99-.   ZBNKPRT1
016500     10  FILLER                              PIC X(5).            ZBNKPRT1
016600     10  WS-LINE5-BALANCE-DASH               PIC X(13).           ZBNKPRT1
016700     10  WS-LINE5-BALANCE REDEFINES WS-LINE5-BALANCE-DASH         ZBNKPRT1
016800                                             PIC Z,ZZZ,ZZZ.99-.   ZBNKPRT1
016900                                                                  ZBNKPRT1
017000 01  WS-CONSOLE-MESSAGE                      PIC X(48).           ZBNKPRT1
017100                                                                  ZBNKPRT1
017200 01  WS-EXEC-PARM.                                                ZBNKPRT1
017300   05  WS-EXEC-PARM-LL                       PIC S9(4) COMP.      ZBNKPRT1
017400   05  WS-EXEC-PARM-DATA                     PIC X(12).           ZBNKPRT1
017500                                                                  ZBNKPRT1
017600 COPY CSTATESD.                                                   ZBNKPRT1
017700                                                                  ZBNKPRT1
017800 COPY CABENDD.                                                    ZBNKPRT1
017900                                                                  ZBNKPRT1
018000 01  WS-PARM-PTR                             POINTER.             ZBNKPRT1
018100 01  WS-PARM-PTR-NUM REDEFINES WS-PARM-PTR   PIC 9(4) COMP.       ZBNKPRT1
018200
       01 DMP-TITLE PIC X(80) VALUE 'CEEDUMP FROM HANDLER ROUTINE'.
       01 DMP-OPTIONS PIC X(255) VALUE 'TRACE FILE VAR STOR'.
       01 FEEDBACK.
          10 FB-SEV PIC S9(4) COMP.
          10 FB-MSGNO PIC S9(4) COMP.
          10 FB-CASE-SEV PIC X.
          10 FB-FAC-ID PIC X(3).
          10 FB-ISINFO PIC S9(8) COMP.                                  ZBNKPRT1
       01 MY-DATE-LILIAN PIC S9(9) BINARY.
       01 MY-SECS-LILIAN PIC S9(9) COMP.
       01 MY-TIME-GREGORIAN PIC X(17).
018300 LINKAGE SECTION.                                                 ZBNKPRT1
018400 01  LK-EXEC-PARM.                                                ZBNKPRT1
018500   05  LK-EXEC-PARM-LL                       PIC S9(4) COMP.      ZBNKPRT1
018600   05  LK-EXEC-PARM-DATA                     PIC X(12).           ZBNKPRT1
018700                                                                  ZBNKPRT1
018800 PROCEDURE DIVISION USING LK-EXEC-PARM.                           ZBNKPRT1
018900                                                                  ZBNKPRT1
019000     PERFORM RUN-TIME.                                            ZBNKPRT1
019100                                                                  ZBNKPRT1
019200     MOVE ZEROES TO WS-EXEC-PARM-LL.                              ZBNKPRT1
019300     MOVE SPACES TO WS-EXEC-PARM-DATA.                            ZBNKPRT1
019400                                                                  ZBNKPRT1
019500     SET WS-PARM-PTR TO ADDRESS OF LK-EXEC-PARM.                  ZBNKPRT1
019600     IF WS-PARM-PTR-NUM IS NOT EQUAL TO ZEROS                     ZBNKPRT1
019700        MOVE LK-EXEC-PARM-LL TO WS-EXEC-PARM-LL                   ZBNKPRT1
019800        IF WS-EXEC-PARM-LL IS GREATER THAN                        ZBNKPRT1
019900             LENGTH OF WS-EXEC-PARM-DATA                          ZBNKPRT1
020000           MOVE LENGTH OF WS-EXEC-PARM-DATA TO WS-EXEC-PARM-LL    ZBNKPRT1
020100        END-IF                                                    ZBNKPRT1
020200        IF WS-EXEC-PARM-LL IS GREATER THAN ZERO                   ZBNKPRT1
020300           MOVE LK-EXEC-PARM-DATA (1:WS-EXEC-PARM-LL)             ZBNKPRT1
020400             TO WS-EXEC-PARM-DATA (1:WS-EXEC-PARM-LL)             ZBNKPRT1
020500        END-IF                                                    ZBNKPRT1
020600     END-IF.                                                      ZBNKPRT1
020700                                                                  ZBNKPRT1
020800     IF WS-EXEC-PARM-LL IS EQUAL TO ZERO                          ZBNKPRT1
020900        MOVE 'No exec card parm present'                          ZBNKPRT1
021000          TO WS-CONSOLE-MESSAGE                                   ZBNKPRT1
021100        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKPRT1
021200     ELSE                                                         ZBNKPRT1
021300       MOVE SPACES TO WS-CONSOLE-MESSAGE                          ZBNKPRT1
021400       STRING 'Exec parm is "' DELIMITED BY SIZE                  ZBNKPRT1
021500              WS-EXEC-PARM-DATA (1:WS-EXEC-PARM-LL)               ZBNKPRT1
021600                DELIMITED BY SIZE                                 ZBNKPRT1
021700              '"' DELIMITED BY SIZE                               ZBNKPRT1
021800         INTO WS-CONSOLE-MESSAGE                                  ZBNKPRT1
021900       PERFORM DISPLAY-CONSOLE-MESSAGE                            ZBNKPRT1
022000     END-IF.                                                      ZBNKPRT1
022100                                                                  ZBNKPRT1
022200     ACCEPT WS-SYS-DATE FROM DAY.                                 ZBNKPRT1
022300     SET DD-ENV-NULL TO TRUE.                                     ZBNKPRT1
022400     SET DDI-YYDDD TO TRUE.                                       ZBNKPRT1
022500     MOVE WS-SYS-DATE TO DDI-DATA.                                ZBNKPRT1
022600     SET DDO-DD-MMM-YYYY TO TRUE.                                 ZBNKPRT1
022700     CALL 'UDATECNV' USING WS-DATE-WORK-AREA.                     ZBNKPRT1
022800     MOVE DDO-DATA TO WS-PRINT-DATE.                              ZBNKPRT1
022900                                                                  ZBNKPRT1
023000     PERFORM EXTRACT-OPEN.                                        ZBNKPRT1
023100     PERFORM PRINTOUT-OPEN.                                       ZBNKPRT1
023200                                                                  ZBNKPRT1
023300     PERFORM UNTIL WS-END-OF-FILE = 'YES'                         ZBNKPRT1
023400       IF WS-END-OF-FILE = 'NO '                                  ZBNKPRT1
023500          PERFORM EXTRACT-GET                                     ZBNKPRT1
023600          IF WS-END-OF-FILE = 'NO '                               ZBNKPRT1
023700             ADD 1 TO WS-RECORDS-READ                             ZBNKPRT1
023800             IF WS-RECORDS-READ IS LESS THAN 6                    ZBNKPRT1
023900                DISPLAY BANKXT01-REC1 UPON CONSOLE                ZBNKPRT1
024000             ELSE                                                 ZBNKPRT1
024100                IF WS-RECORDS-READ IS EQUAL TO 6                  ZBNKPRT1
024200                   MOVE 'Suppressing record display...'           ZBNKPRT1
024300                      TO WS-CONSOLE-MESSAGE                       ZBNKPRT1
024400                   PERFORM DISPLAY-CONSOLE-MESSAGE                ZBNKPRT1
024500                END-IF                                            ZBNKPRT1
024600             END-IF                                               ZBNKPRT1
024700             PERFORM FORMAT-AND-PRINT                             ZBNKPRT1
024800          ELSE                                                    ZBNKPRT1
024900             PERFORM PRINT-TOTAL-TXNS                             ZBNKPRT1
025000             PERFORM PRINT-TOTAL-ASSETS                           ZBNKPRT1
025100          END-IF                                                  ZBNKPRT1
025200       END-IF                                                     ZBNKPRT1
025300     END-PERFORM.                                                 ZBNKPRT1
025400                                                                  ZBNKPRT1
025500     PERFORM EXTRACT-CLOSE.                                       ZBNKPRT1
025600     PERFORM PRINTOUT-CLOSE.                                      ZBNKPRT1
025700                                                                  ZBNKPRT1
025800     PERFORM DISPLAY-CONSOLE-MESSAGE.                             ZBNKPRT1
025900     MOVE 'End Of Job'                                            ZBNKPRT1
026000       TO WS-CONSOLE-MESSAGE.                                     ZBNKPRT1
026100     PERFORM DISPLAY-CONSOLE-MESSAGE.                             ZBNKPRT1
026200                                                                  ZBNKPRT1
026300     PERFORM RUN-TIME.                                            ZBNKPRT1
026400                                                                  ZBNKPRT1
026500     MOVE 0 TO RETURN-CODE.                                       ZBNKPRT1
026600                                                                  ZBNKPRT1
026700     GOBACK.                                                      ZBNKPRT1
026800                                                                  ZBNKPRT1
026900***************************************************************** ZBNKPRT1
027000* Format print lines                                            * ZBNKPRT1
027100***************************************************************** ZBNKPRT1
027200 FORMAT-AND-PRINT.                                                ZBNKPRT1
027300     IF BANKXT01-1-TYPE IS EQUAL TO '1'                           ZBNKPRT1
027400        PERFORM PRINT-TOTAL-TXNS                                  ZBNKPRT1
027500        PERFORM PRINT-TOTAL-ASSETS                                ZBNKPRT1
027600        MOVE WS-LINE1 TO PRINTOUT-REC                             ZBNKPRT1
027700        PERFORM PRINTOUT-PUT                                      ZBNKPRT1
027800        MOVE WS-LINE2 TO PRINTOUT-REC                             ZBNKPRT1
027900        PERFORM PRINTOUT-PUT                                      ZBNKPRT1
028000        MOVE '0' TO WS-LINE3-CC                                   ZBNKPRT1
028100        MOVE BANKXT01-1-NAME TO WS-LINE3-NAME-ADDR                ZBNKPRT1
028200        MOVE WS-PRINTED-DATE TO WS-LINE3-DATE                     ZBNKPRT1
028300        MOVE WS-LINE3 TO PRINTOUT-REC                             ZBNKPRT1
028400        PERFORM PRINTOUT-PUT                                      ZBNKPRT1
028500        MOVE ' ' TO WS-LINE3-CC                                   ZBNKPRT1
028600        MOVE BANKXT01-1-ADDR1 TO WS-LINE3-NAME-ADDR               ZBNKPRT1
028700        ACCEPT WS-SYS-TIME FROM TIME                              ZBNKPRT1
028800        MOVE WS-SYS-TIME (1:2) TO WS-PRINT-TIME-HH                ZBNKPRT1
028900        MOVE ':' TO WS-PRINT-TIME-DOT1                            ZBNKPRT1
029000        MOVE WS-SYS-TIME (3:2) TO WS-PRINT-TIME-MM                ZBNKPRT1
029100        MOVE ':' TO WS-PRINT-TIME-DOT2                            ZBNKPRT1
029200        MOVE WS-SYS-TIME (5:2) TO WS-PRINT-TIME-SS                ZBNKPRT1
029300        MOVE WS-PRINTED-TIME TO WS-LINE3-DATE                     ZBNKPRT1
029400        MOVE WS-LINE3 TO PRINTOUT-REC                             ZBNKPRT1
029500        PERFORM PRINTOUT-PUT                                      ZBNKPRT1
029600        MOVE ' ' TO WS-LINE3-CC                                   ZBNKPRT1
029700        MOVE BANKXT01-1-ADDR2 TO WS-LINE3-NAME-ADDR               ZBNKPRT1
029800        MOVE SPACES TO WS-LINE3-DATE                              ZBNKPRT1
029900        MOVE WS-LINE3 TO PRINTOUT-REC                             ZBNKPRT1
030000        PERFORM PRINTOUT-PUT                                      ZBNKPRT1
030100        MOVE ' ' TO WS-LINE3-CC                                   ZBNKPRT1
030200        MOVE BANKXT01-1-STATE TO STATE-PROV-WK-CODE               ZBNKPRT1
030300        PERFORM EXPAND-STATE-PROV THRU                            ZBNKPRT1
030400                EXPAND-STATE-PROV-EXIT                            ZBNKPRT1
030500        MOVE STATE-PROV-WK-NAME TO WS-LINE3-NAME-ADDR             ZBNKPRT1
030600        MOVE SPACES TO WS-LINE3-DATE                              ZBNKPRT1
030700        MOVE WS-LINE3 TO PRINTOUT-REC                             ZBNKPRT1
030800        PERFORM PRINTOUT-PUT                                      ZBNKPRT1
030900        MOVE ' ' TO WS-LINE3-CC                                   ZBNKPRT1
031000        MOVE BANKXT01-1-CNTRY TO WS-LINE3-NAME-ADDR               ZBNKPRT1
031100        MOVE SPACES TO WS-LINE3-DATE                              ZBNKPRT1
031200        MOVE WS-LINE3 TO PRINTOUT-REC                             ZBNKPRT1
031300        PERFORM PRINTOUT-PUT                                      ZBNKPRT1
031400        MOVE ' ' TO WS-LINE3-CC                                   ZBNKPRT1
031500        MOVE BANKXT01-1-PST-CDE TO WS-LINE3-NAME-ADDR             ZBNKPRT1
031600        MOVE SPACES TO WS-LINE3-DATE                              ZBNKPRT1
031700        MOVE WS-LINE3 TO PRINTOUT-REC                             ZBNKPRT1
031800        PERFORM PRINTOUT-PUT                                      ZBNKPRT1
031900        MOVE WS-LINE4 TO PRINTOUT-REC                             ZBNKPRT1
032000        PERFORM PRINTOUT-PUT                                      ZBNKPRT1
032100        MOVE ZERO TO WS-TOTAL-TXNS                                ZBNKPRT1
032200        MOVE ZERO TO WS-TOTAL-ASSETS                              ZBNKPRT1
032300     END-IF.                                                      ZBNKPRT1
032400     IF BANKXT01-2-TYPE IS EQUAL TO '2'                           ZBNKPRT1
032500        PERFORM PRINT-TOTAL-TXNS                                  ZBNKPRT1
032600        MOVE SPACES TO WS-LINE5                                   ZBNKPRT1
032700        MOVE BANKXT01-2-ACC-NO TO WS-LINE5-ACC-NO                 ZBNKPRT1
032800        MOVE 'Last statement' TO WS-LINE5-DESC-PT1                ZBNKPRT1
032900        MOVE BANKXT01-2-ACC-DESC TO WS-LINE5-DESC-PT2             ZBNKPRT1
033000        MOVE BANKXT01-2-ACC-LAST-STMT-DTE TO DDI-DATA             ZBNKPRT1
033100        SET DD-ENV-NULL TO TRUE                                   ZBNKPRT1
033200        SET DDI-ISO TO TRUE                                       ZBNKPRT1
033300        SET DDO-DD-MMM-YYYY TO TRUE                               ZBNKPRT1
033400        CALL 'UDATECNV' USING WS-DATE-WORK-AREA                   ZBNKPRT1
033500        MOVE DDO-DATA TO WS-LINE5-DATE                            ZBNKPRT1
033600        MOVE BANKXT01-2-ACC-CURR-BAL TO WS-LINE5-BALANCE          ZBNKPRT1
033700        ADD BANKXT01-2-ACC-CURR-BAL TO WS-TOTAL-ASSETS            ZBNKPRT1
033800        MOVE WS-LINE5 TO PRINTOUT-REC                             ZBNKPRT1
033900        PERFORM PRINTOUT-PUT                                      ZBNKPRT1
034000     END-IF.                                                      ZBNKPRT1
034100     IF BANKXT01-3-TYPE IS EQUAL TO '3'                           ZBNKPRT1
034200        MOVE SPACES TO WS-LINE5                                   ZBNKPRT1
034300        MOVE BANKXT01-3-DESC TO WS-LINE5-DESC (4:30)              ZBNKPRT1
034400        MOVE BANKXT01-3-TIMESTAMP (1:10) TO DDI-DATA              ZBNKPRT1
034500        SET DD-ENV-NULL TO TRUE                                   ZBNKPRT1
034600        SET DDI-ISO TO TRUE                                       ZBNKPRT1
034700        SET DDO-DD-MMM-YYYY TO TRUE                               ZBNKPRT1
034800        CALL 'UDATECNV' USING WS-DATE-WORK-AREA                   ZBNKPRT1
034900        MOVE DDO-DATA TO WS-LINE5-DATE                            ZBNKPRT1
035000        MOVE BANKXT01-3-AMOUNT TO WS-LINE5-AMOUNT                 ZBNKPRT1
035100        ADD BANKXT01-3-AMOUNT TO WS-TOTAL-TXNS                    ZBNKPRT1
035200        SET TXNS-PRINTED TO TRUE                                  ZBNKPRT1
035300        MOVE WS-LINE5 TO PRINTOUT-REC                             ZBNKPRT1
035400        PERFORM PRINTOUT-PUT                                      ZBNKPRT1
035500     END-IF.                                                      ZBNKPRT1
035600                                                                  ZBNKPRT1
035700***************************************************************** ZBNKPRT1
035800* Format and print transaction totals                           * ZBNKPRT1
035900***************************************************************** ZBNKPRT1
036000 PRINT-TOTAL-TXNS.                                                ZBNKPRT1
036100     IF TXNS-PRINTED                                              ZBNKPRT1
036200        MOVE SPACES TO WS-LINE5                                   ZBNKPRT1
036300        MOVE '------------' TO WS-LINE5-AMOUNT-DASH               ZBNKPRT1
036400        MOVE WS-LINE5 TO PRINTOUT-REC                             ZBNKPRT1
036500        PERFORM PRINTOUT-PUT                                      ZBNKPRT1
036600        MOVE SPACES TO WS-LINE5-DESC                              ZBNKPRT1
036700        MOVE 'Total transactions' TO WS-LINE5-DESC (4:30)         ZBNKPRT1
036800        MOVE WS-TOTAL-TXNS TO WS-LINE5-AMOUNT                     ZBNKPRT1
036900        MOVE ZERO TO WS-TOTAL-TXNS                                ZBNKPRT1
037000        SET NO-TXNS-PRINTED TO TRUE                               ZBNKPRT1
037100        MOVE WS-LINE5 TO PRINTOUT-REC                             ZBNKPRT1
037200        PERFORM PRINTOUT-PUT                                      ZBNKPRT1
037300     END-IF.                                                      ZBNKPRT1
037400                                                                  ZBNKPRT1
037500                                                                  ZBNKPRT1
037600***************************************************************** ZBNKPRT1
037700* Format and print "page" totals                                * ZBNKPRT1
037800***************************************************************** ZBNKPRT1
037900 PRINT-TOTAL-ASSETS.                                              ZBNKPRT1
038000     IF WS-FIRST-REC IS EQUAL TO 'YES'                            ZBNKPRT1
038100        MOVE 'NO' TO WS-FIRST-REC                                 ZBNKPRT1
038200        SET NO-TXNS-PRINTED TO TRUE                               ZBNKPRT1
038300     ELSE                                                         ZBNKPRT1
038400        MOVE SPACES TO WS-LINE5                                   ZBNKPRT1
038500        MOVE '------------' TO WS-LINE5-BALANCE-DASH              ZBNKPRT1
038600        MOVE WS-LINE5 TO PRINTOUT-REC                             ZBNKPRT1
038700        PERFORM PRINTOUT-PUT                                      ZBNKPRT1
038800        MOVE SPACES TO WS-LINE5                                   ZBNKPRT1
038900        MOVE 'Total Assets' TO WS-LINE5-DESC                      ZBNKPRT1
039000        MOVE WS-TOTAL-ASSETS TO WS-LINE5-BALANCE                  ZBNKPRT1
039100        MOVE WS-LINE5 TO PRINTOUT-REC                             ZBNKPRT1
039200        PERFORM PRINTOUT-PUT                                      ZBNKPRT1
039300     END-IF.                                                      ZBNKPRT1
039400                                                                  ZBNKPRT1
039500***************************************************************** ZBNKPRT1
039600* Open the EXTRACTed data file                                 *  ZBNKPRT1
039700***************************************************************** ZBNKPRT1
039800 EXTRACT-OPEN.                                                    ZBNKPRT1
039900     OPEN INPUT EXTRACT-FILE.                                     ZBNKPRT1
040000     IF WS-EXTRACT-STATUS = '00'                                  ZBNKPRT1
040100        MOVE 'EXTRACT file opened OK'                             ZBNKPRT1
040200          TO WS-CONSOLE-MESSAGE                                   ZBNKPRT1
040300        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKPRT1
040400     ELSE                                                         ZBNKPRT1
040500        MOVE 'EXTRACT file open failure...'                       ZBNKPRT1
040600          TO WS-CONSOLE-MESSAGE                                   ZBNKPRT1
040700        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKPRT1
040800        MOVE WS-EXTRACT-STATUS TO WS-IO-STATUS                    ZBNKPRT1
040900        PERFORM DISPLAY-IO-STATUS                                 ZBNKPRT1
041000        PERFORM ABORT-PROGRAM                                     ZBNKPRT1
041100        END-IF.                                                   ZBNKPRT1
041200                                                                  ZBNKPRT1
041300***************************************************************** ZBNKPRT1
041400* Read a record from the EXTRACTed data file                    * ZBNKPRT1
041500***************************************************************** ZBNKPRT1
041600 EXTRACT-GET.                                                     ZBNKPRT1
041700     READ EXTRACT-FILE.                                           ZBNKPRT1
041800     IF WS-EXTRACT-STATUS NOT = '00'                              ZBNKPRT1
041900        IF WS-EXTRACT-STATUS = '10'                               ZBNKPRT1
042000           MOVE 'YES' TO WS-END-OF-FILE                           ZBNKPRT1
042100        ELSE                                                      ZBNKPRT1
042200           MOVE 'EXTRACT Error readng file ...'                   ZBNKPRT1
042300             TO WS-CONSOLE-MESSAGE                                ZBNKPRT1
042400            PERFORM DISPLAY-CONSOLE-MESSAGE                       ZBNKPRT1
042500            MOVE WS-EXTRACT-STATUS TO WS-IO-STATUS                ZBNKPRT1
042600            PERFORM DISPLAY-IO-STATUS                             ZBNKPRT1
042700            PERFORM ABORT-PROGRAM                                 ZBNKPRT1
042800        END-IF                                                    ZBNKPRT1
042900     END-IF.                                                      ZBNKPRT1
043000                                                                  ZBNKPRT1
043100***************************************************************** ZBNKPRT1
043200* Close the EXTRACTed data file                                 * ZBNKPRT1
043300***************************************************************** ZBNKPRT1
043400 EXTRACT-CLOSE.                                                   ZBNKPRT1
043500     CLOSE EXTRACT-FILE.                                          ZBNKPRT1
043600     IF WS-EXTRACT-STATUS = '00'                                  ZBNKPRT1
043700        MOVE 'EXTRACT file closed OK'                             ZBNKPRT1
043800          TO WS-CONSOLE-MESSAGE                                   ZBNKPRT1
043900        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKPRT1
044000     ELSE                                                         ZBNKPRT1
044100        MOVE 'EXTRACT file close failure...'                      ZBNKPRT1
044200          TO WS-CONSOLE-MESSAGE                                   ZBNKPRT1
044300        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKPRT1
044400        MOVE WS-EXTRACT-STATUS TO WS-IO-STATUS                    ZBNKPRT1
044500        PERFORM DISPLAY-IO-STATUS                                 ZBNKPRT1
044600        PERFORM ABORT-PROGRAM                                     ZBNKPRT1
044700     END-IF.                                                      ZBNKPRT1
044800                                                                  ZBNKPRT1
044900***************************************************************** ZBNKPRT1
045000* Open the seqential print file                                 * ZBNKPRT1
045100***************************************************************** ZBNKPRT1
045200 PRINTOUT-OPEN.                                                   ZBNKPRT1
045300     OPEN OUTPUT PRINTOUT-FILE.                                   ZBNKPRT1
045400     IF WS-PRINTOUT-STATUS = '00'                                 ZBNKPRT1
045500        MOVE 'PRINTOUT file opened OK'                            ZBNKPRT1
045600          TO WS-CONSOLE-MESSAGE                                   ZBNKPRT1
045700        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKPRT1
045800     ELSE                                                         ZBNKPRT1
045900        MOVE 'PRINTOUT file open failure...'                      ZBNKPRT1
046000          TO WS-CONSOLE-MESSAGE                                   ZBNKPRT1
046100        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKPRT1
046200        MOVE WS-PRINTOUT-STATUS TO WS-IO-STATUS                   ZBNKPRT1
046300        PERFORM DISPLAY-IO-STATUS                                 ZBNKPRT1
046400        PERFORM ABORT-PROGRAM                                     ZBNKPRT1
046500        END-IF.                                                   ZBNKPRT1
046600                                                                  ZBNKPRT1
046700***************************************************************** ZBNKPRT1
046800* Write a record to the squential file                          * ZBNKPRT1
046900***************************************************************** ZBNKPRT1
047000 PRINTOUT-PUT.                                                    ZBNKPRT1
047100     WRITE PRINTOUT-REC..                                         ZBNKPRT1
047200     IF WS-PRINTOUT-STATUS NOT = '00'                             ZBNKPRT1
047300        MOVE 'PRINTOUT Error Writing file ...'                    ZBNKPRT1
047400          TO WS-CONSOLE-MESSAGE                                   ZBNKPRT1
047500        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKPRT1
047600        MOVE WS-PRINTOUT-STATUS TO WS-IO-STATUS                   ZBNKPRT1
047700        PERFORM DISPLAY-IO-STATUS                                 ZBNKPRT1
047800        PERFORM ABORT-PROGRAM                                     ZBNKPRT1
047900     END-IF.                                                      ZBNKPRT1
048000                                                                  ZBNKPRT1
048100***************************************************************** ZBNKPRT1
048200* Close the seqential print file                                * ZBNKPRT1
048300***************************************************************** ZBNKPRT1
048400 PRINTOUT-CLOSE.                                                  ZBNKPRT1
048500     CLOSE PRINTOUT-FILE.                                         ZBNKPRT1
048600     IF WS-PRINTOUT-STATUS = '00'                                 ZBNKPRT1
048700        MOVE 'PRINTOUT file closed OK'                            ZBNKPRT1
048800          TO WS-CONSOLE-MESSAGE                                   ZBNKPRT1
048900        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKPRT1
049000     ELSE                                                         ZBNKPRT1
049100        MOVE 'PRINTOUT file close failure...'                     ZBNKPRT1
049200          TO WS-CONSOLE-MESSAGE                                   ZBNKPRT1
049300        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKPRT1
049400        MOVE WS-PRINTOUT-STATUS TO WS-IO-STATUS                   ZBNKPRT1
049500        PERFORM DISPLAY-IO-STATUS                                 ZBNKPRT1
049600        PERFORM ABORT-PROGRAM                                     ZBNKPRT1
049700     END-IF.                                                      ZBNKPRT1
049800                                                                  ZBNKPRT1
049900***************************************************************** ZBNKPRT1
050000* Display the file status bytes. This routine will display as   * ZBNKPRT1
050100* two digits if the full two byte file status is numeric. If    * ZBNKPRT1
050200* second byte is non-numeric then it will be treated as a       * ZBNKPRT1
050300* binary number.                                                * ZBNKPRT1
050400***************************************************************** ZBNKPRT1
050500 DISPLAY-IO-STATUS.                                               ZBNKPRT1
050600     IF WS-IO-STATUS NUMERIC                                      ZBNKPRT1
050700        MOVE SPACE TO WS-CONSOLE-MESSAGE                          ZBNKPRT1
050800        STRING 'File status -' DELIMITED BY SIZE                  ZBNKPRT1
050900               WS-IO-STATUS DELIMITED BY SIZE                     ZBNKPRT1
051000          INTO WS-CONSOLE-MESSAGE                                 ZBNKPRT1
051100        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKPRT1
051200     ELSE                                                         ZBNKPRT1
051300        SUBTRACT WS-TWO-BYTES-BINARY FROM WS-TWO-BYTES-BINARY     ZBNKPRT1
051400        MOVE WS-IO-STAT2 TO WS-TWO-BYTES-RIGHT                    ZBNKPRT1
051500        MOVE SPACE TO WS-CONSOLE-MESSAGE                          ZBNKPRT1
051600        STRING 'File status -' DELIMITED BY SIZE                  ZBNKPRT1
051700               WS-IO-STAT1 DELIMITED BY SIZE                      ZBNKPRT1
051800               '/' DELIMITED BY SIZE                              ZBNKPRT1
051900               WS-TWO-BYTES DELIMITED BY SIZE                     ZBNKPRT1
052000          INTO WS-CONSOLE-MESSAGE                                 ZBNKPRT1
052100        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKPRT1
052200     END-IF.                                                      ZBNKPRT1
052300                                                                  ZBNKPRT1
052400***************************************************************** ZBNKPRT1
052500* Expand the 2 character state/prove code to its full text      * ZBNKPRT1
052600***************************************************************** ZBNKPRT1
052700 EXPAND-STATE-PROV.                                               ZBNKPRT1
052800     MOVE 0 TO STATE-PROV-SUB.                                    ZBNKPRT1
052900     DIVIDE LENGTH OF STATE-PROV-DATA (1) INTO                    ZBNKPRT1
053000       LENGTH OF STATE-PROV-TABLE                                 ZBNKPRT1
053100         GIVING STATE-PROV-COUNT.                                 ZBNKPRT1
053200     MOVE STATE-PROV-WK-CODE TO STATE-PROV-WK-NAME.               ZBNKPRT1
053300 EXPAND-STATE-PROV-LOOP.                                          ZBNKPRT1
053400     ADD 1 TO STATE-PROV-SUB.                                     ZBNKPRT1
053500     IF STATE-PROV-SUB IS GREATER THAN STATE-PROV-COUNT           ZBNKPRT1
053600        GO TO EXPAND-STATE-PROV-EXIT                              ZBNKPRT1
053700     END-IF.                                                      ZBNKPRT1
053800     IF STATE-PROV-WK-CODE IS EQUAL TO                            ZBNKPRT1
053900          STATE-PROV-CODE (STATE-PROV-SUB)                        ZBNKPRT1
054000        MOVE STATE-PROV-NAME (STATE-PROV-SUB) TO                  ZBNKPRT1
054100          STATE-PROV-WK-NAME                                      ZBNKPRT1
054200        GO TO EXPAND-STATE-PROV-EXIT                              ZBNKPRT1
054300     END-IF.                                                      ZBNKPRT1
054400     GO TO EXPAND-STATE-PROV-LOOP.                                ZBNKPRT1
054500 EXPAND-STATE-PROV-EXIT.                                          ZBNKPRT1
054600     EXIT.                                                        ZBNKPRT1
054700                                                                  ZBNKPRT1
054800***************************************************************** ZBNKPRT1
054900* 'ABORT' the program.                                          * ZBNKPRT1
055000* Post a message to the console and issue a goback              * ZBNKPRT1
055100***************************************************************** ZBNKPRT1
055200 ABORT-PROGRAM.                                                   ZBNKPRT1
055300     IF WS-CONSOLE-MESSAGE NOT = SPACES                           ZBNKPRT1
055400        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKPRT1
055500     END-IF.                                                      ZBNKPRT1
055600     MOVE 'Program is abending...'  TO WS-CONSOLE-MESSAGE.        ZBNKPRT1
055700     PERFORM DISPLAY-CONSOLE-MESSAGE.                             ZBNKPRT1
      * HIO - Identify LE routines, routines are not executed,
           IF RETURN-CODE = 16
              CALL 'CEE3DMP' USING DMP-TITLE, DMP-OPTIONS, FEEDBACK
              CALL 'CEELOCT' USING MY-DATE-LILIAN, MY-SECS-LILIAN,
              MY-TIME-GREGORIAN, FEEDBACK
           END-IF
055800     MOVE 16 TO RETURN-CODE.
055900     GOBACK.                                                      ZBNKPRT1
056000                                                                  ZBNKPRT1
056100***************************************************************** ZBNKPRT1
056200* Display CONSOLE messages...                                   * ZBNKPRT1
056300***************************************************************** ZBNKPRT1
056400 DISPLAY-CONSOLE-MESSAGE.                                         ZBNKPRT1
056500     DISPLAY 'ZBNKPRT1 - ' WS-CONSOLE-MESSAGE                     ZBNKPRT1
056600       UPON CONSOLE.                                              ZBNKPRT1
056700     MOVE ALL SPACES TO WS-CONSOLE-MESSAGE.                       ZBNKPRT1
056800                                                                  ZBNKPRT1
056900 COPY CTIMERP.                                                    ZBNKPRT1
057000                                                                  ZBNKPRT1
