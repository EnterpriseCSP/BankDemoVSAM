000100***************************************************************** ZBNKDIAG
000200*                                                               * ZBNKDIAG
000300*   Copyright (C) 1998-2008 Micro Focus. All Rights Reserved.   * ZBNKDIAG
000400*   This demonstration program is provided for use by users     * ZBNKDIAG
000500*   of Micro Focus products and may be used, modified and       * ZBNKDIAG
000600*   distributed as part of your application provided that       * ZBNKDIAG
000700*   you properly acknowledge the copyright of Micro Focus       * ZBNKDIAG
000800*   in this material.                                           * ZBNKDIAG
000900*                                                               * ZBNKDIAG
001000***************************************************************** ZBNKDIAG
001100                                                                  ZBNKDIAG
001200***************************************************************** ZBNKDIAG
001300* Program:     ZBNKDIAG.CBL                                     * ZBNKDIAG
001400* Function:    Show how Mainframe Express handles diagnostics   * ZBNKDIAG
001500***************************************************************** ZBNKDIAG
001600 IDENTIFICATION DIVISION.                                         ZBNKDIAG
001700 PROGRAM-ID.                                                      ZBNKDIAG
001800     ZBNKDIAG.                                                    ZBNKDIAG
001900 DATE-WRITTEN.                                                    ZBNKDIAG
002000     September 2002.                                              ZBNKDIAG
002100 DATE-COMPILED.                                                   ZBNKDIAG
002200     Today.                                                       ZBNKDIAG
002300                                                                  ZBNKDIAG
002400 ENVIRONMENT DIVISION.                                            ZBNKDIAG
002500 INPUT-OUTPUT   SECTION.                                          ZBNKDIAG
002600   FILE-CONTROL.                                                  ZBNKDIAG
002700     SELECT EXTRACT-FILE                                          ZBNKDIAG
002800            ASSIGN       TO EXTRACT                               ZBNKDIAG
002900            ORGANIZATION IS SEQUENTIAL                            ZBNKDIAG
003000            ACCESS MODE  IS SEQUENTIAL                            ZBNKDIAG
003100            FILE STATUS  IS WS-EXTRACT-STATUS.                    ZBNKDIAG
003200     SELECT PRINTOUT-FILE                                         ZBNKDIAG
003300            ASSIGN       TO PRINTOUT                              ZBNKDIAG
003400            ORGANIZATION IS SEQUENTIAL                            ZBNKDIAG
003500            ACCESS MODE  IS SEQUENTIAL                            ZBNKDIAG
003600            FILE STATUS  IS WS-PRINTOUT-STATUS.                   ZBNKDIAG
003700                                                                  ZBNKDIAG
003800 DATA DIVISION.                                                   ZBNKDIAG
003900 FILE SECTION.                                                    ZBNKDIAG
004000                                                                  ZBNKDIAG
004100 FD  EXTRACT-FILE.                                                ZBNKDIAG
004200 COPY CBANKXT1.                                                   ZBNKDIAG
004300                                                                  ZBNKDIAG
004400 FD  PRINTOUT-FILE.                                               ZBNKDIAG
004500 01  PRINTOUT-REC                            PIC X(121).          ZBNKDIAG
004600                                                                  ZBNKDIAG
004700 WORKING-STORAGE SECTION.                                         ZBNKDIAG
004800 COPY CTIMERD.                                                    ZBNKDIAG
004900                                                                  ZBNKDIAG
005000 01  WS-DATE-WORK-AREA.                                           ZBNKDIAG
005100 COPY CDATED.                                                     ZBNKDIAG
005200                                                                  ZBNKDIAG
005300 01  WS-MISC-STORAGE.                                             ZBNKDIAG
005400   05  WS-PROGRAM-ID                         PIC X(8)             ZBNKDIAG
005500       VALUE 'ZBNKDIAG'.                                          ZBNKDIAG
005600   05  WS-EXTRACT-STATUS.                                         ZBNKDIAG
005700     10  WS-EXTRACT-STAT1                    PIC X(1).            ZBNKDIAG
005800     10  WS-EXTRACT-STAT2                    PIC X(1).            ZBNKDIAG
005900                                                                  ZBNKDIAG
006000   05  WS-PRINTOUT-STATUS.                                        ZBNKDIAG
006100     10  WS-PRINTOUT-STAT1                   PIC X(1).            ZBNKDIAG
006200     10  WS-PRINOUTY-STAT2                   PIC X(1).            ZBNKDIAG
006300                                                                  ZBNKDIAG
006400   05  WS-IO-STATUS.                                              ZBNKDIAG
006500     10  WS-IO-STAT1                         PIC X(1).            ZBNKDIAG
006600     10  WS-IO-STAT2                         PIC X(1).            ZBNKDIAG
006700                                                                  ZBNKDIAG
006800   05  WS-TWO-BYTES.                                              ZBNKDIAG
006900     10  WS-TWO-BYTES-LEFT                   PIC X(1).            ZBNKDIAG
007000     10  WS-TWO-BYTES-RIGHT                  PIC X(1).            ZBNKDIAG
007100   05 WS-TWO-BYTES-BINARY REDEFINES WS-TWO-BYTES                  ZBNKDIAG
007200                                             PIC 9(1) COMP.       ZBNKDIAG
007300                                                                  ZBNKDIAG
007400   05  WS-FIRST-REC                          PIC X(3)             ZBNKDIAG
007500       VALUE 'YES'.                                               ZBNKDIAG
007600                                                                  ZBNKDIAG
007700   05  WS-END-OF-FILE                        PIC X(3)             ZBNKDIAG
007800       VALUE 'NO '.                                               ZBNKDIAG
007900                                                                  ZBNKDIAG
008000   05  WS-RECORDS-READ                       PIC 9(5)             ZBNKDIAG
008100       VALUE ZERO.                                                ZBNKDIAG
008200                                                                  ZBNKDIAG
008300   05  WS-TXNS-FLAG                          PIC X(1).            ZBNKDIAG
008400     88  TXNS-PRINTED                        VALUE '1'.           ZBNKDIAG
008500     88  NO-TXNS-PRINTED                     VALUE '0'.           ZBNKDIAG
008600                                                                  ZBNKDIAG
008700   05  WS-SUB1                               PIC 9(3).            ZBNKDIAG
008800   05  WS-SYS-DATE                           PIC 9(5).            ZBNKDIAG
008900   05  WS-PRINTED.                                                ZBNKDIAG
009000     10  FILLER                              PIC X(9)             ZBNKDIAG
009100         VALUE 'Printed: '.                                       ZBNKDIAG
009200     10  WS-PRINT-DATE                       PIC X(11)            ZBNKDIAG
009300         VALUE 'dd mmm yyyy'.                                     ZBNKDIAG
009400                                                                  ZBNKDIAG
009500   05  WS-TOTAL-TXNS                         PIC S9(7)V99 COMP-3. ZBNKDIAG
009600   05  WS-TOTAL-ASSETS                       PIC S9(7)V99 COMP-3. ZBNKDIAG
009700                                                                  ZBNKDIAG
009800                                                                  ZBNKDIAG
009900 01  WS-PRINT-LINES.                                              ZBNKDIAG
010000   05  WS-LINE1.                                                  ZBNKDIAG
010100     10  WS-LINE1-CC                         PIC X(1)             ZBNKDIAG
010200         VALUE '1'.                                               ZBNKDIAG
010300     10  FILLER                              PIC X(40)            ZBNKDIAG
010400         VALUE SPACES.                                            ZBNKDIAG
010500     10  WS-LINE1-HEAD                       PIC X(21)            ZBNKDIAG
010600         VALUE 'Micro Focus Demo Bank'.                           ZBNKDIAG
010700                                                                  ZBNKDIAG
010800   05  WS-LINE2.                                                  ZBNKDIAG
010900     10  WS-LINE2-CC                         PIC X(1)             ZBNKDIAG
011000         VALUE ' '.                                               ZBNKDIAG
011100     10  FILLER                              PIC X(40)            ZBNKDIAG
011200         VALUE SPACES.                                            ZBNKDIAG
011300     10  WS-LINE1-HEAD                       PIC X(20)            ZBNKDIAG
011400         VALUE 'Statement of Account'.                            ZBNKDIAG
011500                                                                  ZBNKDIAG
011600   05  WS-LINE3.                                                  ZBNKDIAG
011700     10  WS-LINE3-CC                         PIC X(1)             ZBNKDIAG
011800         VALUE '0'.                                               ZBNKDIAG
011900     10  WS-LINE3-NAME-ADDR                  PIC X(23)            ZBNKDIAG
012000         VALUE SPACES.                                            ZBNKDIAG
012100     10  FILLER                              PIC X(55)            ZBNKDIAG
012200         VALUE SPACES.                                            ZBNKDIAG
012300     10  WS-LINE3-DATE                       PIC X(20)            ZBNKDIAG
012400         VALUE SPACES.                                            ZBNKDIAG
012500                                                                  ZBNKDIAG
012600   05  WS-LINE4.                                                  ZBNKDIAG
012700     10  WS-LINE4-CC                         PIC X(1)             ZBNKDIAG
012800         VALUE '0'.                                               ZBNKDIAG
012900     10  FILLER                              PIC X(14)            ZBNKDIAG
013000         VALUE 'Account No.'.                                     ZBNKDIAG
013100     10  FILLER                              PIC X(38)            ZBNKDIAG
013200         VALUE 'Description '.                                    ZBNKDIAG
013300     10  FILLER                              PIC X(15)            ZBNKDIAG
013400         VALUE '   Date   '.                                      ZBNKDIAG
013500     10  FILLER                              PIC X(18)            ZBNKDIAG
013600         VALUE '      Amount '.                                   ZBNKDIAG
013700     10  FILLER                              PIC X(18)            ZBNKDIAG
013800         VALUE '     Balance '.                                   ZBNKDIAG
013900                                                                  ZBNKDIAG
014000   05  WS-LINE5.                                                  ZBNKDIAG
014100     10  WS-LINE5-CC                         PIC X(1).            ZBNKDIAG
014200     10  WS-LINE5-ACC-NO                     PIC X(9).            ZBNKDIAG
014300     10  FILLER                              PIC X(5).            ZBNKDIAG
014400     10  WS-LINE5-DESC.                                           ZBNKDIAG
014500       15  WS-LINE5-DESC-PT1                 PIC X(15).           ZBNKDIAG
014600       15  WS-LINE5-DESC-PT2                 PIC X(18).           ZBNKDIAG
014700     10  FILLER                              PIC X(5).            ZBNKDIAG
014800     10  WS-LINE5-DATE                       PIC X(10).           ZBNKDIAG
014900     10  FILLER                              PIC X(5).            ZBNKDIAG
015000     10  WS-LINE5-AMOUNT-DASH                PIC X(13).           ZBNKDIAG
015100     10  WS-LINE5-AMOUNT REDEFINES WS-LINE5-AMOUNT-DASH           ZBNKDIAG
015200                                             PIC Z,ZZZ,ZZ9.99-.   ZBNKDIAG
015300     10  FILLER                              PIC X(5).            ZBNKDIAG
015400     10  WS-LINE5-BALANCE-DASH               PIC X(13).           ZBNKDIAG
015500     10  WS-LINE5-BALANCE REDEFINES WS-LINE5-BALANCE-DASH         ZBNKDIAG
015600                                             PIC Z,ZZZ,ZZZ.99-.   ZBNKDIAG
015700                                                                  ZBNKDIAG
015800 01  WS-CONSOLE-MESSAGE                      PIC X(48).           ZBNKDIAG
015900                                                                  ZBNKDIAG
016000 01  WS-EXEC-PARM                            PIC X(12).           ZBNKDIAG
016100                                                                  ZBNKDIAG
016200* The next line has an error - PIX X(!) should be PIC X(1)        ZBNKDIAG
016300 01  WS-UNUSED-FIELD                         PIC X(1).            ZBNKDIAG
016400                                                                  ZBNKDIAG
016500* The next line is used to show what happens in the project view  ZBNKDIAG
016600* when a copy book is not found                                   ZBNKDIAG
016700*COPY CNOTFND.                                                    ZBNKDIAG
016800*01 ppp pic x(1).                                                 ZBNKDIAG
016900                                                                  ZBNKDIAG
017000 COPY CABENDD.                                                    ZBNKDIAG
017100                                                                  ZBNKDIAG
017200 LINKAGE SECTION.                                                 ZBNKDIAG
017300 01  LK-EXEC-PARM.                                                ZBNKDIAG
017400   05  LK-EXEC-PARM-LL                       PIC S9(4) COMP.      ZBNKDIAG
017500   05  LK-EXEC-PARM-DATA                     PIC X(12).           ZBNKDIAG
017600                                                                  ZBNKDIAG
017700 PROCEDURE DIVISION USING LK-EXEC-PARM.                           ZBNKDIAG
017800                                                                  ZBNKDIAG
017900* The next line has an error - perform should be PERFORM          ZBNKDIAG
018000     PERFORM RUN-TIME.                                            ZBNKDIAG
018100                                                                  ZBNKDIAG
018200     MOVE SPACES TO WS-EXEC-PARM.                                 ZBNKDIAG
018300     IF LK-EXEC-PARM-LL IS EQUAL TO ZERO                          ZBNKDIAG
018400        MOVE 'No exec card parm present'                          ZBNKDIAG
018500          TO WS-CONSOLE-MESSAGE                                   ZBNKDIAG
018600        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKDIAG
018700     ELSE                                                         ZBNKDIAG
018800       MOVE LK-EXEC-PARM-DATA (1:LK-EXEC-PARM-LL) TO              ZBNKDIAG
018900         WS-EXEC-PARM                                             ZBNKDIAG
019000       MOVE SPACES TO WS-CONSOLE-MESSAGE                          ZBNKDIAG
019100       STRING 'Exec parm is "' DELIMITED BY SIZE                  ZBNKDIAG
019200              LK-EXEC-PARM-DATA (1:LK-EXEC-PARM-LL)               ZBNKDIAG
019300                DELIMITED BY SIZE                                 ZBNKDIAG
019400              '"' DELIMITED BY SIZE                               ZBNKDIAG
019500         INTO WS-CONSOLE-MESSAGE                                  ZBNKDIAG
019600       PERFORM DISPLAY-CONSOLE-MESSAGE                            ZBNKDIAG
019700     END-IF.                                                      ZBNKDIAG
019800                                                                  ZBNKDIAG
019900     ACCEPT WS-SYS-DATE FROM DAY.                                 ZBNKDIAG
020000     SET DD-ENV-NULL TO TRUE.                                     ZBNKDIAG
020100     SET DDI-YYDDD TO TRUE.                                       ZBNKDIAG
020200     MOVE WS-SYS-DATE TO DDI-DATA.                                ZBNKDIAG
020300     SET DDO-DD-MMM-YYYY TO TRUE.                                 ZBNKDIAG
020400     CALL 'UDATECNV' USING WS-DATE-WORK-AREA.                     ZBNKDIAG
020500     MOVE DDO-DATA TO WS-PRINT-DATE.                              ZBNKDIAG
020600                                                                  ZBNKDIAG
020700     PERFORM EXTRACT-OPEN.                                        ZBNKDIAG
020800     PERFORM PRINTOUT-OPEN.                                       ZBNKDIAG
020900                                                                  ZBNKDIAG
021000     PERFORM UNTIL WS-END-OF-FILE = 'YES'                         ZBNKDIAG
021100       IF WS-END-OF-FILE = 'NO '                                  ZBNKDIAG
021200          PERFORM EXTRACT-GET                                     ZBNKDIAG
021300          IF WS-END-OF-FILE = 'NO '                               ZBNKDIAG
021400             ADD 1 TO WS-RECORDS-READ                             ZBNKDIAG
021500             IF WS-RECORDS-READ IS LESS THAN 6                    ZBNKDIAG
021600                DISPLAY BANKXT01-REC1 UPON CONSOLE                ZBNKDIAG
021700             ELSE                                                 ZBNKDIAG
021800                IF WS-RECORDS-READ IS EQUAL TO 6                  ZBNKDIAG
021900                   MOVE 'Suppressing record display...'           ZBNKDIAG
022000                      TO WS-CONSOLE-MESSAGE                       ZBNKDIAG
022100                   PERFORM DISPLAY-CONSOLE-MESSAGE                ZBNKDIAG
022200                END-IF                                            ZBNKDIAG
022300             END-IF                                               ZBNKDIAG
022400             PERFORM FORMAT-AND-PRINT                             ZBNKDIAG
022500          ELSE                                                    ZBNKDIAG
022600             PERFORM PRINT-TOTAL-TXNS                             ZBNKDIAG
022700             PERFORM PRINT-TOTAL-ASSETS                           ZBNKDIAG
022800          END-IF                                                  ZBNKDIAG
022900       END-IF                                                     ZBNKDIAG
023000     END-PERFORM.                                                 ZBNKDIAG
023100                                                                  ZBNKDIAG
023200     PERFORM EXTRACT-CLOSE.                                       ZBNKDIAG
023300     PERFORM PRINTOUT-CLOSE.                                      ZBNKDIAG
023400                                                                  ZBNKDIAG
023500     PERFORM DISPLAY-CONSOLE-MESSAGE.                             ZBNKDIAG
023600     MOVE 'End Of Job'                                            ZBNKDIAG
023700       TO WS-CONSOLE-MESSAGE.                                     ZBNKDIAG
023800     PERFORM DISPLAY-CONSOLE-MESSAGE.                             ZBNKDIAG
023900                                                                  ZBNKDIAG
024000     PERFORM RUN-TIME.                                            ZBNKDIAG
024100                                                                  ZBNKDIAG
024200     MOVE 0 TO RETURN-CODE.                                       ZBNKDIAG
024300                                                                  ZBNKDIAG
024400     GOBACK.                                                      ZBNKDIAG
024500                                                                  ZBNKDIAG
024600***************************************************************** ZBNKDIAG
024700* Format print lines                                            * ZBNKDIAG
024800***************************************************************** ZBNKDIAG
024900 FORMAT-AND-PRINT.                                                ZBNKDIAG
025000     IF BANKXT01-1-TYPE IS EQUAL TO '1'                           ZBNKDIAG
025100        PERFORM PRINT-TOTAL-TXNS                                  ZBNKDIAG
025200        PERFORM PRINT-TOTAL-ASSETS                                ZBNKDIAG
025300        MOVE WS-LINE1 TO PRINTOUT-REC                             ZBNKDIAG
025400        PERFORM PRINTOUT-PUT                                      ZBNKDIAG
025500        MOVE WS-LINE2 TO PRINTOUT-REC                             ZBNKDIAG
025600        PERFORM PRINTOUT-PUT                                      ZBNKDIAG
025700        MOVE '0' TO WS-LINE3-CC                                   ZBNKDIAG
025800        MOVE BANKXT01-1-NAME TO WS-LINE3-NAME-ADDR                ZBNKDIAG
025900        MOVE WS-PRINTED TO WS-LINE3-DATE                          ZBNKDIAG
026000        MOVE WS-LINE3 TO PRINTOUT-REC                             ZBNKDIAG
026100        PERFORM PRINTOUT-PUT                                      ZBNKDIAG
026200        MOVE ' ' TO WS-LINE3-CC                                   ZBNKDIAG
026300        MOVE BANKXT01-1-ADDR1 TO WS-LINE3-NAME-ADDR               ZBNKDIAG
026400        MOVE SPACES TO WS-LINE3-DATE                              ZBNKDIAG
026500        MOVE WS-LINE3 TO PRINTOUT-REC                             ZBNKDIAG
026600        PERFORM PRINTOUT-PUT                                      ZBNKDIAG
026700        MOVE ' ' TO WS-LINE3-CC                                   ZBNKDIAG
026800        MOVE BANKXT01-1-ADDR2 TO WS-LINE3-NAME-ADDR               ZBNKDIAG
026900        MOVE SPACES TO WS-LINE3-DATE                              ZBNKDIAG
027000        MOVE WS-LINE3 TO PRINTOUT-REC                             ZBNKDIAG
027100        PERFORM PRINTOUT-PUT                                      ZBNKDIAG
027200        MOVE ' ' TO WS-LINE3-CC                                   ZBNKDIAG
027300        MOVE BANKXT01-1-STATE TO WS-LINE3-NAME-ADDR               ZBNKDIAG
027400        MOVE SPACES TO WS-LINE3-DATE                              ZBNKDIAG
027500        MOVE WS-LINE3 TO PRINTOUT-REC                             ZBNKDIAG
027600        PERFORM PRINTOUT-PUT                                      ZBNKDIAG
027700        MOVE ' ' TO WS-LINE3-CC                                   ZBNKDIAG
027800        MOVE BANKXT01-1-CNTRY TO WS-LINE3-NAME-ADDR               ZBNKDIAG
027900        MOVE SPACES TO WS-LINE3-DATE                              ZBNKDIAG
028000        MOVE WS-LINE3 TO PRINTOUT-REC                             ZBNKDIAG
028100        PERFORM PRINTOUT-PUT                                      ZBNKDIAG
028200        MOVE ' ' TO WS-LINE3-CC                                   ZBNKDIAG
028300        MOVE BANKXT01-1-PST-CDE TO WS-LINE3-NAME-ADDR             ZBNKDIAG
028400        MOVE SPACES TO WS-LINE3-DATE                              ZBNKDIAG
028500        MOVE WS-LINE3 TO PRINTOUT-REC                             ZBNKDIAG
028600        PERFORM PRINTOUT-PUT                                      ZBNKDIAG
028700        MOVE WS-LINE4 TO PRINTOUT-REC                             ZBNKDIAG
028800        PERFORM PRINTOUT-PUT                                      ZBNKDIAG
028900        MOVE ZERO TO WS-TOTAL-TXNS                                ZBNKDIAG
029000        MOVE ZERO TO WS-TOTAL-ASSETS                              ZBNKDIAG
029100     END-IF.                                                      ZBNKDIAG
029200     IF BANKXT01-2-TYPE IS EQUAL TO '2'                           ZBNKDIAG
029300        PERFORM PRINT-TOTAL-TXNS                                  ZBNKDIAG
029400        MOVE SPACES TO WS-LINE5                                   ZBNKDIAG
029500        MOVE BANKXT01-2-ACC-NO TO WS-LINE5-ACC-NO                 ZBNKDIAG
029600        MOVE 'Last statement' TO WS-LINE5-DESC-PT1                ZBNKDIAG
029700        MOVE BANKXT01-2-ACC-DESC TO WS-LINE5-DESC-PT2             ZBNKDIAG
029800        MOVE BANKXT01-2-ACC-LAST-STMT-DTE TO DDI-DATA             ZBNKDIAG
029900        SET DD-ENV-NULL TO TRUE                                   ZBNKDIAG
030000        SET DDI-ISO TO TRUE                                       ZBNKDIAG
030100        SET DDO-DD-MMM-YYYY TO TRUE                               ZBNKDIAG
030200        CALL 'UDATECNV' USING WS-DATE-WORK-AREA                   ZBNKDIAG
030300        MOVE DDO-DATA TO WS-LINE5-DATE                            ZBNKDIAG
030400        MOVE BANKXT01-2-ACC-CURR-BAL TO WS-LINE5-BALANCE          ZBNKDIAG
030500        ADD BANKXT01-2-ACC-CURR-BAL TO WS-TOTAL-ASSETS            ZBNKDIAG
030600        MOVE WS-LINE5 TO PRINTOUT-REC                             ZBNKDIAG
030700        PERFORM PRINTOUT-PUT                                      ZBNKDIAG
030800     END-IF.                                                      ZBNKDIAG
030900     IF BANKXT01-3-TYPE IS EQUAL TO '3'                           ZBNKDIAG
031000        MOVE SPACES TO WS-LINE5                                   ZBNKDIAG
031100        MOVE BANKXT01-3-DESC TO WS-LINE5-DESC (4:30)              ZBNKDIAG
031200        MOVE BANKXT01-3-TIMESTAMP (1:10) TO DDI-DATA              ZBNKDIAG
031300        SET DD-ENV-NULL TO TRUE                                   ZBNKDIAG
031400        SET DDI-ISO TO TRUE                                       ZBNKDIAG
031500        SET DDO-DD-MMM-YYYY TO TRUE                               ZBNKDIAG
031600        CALL 'UDATECNV' USING WS-DATE-WORK-AREA                   ZBNKDIAG
031700        MOVE DDO-DATA TO WS-LINE5-DATE                            ZBNKDIAG
031800        MOVE BANKXT01-3-AMOUNT TO WS-LINE5-AMOUNT                 ZBNKDIAG
031900        ADD BANKXT01-3-AMOUNT TO WS-TOTAL-TXNS                    ZBNKDIAG
032000        SET TXNS-PRINTED TO TRUE                                  ZBNKDIAG
032100        MOVE WS-LINE5 TO PRINTOUT-REC                             ZBNKDIAG
032200        PERFORM PRINTOUT-PUT                                      ZBNKDIAG
032300     END-IF.                                                      ZBNKDIAG
032400                                                                  ZBNKDIAG
032500***************************************************************** ZBNKDIAG
032600* Format and print transaction totals                           * ZBNKDIAG
032700***************************************************************** ZBNKDIAG
032800 PRINT-TOTAL-TXNS.                                                ZBNKDIAG
032900     IF TXNS-PRINTED                                              ZBNKDIAG
033000        MOVE SPACES TO WS-LINE5                                   ZBNKDIAG
033100        MOVE '------------' TO WS-LINE5-AMOUNT-DASH               ZBNKDIAG
033200        MOVE WS-LINE5 TO PRINTOUT-REC                             ZBNKDIAG
033300        PERFORM PRINTOUT-PUT                                      ZBNKDIAG
033400        MOVE SPACES TO WS-LINE5-DESC                              ZBNKDIAG
033500        MOVE 'Total transactions' TO WS-LINE5-DESC (4:30)         ZBNKDIAG
033600        MOVE WS-TOTAL-TXNS TO WS-LINE5-AMOUNT                     ZBNKDIAG
033700        MOVE ZERO TO WS-TOTAL-TXNS                                ZBNKDIAG
033800        SET NO-TXNS-PRINTED TO TRUE                               ZBNKDIAG
033900        MOVE WS-LINE5 TO PRINTOUT-REC                             ZBNKDIAG
034000        PERFORM PRINTOUT-PUT                                      ZBNKDIAG
034100     END-IF.                                                      ZBNKDIAG
034200                                                                  ZBNKDIAG
034300                                                                  ZBNKDIAG
034400***************************************************************** ZBNKDIAG
034500* Format and print "page" totals                                * ZBNKDIAG
034600***************************************************************** ZBNKDIAG
034700 PRINT-TOTAL-ASSETS.                                              ZBNKDIAG
034800     IF WS-FIRST-REC IS EQUAL TO 'YES'                            ZBNKDIAG
034900        MOVE 'NO' TO WS-FIRST-REC                                 ZBNKDIAG
035000        SET NO-TXNS-PRINTED TO TRUE                               ZBNKDIAG
035100     ELSE                                                         ZBNKDIAG
035200        MOVE SPACES TO WS-LINE5                                   ZBNKDIAG
035300        MOVE '------------' TO WS-LINE5-BALANCE-DASH              ZBNKDIAG
035400        MOVE WS-LINE5 TO PRINTOUT-REC                             ZBNKDIAG
035500        PERFORM PRINTOUT-PUT                                      ZBNKDIAG
035600        MOVE SPACES TO WS-LINE5                                   ZBNKDIAG
035700        MOVE 'Total Assets' TO WS-LINE5-DESC                      ZBNKDIAG
035800        MOVE WS-TOTAL-ASSETS TO WS-LINE5-BALANCE                  ZBNKDIAG
035900        MOVE WS-LINE5 TO PRINTOUT-REC                             ZBNKDIAG
036000        PERFORM PRINTOUT-PUT                                      ZBNKDIAG
036100     END-IF.                                                      ZBNKDIAG
036200                                                                  ZBNKDIAG
036300***************************************************************** ZBNKDIAG
036400* Open the EXTRACTed data file                                 *  ZBNKDIAG
036500***************************************************************** ZBNKDIAG
036600 EXTRACT-OPEN.                                                    ZBNKDIAG
036700     OPEN INPUT EXTRACT-FILE.                                     ZBNKDIAG
036800     IF WS-EXTRACT-STATUS = '00'                                  ZBNKDIAG
036900        MOVE 'EXTRACT file opened OK'                             ZBNKDIAG
037000          TO WS-CONSOLE-MESSAGE                                   ZBNKDIAG
037100        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKDIAG
037200     ELSE                                                         ZBNKDIAG
037300        MOVE 'EXTRACT file open failure...'                       ZBNKDIAG
037400          TO WS-CONSOLE-MESSAGE                                   ZBNKDIAG
037500        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKDIAG
037600        MOVE WS-EXTRACT-STATUS TO WS-IO-STATUS                    ZBNKDIAG
037700        PERFORM DISPLAY-IO-STATUS                                 ZBNKDIAG
037800        PERFORM ABORT-PROGRAM                                     ZBNKDIAG
037900        END-IF.                                                   ZBNKDIAG
038000                                                                  ZBNKDIAG
038100***************************************************************** ZBNKDIAG
038200* Read a record from the EXTRACTed data file                    * ZBNKDIAG
038300***************************************************************** ZBNKDIAG
038400 EXTRACT-GET.                                                     ZBNKDIAG
038500     READ EXTRACT-FILE.                                           ZBNKDIAG
038600     IF WS-EXTRACT-STATUS NOT = '00'                              ZBNKDIAG
038700        IF WS-EXTRACT-STATUS = '10'                               ZBNKDIAG
038800           MOVE 'YES' TO WS-END-OF-FILE                           ZBNKDIAG
038900        ELSE                                                      ZBNKDIAG
039000           MOVE 'EXTRACT Error readng file ...'                   ZBNKDIAG
039100             TO WS-CONSOLE-MESSAGE                                ZBNKDIAG
039200            PERFORM DISPLAY-CONSOLE-MESSAGE                       ZBNKDIAG
039300            MOVE WS-EXTRACT-STATUS TO WS-IO-STATUS                ZBNKDIAG
039400            PERFORM DISPLAY-IO-STATUS                             ZBNKDIAG
039500            PERFORM ABORT-PROGRAM                                 ZBNKDIAG
039600        END-IF                                                    ZBNKDIAG
039700     END-IF.                                                      ZBNKDIAG
039800                                                                  ZBNKDIAG
039900***************************************************************** ZBNKDIAG
040000* Close the EXTRACTed data file                                 * ZBNKDIAG
040100***************************************************************** ZBNKDIAG
040200 EXTRACT-CLOSE.                                                   ZBNKDIAG
040300     CLOSE EXTRACT-FILE.                                          ZBNKDIAG
040400     IF WS-EXTRACT-STATUS = '00'                                  ZBNKDIAG
040500        MOVE 'EXTRACT file closed OK'                             ZBNKDIAG
040600          TO WS-CONSOLE-MESSAGE                                   ZBNKDIAG
040700        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKDIAG
040800     ELSE                                                         ZBNKDIAG
040900        MOVE 'EXTRACT file close failure...'                      ZBNKDIAG
041000          TO WS-CONSOLE-MESSAGE                                   ZBNKDIAG
041100        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKDIAG
041200        MOVE WS-EXTRACT-STATUS TO WS-IO-STATUS                    ZBNKDIAG
041300        PERFORM DISPLAY-IO-STATUS                                 ZBNKDIAG
041400        PERFORM ABORT-PROGRAM                                     ZBNKDIAG
041500     END-IF.                                                      ZBNKDIAG
041600                                                                  ZBNKDIAG
041700***************************************************************** ZBNKDIAG
041800* Open the seqential print file                                 * ZBNKDIAG
041900***************************************************************** ZBNKDIAG
042000 PRINTOUT-OPEN.                                                   ZBNKDIAG
042100     OPEN OUTPUT PRINTOUT-FILE.                                   ZBNKDIAG
042200     IF WS-PRINTOUT-STATUS = '00'                                 ZBNKDIAG
042300        MOVE 'PRINTOUT file opened OK'                            ZBNKDIAG
042400          TO WS-CONSOLE-MESSAGE                                   ZBNKDIAG
042500        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKDIAG
042600     ELSE                                                         ZBNKDIAG
042700        MOVE 'PRINTOUT file open failure...'                      ZBNKDIAG
042800          TO WS-CONSOLE-MESSAGE                                   ZBNKDIAG
042900        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKDIAG
043000        MOVE WS-PRINTOUT-STATUS TO WS-IO-STATUS                   ZBNKDIAG
043100        PERFORM DISPLAY-IO-STATUS                                 ZBNKDIAG
043200        PERFORM ABORT-PROGRAM                                     ZBNKDIAG
043300        END-IF.                                                   ZBNKDIAG
043400                                                                  ZBNKDIAG
043500***************************************************************** ZBNKDIAG
043600* Write a record to the squential file                          * ZBNKDIAG
043700***************************************************************** ZBNKDIAG
043800 PRINTOUT-PUT.                                                    ZBNKDIAG
043900     WRITE PRINTOUT-REC..                                         ZBNKDIAG
044000     IF WS-PRINTOUT-STATUS NOT = '00'                             ZBNKDIAG
044100        MOVE 'PRINTOUT Error Writing file ...'                    ZBNKDIAG
044200          TO WS-CONSOLE-MESSAGE                                   ZBNKDIAG
044300        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKDIAG
044400        MOVE WS-PRINTOUT-STATUS TO WS-IO-STATUS                   ZBNKDIAG
044500        PERFORM DISPLAY-IO-STATUS                                 ZBNKDIAG
044600        PERFORM ABORT-PROGRAM                                     ZBNKDIAG
044700     END-IF.                                                      ZBNKDIAG
044800                                                                  ZBNKDIAG
044900***************************************************************** ZBNKDIAG
045000* Close the seqential print file                                * ZBNKDIAG
045100***************************************************************** ZBNKDIAG
045200 PRINTOUT-CLOSE.                                                  ZBNKDIAG
045300     CLOSE PRINTOUT-FILE.                                         ZBNKDIAG
045400     IF WS-PRINTOUT-STATUS = '00'                                 ZBNKDIAG
045500        MOVE 'PRINTOUT file closed OK'                            ZBNKDIAG
045600          TO WS-CONSOLE-MESSAGE                                   ZBNKDIAG
045700        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKDIAG
045800     ELSE                                                         ZBNKDIAG
045900        MOVE 'PRINTOUT file close failure...'                     ZBNKDIAG
046000          TO WS-CONSOLE-MESSAGE                                   ZBNKDIAG
046100        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKDIAG
046200        MOVE WS-PRINTOUT-STATUS TO WS-IO-STATUS                   ZBNKDIAG
046300        PERFORM DISPLAY-IO-STATUS                                 ZBNKDIAG
046400        PERFORM ABORT-PROGRAM                                     ZBNKDIAG
046500     END-IF.                                                      ZBNKDIAG
046600                                                                  ZBNKDIAG
046700***************************************************************** ZBNKDIAG
046800* Display the file status bytes. This routine will display as   * ZBNKDIAG
046900* two digits if the full two byte file status is numeric. If    * ZBNKDIAG
047000* second byte is non-numeric then it will be treated as a       * ZBNKDIAG
047100* binary number.                                                * ZBNKDIAG
047200***************************************************************** ZBNKDIAG
047300 DISPLAY-IO-STATUS.                                               ZBNKDIAG
047400     IF WS-IO-STATUS NUMERIC                                      ZBNKDIAG
047500        MOVE SPACE TO WS-CONSOLE-MESSAGE                          ZBNKDIAG
047600        STRING 'File status -' DELIMITED BY SIZE                  ZBNKDIAG
047700               WS-IO-STATUS DELIMITED BY SIZE                     ZBNKDIAG
047800          INTO WS-CONSOLE-MESSAGE                                 ZBNKDIAG
047900        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKDIAG
048000     ELSE                                                         ZBNKDIAG
048100        SUBTRACT WS-TWO-BYTES-BINARY FROM WS-TWO-BYTES-BINARY     ZBNKDIAG
048200        MOVE WS-IO-STAT2 TO WS-TWO-BYTES-RIGHT                    ZBNKDIAG
048300        MOVE SPACE TO WS-CONSOLE-MESSAGE                          ZBNKDIAG
048400        STRING 'File status -' DELIMITED BY SIZE                  ZBNKDIAG
048500               WS-IO-STAT1 DELIMITED BY SIZE                      ZBNKDIAG
048600               '/' DELIMITED BY SIZE                              ZBNKDIAG
048700               WS-TWO-BYTES DELIMITED BY SIZE                     ZBNKDIAG
048800          INTO WS-CONSOLE-MESSAGE                                 ZBNKDIAG
048900        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKDIAG
049000     END-IF.                                                      ZBNKDIAG
049100                                                                  ZBNKDIAG
049200***************************************************************** ZBNKDIAG
049300* 'ABORT' the program.                                          * ZBNKDIAG
049400* Post a message to the console and issue a STOP RUN            * ZBNKDIAG
049500***************************************************************** ZBNKDIAG
049600 ABORT-PROGRAM.                                                   ZBNKDIAG
049700     IF WS-CONSOLE-MESSAGE NOT = SPACES                           ZBNKDIAG
049800        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKDIAG
049900     END-IF.                                                      ZBNKDIAG
050000     MOVE 'Program is abending...'  TO WS-CONSOLE-MESSAGE.        ZBNKDIAG
050100     PERFORM DISPLAY-CONSOLE-MESSAGE.                             ZBNKDIAG
050200     MOVE 16 TO RETURN-CODE.                                      ZBNKDIAG
050300     STOP RUN.                                                    ZBNKDIAG
050400                                                                  ZBNKDIAG
050500***************************************************************** ZBNKDIAG
050600* Display CONSOLE messages...                                   * ZBNKDIAG
050700***************************************************************** ZBNKDIAG
050800 DISPLAY-CONSOLE-MESSAGE.                                         ZBNKDIAG
050900     DISPLAY 'ZPRNT03 - ' WS-CONSOLE-MESSAGE                      ZBNKDIAG
051000       UPON CONSOLE.                                              ZBNKDIAG
051100     MOVE ALL SPACES TO WS-CONSOLE-MESSAGE.                       ZBNKDIAG
051200                                                                  ZBNKDIAG
051300 COPY CTIMERP.                                                    ZBNKDIAG
051400                                                                  ZBNKDIAG
051500* $ Version 5.98b sequenced on Thursday 5 Feb 2009 at 11:00pm     ZBNKDIAG
