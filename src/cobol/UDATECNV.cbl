000100***************************************************************** UDATECNV
000200*                                                               * UDATECNV
000300*   Copyright (C) 1998-2008 Micro Focus. All Rights Reserved.   * UDATECNV
000400*   This demonstration program is provided for use by users     * UDATECNV
000500*   of Micro Focus products and may be used, modified and       * UDATECNV
000600*   distributed as part of your application provided that       * UDATECNV
000700*   you properly acknowledge the copyright of Micro Focus       * UDATECNV
000800*   in this material.                                           * UDATECNV
000900*                                                               * UDATECNV
001000***************************************************************** UDATECNV
001100                                                                  UDATECNV
001200***************************************************************** UDATECNV
001300* Program:     UDATECNV.CBL                                     * UDATECNV
001400* Function:    Date conversion utility routine                  * UDATECNV
001500***************************************************************** UDATECNV
001600                                                                  UDATECNV
001700 IDENTIFICATION DIVISION.                                         UDATECNV
001800 PROGRAM-ID.                                                      UDATECNV
001900     UDATECNV.                                                    UDATECNV
002000 DATE-WRITTEN.                                                    UDATECNV
002100     September 2002.                                              UDATECNV
002200 DATE-COMPILED.                                                   UDATECNV
002300     Today.                                                       UDATECNV
002400                                                                  UDATECNV
002500 ENVIRONMENT DIVISION.                                            UDATECNV
002600                                                                  UDATECNV
002700 DATA DIVISION.                                                   UDATECNV
002800 WORKING-STORAGE SECTION.                                         UDATECNV
002900 01  WS-MISC-STORAGE.                                             UDATECNV
003000   05  comp5    pic x(2) comp-5.                                  UDATECNV
003100   05  WS-PROGRAM-ID                         PIC X(8)             UDATECNV
003200       VALUE 'UDATECNV'.                                          UDATECNV
003300   05  WS-SAVED-DDI-DATA                     PIC X(20).           UDATECNV
003400   05  WS-MONTH-TABLE.                                            UDATECNV
003500     10  FILLER          VALUE 'Jan'         PIC X(3).            UDATECNV
003600     10  FILLER          VALUE 'Feb'         PIC X(3).            UDATECNV
003700     10  FILLER          VALUE 'Mar'         PIC X(3).            UDATECNV
003800     10  FILLER          VALUE 'Apr'         PIC X(3).            UDATECNV
003900     10  FILLER          VALUE 'May'         PIC X(3).            UDATECNV
004000     10  FILLER          VALUE 'Jun'         PIC X(3).            UDATECNV
004100     10  FILLER          VALUE 'Jul'         PIC X(3).            UDATECNV
004200     10  FILLER          VALUE 'Aug'         PIC X(3).            UDATECNV
004300     10  FILLER          VALUE 'Sep'         PIC X(3).            UDATECNV
004400     10  FILLER          VALUE 'Oct'         PIC X(3).            UDATECNV
004500     10  FILLER          VALUE 'Nov'         PIC X(3).            UDATECNV
004600     10  FILLER          VALUE 'Dec'         PIC X(3).            UDATECNV
004700   05  WS-MONTH-TABLE-R REDEFINES WS-MONTH-TABLE.                 UDATECNV
004800     10  WS-MONTH                            PIC X(3)             UDATECNV
004900         OCCURS 12 TIMES.                                         UDATECNV
005000   05  WS-DAYS-TABLE.                                             UDATECNV
005100     10  WS-DAYS-IN-JAN  VALUE 031           PIC 9(3).            UDATECNV
005200     10  WS-DAYS-IN-FEB  VALUE 028           PIC 9(3).            UDATECNV
005300     10  WS-DAYS-IN-MAR  VALUE 031           PIC 9(3).            UDATECNV
005400     10  WS-DAYS-IN-APR  VALUE 030           PIC 9(3).            UDATECNV
005500     10  WS-DAYS-IN-MAY  VALUE 031           PIC 9(3).            UDATECNV
005600     10  WS-DAYS-IN-JUN  VALUE 030           PIC 9(3).            UDATECNV
005700     10  WS-DAYS-IN-JUL  VALUE 031           PIC 9(3).            UDATECNV
005800     10  WS-DAYS-IN-AUG  VALUE 031           PIC 9(3).            UDATECNV
005900     10  WS-DAYS-IN-SEP  VALUE 030           PIC 9(3).            UDATECNV
006000     10  WS-DAYS-IN-OCT  VALUE 031           PIC 9(3).            UDATECNV
006100     10  WS-DAYS-IN-NOV  VALUE 030           PIC 9(3).            UDATECNV
006200     10  WS-DAYS-IN-DEV  VALUE 031           PIC 9(3).            UDATECNV
006300   05  WS-DAYS-TABLE-R REDEFINES WS-DAYS-TABLE.                   UDATECNV
006400     10  WS-DAYS-IN-MONTH                    PIC 9(3)             UDATECNV
006500         OCCURS 12 TIMES.                                         UDATECNV
006600   05  WS-DAYS                               PIC 9(3).            UDATECNV
006700   05  WS-DAYS-R REDEFINES WS-DAYS.                               UDATECNV
006800     10  FILLER                              PIC X(1).            UDATECNV
006900     10  WS-DAY-OF-MONTH                     PIC X(2).            UDATECNV
007000   05  WS-WORK-MM                            PIC 9(2).            UDATECNV
007100   05  WS-WORK-DD                            PIC 9(2).            UDATECNV
007200   05  WS-TEMP                               PIC 9(2).            UDATECNV
007300   05  WS-SYSTEM-TIME                        PIC 9(8).            UDATECNV
007400   05  WS-SYSTEM-TIME-R REDEFINES WS-SYSTEM-TIME.                 UDATECNV
007500     10  WS-SYSTEM-TIME-HHMMSS               PIC 9(6).            UDATECNV
007600     10  WS-SYSTEM-TIME-DD                   PIC 9(2).            UDATECNV
007700   05  WS-WORK-TIME                          PIC 9(6).            UDATECNV
007800     88  WORK-TIME-INIT                      VALUE 987654.        UDATECNV
007900   05  WS-WORK-TIME-R REDEFINES WS-WORK-TIME.                     UDATECNV
008000     10  WS-WORK-TIME-HH                     PIC X(2).            UDATECNV
008100     10  WS-WORK-TIME-MM                     PIC X(2).            UDATECNV
008200     10  WS-WORK-TIME-SS                     PIC X(2).            UDATECNV
008300                                                                  UDATECNV
008400 COPY CABENDD.                                                    UDATECNV
008500                                                                  UDATECNV
008600 LINKAGE SECTION.                                                 UDATECNV
008700 01  LK-DATE-WORK-AREA.                                           UDATECNV
008800 COPY CDATED.                                                     UDATECNV
008900                                                                  UDATECNV
009000 PROCEDURE DIVISION USING LK-DATE-WORK-AREA.                      UDATECNV
009100     PERFORM TIME-CONVERT THRU                                    UDATECNV
009200             TIME-CONVERT-EXIT.                                   UDATECNV
009300     PERFORM DATE-CONVERT THRU                                    UDATECNV
009400             DATE-CONVERT-EXIT.                                   UDATECNV
009500     GOBACK.                                                      UDATECNV
009600                                                                  UDATECNV
009700 TIME-CONVERT.                                                    UDATECNV
009800     SET WORK-TIME-INIT TO TRUE.                                  UDATECNV
009900     MOVE SPACES TO DD-TIME-OUTPUT.                               UDATECNV
010000                                                                  UDATECNV
010100     IF DD-TIME-INPUT IS NOT NUMERIC                              UDATECNV
010200        GO TO TIME-CONVERT-ERROR                                  UDATECNV
010300     END-IF.                                                      UDATECNV
010400     IF DD-ENV-CICS                                               UDATECNV
010500        MOVE DD-TIME-INPUT-N TO WS-WORK-TIME                      UDATECNV
010600     END-IF.                                                      UDATECNV
010700     IF DD-ENV-IMS                                                UDATECNV
010800        DIVIDE 10 INTO DD-TIME-INPUT-N GIVING WS-WORK-TIME        UDATECNV
010900     END-IF.                                                      UDATECNV
011000     IF DD-ENV-NULL OR                                            UDATECNV
011100        DD-ENV-INET                                               UDATECNV
011200        ACCEPT WS-SYSTEM-TIME FROM TIME                           UDATECNV
011300        MOVE WS-SYSTEM-TIME-HHMMSS TO WS-WORK-TIME                UDATECNV
011400     END-IF.                                                      UDATECNV
011500     IF WORK-TIME-INIT                                            UDATECNV
011600         GO TO TIME-CONVERT-ERROR                                 UDATECNV
011700     END-IF.                                                      UDATECNV
011800     MOVE WS-WORK-TIME-HH TO DD-TIME-OUTPUT-HH.                   UDATECNV
011900     MOVE ':'             TO DD-TIME-OUTPUT-SEP1.                 UDATECNV
012000     MOVE WS-WORK-TIME-MM TO DD-TIME-OUTPUT-MM.                   UDATECNV
012100     MOVE ':'             TO DD-TIME-OUTPUT-SEP2.                 UDATECNV
012200     MOVE WS-WORK-TIME-SS TO DD-TIME-OUTPUT-SS.                   UDATECNV
012300     GO TO TIME-CONVERT-EXIT.                                     UDATECNV
012400                                                                  UDATECNV
012500 TIME-CONVERT-ERROR.                                              UDATECNV
012600     MOVE 'hh:mm:ss' TO DD-TIME-OUTPUT.                           UDATECNV
012700 TIME-CONVERT-EXIT.                                               UDATECNV
012800     EXIT.                                                        UDATECNV
012900                                                                  UDATECNV
013000 DATE-CONVERT.                                                    UDATECNV
013100     MOVE SPACES TO DDO-DATA.                                     UDATECNV
013200                                                                  UDATECNV
013300     IF NOT DDI-ISO AND                                           UDATECNV
013400        NOT DDI-YYYYMMDD AND                                      UDATECNV
013500        NOT DDI-YYMMDD AND                                        UDATECNV
013600        NOT DDI-YYDDD                                             UDATECNV
013700        MOVE 'ERROR1' TO DDO-DATA                                 UDATECNV
013800        GO TO DATE-CONVERT-EXIT                                   UDATECNV
013900     END-IF.                                                      UDATECNV
014000     MOVE DDI-DATA TO WS-SAVED-DDI-DATA.                          UDATECNV
014100     IF DDI-ISO                                                   UDATECNV
014200        PERFORM DATE-CONVERT-IP-OPT1 THRU                         UDATECNV
014300                DATE-CONVERT-IP-OPT1-EXIT                         UDATECNV
014400     END-IF.                                                      UDATECNV
014500     IF DDI-YYYYMMDD                                              UDATECNV
014600        PERFORM DATE-CONVERT-IP-OPT2 THRU                         UDATECNV
014700                DATE-CONVERT-IP-OPT2-EXIT                         UDATECNV
014800     END-IF.                                                      UDATECNV
014900     IF DDI-YYMMDD                                                UDATECNV
015000        PERFORM DATE-CONVERT-IP-OPT3 THRU                         UDATECNV
015100                DATE-CONVERT-IP-OPT3-EXIT                         UDATECNV
015200     END-IF.                                                      UDATECNV
015300     IF DDI-YYDDD                                                 UDATECNV
015400        PERFORM DATE-CONVERT-IP-OPT4 THRU                         UDATECNV
015500                DATE-CONVERT-IP-OPT4-EXIT                         UDATECNV
015600     END-IF.                                                      UDATECNV
015700 DATE-CONVERT-EXIT.                                               UDATECNV
015800     EXIT.                                                        UDATECNV
015900                                                                  UDATECNV
016000* Input option1 - input is ISO (yyyy-mm-dd)                       UDATECNV
016100 DATE-CONVERT-IP-OPT1.                                            UDATECNV
016200     EVALUATE TRUE                                                UDATECNV
016300       WHEN DDO-DD-MMM-YY                                         UDATECNV
016400         MOVE DDI-DATA-ISO-DD TO                                  UDATECNV
016500              DDO-DATA-DD-MMM-YY-DD                               UDATECNV
016600         MOVE '.' TO                                              UDATECNV
016700              DDO-DATA-DD-MMM-YY-DOT1                             UDATECNV
016800         MOVE WS-MONTH (DDI-DATA-ISO-MM-N) TO                     UDATECNV
016900              DDO-DATA-DD-MMM-YY-MMM                              UDATECNV
017000         MOVE '.' TO                                              UDATECNV
017100              DDO-DATA-DD-MMM-YY-DOT2                             UDATECNV
017200         MOVE DDI-DATA-ISO-YYYY (3:2) TO                          UDATECNV
017300              DDO-DATA-DD-MMM-YY-YY                               UDATECNV
017400       WHEN DDO-DD-MMM-YYYY                                       UDATECNV
017500         MOVE DDI-DATA-ISO-DD TO                                  UDATECNV
017600              DDO-DATA-DD-MMM-YYYY-DD                             UDATECNV
017700         MOVE '.' TO                                              UDATECNV
017800              DDO-DATA-DD-MMM-YYYY-DOT1                           UDATECNV
017900         MOVE WS-MONTH (DDI-DATA-ISO-MM-N) TO                     UDATECNV
018000              DDO-DATA-DD-MMM-YYYY-MMM                            UDATECNV
018100         MOVE '.' TO                                              UDATECNV
018200              DDO-DATA-DD-MMM-YYYY-DOT2                           UDATECNV
018300         MOVE DDI-DATA-ISO-YYYY TO                                UDATECNV
018400              DDO-DATA-DD-MMM-YYYY-YYYY                           UDATECNV
018500       WHEN OTHER                                                 UDATECNV
018600         MOVE 'ERROR2' TO DDO-DATA                                UDATECNV
018700     END-EVALUATE.                                                UDATECNV
018800 DATE-CONVERT-IP-OPT1-EXIT.                                       UDATECNV
018900     EXIT.                                                        UDATECNV
019000                                                                  UDATECNV
019100* Input option2 - input is yyyymmdd                               UDATECNV
019200 DATE-CONVERT-IP-OPT2.                                            UDATECNV
019300     EVALUATE TRUE                                                UDATECNV
019400       WHEN DDO-DD-MMM-YY                                         UDATECNV
019500         MOVE DDI-DATA-YYYYMMDD-DD TO                             UDATECNV
019600              DDO-DATA-DD-MMM-YY-DD                               UDATECNV
019700         MOVE '.' TO                                              UDATECNV
019800              DDO-DATA-DD-MMM-YY-DOT1                             UDATECNV
019900         MOVE WS-MONTH (DDI-DATA-YYYYMMDD-MM-N) TO                UDATECNV
020000              DDO-DATA-DD-MMM-YY-MMM                              UDATECNV
020100         MOVE '.' TO                                              UDATECNV
020200              DDO-DATA-DD-MMM-YY-DOT2                             UDATECNV
020300         MOVE DDI-DATA-YYYYMMDD-YYYY (3:2) TO                     UDATECNV
020400              DDO-DATA-DD-MMM-YY-YY                               UDATECNV
020500       WHEN DDO-DD-MMM-YYYY                                       UDATECNV
020600         MOVE DDI-DATA-YYYYMMDD-DD TO                             UDATECNV
020700              DDO-DATA-DD-MMM-YYYY-DD                             UDATECNV
020800         MOVE '.' TO                                              UDATECNV
020900              DDO-DATA-DD-MMM-YYYY-DOT1                           UDATECNV
021000         MOVE WS-MONTH (DDI-DATA-YYYYMMDD-MM-N) TO                UDATECNV
021100              DDO-DATA-DD-MMM-YYYY-MMM                            UDATECNV
021200         MOVE '.' TO                                              UDATECNV
021300              DDO-DATA-DD-MMM-YYYY-DOT2                           UDATECNV
021400         MOVE DDI-DATA-YYYYMMDD-YYYY TO                           UDATECNV
021500              DDO-DATA-DD-MMM-YYYY-YYYY                           UDATECNV
021600       WHEN OTHER                                                 UDATECNV
021700         MOVE 'ERROR2' TO DDO-DATA                                UDATECNV
021800     END-EVALUATE.                                                UDATECNV
021900 DATE-CONVERT-IP-OPT2-EXIT.                                       UDATECNV
022000     EXIT.                                                        UDATECNV
022100                                                                  UDATECNV
022200* Input option3 - input is yymmdd                                 UDATECNV
022300 DATE-CONVERT-IP-OPT3.                                            UDATECNV
022400     EVALUATE TRUE                                                UDATECNV
022500       WHEN DDO-DD-MMM-YY                                         UDATECNV
022600         MOVE DDI-DATA-YYMMDD-DD TO                               UDATECNV
022700              DDO-DATA-DD-MMM-YY-DD                               UDATECNV
022800         MOVE '.' TO                                              UDATECNV
022900              DDO-DATA-DD-MMM-YY-DOT1                             UDATECNV
023000         MOVE WS-MONTH (DDI-DATA-YYMMDD-MM-N) TO                  UDATECNV
023100              DDO-DATA-DD-MMM-YY-MMM                              UDATECNV
023200         MOVE '.' TO                                              UDATECNV
023300              DDO-DATA-DD-MMM-YY-DOT2                             UDATECNV
023400         MOVE DDI-DATA-YYMMDD-YY TO                               UDATECNV
023500              DDO-DATA-DD-MMM-YY-YY                               UDATECNV
023600       WHEN DDO-DD-MMM-YYYY                                       UDATECNV
023700         MOVE DDI-DATA-YYMMDD-DD TO                               UDATECNV
023800              DDO-DATA-DD-MMM-YYYY-DD                             UDATECNV
023900         MOVE '.' TO                                              UDATECNV
024000              DDO-DATA-DD-MMM-YYYY-DOT1                           UDATECNV
024100         MOVE WS-MONTH (DDI-DATA-YYMMDD-MM-N) TO                  UDATECNV
024200              DDO-DATA-DD-MMM-YYYY-MMM                            UDATECNV
024300         MOVE '.' TO                                              UDATECNV
024400              DDO-DATA-DD-MMM-YYYY-DOT2                           UDATECNV
024500         MOVE DDI-DATA-YYMMDD-YY TO                               UDATECNV
024600              DDO-DATA-DD-MMM-YYYY-YYYY (3:2)                     UDATECNV
024700         IF DDI-DATA-YYMMDD-YY IS LESS THAN '50'                  UDATECNV
024800            MOVE '20' TO DDO-DATA-DD-MMM-YYYY-YYYY (1:2)          UDATECNV
024900         ELSE                                                     UDATECNV
025000            MOVE '19' TO DDO-DATA-DD-MMM-YYYY-YYYY (1:2)          UDATECNV
025100         END-IF                                                   UDATECNV
025200       WHEN OTHER                                                 UDATECNV
025300         MOVE 'ERROR2' TO DDO-DATA                                UDATECNV
025400     END-EVALUATE.                                                UDATECNV
025500 DATE-CONVERT-IP-OPT3-EXIT.                                       UDATECNV
025600     EXIT.                                                        UDATECNV
025700                                                                  UDATECNV
025800* Input option4 - input is yyddd                                  UDATECNV
025900 DATE-CONVERT-IP-OPT4.                                            UDATECNV
026000     DIVIDE 4 INTO DDI-DATA-YYDDD-YY-N                            UDATECNV
026100       GIVING WS-TEMP.                                            UDATECNV
026200     MULTIPLY WS-TEMP BY 4 GIVING WS-TEMP.                        UDATECNV
026300     IF WS-TEMP EQUAL TO DDI-DATA-YYDDD-YY-N                      UDATECNV
026400        MOVE 029 TO WS-DAYS-IN-MONTH(2)                           UDATECNV
026500     ELSE                                                         UDATECNV
026600        MOVE 028 TO WS-DAYS-IN-MONTH(2)                           UDATECNV
026700     END-IF.                                                      UDATECNV
026800     MOVE DDI-DATA-YYDDD-DDD TO WS-DAYS.                          UDATECNV
026900     MOVE 01 TO WS-WORK-MM.                                       UDATECNV
027000 DATE-CONVERT-IP-OPT4-DAYS.                                       UDATECNV
027100     IF WS-DAYS IS GREATER THAN WS-DAYS-IN-MONTH(WS-WORK-MM)      UDATECNV
027200        SUBTRACT WS-DAYS-IN-MONTH(WS-WORK-MM) FROM WS-DAYS        UDATECNV
027300        ADD 1 TO WS-WORK-MM                                       UDATECNV
027400        GO TO DATE-CONVERT-IP-OPT4-DAYS.                          UDATECNV
027500     EVALUATE TRUE                                                UDATECNV
027600       WHEN DDO-DD-MMM-YY                                         UDATECNV
027700         MOVE WS-DAY-OF-MONTH TO                                  UDATECNV
027800              DDO-DATA-DD-MMM-YY-DD                               UDATECNV
027900         MOVE '.' TO                                              UDATECNV
028000              DDO-DATA-DD-MMM-YY-DOT1                             UDATECNV
028100         MOVE WS-MONTH (WS-WORK-MM) TO                            UDATECNV
028200              DDO-DATA-DD-MMM-YY-MMM                              UDATECNV
028300         MOVE '.' TO                                              UDATECNV
028400              DDO-DATA-DD-MMM-YY-DOT2                             UDATECNV
028500         MOVE DDI-DATA-YYDDD-YY TO                                UDATECNV
028600              DDO-DATA-DD-MMM-YY-YY                               UDATECNV
028700       WHEN DDO-DD-MMM-YYYY                                       UDATECNV
028800         MOVE WS-DAY-OF-MONTH TO                                  UDATECNV
028900              DDO-DATA-DD-MMM-YYYY-DD                             UDATECNV
029000         MOVE '.' TO                                              UDATECNV
029100              DDO-DATA-DD-MMM-YYYY-DOT1                           UDATECNV
029200         MOVE WS-MONTH (WS-WORK-MM) TO                            UDATECNV
029300              DDO-DATA-DD-MMM-YYYY-MMM                            UDATECNV
029400         MOVE '.' TO                                              UDATECNV
029500              DDO-DATA-DD-MMM-YYYY-DOT2                           UDATECNV
029600         MOVE DDI-DATA-YYDDD-YY TO                                UDATECNV
029700              DDO-DATA-DD-MMM-YYYY-YYYY (3:2)                     UDATECNV
029800         IF DDI-DATA-YYDDD-YY IS LESS THAN '50'                   UDATECNV
029900            MOVE '20' TO DDO-DATA-DD-MMM-YYYY-YYYY (1:2)          UDATECNV
030000         ELSE                                                     UDATECNV
030100            MOVE '19' TO DDO-DATA-DD-MMM-YYYY-YYYY (1:2)          UDATECNV
030200         END-IF                                                   UDATECNV
030300       WHEN OTHER                                                 UDATECNV
030400         MOVE 'ERROR2' TO DDO-DATA                                UDATECNV
030500     END-EVALUATE.                                                UDATECNV
030600 DATE-CONVERT-IP-OPT4-EXIT.                                       UDATECNV
030700     EXIT.                                                        UDATECNV
030800                                                                  UDATECNV
030900* $ Version 5.98b sequenced on Thursday 5 Feb 2009 at 11:00pm     UDATECNV
