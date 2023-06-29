000100***************************************************************** zbnkext1
000200*                                                               * zbnkext1
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * zbnkext1
000400*   This demonstration program is provided for use by users     * zbnkext1
000500*   of Micro Focus products and may be used, modified and       * zbnkext1
000600*   distributed as part of your application provided that       * zbnkext1
000700*   you properly acknowledge the copyright of Micro Focus       * zbnkext1
000800*   in this material.                                           * zbnkext1
000900*                                                               * zbnkext1
001000***************************************************************** zbnkext1
001100                                                                  zbnkext1
001200***************************************************************** zbnkext1
001300* Prgram:      ZBNKEXT1.CBL                                     * zbnkext1
001400* Function:    Extract data to print bank statements            * zbnkext1
001500***************************************************************** zbnkext1
001600 IDENTIFICATION DIVISION.                                         zbnkext1
001700 PROGRAM-ID.                                                      zbnkext1
001800     ZBNKEXT1.                                                    zbnkext1
001900 DATE-WRITTEN.                                                    zbnkext1
002000     September 2002.                                              zbnkext1
002100 DATE-COMPILED.                                                   zbnkext1
002200     Today.                                                       zbnkext1
002300 ENVIRONMENT DIVISION.                                            zbnkext1
002400 INPUT-OUTPUT   SECTION.                                          zbnkext1
002500   FILE-CONTROL.                                                  zbnkext1
002600     SELECT EXTRACT-FILE                                          zbnkext1
002700            ASSIGN       TO EXTRACT                               zbnkext1
002800            ORGANIZATION IS SEQUENTIAL                            zbnkext1
002900            ACCESS MODE  IS SEQUENTIAL                            zbnkext1
003000            FILE STATUS  IS WS-EXTRACT-STATUS.                    zbnkext1
003100                                                                  zbnkext1
003200 DATA DIVISION.                                                   zbnkext1
003300 FILE SECTION.                                                    zbnkext1
003400 FD  EXTRACT-FILE                                                 zbnkext1
003500     RECORDING MODE IS V                                          zbnkext1
003600     RECORD CONTAINS 66 TO 95 CHARACTERS.                         zbnkext1
003700 COPY CBANKXT1.                                                   zbnkext1
003800                                                                  zbnkext1
003900 WORKING-STORAGE SECTION.                                         zbnkext1
004000 COPY CTIMERD.                                                    zbnkext1
004100                                                                  zbnkext1
004200 01  WS-MISC-STORAGE.                                             zbnkext1
004300   05  WS-PROGRAM-ID                         PIC X(8)             zbnkext1
004400       VALUE 'ZBNKEXT1'.                                          zbnkext1
004500   05  WS-EXTRACT-STATUS.                                         zbnkext1
004600     10  WS-EXTRACT-STAT1                    PIC X(1).            zbnkext1
004700     10  WS-EXTRACT-STAT2                    PIC X(1).            zbnkext1
004800                                                                  zbnkext1
004900   05  WS-IO-STATUS.                                              zbnkext1
005000     10  WS-IO-STAT1                         PIC X(1).            zbnkext1
005100     10  WS-IO-STAT2                         PIC X(1).            zbnkext1
005200                                                                  zbnkext1
005300   05  WS-TWO-BYTES.                                              zbnkext1
005400     10  WS-TWO-BYTES-LEFT                   PIC X(1).            zbnkext1
005500     10  WS-TWO-BYTES-RIGHT                  PIC X(1).            zbnkext1
005600   05 WS-TWO-BYTES-BINARY REDEFINES WS-TWO-BYTES                  zbnkext1
005700                                             PIC 9(1) COMP.       zbnkext1
005800                                                                  zbnkext1
005900   05  WS-RECORD-COUNTER1                    PIC 9(5)             zbnkext1
006000       VALUE ZERO.                                                zbnkext1
006100   05  WS-RECORD-COUNTER2                    PIC 9(5)             zbnkext1
006200       VALUE ZERO.                                                zbnkext1
006300                                                                  zbnkext1
006400   05  WS-LAST-PID                           PIC X(5)             zbnkext1
006500       VALUE LOW-VALUES.                                          zbnkext1
006600                                                                  zbnkext1
006700 01  WS-ZBNKRPC1-FIELDS.                                          zbnkext1
006800   05  WS-ZBNKRPC1-REQUESTED                 PIC X(1)             zbnkext1
006900       VALUE LOW-VALUES.                                          zbnkext1
007000     88  RPC-REQUESTED                       VALUE 'Y'.           zbnkext1
007100   05  WS-ZBNKRPC1-PGM                       PIC X(8)             zbnkext1
007200       VALUE SPACES.                                              zbnkext1
007300   05  WS-ZBNKRPC1-IND                       PIC X(1)             zbnkext1
007400       VALUE LOW-VALUES.                                          zbnkext1
007500   05  WS-ZBNKRPC1-DATA.                                          zbnkext1
007600     10  WS-ZBNKRPC1-DATA-PT1                PIC X(80).           zbnkext1
007700     10  WS-ZBNKRPC1-DATA-PT2                PIC X(80).           zbnkext1
007800                                                                  zbnkext1
007900 01  WS-DATA-REPOSITORY.                                          zbnkext1
008000   05  WS-DATA-ACCESS                        PIC X(3).            zbnkext1
008100     88  DATA-ACCESS-DLI                     VALUE 'DLI'.         zbnkext1
008200     88  DATA-ACCESS-SQL                     VALUE 'SQL'.         zbnkext1
008300     88  DATA-ACCESS-VSM                     VALUE 'VSM'.         zbnkext1
008400   05  WS-DATA-ACCESS-SQL-TYPE               PIC X(3).            zbnkext1
008500     88  SQL-ACCESS-DB2                      VALUE 'DB2'.         zbnkext1
008600     88  SQL-ACCESS-XDB                      VALUE 'XDB'.         zbnkext1
008700                                                                  zbnkext1
008800 01  WS-CONSOLE-MESSAGE                      PIC X(60).           zbnkext1
008900                                                                  zbnkext1
009000 01  WS-EXEC-PARM.                                                zbnkext1
009100   05  WS-EXEC-PARM-LL                       PIC S9(4) COMP.      zbnkext1
009200   05  WS-EXEC-PARM-DATA                     PIC X(12).           zbnkext1
009300                                                                  zbnkext1
009400 01  WS-PARM-PTR                             POINTER.             zbnkext1
009500 01  WS-PARM-PTR-NUM REDEFINES WS-PARM-PTR   PIC X(4) COMP-5.     zbnkext1
009600                                                                  zbnkext1
009700 01  WS-COMMAREA.                                                 zbnkext1
009800 COPY CIOFUNCS.                                                   zbnkext1
009900 COPY CBANKD51.                                                   zbnkext1
010000 COPY CBANKD52.                                                   zbnkext1
010100                                                                  zbnkext1
010200 COPY CABENDD.                                                    zbnkext1
010300                                                                  zbnkext1
010400 COPY CIMSCONS.                                                   zbnkext1
010500                                                                  zbnkext1
010600 COPY CIMSAIB.                                                    zbnkext1
010700                                                                  zbnkext1
010800 01  WS-ENV-AREA                             PIC X(200).          zbnkext1
010900 01  WS-ENV-AREA-R REDEFINES WS-ENV-AREA.                         zbnkext1
011000   05  WS-ENVIRON-DATA                       PIC X(100).          zbnkext1
011100   05  WS-ENV-DATA REDEFINES WS-ENVIRON-DATA.                     zbnkext1
011200     10  WS-ENV-ID                           PIC X(8).            zbnkext1
011300     10  WS-ENV-REL                          PIC X(4).            zbnkext1
011400     10  WS-ENV-CTLTYPE                      PIC X(8).            zbnkext1
011500     10  WS-ENV-APPTYPE                      PIC X(8).            zbnkext1
011600     10  WS-ENV-RGNID                        PIC X(4).            zbnkext1
011700     10  WS-ENV-APPNAME                      PIC X(8).            zbnkext1
011800     10  WS-ENV-PSBNAME                      PIC X(8).            zbnkext1
011900     10  WS-ENV-TRNNAME                      PIC X(8).            zbnkext1
012000     10  WS-ENV-UID                          PIC X(8).            zbnkext1
012100     10  WS-ENV-GRPNAME                      PIC X(8).            zbnkext1
012200     10  WS-ENV-STATUS                       PIC X(4).            zbnkext1
012300     10  WS-ENV-RECTOK                       POINTER.             zbnkext1
012400     10  WS-ENV-ADDRPRM                      POINTER.             zbnkext1
012500     10  WS-ENV-SHRQ                         PIC X(4).            zbnkext1
012600     10  WS-ENV-UADS                         PIC X(8).            zbnkext1
012700     10  WS-ENV-UIND                         PIC X(4).            zbnkext1
012800   05  WS-RECOVER-TOKEN                      PIC X(18).           zbnkext1
012900                                                                  zbnkext1
013000 LINKAGE SECTION.                                                 zbnkext1
013100 01  LK-EXEC-PARM.                                                zbnkext1
013200   05  LK-EXEC-PARM-LL                       PIC S9(4) COMP.      zbnkext1
013300   05  LK-EXEC-PARM-DATA                     PIC X(32).           zbnkext1
013400                                                                  zbnkext1
013500 PROCEDURE DIVISION USING LK-EXEC-PARM.                           zbnkext1
013600***************************************************************** zbnkext1
013700* Perform RUN-TIME to initialse time and display start time     * zbnkext1
013800***************************************************************** zbnkext1
013900     PERFORM RUN-TIME.                                            zbnkext1
014000                                                                  zbnkext1
016800                                                                  zbnkext1
016900***************************************************************** zbnkext1
017000* EXEC-CARD processing is slightly different from normal MVS    * zbnkext1
017100* processing in that we check the pointer (or address) of the   * zbnkext1
017200* parm area first. This is so that we can migrate it to         * zbnkext1
017300* distributed (Windows/Unix) environment wihout change.         * zbnkext1
017400***************************************************************** zbnkext1
017500     MOVE ZEROES TO WS-EXEC-PARM-LL.                              zbnkext1
017600     MOVE SPACES TO WS-EXEC-PARM-DATA.                            zbnkext1
017700                                                                  zbnkext1
017800     SET WS-PARM-PTR TO ADDRESS OF LK-EXEC-PARM.                  zbnkext1
017900     IF WS-PARM-PTR-NUM IS NOT EQUAL TO ZEROS                     zbnkext1
018000        MOVE LK-EXEC-PARM-LL TO WS-EXEC-PARM-LL                   zbnkext1
018100        IF WS-EXEC-PARM-LL IS GREATER THAN                        zbnkext1
018200             LENGTH OF WS-EXEC-PARM-DATA                          zbnkext1
018300           MOVE LENGTH OF WS-EXEC-PARM-DATA TO WS-EXEC-PARM-LL    zbnkext1
018400        END-IF                                                    zbnkext1
018500        IF WS-EXEC-PARM-LL IS GREATER THAN ZERO                   zbnkext1
018600           MOVE LK-EXEC-PARM-DATA (1:WS-EXEC-PARM-LL)             zbnkext1
018700             TO WS-EXEC-PARM-DATA (1:WS-EXEC-PARM-LL)             zbnkext1
018800        END-IF                                                    zbnkext1
018900     END-IF.                                                      zbnkext1
019000                                                                  zbnkext1
019100     IF WS-EXEC-PARM-LL IS EQUAL TO ZERO                          zbnkext1
019200        MOVE 'No exec card parm present'                          zbnkext1
019300          TO WS-CONSOLE-MESSAGE                                   zbnkext1
019400        PERFORM DISPLAY-CONSOLE-MESSAGE                           zbnkext1
019500        MOVE '  Selecting all records'                            zbnkext1
019600          TO WS-CONSOLE-MESSAGE                                   zbnkext1
019700        PERFORM DISPLAY-CONSOLE-MESSAGE                           zbnkext1
019800        MOVE 3 TO WS-EXEC-PARM-LL                                 zbnkext1
019900        MOVE 'ALL' TO WS-EXEC-PARM-DATA                           zbnkext1
020000     ELSE                                                         zbnkext1
020100       MOVE SPACES TO WS-CONSOLE-MESSAGE                          zbnkext1
020200       STRING 'Exec parm is "' DELIMITED BY SIZE                  zbnkext1
020300              WS-EXEC-PARM-DATA (1:WS-EXEC-PARM-LL)               zbnkext1
020400                DELIMITED BY SIZE                                 zbnkext1
020500              '"' DELIMITED BY SIZE                               zbnkext1
020600         INTO WS-CONSOLE-MESSAGE                                  zbnkext1
020700       PERFORM DISPLAY-CONSOLE-MESSAGE                            zbnkext1
020800       MOVE SPACES TO WS-CONSOLE-MESSAGE                          zbnkext1
020900       STRING '  Selecting records for ' DELIMITED BY SIZE        zbnkext1
021000              WS-EXEC-PARM-DATA (1:WS-EXEC-PARM-LL)               zbnkext1
021100                DELIMITED BY SIZE                                 zbnkext1
021200              ' only' DELIMITED BY SIZE                           zbnkext1
021300         INTO WS-CONSOLE-MESSAGE                                  zbnkext1
021400       PERFORM DISPLAY-CONSOLE-MESSAGE                            zbnkext1
021500     END-IF.                                                      zbnkext1
021600     INSPECT WS-EXEC-PARM-DATA (1:WS-EXEC-PARM-LL)                zbnkext1
021700       CONVERTING 'abcdefghijklmnopqrstuvwxyz'                    zbnkext1
021800               TO 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.                   zbnkext1
021900                                                                  zbnkext1
022000***************************************************************** zbnkext1
022100* Check to see if we want to demonstrate MFE calling a module   * zbnkext1
022200* that resides on the mainframe.                                * zbnkext1
022300***************************************************************** zbnkext1
022400     IF RPC-REQUESTED                                             zbnkext1
022500        PERFORM RPC-PROCESS                                       zbnkext1
022600     END-IF.                                                      zbnkext1
022700                                                                  zbnkext1
022800***************************************************************** zbnkext1
022900* Open our output file                                          * zbnkext1
023000***************************************************************** zbnkext1
023100     PERFORM EXTRACT-OPEN.                                        zbnkext1
023200                                                                  zbnkext1
023300***************************************************************** zbnkext1
023400* Open the customer details input then read the data and create * zbnkext1
023500* output records as appropriate.                                * zbnkext1
023600***************************************************************** zbnkext1
023700     PERFORM SOURCE1-OPEN.                                        zbnkext1
023800     PERFORM UNTIL IO-REQUEST-STATUS-EOF                          zbnkext1
023900       IF NOT IO-REQUEST-STATUS-EOF                               zbnkext1
024000          PERFORM SOURCE1-READ                                    zbnkext1
024100          IF IO-REQUEST-STATUS-OK                                 zbnkext1
024200             ADD 1 TO WS-RECORD-COUNTER1                          zbnkext1
024300             IF WS-RECORD-COUNTER1 IS LESS THAN 6                 zbnkext1
024400                MOVE WS-COMMAREA TO WS-CONSOLE-MESSAGE            zbnkext1
024500                PERFORM DISPLAY-CONSOLE-MESSAGE                   zbnkext1
024600             ELSE                                                 zbnkext1
024700                IF WS-RECORD-COUNTER2 IS EQUAL TO 6               zbnkext1
024800                   MOVE 'Suppressing record display...'           zbnkext1
024900                      TO WS-CONSOLE-MESSAGE                       zbnkext1
025000                   PERFORM DISPLAY-CONSOLE-MESSAGE                zbnkext1
025100                END-IF                                            zbnkext1
025200             END-IF                                               zbnkext1
025300                                                                  zbnkext1
025400             IF CD51O-PID IS NOT EQUAL TO WS-LAST-PID             zbnkext1
025500                MOVE SPACES TO BANKXT01-REC0                      zbnkext1
025600                MOVE '0' TO BANKXT01-0-TYPE                       zbnkext1
025700                MOVE CD51O-PID TO BANKXT01-1-PID                  zbnkext1
025800                MOVE CD51O-NAME TO BANKXT01-0-NAME                zbnkext1
025900                MOVE CD51O-EMAIL TO BANKXT01-0-EMAIL              zbnkext1
026000                PERFORM EXTRACT-PUT                               zbnkext1
026100                MOVE SPACES TO BANKXT01-REC1                      zbnkext1
026200                MOVE '1' TO BANKXT01-1-TYPE                       zbnkext1
026300                MOVE CD51O-PID TO BANKXT01-1-PID                  zbnkext1
026400                MOVE CD51O-NAME TO BANKXT01-1-NAME                zbnkext1
026500                MOVE CD51O-ADDR1 TO BANKXT01-1-ADDR1              zbnkext1
026600                MOVE CD51O-ADDR2 TO BANKXT01-1-ADDR2              zbnkext1
026700                MOVE CD51O-STATE TO BANKXT01-1-STATE              zbnkext1
026800                MOVE CD51O-CNTRY TO BANKXT01-1-CNTRY              zbnkext1
026900                MOVE CD51O-POST-CODE TO BANKXT01-1-PST-CDE        zbnkext1
027000                PERFORM EXTRACT-PUT                               zbnkext1
027100                MOVE CD51O-PID TO WS-LAST-PID                     zbnkext1
027200             END-IF                                               zbnkext1
027300             MOVE SPACES TO BANKXT01-REC2                         zbnkext1
027400             MOVE '2' TO BANKXT01-2-TYPE                          zbnkext1
027500             MOVE CD51O-PID TO BANKXT01-2-PID                     zbnkext1
027600             MOVE CD51O-ACC-NO TO BANKXT01-2-ACC-NO               zbnkext1
027700             MOVE CD51O-ACC-DESC TO BANKXT01-2-ACC-DESC           zbnkext1
027800             MOVE CD51O-ACC-CURR-BAL TO BANKXT01-2-ACC-CURR-BAL   zbnkext1
027900             MOVE CD51O-ACC-LAST-STMT-DTE                         zbnkext1
028000               TO BANKXT01-2-ACC-LAST-STMT-DTE                    zbnkext1
028100             MOVE CD51O-ACC-LAST-STMT-BAL                         zbnkext1
028200               TO BANKXT01-2-ACC-LAST-STMT-BAL                    zbnkext1
028300             PERFORM EXTRACT-PUT                                  zbnkext1
028400          END-IF                                                  zbnkext1
028500       END-IF                                                     zbnkext1
028600     END-PERFORM.                                                 zbnkext1
028700     PERFORM SOURCE1-CLOSE.                                       zbnkext1
028800                                                                  zbnkext1
028900***************************************************************** zbnkext1
029000* Open the transactions details file then read the data and     * zbnkext1
029100* create output records as appropriate.                         * zbnkext1
029200***************************************************************** zbnkext1
029300     PERFORM SOURCE2-OPEN.                                        zbnkext1
029400     PERFORM UNTIL IO-REQUEST-STATUS-EOF                          zbnkext1
029500       IF NOT IO-REQUEST-STATUS-EOF                               zbnkext1
029600          PERFORM SOURCE2-READ                                    zbnkext1
029700          IF IO-REQUEST-STATUS-OK                                 zbnkext1
029800             ADD 1 TO WS-RECORD-COUNTER2                          zbnkext1
029900             IF WS-RECORD-COUNTER2 IS LESS THAN 6                 zbnkext1
030000                MOVE WS-COMMAREA TO WS-CONSOLE-MESSAGE            zbnkext1
030100                PERFORM DISPLAY-CONSOLE-MESSAGE                   zbnkext1
030200             ELSE                                                 zbnkext1
030300                IF WS-RECORD-COUNTER2 IS EQUAL TO 6               zbnkext1
030400                   MOVE 'Suppressing record display...'           zbnkext1
030500                      TO WS-CONSOLE-MESSAGE                       zbnkext1
030600                   PERFORM DISPLAY-CONSOLE-MESSAGE                zbnkext1
030700                END-IF                                            zbnkext1
030800             END-IF                                               zbnkext1
030900                                                                  zbnkext1
031000             MOVE SPACES TO BANKXT01-REC3                         zbnkext1
031100             MOVE '3' TO BANKXT01-3-TYPE                          zbnkext1
031200             MOVE CD52O-PID TO BANKXT01-3-PID                     zbnkext1
031300             MOVE CD52O-ACC-NO TO BANKXT01-2-ACC-NO               zbnkext1
031400             MOVE CD52O-AMOUNT TO BANKXT01-3-AMOUNT               zbnkext1
031500             MOVE CD52O-TIMESTAMP TO BANKXT01-3-TIMESTAMP         zbnkext1
031600             MOVE CD52O-DESC TO BANKXT01-3-DESC                   zbnkext1
031700             PERFORM EXTRACT-PUT                                  zbnkext1
031800          END-IF                                                  zbnkext1
031900       END-IF                                                     zbnkext1
032000     END-PERFORM.                                                 zbnkext1
032100     PERFORM SOURCE2-CLOSE.                                       zbnkext1
032200                                                                  zbnkext1
032300***************************************************************** zbnkext1
032400* Close our output file                                         * zbnkext1
032500***************************************************************** zbnkext1
032600     PERFORM EXTRACT-CLOSE.                                       zbnkext1
032700                                                                  zbnkext1
032800***************************************************************** zbnkext1
032900* Display messages to show what we created                      * zbnkext1
033000***************************************************************** zbnkext1
033100     MOVE 'SOURCE data has been extracted'                        zbnkext1
033200       TO WS-CONSOLE-MESSAGE.                                     zbnkext1
033300     PERFORM DISPLAY-CONSOLE-MESSAGE.                             zbnkext1
033400     MOVE SPACES TO WS-CONSOLE-MESSAGE.                           zbnkext1
033500     STRING WS-RECORD-COUNTER1 DELIMITED BY SIZE                  zbnkext1
033600            ' from SOURCE1 (Customer details)'                    zbnkext1
033700              DELIMITED BY SIZE                                   zbnkext1
033800       INTO WS-CONSOLE-MESSAGE.                                   zbnkext1
033900     PERFORM DISPLAY-CONSOLE-MESSAGE.                             zbnkext1
034000     MOVE SPACES TO WS-CONSOLE-MESSAGE.                           zbnkext1
034100     STRING WS-RECORD-COUNTER2 DELIMITED BY SIZE                  zbnkext1
034200            ' from SOURCE2 (Transactions)'                        zbnkext1
034300              DELIMITED BY SIZE                                   zbnkext1
034400       INTO WS-CONSOLE-MESSAGE.                                   zbnkext1
034500     PERFORM DISPLAY-CONSOLE-MESSAGE.                             zbnkext1
034600     MOVE 'End Of Job'                                            zbnkext1
034700       TO WS-CONSOLE-MESSAGE.                                     zbnkext1
034800     PERFORM DISPLAY-CONSOLE-MESSAGE.                             zbnkext1
034900                                                                  zbnkext1
035000***************************************************************** zbnkext1
035100* Perform RUN-TIME to calculate run time and display end time   * zbnkext1
035200***************************************************************** zbnkext1
035300     PERFORM RUN-TIME.                                            zbnkext1
035400                                                                  zbnkext1
035500***************************************************************** zbnkext1
035600* Step return code and return                                   * zbnkext1
035700***************************************************************** zbnkext1
035800     MOVE 0 TO RETURN-CODE.                                       zbnkext1
035900                                                                  zbnkext1
036000     GOBACK.                                                      zbnkext1
036100                                                                  zbnkext1
036200***************************************************************** zbnkext1
036300* Open the source file                                          * zbnkext1
036400***************************************************************** zbnkext1
036500 SOURCE1-OPEN.                                                    zbnkext1
036600     MOVE SPACES TO WS-COMMAREA.                                  zbnkext1
036700     MOVE WS-EXEC-PARM-DATA TO CD51I-PID.                         zbnkext1
036800     SET IO-REQUEST-FUNCTION-OPEN TO TRUE.                        zbnkext1
036900     CALL 'DBANK51P' USING WS-COMMAREA.                           zbnkext1
037000     IF IO-REQUEST-STATUS-OK                                      zbnkext1
037100        MOVE 'SOURCE1 (Customer details) file opened OK'          zbnkext1
037200          TO WS-CONSOLE-MESSAGE                                   zbnkext1
037300        PERFORM DISPLAY-CONSOLE-MESSAGE                           zbnkext1
037400     ELSE                                                         zbnkext1
037500        MOVE 'SOURCE1 (Customer details) file open failure...'    zbnkext1
037600          TO WS-CONSOLE-MESSAGE                                   zbnkext1
037700        PERFORM DISPLAY-CONSOLE-MESSAGE                           zbnkext1
037800        PERFORM ABORT-PROGRAM                                     zbnkext1
037900        END-IF.                                                   zbnkext1
038000 SOURCE2-OPEN.                                                    zbnkext1
038100     MOVE SPACES TO WS-COMMAREA.                                  zbnkext1
038200     MOVE WS-EXEC-PARM-DATA TO CD52I-PID.                         zbnkext1
038300     SET IO-REQUEST-FUNCTION-OPEN TO TRUE.                        zbnkext1
038400     CALL 'DBANK52P' USING WS-COMMAREA.                           zbnkext1
038500     IF IO-REQUEST-STATUS-OK                                      zbnkext1
038600        MOVE 'SOURCE2 (Transactions) file opened OK'              zbnkext1
038700          TO WS-CONSOLE-MESSAGE                                   zbnkext1
038800        PERFORM DISPLAY-CONSOLE-MESSAGE                           zbnkext1
038900     ELSE                                                         zbnkext1
039000        MOVE 'SOURCE2 (Transactions) file open failure...'        zbnkext1
039100          TO WS-CONSOLE-MESSAGE                                   zbnkext1
039200        PERFORM DISPLAY-CONSOLE-MESSAGE                           zbnkext1
039300        PERFORM ABORT-PROGRAM                                     zbnkext1
039400        END-IF.                                                   zbnkext1
039500                                                                  zbnkext1
039600***************************************************************** zbnkext1
039700* Read a record from the source file                            * zbnkext1
039800***************************************************************** zbnkext1
039900 SOURCE1-READ.                                                    zbnkext1
040000     MOVE SPACES TO WS-COMMAREA.                                  zbnkext1
040100     MOVE WS-EXEC-PARM-DATA TO CD51I-PID.                         zbnkext1
040200     SET IO-REQUEST-FUNCTION-READ TO TRUE.                        zbnkext1
040300     CALL 'DBANK51P' USING WS-COMMAREA.                           zbnkext1
040400     IF IO-REQUEST-STATUS-ERROR                                   zbnkext1
040500        MOVE 'SOURCE1 (Customer details) Error reading file ...'  zbnkext1
040600          TO WS-CONSOLE-MESSAGE                                   zbnkext1
040700         PERFORM DISPLAY-CONSOLE-MESSAGE                          zbnkext1
040800         PERFORM ABORT-PROGRAM                                    zbnkext1
040900     END-IF.                                                      zbnkext1
041000 SOURCE2-READ.                                                    zbnkext1
041100     MOVE SPACES TO WS-COMMAREA.                                  zbnkext1
041200     MOVE WS-EXEC-PARM-DATA TO CD52I-PID.                         zbnkext1
041300     SET IO-REQUEST-FUNCTION-READ TO TRUE.                        zbnkext1
041400     CALL 'DBANK52P' USING WS-COMMAREA.                           zbnkext1
041500     IF IO-REQUEST-STATUS-ERROR                                   zbnkext1
041600        MOVE 'SOURCE2 (Transactions) Error reading file ...'      zbnkext1
041700          TO WS-CONSOLE-MESSAGE                                   zbnkext1
041800         PERFORM DISPLAY-CONSOLE-MESSAGE                          zbnkext1
041900         PERFORM ABORT-PROGRAM                                    zbnkext1
042000     END-IF.                                                      zbnkext1
042100                                                                  zbnkext1
042200***************************************************************** zbnkext1
042300* Close the source file.                                        * zbnkext1
042400***************************************************************** zbnkext1
042500 SOURCE1-CLOSE.                                                   zbnkext1
042600     MOVE SPACES TO WS-COMMAREA.                                  zbnkext1
042700     MOVE WS-EXEC-PARM-DATA TO CD51I-PID.                         zbnkext1
042800     SET IO-REQUEST-FUNCTION-CLOSE TO TRUE.                       zbnkext1
042900     CALL 'DBANK51P' USING WS-COMMAREA.                           zbnkext1
043000     IF IO-REQUEST-STATUS-ERROR                                   zbnkext1
043100        MOVE 'SOURCE1 (Customer details) Error closing file ...'  zbnkext1
043200          TO WS-CONSOLE-MESSAGE                                   zbnkext1
043300         PERFORM DISPLAY-CONSOLE-MESSAGE                          zbnkext1
043400         PERFORM ABORT-PROGRAM                                    zbnkext1
043500     END-IF.                                                      zbnkext1
043600 SOURCE2-CLOSE.                                                   zbnkext1
043700     MOVE SPACES TO WS-COMMAREA.                                  zbnkext1
043800     MOVE WS-EXEC-PARM-DATA TO CD52I-PID.                         zbnkext1
043900     SET IO-REQUEST-FUNCTION-CLOSE TO TRUE.                       zbnkext1
044000     CALL 'DBANK52P' USING WS-COMMAREA.                           zbnkext1
044100     IF IO-REQUEST-STATUS-ERROR                                   zbnkext1
044200        MOVE 'SOURCE2 (Transactions) Error closing file ...'      zbnkext1
044300          TO WS-CONSOLE-MESSAGE                                   zbnkext1
044400         PERFORM DISPLAY-CONSOLE-MESSAGE                          zbnkext1
044500         PERFORM ABORT-PROGRAM                                    zbnkext1
044600     END-IF.                                                      zbnkext1
044700                                                                  zbnkext1
044800***************************************************************** zbnkext1
044900* Open the seqential extract file as output                     * zbnkext1
045000***************************************************************** zbnkext1
045100 EXTRACT-OPEN.                                                    zbnkext1
045200     OPEN OUTPUT EXTRACT-FILE.                                    zbnkext1
045300     IF WS-EXTRACT-STATUS = '00'                                  zbnkext1
045400        MOVE 'EXTRACT file opened OK'                             zbnkext1
045500          TO WS-CONSOLE-MESSAGE                                   zbnkext1
045600        PERFORM DISPLAY-CONSOLE-MESSAGE                           zbnkext1
045700     ELSE                                                         zbnkext1
045800        MOVE 'EXTRACT file open failure...'                       zbnkext1
045900          TO WS-CONSOLE-MESSAGE                                   zbnkext1
046000        PERFORM DISPLAY-CONSOLE-MESSAGE                           zbnkext1
046100        MOVE WS-EXTRACT-STATUS TO WS-IO-STATUS                    zbnkext1
046200        PERFORM DISPLAY-IO-STATUS                                 zbnkext1
046300        PERFORM ABORT-PROGRAM                                     zbnkext1
046400        END-IF.                                                   zbnkext1
046500                                                                  zbnkext1
046600***************************************************************** zbnkext1
046700* Write a record to the squential file                          * zbnkext1
046800***************************************************************** zbnkext1
046900 EXTRACT-PUT.                                                     zbnkext1
047000     IF BANKXT01-1-TYPE IS EQUAL TO '0'                           zbnkext1
047100        WRITE BANKXT01-REC0                                       zbnkext1
047200     END-IF.                                                      zbnkext1
047300     IF BANKXT01-1-TYPE IS EQUAL TO '1'                           zbnkext1
047400        WRITE BANKXT01-REC1                                       zbnkext1
047500     END-IF.                                                      zbnkext1
047600     IF BANKXT01-2-TYPE IS EQUAL TO '2'                           zbnkext1
047700        WRITE BANKXT01-REC2                                       zbnkext1
047800     END-IF.                                                      zbnkext1
047900     IF BANKXT01-3-TYPE IS EQUAL TO '3'                           zbnkext1
048000        WRITE BANKXT01-REC3                                       zbnkext1
048100     END-IF.                                                      zbnkext1
048200     IF WS-EXTRACT-STATUS NOT = '00'                              zbnkext1
048300        MOVE 'EXTRACT Error Writing file ...'                     zbnkext1
048400          TO WS-CONSOLE-MESSAGE                                   zbnkext1
048500        PERFORM DISPLAY-CONSOLE-MESSAGE                           zbnkext1
048600        MOVE WS-EXTRACT-STATUS TO WS-IO-STATUS                    zbnkext1
048700        PERFORM DISPLAY-IO-STATUS                                 zbnkext1
048800        PERFORM ABORT-PROGRAM                                     zbnkext1
048900     END-IF.                                                      zbnkext1
049000                                                                  zbnkext1
049100***************************************************************** zbnkext1
049200* Close the seqential extract file                              * zbnkext1
049300***************************************************************** zbnkext1
049400 EXTRACT-CLOSE.                                                   zbnkext1
049500     CLOSE EXTRACT-FILE.                                          zbnkext1
049600     IF WS-EXTRACT-STATUS = '00'                                  zbnkext1
049700        MOVE 'EXTRACT file closed OK'                             zbnkext1
049800          TO WS-CONSOLE-MESSAGE                                   zbnkext1
049900        PERFORM DISPLAY-CONSOLE-MESSAGE                           zbnkext1
050000     ELSE                                                         zbnkext1
050100        MOVE 'EXTRACT file close failure...'                      zbnkext1
050200          TO WS-CONSOLE-MESSAGE                                   zbnkext1
050300        PERFORM DISPLAY-CONSOLE-MESSAGE                           zbnkext1
050400        MOVE WS-EXTRACT-STATUS TO WS-IO-STATUS                    zbnkext1
050500        PERFORM DISPLAY-IO-STATUS                                 zbnkext1
050600        PERFORM ABORT-PROGRAM                                     zbnkext1
050700     END-IF.                                                      zbnkext1
050800                                                                  zbnkext1
050900***************************************************************** zbnkext1
051000* Display the file status bytes. This routine will display as   * zbnkext1
051100* two digits if the full two byte file status is numeric. If    * zbnkext1
051200* second byte is non-numeric then it will be treated as a       * zbnkext1
051300* binary number.                                                * zbnkext1
051400***************************************************************** zbnkext1
051500 DISPLAY-IO-STATUS.                                               zbnkext1
051600     IF WS-IO-STATUS NUMERIC                                      zbnkext1
051700        MOVE SPACE TO WS-CONSOLE-MESSAGE                          zbnkext1
051800        STRING 'File status -' DELIMITED BY SIZE                  zbnkext1
051900               WS-IO-STATUS DELIMITED BY SIZE                     zbnkext1
052000          INTO WS-CONSOLE-MESSAGE                                 zbnkext1
052100        PERFORM DISPLAY-CONSOLE-MESSAGE                           zbnkext1
052200     ELSE                                                         zbnkext1
052300        SUBTRACT WS-TWO-BYTES-BINARY FROM WS-TWO-BYTES-BINARY     zbnkext1
052400        MOVE WS-IO-STAT2 TO WS-TWO-BYTES-RIGHT                    zbnkext1
052500        MOVE SPACE TO WS-CONSOLE-MESSAGE                          zbnkext1
052600        STRING 'File status -' DELIMITED BY SIZE                  zbnkext1
052700               WS-IO-STAT1 DELIMITED BY SIZE                      zbnkext1
052800               '/' DELIMITED BY SIZE                              zbnkext1
052900               WS-TWO-BYTES DELIMITED BY SIZE                     zbnkext1
053000          INTO WS-CONSOLE-MESSAGE                                 zbnkext1
053100        PERFORM DISPLAY-CONSOLE-MESSAGE                           zbnkext1
053200     END-IF.                                                      zbnkext1
053300                                                                  zbnkext1
053400***************************************************************** zbnkext1
053500* 'ABORT' the program.                                          * zbnkext1
053600* Post a message to the console and issue a STOP RUN            * zbnkext1
053700***************************************************************** zbnkext1
053800 ABORT-PROGRAM.                                                   zbnkext1
053900     IF WS-CONSOLE-MESSAGE NOT = SPACES                           zbnkext1
054000        PERFORM DISPLAY-CONSOLE-MESSAGE                           zbnkext1
054100     END-IF.                                                      zbnkext1
054200     MOVE 'Program is abending...'  TO WS-CONSOLE-MESSAGE.        zbnkext1
054300     PERFORM DISPLAY-CONSOLE-MESSAGE.                             zbnkext1
054400     MOVE 16 TO RETURN-CODE.                                      zbnkext1
054500     GOBACK.                                                      zbnkext1
054600                                                                  zbnkext1
054700***************************************************************** zbnkext1
054800* This process will attempt to call a small module which is     * zbnkext1
054900* meant toreside on th emainframe                               * zbnkext1
055000***************************************************************** zbnkext1
055100 RPC-PROCESS.                                                     zbnkext1
055200     MOVE '0' TO WS-ZBNKRPC1-IND.                                 zbnkext1
055300     MOVE LOW-VALUES TO WS-ZBNKRPC1-DATA-PT1.                     zbnkext1
055400     MOVE HIGH-VALUES TO WS-ZBNKRPC1-DATA-PT2.                    zbnkext1
055500     MOVE 'ZBNKRPC1' TO WS-ZBNKRPC1-PGM.                          zbnkext1
055600     CALL WS-ZBNKRPC1-PGM USING WS-ZBNKRPC1-DATA                  zbnkext1
055700       ON EXCEPTION                                               zbnkext1
055800         MOVE '1' TO WS-ZBNKRPC1-IND                              zbnkext1
055900     END-CALL.                                                    zbnkext1
056000     IF WS-ZBNKRPC1-IND IS EQUAL TO '1'                           zbnkext1
056100        MOVE 'Call to ZBNKRPC1 failed. Program not found.'        zbnkext1
056200          TO WS-CONSOLE-MESSAGE                                   zbnkext1
056300        PERFORM DISPLAY-CONSOLE-MESSAGE                           zbnkext1
056400     ELSE                                                         zbnkext1
056500        IF WS-ZBNKRPC1-DATA-PT1 IS EQUAL TO LOW-VALUES AND        zbnkext1
056600           WS-ZBNKRPC1-DATA-PT2 IS EQUAL TO HIGH-VALUES           zbnkext1
056700           MOVE 'Call to ZBNKRPC1 was to a stub program.'         zbnkext1
056800             TO WS-CONSOLE-MESSAGE                                zbnkext1
056900           PERFORM DISPLAY-CONSOLE-MESSAGE                        zbnkext1
057000           MOVE 'Passed data area was unchanged.'                 zbnkext1
057100             TO WS-CONSOLE-MESSAGE                                zbnkext1
057200           PERFORM DISPLAY-CONSOLE-MESSAGE                        zbnkext1
057300        ELSE                                                      zbnkext1
057400           MOVE WS-ZBNKRPC1-DATA-PT1 TO WS-CONSOLE-MESSAGE        zbnkext1
057500           PERFORM DISPLAY-CONSOLE-MESSAGE                        zbnkext1
057600           MOVE WS-ZBNKRPC1-DATA-PT2 TO WS-CONSOLE-MESSAGE        zbnkext1
057700           PERFORM DISPLAY-CONSOLE-MESSAGE                        zbnkext1
057800        END-IF                                                    zbnkext1
057900     END-IF.                                                      zbnkext1
058000                                                                  zbnkext1
058100***************************************************************** zbnkext1
058200* Display CONSOLE messages...                                   * zbnkext1
058300***************************************************************** zbnkext1
058400 DISPLAY-CONSOLE-MESSAGE.                                         zbnkext1
058500     DISPLAY WS-PROGRAM-ID ' - ' WS-CONSOLE-MESSAGE.              zbnkext1
058600     DISPLAY WS-PROGRAM-ID ' - ' WS-CONSOLE-MESSAGE               zbnkext1
058700       UPON CONSOLE.                                              zbnkext1
058800     MOVE ALL SPACES TO WS-CONSOLE-MESSAGE.                       zbnkext1
058900                                                                  zbnkext1
059000*COPY CTIMERP.                                                    zbnkext1
001500 RUN-TIME.                                                        ctimerp
001600     IF TIMER-START IS EQUAL TO ZERO                              ctimerp
001700        ACCEPT TIMER-START FROM TIME                              ctimerp
001800        MOVE 'Timer started' TO WS-CONSOLE-MESSAGE                ctimerp
001900        PERFORM DISPLAY-CONSOLE-MESSAGE                           ctimerp
002000     ELSE                                                         ctimerp
002100        ACCEPT TIMER-END FROM TIME                                ctimerp
002200        MOVE 'Timer stopped' TO WS-CONSOLE-MESSAGE                ctimerp
002300        PERFORM DISPLAY-CONSOLE-MESSAGE                           ctimerp
002400        COMPUTE TIMER-ELAPSED =                                   ctimerp
002500                  ((TIMER-END-HH * 60 * 60 * 100) +               ctimerp
002600                   (TIMER-END-MM * 60 * 100) +                    ctimerp
002700                   (TIMER-END-SS * 100) +                         ctimerp
002800                    TIMER-END-DD) -                               ctimerp
002900                  ((TIMER-START-HH * 60 * 60 * 100) +             ctimerp
003000                   (TIMER-START-MM * 60 * 100) +                  ctimerp
003100                   (TIMER-START-SS * 100) +                       ctimerp
003200                    TIMER-START-DD)                               ctimerp
003300        MOVE TIMER-ELAPSED-R TO TIMER-RUN-TIME-ELAPSED            ctimerp
003400        MOVE TIMER-RUN-TIME TO WS-CONSOLE-MESSAGE                 ctimerp
003500        PERFORM DISPLAY-CONSOLE-MESSAGE                           ctimerp
003600     END-IF.                                                      ctimerp
003700                                                                  ctimerp
059100                                                                  zbnkext1
059200* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     zbnkext1
