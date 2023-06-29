000100***************************************************************** ZBNKPRT2
000200*                                                               * ZBNKPRT2
000300*   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * ZBNKPRT2
000400*   This demonstration program is provided for use by users     * ZBNKPRT2
000500*   of Micro Focus products and may be used, modified and       * ZBNKPRT2
000600*   distributed as part of your application provided that       * ZBNKPRT2
000700*   you properly acknowledge the copyright of Micro Focus       * ZBNKPRT2
000800*   in this material.                                           * ZBNKPRT2
000900*                                                               * ZBNKPRT2
001000***************************************************************** ZBNKPRT2
001100                                                                  ZBNKPRT2
001200***************************************************************** ZBNKPRT2
001300* Prgram:      ZBNKPRT2.CBL                                     * ZBNKPRT2
001400* Function:    Print the data extracted from the policy file    * ZBNKPRT2
001500***************************************************************** ZBNKPRT2
001600 IDENTIFICATION DIVISION.                                         ZBNKPRT2
001700 PROGRAM-ID.                                                      ZBNKPRT2
001800     ZBNKPRT2.                                                    ZBNKPRT2
001900 DATE-WRITTEN.                                                    ZBNKPRT2
002000     September 2002.                                              ZBNKPRT2
002100 DATE-COMPILED.                                                   ZBNKPRT2
002200     Today.                                                       ZBNKPRT2
002300                                                                  ZBNKPRT2
002400 ENVIRONMENT DIVISION.                                            ZBNKPRT2
002500 INPUT-OUTPUT   SECTION.                                          ZBNKPRT2
002600   FILE-CONTROL.                                                  ZBNKPRT2
002700     SELECT EXTRACT-FILE                                          ZBNKPRT2
002800            ASSIGN       TO EXTRACT                               ZBNKPRT2
002900            ORGANIZATION IS SEQUENTIAL                            ZBNKPRT2
003000            ACCESS MODE  IS SEQUENTIAL                            ZBNKPRT2
003100            FILE STATUS  IS WS-EXTRACT-STATUS.                    ZBNKPRT2
003200     SELECT PRINTOUT-FILE                                         ZBNKPRT2
003300            ASSIGN       TO PRINTOUT                              ZBNKPRT2
003400            ORGANIZATION IS SEQUENTIAL                            ZBNKPRT2
003500            ACCESS MODE  IS SEQUENTIAL                            ZBNKPRT2
003600            FILE STATUS  IS WS-PRINTOUT-STATUS.                   ZBNKPRT2
003700                                                                  ZBNKPRT2
003800 DATA DIVISION.                                                   ZBNKPRT2
003900 FILE SECTION.                                                    ZBNKPRT2
004000                                                                  ZBNKPRT2
004100 FD  EXTRACT-FILE.                                                ZBNKPRT2
004200 01  EXTRACT-REC.                                                 ZBNKPRT2
004300 COPY CBANKXT2.                                                   ZBNKPRT2
004400                                                                  ZBNKPRT2
004500 FD  PRINTOUT-FILE.                                               ZBNKPRT2
004600 01  PRINTOUT-REC                            PIC X(121).          ZBNKPRT2
004700                                                                  ZBNKPRT2
004800 WORKING-STORAGE SECTION.                                         ZBNKPRT2
004900 COPY CTIMERD.                                                    ZBNKPRT2
005000                                                                  ZBNKPRT2
005100 01  WS-MISC-STORAGE.                                             ZBNKPRT2
005200   05  WS-PROGRAM-ID                         PIC X(8)             ZBNKPRT2
005300       VALUE 'ZBNKPRT2'.                                          ZBNKPRT2
005400   05  WS-EXTRACT-STATUS.                                         ZBNKPRT2
005500     10  WS-EXTRACT-STAT1                    PIC X(1).            ZBNKPRT2
005600     10  WS-EXTRACT-STAT2                    PIC X(1).            ZBNKPRT2
005700                                                                  ZBNKPRT2
005800   05  WS-PRINTOUT-STATUS.                                        ZBNKPRT2
005900     10  WS-PRINTOUT-STAT1                   PIC X(1).            ZBNKPRT2
006000     10  WS-PRINOUTY-STAT2                   PIC X(1).            ZBNKPRT2
006100                                                                  ZBNKPRT2
006200   05  WS-IO-STATUS.                                              ZBNKPRT2
006300     10  WS-IO-STAT1                         PIC X(1).            ZBNKPRT2
006400     10  WS-IO-STAT2                         PIC X(1).            ZBNKPRT2
006500                                                                  ZBNKPRT2
006600   05  WS-TWO-BYTES.                                              ZBNKPRT2
006700     10  WS-TWO-BYTES-LEFT                   PIC X(1).            ZBNKPRT2
006800     10  WS-TWO-BYTES-RIGHT                  PIC X(1).            ZBNKPRT2
006900   05 WS-TWO-BYTES-BINARY REDEFINES WS-TWO-BYTES                  ZBNKPRT2
007000                                             PIC 9(1) COMP.       ZBNKPRT2
007100                                                                  ZBNKPRT2
007200   05  WS-END-OF-FILE                        PIC X(3)             ZBNKPRT2
007300       VALUE 'NO '.                                               ZBNKPRT2
007400                                                                  ZBNKPRT2
007500   05  WS-RECORDS-READ                       PIC 9(5)             ZBNKPRT2
007600       VALUE ZERO.                                                ZBNKPRT2
007700                                                                  ZBNKPRT2
007800   05  WS-SUB1                               PIC 9(3).            ZBNKPRT2
007900                                                                  ZBNKPRT2
008000 01  WS-PRINT-LINES.                                              ZBNKPRT2
008100   05  WS-LINE1.                                                  ZBNKPRT2
008200     10  WS-LINE1-TOP-LEFT                   PIC X(1)             ZBNKPRT2
008300         VALUE '.'.                                               ZBNKPRT2
008400     10  WS-LINE1-DASH                       PIC X(35)            ZBNKPRT2
008500         VALUE SPACES.                                            ZBNKPRT2
008600     10  WS-LINE1-TOP-RIGHT                  PIC X(1)             ZBNKPRT2
008700         VALUE '.'.                                               ZBNKPRT2
008800                                                                  ZBNKPRT2
008900   05  WS-LINE2.                                                  ZBNKPRT2
009000     10  FILLER                              PIC X(6)             ZBNKPRT2
009100         VALUE '| To: '.                                          ZBNKPRT2
009200     10  WS-LINE2-NAME                       PIC X(25)            ZBNKPRT2
009300         VALUE SPACES.                                            ZBNKPRT2
009400     10  FILLER                              PIC X(6)             ZBNKPRT2
009500         VALUE '     |'.                                          ZBNKPRT2
009600                                                                  ZBNKPRT2
009700   05  WS-LINE3.                                                  ZBNKPRT2
009800     10  FILLER                              PIC X(6)             ZBNKPRT2
009900         VALUE '|     '.                                          ZBNKPRT2
010000     10  WS-LINE3-ADDR1                      PIC X(25)            ZBNKPRT2
010100         VALUE SPACES.                                            ZBNKPRT2
010200     10  FILLER                              PIC X(6)             ZBNKPRT2
010300         VALUE '     |'.                                          ZBNKPRT2
010400                                                                  ZBNKPRT2
010500   05  WS-LINE4.                                                  ZBNKPRT2
010600     10  FILLER                              PIC X(6)             ZBNKPRT2
010700         VALUE '|     '.                                          ZBNKPRT2
010800     10  WS-LINE4-ADDR2                      PIC X(25)            ZBNKPRT2
010900         VALUE SPACES.                                            ZBNKPRT2
011000     10  FILLER                              PIC X(6)             ZBNKPRT2
011100         VALUE '     |'.                                          ZBNKPRT2
011200                                                                  ZBNKPRT2
011300   05  WS-LINE5.                                                  ZBNKPRT2
011400     10  FILLER                              PIC X(6)             ZBNKPRT2
011500         VALUE '|     '.                                          ZBNKPRT2
011600     10  WS-LINE5-ADDR3                      PIC X(25)            ZBNKPRT2
011700         VALUE SPACES.                                            ZBNKPRT2
011800     10  FILLER                              PIC X(6)             ZBNKPRT2
011900         VALUE '     |'.                                          ZBNKPRT2
012000                                                                  ZBNKPRT2
012100   05  WS-LINE6.                                                  ZBNKPRT2
012200     10  FILLER                              PIC X(6)             ZBNKPRT2
012300         VALUE '|     '.                                          ZBNKPRT2
012400     10  WS-LINE6-POST-CODE                  PIC X(6)             ZBNKPRT2
012500         VALUE SPACES.                                            ZBNKPRT2
012600     10  FILLER                              PIC X(19)            ZBNKPRT2
012700         VALUE SPACES.                                            ZBNKPRT2
012800     10  FILLER                              PIC X(6)             ZBNKPRT2
012900         VALUE '     |'.                                          ZBNKPRT2
013000                                                                  ZBNKPRT2
013100   05  WS-LINE7.                                                  ZBNKPRT2
013200     10  FILLER                              PIC X(6)             ZBNKPRT2
013300         VALUE '| Ref:'.                                          ZBNKPRT2
013400     10  WS-LINE7-PID                        PIC X(5)             ZBNKPRT2
013500         VALUE SPACES.                                            ZBNKPRT2
013600     10  WS-LINE7-SLASH                      PIC X(1)             ZBNKPRT2
013700         VALUE '/'.                                               ZBNKPRT2
013800     10  WS-LINE7-POLICY-ID                  PIC X(8)             ZBNKPRT2
013900         VALUE SPACES.                                            ZBNKPRT2
014000     10  FILLER                              PIC X(11)            ZBNKPRT2
014100         VALUE SPACES.                                            ZBNKPRT2
014200     10  FILLER                              PIC X(6)             ZBNKPRT2
014300         VALUE '     |'.                                          ZBNKPRT2
014400                                                                  ZBNKPRT2
014500   05  WS-LINE8.                                                  ZBNKPRT2
014600     10  WS-LINE8-BTM-LEFT                   PIC X(1)             ZBNKPRT2
014700         VALUE '.'.                                               ZBNKPRT2
014800     10  WS-LINE8-DASH                       PIC X(35)            ZBNKPRT2
014900         VALUE SPACES.                                            ZBNKPRT2
015000     10  WS-LINE8-BTM-RIGHT                  PIC X(1)             ZBNKPRT2
015100         VALUE '.'.                                               ZBNKPRT2
015200                                                                  ZBNKPRT2
015300 01  WS-PRINT-LINES-R REDEFINES WS-PRINT-LINES.                   ZBNKPRT2
015400   05  WS-LINE                               PIC X(37)            ZBNKPRT2
015500       OCCURS 8 TIMES.                                            ZBNKPRT2
015600                                                                  ZBNKPRT2
015700 01  WS-CONSOLE-MESSAGE                      PIC X(48).           ZBNKPRT2
015800                                                                  ZBNKPRT2
015900 01  WS-EXEC-PARM                            PIC X(12).           ZBNKPRT2
016000                                                                  ZBNKPRT2
016100 COPY CABENDD.                                                    ZBNKPRT2
016200                                                                  ZBNKPRT2
016300 LINKAGE SECTION.                                                 ZBNKPRT2
016400 01  LK-EXEC-PARM.                                                ZBNKPRT2
016500   05  LK-EXEC-PARM-LL                       PIC S9(4) COMP.      ZBNKPRT2
016600   05  LK-EXEC-PARM-DATA                     PIC X(12).           ZBNKPRT2
016700                                                                  ZBNKPRT2
016800 PROCEDURE DIVISION USING LK-EXEC-PARM.                           ZBNKPRT2
016900                                                                  ZBNKPRT2
017000     PERFORM RUN-TIME.                                            ZBNKPRT2
017100                                                                  ZBNKPRT2
017200     MOVE SPACES TO WS-EXEC-PARM.                                 ZBNKPRT2
017300     IF LK-EXEC-PARM-LL IS EQUAL TO ZERO                          ZBNKPRT2
017400        MOVE 'No exec card parm present'                          ZBNKPRT2
017500          TO WS-CONSOLE-MESSAGE                                   ZBNKPRT2
017600        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKPRT2
017700     ELSE                                                         ZBNKPRT2
017800       MOVE LK-EXEC-PARM-DATA (1:LK-EXEC-PARM-LL) TO              ZBNKPRT2
017900         WS-EXEC-PARM                                             ZBNKPRT2
018000       MOVE SPACES TO WS-CONSOLE-MESSAGE                          ZBNKPRT2
018100       STRING 'Exec parm is "' DELIMITED BY SIZE                  ZBNKPRT2
018200              LK-EXEC-PARM-DATA (1:LK-EXEC-PARM-LL)               ZBNKPRT2
018300                DELIMITED BY SIZE                                 ZBNKPRT2
018400              '"' DELIMITED BY SIZE                               ZBNKPRT2
018500         INTO WS-CONSOLE-MESSAGE                                  ZBNKPRT2
018600       PERFORM DISPLAY-CONSOLE-MESSAGE                            ZBNKPRT2
018700     END-IF.                                                      ZBNKPRT2
018800                                                                  ZBNKPRT2
018900     MOVE '/' TO WS-LINE1-TOP-LEFT.                               ZBNKPRT2
019000     MOVE ALL '-' TO WS-LINE1-DASH.                               ZBNKPRT2
019100     MOVE '\' TO WS-LINE1-TOP-RIGHT.                              ZBNKPRT2
019200     MOVE '\' TO WS-LINE8-BTM-LEFT.                               ZBNKPRT2
019300     MOVE ALL '-' TO WS-LINE8-DASH.                               ZBNKPRT2
019400     MOVE '/' TO WS-LINE8-BTM-RIGHT.                              ZBNKPRT2
019500                                                                  ZBNKPRT2
019600     PERFORM EXTRACT-OPEN.                                        ZBNKPRT2
019700     PERFORM PRINTOUT-OPEN.                                       ZBNKPRT2
019800                                                                  ZBNKPRT2
019900     PERFORM UNTIL WS-END-OF-FILE = 'YES'                         ZBNKPRT2
020000       IF WS-END-OF-FILE = 'NO '                                  ZBNKPRT2
020100          PERFORM EXTRACT-GET                                     ZBNKPRT2
020200          IF WS-END-OF-FILE = 'NO '                               ZBNKPRT2
020300             ADD 1 TO WS-RECORDS-READ                             ZBNKPRT2
020400             IF WS-RECORDS-READ IS LESS THAN 6                    ZBNKPRT2
020500                DISPLAY EXTRACT-REC UPON CONSOLE                  ZBNKPRT2
020600             ELSE                                                 ZBNKPRT2
020700                IF WS-RECORDS-READ IS EQUAL TO 6                  ZBNKPRT2
020800                   MOVE 'Suppressing record display...'           ZBNKPRT2
020900                      TO WS-CONSOLE-MESSAGE                       ZBNKPRT2
021000                   PERFORM DISPLAY-CONSOLE-MESSAGE                ZBNKPRT2
021100                END-IF                                            ZBNKPRT2
021200             END-IF                                               ZBNKPRT2
021300             PERFORM FORMAT-AND-PRINT                             ZBNKPRT2
021400          END-IF                                                  ZBNKPRT2
021500       END-IF                                                     ZBNKPRT2
021600     END-PERFORM.                                                 ZBNKPRT2
021700                                                                  ZBNKPRT2
021800     PERFORM EXTRACT-CLOSE.                                       ZBNKPRT2
021900     PERFORM PRINTOUT-CLOSE.                                      ZBNKPRT2
022000                                                                  ZBNKPRT2
022100     PERFORM DISPLAY-CONSOLE-MESSAGE.                             ZBNKPRT2
022200     MOVE 'End Of Job'                                            ZBNKPRT2
022300       TO WS-CONSOLE-MESSAGE.                                     ZBNKPRT2
022400     PERFORM DISPLAY-CONSOLE-MESSAGE.                             ZBNKPRT2
022500                                                                  ZBNKPRT2
022600     PERFORM RUN-TIME.                                            ZBNKPRT2
022700                                                                  ZBNKPRT2
022800     MOVE 0 TO RETURN-CODE.                                       ZBNKPRT2
022900                                                                  ZBNKPRT2
023000     GOBACK.                                                      ZBNKPRT2
023100                                                                  ZBNKPRT2
023200***************************************************************** ZBNKPRT2
023300* Format print lines                                            * ZBNKPRT2
023400***************************************************************** ZBNKPRT2
023500 FORMAT-AND-PRINT.                                                ZBNKPRT2
023600     MOVE BANKXT02-NAME TO WS-LINE2-NAME.                         ZBNKPRT2
023700     MOVE BANKXT02-ADDR1 TO WS-LINE3-ADDR1.                       ZBNKPRT2
023800     MOVE BANKXT02-ADDR2 TO WS-LINE4-ADDR2.                       ZBNKPRT2
023900     MOVE BANKXT02-ADDR3 TO WS-LINE5-ADDR3.                       ZBNKPRT2
024000     MOVE BANKXT02-ZIP TO WS-LINE6-POST-CODE.                     ZBNKPRT2
024100     MOVE BANKXT02-PID TO WS-LINE7-PID.                           ZBNKPRT2
024200     MOVE BANKXT02-POLICY-ID TO WS-LINE7-POLICY-ID.               ZBNKPRT2
024300     MOVE 1 TO WS-SUB1.                                           ZBNKPRT2
024400     PERFORM UNTIL WS-SUB1 IS GREATER THAN 8                      ZBNKPRT2
024500       MOVE WS-LINE (WS-SUB1) TO PRINTOUT-REC                     ZBNKPRT2
024600       PERFORM PRINTOUT-PUT                                       ZBNKPRT2
024700       ADD 1 TO WS-SUB1                                           ZBNKPRT2
024800     END-PERFORM.                                                 ZBNKPRT2
024900     MOVE SPACES TO PRINTOUT-REC.                                 ZBNKPRT2
025000     PERFORM PRINTOUT-PUT.                                        ZBNKPRT2
025100                                                                  ZBNKPRT2
025200***************************************************************** ZBNKPRT2
025300* Open the EXTRACTed data file                                 *  ZBNKPRT2
025400***************************************************************** ZBNKPRT2
025500 EXTRACT-OPEN.                                                    ZBNKPRT2
025600     OPEN INPUT EXTRACT-FILE.                                     ZBNKPRT2
025700     IF WS-EXTRACT-STATUS = '00'                                  ZBNKPRT2
025800        MOVE 'EXTRACT file opened OK'                             ZBNKPRT2
025900          TO WS-CONSOLE-MESSAGE                                   ZBNKPRT2
026000        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKPRT2
026100     ELSE                                                         ZBNKPRT2
026200        MOVE 'EXTRACT file open failure...'                       ZBNKPRT2
026300          TO WS-CONSOLE-MESSAGE                                   ZBNKPRT2
026400        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKPRT2
026500        MOVE WS-EXTRACT-STATUS TO WS-IO-STATUS                    ZBNKPRT2
026600        PERFORM DISPLAY-IO-STATUS                                 ZBNKPRT2
026700        PERFORM ABORT-PROGRAM                                     ZBNKPRT2
026800        END-IF.                                                   ZBNKPRT2
026900                                                                  ZBNKPRT2
027000***************************************************************** ZBNKPRT2
027100* Read a record from the EXTRACTed data file                    * ZBNKPRT2
027200***************************************************************** ZBNKPRT2
027300 EXTRACT-GET.                                                     ZBNKPRT2
027400     READ EXTRACT-FILE.                                           ZBNKPRT2
027500     IF WS-EXTRACT-STATUS NOT = '00'                              ZBNKPRT2
027600        IF WS-EXTRACT-STATUS = '10'                               ZBNKPRT2
027700           MOVE 'YES' TO WS-END-OF-FILE                           ZBNKPRT2
027800        ELSE                                                      ZBNKPRT2
027900           MOVE 'EXTRACT Error readng file ...'                   ZBNKPRT2
028000             TO WS-CONSOLE-MESSAGE                                ZBNKPRT2
028100            PERFORM DISPLAY-CONSOLE-MESSAGE                       ZBNKPRT2
028200            MOVE WS-EXTRACT-STATUS TO WS-IO-STATUS                ZBNKPRT2
028300            PERFORM DISPLAY-IO-STATUS                             ZBNKPRT2
028400            PERFORM ABORT-PROGRAM                                 ZBNKPRT2
028500        END-IF                                                    ZBNKPRT2
028600     END-IF.                                                      ZBNKPRT2
028700                                                                  ZBNKPRT2
028800***************************************************************** ZBNKPRT2
028900* Close the EXTRACTed data file                                 * ZBNKPRT2
029000***************************************************************** ZBNKPRT2
029100 EXTRACT-CLOSE.                                                   ZBNKPRT2
029200     CLOSE EXTRACT-FILE.                                          ZBNKPRT2
029300     IF WS-EXTRACT-STATUS = '00'                                  ZBNKPRT2
029400        MOVE 'EXTRACT file closed OK'                             ZBNKPRT2
029500          TO WS-CONSOLE-MESSAGE                                   ZBNKPRT2
029600        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKPRT2
029700     ELSE                                                         ZBNKPRT2
029800        MOVE 'EXTRACT file close failure...'                      ZBNKPRT2
029900          TO WS-CONSOLE-MESSAGE                                   ZBNKPRT2
030000        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKPRT2
030100        MOVE WS-EXTRACT-STATUS TO WS-IO-STATUS                    ZBNKPRT2
030200        PERFORM DISPLAY-IO-STATUS                                 ZBNKPRT2
030300        PERFORM ABORT-PROGRAM                                     ZBNKPRT2
030400     END-IF.                                                      ZBNKPRT2
030500                                                                  ZBNKPRT2
030600***************************************************************** ZBNKPRT2
030700* Open the seqential print file                                 * ZBNKPRT2
030800***************************************************************** ZBNKPRT2
030900 PRINTOUT-OPEN.                                                   ZBNKPRT2
031000     OPEN OUTPUT PRINTOUT-FILE.                                   ZBNKPRT2
031100     IF WS-PRINTOUT-STATUS = '00'                                 ZBNKPRT2
031200        MOVE 'PRINTOUT file opened OK'                            ZBNKPRT2
031300          TO WS-CONSOLE-MESSAGE                                   ZBNKPRT2
031400        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKPRT2
031500     ELSE                                                         ZBNKPRT2
031600        MOVE 'PRINTOUT file open failure...'                      ZBNKPRT2
031700          TO WS-CONSOLE-MESSAGE                                   ZBNKPRT2
031800        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKPRT2
031900        MOVE WS-PRINTOUT-STATUS TO WS-IO-STATUS                   ZBNKPRT2
032000        PERFORM DISPLAY-IO-STATUS                                 ZBNKPRT2
032100        PERFORM ABORT-PROGRAM                                     ZBNKPRT2
032200        END-IF.                                                   ZBNKPRT2
032300                                                                  ZBNKPRT2
032400***************************************************************** ZBNKPRT2
032500* Write a record to the squential file                          * ZBNKPRT2
032600***************************************************************** ZBNKPRT2
032700 PRINTOUT-PUT.                                                    ZBNKPRT2
032800     WRITE PRINTOUT-REC..                                         ZBNKPRT2
032900     IF WS-PRINTOUT-STATUS NOT = '00'                             ZBNKPRT2
033000        MOVE 'PRINTOUT Error Writing file ...'                    ZBNKPRT2
033100          TO WS-CONSOLE-MESSAGE                                   ZBNKPRT2
033200        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKPRT2
033300        MOVE WS-PRINTOUT-STATUS TO WS-IO-STATUS                   ZBNKPRT2
033400        PERFORM DISPLAY-IO-STATUS                                 ZBNKPRT2
033500        PERFORM ABORT-PROGRAM                                     ZBNKPRT2
033600     END-IF.                                                      ZBNKPRT2
033700                                                                  ZBNKPRT2
033800***************************************************************** ZBNKPRT2
033900* Close the seqential print file                                * ZBNKPRT2
034000***************************************************************** ZBNKPRT2
034100 PRINTOUT-CLOSE.                                                  ZBNKPRT2
034200     CLOSE PRINTOUT-FILE.                                         ZBNKPRT2
034300     IF WS-PRINTOUT-STATUS = '00'                                 ZBNKPRT2
034400        MOVE 'PRINTOUT file closed OK'                            ZBNKPRT2
034500          TO WS-CONSOLE-MESSAGE                                   ZBNKPRT2
034600        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKPRT2
034700     ELSE                                                         ZBNKPRT2
034800        MOVE 'PRINTOUT file close failure...'                     ZBNKPRT2
034900          TO WS-CONSOLE-MESSAGE                                   ZBNKPRT2
035000        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKPRT2
035100        MOVE WS-PRINTOUT-STATUS TO WS-IO-STATUS                   ZBNKPRT2
035200        PERFORM DISPLAY-IO-STATUS                                 ZBNKPRT2
035300        PERFORM ABORT-PROGRAM                                     ZBNKPRT2
035400     END-IF.                                                      ZBNKPRT2
035500                                                                  ZBNKPRT2
035600***************************************************************** ZBNKPRT2
035700* Display the file status bytes. This routine will display as   * ZBNKPRT2
035800* two digits if the full two byte file status is numeric. If    * ZBNKPRT2
035900* second byte is non-numeric then it will be treated as a       * ZBNKPRT2
036000* binary number.                                                * ZBNKPRT2
036100***************************************************************** ZBNKPRT2
036200 DISPLAY-IO-STATUS.                                               ZBNKPRT2
036300     IF WS-IO-STATUS NUMERIC                                      ZBNKPRT2
036400        MOVE SPACE TO WS-CONSOLE-MESSAGE                          ZBNKPRT2
036500        STRING 'File status -' DELIMITED BY SIZE                  ZBNKPRT2
036600               WS-IO-STATUS DELIMITED BY SIZE                     ZBNKPRT2
036700          INTO WS-CONSOLE-MESSAGE                                 ZBNKPRT2
036800        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKPRT2
036900     ELSE                                                         ZBNKPRT2
037000        SUBTRACT WS-TWO-BYTES-BINARY FROM WS-TWO-BYTES-BINARY     ZBNKPRT2
037100        MOVE WS-IO-STAT2 TO WS-TWO-BYTES-RIGHT                    ZBNKPRT2
037200        MOVE SPACE TO WS-CONSOLE-MESSAGE                          ZBNKPRT2
037300        STRING 'File status -' DELIMITED BY SIZE                  ZBNKPRT2
037400               WS-IO-STAT1 DELIMITED BY SIZE                      ZBNKPRT2
037500               '/' DELIMITED BY SIZE                              ZBNKPRT2
037600               WS-TWO-BYTES DELIMITED BY SIZE                     ZBNKPRT2
037700          INTO WS-CONSOLE-MESSAGE                                 ZBNKPRT2
037800        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKPRT2
037900     END-IF.                                                      ZBNKPRT2
038000                                                                  ZBNKPRT2
038100***************************************************************** ZBNKPRT2
038200* 'ABORT' the program.                                          * ZBNKPRT2
038300* Post a message to the console and issue a goback              * ZBNKPRT2
038400***************************************************************** ZBNKPRT2
038500 ABORT-PROGRAM.                                                   ZBNKPRT2
038600     IF WS-CONSOLE-MESSAGE NOT = SPACES                           ZBNKPRT2
038700        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKPRT2
038800     END-IF.                                                      ZBNKPRT2
038900     MOVE 'Program is abending...'  TO WS-CONSOLE-MESSAGE.        ZBNKPRT2
039000     PERFORM DISPLAY-CONSOLE-MESSAGE.                             ZBNKPRT2
039100     MOVE 16 TO RETURN-CODE.                                      ZBNKPRT2
039200     GOBACK.                                                      ZBNKPRT2
039300                                                                  ZBNKPRT2
039400***************************************************************** ZBNKPRT2
039500* Display CONSOLE messages...                                   * ZBNKPRT2
039600***************************************************************** ZBNKPRT2
039700 DISPLAY-CONSOLE-MESSAGE.                                         ZBNKPRT2
039800     DISPLAY WS-PROGRAM-ID ' - ' WS-CONSOLE-MESSAGE               ZBNKPRT2
039900       UPON CONSOLE.                                              ZBNKPRT2
040000     MOVE ALL SPACES TO WS-CONSOLE-MESSAGE.                       ZBNKPRT2
040100                                                                  ZBNKPRT2
040200 COPY CTIMERP.                                                    ZBNKPRT2
040300                                                                  ZBNKPRT2
