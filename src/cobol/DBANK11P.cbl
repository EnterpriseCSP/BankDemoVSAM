000100***************************************************************** dbank11p
000200*                                                               * dbank11p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * dbank11p
000400*   This demonstration program is provided for use by users     * dbank11p
000500*   of Micro Focus products and may be used, modified and       * dbank11p
000600*   distributed as part of your application provided that       * dbank11p
000700*   you properly acknowledge the copyright of Micro Focus       * dbank11p
000800*   in this material.                                           * dbank11p
000900*                                                               * dbank11p
001000***************************************************************** dbank11p
001100                                                                  dbank11p
001200***************************************************************** dbank11p
001300* Program:     DBANK11P.CBL                                     * dbank11p
001400* Function:    Obtain Bank Account Details (Extended)           * dbank11p
001500*              VSAM Version                                     * dbank11p
001600***************************************************************** dbank11p
001700                                                                  dbank11p
001800 IDENTIFICATION DIVISION.                                         dbank11p
001900 PROGRAM-ID.                                                      dbank11p
002000     DBANK11P.                                                    dbank11p
002100 DATE-WRITTEN.                                                    dbank11p
002200     September 2002.                                              dbank11p
002300 DATE-COMPILED.                                                   dbank11p
002400     Today.                                                       dbank11p
002500                                                                  dbank11p
002600 ENVIRONMENT DIVISION.                                            dbank11p
002700                                                                  dbank11p
002800 DATA DIVISION.                                                   dbank11p
002900                                                                  dbank11p
003000 WORKING-STORAGE SECTION.                                         dbank11p
003100 01  WS-MISC-STORAGE.                                             dbank11p
003200   05  WS-PROGRAM-ID                         PIC X(8)             dbank11p
003300       VALUE 'DBANK11P'.                                          dbank11p
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            dbank11p
003500   05  WS-RESP                               PIC S9(8) COMP.      dbank11p
003600   05  WS-BNKACC-RID                         PIC X(9).            dbank11p
003700   05  WS-BNKTXN-AIX1-RID                    PIC X(35).           dbank11p
003800   05  WS-BNKTXN-AIX1-RID-R REDEFINES WS-BNKTXN-AIX1-RID.         dbank11p
003900     10  WS-BNKTXN-AIX1-RID-ACC              PIC X(9).            dbank11p
004000     10  WS-BNKTXN-AIX1-RID-STAMP.                                dbank11p
004100       15  WS-BNKTXN-AIX1-RID-DATE           PIC X(10).           dbank11p
004200       15  WS-BNKTXN-AIX1-RID-DOT1           PIC X(1).            dbank11p
004300       15  WS-BNKTXN-AIX1-RID-TIME           PIC X(15).           dbank11p
004400       15  WS-BNKTXN-AIX1-RID-TIME-R REDEFINES                    dbank11p
004500             WS-BNKTXN-AIX1-RID-TIME.                             dbank11p
004600         20  WS-BNKTXN-AIX1-RID-HH           PIC 9(2).            dbank11p
004700         20  WS-BNKTXN-AIX1-RID-DOT2         PIC X(1).            dbank11p
004800         20  WS-BNKTXN-AIX1-RID-MM           PIC 9(2).            dbank11p
004900         20  WS-BNKTXN-AIX1-RID-DOT3         PIC X(1).            dbank11p
005000         20  WS-BNKTXN-AIX1-RID-SS           PIC 9(2).            dbank11p
005100         20  WS-BNKTXN-AIX1-RID-DOT4         PIC X(1).            dbank11p
005200         20  WS-BNKTXN-AIX1-RID-DEC          PIC 9(6).            dbank11p
005300   05  WS-TRANS-COUNT                        PIC S9(10) COMP-3.   dbank11p
005400   05  WS-TRANS-EDIT                         PIC Z(6)9.           dbank11p
005500   05  WS-TRANS-EDIT-X REDEFINES WS-TRANS-EDIT                    dbank11p
005600                                             PIC X(7).            dbank11p
005700                                                                  dbank11p
005800 COPY CTSTAMPD.                                                   dbank11p
005900                                                                  dbank11p
006000 01  WS-BNKACC-REC.                                               dbank11p
006100 COPY CBANKVAC.                                                   dbank11p
006200                                                                  dbank11p
006300 01  WS-BNKTXN-REC.                                               dbank11p
006400 COPY CBANKVTX.                                                   dbank11p
006500                                                                  dbank11p
006600 01  WS-COMMAREA.                                                 dbank11p
006700 COPY CBANKD11.                                                   dbank11p
006800                                                                  dbank11p
006900 COPY CABENDD.                                                    dbank11p
007000                                                                  dbank11p
007100 LINKAGE SECTION.                                                 dbank11p
007200 01  DFHCOMMAREA.                                                 dbank11p
007300   05  LK-COMMAREA                           PIC X(1)             dbank11p
007400       OCCURS 1 TO 6144 TIMES                                     dbank11p
007500         DEPENDING ON WS-COMMAREA-LENGTH.                         dbank11p
007600                                                                  dbank11p
007700 COPY CENTRY.                                                     dbank11p
007800***************************************************************** dbank11p
007900* Move the passed data to our area                              * dbank11p
008000***************************************************************** dbank11p
008100     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            dbank11p
008200     MOVE DFHCOMMAREA TO WS-COMMAREA.                             dbank11p
008300                                                                  dbank11p
008400***************************************************************** dbank11p
008500* Initialize our output area                                    * dbank11p
008600***************************************************************** dbank11p
008700     MOVE SPACES TO CD11O-DATA.                                   dbank11p
008800                                                                  dbank11p
008900***************************************************************** dbank11p
009000* Now attempt to get the requested record                       * dbank11p
009100***************************************************************** dbank11p
009200     MOVE CD11I-ACCNO TO WS-BNKACC-RID.                           dbank11p
009300     EXEC CICS READ FILE('BNKACC')                                dbank11p
009400                    INTO(WS-BNKACC-REC)                           dbank11p
009500                    LENGTH(LENGTH OF WS-BNKACC-REC)               dbank11p
009600                    RIDFLD(WS-BNKACC-RID)                         dbank11p
009700                    RESP(WS-RESP)                                 dbank11p
009800     END-EXEC.                                                    dbank11p
009900                                                                  dbank11p
010000***************************************************************** dbank11p
010100* Did we get the record OK                                      * dbank11p
010200***************************************************************** dbank11p
010300     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)                   dbank11p
010400        MOVE SPACES TO CD11O-ACCNO                                dbank11p
010500        GO TO FINISH                                              dbank11p
010600     END-IF.                                                      dbank11p
010700                                                                  dbank11p
010800***************************************************************** dbank11p
010900* We got the record OK                                          * dbank11p
011000***************************************************************** dbank11p
011100     MOVE BAC-REC-ACCNO TO CD11O-ACCNO.                           dbank11p
011200     MOVE ALL '?' TO CD11O-DESC.                                  dbank11p
011300     MOVE BAC-REC-BALANCE TO CD11O-BAL-N.                         dbank11p
011400     MOVE BAC-REC-LAST-STMT-DTE TO CD11O-DTE.                     dbank11p
011500     MOVE 'No' TO CD11O-TRANS.                                    dbank11p
011600     MOVE BAC-REC-ATM-ENABLED TO CD11O-ATM-ENABLED.               dbank11p
011700     MOVE BAC-REC-ATM-DAY-LIMIT TO CD11O-ATM-LIM-N.               dbank11p
011800     MOVE BAC-REC-ATM-DAY-DTE TO CD11O-ATM-LDTE.                  dbank11p
011900     MOVE BAC-REC-ATM-DAY-AMT TO CD11O-ATM-LAMT-N.                dbank11p
012000     MOVE BAC-REC-RP1-DAY TO CD11O-RP1DAY.                        dbank11p
012100     MOVE BAC-REC-RP1-AMOUNT TO CD11O-RP1AMT-N.                   dbank11p
012200     MOVE BAC-REC-RP1-PID TO CD11O-RP1PID.                        dbank11p
012300     MOVE BAC-REC-RP1-ACCNO TO CD11O-RP1ACC.                      dbank11p
012400     MOVE BAC-REC-RP1-LAST-PAY TO CD11O-RP1DTE.                   dbank11p
012500     MOVE BAC-REC-RP2-DAY TO CD11O-RP2DAY.                        dbank11p
012600     MOVE BAC-REC-RP2-AMOUNT TO CD11O-RP2AMT-N.                   dbank11p
012700     MOVE BAC-REC-RP2-PID TO CD11O-RP2PID.                        dbank11p
012800     MOVE BAC-REC-RP2-ACCNO TO CD11O-RP2ACC.                      dbank11p
012900     MOVE BAC-REC-RP2-LAST-PAY TO CD11O-RP2DTE.                   dbank11p
013000     MOVE BAC-REC-RP3-DAY TO CD11O-RP3DAY.                        dbank11p
013100     MOVE BAC-REC-RP3-AMOUNT TO CD11O-RP3AMT-N.                   dbank11p
013200     MOVE BAC-REC-RP3-PID TO CD11O-RP3PID.                        dbank11p
013300     MOVE BAC-REC-RP3-ACCNO TO CD11O-RP3ACC.                      dbank11p
013400     MOVE BAC-REC-RP3-LAST-PAY TO CD11O-RP3DTE.                   dbank11p
013500                                                                  dbank11p
013600***************************************************************** dbank11p
013700* Check for transactions                                        * dbank11p
013800***************************************************************** dbank11p
013900 BROWSE-START.                                                    dbank11p
014000     MOVE 0 TO WS-TRANS-COUNT.                                    dbank11p
014100     MOVE LOW-VALUES TO WS-BNKTXN-AIX1-RID.                       dbank11p
014200     MOVE CD11I-ACCNO TO WS-BNKTXN-AIX1-RID-ACC.                  dbank11p
014300     EXEC CICS STARTBR FILE('BNKTXN1')                            dbank11p
014400                       RIDFLD(WS-BNKTXN-AIX1-RID)                 dbank11p
014500                       GTEQ                                       dbank11p
014600     END-EXEC.                                                    dbank11p
014700     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)                   dbank11p
014800        GO TO BROWSE-STOP                                         dbank11p
014900     END-IF.                                                      dbank11p
015000 BROWSE-LOOP.                                                     dbank11p
015100     EXEC CICS READNEXT FILE('BNKTXN1')                           dbank11p
015200                        INTO(WS-BNKTXN-REC)                       dbank11p
015300                        LENGTH(LENGTH OF WS-BNKTXN-REC)           dbank11p
015400                        RIDFLD(WS-BNKTXN-AIX1-RID)                dbank11p
015500                        RESP(WS-RESP)                             dbank11p
015600     END-EXEC.                                                    dbank11p
015700     IF WS-RESP IS EQUAL TO DFHRESP(NORMAL)                       dbank11p
015800        IF CD11I-ACCNO IS EQUAL TO BTX-REC-ACCNO                  dbank11p
015900           ADD 1 TO WS-TRANS-COUNT                                dbank11p
016000           GO TO BROWSE-LOOP                                      dbank11p
016100        END-IF                                                    dbank11p
016200     END-IF.                                                      dbank11p
016300                                                                  dbank11p
016400 BROWSE-STOP.                                                     dbank11p
016500     EXEC CICS ENDBR FILE('BNKTXN1')                              dbank11p
016600     END-EXEC.                                                    dbank11p
016700                                                                  dbank11p
016800     IF WS-TRANS-COUNT IS EQUAL TO 0                              dbank11p
016900        MOVE 'No' TO CD11O-TRANS                                  dbank11p
017000     ELSE                                                         dbank11p
017100        MOVE WS-TRANS-COUNT TO WS-TRANS-EDIT                      dbank11p
017200        PERFORM TRANS-LEFT-JUST                                   dbank11p
017300        MOVE WS-TRANS-EDIT-X TO CD11O-TRANS                       dbank11p
017400     END-IF.                                                      dbank11p
017500                                                                  dbank11p
017600***************************************************************** dbank11p
017700* Move the result back to the callers area                      * dbank11p
017800***************************************************************** dbank11p
017900     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).       dbank11p
018000                                                                  dbank11p
018100***************************************************************** dbank11p
018200* Return to our caller                                          * dbank11p
018300***************************************************************** dbank11p
018400 FINISH.                                                          dbank11p
018500 COPY CRETURN.                                                    dbank11p
018600                                                                  dbank11p
018700 TRANS-LEFT-JUST.                                                 dbank11p
018800     IF WS-TRANS-EDIT-X(1:1) IS EQUAL TO SPACE                    dbank11p
018900        MOVE WS-TRANS-EDIT-X(2:LENGTH OF WS-TRANS-EDIT-X - 1)     dbank11p
019000          TO WS-TRANS-EDIT-X(1:LENGTH OF WS-TRANS-EDIT-X - 1)     dbank11p
019100        MOVE SPACE                                                dbank11p
019200          TO WS-TRANS-EDIT-X(LENGTH OF WS-TRANS-EDIT-X:1)         dbank11p
019300        GO TO TRANS-LEFT-JUST.                                    dbank11p
019400                                                                  dbank11p
019500* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     dbank11p
