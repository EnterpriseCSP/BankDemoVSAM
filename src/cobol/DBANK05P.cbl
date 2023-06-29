000100***************************************************************** dbank05p
000200*                                                               * dbank05p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * dbank05p
000400*   This demonstration program is provided for use by users     * dbank05p
000500*   of Micro Focus products and may be used, modified and       * dbank05p
000600*   distributed as part of your application provided that       * dbank05p
000700*   you properly acknowledge the copyright of Micro Focus       * dbank05p
000800*   in this material.                                           * dbank05p
000900*                                                               * dbank05p
001000***************************************************************** dbank05p
001100                                                                  dbank05p
001200***************************************************************** dbank05p
001300* Program:     DBANK05P.CBL                                     * dbank05p
001400* Function:    Obtain list of transactions for an account       * dbank05p
001500*              VSAM Version                                     * dbank05p
001600***************************************************************** dbank05p
001700                                                                  dbank05p
001800 IDENTIFICATION DIVISION.                                         dbank05p
001900 PROGRAM-ID.                                                      dbank05p
002000     DBANK05P.                                                    dbank05p
002100 DATE-WRITTEN.                                                    dbank05p
002200     September 2002.                                              dbank05p
002300 DATE-COMPILED.                                                   dbank05p
002400     Today.                                                       dbank05p
002500                                                                  dbank05p
002600 ENVIRONMENT DIVISION.                                            dbank05p
002700                                                                  dbank05p
002800 DATA DIVISION.                                                   dbank05p
002900                                                                  dbank05p
003000 WORKING-STORAGE SECTION.                                         dbank05p
003100 01  WS-MISC-STORAGE.                                             dbank05p
003200   05  WS-PROGRAM-ID                         PIC X(8)             dbank05p
003300       VALUE 'DBANK05P'.                                          dbank05p
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            dbank05p
003500   05  WS-SUB1                               PIC S9(4) COMP.      dbank05p
003600   05  WS-SUB2                               PIC S9(4) COMP.      dbank05p
003700   05  WS-RESP                               PIC S9(8) COMP.      dbank05p
003800   05  WS-WORK1                              PIC S9(15) COMP-3.   dbank05p
003900   05  WS-WORK2                              PIC S9(15) COMP-3.   dbank05p
004000   05  WS-BNKTXN-AIX1-RID                    PIC X(35).           dbank05p
004100   05  WS-BNKTXN-AIX1-RID-R REDEFINES WS-BNKTXN-AIX1-RID.         dbank05p
004200     10  WS-BNKTXN-AIX1-RID-ACC              PIC X(9).            dbank05p
004300     10  WS-BNKTXN-AIX1-RID-STAMP.                                dbank05p
004400       15  WS-BNKTXN-AIX1-RID-DATE           PIC X(10).           dbank05p
004500       15  WS-BNKTXN-AIX1-RID-DOT1           PIC X(1).            dbank05p
004600       15  WS-BNKTXN-AIX1-RID-TIME           PIC X(15).           dbank05p
004700       15  WS-BNKTXN-AIX1-RID-TIME-R REDEFINES                    dbank05p
004800             WS-BNKTXN-AIX1-RID-TIME.                             dbank05p
004900         20  WS-BNKTXN-AIX1-RID-HH           PIC 9(2).            dbank05p
005000         20  WS-BNKTXN-AIX1-RID-DOT2         PIC X(1).            dbank05p
005100         20  WS-BNKTXN-AIX1-RID-MM           PIC 9(2).            dbank05p
005200         20  WS-BNKTXN-AIX1-RID-DOT3         PIC X(1).            dbank05p
005300         20  WS-BNKTXN-AIX1-RID-SS           PIC 9(2).            dbank05p
005400         20  WS-BNKTXN-AIX1-RID-DOT4         PIC X(1).            dbank05p
005500         20  WS-BNKTXN-AIX1-RID-DEC          PIC 9(6).            dbank05p
005600                                                                  dbank05p
005700 01  WS-BNKTXN-REC.                                               dbank05p
005800 COPY CBANKVTX.                                                   dbank05p
005900                                                                  dbank05p
006000 COPY CBANKTXD.                                                   dbank05p
006100                                                                  dbank05p
006200 01  WS-TWOS-COMP.                                                dbank05p
006300   05  WS-TWOS-COMP-REQ                      PIC X(1).            dbank05p
006400     88  WS-TWOS-COMP-REQ-YES                VALUE 'Y'.           dbank05p
006500     88  WS-TWOS-COMP-REQ-NO                 VALUE 'N'.           dbank05p
006600   05  WS-TWOS-COMP-LEN                      PIC S9(4) COMP.      dbank05p
006700   05  WS-TWOS-COMP-INPUT                    PIC X(256).          dbank05p
006800   05  WS-TWOS-COMP-OUTPUT                   PIC X(256).          dbank05p
006900                                                                  dbank05p
007000 01  WS-COMMAREA.                                                 dbank05p
007100 COPY CBANKD05.                                                   dbank05p
007200                                                                  dbank05p
007300 COPY CABENDD.                                                    dbank05p
007400                                                                  dbank05p
007500 LINKAGE SECTION.                                                 dbank05p
007600 01  DFHCOMMAREA.                                                 dbank05p
007700   05  LK-COMMAREA                           PIC X(1)             dbank05p
007800       OCCURS 1 TO 4096 TIMES                                     dbank05p
007900         DEPENDING ON WS-COMMAREA-LENGTH.                         dbank05p
008000                                                                  dbank05p
008100 COPY CENTRY.                                                     dbank05p
008200***************************************************************** dbank05p
008300* Move the passed data to our area                              * dbank05p
008400***************************************************************** dbank05p
008500     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            dbank05p
008600     MOVE DFHCOMMAREA TO WS-COMMAREA.                             dbank05p
008700                                                                  dbank05p
008800***************************************************************** dbank05p
008900* Initialize our output area                                    * dbank05p
009000***************************************************************** dbank05p
009100     MOVE SPACES TO CD05O-DATA.                                   dbank05p
009200                                                                  dbank05p
009300***************************************************************** dbank05p
009400* Setup the start position for the browse                       * dbank05p
009500***************************************************************** dbank05p
009600     MOVE CD05I-ACC TO WS-BNKTXN-AIX1-RID-ACC.                    dbank05p
009700     MOVE CD05I-START-ID TO WS-BNKTXN-AIX1-RID-STAMP.             dbank05p
009800* We can't do a GT or LT, only GTEQ, as we can with DL1 or SQL.   dbank05p
009900* Thus we will 'fix' the search key by adjusting the time stamp   dbank05p
010000* up by 1 if we are going GT.                                     dbank05p
010100* We will convert the time part the timestamp (hh:mm:ss.dddddd)   dbank05p
010200* to a single nunber be the number of milli-sec. We then add or   dbank05p
010300* subtract 1 as appropriate and then convert it back. This should dbank05p
010400* work for any time that doesen't show as 23:59:59.999999 as this dbank05p
010500* will cause a change in the date.                                dbank05p
010600     IF CD05-START-HIGH                                           dbank05p
010700        COMPUTE WS-WORK1 =                                        dbank05p
010800                (WS-BNKTXN-AIX1-RID-HH * 60 * 60 * 1000000) +     dbank05p
010900                (WS-BNKTXN-AIX1-RID-MM * 60 * 1000000) +          dbank05p
011000                (WS-BNKTXN-AIX1-RID-SS * 1000000) +               dbank05p
011100                WS-BNKTXN-AIX1-RID-DEC                            dbank05p
011200        ADD 1 TO WS-WORK1                                         dbank05p
011300        DIVIDE 1000000 INTO WS-WORK1                              dbank05p
011400          GIVING WS-WORK2                                         dbank05p
011500            REMAINDER WS-BNKTXN-AIX1-RID-DEC                      dbank05p
011600        MOVE WS-WORK2 TO WS-WORK1                                 dbank05p
011700        DIVIDE 60 INTO WS-WORK1                                   dbank05p
011800          GIVING WS-WORK2                                         dbank05p
011900            REMAINDER WS-BNKTXN-AIX1-RID-SS                       dbank05p
012000        MOVE WS-WORK2 TO WS-WORK1                                 dbank05p
012100        DIVIDE 60 INTO WS-WORK1                                   dbank05p
012200          GIVING WS-WORK2                                         dbank05p
012300            REMAINDER WS-BNKTXN-AIX1-RID-MM                       dbank05p
012400        MOVE WS-WORK2 TO WS-WORK1                                 dbank05p
012500        MOVE WS-WORK1 TO WS-BNKTXN-AIX1-RID-HH                    dbank05p
012600     END-IF.                                                      dbank05p
012700     EXEC CICS STARTBR FILE('BNKTXN1')                            dbank05p
012800                       RIDFLD(WS-BNKTXN-AIX1-RID)                 dbank05p
012900                       GTEQ                                       dbank05p
013000     END-EXEC.                                                    dbank05p
013100                                                                  dbank05p
013200     MOVE 0 TO WS-SUB1.                                           dbank05p
013300                                                                  dbank05p
013400***************************************************************** dbank05p
013500* Now attempt to get the requested records                      * dbank05p
013600***************************************************************** dbank05p
013700 TRANSACTION-FETCH-LOOP.                                          dbank05p
013800     ADD 1 TO WS-SUB1.                                            dbank05p
013900     IF WS-SUB1 IS GREATER THAN 9                                 dbank05p
014000        SET CD05-IS-MORE-DATA TO TRUE                             dbank05p
014100        GO TO TRANSACTION-FETCH-LOOP-EXIT                         dbank05p
014200     END-IF.                                                      dbank05p
014300     IF CD05-START-EQUAL OR                                       dbank05p
014400        CD05-START-HIGH                                           dbank05p
014500        EXEC CICS READNEXT FILE('BNKTXN1')                        dbank05p
014600                           INTO(WS-BNKTXN-REC)                    dbank05p
014700                           LENGTH(LENGTH OF WS-BNKTXN-REC)        dbank05p
014800                           RIDFLD(WS-BNKTXN-AIX1-RID)             dbank05p
014900                           RESP(WS-RESP)                          dbank05p
015000        END-EXEC                                                  dbank05p
015100     END-IF.                                                      dbank05p
015200* If we are reading 'low' then we need to read backwards. This is dbank05p
015300* OK except we want the record prior to the on with the provided  dbank05p
015400* key so we throw away the 1st record.                            dbank05p
015500     IF CD05-START-LOW                                            dbank05p
015600        EXEC CICS READPREV FILE('BNKTXN1')                        dbank05p
015700                           INTO(WS-BNKTXN-REC)                    dbank05p
015800                           LENGTH(LENGTH OF WS-BNKTXN-REC)        dbank05p
015900                           RIDFLD(WS-BNKTXN-AIX1-RID)             dbank05p
016000                           RESP(WS-RESP)                          dbank05p
016100        END-EXEC                                                  dbank05p
016200        IF WS-SUB1 IS EQUAL TO 1                                  dbank05p
016300           EXEC CICS READPREV FILE('BNKTXN1')                     dbank05p
016400                              INTO(WS-BNKTXN-REC)                 dbank05p
016500                              LENGTH(LENGTH OF WS-BNKTXN-REC)     dbank05p
016600                              RIDFLD(WS-BNKTXN-AIX1-RID)          dbank05p
016700                              RESP(WS-RESP)                       dbank05p
016800           END-EXEC                                               dbank05p
016900        END-IF                                                    dbank05p
017000     END-IF.                                                      dbank05p
017100                                                                  dbank05p
017200***************************************************************** dbank05p
017300* Did we get the record OK                                      * dbank05p
017400***************************************************************** dbank05p
017500     IF CD05-START-LOW                                            dbank05p
017600        IF WS-SUB1 IS GREATER THAN 8                              dbank05p
017700           MOVE WS-SUB1 TO WS-SUB2                                dbank05p
017800        ELSE                                                      dbank05p
017900           SUBTRACT WS-SUB1 FROM 9 GIVING WS-SUB2                 dbank05p
018000        END-IF                                                    dbank05p
018100     ELSE                                                         dbank05p
018200        MOVE WS-SUB1 TO WS-SUB2                                   dbank05p
018300     END-IF.                                                      dbank05p
018400     IF WS-RESP IS EQUAL TO DFHRESP(NORMAL)                       dbank05p
018500        IF CD05I-ACC IS EQUAL TO BTX-REC-ACCNO                    dbank05p
018600           SET CD05-IS-DATA TO TRUE                               dbank05p
018700           MOVE BTX-REC-TIMESTAMP TO CD05O-ID (WS-SUB2)           dbank05p
018800           MOVE BTX-REC-AMOUNT TO CD05O-AMT-N (WS-SUB2)           dbank05p
018900           MOVE BTX-REC-DATA-OLD TO TXN-T1-OLD                    dbank05p
019000           MOVE TXN-T1-OLD-DESC TO CD05O-DESC (WS-SUB2)           dbank05p
019100           GO TO TRANSACTION-FETCH-LOOP                           dbank05p
019200        ELSE                                                      dbank05p
019300           SET CD05-NO-MORE-DATA TO TRUE                          dbank05p
019400           GO TO TRANSACTION-FETCH-LOOP-EXIT                      dbank05p
019500        END-IF                                                    dbank05p
019600     ELSE                                                         dbank05p
019700        SET CD05-NO-MORE-DATA TO TRUE                             dbank05p
019800        GO TO TRANSACTION-FETCH-LOOP-EXIT                         dbank05p
019900     END-IF.                                                      dbank05p
020000                                                                  dbank05p
020100 TRANSACTION-FETCH-LOOP-EXIT.                                     dbank05p
020200     EXEC CICS ENDBR FILE('BNKTXN1')                              dbank05p
020300     END-EXEC.                                                    dbank05p
020400                                                                  dbank05p
020500***************************************************************** dbank05p
020600* Move the result back to the callers area                      * dbank05p
020700***************************************************************** dbank05p
020800     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).       dbank05p
020900                                                                  dbank05p
021000***************************************************************** dbank05p
021100* Return to our caller                                          * dbank05p
021200***************************************************************** dbank05p
021300 COPY CRETURN.                                                    dbank05p
021400                                                                  dbank05p
021500* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     dbank05p
