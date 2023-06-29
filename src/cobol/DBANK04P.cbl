000100***************************************************************** dbank04p
000200*                                                               * dbank04p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * dbank04p
000400*   This demonstration program is provided for use by users     * dbank04p
000500*   of Micro Focus products and may be used, modified and       * dbank04p
000600*   distributed as part of your application provided that       * dbank04p
000700*   you properly acknowledge the copyright of Micro Focus       * dbank04p
000800*   in this material.                                           * dbank04p
000900*                                                               * dbank04p
001000***************************************************************** dbank04p
001100                                                                  dbank04p
001200***************************************************************** dbank04p
001300* Program:     DBANK04P.CBL                                     * dbank04p
001400* Function:    Update acount balances                           * dbank04p
001500*              VSAM Version                                     * dbank04p
001600***************************************************************** dbank04p
001700                                                                  dbank04p
001800 IDENTIFICATION DIVISION.                                         dbank04p
001900 PROGRAM-ID.                                                      dbank04p
002000     DBANK04P.                                                    dbank04p
002100 DATE-WRITTEN.                                                    dbank04p
002200     September 2002.                                              dbank04p
002300 DATE-COMPILED.                                                   dbank04p
002400     Today.                                                       dbank04p
002500                                                                  dbank04p
002600 ENVIRONMENT DIVISION.                                            dbank04p
002700                                                                  dbank04p
002800 DATA DIVISION.                                                   dbank04p
002900                                                                  dbank04p
003000 WORKING-STORAGE SECTION.                                         dbank04p
003100 01  WS-MISC-STORAGE.                                             dbank04p
003200   05  WS-PROGRAM-ID                         PIC X(8)             dbank04p
003300       VALUE 'DBANK04P'.                                          dbank04p
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            dbank04p
003500   05  WS-READ-TOKEN-FROM                    PIC S9(8) COMP.      dbank04p
003600   05  WS-READ-TOKEN-TO                      PIC S9(8) COMP.      dbank04p
003700   05  WS-RESP                               PIC S9(8) COMP.      dbank04p
003800   05  WS-BNKACC-FROM-RID                    PIC X(9).            dbank04p
003900   05  WS-BNKACC-TO-RID                      PIC X(9).            dbank04p
004000                                                                  dbank04p
004100 COPY CTSTAMPD.                                                   dbank04p
004200                                                                  dbank04p
004300 01  WS-BNKACC-FROM-REC.                                          dbank04p
004400 COPY CBANKVAC.                                                   dbank04p
004500                                                                  dbank04p
004600 01  WS-BNKACC-TO-REC.                                            dbank04p
004700 COPY CBANKVAC.                                                   dbank04p
004800                                                                  dbank04p
004900 01  WS-COMMAREA.                                                 dbank04p
005000 COPY CBANKD04.                                                   dbank04p
005100                                                                  dbank04p
005200 COPY CABENDD.                                                    dbank04p
005300                                                                  dbank04p
005400 LINKAGE SECTION.                                                 dbank04p
005500 01  DFHCOMMAREA.                                                 dbank04p
005600   05  LK-COMMAREA                           PIC X(1)             dbank04p
005700       OCCURS 1 TO 4096 TIMES                                     dbank04p
005800         DEPENDING ON WS-COMMAREA-LENGTH.                         dbank04p
005900                                                                  dbank04p
006000 COPY CENTRY.                                                     dbank04p
006100***************************************************************** dbank04p
006200* Move the passed data to our area                              * dbank04p
006300***************************************************************** dbank04p
006400     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            dbank04p
006500     MOVE DFHCOMMAREA TO WS-COMMAREA.                             dbank04p
006600                                                                  dbank04p
006700***************************************************************** dbank04p
006800* Initialize our output area                                    * dbank04p
006900***************************************************************** dbank04p
007000     MOVE SPACES TO CD04O-DATA.                                   dbank04p
007100     SET CD04O-UPDATE-FAIL TO TRUE.                               dbank04p
007200     MOVE '0001-01-01-00.000.00.00000' TO CD04O-TIMESTAMP.        dbank04p
007300                                                                  dbank04p
007400***************************************************************** dbank04p
007500* Try to the the 'from' account to check the balance            * dbank04p
007600***************************************************************** dbank04p
007700     MOVE CD04I-FROM-ACC TO WS-BNKACC-FROM-RID.                   dbank04p
007800     EXEC CICS READ FILE('BNKACC')                                dbank04p
007900                    UPDATE                                        dbank04p
008000                    INTO(WS-BNKACC-FROM-REC)                      dbank04p
008100                    LENGTH(LENGTH OF WS-BNKACC-FROM-REC)          dbank04p
008200                    RIDFLD(WS-BNKACC-FROM-RID)                    dbank04p
008300                    TOKEN(WS-READ-TOKEN-FROM)                     dbank04p
008400                    RESP(WS-RESP)                                 dbank04p
008500     END-EXEC.                                                    dbank04p
008600                                                                  dbank04p
008700***************************************************************** dbank04p
008800* Did we get the record OK                                      * dbank04p
008900***************************************************************** dbank04p
009000     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)                   dbank04p
009100        MOVE 'Unable to read FROM account details'                dbank04p
009200          TO CD04O-MSG                                            dbank04p
009300        GO TO DBANK04P-EXIT                                       dbank04p
009400     END-IF.                                                      dbank04p
009500     IF CD04I-FROM-OLD-BAL IS NOT EQUAL TO                        dbank04p
009600          BAC-REC-BALANCE IN WS-BNKACC-FROM-REC                   dbank04p
009700        MOVE 'FROM account balance has changed'                   dbank04p
009800          TO CD04O-MSG                                            dbank04p
009900        GO TO DBANK04P-EXIT                                       dbank04p
010000     END-IF.                                                      dbank04p
010100                                                                  dbank04p
010200***************************************************************** dbank04p
010300* Try to the the 'to' account to check the balance              * dbank04p
010400***************************************************************** dbank04p
010500     MOVE CD04I-TO-ACC TO WS-BNKACC-TO-RID.                       dbank04p
010600     EXEC CICS READ FILE('BNKACC')                                dbank04p
010700                    UPDATE                                        dbank04p
010800                    INTO(WS-BNKACC-TO-REC)                        dbank04p
010900                    LENGTH(LENGTH OF WS-BNKACC-TO-REC)            dbank04p
011000                    RIDFLD(WS-BNKACC-TO-RID)                      dbank04p
011100                    TOKEN(WS-READ-TOKEN-TO)                       dbank04p
011200                    RESP(WS-RESP)                                 dbank04p
011300     END-EXEC.                                                    dbank04p
011400                                                                  dbank04p
011500***************************************************************** dbank04p
011600* Did we get the record OK                                      * dbank04p
011700***************************************************************** dbank04p
011800     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)                   dbank04p
011900        MOVE 'Unable to read TO account details'                  dbank04p
012000          TO CD04O-MSG                                            dbank04p
012100        GO TO DBANK04P-EXIT                                       dbank04p
012200     END-IF.                                                      dbank04p
012300     IF CD04I-TO-OLD-BAL IS NOT EQUAL TO                          dbank04p
012400          BAC-REC-BALANCE IN WS-BNKACC-TO-REC                     dbank04p
012500        MOVE 'TO account balance has changed'                     dbank04p
012600          TO CD04O-MSG                                            dbank04p
012700        GO TO DBANK04P-EXIT                                       dbank04p
012800     END-IF.                                                      dbank04p
012900                                                                  dbank04p
013000***************************************************************** dbank04p
013100* Try to update the records                                     * dbank04p
013200***************************************************************** dbank04p
013300     MOVE CD04I-FROM-NEW-BAL                                      dbank04p
013400       TO BAC-REC-BALANCE IN WS-BNKACC-FROM-REC.                  dbank04p
013500     EXEC CICS REWRITE FILE('BNKACC')                             dbank04p
013600                       FROM(WS-BNKACC-FROM-REC)                   dbank04p
013700                       LENGTH(LENGTH OF WS-BNKACC-FROM-REC)       dbank04p
013800                       TOKEN(WS-READ-TOKEN-FROM)                  dbank04p
013900                       RESP(WS-RESP)                              dbank04p
014000     END-EXEC.                                                    dbank04p
014100     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)                   dbank04p
014200        MOVE 'Unable to rewrite FROM account details'             dbank04p
014300          TO CD04O-MSG                                            dbank04p
014400        GO TO DBANK04P-EXIT                                       dbank04p
014500     END-IF.                                                      dbank04p
014600                                                                  dbank04p
014700     MOVE CD04I-TO-NEW-BAL                                        dbank04p
014800       TO BAC-REC-BALANCE IN WS-BNKACC-TO-REC.                    dbank04p
014900     EXEC CICS REWRITE FILE('BNKACC')                             dbank04p
015000                       FROM(WS-BNKACC-TO-REC)                     dbank04p
015100                       LENGTH(LENGTH OF WS-BNKACC-TO-REC)         dbank04p
015200                       TOKEN(WS-READ-TOKEN-TO)                    dbank04p
015300                       RESP(WS-RESP)                              dbank04p
015400     END-EXEC.                                                    dbank04p
015500     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)                   dbank04p
015600        MOVE 'Unable to rewrite TO account details'               dbank04p
015700          TO CD04O-MSG                                            dbank04p
015800        GO TO DBANK04P-EXIT                                       dbank04p
015900     END-IF.                                                      dbank04p
016000                                                                  dbank04p
016100***************************************************************** dbank04p
016200* If we got this far then the accounts should have been updated * dbank04p
016300***************************************************************** dbank04p
016400* Simulate SQL TIMESTAMP function                                 dbank04p
016500 COPY CTSTAMPP.                                                   dbank04p
016600     MOVE WS-TIMESTAMP TO CD04O-TIMESTAMP.                        dbank04p
016700     SET CD04O-UPDATE-OK TO TRUE.                                 dbank04p
016800                                                                  dbank04p
016900 DBANK04P-EXIT.                                                   dbank04p
017000***************************************************************** dbank04p
017100* Move the result back to the callers area                      * dbank04p
017200***************************************************************** dbank04p
017300     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).       dbank04p
017400                                                                  dbank04p
017500***************************************************************** dbank04p
017600* Return to our caller                                          * dbank04p
017700***************************************************************** dbank04p
017800 COPY CRETURN.                                                    dbank04p
017900                                                                  dbank04p
018000* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     dbank04p
