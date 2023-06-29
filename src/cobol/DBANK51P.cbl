000100***************************************************************** dbank51p
000200*                                                               * dbank51p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * dbank51p
000400*   This demonstration program is provided for use by users     * dbank51p
000500*   of Micro Focus products and may be used, modified and       * dbank51p
000600*   distributed as part of your application provided that       * dbank51p
000700*   you properly acknowledge the copyright of Micro Focus       * dbank51p
000800*   in this material.                                           * dbank51p
000900*                                                               * dbank51p
001000***************************************************************** dbank51p
001100                                                                  dbank51p
001200***************************************************************** dbank51p
001300* Program:     DBANK51P.CBL                                     * dbank51p
001400* Function:    Sequential read of bank data for batch job       * dbank51p
001500*              VSAM version                                     * dbank51p
001600***************************************************************** dbank51p
001700                                                                  dbank51p
001800 IDENTIFICATION DIVISION.                                         dbank51p
001900 PROGRAM-ID.                                                      dbank51p
002000     DBANK51P.                                                    dbank51p
002100 DATE-WRITTEN.                                                    dbank51p
002200     September 2002.                                              dbank51p
002300 DATE-COMPILED.                                                   dbank51p
002400     Today.                                                       dbank51p
002500                                                                  dbank51p
002600 ENVIRONMENT DIVISION.                                            dbank51p
002700                                                                  dbank51p
002800 INPUT-OUTPUT   SECTION.                                          dbank51p
002900   FILE-CONTROL.                                                  dbank51p
003000     SELECT BNKACC-FILE                                           dbank51p
003100            ASSIGN       TO BNKACC                                dbank51p
003200            ORGANIZATION IS INDEXED                               dbank51p
003300            ACCESS MODE  IS SEQUENTIAL                            dbank51p
003400            RECORD KEY   IS BAC-REC-ACCNO                         dbank51p
003500            ALTERNATE KEY IS BAC-REC-PID WITH DUPLICATES          dbank51p
003600            FILE STATUS  IS WS-BNKACC-STATUS.                     dbank51p
003700                                                                  dbank51p
003800     SELECT BNKCUST-FILE                                          dbank51p
003900            ASSIGN       TO BNKCUST                               dbank51p
004000            ORGANIZATION IS INDEXED                               dbank51p
004100            ACCESS MODE  IS RANDOM                                dbank51p
004200            RECORD KEY   IS BCS-REC-PID                           dbank51p
004300            ALTERNATE KEY IS BCS-REC-NAME                         dbank51p
004400              WITH DUPLICATES                                     dbank51p
004500            ALTERNATE KEY IS BCS-REC-NAME-FF                      dbank51p
004600              WITH DUPLICATES                                     dbank51p
004700            FILE STATUS  IS WS-BNKCUST-STATUS.                    dbank51p
004800                                                                  dbank51p
004900     SELECT BNKATYP-FILE                                          dbank51p
005000            ASSIGN       TO BNKATYP                               dbank51p
005100            ORGANIZATION IS INDEXED                               dbank51p
005200            ACCESS MODE  IS RANDOM                                dbank51p
005300            RECORD KEY   IS BAT-REC-TYPE                          dbank51p
005400            FILE STATUS  IS WS-BNKATYP-STATUS.                    dbank51p
005500                                                                  dbank51p
005600 DATA DIVISION.                                                   dbank51p
005700                                                                  dbank51p
005800 FILE SECTION.                                                    dbank51p
005900 FD  BNKACC-FILE.                                                 dbank51p
006000 01  BNKACC-REC.                                                  dbank51p
006100 COPY CBANKVAC.                                                   dbank51p
006200                                                                  dbank51p
006300 FD  BNKCUST-FILE.                                                dbank51p
006400 01  BNKCUST-REC.                                                 dbank51p
006500 COPY CBANKVCS.                                                   dbank51p
006600                                                                  dbank51p
006700 FD  BNKATYP-FILE.                                                dbank51p
006800 01  BNKATYP-REC.                                                 dbank51p
006900 COPY CBANKVAT.                                                   dbank51p
007000                                                                  dbank51p
007100 WORKING-STORAGE SECTION.                                         dbank51p
007200 01  WS-MISC-STORAGE.                                             dbank51p
007300   05  WS-PROGRAM-ID                         PIC X(8)             dbank51p
007400       VALUE 'DBANK51P'.                                          dbank51p
007500   05  WS-COMMAREA-LENGTH                    PIC 9(5).            dbank51p
007600   05  WS-SUB1                               PIC S9(4) COMP.      dbank51p
007700                                                                  dbank51p
007800   05  WS-BNKACC-STATUS.                                          dbank51p
007900     10  WS-BNKACC-STAT1                     PIC X(1).            dbank51p
008000     10  WS-BNKACC-STAT2                     PIC X(1).            dbank51p
008100                                                                  dbank51p
008200   05  WS-BNKCUST-STATUS.                                         dbank51p
008300     10  WS-BNKCUST-STAT1                    PIC X(1).            dbank51p
008400     10  WS-BNKCUST-STAT2                    PIC X(1).            dbank51p
008500                                                                  dbank51p
008600   05  WS-BNKATYP-STATUS.                                         dbank51p
008700     10  WS-BNKATYP-STAT1                    PIC X(1).            dbank51p
008800     10  WS-BNKATYP-STAT2                    PIC X(1).            dbank51p
008900                                                                  dbank51p
009000 01  WS-COMMAREA.                                                 dbank51p
009100 COPY CIOFUNCS.                                                   dbank51p
009200 COPY CBANKD51.                                                   dbank51p
009300                                                                  dbank51p
009400 LINKAGE SECTION.                                                 dbank51p
009500 01  DFHCOMMAREA.                                                 dbank51p
009600   05  LK-COMMAREA                           PIC X(1)             dbank51p
009700         OCCURS 1 TO 4096 TIMES                                   dbank51p
009800           DEPENDING ON WS-COMMAREA-LENGTH.                       dbank51p
009900                                                                  dbank51p
010000 PROCEDURE DIVISION USING DFHCOMMAREA.                            dbank51p
010100***************************************************************** dbank51p
010200* Move the passed data to our area                              * dbank51p
010300***************************************************************** dbank51p
010400     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            dbank51p
010500     MOVE DFHCOMMAREA TO WS-COMMAREA.                             dbank51p
010600                                                                  dbank51p
010700***************************************************************** dbank51p
010800* Initialize our output area                                    * dbank51p
010900***************************************************************** dbank51p
011000     MOVE SPACES TO CD51O-DATA.                                   dbank51p
011100                                                                  dbank51p
011200***************************************************************** dbank51p
011300* Check what is required                                        * dbank51p
011400***************************************************************** dbank51p
011500     EVALUATE TRUE                                                dbank51p
011600       WHEN IO-REQUEST-FUNCTION-OPEN                              dbank51p
011700        PERFORM OPEN-FILE THRU                                    dbank51p
011800                OPEN-FILE-EXIT                                    dbank51p
011900       WHEN IO-REQUEST-FUNCTION-READ                              dbank51p
012000        PERFORM READ-FILE THRU                                    dbank51p
012100                READ-FILE-EXIT                                    dbank51p
012200       WHEN IO-REQUEST-FUNCTION-CLOSE                             dbank51p
012300        PERFORM CLOSE-FILE THRU                                   dbank51p
012400                CLOSE-FILE-EXIT                                   dbank51p
012500       WHEN OTHER                                                 dbank51p
012600        SET IO-REQUEST-STATUS-ERROR TO TRUE                       dbank51p
012700     END-EVALUATE.                                                dbank51p
012800                                                                  dbank51p
012900***************************************************************** dbank51p
013000* Move the result back to the callers area                      * dbank51p
013100***************************************************************** dbank51p
013200     MOVE WS-COMMAREA TO DFHCOMMAREA  (1:WS-COMMAREA-LENGTH).     dbank51p
013300                                                                  dbank51p
013400***************************************************************** dbank51p
013500* Return to our caller                                          * dbank51p
013600***************************************************************** dbank51p
013700     GOBACK.                                                      dbank51p
013800                                                                  dbank51p
013900                                                                  dbank51p
014000***************************************************************** dbank51p
014100* Open the file so we can read ACC sequentially, others randomly* dbank51p
014200***************************************************************** dbank51p
014300 OPEN-FILE.                                                       dbank51p
014400     OPEN INPUT BNKACC-FILE.                                      dbank51p
014500     OPEN INPUT BNKCUST-FILE.                                     dbank51p
014600     OPEN INPUT BNKATYP-FILE.                                     dbank51p
014700     IF CD51-REQUESTED-ALL                                        dbank51p
014800        MOVE LOW-VALUES TO BAC-REC-PID                            dbank51p
014900        START BNKACC-FILE KEY GREATER THAN BAC-REC-PID            dbank51p
015000     ELSE                                                         dbank51p
015100        MOVE CD51I-PID TO BAC-REC-PID                             dbank51p
015200        START BNKACC-FILE KEY EQUAL BAC-REC-PID                   dbank51p
015300     END-IF                                                       dbank51p
015400     IF WS-BNKACC-STATUS = '00' AND                               dbank51p
015500        WS-BNKCUST-STATUS = '00' AND                              dbank51p
015600        WS-BNKATYP-STATUS = '00'                                  dbank51p
015700        SET IO-REQUEST-STATUS-OK TO TRUE                          dbank51p
015800     ELSE                                                         dbank51p
015900        SET IO-REQUEST-STATUS-ERROR TO TRUE                       dbank51p
016000     END-IF.                                                      dbank51p
016100 OPEN-FILE-EXIT.                                                  dbank51p
016200     EXIT.                                                        dbank51p
016300                                                                  dbank51p
016400***************************************************************** dbank51p
016500* Read sequentially through the customer file                   * dbank51p
016600***************************************************************** dbank51p
016700 READ-FILE.                                                       dbank51p
016800     READ BNKACC-FILE.                                            dbank51p
016900* If key is greater than the one we want, fake end-of-file        dbank51p
017000     IF NOT CD51-REQUESTED-ALL AND                                dbank51p
017100        BAC-REC-PID IS NOT EQUAL TO CD51I-PID                     dbank51p
017200        MOVE '10' TO WS-BNKACC-STATUS                             dbank51p
017300     END-IF.                                                      dbank51p
017400* Was read ok?                                                    dbank51p
017500     IF WS-BNKACC-STATUS IS EQUAL TO '00'                         dbank51p
017600        SET IO-REQUEST-STATUS-OK TO TRUE                          dbank51p
017700     END-IF.                                                      dbank51p
017800* Was read a duplicate key?                                       dbank51p
017900     IF WS-BNKACC-STATUS IS EQUAL TO '02'                         dbank51p
018000        MOVE '00' TO WS-BNKACC-STATUS                             dbank51p
018100        SET IO-REQUEST-STATUS-OK TO TRUE                          dbank51p
018200     END-IF.                                                      dbank51p
018300* Was read at end-of-file?                                        dbank51p
018400     IF WS-BNKACC-STATUS IS EQUAL TO '10'                         dbank51p
018500        SET IO-REQUEST-STATUS-EOF TO TRUE                         dbank51p
018600     END-IF.                                                      dbank51p
018700     IF WS-BNKACC-STATUS IS NOT EQUAL TO '00' AND                 dbank51p
018800        WS-BNKACC-STATUS IS NOT EQUAL TO '10'                     dbank51p
018900        SET IO-REQUEST-STATUS-ERROR TO TRUE                       dbank51p
019000     END-IF.                                                      dbank51p
019100     IF WS-BNKACC-STATUS IS EQUAL TO '00'                         dbank51p
019200        MOVE BAC-REC-PID TO CD51O-PID                             dbank51p
019300        MOVE BAC-REC-ACCNO TO CD51O-ACC-NO                        dbank51p
019400        MOVE BAC-REC-BALANCE TO CD51O-ACC-CURR-BAL                dbank51p
019500        MOVE BAC-REC-LAST-STMT-DTE TO CD51O-ACC-LAST-STMT-DTE     dbank51p
019600        MOVE BAC-REC-LAST-STMT-BAL TO CD51O-ACC-LAST-STMT-BAL     dbank51p
019700        IF BAC-REC-PID IS NOT EQUAL TO BCS-REC-PID                dbank51p
019800           MOVE BAC-REC-PID TO BCS-REC-PID                        dbank51p
019900           READ BNKCUST-FILE                                      dbank51p
020000           IF WS-BNKCUST-STATUS IS NOT EQUAL TO '00'              dbank51p
020100              MOVE SPACES TO BCS-RECORD                           dbank51p
020200              MOVE 'Customer name unavailable' TO BCS-REC-NAME    dbank51p
020300           END-IF                                                 dbank51p
020400        END-IF                                                    dbank51p
020500                                                                  dbank51p
020600        MOVE BCS-REC-NAME TO CD51O-NAME                           dbank51p
020700        MOVE BCS-REC-ADDR1 TO CD51O-ADDR1                         dbank51p
020800        MOVE BCS-REC-ADDR2 TO CD51O-ADDR2                         dbank51p
020900        MOVE BCS-REC-STATE TO CD51O-STATE                         dbank51p
021000        MOVE BCS-REC-CNTRY TO CD51O-CNTRY                         dbank51p
021100        MOVE BCS-REC-POST-CODE TO CD51O-POST-CODE                 dbank51p
021200        MOVE BCS-REC-EMAIL TO CD51O-EMAIL                         dbank51p
021300                                                                  dbank51p
021400        MOVE BAC-REC-TYPE TO BAT-REC-TYPE                         dbank51p
021500        READ BNKATYP-FILE                                         dbank51p
021600        IF WS-BNKATYP-STATUS IS NOT EQUAL TO '00'                 dbank51p
021700           MOVE 'A/C description unavailable' TO CD51O-ACC-DESC   dbank51p
021800        ELSE                                                      dbank51p
021900           MOVE BAT-REC-DESC TO CD51O-ACC-DESC                    dbank51p
022000        END-IF                                                    dbank51p
022100     END-IF.                                                      dbank51p
022200 READ-FILE-EXIT.                                                  dbank51p
022300     EXIT.                                                        dbank51p
022400                                                                  dbank51p
022500***************************************************************** dbank51p
022600* Close the file                                                * dbank51p
022700***************************************************************** dbank51p
022800 CLOSE-FILE.                                                      dbank51p
022900     CLOSE BNKCUST-FILE.                                          dbank51p
023000     CLOSE BNKACC-FILE.                                           dbank51p
023100     CLOSE BNKATYP-FILE.                                          dbank51p
023200     IF WS-BNKCUST-STATUS = '00' AND                              dbank51p
023300        WS-BNKACC-STATUS = '00' AND                               dbank51p
023400        WS-BNKATYP-STATUS = '00'                                  dbank51p
023500        SET IO-REQUEST-STATUS-OK TO TRUE                          dbank51p
023600     ELSE                                                         dbank51p
023700       SET IO-REQUEST-STATUS-ERROR TO TRUE                        dbank51p
023800     END-IF.                                                      dbank51p
023900 CLOSE-FILE-EXIT.                                                 dbank51p
024000     EXIT.                                                        dbank51p
024100                                                                  dbank51p
024200* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     dbank51p
