000100***************************************************************** dbank52p
000200*                                                               * dbank52p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * dbank52p
000400*   This demonstration program is provided for use by users     * dbank52p
000500*   of Micro Focus products and may be used, modified and       * dbank52p
000600*   distributed as part of your application provided that       * dbank52p
000700*   you properly acknowledge the copyright of Micro Focus       * dbank52p
000800*   in this material.                                           * dbank52p
000900*                                                               * dbank52p
001000***************************************************************** dbank52p
001100                                                                  dbank52p
001200***************************************************************** dbank52p
001300* Program:     DBANK52P.CBL                                     * dbank52p
001400* Function:    Sequential read of bank data for batch job       * dbank52p
001500*              VSAM version                                     * dbank52p
001600***************************************************************** dbank52p
001700                                                                  dbank52p
001800 IDENTIFICATION DIVISION.                                         dbank52p
001900 PROGRAM-ID.                                                      dbank52p
002000     DBANK52P.                                                    dbank52p
002100 DATE-WRITTEN.                                                    dbank52p
002200     September 2002.                                              dbank52p
002300 DATE-COMPILED.                                                   dbank52p
002400     Today.                                                       dbank52p
002500                                                                  dbank52p
002600 ENVIRONMENT DIVISION.                                            dbank52p
002700                                                                  dbank52p
002800 INPUT-OUTPUT   SECTION.                                          dbank52p
002900   FILE-CONTROL.                                                  dbank52p
003000     SELECT BNKTXN-FILE                                           dbank52p
003100            ASSIGN       TO BNKTXN                                dbank52p
003200            ORGANIZATION IS INDEXED                               dbank52p
003300            ACCESS MODE  IS SEQUENTIAL                            dbank52p
003400            RECORD KEY   IS BTX-REC-TIMESTAMP                     dbank52p
003500            ALTERNATE KEY IS BTX-REC-ALTKEY1 WITH DUPLICATES      dbank52p
003600            FILE STATUS  IS WS-BNKTXN-STATUS.                     dbank52p
003700                                                                  dbank52p
003800 DATA DIVISION.                                                   dbank52p
003900                                                                  dbank52p
004000 FILE SECTION.                                                    dbank52p
004100 FD  BNKTXN-FILE.                                                 dbank52p
004200 01  BNKTXN-REC.                                                  dbank52p
004300 COPY CBANKVTX.                                                   dbank52p
004400                                                                  dbank52p
004500 WORKING-STORAGE SECTION.                                         dbank52p
004600 01  WS-MISC-STORAGE.                                             dbank52p
004700   05  WS-PROGRAM-ID                         PIC X(8)             dbank52p
004800       VALUE 'DBANK52P'.                                          dbank52p
004900   05  WS-COMMAREA-LENGTH                    PIC 9(5).            dbank52p
005000   05  WS-SUB1                               PIC S9(4) COMP.      dbank52p
005100                                                                  dbank52p
005200   05  WS-BNKTXN-STATUS.                                          dbank52p
005300     10  WS-BNKTXN-STAT1                     PIC X(1).            dbank52p
005400     10  WS-BNKTXN-STAT2                     PIC X(1).            dbank52p
005500                                                                  dbank52p
005600 01  WS-COMMAREA.                                                 dbank52p
005700 COPY CIOFUNCS.                                                   dbank52p
005800 COPY CBANKD51.                                                   dbank52p
005900 COPY CBANKD52.                                                   dbank52p
006000                                                                  dbank52p
006100 COPY CBANKTXD.                                                   dbank52p
006200                                                                  dbank52p
006300 LINKAGE SECTION.                                                 dbank52p
006400 01  DFHCOMMAREA.                                                 dbank52p
006500   05  LK-COMMAREA                           PIC X(1)             dbank52p
006600         OCCURS 1 TO 4096 TIMES                                   dbank52p
006700           DEPENDING ON WS-COMMAREA-LENGTH.                       dbank52p
006800                                                                  dbank52p
006900 PROCEDURE DIVISION USING DFHCOMMAREA.                            dbank52p
007000***************************************************************** dbank52p
007100* Move the passed data to our area                              * dbank52p
007200***************************************************************** dbank52p
007300     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            dbank52p
007400     MOVE DFHCOMMAREA TO WS-COMMAREA.                             dbank52p
007500                                                                  dbank52p
007600***************************************************************** dbank52p
007700* Initialize our output area                                    * dbank52p
007800***************************************************************** dbank52p
007900     MOVE SPACES TO CD52O-DATA.                                   dbank52p
008000                                                                  dbank52p
008100***************************************************************** dbank52p
008200* Check what is required                                        * dbank52p
008300***************************************************************** dbank52p
008400     EVALUATE TRUE                                                dbank52p
008500       WHEN IO-REQUEST-FUNCTION-OPEN                              dbank52p
008600        PERFORM OPEN-FILE THRU                                    dbank52p
008700                OPEN-FILE-EXIT                                    dbank52p
008800       WHEN IO-REQUEST-FUNCTION-READ                              dbank52p
008900        PERFORM READ-FILE THRU                                    dbank52p
009000                READ-FILE-EXIT                                    dbank52p
009100       WHEN IO-REQUEST-FUNCTION-CLOSE                             dbank52p
009200        PERFORM CLOSE-FILE THRU                                   dbank52p
009300                CLOSE-FILE-EXIT                                   dbank52p
009400       WHEN OTHER                                                 dbank52p
009500        SET IO-REQUEST-STATUS-ERROR TO TRUE                       dbank52p
009600     END-EVALUATE.                                                dbank52p
009700                                                                  dbank52p
009800***************************************************************** dbank52p
009900* Move the result back to the callers area                      * dbank52p
010000***************************************************************** dbank52p
010100     MOVE WS-COMMAREA TO DFHCOMMAREA  (1:WS-COMMAREA-LENGTH).     dbank52p
010200                                                                  dbank52p
010300***************************************************************** dbank52p
010400* Return to our caller                                          * dbank52p
010500***************************************************************** dbank52p
010600     GOBACK.                                                      dbank52p
010700                                                                  dbank52p
010800                                                                  dbank52p
010900***************************************************************** dbank52p
011000* Open the file so we can read TXN sequentially                 * dbank52p
011100***************************************************************** dbank52p
011200 OPEN-FILE.                                                       dbank52p
011300     OPEN INPUT BNKTXN-FILE.                                      dbank52p
011400     IF WS-BNKTXN-STATUS = '00'                                   dbank52p
011500        SET IO-REQUEST-STATUS-OK TO TRUE                          dbank52p
011600     ELSE                                                         dbank52p
011700        SET IO-REQUEST-STATUS-ERROR TO TRUE                       dbank52p
011800     END-IF.                                                      dbank52p
011900 OPEN-FILE-EXIT.                                                  dbank52p
012000     EXIT.                                                        dbank52p
012100                                                                  dbank52p
012200***************************************************************** dbank52p
012300* Read sequentially through the transaction file                * dbank52p
012400***************************************************************** dbank52p
012500 READ-FILE.                                                       dbank52p
012600     READ BNKTXN-FILE.                                            dbank52p
012700* Was read ok?                                                    dbank52p
012800     IF WS-BNKTXN-STATUS IS EQUAL TO '00'                         dbank52p
012900        SET IO-REQUEST-STATUS-OK TO TRUE                          dbank52p
013000     END-IF.                                                      dbank52p
013100* Was read at end-of-file?                                        dbank52p
013200     IF WS-BNKTXN-STATUS IS EQUAL TO '10'                         dbank52p
013300        SET IO-REQUEST-STATUS-EOF TO TRUE                         dbank52p
013400     END-IF.                                                      dbank52p
013500     IF WS-BNKTXN-STATUS IS NOT EQUAL TO '00' AND                 dbank52p
013600        WS-BNKTXN-STATUS IS NOT EQUAL TO '10'                     dbank52p
013700        SET IO-REQUEST-STATUS-ERROR TO TRUE                       dbank52p
013800     END-IF.                                                      dbank52p
013900     IF WS-BNKTXN-STATUS IS EQUAL TO '00'                         dbank52p
014000        IF BTX-REC-TYPE IS EQUAL TO '1' AND                       dbank52p
014100           (BTX-REC-PID IS EQUAL TO CD52I-PID OR                  dbank52p
014200            CD52-REQUESTED-ALL)                                   dbank52p
014300           MOVE BTX-REC-PID TO CD52O-PID                          dbank52p
014400           MOVE BTX-REC-ACCNO TO CD52O-ACC-NO                     dbank52p
014500           MOVE BTX-REC-TIMESTAMP TO CD52O-TIMESTAMP              dbank52p
014600           MOVE BTX-REC-AMOUNT TO CD52O-AMOUNT                    dbank52p
014700           MOVE BTX-REC-DATA-OLD TO TXN-DATA-OLD                  dbank52p
014800           MOVE TXN-T1-OLD-DESC TO CD52O-DESC                     dbank52p
014900        ELSE                                                      dbank52p
015000           GO TO READ-FILE                                        dbank52p
015100        END-IF                                                    dbank52p
015200     END-IF.                                                      dbank52p
015300 READ-FILE-EXIT.                                                  dbank52p
015400     EXIT.                                                        dbank52p
015500                                                                  dbank52p
015600***************************************************************** dbank52p
015700* Close the file                                                * dbank52p
015800***************************************************************** dbank52p
015900 CLOSE-FILE.                                                      dbank52p
016000     CLOSE BNKTXN-FILE.                                           dbank52p
016100     IF WS-BNKTXN-STATUS = '00'                                   dbank52p
016200        SET IO-REQUEST-STATUS-OK TO TRUE                          dbank52p
016300     ELSE                                                         dbank52p
016400       SET IO-REQUEST-STATUS-ERROR TO TRUE                        dbank52p
016500     END-IF.                                                      dbank52p
016600 CLOSE-FILE-EXIT.                                                 dbank52p
016700     EXIT.                                                        dbank52p
016800                                                                  dbank52p
016900* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     dbank52p
