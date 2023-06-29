000100***************************************************************** dbank06p
000200*                                                               * dbank06p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * dbank06p
000400*   This demonstration program is provided for use by users     * dbank06p
000500*   of Micro Focus products and may be used, modified and       * dbank06p
000600*   distributed as part of your application provided that       * dbank06p
000700*   you properly acknowledge the copyright of Micro Focus       * dbank06p
000800*   in this material.                                           * dbank06p
000900*                                                               * dbank06p
001000***************************************************************** dbank06p
001100                                                                  dbank06p
001200***************************************************************** dbank06p
001300* Program:     DBANK06P.CBL                                     * dbank06p
001400* Function:    Write transaction records for audit trail        * dbank06p
001500*              VSAM Version                                     * dbank06p
001600***************************************************************** dbank06p
001700                                                                  dbank06p
001800 IDENTIFICATION DIVISION.                                         dbank06p
001900 PROGRAM-ID.                                                      dbank06p
002000     DBANK06P.                                                    dbank06p
002100 DATE-WRITTEN.                                                    dbank06p
002200     September 2002.                                              dbank06p
002300 DATE-COMPILED.                                                   dbank06p
002400     Today.                                                       dbank06p
002500                                                                  dbank06p
002600 ENVIRONMENT DIVISION.                                            dbank06p
002700                                                                  dbank06p
002800 DATA DIVISION.                                                   dbank06p
002900                                                                  dbank06p
003000 WORKING-STORAGE SECTION.                                         dbank06p
003100 01  WS-MISC-STORAGE.                                             dbank06p
003200   05  WS-PROGRAM-ID                         PIC X(8)             dbank06p
003300       VALUE 'DBANK06P'.                                          dbank06p
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            dbank06p
003500   05  WS-RESP                               PIC S9(8) COMP.      dbank06p
003600   05  WS-BNKTXN-RID                         PIC X(26).           dbank06p
003700                                                                  dbank06p
003800 01  WS-BNKTXN-REC.                                               dbank06p
003900 COPY CBANKVTX.                                                   dbank06p
004000                                                                  dbank06p
004100 01  WS-TWOS-COMP.                                                dbank06p
004200   05  WS-TWOS-COMP-REQ                      PIC X(1).            dbank06p
004300     88  WS-TWOS-COMP-REQ-YES                VALUE 'Y'.           dbank06p
004400     88  WS-TWOS-COMP-REQ-NO                 VALUE 'N'.           dbank06p
004500   05  WS-TWOS-COMP-LEN                      PIC S9(4) COMP.      dbank06p
004600   05  WS-TWOS-COMP-INPUT                    PIC X(256).          dbank06p
004700   05  WS-TWOS-COMP-OUTPUT                   PIC X(256).          dbank06p
004800                                                                  dbank06p
004900 01  WS-COMMAREA.                                                 dbank06p
005000 COPY CBANKD06.                                                   dbank06p
005100                                                                  dbank06p
005200 COPY CBANKTXD.                                                   dbank06p
005300                                                                  dbank06p
005400 COPY CABENDD.                                                    dbank06p
005500                                                                  dbank06p
005600 LINKAGE SECTION.                                                 dbank06p
005700 01  DFHCOMMAREA.                                                 dbank06p
005800   05  LK-COMMAREA                           PIC X(1)             dbank06p
005900       OCCURS 1 TO 4096 TIMES                                     dbank06p
006000         DEPENDING ON WS-COMMAREA-LENGTH.                         dbank06p
006100                                                                  dbank06p
006200 COPY CENTRY.                                                     dbank06p
006300***************************************************************** dbank06p
006400* Move the passed data to our area                              * dbank06p
006500***************************************************************** dbank06p
006600     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            dbank06p
006700     MOVE DFHCOMMAREA TO WS-COMMAREA.                             dbank06p
006800                                                                  dbank06p
006900***************************************************************** dbank06p
007000* Initialize our output area                                    * dbank06p
007100***************************************************************** dbank06p
007200     MOVE SPACES TO CD06O-DATA.                                   dbank06p
007300                                                                  dbank06p
007400***************************************************************** dbank06p
007500* Insert two rows/records into the database/file                * dbank06p
007600***************************************************************** dbank06p
007700***************************************************************** dbank06p
007800* First row/record is for the from-to transaction               * dbank06p
007900***************************************************************** dbank06p
008000     MOVE '0' TO CD06I-TIMESTAMP (26:1).                          dbank06p
008100     MOVE CD06I-FROM-PID TO BTX-REC-PID.                          dbank06p
008200     MOVE '1' TO BTX-REC-TYPE.                                    dbank06p
008300     MOVE '1' TO BTX-REC-SUB-TYPE.                                dbank06p
008400     MOVE CD06I-FROM-ACC TO BTX-REC-ACCNO.                        dbank06p
008500     MOVE CD06I-TIMESTAMP TO BTX-REC-TIMESTAMP.                   dbank06p
008600     MOVE CD06I-FROM-AMOUNT TO BTX-REC-AMOUNT.                    dbank06p
008700     MOVE SPACES TO TXN-DATA.                                     dbank06p
008800     MOVE CD06I-FROM-DESC TO TXN-T1-OLD-DESC.                     dbank06p
008900     MOVE TXN-DATA-OLD TO BTX-REC-DATA-OLD.                       dbank06p
009000     MOVE TXN-DATA-NEW TO BTX-REC-DATA-NEW.                       dbank06p
009100     MOVE CD06I-TIMESTAMP TO WS-TWOS-COMP-INPUT.                  dbank06p
009200     MOVE LOW-VALUES TO WS-TWOS-COMP-OUTPUT.                      dbank06p
009300     MOVE LENGTH OF CD06I-TIMESTAMP TO WS-TWOS-COMP-LEN.          dbank06p
009400     CALL 'UTWOSCMP' USING WS-TWOS-COMP-LEN                       dbank06p
009500                           WS-TWOS-COMP-INPUT                     dbank06p
009600                           WS-TWOS-COMP-OUTPUT.                   dbank06p
009700     MOVE WS-TWOS-COMP-OUTPUT TO BTX-REC-TIMESTAMP-FF.            dbank06p
009800     MOVE CD06I-TIMESTAMP TO WS-BNKTXN-RID.                       dbank06p
009900     EXEC CICS WRITE FILE('BNKTXN')                               dbank06p
010000                          FROM(WS-BNKTXN-REC)                     dbank06p
010100                          LENGTH(LENGTH OF WS-BNKTXN-REC)         dbank06p
010200                          RIDFLD(WS-BNKTXN-RID)                   dbank06p
010300                          KEYLENGTH(LENGTH OF WS-BNKTXN-RID)      dbank06p
010400                          RESP(WS-RESP)                           dbank06p
010500     END-EXEC.                                                    dbank06p
010600                                                                  dbank06p
010700***************************************************************** dbank06p
010800* Did we get the record OK                                      * dbank06p
010900***************************************************************** dbank06p
011000     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)                   dbank06p
011100        SET CD06O-UPDATE-FAIL TO TRUE                             dbank06p
011200        MOVE 'Unable to write FROM transaction record'            dbank06p
011300          TO CD06O-MSG                                            dbank06p
011400     END-IF.                                                      dbank06p
011500                                                                  dbank06p
011600***************************************************************** dbank06p
011700* Second row/record is for the to-from transaction              * dbank06p
011800***************************************************************** dbank06p
011900     MOVE '1' TO CD06I-TIMESTAMP (26:1).                          dbank06p
012000     MOVE CD06I-TO-PID TO BTX-REC-PID.                            dbank06p
012100     MOVE '1' TO BTX-REC-TYPE.                                    dbank06p
012200     MOVE '2' TO BTX-REC-SUB-TYPE.                                dbank06p
012300     MOVE CD06I-TO-ACC TO BTX-REC-ACCNO.                          dbank06p
012400     MOVE CD06I-TIMESTAMP TO BTX-REC-TIMESTAMP.                   dbank06p
012500     MOVE CD06I-TO-AMOUNT TO BTX-REC-AMOUNT.                      dbank06p
012600     MOVE SPACES TO TXN-DATA.                                     dbank06p
012700     MOVE CD06I-TO-DESC TO TXN-T1-OLD-DESC.                       dbank06p
012800     MOVE TXN-DATA-OLD TO BTX-REC-DATA-OLD.                       dbank06p
012900     MOVE TXN-DATA-NEW TO BTX-REC-DATA-NEW.                       dbank06p
013000     MOVE CD06I-TIMESTAMP TO WS-TWOS-COMP-INPUT.                  dbank06p
013100     MOVE LOW-VALUES TO WS-TWOS-COMP-OUTPUT.                      dbank06p
013200     MOVE LENGTH OF CD06I-TIMESTAMP TO WS-TWOS-COMP-LEN.          dbank06p
013300     CALL 'UTWOSCMP' USING WS-TWOS-COMP-LEN                       dbank06p
013400                           WS-TWOS-COMP-INPUT                     dbank06p
013500                           WS-TWOS-COMP-OUTPUT.                   dbank06p
013600     MOVE WS-TWOS-COMP-OUTPUT TO BTX-REC-TIMESTAMP-FF.            dbank06p
013700     MOVE CD06I-TIMESTAMP TO WS-BNKTXN-RID.                       dbank06p
013800     EXEC CICS WRITE FILE('BNKTXN')                               dbank06p
013900                          FROM(WS-BNKTXN-REC)                     dbank06p
014000                          LENGTH(LENGTH OF WS-BNKTXN-REC)         dbank06p
014100                          RIDFLD(WS-BNKTXN-RID)                   dbank06p
014200                          KEYLENGTH(LENGTH OF WS-BNKTXN-RID)      dbank06p
014300                          RESP(WS-RESP)                           dbank06p
014400     END-EXEC.                                                    dbank06p
014500                                                                  dbank06p
014600***************************************************************** dbank06p
014700* Did we get the record OK                                      * dbank06p
014800***************************************************************** dbank06p
014900     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)                   dbank06p
015000        SET CD06O-UPDATE-FAIL TO TRUE                             dbank06p
015100        MOVE 'Unable to write TO transaction record'              dbank06p
015200          TO CD06O-MSG                                            dbank06p
015300     END-IF.                                                      dbank06p
015400                                                                  dbank06p
015500                                                                  dbank06p
015600***************************************************************** dbank06p
015700* Move the result back to the callers area                      * dbank06p
015800***************************************************************** dbank06p
015900     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).       dbank06p
016000                                                                  dbank06p
016100***************************************************************** dbank06p
016200* Return to our caller                                          * dbank06p
016300***************************************************************** dbank06p
016400 COPY CRETURN.                                                    dbank06p
016500                                                                  dbank06p
016600* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     dbank06p
