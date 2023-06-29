000100***************************************************************** dbank07p
000200*                                                               * dbank07p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * dbank07p
000400*   This demonstration program is provided for use by users     * dbank07p
000500*   of Micro Focus products and may be used, modified and       * dbank07p
000600*   distributed as part of your application provided that       * dbank07p
000700*   you properly acknowledge the copyright of Micro Focus       * dbank07p
000800*   in this material.                                           * dbank07p
000900*                                                               * dbank07p
001000***************************************************************** dbank07p
001100                                                                  dbank07p
001200***************************************************************** dbank07p
001300* Program:     DBANK07P.CBL                                     * dbank07p
001400* Function:    Write transaction records for audit trail        * dbank07p
001500*              VSAM Version                                     * dbank07p
001600***************************************************************** dbank07p
001700                                                                  dbank07p
001800 IDENTIFICATION DIVISION.                                         dbank07p
001900 PROGRAM-ID.                                                      dbank07p
002000     DBANK07P.                                                    dbank07p
002100 DATE-WRITTEN.                                                    dbank07p
002200     September 2002.                                              dbank07p
002300 DATE-COMPILED.                                                   dbank07p
002400     Today.                                                       dbank07p
002500                                                                  dbank07p
002600 ENVIRONMENT DIVISION.                                            dbank07p
002700                                                                  dbank07p
002800 DATA DIVISION.                                                   dbank07p
002900                                                                  dbank07p
003000 WORKING-STORAGE SECTION.                                         dbank07p
003100 01  WS-MISC-STORAGE.                                             dbank07p
003200   05  WS-PROGRAM-ID                         PIC X(8)             dbank07p
003300       VALUE 'DBANK07P'.                                          dbank07p
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            dbank07p
003500   05  WS-RESP                               PIC S9(8) COMP.      dbank07p
003600   05  WS-BNKTXN-RID                         PIC X(26).           dbank07p
003700                                                                  dbank07p
003800 01  WS-BNKTXN-REC.                                               dbank07p
003900 COPY CBANKVTX.                                                   dbank07p
004000                                                                  dbank07p
004100 COPY CBANKTXD.                                                   dbank07p
004200                                                                  dbank07p
004300 01  WS-TWOS-COMP.                                                dbank07p
004400   05  WS-TWOS-COMP-REQ                      PIC X(1).            dbank07p
004500     88  WS-TWOS-COMP-REQ-YES                VALUE 'Y'.           dbank07p
004600     88  WS-TWOS-COMP-REQ-NO                 VALUE 'N'.           dbank07p
004700   05  WS-TWOS-COMP-LEN                      PIC S9(4) COMP.      dbank07p
004800   05  WS-TWOS-COMP-INPUT                    PIC X(256).          dbank07p
004900   05  WS-TWOS-COMP-OUTPUT                   PIC X(256).          dbank07p
005000                                                                  dbank07p
005100 01  WS-COMMAREA.                                                 dbank07p
005200 COPY CBANKD07.                                                   dbank07p
005300                                                                  dbank07p
005400 COPY CTSTAMPD.                                                   dbank07p
005500                                                                  dbank07p
005600 COPY CABENDD.                                                    dbank07p
005700                                                                  dbank07p
005800 LINKAGE SECTION.                                                 dbank07p
005900 01  DFHCOMMAREA.                                                 dbank07p
006000   05  LK-COMMAREA                           PIC X(1)             dbank07p
006100       OCCURS 1 TO 4096 TIMES                                     dbank07p
006200         DEPENDING ON WS-COMMAREA-LENGTH.                         dbank07p
006300                                                                  dbank07p
006400 COPY CENTRY.                                                     dbank07p
006500***************************************************************** dbank07p
006600* Move the passed data to our area                              * dbank07p
006700***************************************************************** dbank07p
006800     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            dbank07p
006900     MOVE DFHCOMMAREA TO WS-COMMAREA.                             dbank07p
007000                                                                  dbank07p
007100***************************************************************** dbank07p
007200* Initialize our output area                                    * dbank07p
007300***************************************************************** dbank07p
007400     MOVE SPACES TO CD07O-DATA.                                   dbank07p
007500                                                                  dbank07p
007600***************************************************************** dbank07p
007700* Insert row/record into the database/file                      * dbank07p
007800***************************************************************** dbank07p
007900 COPY CTSTAMPP.                                                   dbank07p
008000     MOVE WS-TIMESTAMP TO CD07I-TIMESTAMP.                        dbank07p
008100     MOVE SPACES TO BTX-RECORD.                                   dbank07p
008200     MOVE '0' TO CD07I-TIMESTAMP (26:1).                          dbank07p
008300     MOVE CD07I-PERSON-PID TO BTX-REC-PID.                        dbank07p
008400     MOVE '2' TO BTX-REC-TYPE.                                    dbank07p
008500     MOVE ' ' TO BTX-REC-SUB-TYPE.                                dbank07p
008600     MOVE SPACES TO BTX-REC-ACCNO.                                dbank07p
008700     MOVE CD07I-TIMESTAMP TO BTX-REC-TIMESTAMP.                   dbank07p
008800     MOVE ZERO TO BTX-REC-AMOUNT.                                 dbank07p
008900     MOVE CD07I-OLD-DATA TO BTX-REC-DATA-OLD.                     dbank07p
009000     MOVE CD07I-NEW-DATA TO BTX-REC-DATA-NEW.                     dbank07p
009100     MOVE CD07I-TIMESTAMP TO WS-TWOS-COMP-INPUT.                  dbank07p
009200     MOVE LOW-VALUES TO WS-TWOS-COMP-OUTPUT.                      dbank07p
009300     MOVE LENGTH OF CD07I-TIMESTAMP TO WS-TWOS-COMP-LEN.          dbank07p
009400     CALL 'UTWOSCMP' USING WS-TWOS-COMP-LEN                       dbank07p
009500                           WS-TWOS-COMP-INPUT                     dbank07p
009600                           WS-TWOS-COMP-OUTPUT.                   dbank07p
009700     MOVE WS-TWOS-COMP-OUTPUT TO BTX-REC-TIMESTAMP-FF.            dbank07p
009800     MOVE CD07I-TIMESTAMP TO WS-BNKTXN-RID.                       dbank07p
009900     EXEC CICS WRITE FILE('BNKTXN')                               dbank07p
010000                          FROM(WS-BNKTXN-REC)                     dbank07p
010100                          LENGTH(LENGTH OF WS-BNKTXN-REC)         dbank07p
010200                          RIDFLD(WS-BNKTXN-RID)                   dbank07p
010300                          KEYLENGTH(LENGTH OF WS-BNKTXN-RID)      dbank07p
010400                          RESP(WS-RESP)                           dbank07p
010500     END-EXEC.                                                    dbank07p
010600                                                                  dbank07p
010700***************************************************************** dbank07p
010800* Did we get the record OK                                      * dbank07p
010900***************************************************************** dbank07p
011000     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)                   dbank07p
011100        SET CD07O-UPDATE-FAIL TO TRUE                             dbank07p
011200        MOVE 'Unable to insert contact info audit record'         dbank07p
011300          TO CD07O-MSG                                            dbank07p
011400     END-IF.                                                      dbank07p
011500                                                                  dbank07p
011600***************************************************************** dbank07p
011700* Move the result back to the callers area                      * dbank07p
011800***************************************************************** dbank07p
011900     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).       dbank07p
012000                                                                  dbank07p
012100***************************************************************** dbank07p
012200* Return to our caller                                          * dbank07p
012300***************************************************************** dbank07p
012400 COPY CRETURN.                                                    dbank07p
012500                                                                  dbank07p
012600* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     dbank07p
