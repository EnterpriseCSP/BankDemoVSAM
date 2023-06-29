000100***************************************************************** dbank08p
000200*                                                               * dbank08p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * dbank08p
000400*   This demonstration program is provided for use by users     * dbank08p
000500*   of Micro Focus products and may be used, modified and       * dbank08p
000600*   distributed as part of your application provided that       * dbank08p
000700*   you properly acknowledge the copyright of Micro Focus       * dbank08p
000800*   in this material.                                           * dbank08p
000900*                                                               * dbank08p
001000***************************************************************** dbank08p
001100                                                                  dbank08p
001200***************************************************************** dbank08p
001300* Program:     DBANK08P.CBL                                     * dbank08p
001400* Function:    Obtain count of number of accounts user has      * dbank08p
001500*              VSAM version                                     * dbank08p
001600***************************************************************** dbank08p
001700                                                                  dbank08p
001800 IDENTIFICATION DIVISION.                                         dbank08p
001900 PROGRAM-ID.                                                      dbank08p
002000     DBANK08P.                                                    dbank08p
002100 DATE-WRITTEN.                                                    dbank08p
002200     September 2002.                                              dbank08p
002300 DATE-COMPILED.                                                   dbank08p
002400     Today.                                                       dbank08p
002500                                                                  dbank08p
002600 ENVIRONMENT DIVISION.                                            dbank08p
002700                                                                  dbank08p
002800 DATA DIVISION.                                                   dbank08p
002900                                                                  dbank08p
003000 WORKING-STORAGE SECTION.                                         dbank08p
003100 01  WS-MISC-STORAGE.                                             dbank08p
003200   05  WS-PROGRAM-ID                         PIC X(8)             dbank08p
003300       VALUE 'DBANK08P'.                                          dbank08p
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            dbank08p
003500   05  WS-RESP                               PIC S9(8) COMP.      dbank08p
003600   05  WS-SUB1                               PIC S9(4) COMP.      dbank08p
003700   05  WS-BNKACCT-AIX1-RID                   PIC X(5).            dbank08p
003800                                                                  dbank08p
003900 01  WS-BNKACCT-REC.                                              dbank08p
004000 COPY CBANKVAC.                                                   dbank08p
004100                                                                  dbank08p
004200 01  WS-COMMAREA.                                                 dbank08p
004300 COPY CBANKD08.                                                   dbank08p
004400                                                                  dbank08p
004500 COPY CABENDD.                                                    dbank08p
004600                                                                  dbank08p
004700 LINKAGE SECTION.                                                 dbank08p
004800 01  DFHCOMMAREA.                                                 dbank08p
004900   05  LK-COMMAREA                           PIC X(1)             dbank08p
005000       OCCURS 1 TO 4096 TIMES                                     dbank08p
005100         DEPENDING ON WS-COMMAREA-LENGTH.                         dbank08p
005200                                                                  dbank08p
005300 COPY CENTRY.                                                     dbank08p
005400***************************************************************** dbank08p
005500* Move the passed data to our area                              * dbank08p
005600***************************************************************** dbank08p
005700     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            dbank08p
005800     MOVE DFHCOMMAREA TO WS-COMMAREA.                             dbank08p
005900                                                                  dbank08p
006000***************************************************************** dbank08p
006100* Initialize our output area                                    * dbank08p
006200***************************************************************** dbank08p
006300     MOVE SPACES TO CD08O-DATA.                                   dbank08p
006400                                                                  dbank08p
006500***************************************************************** dbank08p
006600* Set up the start position for the browse                      * dbank08p
006700***************************************************************** dbank08p
006800     MOVE CD08I-CONTACT-ID TO WS-BNKACCT-AIX1-RID.                dbank08p
006900                                                                  dbank08p
007000***************************************************************** dbank08p
007100* Start browsing the file                                       * dbank08p
007200***************************************************************** dbank08p
007300     EXEC CICS STARTBR FILE('BNKACC1')                            dbank08p
007400                       RIDFLD(WS-BNKACCT-AIX1-RID)                dbank08p
007500                       GTEQ                                       dbank08p
007600     END-EXEC.                                                    dbank08p
007700                                                                  dbank08p
007800***************************************************************** dbank08p
007900* Now browse the selected recs and move up to 5 into our area   * dbank08p
008000***************************************************************** dbank08p
008100     MOVE 0 TO CD08O-COUNT.                                       dbank08p
008200 ACCOUNT-FETCH-LOOP.                                              dbank08p
008300     EXEC CICS READNEXT FILE('BNKACC1')                           dbank08p
008400                        INTO(WS-BNKACCT-REC)                      dbank08p
008500                        LENGTH(LENGTH OF WS-BNKACCT-REC)          dbank08p
008600                        RIDFLD(WS-BNKACCT-AIX1-RID)               dbank08p
008700                        RESP(WS-RESP)                             dbank08p
008800     END-EXEC.                                                    dbank08p
008900     IF (WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL) AND              dbank08p
009000         WS-RESP IS NOT EQUAL TO DFHRESP(DUPKEY)) OR              dbank08p
009100        BAC-REC-PID IS NOT EQUAL TO CD08I-CONTACT-ID              dbank08p
009200        GO TO ACCOUNT-FETCH-LOOP-EXIT                             dbank08p
009300     ELSE                                                         dbank08p
009400        ADD 1 TO CD08O-COUNT                                      dbank08p
009500        GO TO ACCOUNT-FETCH-LOOP                                  dbank08p
009600     END-IF.                                                      dbank08p
009700                                                                  dbank08p
009800***************************************************************** dbank08p
009900* We quit the loop for some reason                              * dbank08p
010000***************************************************************** dbank08p
010100 ACCOUNT-FETCH-LOOP-EXIT.                                         dbank08p
010200     EXEC CICS ENDBR FILE('BNKACC1')                              dbank08p
010300     END-EXEC.                                                    dbank08p
010400                                                                  dbank08p
010500***************************************************************** dbank08p
010600* Move the result back to the callers area                      * dbank08p
010700***************************************************************** dbank08p
010800     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).       dbank08p
010900                                                                  dbank08p
011000***************************************************************** dbank08p
011100* Return to our caller                                          * dbank08p
011200***************************************************************** dbank08p
011300 COPY CRETURN.                                                    dbank08p
011400                                                                  dbank08p
011500* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     dbank08p
