000100***************************************************************** dhelp01p
000200*                                                               * dhelp01p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * dhelp01p
000400*   This demonstration program is provided for use by users     * dhelp01p
000500*   of Micro Focus products and may be used, modified and       * dhelp01p
000600*   distributed as part of your application provided that       * dhelp01p
000700*   you properly acknowledge the copyright of Micro Focus       * dhelp01p
000800*   in this material.                                           * dhelp01p
000900*                                                               * dhelp01p
001000***************************************************************** dhelp01p
001100                                                                  dhelp01p
001200***************************************************************** dhelp01p
001300* Program:     DHELP01P.CBL                                     * dhelp01p
001400* Function:    Obtain screen help information                   * dhelp01p
001500*              VSAM version                                     * dhelp01p
001600***************************************************************** dhelp01p
001700                                                                  dhelp01p
001800 IDENTIFICATION DIVISION.                                         dhelp01p
001900 PROGRAM-ID.                                                      dhelp01p
002000     DHELP01P.                                                    dhelp01p
002100 DATE-WRITTEN.                                                    dhelp01p
002200     September 2002.                                              dhelp01p
002300 DATE-COMPILED.                                                   dhelp01p
002400     Today.                                                       dhelp01p
002500                                                                  dhelp01p
002600 ENVIRONMENT DIVISION.                                            dhelp01p
002700                                                                  dhelp01p
002800 DATA DIVISION.                                                   dhelp01p
002900                                                                  dhelp01p
003000 WORKING-STORAGE SECTION.                                         dhelp01p
003100 01  WS-MISC-STORAGE.                                             dhelp01p
003200   05  WS-PROGRAM-ID                         PIC X(8)             dhelp01p
003300       VALUE 'DHELP01P'.                                          dhelp01p
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            dhelp01p
003500   05  WS-SUB1                               PIC S9(4) COMP.      dhelp01p
003600   05  WS-RESP                               PIC S9(4) COMP.      dhelp01p
003700   05  WS-HELP-RID                           PIC X(8).            dhelp01p
003800   05  WS-LINE                               PIC X(2).            dhelp01p
003900   05  WS-LINE-N REDEFINES WS-LINE           PIC 9(2).            dhelp01p
004000                                                                  dhelp01p
004100 01  WS-COMMAREA.                                                 dhelp01p
004200 COPY CHELPD01.                                                   dhelp01p
004300                                                                  dhelp01p
004400 01  WS-HELP-REC.                                                 dhelp01p
004500 COPY CHELPVSM.                                                   dhelp01p
004600                                                                  dhelp01p
004700 COPY CABENDD.                                                    dhelp01p
004800                                                                  dhelp01p
004900 LINKAGE SECTION.                                                 dhelp01p
005000 01  DFHCOMMAREA.                                                 dhelp01p
005100   05  LK-COMMAREA                           PIC X(1)             dhelp01p
005200         OCCURS 1 TO 4096 TIMES                                   dhelp01p
005300           DEPENDING ON WS-COMMAREA-LENGTH.                       dhelp01p
005400                                                                  dhelp01p
005500 COPY CENTRY.                                                     dhelp01p
005600***************************************************************** dhelp01p
005700* Move the passed data to our area                              * dhelp01p
005800***************************************************************** dhelp01p
005900     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            dhelp01p
006000     MOVE DFHCOMMAREA TO WS-COMMAREA.                             dhelp01p
006100                                                                  dhelp01p
006200***************************************************************** dhelp01p
006300* Initialize our output area                                    * dhelp01p
006400***************************************************************** dhelp01p
006500     MOVE SPACES TO HELP01O-DATA.                                 dhelp01p
006600     MOVE HELP01I-SCRN TO HELP01O-SCRN.                           dhelp01p
006700     SET HELP-FOUND TO TRUE.                                      dhelp01p
006800                                                                  dhelp01p
006900***************************************************************** dhelp01p
007000* Set up start position for the browse                          * dhelp01p
007100***************************************************************** dhelp01p
007200     MOVE HELP01I-SCRN TO WS-HELP-RID (1:6).                      dhelp01p
007300     MOVE SPACES TO WS-HELP-RID (7:2).                            dhelp01p
007400     MOVE HELP01I-SCRN TO HELP01O-SCRN.                           dhelp01p
007500                                                                  dhelp01p
007600***************************************************************** dhelp01p
007700* Start browsing the file                                       * dhelp01p
007800***************************************************************** dhelp01p
007900     EXEC CICS STARTBR FILE('BNKHELP')                            dhelp01p
008000                      RIDFLD(WS-HELP-RID)                         dhelp01p
008100                      GTEQ                                        dhelp01p
008200     END-EXEC.                                                    dhelp01p
008300                                                                  dhelp01p
008400***************************************************************** dhelp01p
008500* Now browse the selected rows are move up to 19 into our area  * dhelp01p
008600***************************************************************** dhelp01p
008700     MOVE 0 TO WS-SUB1.                                           dhelp01p
008800 TEXT-FETCH-LOOP.                                                 dhelp01p
008900     ADD 1 TO WS-SUB1.                                            dhelp01p
009000     IF WS-SUB1 IS GREATER THAN 19                                dhelp01p
009100        GO TO TEXT-FETCH-LOOP-EXIT.                               dhelp01p
009200     EXEC CICS READNEXT FILE('BNKHELP')                           dhelp01p
009300                        INTO(WS-HELP-REC)                         dhelp01p
009400                        LENGTH(LENGTH OF WS-HELP-REC)             dhelp01p
009500                        RIDFLD(WS-HELP-RID)                       dhelp01p
009600                        RESP(WS-RESP)                             dhelp01p
009700     END-EXEC.                                                    dhelp01p
009800                                                                  dhelp01p
009900***************************************************************** dhelp01p
010000* Did we get the record OK                                      * dhelp01p
010100***************************************************************** dhelp01p
010200     IF WS-RESP IS EQUAL TO DFHRESP(NORMAL)                       dhelp01p
010300        IF HLP-SCRN IS EQUAL TO HELP01I-SCRN                      dhelp01p
010400           MOVE HLP-LINE TO WS-LINE                               dhelp01p
010500           IF WS-LINE IS NUMERIC                                  dhelp01p
010600              IF WS-LINE-N IS GREATER THAN 00 AND                 dhelp01p
010700                 WS-LINE-N IS NOT GREATER THAN 19                 dhelp01p
010800                 MOVE HLP-TEXT TO HELP01O-LINE (WS-LINE-N)        dhelp01p
010900              END-IF                                              dhelp01p
011000           END-IF                                                 dhelp01p
011100           GO TO TEXT-FETCH-LOOP                                  dhelp01p
011200        ELSE                                                      dhelp01p
011300           IF HELP01O-INDIVIDUAL-LINES IS EQUAL TO SPACES         dhelp01p
011400              SET HELP-NOT-FOUND TO TRUE                          dhelp01p
011500              MOVE 'No help available'                            dhelp01p
011600                TO HELP01O-LINE (10) (30:17)                      dhelp01p
011700           END-IF                                                 dhelp01p
011800     ELSE                                                         dhelp01p
011900        IF HELP01O-INDIVIDUAL-LINES IS EQUAL TO SPACES            dhelp01p
012000           SET HELP-NOT-FOUND TO TRUE                             dhelp01p
012100           MOVE 'No help available' TO HELP01O-LINE (10) (30:17)  dhelp01p
012200        END-IF                                                    dhelp01p
012300        GO TO TEXT-FETCH-LOOP-EXIT                                dhelp01p
012400     END-IF.                                                      dhelp01p
012500                                                                  dhelp01p
012600***************************************************************** dhelp01p
012700* We quit the loop for some reason                              * dhelp01p
012800***************************************************************** dhelp01p
012900 TEXT-FETCH-LOOP-EXIT.                                            dhelp01p
013000     EXEC CICS ENDBR FILE('BNKHELP')                              dhelp01p
013100     END-EXEC.                                                    dhelp01p
013200                                                                  dhelp01p
013300***************************************************************** dhelp01p
013400* Move the result back to the callers area                      * dhelp01p
013500***************************************************************** dhelp01p
013600     MOVE WS-COMMAREA TO DFHCOMMAREA (1:WS-COMMAREA-LENGTH).      dhelp01p
013700                                                                  dhelp01p
013800***************************************************************** dhelp01p
013900* Return to our caller                                          * dhelp01p
014000***************************************************************** dhelp01p
014100 COPY CRETURN.                                                    dhelp01p
014200                                                                  dhelp01p
014300* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     dhelp01p
