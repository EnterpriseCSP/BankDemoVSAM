000100***************************************************************** bcash02p
000200*                                                               * bcash02p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * bcash02p
000400*   This demonstration program is provided for use by users     * bcash02p
000500*   of Micro Focus products and may be used, modified and       * bcash02p
000600*   distributed as part of your application provided that       * bcash02p
000700*   you properly acknowledge the copyright of Micro Focus       * bcash02p
000800*   in this material.                                           * bcash02p
000900*                                                               * bcash02p
001000***************************************************************** bcash02p
001100                                                                  bcash02p
001200***************************************************************** bcash02p
001300* Program:     BCASH02P.CBL                                     * bcash02p
001400* Layer:       Business logic                                   * bcash02p
001500* Function:    ATM - tranfer funds between accounts             * bcash02p
001600***************************************************************** bcash02p
001700                                                                  bcash02p
001800 IDENTIFICATION DIVISION.                                         bcash02p
001900 PROGRAM-ID.                                                      bcash02p
002000     BCASH02P.                                                    bcash02p
002100 DATE-WRITTEN.                                                    bcash02p
002200     September 2002.                                              bcash02p
002300 DATE-COMPILED.                                                   bcash02p
002400     Today.                                                       bcash02p
002500                                                                  bcash02p
002600 ENVIRONMENT DIVISION.                                            bcash02p
002700                                                                  bcash02p
002800 DATA DIVISION.                                                   bcash02p
002900 WORKING-STORAGE SECTION.                                         bcash02p
003000 01  WS-MISC-STORAGE.                                             bcash02p
003100   05  WS-PROGRAM-ID                         PIC X(8)             bcash02p
003200       VALUE 'BCASH02P'.                                          bcash02p
003300   05  WS-COMMAREA-LENGTH                    PIC 9(5).            bcash02p
003400   05  WS-SUB                                PIC 9(3).            bcash02p
003500 01  WS-CASH-DATA.                                                bcash02p
003600 COPY CCASHDAT.                                                   bcash02p
003700                                                                  bcash02p
003800 01  WS-BANK-DATA.                                                bcash02p
003900 COPY CBANKDAT.                                                   bcash02p
004000*                                                                 bcash02p
004100*01  WS-ACCOUNT-DATA.                                             bcash02p
004200*COPY CCASHD02.                                                   bcash02p
004300                                                                  bcash02p
004400 COPY CABENDD.                                                    bcash02p
004500                                                                  bcash02p
004600 LINKAGE SECTION.                                                 bcash02p
004700 01  DFHCOMMAREA.                                                 bcash02p
004800   05  LK-COMMAREA                           PIC X(6144).         bcash02p
004900                                                                  bcash02p
005000 COPY CENTRY.                                                     bcash02p
005100***************************************************************** bcash02p
005200* Make ourselves re-entrant                                     * bcash02p
005300***************************************************************** bcash02p
005400                                                                  bcash02p
005500***************************************************************** bcash02p
005600* Move the passed area to our area                              * bcash02p
005700***************************************************************** bcash02p
005800     MOVE DFHCOMMAREA (1:LENGTH OF WS-CASH-DATA) TO WS-CASH-DATA. bcash02p
005900                                                                  bcash02p
006000***************************************************************** bcash02p
006100* Ensure error message is cleared                               * bcash02p
006200***************************************************************** bcash02p
006300     MOVE SPACES TO CASH-ERROR-MSG.                               bcash02p
006400                                                                  bcash02p
006500***************************************************************** bcash02p
006600* This is the main process                                      * bcash02p
006700***************************************************************** bcash02p
006800     MOVE LOW-VALUES TO WS-BANK-DATA.                             bcash02p
006900     SET BANK-AID-ENTER TO TRUE.                                  bcash02p
007000     MOVE 'MBANK50' TO BANK-LAST-MAPSET.                          bcash02p
007100     MOVE CASH-USERID TO BANK-USERID.                             bcash02p
007200                                                                  bcash02p
007300                                                                  bcash02p
007400     MOVE CASH-ATM2-XFER-AMT TO BANK-SCR50-XFER                   bcash02p
007500     MOVE 'X' TO BANK-SCR50-FRM1.                                 bcash02p
007600     MOVE CASH-ATM2-FROM-ACC TO BANK-SCR50-ACC1.                  bcash02p
007700     MOVE CASH-ATM2-FROM-BAL TO BANK-SCR50-BAL1.                  bcash02p
007800     MOVE 'X' TO BANK-SCR50-TO2.                                  bcash02p
007900     MOVE CASH-ATM2-TO-ACC TO BANK-SCR50-ACC2.                    bcash02p
008000     MOVE CASH-ATM2-TO-BAL TO BANK-SCR50-BAL2.                    bcash02p
008100                                                                  bcash02p
008200     EXEC CICS LINK PROGRAM('BBANK50P')                           bcash02p
008300                    COMMAREA(WS-BANK-DATA)                        bcash02p
008400                    LENGTH(LENGTH OF WS-BANK-DATA)                bcash02p
008500     END-EXEC.                                                    bcash02p
008600                                                                  bcash02p
008700     MOVE BANK-ERROR-MSG TO CASH-ERROR-MSG.                       bcash02p
008800                                                                  bcash02p
008900 COMMON-RETURN.                                                   bcash02p
009000     MOVE WS-CASH-DATA TO DFHCOMMAREA (1:LENGTH OF WS-CASH-DATA). bcash02p
009100 COPY CRETURN.                                                    bcash02p
009200                                                                  bcash02p
009300* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     bcash02p
