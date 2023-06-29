000100***************************************************************** bcash00p
000200*                                                               * bcash00p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * bcash00p
000400*   This demonstration program is provided for use by users     * bcash00p
000500*   of Micro Focus products and may be used, modified and       * bcash00p
000600*   distributed as part of your application provided that       * bcash00p
000700*   you properly acknowledge the copyright of Micro Focus       * bcash00p
000800*   in this material.                                           * bcash00p
000900*                                                               * bcash00p
001000***************************************************************** bcash00p
001100                                                                  bcash00p
001200***************************************************************** bcash00p
001300* Program:     BCASH00P.CBL                                     * bcash00p
001400* Layer:       Business logic                                   * bcash00p
001500* Function:    ATM - entry point and sign-0n validation         * bcash00p
001600***************************************************************** bcash00p
001700                                                                  bcash00p
001800 IDENTIFICATION DIVISION.                                         bcash00p
001900 PROGRAM-ID.                                                      bcash00p
002000     BCASH00P.                                                    bcash00p
002100 DATE-WRITTEN.                                                    bcash00p
002200     September 2002.                                              bcash00p
002300 DATE-COMPILED.                                                   bcash00p
002400     Today.                                                       bcash00p
002500                                                                  bcash00p
002600 ENVIRONMENT DIVISION.                                            bcash00p
002700                                                                  bcash00p
002800 DATA DIVISION.                                                   bcash00p
002900 WORKING-STORAGE SECTION.                                         bcash00p
003000 01  WS-MISC-STORAGE.                                             bcash00p
003100   05  WS-PROGRAM-ID                         PIC X(8)             bcash00p
003200       VALUE 'BCASH00P'.                                          bcash00p
003300   05  WS-COMMAREA-LENGTH                    PIC 9(5).            bcash00p
003400   05  WS-BUSINESS-PROGRAM                   PIC X(8)             bcash00p
003500       VALUE 'BCASH0?P'.                                          bcash00p
003600                                                                  bcash00p
003700 01  WS-CASH-DATA.                                                bcash00p
003800 COPY CCASHDAT.                                                   bcash00p
003900                                                                  bcash00p
004000 01  WS-PERSON-DATA.                                              bcash00p
004100 COPY CCASHD01.                                                   bcash00p
004200                                                                  bcash00p
004300 COPY CABENDD.                                                    bcash00p
004400                                                                  bcash00p
004500 LINKAGE SECTION.                                                 bcash00p
004600 01  DFHCOMMAREA.                                                 bcash00p
004700   05  LK-COMMAREA                           PIC X(6144).         bcash00p
004800                                                                  bcash00p
004900 COPY CENTRY.                                                     bcash00p
005000***************************************************************** bcash00p
005100* Make ourselves re-entrant                                     * bcash00p
005200***************************************************************** bcash00p
005300                                                                  bcash00p
005400***************************************************************** bcash00p
005500* Move the passed area to our area                              * bcash00p
005600***************************************************************** bcash00p
005700     MOVE DFHCOMMAREA (1:LENGTH OF WS-CASH-DATA) TO WS-CASH-DATA. bcash00p
005800                                                                  bcash00p
005900***************************************************************** bcash00p
006000* Ensure error message is cleared                               * bcash00p
006100***************************************************************** bcash00p
006200     MOVE SPACES TO CASH-ERROR-MSG.                               bcash00p
006300                                                                  bcash00p
006400***************************************************************** bcash00p
006500* This is the main process                                      * bcash00p
006600***************************************************************** bcash00p
006700     MOVE SPACES TO CD01-DATA.                                    bcash00p
006800     MOVE CASH-USERID TO CD01I-CONTACT-ID.                        bcash00p
006900* Now go get the data                                             bcash00p
007000 COPY CCASHX01.                                                   bcash00p
007100     SET CASH-PIN-STATUS-UNKNOWN TO TRUE.                         bcash00p
007200     EVALUATE TRUE                                                bcash00p
007300       WHEN CD01O-PIN IS EQUAL TO '????'                          bcash00p
007400         SET CASH-PIN-STATUS-NO-USER TO TRUE                      bcash00p
007500         MOVE 'Unknow user' TO CASH-ERROR-MSG                     bcash00p
007600       WHEN CD01O-PIN IS EQUAL TO '    '                          bcash00p
007700         SET CASH-PIN-STATUS-NO-PIN TO TRUE                       bcash00p
007800         MOVE 'No PIN on file for user' TO CASH-ERROR-MSG         bcash00p
007900       WHEN CD01O-PIN IS EQUAL TO CASH-PIN                        bcash00p
008000         SET CASH-PIN-STATUS-OK TO TRUE                           bcash00p
008100         MOVE SPACES TO CASH-ERROR-MSG                            bcash00p
008200       WHEN OTHER                                                 bcash00p
008300         SET CASH-PIN-STATUS-INVALID TO TRUE                      bcash00p
008400         MOVE 'PIN invalid' TO CASH-ERROR-MSG                     bcash00p
008500     END-EVALUATE.                                                bcash00p
008600     IF NOT CASH-PIN-STATUS-OK                                    bcash00p
008700        GO TO COMMON-RETURN                                       bcash00p
008800     END-IF.                                                      bcash00p
008900     MOVE CASH-REQUEST-CODE TO WS-BUSINESS-PROGRAM(7:1)           bcash00p
009000     EXEC CICS LINK PROGRAM(WS-BUSINESS-PROGRAM)                  bcash00p
009100                    COMMAREA(WS-CASH-DATA)                        bcash00p
009200                    LENGTH(LENGTH OF WS-CASH-DATA)                bcash00p
009300     END-EXEC.                                                    bcash00p
009400                                                                  bcash00p
009500 COMMON-RETURN.                                                   bcash00p
009600     MOVE WS-CASH-DATA TO DFHCOMMAREA (1:LENGTH OF WS-CASH-DATA). bcash00p
009700 COPY CRETURN.                                                    bcash00p
009800                                                                  bcash00p
009900* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     bcash00p
