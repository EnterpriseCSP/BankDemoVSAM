000100***************************************************************** bbank10p
000200*                                                               * bbank10p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * bbank10p
000400*   This demonstration program is provided for use by users     * bbank10p
000500*   of Micro Focus products and may be used, modified and       * bbank10p
000600*   distributed as part of your application provided that       * bbank10p
000700*   you properly acknowledge the copyright of Micro Focus       * bbank10p
000800*   in this material.                                           * bbank10p
000900*                                                               * bbank10p
001000***************************************************************** bbank10p
001100*                                                                  bbank10
001200***************************************************************** bbank10p
001300* Program:     BBANK10P.CBL                                     * bbank10p
001400* Layer:       Business logic                                   * bbank10p
001500* Function:    Signon to system to identify user                * bbank10p
001600***************************************************************** bbank10p
001700                                                                  bbank10p
001800 IDENTIFICATION DIVISION.                                         bbank10p
001900 PROGRAM-ID.                                                      bbank10p
002000     BBANK10P.                                                    bbank10p
002100 DATE-WRITTEN.                                                    bbank10p
002200     September 2002.                                              bbank10p
002300 DATE-COMPILED.                                                   bbank10p
002400     Today.                                                       bbank10p
002500                                                                  bbank10p
002600 ENVIRONMENT DIVISION.                                            bbank10p
002700                                                                  bbank10p
002800 DATA DIVISION.                                                   bbank10p
002900 WORKING-STORAGE SECTION.                                         bbank10p
003000 01  WS-MISC-STORAGE.                                             bbank10p
003100   05  WS-PROGRAM-ID                         PIC X(8)             bbank10p
003200       VALUE 'BBANK10P'.                                          bbank10p
003300   05  WS-INPUT-FLAG                         PIC X(1).            bbank10p
003400     88  INPUT-OK                            VALUE '0'.           bbank10p
003500     88  INPUT-ERROR                         VALUE '1'.           bbank10p
003600   05  WS-RETURN-FLAG                        PIC X(1).            bbank10p
003700     88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.    bbank10p
003800     88  WS-RETURN-FLAG-ON                   VALUE '1'.           bbank10p
003900   05  WS-RETURN-MSG                         PIC X(75).           bbank10p
004000     88  WS-RETURN-MSG-OFF                   VALUE SPACES.        bbank10p
004100   05  WS-PFK-FLAG                           PIC X(1).            bbank10p
004200     88  PFK-VALID                           VALUE '0'.           bbank10p
004300     88  PFK-INVALID                         VALUE '1'.           bbank10p
004400   05  WS-ERROR-MSG                          PIC X(75).           bbank10p
004500                                                                  bbank10p
004600 01  WS-BANK-DATA.                                                bbank10p
004700 COPY CBANKDAT.                                                   bbank10p
004800                                                                  bbank10p
004900 01  WS-HELP-DATA.                                                bbank10p
005000 COPY CHELPD01.                                                   bbank10p
005100                                                                  bbank10p
005200 01  WS-PERSON.                                                   bbank10p
005300 COPY CBANKD01.                                                   bbank10p
005400                                                                  bbank10p
005500 01  WS-SECURITY.                                                 bbank10p
005600 COPY CPSWDD01.                                                   bbank10p
005700                                                                  bbank10p
005800 COPY CABENDD.                                                    bbank10p
005900                                                                  bbank10p
006000 LINKAGE SECTION.                                                 bbank10p
006100 01  DFHCOMMAREA.                                                 bbank10p
006200   05  LK-COMMAREA                           PIC X(6144).         bbank10p
006300                                                                  bbank10p
006400*COPY CENTRY.                                                     bbank10p
001400 PROCEDURE DIVISION.                                              centry
001500                                                                  centry
006500***************************************************************** bbank10p
006600* Make ourselves re-entrant                                     * bbank10p
006700***************************************************************** bbank10p
006800     MOVE SPACES TO WS-ERROR-MSG.                                 bbank10p
006900                                                                  bbank10p
007000***************************************************************** bbank10p
007100* Move the passed area to our area                              * bbank10p
007200***************************************************************** bbank10p
007300     MOVE DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA) TO WS-BANK-DATA. bbank10p
007400                                                                  bbank10p
007500***************************************************************** bbank10p
007600* Ensure error message is cleared                               * bbank10p
007700***************************************************************** bbank10p
007800     MOVE SPACES TO BANK-ERROR-MSG.                               bbank10p
007900                                                                  bbank10p
008000***************************************************************** bbank10p
008100* If this is the first time in, then we have to do set up the   * bbank10p
008200* COMMAREA and ask for the first map to be displayed.           * bbank10p
008300***************************************************************** bbank10p
008400     IF BANK-NO-CONV-IN-PROGRESS                                  bbank10p
008500        SET BANK-CONV-IN-PROGRESS TO TRUE                         bbank10p
008600        MOVE 'BBANK10P' TO BANK-LAST-PROG                         bbank10p
008700        MOVE 'BBANK10P' TO BANK-NEXT-PROG                         bbank10p
008800        MOVE LOW-VALUES TO BANK-SIGNON-ID                         bbank10p
008900        MOVE LOW-VALUES TO BANK-USERID                            bbank10p
009000        MOVE LOW-VALUES TO BANK-PSWD                              bbank10p
009100        MOVE SPACES TO BANK-LAST-MAPSET                           bbank10p
009200        MOVE SPACES TO BANK-LAST-MAP                              bbank10p
009300        MOVE 'MBANK10' TO BANK-NEXT-MAPSET                        bbank10p
009400        MOVE 'BANK10A' TO BANK-NEXT-MAP                           bbank10p
009500        GO TO COMMON-RETURN                                       bbank10p
009600     END-IF.                                                      bbank10p
009700                                                                  bbank10p
009800***************************************************************** bbank10p
009900* This is the main process                                      * bbank10p
010000***************************************************************** bbank10p
010100                                                                  bbank10p
010200***************************************************************** bbank10p
010300* Save the passed return message and then turn it off           * bbank10p
010400***************************************************************** bbank10p
010500     MOVE BANK-RETURN-MSG TO WS-RETURN-MSG.                       bbank10p
010600     SET BANK-RETURN-MSG-OFF TO TRUE.                             bbank10p
010700                                                                  bbank10p
010800***************************************************************** bbank10p
010900* Check the AID to see if its valid at this point               * bbank10p
011000***************************************************************** bbank10p
011100     SET PFK-INVALID TO TRUE.                                     bbank10p
011200     IF BANK-AID-ENTER OR                                         bbank10p
011300        BANK-AID-PFK03                                            bbank10p
011400        SET PFK-VALID TO TRUE                                     bbank10p
011500     END-IF.                                                      bbank10p
011600     IF BANK-AID-PFK01 AND                                        bbank10p
011700        BANK-HELP-INACTIVE                                        bbank10p
011800        SET BANK-HELP-ACTIVE TO TRUE                              bbank10p
011900        SET PFK-VALID TO TRUE                                     bbank10p
012000     END-IF.                                                      bbank10p
012100     IF BANK-AID-PFK04 AND                                        bbank10p
012200        BANK-HELP-ACTIVE                                          bbank10p
012300        SET PFK-VALID TO TRUE                                     bbank10p
012400     END-IF.                                                      bbank10p
012500     IF PFK-INVALID                                               bbank10p
012600        SET BANK-AID-ENTER TO TRUE                                bbank10p
012700     END-IF.                                                      bbank10p
012800                                                                  bbank10p
012900***************************************************************** bbank10p
013000* Check the AID to see if we have to quit                       * bbank10p
013100***************************************************************** bbank10p
013200     IF BANK-AID-PFK03                                            bbank10p
013300        MOVE 'BBANK10P' TO BANK-LAST-PROG                         bbank10p
013400        MOVE 'BBANK99P' TO BANK-NEXT-PROG                         bbank10p
013500        MOVE 'MBANK10' TO BANK-LAST-MAPSET                        bbank10p
013600        MOVE 'BANK10A' TO BANK-LAST-MAP                           bbank10p
013700        MOVE 'MBANK99' TO BANK-NEXT-MAPSET                        bbank10p
013800        MOVE 'BANK99A' TO BANK-NEXT-MAP                           bbank10p
013900        GO TO COMMON-RETURN                                       bbank10p
014000     END-IF.                                                      bbank10p
014100                                                                  bbank10p
014200***************************************************************** bbank10p
014300* Check the to see if user needs or has been using help         * bbank10p
014400***************************************************************** bbank10p
014500     IF BANK-HELP-ACTIVE                                          bbank10p
014600        IF BANK-AID-PFK04                                         bbank10p
014700           SET BANK-HELP-INACTIVE TO TRUE                         bbank10p
014800           MOVE 00 TO BANK-HELP-SCREEN                            bbank10p
014900           MOVE 'BBANK10P' TO BANK-LAST-PROG                      bbank10p
015000           MOVE 'BBANK10P' TO BANK-NEXT-PROG                      bbank10p
015100           MOVE 'MBANK10' TO BANK-LAST-MAPSET                     bbank10p
015200           MOVE 'HELP10A' TO BANK-LAST-MAP                        bbank10p
015300           MOVE 'MBANK10' TO BANK-NEXT-MAPSET                     bbank10p
015400           MOVE 'BANK10A' TO BANK-NEXT-MAP                        bbank10p
015500           GO TO COMMON-RETURN                                    bbank10p
015600        ELSE                                                      bbank10p
015700           MOVE 01 TO BANK-HELP-SCREEN                            bbank10p
015800           MOVE 'BBANK10P' TO BANK-LAST-PROG                      bbank10p
015900           MOVE 'BBANK10P' TO BANK-NEXT-PROG                      bbank10p
016000           MOVE 'MBANK10' TO BANK-LAST-MAPSET                     bbank10p
016100           MOVE 'BANK10A' TO BANK-LAST-MAP                        bbank10p
016200           MOVE 'MBANK10' TO BANK-NEXT-MAPSET                     bbank10p
016300           MOVE 'HELP10A' TO BANK-NEXT-MAP                        bbank10p
016400           MOVE 'BANK10' TO HELP01I-SCRN                          bbank10p
016500*          COPY CHELPX01.                                         bbank10p
001600           EXEC CICS LINK PROGRAM('DHELP01P')                     chelpx01
001700                    COMMAREA(HELP01-DATA)                         chelpx01
001800                    LENGTH(LENGTH OF HELP01-DATA)                 chelpx01
001900           END-EXEC                                               chelpx01

016600           MOVE HELP01O-DATA TO BANK-HELP-DATA                    bbank10p
016700           GO TO COMMON-RETURN                                    bbank10p
016800     END-IF.                                                      bbank10p
016900                                                                  bbank10p
017000     PERFORM VALIDATE-USER THRU                                   bbank10p
017100             VALIDATE-USER-EXIT.                                  bbank10p
017200                                                                  bbank10p
017300* If we had an error display error and return                     bbank10p
017400     IF INPUT-ERROR                                               bbank10p
017500        MOVE WS-ERROR-MSG TO BANK-ERROR-MSG                       bbank10p
017600        MOVE 'SBANK10P' TO BANK-LAST-PROG                         bbank10p
017700        MOVE 'SBANK10P' TO BANK-NEXT-PROG                         bbank10p
017800        MOVE 'MBANK10' TO BANK-LAST-MAPSET                        bbank10p
017900        MOVE 'BANK10A' TO BANK-LAST-MAP                           bbank10p
018000        MOVE 'MBANK10' TO BANK-NEXT-MAPSET                        bbank10p
018100        MOVE 'BANK10A' TO BANK-NEXT-MAP                           bbank10p
018200        GO TO COMMON-RETURN                                       bbank10p
018300     END-IF.                                                      bbank10p
018400                                                                  bbank10p
018500     MOVE 'BBANK20P' TO BANK-NEXT-PROG.                           bbank10p
018600     GO TO COMMON-RETURN.                                         bbank10p
018700                                                                  bbank10p
018800 COMMON-RETURN.                                                   bbank10p
018900     MOVE WS-BANK-DATA TO DFHCOMMAREA (1:LENGTH OF WS-BANK-DATA). bbank10p
019000*COPY CRETURN.                                                    bbank10p
001400     EXEC CICS RETURN                                             creturn
001500     END-EXEC.                                                    creturn
001600     GOBACK.                                                      creturn
001700                                                                  creturn
019100                                                                  bbank10p
019200 VALIDATE-USER.                                                   bbank10p
019300     SET INPUT-OK TO TRUE.                                        bbank10p
019400     INSPECT BANK-SIGNON-ID                                       bbank10p
019500       CONVERTING 'abcdefghijklmnopqrstuvwxyz'                    bbank10p
019600               TO 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.                   bbank10p
019700     IF BANK-SIGNON-ID IS EQUAL TO 'GUEST'                        bbank10p
019800        MOVE 'GUEST' TO BANK-USERID                               bbank10p
019900        MOVE 'Guest' TO BANK-USERID-NAME                          bbank10p
020000        GO TO VALIDATE-USER-EXIT                                  bbank10p
020100     END-IF.                                                      bbank10p
020200     IF BANK-SIGNON-ID IS EQUAL TO LOW-VALUES                     bbank10p
020300        MOVE 'Please input user name' TO WS-ERROR-MSG             bbank10p
020400        GO TO VALIDATE-USER-ERROR                                 bbank10p
020500     END-IF.                                                      bbank10p
020600     IF BANK-PSWD IS EQUAL TO LOW-VALUES                          bbank10p
020700        MOVE 'Please input your password' TO WS-ERROR-MSG         bbank10p
020800        GO TO VALIDATE-USER-ERROR                                 bbank10p
020900     END-IF.                                                      bbank10p
021000* We now make sure the user is valid.......                       bbank10p
021100     MOVE SPACES TO CPSWDD01-DATA.                                bbank10p
021200     MOVE BANK-SIGNON-ID TO CPSWDD01I-USERID.                     bbank10p
021300     MOVE BANK-PSWD TO CPSWDD01I-PASSWORD                         bbank10p
021400* If user starts with "Z" then treat as "B"                       bbank10p
021500     IF CPSWDD01I-USERID(1:1) IS EQUAL TO 'Z'                     bbank10p
021600        MOVE 'B' TO  CPSWDD01I-USERID(1:1)                        bbank10p
021700     END-IF.                                                      bbank10p
021800                                                                  bbank10p
021900     SET PSWD-SIGNON TO TRUE                                      bbank10p
022000                                                                  bbank10p
022100*COPY CPSWDX01.                                                   bbank10p
001600     EXEC CICS LINK PROGRAM('SPSWD01P')                           cpswdx01
001700                    COMMAREA(CPSWDD01-DATA)                       cpswdx01
001800                    LENGTH(LENGTH OF CPSWDD01-DATA)               cpswdx01
001900     END-EXEC                                                     cpswdx01

022200     IF CPSWDD01O-MESSAGE IS NOT EQUAL TO SPACES                  bbank10p
022300        MOVE CPSWDD01O-MESSAGE TO WS-ERROR-MSG                    bbank10p
022400        GO TO VALIDATE-USER-ERROR                                 bbank10p
022500     END-IF.                                                      bbank10p
022600* We now make sure the user is actually a customer......          bbank10p
022700     MOVE SPACES TO CD01-DATA.                                    bbank10p
022800     MOVE BANK-SIGNON-ID TO CD01I-PERSON-PID.                     bbank10p
022900* If user starts with "Z" then treat as "B"                       bbank10p
023000     IF CD01I-PERSON-PID(1:1) IS EQUAL TO 'Z'                     bbank10p
023100        MOVE 'B' TO  CD01I-PERSON-PID(1:1)                        bbank10p
023200     END-IF.                                                      bbank10p
023300*COPY CBANKX01.                                                   bbank10p
001600     EXEC CICS LINK PROGRAM('DBANK01P')                           cbankx01
001700                    COMMAREA(CD01-DATA)                           cbankx01
001800                    LENGTH(LENGTH OF CD01-DATA)                   cbankx01
001900     END-EXEC.                                                    cbankx01

023400     IF CD01O-PERSON-PID IS EQUAL TO SPACES                       bbank10p
023500        MOVE CD01O-PERSON-NAME TO WS-ERROR-MSG                    bbank10p
023600        GO TO VALIDATE-USER-ERROR                                 bbank10p
023700     ELSE                                                         bbank10p
023800        MOVE CD01O-PERSON-NAME TO BANK-USERID-NAME                bbank10p
023900        MOVE BANK-SIGNON-ID TO BANK-USERID                        bbank10p
024000        IF BANK-USERID(1:1) IS EQUAL TO 'Z'                       bbank10p
024100           MOVE 'B' TO  BANK-USERID(1:1)                          bbank10p
024200        END-IF                                                    bbank10p
024300        GO TO VALIDATE-USER-EXIT                                  bbank10p
024400     END-IF.                                                      bbank10p
024500 VALIDATE-USER-ERROR.                                             bbank10p
024600     SET INPUT-ERROR TO TRUE.                                     bbank10p
024700 VALIDATE-USER-EXIT.                                              bbank10p
024800     EXIT.                                                        bbank10p
024900                                                                  bbank10p
025000* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     bbank10p
