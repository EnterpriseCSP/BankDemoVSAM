000100***************************************************************** SPSWD01P
000200*                                                               * SPSWD01P
000300*   Copyright (C) 1998-2008 Micro Focus. All Rights Reserved.   * SPSWD01P
000400*   This demonstration program is provided for use by users     * SPSWD01P
000500*   of Micro Focus products and may be used, modified and       * SPSWD01P
000600*   distributed as part of your application provided that       * SPSWD01P
000700*   you properly acknowledge the copyright of Micro Focus       * SPSWD01P
000800*   in this material.                                           * SPSWD01P
000900*                                                               * SPSWD01P
001000***************************************************************** SPSWD01P
001100                                                                  SPSWD01P
001200***************************************************************** SPSWD01P
001300* Program:     SPSWD01P.CBL (CICS Version)                      * SPSWD01P
001400* Layer:       Transaction manager specific                     * SPSWD01P
001500* Function:    Perform security operations (sigon, signoff etc)  *SPSWD01P
001600***************************************************************** SPSWD01P
001700                                                                  SPSWD01P
001800 IDENTIFICATION DIVISION.                                         SPSWD01P
001900 PROGRAM-ID.                                                      SPSWD01P
002000     SPSWD01P.                                                    SPSWD01P
002100 DATE-WRITTEN.                                                    SPSWD01P
002200     September 2002.                                              SPSWD01P
002300 DATE-COMPILED.                                                   SPSWD01P
002400     Today.                                                       SPSWD01P
002500                                                                  SPSWD01P
002600 ENVIRONMENT DIVISION.                                            SPSWD01P
002700                                                                  SPSWD01P
002800 DATA DIVISION.                                                   SPSWD01P
002900                                                                  SPSWD01P
003000 WORKING-STORAGE SECTION.                                         SPSWD01P
003100 01  WS-MISC-STORAGE.                                             SPSWD01P
003200   05  WS-PROGRAM-ID                         PIC X(8)             SPSWD01P
003300       VALUE 'SPSWD01P'.                                          SPSWD01P
003400   05  WS-COMMAREA-LENGTH                    PIC 9(5).            SPSWD01P
003500   05  WS-RESP                               PIC S9(8) COMP.      SPSWD01P
003600   05  WS-EIBRESP-DISP                       PIC ZZ9.             SPSWD01P
003700   05  WS-EIBRESP2-DISP                      PIC ZZ9.             SPSWD01P
003800   05  WS-SECURITY-TRAN                      PIC X(8).            SPSWD01P
003900   05  WS-SECURITY-FLAG                      PIC X(1).            SPSWD01P
004000     88  SECURITY-REQUIRED                   VALUE 'Y'.           SPSWD01P
004100                                                                  SPSWD01P
004200 01  WS-COMMAREA.                                                 SPSWD01P
004300 COPY CPSWDD01.                                                   SPSWD01P
004400                                                                  SPSWD01P
004500 01  WS-MSG-DATA                             PIC X(80).           SPSWD01P
004600 01  WS-MSG-LEN                              PIC S9(8) COMP.      SPSWD01P
004700                                                                  SPSWD01P
004800 COPY CABENDD.                                                    SPSWD01P
004900                                                                  SPSWD01P
005000 LINKAGE SECTION.                                                 SPSWD01P
005100 01  DFHCOMMAREA.                                                 SPSWD01P
005200   05  LK-COMMAREA                           PIC X(1)             SPSWD01P
005300       OCCURS 1 TO 4096 TIMES                                     SPSWD01P
005400         DEPENDING ON WS-COMMAREA-LENGTH.                         SPSWD01P
005500                                                                  SPSWD01P
005600 COPY CENTRY.                                                     SPSWD01P
005700***************************************************************** SPSWD01P
005800* Move the passed data to our area                              * SPSWD01P
005900***************************************************************** SPSWD01P
006000     MOVE LENGTH OF WS-COMMAREA TO WS-COMMAREA-LENGTH.            SPSWD01P
006100     MOVE DFHCOMMAREA TO WS-COMMAREA.                             SPSWD01P
006200                                                                  SPSWD01P
006300***************************************************************** SPSWD01P
006400* Initialize our output area                                    * SPSWD01P
006500***************************************************************** SPSWD01P
006600     MOVE SPACES TO CPSWDD01O-DATA.                               SPSWD01P
006700                                                                  SPSWD01P
006800***************************************************************** SPSWD01P
006900* Call SSECUREP to see if we need to do security processing     * SPSWD01P
007000***************************************************************** SPSWD01P
007100     MOVE EIBTRNID TO WS-SECURITY-TRAN.                           SPSWD01P
007200     CALL 'SSECUREP' USING WS-SECURITY-TRAN                       SPSWD01P
007300                           WS-SECURITY-FLAG.                      SPSWD01P
007400                                                                  SPSWD01P
007500***************************************************************** SPSWD01P
007600* If required perform requested processing                      * SPSWD01P
007700***************************************************************** SPSWD01P
007800     IF SECURITY-REQUIRED                                         SPSWD01P
007900        EVALUATE TRUE                                             SPSWD01P
008000          WHEN PSWD-NOOP                                          SPSWD01P
008100            PERFORM NOOP-PROCESS                                  SPSWD01P
008200          WHEN PSWD-SIGNON                                        SPSWD01P
008300            PERFORM SIGNON-PROCESS                                SPSWD01P
008400          WHEN PSWD-SIGNOFF                                       SPSWD01P
008500            PERFORM SIGNOFF-PROCESS                               SPSWD01P
008600          WHEN OTHER                                              SPSWD01P
008700            PERFORM NOOP-PROCESS                                  SPSWD01P
008800        END-EVALUATE                                              SPSWD01P
008900     END-IF.                                                      SPSWD01P
009000     INSPECT CPSWDD01O-MESSAGE REPLACING ALL '~' BY ' '.          SPSWD01P
009100                                                                  SPSWD01P
009200***************************************************************** SPSWD01P
009300* Move the result back to the callers area                      * SPSWD01P
009400***************************************************************** SPSWD01P
009500     MOVE WS-COMMAREA TO DFHCOMMAREA(1:WS-COMMAREA-LENGTH).       SPSWD01P
009600                                                                  SPSWD01P
009700***************************************************************** SPSWD01P
009800* Return to our caller                                          * SPSWD01P
009900***************************************************************** SPSWD01P
010000 COPY CRETURN.                                                    SPSWD01P
010100                                                                  SPSWD01P
010200***************************************************************** SPSWD01P
010300* SIGNON Process                                                * SPSWD01P
010400***************************************************************** SPSWD01P
010500 SIGNON-PROCESS.                                                  SPSWD01P
010600     EXEC CICS SIGNOFF                                            SPSWD01P
010700          RESP(WS-RESP)                                           SPSWD01P
010800     END-EXEC.                                                    SPSWD01P
010900                                                                  SPSWD01P
011000     EXEC CICS SIGNON                                             SPSWD01P
011100          USERID(CPSWDD01I-USERID)                                SPSWD01P
011200          PASSWORD(CPSWDD01I-PASSWORD)                            SPSWD01P
011300          RESP(WS-RESP)                                           SPSWD01P
011400     END-EXEC.                                                    SPSWD01P
011500     IF WS-RESP IS EQUAL TO DFHRESP(USERIDERR) AND                SPSWD01P
011600        EIBRESP2 IS EQUAL TO 8                                    SPSWD01P
011700        MOVE FUNCTION LOWER-CASE(CPSWDD01I-USERID)                SPSWD01P
011800          TO CPSWDD01I-USERID                                     SPSWD01P
011900        EXEC CICS SIGNON                                          SPSWD01P
012000             USERID(CPSWDD01I-USERID)                             SPSWD01P
012100             PASSWORD(CPSWDD01I-PASSWORD)                         SPSWD01P
012200             RESP(WS-RESP)                                        SPSWD01P
012300        END-EXEC                                                  SPSWD01P
012400     END-IF.                                                      SPSWD01P
012500                                                                  SPSWD01P
012600     IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)                   SPSWD01P
012700        MOVE EIBRESP TO WS-EIBRESP-DISP                           SPSWD01P
012800        MOVE EIBRESP2 TO WS-EIBRESP2-DISP                         SPSWD01P
012900        MOVE SPACES TO WS-MSG-DATA                                SPSWD01P
013000        IF WS-RESP IS EQUAL TO DFHRESP(NOTAUTH)                   SPSWD01P
013100           IF EIBRESP2 IS EQUAL TO 1                              SPSWD01P
013200              MOVE 'A password is required~'                      SPSWD01P
013300                TO WS-MSG-DATA                                    SPSWD01P
013400           END-IF                                                 SPSWD01P
013500           IF EIBRESP2 IS EQUAL TO 2                              SPSWD01P
013600              MOVE 'The supplied password is wrong~'              SPSWD01P
013700                TO WS-MSG-DATA                                    SPSWD01P
013800           END-IF                                                 SPSWD01P
013900           IF EIBRESP2 IS EQUAL TO 3                              SPSWD01P
014000              MOVE 'A new password is requied~'                   SPSWD01P
014100                TO WS-MSG-DATA                                    SPSWD01P
014200           END-IF                                                 SPSWD01P
014300           IF EIBRESP2 IS EQUAL TO 4                              SPSWD01P
014400              MOVE 'The new password is not acceptable~'          SPSWD01P
014500                TO WS-MSG-DATA                                    SPSWD01P
014600           END-IF                                                 SPSWD01P
014700           IF EIBRESP2 IS EQUAL TO 19                             SPSWD01P
014800              MOVE 'The USERID is revoked~'                       SPSWD01P
014900                TO WS-MSG-DATA                                    SPSWD01P
015000           END-IF                                                 SPSWD01P
015100           IF WS-MSG-DATA IS EQUAL TO SPACES                      SPSWD01P
015200              STRING 'EIBRESP=NOTAUTH, EIBRESP2='                 SPSWD01P
015300                       DELIMITED BY SIZE                          SPSWD01P
015400                     WS-EIBRESP2-DISP DELIMITED BY SIZE           SPSWD01P
015500                     '~' DELIMITED BY SIZE                        SPSWD01P
015600                INTO WS-MSG-DATA                                  SPSWD01P
015700           END-IF                                                 SPSWD01P
015800        END-IF                                                    SPSWD01P
015900        IF WS-RESP IS EQUAL TO DFHRESP(USERIDERR)                 SPSWD01P
016000           IF EIBRESP2 IS EQUAL TO 8                              SPSWD01P
016100              MOVE 'USERID not known to security manager~'        SPSWD01P
016200                TO WS-MSG-DATA                                    SPSWD01P
016300           END-IF                                                 SPSWD01P
016400           IF EIBRESP2 IS EQUAL TO 30                             SPSWD01P
016500              MOVE 'USERID is blank/null~'                        SPSWD01P
016600                TO WS-MSG-DATA                                    SPSWD01P
016700           END-IF                                                 SPSWD01P
016800           IF WS-MSG-DATA IS EQUAL TO SPACES                      SPSWD01P
016900              STRING 'EIBRESP=USERIDERR, EIBRESP2='               SPSWD01P
017000                       DELIMITED BY SIZE                          SPSWD01P
017100                     WS-EIBRESP2-DISP DELIMITED BY SIZE           SPSWD01P
017200                     '~' DELIMITED BY SIZE                        SPSWD01P
017300                INTO WS-MSG-DATA                                  SPSWD01P
017400           END-IF                                                 SPSWD01P
017500        END-IF                                                    SPSWD01P
017600        IF WS-RESP IS EQUAL TO DFHRESP(INVREQ)                    SPSWD01P
017700           IF WS-MSG-DATA IS EQUAL TO SPACES                      SPSWD01P
017800              STRING 'EIBRESP=NOTAUTH, EIBRESP2='                 SPSWD01P
017900                       DELIMITED BY SIZE                          SPSWD01P
018000                     WS-EIBRESP2-DISP DELIMITED BY SIZE           SPSWD01P
018100                     '~' DELIMITED BY SIZE                        SPSWD01P
018200                INTO WS-MSG-DATA                                  SPSWD01P
018300           END-IF                                                 SPSWD01P
018400        END-IF                                                    SPSWD01P
018500        IF WS-RESP IS NOT EQUAL TO DFHRESP(INVREQ) AND            SPSWD01P
018600           WS-RESP IS NOT EQUAL TO DFHRESP(NOTAUTH) AND           SPSWD01P
018700           WS-RESP IS NOT EQUAL TO DFHRESP(USERIDERR)             SPSWD01P
018800           STRING EIBTRMID DELIMITED BY SIZE                      SPSWD01P
018900                  ' Invalid request. EIBRESP=' DELIMITED BY SIZE  SPSWD01P
019000                  WS-EIBRESP-DISP DELIMITED BY SIZE               SPSWD01P
019100                  ', EIBRESP2=' DELIMITED BY SIZE                 SPSWD01P
019200                  WS-EIBRESP2-DISP DELIMITED BY SIZE              SPSWD01P
019300                  '~' DELIMITED BY SIZE                           SPSWD01P
019400             INTO WS-MSG-DATA                                     SPSWD01P
019500           MOVE WS-MSG-DATA TO CPSWDD01O-MESSAGE                  SPSWD01P
019600           PERFORM DISPLAY-MSG                                    SPSWD01P
019700        END-IF                                                    SPSWD01P
019800        MOVE WS-MSG-DATA TO CPSWDD01O-MESSAGE                     SPSWD01P
019900        PERFORM DISPLAY-MSG                                       SPSWD01P
020000     END-IF.                                                      SPSWD01P
020100                                                                  SPSWD01P
020200 SIGNON-PROCESS-EXIT.                                             SPSWD01P
020300     EXIT.                                                        SPSWD01P
020400                                                                  SPSWD01P
020500***************************************************************** SPSWD01P
020600* SIGNOFF Process                                               * SPSWD01P
020700***************************************************************** SPSWD01P
020800 SIGNOFF-PROCESS.                                                 SPSWD01P
020900     EXEC CICS SIGNOFF                                            SPSWD01P
021000          RESP(WS-RESP)                                           SPSWD01P
021100     END-EXEC.                                                    SPSWD01P
021200     IF WS-RESP IS EQUAL TO DFHRESP(NORMAL)                       SPSWD01P
021300        GO TO SIGNOFF-PROCESS-EXIT                                SPSWD01P
021400     END-IF.                                                      SPSWD01P
021500     MOVE EIBRESP TO WS-EIBRESP-DISP.                             SPSWD01P
021600     MOVE EIBRESP2 TO WS-EIBRESP2-DISP.                           SPSWD01P
021700     IF WS-RESP IS EQUAL TO DFHRESP(INVREQ)                       SPSWD01P
021800        MOVE SPACES TO WS-MSG-DATA                                SPSWD01P
021900        STRING EIBTRMID DELIMITED BY SIZE                         SPSWD01P
022000               ' Invalid operation. EIBRESP=' DELIMITED BY SIZE   SPSWD01P
022100               WS-EIBRESP-DISP DELIMITED BY SIZE                  SPSWD01P
022200               ', EIBRESP2=' DELIMITED BY SIZE                    SPSWD01P
022300               WS-EIBRESP2-DISP DELIMITED BY SIZE                 SPSWD01P
022400               '~' DELIMITED BY SIZE                              SPSWD01P
022500          INTO WS-MSG-DATA                                        SPSWD01P
022600        PERFORM DISPLAY-MSG                                       SPSWD01P
022700        GO TO SIGNOFF-PROCESS-EXIT                                SPSWD01P
022800     ELSE                                                         SPSWD01P
022900        MOVE SPACES TO WS-MSG-DATA                                SPSWD01P
023000        STRING EIBTRMID DELIMITED BY SIZE                         SPSWD01P
023100               ' Invalid request. EIBRESP=' DELIMITED BY SIZE     SPSWD01P
023200               WS-EIBRESP-DISP DELIMITED BY SIZE                  SPSWD01P
023300               ', EIBRESP2=' DELIMITED BY SIZE                    SPSWD01P
023400               WS-EIBRESP2-DISP DELIMITED BY SIZE                 SPSWD01P
023500               '~' DELIMITED BY SIZE                              SPSWD01P
023600          INTO WS-MSG-DATA                                        SPSWD01P
023700        PERFORM DISPLAY-MSG                                       SPSWD01P
023800        GO TO SIGNOFF-PROCESS-EXIT                                SPSWD01P
023900     END-IF                                                       SPSWD01P
024000     .                                                            SPSWD01P
024100 SIGNOFF-PROCESS-EXIT.                                            SPSWD01P
024200     EXIT.                                                        SPSWD01P
024300                                                                  SPSWD01P
024400***************************************************************** SPSWD01P
024500* NOOP Process                                                  * SPSWD01P
024600***************************************************************** SPSWD01P
024700 NOOP-PROCESS.                                                    SPSWD01P
024800     CONTINUE.                                                    SPSWD01P
024900 NOOP-PROCESS-EXIT.                                               SPSWD01P
025000     EXIT.                                                        SPSWD01P
025100                                                                  SPSWD01P
025200***************************************************************** SPSWD01P
025300* Write the log message                                         * SPSWD01P
025400***************************************************************** SPSWD01P
025500 DISPLAY-MSG.                                                     SPSWD01P
025600     MOVE 0 TO WS-MSG-LEN.                                        SPSWD01P
025700     INSPECT WS-MSG-DATA TALLYING WS-MSG-LEN                      SPSWD01P
025800       FOR CHARACTERS BEFORE '~'.                                 SPSWD01P
025900     EXEC CICS WRITE                                              SPSWD01P
026000               OPERATOR                                           SPSWD01P
026100               TEXT(WS-MSG-DATA)                                  SPSWD01P
026200               TEXTLENGTH(WS-MSG-LEN)                             SPSWD01P
026300     END-EXEC.                                                    SPSWD01P
026400                                                                  SPSWD01P
026500* $ Version 5.98b sequenced on Thursday 5 Feb 2009 at 11:00pm     SPSWD01P
