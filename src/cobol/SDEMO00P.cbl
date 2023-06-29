000100***************************************************************** SDEMO00P
000200*                                                               * SDEMO00P
000300*   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * SDEMO00P
000400*   This demonstration program is provided for use by users     * SDEMO00P
000500*   of Micro Focus products and may be used, modified and       * SDEMO00P
000600*   distributed as part of your application provided that       * SDEMO00P
000700*   you properly acknowledge the copyright of Micro Focus       * SDEMO00P
000800*   in this material.                                           * SDEMO00P
000900*                                                               * SDEMO00P
001000***************************************************************** SDEMO00P
001100                                                                  SDEMO00P
001200***************************************************************** SDEMO00P
001300* Program:     SDEMO00P.CBL (CICS Version)                      * SDEMO00P
001400* Layer:       Screen handling                                  * SDEMO00P
001500* Function:    Screen handling control module                   * SDEMO00P
001600***************************************************************** SDEMO00P
001700                                                                  SDEMO00P
001800 IDENTIFICATION DIVISION.                                         SDEMO00P
001900 PROGRAM-ID.                                                      SDEMO00P
002000     SDEMO00P.                                                    SDEMO00P
002100 DATE-WRITTEN.                                                    SDEMO00P
002200     September 2002.                                              SDEMO00P
002300 DATE-COMPILED.                                                   SDEMO00P
002400     Today.                                                       SDEMO00P
002500                                                                  SDEMO00P
002600 ENVIRONMENT DIVISION.                                            SDEMO00P
002700                                                                  SDEMO00P
002800 DATA DIVISION.                                                   SDEMO00P
002900 WORKING-STORAGE SECTION.                                         SDEMO00P
003000 01  WS-MISC-STORAGE.                                             SDEMO00P
003100   05  WS-PROGRAM-ID                         PIC X(8)             SDEMO00P
003200       VALUE 'SDEMO00P'.                                          SDEMO00P
003300   05  WS-TRAN-ID                            PIC X(4).            SDEMO00P
003400   05  WS-SCREEN-LOGIC-PGM                   PIC X(8)             SDEMO00P
003500       VALUE SPACES.                                              SDEMO00P
003600   05  WS-DYNAMIC-PGM                        PIC X(8)             SDEMO00P
003700       VALUE 'UNKNOWN'.                                           SDEMO00P
003800   05  WS-ABSTIME                            PIC S9(15) COMP-3.   SDEMO00P
003900   05  WS-RESP                               PIC S9(8) COMP.      SDEMO00P
004000   05  WS-CICS-INQ-FIELDS.                                        SDEMO00P
004100     10  WS-CICS-INQ-TRAN                    PIC X(4).            SDEMO00P
004200     10  WS-CICS-INQ-PGM                     PIC X(8).            SDEMO00P
004300     10  WS-CICS-INQ-STATUS                  PIC S9(8) COMP.      SDEMO00P
004400   05  WS-TXN-SWITCH-IND                     PIC X(1).            SDEMO00P
004500     88  TXN-SWITCH-OK                       VALUE '0'.           SDEMO00P
004600     88  TXN-SWITCH-ERR1                     VALUE '1'.           SDEMO00P
004700     88  TXN-SWITCH-ERR2                     VALUE '2'.           SDEMO00P
004800   05  WS-CICS-LOAD-FIELDS.                                       SDEMO00P
004900     10  WS-CICS-LOAD-PGM                    PIC X(8).            SDEMO00P
005000     10  WS-CICS-LOAD-ENTRY                  POINTER.             SDEMO00P
005100     10  WS-CICS-LOAD-ENTRY-R REDEFINES WS-CICS-LOAD-ENTRY        SDEMO00P
005200                                             PIC X(4).            SDEMO00P
005300                                                                  SDEMO00P
005400 01  WS-DEMO-DATA.                                                SDEMO00P
005500 COPY CDEMODAT.                                                   SDEMO00P
005600                                                                  SDEMO00P
005700 01  WS-DEMO-OPTIONS.                                             SDEMO00P
005800 COPY COPTIONS.                                                   SDEMO00P
005900                                                                  SDEMO00P
006000 01  WS-PASSED-DATA.                                              SDEMO00P
006100   05  FILLER                                PIC X(8).            SDEMO00P
006200                                                                  SDEMO00P
006300                                                                  SDEMO00P
006400 01  TS-DATA.                                                     SDEMO00P
006500   05  TS-QUEUE-NAME                         PIC X(8).            SDEMO00P
006600   05  TS-QUEUE-NAME-PARTS REDEFINES TS-QUEUE-NAME.               SDEMO00P
006700     10  TS-QUEUE-NAME-PART1                 PIC X(4).            SDEMO00P
006800     10  TS-QUEUE-NAME-PART2                 PIC 9(4).            SDEMO00P
006900   05  TS-QUEUE-LEN                          PIC S9(4) COMP.      SDEMO00P
007000   05  TS-QUEUE-ITEM                         PIC S9(4) COMP.      SDEMO00P
007100   05  TS-QUEUE-DATA                         PIC X(6144).         SDEMO00P
007200                                                                  SDEMO00P
007300 COPY DFHAID.                                                     SDEMO00P
007400                                                                  SDEMO00P
007500 COPY DFHBMSCA.                                                   SDEMO00P
007600                                                                  SDEMO00P
007700 COPY CABENDD.                                                    SDEMO00P
007800                                                                  SDEMO00P
007900 LINKAGE SECTION.                                                 SDEMO00P
008000 01  DFHCOMMAREA.                                                 SDEMO00P
008100   05  LK-TS-QUEUE-NAME                      PIC X(8).            SDEMO00P
008200 COPY CDEMOEXT.                                                   SDEMO00P
008300                                                                  SDEMO00P
008400 PROCEDURE DIVISION.                                              SDEMO00P
008500***************************************************************** SDEMO00P
008600* Store our transaction-id                                      * SDEMO00P
008700***************************************************************** SDEMO00P
008800     MOVE EIBTRNID TO WS-TRAN-ID.                                 SDEMO00P
008900                                                                  SDEMO00P
009000***************************************************************** SDEMO00P
009100* If this is the first time in, then we assume we are running   * SDEMO00P
009200* from a CICS terminal so we display map DEMO01A and return with* SDEMO00P
009300* with our COMMAREA set up.                                     * SDEMO00P
009400***************************************************************** SDEMO00P
009500     IF (EIBCALEN IS EQUAL TO 0) OR                               SDEMO00P
009600        (EIBCALEN IS NOT EQUAL TO 0 AND                           SDEMO00P
009700         LK-TS-QUEUE-NAME IS EQUAL TO 'INET****')                 SDEMO00P
009800        MOVE LOW-VALUES TO WS-DEMO-DATA                           SDEMO00P
009900        IF EIBCALEN IS EQUAL TO 0                                 SDEMO00P
010000           SET DEMO-ENV-CICS TO TRUE                              SDEMO00P
010100        ELSE                                                      SDEMO00P
010200           SET DEMO-ENV-INET TO TRUE                              SDEMO00P
010300        END-IF                                                    SDEMO00P
010400        SET DEMO-NO-CONV-IN-PROGRESS TO TRUE                      SDEMO00P
010500        MOVE SPACES TO DEMO-LAST-MAPSET                           SDEMO00P
010600        MOVE SPACES TO DEMO-LAST-MAP                              SDEMO00P
010700        MOVE SPACES TO DEMO-LAST-PROG                             SDEMO00P
010800        MOVE SPACES TO DEMO-NEXT-PROG                             SDEMO00P
010900        MOVE SPACES TO DEMO-NEXT-TRAN                             SDEMO00P
011000        MOVE WS-TRAN-ID TO DEMO-CICS-TRANCODE                     SDEMO00P
011100        EXEC CICS ASKTIME                                         SDEMO00P
011200                  ABSTIME(WS-ABSTIME)                             SDEMO00P
011300        END-EXEC                                                  SDEMO00P
011400        MOVE DEMO-ENV TO TS-QUEUE-NAME-PART1                      SDEMO00P
011500        MOVE WS-ABSTIME TO TS-QUEUE-NAME-PART2                    SDEMO00P
011600        EXEC CICS DELETEQ TS                                      SDEMO00P
011700                  QUEUE(TS-QUEUE-NAME)                            SDEMO00P
011800                  RESP(WS-RESP)                                   SDEMO00P
011900        END-EXEC                                                  SDEMO00P
012000        MOVE SPACES TO TS-QUEUE-DATA                              SDEMO00P
012100        MOVE LENGTH OF TS-QUEUE-DATA TO TS-QUEUE-LEN              SDEMO00P
012200        MOVE 0 TO TS-QUEUE-ITEM                                   SDEMO00P
012300        MOVE 0 TO WS-RESP                                         SDEMO00P
012400        EXEC CICS WRITEQ TS                                       SDEMO00P
012500                  QUEUE(TS-QUEUE-NAME)                            SDEMO00P
012600                  FROM(TS-QUEUE-DATA)                             SDEMO00P
012700                  LENGTH(TS-QUEUE-LEN)                            SDEMO00P
012800                  ITEM(TS-QUEUE-ITEM)                             SDEMO00P
012900                  RESP(WS-RESP)                                   SDEMO00P
013000        END-EXEC                                                  SDEMO00P
013100        IF DEMO-ENV-INET                                          SDEMO00P
013200           MOVE TS-QUEUE-NAME TO LK-TS-QUEUE-NAME                 SDEMO00P
013300        END-IF                                                    SDEMO00P
013400     ELSE                                                         SDEMO00P
013500        MOVE LOW-VALUES TO WS-DEMO-DATA                           SDEMO00P
013600        MOVE LK-TS-QUEUE-NAME TO TS-QUEUE-NAME                    SDEMO00P
013700        MOVE 1 TO TS-QUEUE-ITEM                                   SDEMO00P
013800        EXEC CICS READQ TS                                        SDEMO00P
013900                  QUEUE(TS-QUEUE-NAME)                            SDEMO00P
014000                  INTO(TS-QUEUE-DATA)                             SDEMO00P
014100                  LENGTH(TS-QUEUE-LEN)                            SDEMO00P
014200                  ITEM(TS-QUEUE-ITEM)                             SDEMO00P
014300        END-EXEC                                                  SDEMO00P
014400        MOVE TS-QUEUE-DATA TO WS-DEMO-DATA                        SDEMO00P
014500     END-IF.                                                      SDEMO00P
014600                                                                  SDEMO00P
014700***************************************************************** SDEMO00P
014800* If we get this far then this is not the first time in as we   * SDEMO00P
014900* have a COMMAREA. Check that DEMO-ENV is set correctly to      * SDEMO00P
015000* ensure we are running in the correct environment etc          * SDEMO00P
015100***************************************************************** SDEMO00P
015200     IF NOT DEMO-ENV-CICS AND                                     SDEMO00P
015300        NOT DEMO-ENV-INET                                         SDEMO00P
015400        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       SDEMO00P
015500        MOVE '0001' TO ABEND-CODE                                 SDEMO00P
015600        MOVE SPACES TO ABEND-REASON                               SDEMO00P
015700        COPY CABENDPO.                                            SDEMO00P
015800     END-IF.                                                      SDEMO00P
015900                                                                  SDEMO00P
016000***************************************************************** SDEMO00P
016100* This is the main process                                      * SDEMO00P
016200***************************************************************** SDEMO00P
016300                                                                  SDEMO00P
016400***************************************************************** SDEMO00P
016500* Map the AID in the EIB to our common area                     * SDEMO00P
016600***************************************************************** SDEMO00P
016700     IF DEMO-ENV-INET                                             SDEMO00P
016800        MOVE EXT-IP-AID TO DEMO-AID                               SDEMO00P
016900     ELSE                                                         SDEMO00P
017000        EVALUATE TRUE                                             SDEMO00P
017100          WHEN EIBAID IS EQUAL TO DFHENTER                        SDEMO00P
017200            SET DEMO-AID-ENTER TO TRUE                            SDEMO00P
017300          WHEN EIBAID IS EQUAL TO DFHCLEAR                        SDEMO00P
017400            SET DEMO-AID-CLEAR TO TRUE                            SDEMO00P
017500          WHEN EIBAID IS EQUAL TO DFHPA1                          SDEMO00P
017600            SET DEMO-AID-PA1   TO TRUE                            SDEMO00P
017700          WHEN EIBAID IS EQUAL TO DFHPA2                          SDEMO00P
017800            SET DEMO-AID-PA2   TO TRUE                            SDEMO00P
017900          WHEN EIBAID IS EQUAL TO DFHPF1                          SDEMO00P
018000            SET DEMO-AID-PFK01 TO TRUE                            SDEMO00P
018100          WHEN EIBAID IS EQUAL TO DFHPF2                          SDEMO00P
018200            SET DEMO-AID-PFK02 TO TRUE                            SDEMO00P
018300          WHEN EIBAID IS EQUAL TO DFHPF3                          SDEMO00P
018400            SET DEMO-AID-PFK03 TO TRUE                            SDEMO00P
018500          WHEN EIBAID IS EQUAL TO DFHPF4                          SDEMO00P
018600            SET DEMO-AID-PFK04 TO TRUE                            SDEMO00P
018700          WHEN EIBAID IS EQUAL TO DFHPF5                          SDEMO00P
018800            SET DEMO-AID-PFK05 TO TRUE                            SDEMO00P
018900          WHEN EIBAID IS EQUAL TO DFHPF6                          SDEMO00P
019000            SET DEMO-AID-PFK06 TO TRUE                            SDEMO00P
019100          WHEN EIBAID IS EQUAL TO DFHPF7                          SDEMO00P
019200            SET DEMO-AID-PFK07 TO TRUE                            SDEMO00P
019300          WHEN EIBAID IS EQUAL TO DFHPF8                          SDEMO00P
019400            SET DEMO-AID-PFK08 TO TRUE                            SDEMO00P
019500          WHEN EIBAID IS EQUAL TO DFHPF9                          SDEMO00P
019600            SET DEMO-AID-PFK09 TO TRUE                            SDEMO00P
019700          WHEN EIBAID IS EQUAL TO DFHPF10                         SDEMO00P
019800            SET DEMO-AID-PFK10 TO TRUE                            SDEMO00P
019900          WHEN EIBAID IS EQUAL TO DFHPF11                         SDEMO00P
020000            SET DEMO-AID-PFK11 TO TRUE                            SDEMO00P
020100          WHEN EIBAID IS EQUAL TO DFHPF12                         SDEMO00P
020200            SET DEMO-AID-PFK12 TO TRUE                            SDEMO00P
020300          WHEN EIBAID IS EQUAL TO DFHPF13                         SDEMO00P
020400            SET DEMO-AID-PFK01 TO TRUE                            SDEMO00P
020500          WHEN EIBAID IS EQUAL TO DFHPF14                         SDEMO00P
020600           SET DEMO-AID-PFK02 TO TRUE                             SDEMO00P
020700          WHEN EIBAID IS EQUAL TO DFHPF15                         SDEMO00P
020800            SET DEMO-AID-PFK03 TO TRUE                            SDEMO00P
020900          WHEN EIBAID IS EQUAL TO DFHPF16                         SDEMO00P
021000            SET DEMO-AID-PFK04 TO TRUE                            SDEMO00P
021100          WHEN EIBAID IS EQUAL TO DFHPF17                         SDEMO00P
021200            SET DEMO-AID-PFK05 TO TRUE                            SDEMO00P
021300          WHEN EIBAID IS EQUAL TO DFHPF18                         SDEMO00P
021400            SET DEMO-AID-PFK06 TO TRUE                            SDEMO00P
021500          WHEN EIBAID IS EQUAL TO DFHPF19                         SDEMO00P
021600            SET DEMO-AID-PFK07 TO TRUE                            SDEMO00P
021700          WHEN EIBAID IS EQUAL TO DFHPF20                         SDEMO00P
021800            SET DEMO-AID-PFK08 TO TRUE                            SDEMO00P
021900          WHEN EIBAID IS EQUAL TO DFHPF21                         SDEMO00P
022000            SET DEMO-AID-PFK09 TO TRUE                            SDEMO00P
022100          WHEN EIBAID IS EQUAL TO DFHPF22                         SDEMO00P
022200            SET DEMO-AID-PFK10 TO TRUE                            SDEMO00P
022300          WHEN EIBAID IS EQUAL TO DFHPF23                         SDEMO00P
022400            SET DEMO-AID-PFK11 TO TRUE                            SDEMO00P
022500          WHEN EIBAID IS EQUAL TO DFHPF24                         SDEMO00P
022600            SET DEMO-AID-PFK12 TO TRUE                            SDEMO00P
022700          WHEN OTHER                                              SDEMO00P
022800            SET DEMO-AID-ENTER TO TRUE                            SDEMO00P
022900        END-EVALUATE                                              SDEMO00P
023000     END-IF.                                                      SDEMO00P
023100                                                                  SDEMO00P
023200***************************************************************** SDEMO00P
023300* Check the AID to see if we have to toggle the colour setting  * SDEMO00P
023400***************************************************************** SDEMO00P
023500     IF DEMO-AID-PFK02                                            SDEMO00P
023600        SET DEMO-AID-ENTER TO TRUE                                SDEMO00P
023700        IF COLOUR-ON                                              SDEMO00P
023800           SET COLOUR-OFF TO TRUE                                 SDEMO00P
023900        ELSE                                                      SDEMO00P
024000           SET COLOUR-ON TO TRUE                                  SDEMO00P
024100        END-IF                                                    SDEMO00P
024200     END-IF.                                                      SDEMO00P
024300                                                                  SDEMO00P
024400***************************************************************** SDEMO00P
024500* If the DEMO-NEXT-PROG is not the same as DEMO-LAST-PROG then  * SDEMO00P
024600* we have to go to the next program                             * SDEMO00P
024700***************************************************************** SDEMO00P
024800 CHECK-PROGRAM-SWITCH.                                            SDEMO00P
024900     IF DEMO-NEXT-PROG IS NOT EQUAL TO DEMO-LAST-PROG             SDEMO00P
025000        EXEC CICS LINK PROGRAM(DEMO-NEXT-PROG)                    SDEMO00P
025100                       COMMAREA(WS-DEMO-DATA)                     SDEMO00P
025200                       LENGTH(LENGTH OF WS-DEMO-DATA)             SDEMO00P
025300        END-EXEC                                                  SDEMO00P
025400        GO TO CHECK-PROGRAM-SWITCH                                SDEMO00P
025500     END-IF.                                                      SDEMO00P
025600                                                                  SDEMO00P
025700***************************************************************** SDEMO00P
025800* We determine what the last screen displayed was and call the  * SDEMO00P
025900* the appropriate routine to handle it.                         * SDEMO00P
026000***************************************************************** SDEMO00P
026100     EVALUATE TRUE                                                SDEMO00P
026200       WHEN DEMO-LAST-MAPSET IS EQUAL TO SPACES                   SDEMO00P
026300         MOVE 'SDEMO10P' TO WS-SCREEN-LOGIC-PGM                   SDEMO00P
026400       WHEN OTHER                                                 SDEMO00P
026500         STRING 'SDEMO' DELIMITED BY SIZE                         SDEMO00P
026600                DEMO-LAST-MAPSET(6:2) DELIMITED BY SIZE           SDEMO00P
026700                'P' DELIMITED BY SIZE                             SDEMO00P
026800           INTO WS-SCREEN-LOGIC-PGM                               SDEMO00P
026900     END-EVALUATE.                                                SDEMO00P
027000     SET DEMO-MAP-FUNCTION-GET TO TRUE.                           SDEMO00P
027100     EXEC CICS LINK PROGRAM(WS-SCREEN-LOGIC-PGM)                  SDEMO00P
027200                    COMMAREA(WS-DEMO-DATA)                        SDEMO00P
027300                    LENGTH(LENGTH OF WS-DEMO-DATA)                SDEMO00P
027400     END-EXEC.                                                    SDEMO00P
027500                                                                  SDEMO00P
027600***************************************************************** SDEMO00P
027700* Now we have to see what is required from the business logic   * SDEMO00P
027800* Essentially the choices will be:                              * SDEMO00P
027900* - switch to another transaction.                              * SDEMO00P
028000*   DEMO-NEXT-TRAN is not blank and contains the next tran code * SDEMO00P
028100*   and the program is in DEMO-NEXT-PROG                        * SDEMO00P
028200* - switch to another part of this transaction                  * SDEMO00P
028300*   (which will be in DEMO-NEXT-PROG) or display the next screen* SDEMO00P
028400*    which will be in DEMO-NEXT-MAPSET/DEMO-NEXT-MAP)           * SDEMO00P
028500***************************************************************** SDEMO00P
028600* Check for a transaction switch first                            SDEMO00P
028700 CHECK-FOR-TXN-SWITCH.                                            SDEMO00P
028800     IF DEMO-NEXT-TRAN IS NOT EQUAL TO SPACES                     SDEMO00P
028900        MOVE DEMO-NEXT-TRAN TO WS-CICS-INQ-TRAN                   SDEMO00P
029000        SET TXN-SWITCH-OK TO TRUE                                 SDEMO00P
029100        EXEC CICS INQUIRE TRANSACTION(WS-CICS-INQ-TRAN)           SDEMO00P
029200                          PROGRAM(WS-CICS-INQ-PGM)                SDEMO00P
029300                          STATUS(WS-CICS-INQ-STATUS)              SDEMO00P
029400                          RESP(WS-RESP)                           SDEMO00P
029500        END-EXEC                                                  SDEMO00P
029600        IF WS-RESP IS NOT EQUAL TO DFHRESP(NORMAL) OR             SDEMO00P
029700           WS-CICS-INQ-STATUS IS NOT EQUAL TO DFHVALUE(ENABLED)   SDEMO00P
029800           SET TXN-SWITCH-ERR1 TO TRUE                            SDEMO00P
029900           MOVE SPACES TO DEMO-ERROR-MSG                          SDEMO00P
030000           STRING 'Sorry. Requested transaction '                 SDEMO00P
030100                    DELIMITED BY SIZE                             SDEMO00P
030200                  WS-CICS-INQ-TRAN DELIMITED BY SIZE              SDEMO00P
030300                  ' is not available.' DELIMITED BY SIZE          SDEMO00P
030400             INTO DEMO-ERROR-MSG                                  SDEMO00P
030500        END-IF                                                    SDEMO00P
030600        IF TXN-SWITCH-OK                                          SDEMO00P
030700           MOVE WS-CICS-INQ-PGM TO WS-CICS-LOAD-PGM               SDEMO00P
030800           EXEC CICS LOAD PROGRAM(WS-CICS-LOAD-PGM)               SDEMO00P
030900                          ENTRY(WS-CICS-LOAD-ENTRY)               SDEMO00P
031000                          RESP(WS-RESP)                           SDEMO00P
031100           END-EXEC                                               SDEMO00P
031200           IF WS-CICS-LOAD-ENTRY-R IS EQUAL TO LOW-VALUES         SDEMO00P
031300              SET TXN-SWITCH-ERR2 TO TRUE                         SDEMO00P
031400              MOVE SPACES TO DEMO-ERROR-MSG                       SDEMO00P
031500              STRING 'Sorry. Requested program '                  SDEMO00P
031600                       DELIMITED BY SIZE                          SDEMO00P
031700                     WS-CICS-INQ-PGM DELIMITED BY SIZE            SDEMO00P
031800                     ' is not available.' DELIMITED BY SIZE       SDEMO00P
031900                INTO DEMO-ERROR-MSG                               SDEMO00P
032000           END-IF                                                 SDEMO00P
032100        END-IF                                                    SDEMO00P
032200        IF TXN-SWITCH-OK                                          SDEMO00P
032300           IF DEMO-NEXT-TRAN IS EQUAL TO 'PRDA' OR                SDEMO00P
032400              DEMO-NEXT-TRAN IS EQUAL TO 'PRDB' OR                SDEMO00P
032500              DEMO-NEXT-TRAN IS EQUAL TO 'BANK' OR                SDEMO00P
032600              DEMO-NEXT-TRAN IS EQUAL TO 'INSC'                   SDEMO00P
032700              MOVE 'COLOUR=X' TO WS-PASSED-DATA                   SDEMO00P
032800              MOVE DEMO-COLOUR-SETTING TO WS-PASSED-DATA (8:1)    SDEMO00P
032900              EXEC CICS START TRANSID(DEMO-NEXT-TRAN)             SDEMO00P
033000                              TERMID(EIBTRMID)                    SDEMO00P
033100                              AFTER SECONDS(1)                    SDEMO00P
033200                              FROM(WS-PASSED-DATA)                SDEMO00P
033300                              LENGTH(LENGTH OF WS-PASSED-DATA)    SDEMO00P
033400              END-EXEC                                            SDEMO00P
033500           ELSE                                                   SDEMO00P
033600              EXEC CICS START TRANSID(DEMO-NEXT-TRAN)             SDEMO00P
033700                              TERMID(EIBTRMID)                    SDEMO00P
033800                              AFTER SECONDS(2)                    SDEMO00P
033900              END-EXEC                                            SDEMO00P
034000           END-IF                                                 SDEMO00P
034100           EXEC CICS RETURN                                       SDEMO00P
034200           END-EXEC                                               SDEMO00P
034300           GOBACK                                                 SDEMO00P
034400        ELSE                                                      SDEMO00P
034500          MOVE DEMO-LAST-PROG TO DEMO-NEXT-PROG                   SDEMO00P
034600        END-IF                                                    SDEMO00P
034700     END-IF.                                                      SDEMO00P
034800* Next check for a program switch                                 SDEMO00P
034900 CHECK-FOR-PGM-SWITCH.                                            SDEMO00P
035000     IF DEMO-NEXT-PROG IS NOT EQUAL TO DEMO-LAST-PROG             SDEMO00P
035100        EXEC CICS LINK PROGRAM(DEMO-NEXT-PROG)                    SDEMO00P
035200                       COMMAREA(WS-DEMO-DATA)                     SDEMO00P
035300                       LENGTH(LENGTH OF WS-DEMO-DATA)             SDEMO00P
035400        END-EXEC                                                  SDEMO00P
035500        GO TO CHECK-FOR-PGM-SWITCH                                SDEMO00P
035600     END-IF.                                                      SDEMO00P
035700                                                                  SDEMO00P
035800***************************************************************** SDEMO00P
035900* We determine which screen we have to display and call the     * SDEMO00P
036000* appropriate routine to handle it.                             * SDEMO00P
036100***************************************************************** SDEMO00P
036200*    MOVE LOW-VALUE TO MAPAREA.                                   SDEMO00P
036300     STRING 'SDEMO' DELIMITED BY SIZE                             SDEMO00P
036400             DEMO-NEXT-MAPSET(6:2) DELIMITED BY SIZE              SDEMO00P
036500            'P' DELIMITED BY SIZE                                 SDEMO00P
036600        INTO WS-SCREEN-LOGIC-PGM.                                 SDEMO00P
036700     SET DEMO-MAP-FUNCTION-PUT TO TRUE.                           SDEMO00P
036800     EXEC CICS LINK PROGRAM(WS-SCREEN-LOGIC-PGM)                  SDEMO00P
036900                    COMMAREA(WS-DEMO-DATA)                        SDEMO00P
037000                    LENGTH(LENGTH OF WS-DEMO-DATA)                SDEMO00P
037100     END-EXEC.                                                    SDEMO00P
037200                                                                  SDEMO00P
037300***************************************************************** SDEMO00P
037400* Now we have to have finished and can return to our invoker.   * SDEMO00P
037500* Before retuning, we write out any data we wish to preserve    * SDEMO00P
037600* to TS. So we can retrieve this data we keep the TS queue id   * SDEMO00P
037700***************************************************************** SDEMO00P
037800* Now return to CICS                                              SDEMO00P
037900     MOVE WS-DEMO-DATA TO TS-QUEUE-DATA.                          SDEMO00P
038000     MOVE LENGTH OF TS-QUEUE-DATA TO TS-QUEUE-LEN.                SDEMO00P
038100     MOVE 1 TO TS-QUEUE-ITEM.                                     SDEMO00P
038200     MOVE 0 TO WS-RESP.                                           SDEMO00P
038300     EXEC CICS WRITEQ TS                                          SDEMO00P
038400               QUEUE(TS-QUEUE-NAME)                               SDEMO00P
038500               FROM(TS-QUEUE-DATA)                                SDEMO00P
038600               LENGTH(TS-QUEUE-LEN)                               SDEMO00P
038700               ITEM(TS-QUEUE-ITEM)                                SDEMO00P
038800               REWRITE                                            SDEMO00P
038900               RESP(WS-RESP)                                      SDEMO00P
039000     END-EXEC.                                                    SDEMO00P
039100                                                                  SDEMO00P
039200     IF DEMO-CICS-TRANCODE IS EQUAL TO SPACES OR                  SDEMO00P
039300        DEMO-ENV-INET                                             SDEMO00P
039400        EXEC CICS RETURN                                          SDEMO00P
039500        END-EXEC                                                  SDEMO00P
039600     ELSE                                                         SDEMO00P
039700        EXEC CICS RETURN                                          SDEMO00P
039800                  TRANSID(DEMO-CICS-TRANCODE)                     SDEMO00P
039900                  COMMAREA(TS-QUEUE-NAME)                         SDEMO00P
040000                  LENGTH(LENGTH OF TS-QUEUE-NAME)                 SDEMO00P
040100        END-EXEC                                                  SDEMO00P
040200     END-IF.                                                      SDEMO00P
040300     GOBACK.                                                      SDEMO00P
040400                                                                  SDEMO00P
