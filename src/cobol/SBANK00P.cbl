000100***************************************************************** sbank00p
000200*                                                               * sbank00p
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * sbank00p
000400*   This demonstration program is provided for use by users     * sbank00p
000500*   of Micro Focus products and may be used, modified and       * sbank00p
000600*   distributed as part of your application provided that       * sbank00p
000700*   you properly acknowledge the copyright of Micro Focus       * sbank00p
000800*   in this material.                                           * sbank00p
000900*                                                               * sbank00p
001000***************************************************************** sbank00p
001100                                                                  sbank00p
001200***************************************************************** sbank00p
001300* Program:     SBANK00P.CBL (CICS Version)                      * sbank00p
001400* Layer:       Screen handling                                  * sbank00p
001500* Function:    Screen handling control module                   * sbank00p
001600***************************************************************** sbank00p
001700                                                                  sbank00p
001800 IDENTIFICATION DIVISION.                                         sbank00p
001900 PROGRAM-ID.                                                      sbank00p
002000     SBANK00P.                                                    sbank00p
002100 DATE-WRITTEN.                                                    sbank00p
002200     September 2002.                                              sbank00p
002300 DATE-COMPILED.                                                   sbank00p
002400     Today.                                                       sbank00p
002500                                                                  sbank00p
002600 ENVIRONMENT DIVISION.                                            sbank00p
002700                                                                  sbank00p
002800 DATA DIVISION.                                                   sbank00p
002900 WORKING-STORAGE SECTION.                                         sbank00p
003000 01  WS-MISC-STORAGE.                                             sbank00p
003100   05  WS-PROGRAM-ID                         PIC X(8)             sbank00p
003200       VALUE 'SBANK00P'.                                          sbank00p
003300   05  WS-TRAN-ID                            PIC X(4).            sbank00p
003400   05  WS-SCREEN-LOGIC-PGM                   PIC X(8)             sbank00p
003500       VALUE SPACES.                                              sbank00p
003600   05  WS-DYNAMIC-PGM                        PIC X(8)             sbank00p
003700       VALUE 'UNKNOWN'.                                           sbank00p
003800   05  WS-ABSTIME                            PIC S9(15) COMP-3.   sbank00p
003900   05  WS-RESP                               PIC S9(8) COMP.      sbank00p
004000   05  WS-INPUT-SOURCE-MSG.                                       sbank00p
004100     10  FILLER                              PIC X(20)            sbank00p
004200         VALUE 'Input received from '.                            sbank00p
004300     10  WS-INPUT-SOURCE-MSG-CALL-TYPE       PIC X(8).            sbank00p
004400 01  WS-BANK-DATA-AREAS.                                          sbank00p
004500   05  WS-BANK-DATA.                                              sbank00p
004600 COPY CBANKDAT.                                                   sbank00p
004700   05  WS-BANK-EXT-DATA.                                          sbank00p
004800 COPY CBANKEXT.                                                   sbank00p
004900                                                                  sbank00p
005000 01  TS-DATA.                                                     sbank00p
005100   05  TS-QUEUE-NAME                         PIC X(8).            sbank00p
005200   05  TS-QUEUE-NAME-PARTS REDEFINES TS-QUEUE-NAME.               sbank00p
005300     10  TS-QUEUE-NAME-PART1                 PIC X(4).            sbank00p
005400     10  TS-QUEUE-NAME-PART2                 PIC 9(4).            sbank00p
005500   05  TS-QUEUE-LEN                          PIC S9(4) COMP.      sbank00p
005600   05  TS-QUEUE-ITEM                         PIC S9(4) COMP.      sbank00p
005700   05  TS-QUEUE-DATA                         PIC X(6144).         sbank00p
005800                                                                  sbank00p
005900 COPY DFHAID.                                                     sbank00p
006000                                                                  sbank00p
006100 COPY DFHBMSCA.                                                   sbank00p
006200                                                                  sbank00p
006300 COPY CABENDD.                                                    sbank00p
006400                                                                  sbank00p
006500 01  load-ptr pointer.                                            sbank00p
006600                                                                  sbank00p
006700                                                                  sbank00p
006800 LINKAGE SECTION.                                                 sbank00p
006900 01  DFHCOMMAREA.                                                 sbank00p
007000   05  LK-TS-QUEUE-NAME                      PIC X(8).            sbank00p
007100   05  LK-CALL-TYPE                          PIC X(8).            sbank00p
007200     88  CALL-TYPE-CICSECI                   VALUE 'CICSECI'.     sbank00p
007300     88  CALL-TYPE-WEBSERV                   VALUE 'WEBSERV'.     sbank00p
007400   05  LK-PASSED-DATA                        PIC X(1024).         sbank00p
007500*COPY CBANKEXT.                                                   sbank00p
007600                                                                  sbank00p
007700 PROCEDURE DIVISION.                                              sbank00p
007800***************************************************************** sbank00p
007900* Write entry to log to show we have been invoked               * sbank00p
008000***************************************************************** sbank00p
008100     COPY CTRACE.                                                 sbank00p
008200                                                                  sbank00p
008300***************************************************************** sbank00p
008400* Store our transaction-id                                      * sbank00p
008500***************************************************************** sbank00p
008600     MOVE EIBTRNID TO WS-TRAN-ID.                                 sbank00p
008700                                                                  sbank00p
008800***************************************************************** sbank00p
008900* If we have a commarea then its either not the first time in   * sbank00p
009000* from a terminal or we have come from other than a terminal so * sbank00p
009100* display the call type so we know where we came from           * sbank00p
009200***************************************************************** sbank00p
009300     IF EIBCALEN IS NOT LESS THAN 8                               sbank00p
009400        IF CALL-TYPE-CICSECI OR                                   sbank00p
009500           CALL-TYPE-WEBSERV                                      sbank00p
009600           MOVE LK-CALL-TYPE TO WS-INPUT-SOURCE-MSG-CALL-TYPE     sbank00p
009700*          EXEC CICS WRITE OPERATOR                               sbank00p
009800*                    TEXT(WS-INPUT-SOURCE-MSG)                    sbank00p
009900*                    TEXTLENGTH(LENGTH OF WS-INPUT-SOURCE-MSG)    sbank00p
010000*          END-EXEC                                               sbank00p
010100         END-IF                                                   sbank00p
010200      END-IF.                                                     sbank00p
010300                                                                  sbank00p
010400***************************************************************** sbank00p
010500* If this is the first time in, then we assume we are running   * sbank00p
010600* from a CICS terminal so we display map BANK10M and return with* sbank00p
010700* with our COMMAREA set up.                                     * sbank00p
010800***************************************************************** sbank00p
010900     IF (EIBCALEN IS EQUAL TO 0) OR                               sbank00p
011000        (EIBCALEN IS NOT EQUAL TO 0 AND                           sbank00p
011100         LK-TS-QUEUE-NAME IS EQUAL TO 'INET****')                 sbank00p
011200        MOVE LOW-VALUES TO WS-BANK-DATA-AREAS                     sbank00p
011300        IF EIBCALEN IS EQUAL TO 0                                 sbank00p
011400           SET BANK-ENV-CICS TO TRUE                              sbank00p
011500           EXEC CICS RETRIEVE                                     sbank00p
011600                     INTO(TS-DATA)                                sbank00p
011700                     LENGTH(LENGTH OF TS-DATA)                    sbank00p
011800                     RESP(WS-RESP)                                sbank00p
011900           END-EXEC                                               sbank00p
012000           IF TS-DATA(1:7) IS EQUAL TO 'COLOUR='                  sbank00p
012100              MOVE TS-DATA(8:1) TO BANK-COLOUR-SETTING            sbank00p
012200           END-IF                                                 sbank00p
012300        ELSE                                                      sbank00p
012400           SET BANK-ENV-INET TO TRUE                              sbank00p
012500        END-IF                                                    sbank00p
012600        SET BANK-NO-CONV-IN-PROGRESS TO TRUE                      sbank00p
012700        MOVE SPACES TO BANK-LAST-MAPSET                           sbank00p
012800        MOVE SPACES TO BANK-LAST-MAP                              sbank00p
012900        MOVE SPACES TO BANK-LAST-PROG                             sbank00p
013000        MOVE SPACES TO BANK-NEXT-PROG                             sbank00p
013100        MOVE WS-TRAN-ID TO BANK-CICS-TRANCODE                     sbank00p
013200        EXEC CICS ASKTIME                                         sbank00p
013300                  ABSTIME(WS-ABSTIME)                             sbank00p
013400        END-EXEC                                                  sbank00p
013500        MOVE BANK-ENV TO TS-QUEUE-NAME-PART1                      sbank00p
013600*       MOVE WS-ABSTIME TO TS-QUEUE-NAME-PART2                    sbank00p
013601        MOVE EIBTASKN   TO TS-QUEUE-NAME-PART2                    StuC
013700        EXEC CICS DELETEQ TS                                      sbank00p
013800                  QUEUE(TS-QUEUE-NAME)                            sbank00p
013900                  RESP(WS-RESP)                                   sbank00p
014000        END-EXEC                                                  sbank00p
014100        MOVE SPACES TO TS-QUEUE-DATA                              sbank00p
014200        MOVE LENGTH OF TS-QUEUE-DATA TO TS-QUEUE-LEN              sbank00p
014300        MOVE 0 TO TS-QUEUE-ITEM                                   sbank00p
014400        MOVE 0 TO WS-RESP                                         sbank00p
014500        EXEC CICS WRITEQ TS                                       sbank00p
014600                  QUEUE(TS-QUEUE-NAME)                            sbank00p
014700                  FROM(TS-QUEUE-DATA)                             sbank00p
014800                  LENGTH(TS-QUEUE-LEN)                            sbank00p
014900                  ITEM(TS-QUEUE-ITEM)                             sbank00p
015000                  RESP(WS-RESP)                                   sbank00p
015100        END-EXEC                                                  sbank00p
015200        exec cics write operator                                  sbank00p
015300                        text(ts-queue-name)                       sbank00p
015400                        textlength(length of ts-queue-name)       sbank00p
015500        end-exec                                                  sbank00p
015600                                                                  sbank00p
015700        IF BANK-ENV-INET                                          sbank00p
015800           MOVE TS-QUEUE-NAME TO LK-TS-QUEUE-NAME                 sbank00p
015900        END-IF                                                    sbank00p
016000     ELSE                                                         sbank00p
016100        MOVE LOW-VALUES TO WS-BANK-DATA                           sbank00p
016200        MOVE LK-TS-QUEUE-NAME TO TS-QUEUE-NAME                    sbank00p
016300        MOVE LENGTH OF TS-QUEUE-DATA TO TS-QUEUE-LEN              sbank00p
016400        MOVE 1 TO TS-QUEUE-ITEM                                   sbank00p
016500        EXEC CICS READQ TS                                        sbank00p
016600                  QUEUE(TS-QUEUE-NAME)                            sbank00p
016700                  INTO(TS-QUEUE-DATA)                             sbank00p
016800                  LENGTH(TS-QUEUE-LEN)                            sbank00p
016900                  ITEM(TS-QUEUE-ITEM)                             sbank00p
017000        END-EXEC                                                  sbank00p
017100        MOVE TS-QUEUE-DATA TO WS-BANK-DATA                        sbank00p
017200        IF BANK-ENV-INET                                          sbank00p
017300           MOVE LK-PASSED-DATA (1:EIBCALEN) TO WS-BANK-EXT-DATA   sbank00p
017400           IF CALL-TYPE-WEBSERV                                   sbank00p
017500              INSPECT WS-BANK-EXT-DATA REPLACING ALL '~' BY       sbank00p
017600                LOW-VALUES                                        sbank00p
017700           END-IF                                                 sbank00p
017800        END-IF                                                    sbank00p
017900     END-IF.                                                      sbank00p
018000                                                                  sbank00p
018100***************************************************************** sbank00p
018200* If we get this far then this is not the first time in as we   * sbank00p
018300* have a COMMAREA. Check that BANK-ENV is set correctly to      * sbank00p
018400* ensure we are running in the correct environment etc          * sbank00p
018500***************************************************************** sbank00p
018600     IF NOT BANK-ENV-CICS AND                                     sbank00p
018700        NOT BANK-ENV-INET                                         sbank00p
018800        MOVE WS-PROGRAM-ID TO ABEND-CULPRIT                       sbank00p
018900        MOVE 'S001' TO ABEND-CODE                                 sbank00p
019000        MOVE 'Inavlid environment' TO ABEND-REASON                sbank00p
019100        COPY CABENDPO.                                            sbank00p
019200     END-IF.                                                      sbank00p
019300                                                                  sbank00p
019400***************************************************************** sbank00p
019500* This is the main process                                      * sbank00p
019600***************************************************************** sbank00p
019700                                                                  sbank00p
019800***************************************************************** sbank00p
019900* Map the AID in the EIB to our common area                     * sbank00p
020000***************************************************************** sbank00p
020100     IF BANK-ENV-INET                                             sbank00p
020200        MOVE EXT-IP-AID TO BANK-AID                               sbank00p
020300     ELSE                                                         sbank00p
020400        EVALUATE TRUE                                             sbank00p
020500          WHEN EIBAID IS EQUAL TO DFHENTER                        sbank00p
020600            SET BANK-AID-ENTER TO TRUE                            sbank00p
020700          WHEN EIBAID IS EQUAL TO DFHCLEAR                        sbank00p
020800            SET BANK-AID-CLEAR TO TRUE                            sbank00p
020900          WHEN EIBAID IS EQUAL TO DFHPA1                          sbank00p
021000            SET BANK-AID-PA1   TO TRUE                            sbank00p
021100          WHEN EIBAID IS EQUAL TO DFHPA2                          sbank00p
021200            SET BANK-AID-PA2   TO TRUE                            sbank00p
021300          WHEN EIBAID IS EQUAL TO DFHPF1                          sbank00p
021400            SET BANK-AID-PFK01 TO TRUE                            sbank00p
021500          WHEN EIBAID IS EQUAL TO DFHPF2                          sbank00p
021600            SET BANK-AID-PFK02 TO TRUE                            sbank00p
021700          WHEN EIBAID IS EQUAL TO DFHPF3                          sbank00p
021800            SET BANK-AID-PFK03 TO TRUE                            sbank00p
021900          WHEN EIBAID IS EQUAL TO DFHPF4                          sbank00p
022000            SET BANK-AID-PFK04 TO TRUE                            sbank00p
022100          WHEN EIBAID IS EQUAL TO DFHPF5                          sbank00p
022200            SET BANK-AID-PFK05 TO TRUE                            sbank00p
022300          WHEN EIBAID IS EQUAL TO DFHPF6                          sbank00p
022400            SET BANK-AID-PFK06 TO TRUE                            sbank00p
022500          WHEN EIBAID IS EQUAL TO DFHPF7                          sbank00p
022600            SET BANK-AID-PFK07 TO TRUE                            sbank00p
022700          WHEN EIBAID IS EQUAL TO DFHPF8                          sbank00p
022800            SET BANK-AID-PFK08 TO TRUE                            sbank00p
022900          WHEN EIBAID IS EQUAL TO DFHPF9                          sbank00p
023000            SET BANK-AID-PFK09 TO TRUE                            sbank00p
023100          WHEN EIBAID IS EQUAL TO DFHPF10                         sbank00p
023200            SET BANK-AID-PFK10 TO TRUE                            sbank00p
023300          WHEN EIBAID IS EQUAL TO DFHPF11                         sbank00p
023400            SET BANK-AID-PFK11 TO TRUE                            sbank00p
023500          WHEN EIBAID IS EQUAL TO DFHPF12                         sbank00p
023600            SET BANK-AID-PFK12 TO TRUE                            sbank00p
023700          WHEN EIBAID IS EQUAL TO DFHPF13                         sbank00p
023800            SET BANK-AID-PFK01 TO TRUE                            sbank00p
023900          WHEN EIBAID IS EQUAL TO DFHPF14                         sbank00p
024000           SET BANK-AID-PFK02 TO TRUE                             sbank00p
024100          WHEN EIBAID IS EQUAL TO DFHPF15                         sbank00p
024200            SET BANK-AID-PFK03 TO TRUE                            sbank00p
024300          WHEN EIBAID IS EQUAL TO DFHPF16                         sbank00p
024400            SET BANK-AID-PFK04 TO TRUE                            sbank00p
024500          WHEN EIBAID IS EQUAL TO DFHPF17                         sbank00p
024600            SET BANK-AID-PFK05 TO TRUE                            sbank00p
024700          WHEN EIBAID IS EQUAL TO DFHPF18                         sbank00p
024800            SET BANK-AID-PFK06 TO TRUE                            sbank00p
024900          WHEN EIBAID IS EQUAL TO DFHPF19                         sbank00p
025000            SET BANK-AID-PFK07 TO TRUE                            sbank00p
025100          WHEN EIBAID IS EQUAL TO DFHPF20                         sbank00p
025200            SET BANK-AID-PFK08 TO TRUE                            sbank00p
025300          WHEN EIBAID IS EQUAL TO DFHPF21                         sbank00p
025400            SET BANK-AID-PFK09 TO TRUE                            sbank00p
025500          WHEN EIBAID IS EQUAL TO DFHPF22                         sbank00p
025600            SET BANK-AID-PFK10 TO TRUE                            sbank00p
025700          WHEN EIBAID IS EQUAL TO DFHPF23                         sbank00p
025800            SET BANK-AID-PFK11 TO TRUE                            sbank00p
025900          WHEN EIBAID IS EQUAL TO DFHPF24                         sbank00p
026000            SET BANK-AID-PFK12 TO TRUE                            sbank00p
026100          WHEN OTHER                                              sbank00p
026200            SET BANK-AID-ENTER TO TRUE                            sbank00p
026300        END-EVALUATE                                              sbank00p
026400     END-IF.                                                      sbank00p
026500                                                                  sbank00p
026600***************************************************************** sbank00p
026700* Check the AID to see if we have to toggle the colour setting  * sbank00p
026800***************************************************************** sbank00p
026900     IF BANK-AID-PFK02                                            sbank00p
027000        SET BANK-AID-ENTER TO TRUE                                sbank00p
027100        IF COLOUR-ON                                              sbank00p
027200           SET COLOUR-OFF TO TRUE                                 sbank00p
027300        ELSE                                                      sbank00p
027400           SET COLOUR-ON TO TRUE                                  sbank00p
027500        END-IF                                                    sbank00p
027600     END-IF.                                                      sbank00p
027700                                                                  sbank00p
027800***************************************************************** sbank00p
027900* If the BANK-NEXT-PROG is not the same as BANK-LAST-PROG then  * sbank00p
028000* we have to go to the next program                             * sbank00p
028100***************************************************************** sbank00p
028200 CHECK-PROGRAM-SWITCH.                                            sbank00p
028300     IF BANK-NEXT-PROG IS NOT EQUAL TO BANK-LAST-PROG             sbank00p
028400        EXEC CICS LINK PROGRAM(BANK-NEXT-PROG)                    sbank00p
028500                       COMMAREA(WS-BANK-DATA-AREAS)               sbank00p
028600                       LENGTH(LENGTH OF WS-BANK-DATA-AREAS)       sbank00p
028700        END-EXEC                                                  sbank00p
028800        GO TO CHECK-PROGRAM-SWITCH                                sbank00p
028900     END-IF.                                                      sbank00p
029000                                                                  sbank00p
029100***************************************************************** sbank00p
029200* We determine what the last screen displayed was and call the  * sbank00p
029300* the appropriate routine to handle it.                         * sbank00p
029400***************************************************************** sbank00p
029500     EVALUATE TRUE                                                sbank00p
029600       WHEN BANK-LAST-MAPSET IS EQUAL TO SPACES                   sbank00p
029700         MOVE 'SBANK10P' TO WS-SCREEN-LOGIC-PGM                   sbank00p
029800       WHEN OTHER                                                 sbank00p
029900         STRING 'SBANK' DELIMITED BY SIZE                         sbank00p
030000                BANK-LAST-MAPSET(6:2) DELIMITED BY SIZE           sbank00p
030100                'P' DELIMITED BY SIZE                             sbank00p
030200           INTO WS-SCREEN-LOGIC-PGM                               sbank00p
030300     END-EVALUATE.                                                sbank00p
030400     SET BANK-MAP-FUNCTION-GET TO TRUE.                           sbank00p
030500     EXEC CICS LINK PROGRAM(WS-SCREEN-LOGIC-PGM)                  sbank00p
030600                    COMMAREA(WS-BANK-DATA-AREAS)                  sbank00p
030700                    LENGTH(LENGTH OF WS-BANK-DATA-AREAS)          sbank00p
030800     END-EXEC.                                                    sbank00p
030900                                                                  sbank00p
031000***************************************************************** sbank00p
031100* Now we have to see what is required from the business logic   * sbank00p
031200* Essentially the choices will be switch to another program     * sbank00p
031300* (which will be in BANK-NEXT-PROG) or display thge next screen * sbank00p
031400* (which will be in BANK-NEXT-MAPSET/BANK-NEXT-MAP)             * sbank00p
031500***************************************************************** sbank00p
031600* Check for a program switch first                                sbank00p
031700 CHECK-FOR-PGM-SWITCH.                                            sbank00p
031800     IF BANK-NEXT-PROG IS NOT EQUAL TO BANK-LAST-PROG             sbank00p
031900        EXEC CICS LINK PROGRAM(BANK-NEXT-PROG)                    sbank00p
032000                       COMMAREA(WS-BANK-DATA-AREAS)               sbank00p
032100                       LENGTH(LENGTH OF WS-BANK-DATA-AREAS)       sbank00p
032200        END-EXEC                                                  sbank00p
032300        GO TO CHECK-FOR-PGM-SWITCH                                sbank00p
032400     END-IF.                                                      sbank00p
032500                                                                  sbank00p
032600***************************************************************** sbank00p
032700* We determine which screen we have to display and call the     * sbank00p
032800* appropriate routine to handle it.                             * sbank00p
032900***************************************************************** sbank00p
033000*    MOVE LOW-VALUE TO MAPAREA.                                   sbank00p
033100     STRING 'SBANK' DELIMITED BY SIZE                             sbank00p
033200             BANK-NEXT-MAPSET(6:2) DELIMITED BY SIZE              sbank00p
033300            'P' DELIMITED BY SIZE                                 sbank00p
033400        INTO WS-SCREEN-LOGIC-PGM.                                 sbank00p
033500     SET BANK-MAP-FUNCTION-PUT TO TRUE.                           sbank00p
033600     EXEC CICS LINK PROGRAM(WS-SCREEN-LOGIC-PGM)                  sbank00p
033700                    COMMAREA(WS-BANK-DATA-AREAS)                  sbank00p
033800                    LENGTH(LENGTH OF WS-BANK-DATA-AREAS)          sbank00p
033900     END-EXEC.                                                    sbank00p
034000                                                                  sbank00p
034100***************************************************************** sbank00p
034200* Now we have to have finished and can return to our invoker.   * sbank00p
034300* Before retuning, we write out any data we wish to preserve    * sbank00p
034400* to TS. So we can retrieve this data we keep the TS queue id   * sbank00p
034500***************************************************************** sbank00p
034600* Now return to CICS                                              sbank00p
034700     MOVE WS-BANK-DATA TO TS-QUEUE-DATA.                          sbank00p
034800     MOVE LENGTH OF TS-QUEUE-DATA TO TS-QUEUE-LEN.                sbank00p
034900     MOVE 1 TO TS-QUEUE-ITEM.                                     sbank00p
035000     MOVE 0 TO WS-RESP.                                           sbank00p
035100     EXEC CICS WRITEQ TS                                          sbank00p
035200               QUEUE(TS-QUEUE-NAME)                               sbank00p
035300               FROM(TS-QUEUE-DATA)                                sbank00p
035400               LENGTH(TS-QUEUE-LEN)                               sbank00p
035500               ITEM(TS-QUEUE-ITEM)                                sbank00p
035600               REWRITE                                            sbank00p
035700               RESP(WS-RESP)                                      sbank00p
035800     END-EXEC.                                                    sbank00p
035900                                                                  sbank00p
036000     IF BANK-ENV-INET                                             sbank00p
036100        IF CALL-TYPE-WEBSERV                                      sbank00p
036200           INSPECT WS-BANK-EXT-DATA REPLACING ALL LOW-VALUES BY   sbank00p
036300             '~'                                                  sbank00p
036400        END-IF                                                    sbank00p
036500        MOVE WS-BANK-EXT-DATA TO LK-PASSED-DATA                   sbank00p
036600     END-IF.                                                      sbank00p
036700                                                                  sbank00p
036800     IF BANK-CICS-TRANCODE IS EQUAL TO SPACES OR                  sbank00p
036900        BANK-ENV-INET                                             sbank00p
037000        EXEC CICS RETURN                                          sbank00p
037100        END-EXEC                                                  sbank00p
037200     ELSE                                                         sbank00p
037300        EXEC CICS RETURN                                          sbank00p
037400                  TRANSID(BANK-CICS-TRANCODE)                     sbank00p
037500                  COMMAREA(TS-QUEUE-NAME)                         sbank00p
037600                  LENGTH(LENGTH OF TS-QUEUE-NAME)                 sbank00p
037700        END-EXEC                                                  sbank00p
037800     END-IF.                                                      sbank00p
037900     GOBACK.                                                      sbank00p
038000                                                                  sbank00p
038100* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     sbank00p
