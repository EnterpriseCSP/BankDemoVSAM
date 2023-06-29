000100***************************************************************** ZBNKE35
000200*                                                               * ZBNKE35
000300*   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * ZBNKE35
000400*   This demonstration program is provided for use by users     * ZBNKE35
000500*   of Micro Focus products and may be used, modified and       * ZBNKE35
000600*   distributed as part of your application provided that       * ZBNKE35
000700*   you properly acknowledge the copyright of Micro Focus       * ZBNKE35
000800*   in this material.                                           * ZBNKE35
000900*                                                               * ZBNKE35
001000***************************************************************** ZBNKE35
001100                                                                  ZBNKE35
001200***************************************************************** ZBNKE35
001300* Program:     ZBNKE35.CBL                                      * ZBNKE35
001400* Function:    Demonstrate E35 capability.                      * ZBNKE35
001500***************************************************************** ZBNKE35
001600*MFJSORT provides a standard linkage area to the E15 and E35    * ZBNKE35
001700*exit program. It is compatible with the mainframe utility      * ZBNKE35
001800*and should be defined as follows:                              * ZBNKE35
001900*                                                               * ZBNKE35
002000*COBOL statement      PIC        Value                          * ZBNKE35
002100* RECORD-FLAGS        9(8)COMP   0 - first record passed        * ZBNKE35
002200*                                4 - subsequent records passed  * ZBNKE35
002300*                                8 - last record passed         * ZBNKE35
002400* ENTRY-BUFFER        X(n)       Contents of the input record.  * ZBNKE35
002500*                                Do not change this area.       * ZBNKE35
002600* EXIT-BUFFER         X(n)       Contents of the new or altered * ZBNKE35
002700*                                  record provided by the exit. * ZBNKE35
002800* UNUSED-ENTRY        9(8)COMP   Not used                       * ZBNKE35
002900*                                                               * ZBNKE35
003000* UNUSED-ENTRY        9(8)COMP   Not used                       * ZBNKE35
003100*                                                               * ZBNKE35
003200* ENTRY-RECORD-LENGTH 9(8)COMP   Length of the input record.    * ZBNKE35
003300* EXIT-RECORD-LENGTH  9(8)COMP   Length of the new or altered   * ZBNKE35
003400*                                  record provided by the exit. * ZBNKE35
003500* UNUSED-ENTRY        9(8)COMP   Not used                       * ZBNKE35
003600* EXIT-AREA-LENGTH    9(4)COMP   Length of the exit area        * ZBNKE35
003700*                                  scratchpad.                  * ZBNKE35
003800*                                Do not change this field.      * ZBNKE35
003900* EXIT-AREA           X(n)       Exit area scratchpad used by   * ZBNKE35
004000*                                  the exit to maintain         * ZBNKE35
004100*                                  variables between calls to   * ZBNKE35
004200*                                  the exit program.            * ZBNKE35
004300*                                                               * ZBNKE35
004400* Return Code         Meaning                                   * ZBNKE35
004500*   0                 No action required                        * ZBNKE35
004600*   4                 Delete the current record.                * ZBNKE35
004700*                       For E15, the record is not sorted.      * ZBNKE35
004800*                       For E35, the record is not written to   * ZBNKE35
004900*                         the output dataset                    * ZBNKE35
005000*   8                 Do not call this exit again;              * ZBNKE35
005100*                       exit processing is no longer required   * ZBNKE35
005200*   12                Insert the current record.                * ZBNKE35
005300*                       For E15, the record is inserted for     * ZBNKE35
005400*                         sorting.                              * ZBNKE35
005500*                       For E35, the record is written to the   * ZBNKE35
005600*                         output dataset                        * ZBNKE35
005700*   16                Terminate. The job step is terminated     * ZBNKE35
005800*                       with the condition code set to 16       * ZBNKE35
005900*   20                Alter the current record.                 * ZBNKE35
006000*                       For E15, the altered record is passed   * ZBNKE35
006100*                         to the sort.                          * ZBNKE35
006200*                       For E35, the altered record is written  * ZBNKE35
006300*                         to the output dataset                 * ZBNKE35
006400***************************************************************** ZBNKE35
006500                                                                  ZBNKE35
006600 IDENTIFICATION DIVISION.                                         ZBNKE35
006700 PROGRAM-ID.                                                      ZBNKE35
006800     ZBNKE35.                                                     ZBNKE35
006900 DATE-WRITTEN.                                                    ZBNKE35
007000     September 2002.                                              ZBNKE35
007100 DATE-COMPILED.                                                   ZBNKE35
007200     Today.                                                       ZBNKE35
007300 ENVIRONMENT DIVISION.                                            ZBNKE35
007400                                                                  ZBNKE35
007500 DATA DIVISION.                                                   ZBNKE35
007600                                                                  ZBNKE35
007700 WORKING-STORAGE SECTION.                                         ZBNKE35
007800 01  WS-MISC-STORAGE.                                             ZBNKE35
007900   05  WS-PROGRAM-ID                         PIC X(8)             ZBNKE35
008000       VALUE 'ZBNKE35 '.                                          ZBNKE35
008100   05  WS-REC-COUNT-READ                     PIC 9(5).            ZBNKE35
008200   05  WS-REC-COUNT-WRITTEN                  PIC 9(5).            ZBNKE35
008300   05  WS-REC-COUNT-DELETED                  PIC 9(5).            ZBNKE35
008400   05  WS-SAVED-PID                          PIC X(5).            ZBNKE35
008500                                                                  ZBNKE35
008600 01  WS-CONSOLE-MESSAGE                      PIC X(48).           ZBNKE35
008700                                                                  ZBNKE35
008800 COPY CABENDD.                                                    ZBNKE35
008900                                                                  ZBNKE35
009000 LINKAGE SECTION.                                                 ZBNKE35
009100 01  LK-EXIT-RECORD-FLAGS                    PIC 9(8) COMP.       ZBNKE35
009200   88  FIRST-RECORD                          VALUE 0.             ZBNKE35
009300   88  SUBSEQUENT-RECORD                     VALUE 4.             ZBNKE35
009400   88  LAST-RECORD                           VALUE 8.             ZBNKE35
009500                                                                  ZBNKE35
009600 01  LK-EXIT-ENTRY-BUFFER                    PIC X(116).          ZBNKE35
009700                                                                  ZBNKE35
009800 01  LK-EXIT-EXIT-BUFFER                     PIC X(116).          ZBNKE35
009900                                                                  ZBNKE35
010000 01  LK-EXIT-UNUSED-1                        PIC 9(8) COMP.       ZBNKE35
010100                                                                  ZBNKE35
010200 01  LK-EXIT-UNUSED-2                        PIC 9(8) COMP.       ZBNKE35
010300                                                                  ZBNKE35
010400 01  LK-EXIT-ENTRY-REC-LEN                   PIC 9(8) COMP.       ZBNKE35
010500                                                                  ZBNKE35
010600 01  LK-EXIT-EXIT-REC-LEN                    PIC 9(8) COMP.       ZBNKE35
010700                                                                  ZBNKE35
010800 01  LK-EXIT-SPA-LEN                         PIC 9(4) COMP.       ZBNKE35
010900                                                                  ZBNKE35
011000 01  LK-EXIT-SPA.                                                 ZBNKE35
011100   05  LK-EXIT-SPA-BYTE                      PIC X(1)             ZBNKE35
011200       OCCURS 1 TO 9999 TIMES DEPENDING ON LK-EXIT-SPA-LEN.       ZBNKE35
011300                                                                  ZBNKE35
011400 PROCEDURE DIVISION USING LK-EXIT-RECORD-FLAGS                    ZBNKE35
011500                          LK-EXIT-ENTRY-BUFFER                    ZBNKE35
011600                          LK-EXIT-EXIT-BUFFER                     ZBNKE35
011700                          LK-EXIT-UNUSED-1                        ZBNKE35
011800                          LK-EXIT-UNUSED-2                        ZBNKE35
011900                          LK-EXIT-ENTRY-REC-LEN                   ZBNKE35
012000                          LK-EXIT-EXIT-REC-LEN                    ZBNKE35
012100                          LK-EXIT-SPA-LEN                         ZBNKE35
012200                          LK-EXIT-SPA.                            ZBNKE35
012300                                                                  ZBNKE35
012400                                                                  ZBNKE35
012500     IF FIRST-RECORD                                              ZBNKE35
012600        MOVE 'E35 exit invoked' TO WS-CONSOLE-MESSAGE             ZBNKE35
012700        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKE35
012800        MOVE 0 TO WS-REC-COUNT-READ                               ZBNKE35
012900        MOVE 0 TO WS-REC-COUNT-WRITTEN                            ZBNKE35
013000        MOVE 0 TO WS-REC-COUNT-DELETED                            ZBNKE35
013100        MOVE LOW-VALUES TO WS-SAVED-PID                           ZBNKE35
013200     END-IF.                                                      ZBNKE35
013300                                                                  ZBNKE35
013400     IF FIRST-RECORD OR                                           ZBNKE35
013500        SUBSEQUENT-RECORD                                         ZBNKE35
013600        ADD 1 TO WS-REC-COUNT-READ                                ZBNKE35
013700        IF LK-EXIT-ENTRY-BUFFER (1:1) IS NOT EQUAL TO '1'         ZBNKE35
013800           ADD 1 TO WS-REC-COUNT-WRITTEN                          ZBNKE35
013900           MOVE 0 TO RETURN-CODE                                  ZBNKE35
014000        ELSE                                                      ZBNKE35
014100           IF LK-EXIT-ENTRY-BUFFER (2:5) IS EQUAL TO WS-SAVED-PID ZBNKE35
014200              ADD 1 TO WS-REC-COUNT-DELETED                       ZBNKE35
014300              MOVE 4 TO RETURN-CODE                               ZBNKE35
014400           ELSE                                                   ZBNKE35
014500              MOVE LK-EXIT-ENTRY-BUFFER (2:5) TO WS-SAVED-PID     ZBNKE35
014600              ADD 1 TO WS-REC-COUNT-WRITTEN                       ZBNKE35
014700              MOVE 0 TO RETURN-CODE                               ZBNKE35
014800           END-IF                                                 ZBNKE35
014900        END-IF                                                    ZBNKE35
015000     ELSE                                                         ZBNKE35
015100        MOVE 'E35 exit terminating at EOF' TO WS-CONSOLE-MESSAGE  ZBNKE35
015200        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKE35
015300        STRING 'E35 exit: records read......: 'DELIMITED BY SIZE  ZBNKE35
015400               WS-REC-COUNT-READ DELIMITED BY SIZE                ZBNKE35
015500          INTO WS-CONSOLE-MESSAGE                                 ZBNKE35
015600        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKE35
015700        STRING 'E35 exit: records deleted...: ' DELIMITED BY SIZE ZBNKE35
015800               WS-REC-COUNT-DELETED DELIMITED BY SIZE             ZBNKE35
015900          INTO WS-CONSOLE-MESSAGE                                 ZBNKE35
016000        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKE35
016100        STRING 'E35 exit: records written...: ' DELIMITED BY SIZE ZBNKE35
016200               WS-REC-COUNT-WRITTEN DELIMITED BY SIZE             ZBNKE35
016300          INTO WS-CONSOLE-MESSAGE                                 ZBNKE35
016400        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKE35
016500        MOVE 8 TO RETURN-CODE                                     ZBNKE35
016600     END-IF.                                                      ZBNKE35
016700                                                                  ZBNKE35
016800     GOBACK.                                                      ZBNKE35
016900                                                                  ZBNKE35
017000                                                                  ZBNKE35
017100***************************************************************** ZBNKE35
017200* Display CONSOLE messages...                                   * ZBNKE35
017300***************************************************************** ZBNKE35
017400 DISPLAY-CONSOLE-MESSAGE.                                         ZBNKE35
017500     DISPLAY WS-PROGRAM-ID ' - ' WS-CONSOLE-MESSAGE               ZBNKE35
017600       UPON CONSOLE.                                              ZBNKE35
017700     MOVE ALL SPACES TO WS-CONSOLE-MESSAGE.                       ZBNKE35
017800                                                                  ZBNKE35
