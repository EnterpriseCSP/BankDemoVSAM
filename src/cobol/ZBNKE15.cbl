000100***************************************************************** ZBNKE15
000200*                                                               * ZBNKE15
000300*   Copyright (C) 1998-2002 Micro Focus. All Rights Reserved.   * ZBNKE15
000400*   This demonstration program is provided for use by users     * ZBNKE15
000500*   of Micro Focus products and may be used, modified and       * ZBNKE15
000600*   distributed as part of your application provided that       * ZBNKE15
000700*   you properly acknowledge the copyright of Micro Focus       * ZBNKE15
000800*   in this material.                                           * ZBNKE15
000900*                                                               * ZBNKE15
001000***************************************************************** ZBNKE15
001100                                                                  ZBNKE15
001200***************************************************************** ZBNKE15
001300* Program:     ZBNKE15.CBL                                      * ZBNKE15
001400* Function:    Demonstrate E15 capability.                      * ZBNKE15
001500***************************************************************** ZBNKE15
001600*MFJSORT provides a standard linkage area to the E15 and E35    * ZBNKE15
001700*exit program. It is compatible with the mainframe utility      * ZBNKE15
001800*and should be defined as follows:                              * ZBNKE15
001900*                                                               * ZBNKE15
002000*COBOL statement      PIC        Value                          * ZBNKE15
002100* RECORD-FLAGS        9(8)COMP   0 - first record passed        * ZBNKE15
002200*                                4 - subsequent records passed  * ZBNKE15
002300*                                8 - last record passed         * ZBNKE15
002400* ENTRY-BUFFER        X(n)       Contents of the input record.  * ZBNKE15
002500*                                Do not change this area.       * ZBNKE15
002600* EXIT-BUFFER         X(n)       Contents of the new or altered * ZBNKE15
002700*                                  record provided by the exit. * ZBNKE15
002800* UNUSED-ENTRY        9(8)COMP   Not used                       * ZBNKE15
002900*                                                               * ZBNKE15
003000* UNUSED-ENTRY        9(8)COMP   Not used                       * ZBNKE15
003100*                                                               * ZBNKE15
003200* ENTRY-RECORD-LENGTH 9(8)COMP   Length of the input record.    * ZBNKE15
003300* EXIT-RECORD-LENGTH  9(8)COMP   Length of the new or altered   * ZBNKE15
003400*                                  record provided by the exit. * ZBNKE15
003500* UNUSED-ENTRY        9(8)COMP   Not used                       * ZBNKE15
003600* EXIT-AREA-LENGTH    9(4)COMP   Length of the exit area        * ZBNKE15
003700*                                  scratchpad.                  * ZBNKE15
003800*                                Do not change this field.      * ZBNKE15
003900* EXIT-AREA           X(n)       Exit area scratchpad used by   * ZBNKE15
004000*                                  the exit to maintain         * ZBNKE15
004100*                                  variables between calls to   * ZBNKE15
004200*                                  the exit program.            * ZBNKE15
004300*                                                               * ZBNKE15
004400* Return Code         Meaning                                   * ZBNKE15
004500*   0                 No action required                        * ZBNKE15
004600*   4                 Delete the current record.                * ZBNKE15
004700*                       For E15, the record is not sorted.      * ZBNKE15
004800*                       For E35, the record is not written to   * ZBNKE15
004900*                         the output dataset                    * ZBNKE15
005000*   8                 Do not call this exit again;              * ZBNKE15
005100*                       exit processing is no longer required   * ZBNKE15
005200*   12                Insert the current record.                * ZBNKE15
005300*                       For E15, the record is inserted for     * ZBNKE15
005400*                         sorting.                              * ZBNKE15
005500*                       For E35, the record is written to the   * ZBNKE15
005600*                         output dataset                        * ZBNKE15
005700*   16                Terminate. The job step is terminated     * ZBNKE15
005800*                       with the condition code set to 16       * ZBNKE15
005900*   20                Alter the current record.                 * ZBNKE15
006000*                       For E15, the altered record is passed   * ZBNKE15
006100*                         to the sort.                          * ZBNKE15
006200*                       For E35, the altered record is written  * ZBNKE15
006300*                         to the output dataset                 * ZBNKE15
006400***************************************************************** ZBNKE15
006500                                                                  ZBNKE15
006600 IDENTIFICATION DIVISION.                                         ZBNKE15
006700 PROGRAM-ID.                                                      ZBNKE15
006800     ZBNKE15.                                                     ZBNKE15
006900 DATE-WRITTEN.                                                    ZBNKE15
007000     September 2002.                                              ZBNKE15
007100 DATE-COMPILED.                                                   ZBNKE15
007200     Today.                                                       ZBNKE15
007300 ENVIRONMENT DIVISION.                                            ZBNKE15
007400                                                                  ZBNKE15
007500 DATA DIVISION.                                                   ZBNKE15
007600                                                                  ZBNKE15
007700 WORKING-STORAGE SECTION.                                         ZBNKE15
007800 01  WS-MISC-STORAGE.                                             ZBNKE15
007900   05  WS-PROGRAM-ID                         PIC X(8)             ZBNKE15
008000       VALUE 'ZBNKE15 '.                                          ZBNKE15
008100   05  WS-REC-COUNT                          PIC 9(5).            ZBNKE15
008200                                                                  ZBNKE15
008300 01  WS-CONSOLE-MESSAGE                      PIC X(48).           ZBNKE15
008400                                                                  ZBNKE15
008500 COPY CABENDD.                                                    ZBNKE15
008600                                                                  ZBNKE15
008700 LINKAGE SECTION.                                                 ZBNKE15
008800 01  LK-EXIT-RECORD-FLAGS                    PIC 9(8) COMP.       ZBNKE15
008900   88  FIRST-RECORD                          VALUE 0.             ZBNKE15
009000   88  SUBSEQUENT-RECORD                     VALUE 4.             ZBNKE15
009100   88  LAST-RECORD                           VALUE 8.             ZBNKE15
009200                                                                  ZBNKE15
009300 01  LK-EXIT-ENTRY-BUFFER                    PIC X(116).          ZBNKE15
009400                                                                  ZBNKE15
009500 01  LK-EXIT-EXIT-BUFFER                     PIC X(116).          ZBNKE15
009600                                                                  ZBNKE15
009700 01  LK-EXIT-UNUSED-1                        PIC 9(8) COMP.       ZBNKE15
009800                                                                  ZBNKE15
009900 01  LK-EXIT-UNUSED-2                        PIC 9(8) COMP.       ZBNKE15
010000                                                                  ZBNKE15
010100 01  LK-EXIT-ENTRY-REC-LEN                   PIC 9(8) COMP.       ZBNKE15
010200                                                                  ZBNKE15
010300 01  LK-EXIT-EXIT-REC-LEN                    PIC 9(8) COMP.       ZBNKE15
010400                                                                  ZBNKE15
010500 01  LK-EXIT-SPA-LEN                         PIC 9(4) COMP.       ZBNKE15
010600                                                                  ZBNKE15
010700 01  LK-EXIT-SPA.                                                 ZBNKE15
010800   05  LK-EXIT-SPA-BYTE                      PIC X(1)             ZBNKE15
010900       OCCURS 1 TO 9999 TIMES DEPENDING ON LK-EXIT-SPA-LEN.       ZBNKE15
011000                                                                  ZBNKE15
011100 PROCEDURE DIVISION USING LK-EXIT-RECORD-FLAGS                    ZBNKE15
011200                          LK-EXIT-ENTRY-BUFFER                    ZBNKE15
011300                          LK-EXIT-EXIT-BUFFER                     ZBNKE15
011400                          LK-EXIT-UNUSED-1                        ZBNKE15
011500                          LK-EXIT-UNUSED-2                        ZBNKE15
011600                          LK-EXIT-ENTRY-REC-LEN                   ZBNKE15
011700                          LK-EXIT-EXIT-REC-LEN                    ZBNKE15
011800                          LK-EXIT-SPA-LEN                         ZBNKE15
011900                          LK-EXIT-SPA.                            ZBNKE15
012000                                                                  ZBNKE15
012100                                                                  ZBNKE15
012200     IF FIRST-RECORD                                              ZBNKE15
012300        MOVE 'E15 exit invoked' TO WS-CONSOLE-MESSAGE             ZBNKE15
012400        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKE15
012500        MOVE 0 TO WS-REC-COUNT                                    ZBNKE15
012600     END-IF.                                                      ZBNKE15
012700                                                                  ZBNKE15
012800     IF FIRST-RECORD OR                                           ZBNKE15
012900        SUBSEQUENT-RECORD                                         ZBNKE15
013000        ADD 1 TO WS-REC-COUNT                                     ZBNKE15
013100        MOVE 0 TO RETURN-CODE                                     ZBNKE15
013200     ELSE                                                         ZBNKE15
013300        MOVE 'E15 exit terminating at EOF' TO WS-CONSOLE-MESSAGE  ZBNKE15
013400        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKE15
013500        STRING 'having processed ' DELIMITED BY SIZE              ZBNKE15
013600               WS-REC-COUNT DELIMITED BY SIZE                     ZBNKE15
013700               ' records' DELIMITED BY SIZE                       ZBNKE15
013800          INTO WS-CONSOLE-MESSAGE                                 ZBNKE15
013900        PERFORM DISPLAY-CONSOLE-MESSAGE                           ZBNKE15
014000        MOVE 8 TO RETURN-CODE                                     ZBNKE15
014100     END-IF.                                                      ZBNKE15
014200                                                                  ZBNKE15
014300     GOBACK.                                                      ZBNKE15
014400                                                                  ZBNKE15
014500                                                                  ZBNKE15
014600***************************************************************** ZBNKE15
014700* Display CONSOLE messages...                                   * ZBNKE15
014800***************************************************************** ZBNKE15
014900 DISPLAY-CONSOLE-MESSAGE.                                         ZBNKE15
015000     DISPLAY WS-PROGRAM-ID ' - ' WS-CONSOLE-MESSAGE               ZBNKE15
015100       UPON CONSOLE.                                              ZBNKE15
015200     MOVE ALL SPACES TO WS-CONSOLE-MESSAGE.                       ZBNKE15
015300                                                                  ZBNKE15
