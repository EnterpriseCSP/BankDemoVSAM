000100***************************************************************** SBNKPLT1
000200*                                                               * SBNKPLT1
000300*   Copyright (C) 1998-2008 Micro Focus. All Rights Reserved.   * SBNKPLT1
000400*   This demonstration program is provided for use by users     * SBNKPLT1
000500*   of Micro Focus products and may be used, modified and       * SBNKPLT1
000600*   distributed as part of your application provided that       * SBNKPLT1
000700*   you properly acknowledge the copyright of Micro Focus       * SBNKPLT1
000800*   in this material.                                           * SBNKPLT1
000900*                                                               * SBNKPLT1
001000***************************************************************** SBNKPLT1
001100                                                                  SBNKPLT1
001200***************************************************************** SBNKPLT1
001300* Program:     SBNKPLT1.CBL (CICS Version)                      * SBNKPLT1
001400* Layer:       Transaction manager specific                     * SBNKPLT1
001500* Function:    Start EZACIC20 if multi region                    *SBNKPLT1
001600***************************************************************** SBNKPLT1
001700                                                                  SBNKPLT1
001800 IDENTIFICATION DIVISION.                                         SBNKPLT1
001900 PROGRAM-ID.                                                      SBNKPLT1
002000     SBNKPLT1.                                                    SBNKPLT1
002100 DATA DIVISION.                                                   SBNKPLT1
002200 WORKING-STORAGE SECTION.                                         SBNKPLT1
002300 01  WS-ALL.                                                      SBNKPLT1
002400   05  WS-PROGRAM                            PIC X(8)             SBNKPLT1
002500       VALUE 'SBNKPLT1'.                                          SBNKPLT1
002600   05  WS-AMAXTASK                           PIC 9(9) COMP.       SBNKPLT1
002700   05  WS-RESP                               PIC S9(8) COMP.      SBNKPLT1
002800   05  WS-MESSAGE.                                                SBNKPLT1
002900     10  WS-MESSAGE-HDR                      PIC X(11).           SBNKPLT1
003000     10  WS-MESSAGE-TXT                      PIC X(110).          SBNKPLT1
003100   05  WS-MESSAGE-LENGTH                     PIC s9(8) comp.      SBNKPLT1
003200                                                                  SBNKPLT1
003300 PROCEDURE DIVISION.                                              SBNKPLT1
003400     MOVE SPACES TO WS-MESSAGE                                    SBNKPLT1
003500     EXEC CICS INQUIRE SYSTEM                                     SBNKPLT1
003600                       AMAXTASKS(WS-AMAXTASK)                     SBNKPLT1
003700     END-EXEC.                                                    SBNKPLT1
003800     IF WS-AMAXTASK IS EQUAL TO 0                                 SBNKPLT1
003900        STRING 'Single tasking region detected.' DELIMITED BY SIZESBNKPLT1
004000               X'1A' DELIMITED BY SIZE                            SBNKPLT1
004100          INTO WS-MESSAGE-TXT                                     SBNKPLT1
004200        PERFORM DISPLAY-MSG                                       SBNKPLT1
004300        STRING 'EZACIC20 will not be started.' DELIMITED BY SIZE  SBNKPLT1
004400               X'1A' DELIMITED BY SIZE                            SBNKPLT1
004500          INTO WS-MESSAGE-TXT                                     SBNKPLT1
004600        PERFORM DISPLAY-MSG                                       SBNKPLT1
004700     ELSE                                                         SBNKPLT1
004800        STRING 'Mutli tasking region detected.' DELIMITED BY SIZE SBNKPLT1
004900               X'1A' DELIMITED BY SIZE                            SBNKPLT1
005000          INTO WS-MESSAGE-TXT                                     SBNKPLT1
005100        PERFORM DISPLAY-MSG                                       SBNKPLT1
005200        EXEC CICS LINK PROGRAM('EZACIC20')                        SBNKPLT1
005300                       RESP(WS-RESP)                              SBNKPLT1
005400        END-EXEC                                                  SBNKPLT1
005500        IF WS-RESP IS EQUAL TO DFHRESP(NORMAL)                    SBNKPLT1
005600           STRING 'EZACIC20 started.' DELIMITED BY SIZE           SBNKPLT1
005700                  X'1A' DELIMITED BY SIZE                         SBNKPLT1
005800             INTO WS-MESSAGE-TXT                                  SBNKPLT1
005900           PERFORM DISPLAY-MSG                                    SBNKPLT1
006000        ELSE                                                      SBNKPLT1
006100           STRING 'EZACIC20 failed to start.' DELIMITED BY SIZE   SBNKPLT1
006200                  X'1A' DELIMITED BY SIZE                         SBNKPLT1
006300             INTO WS-MESSAGE-TXT                                  SBNKPLT1
006400           PERFORM DISPLAY-MSG                                    SBNKPLT1
006500     END-IF.                                                      SBNKPLT1
006600     EXEC CICS RETURN                                             SBNKPLT1
006700     END-EXEC.                                                    SBNKPLT1
006800     GOBACK.                                                      SBNKPLT1
006900                                                                  SBNKPLT1
007000 DISPLAY-MSG.                                                     SBNKPLT1
007100     MOVE WS-PROGRAM TO WS-MESSAGE-HDR(1:8).                      SBNKPLT1
007200     MOVE ' - ' TO WS-MESSAGE-HDR(9:3).                           SBNKPLT1
007300     MOVE 0 TO WS-MESSAGE-LENGTH.                                 SBNKPLT1
007400     INSPECT WS-MESSAGE TALLYING WS-MESSAGE-LENGTH                SBNKPLT1
007500       FOR CHARACTERS BEFORE X'1A'.                               SBNKPLT1
007600     EXEC CICS WRITE                                              SBNKPLT1
007700               OPERATOR                                           SBNKPLT1
007800               TEXT(WS-MESSAGE)                                   SBNKPLT1
007900               TEXTLENGTH(WS-MESSAGE-LENGTH)                      SBNKPLT1
008000     END-EXEC.                                                    SBNKPLT1
008100     MOVE SPACES TO WS-MESSAGE.                                   SBNKPLT1
008200                                                                  SBNKPLT1
008300* $ Version 5.98b sequenced on Thursday 5 Feb 2009 at 11:00pm     SBNKPLT1
