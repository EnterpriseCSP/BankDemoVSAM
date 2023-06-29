000100***************************************************************** UTWOSCMP
000200*                                                               * UTWOSCMP
000300*   Copyright (C) 1998-2008 Micro Focus. All Rights Reserved.   * UTWOSCMP
000400*   This demonstration program is provided for use by users     * UTWOSCMP
000500*   of Micro Focus products and may be used, modified and       * UTWOSCMP
000600*   distributed as part of your application provided that       * UTWOSCMP
000700*   you properly acknowledge the copyright of Micro Focus       * UTWOSCMP
000800*   in this material.                                           * UTWOSCMP
000900*                                                               * UTWOSCMP
001000***************************************************************** UTWOSCMP
001100                                                                  UTWOSCMP
001200***************************************************************** UTWOSCMP
001300* Program:     UTWOSCMP.CBL                                     * UTWOSCMP
001400* Function:    ??conversion utility routine                     * UTWOSCMP
001500***************************************************************** UTWOSCMP
001600                                                                  UTWOSCMP
001700 IDENTIFICATION DIVISION.                                         UTWOSCMP
001800 PROGRAM-ID.                                                      UTWOSCMP
001900     UTWOSCMP.                                                    UTWOSCMP
002000 DATE-WRITTEN.                                                    UTWOSCMP
002100     September 2002.                                              UTWOSCMP
002200 DATE-COMPILED.                                                   UTWOSCMP
002300     Today.                                                       UTWOSCMP
002400                                                                  UTWOSCMP
002500 ENVIRONMENT DIVISION.                                            UTWOSCMP
002600                                                                  UTWOSCMP
002700 DATA DIVISION.                                                   UTWOSCMP
002800 WORKING-STORAGE SECTION.                                         UTWOSCMP
002900 01  WS-MISC-STORAGE.                                             UTWOSCMP
003000   05  WS-PROGRAM-ID                         PIC X(8)             UTWOSCMP
003100       VALUE 'UTWOSCMP'.                                          UTWOSCMP
003200   05  WS-LEN                                PIC 9(4) COMP.       UTWOSCMP
003300                                                                  UTWOSCMP
003400   05  WS-WORK-INPUT.                                             UTWOSCMP
003500     10  WS-WORK-INPUT-N                     PIC 9(4) COMP.       UTWOSCMP
003600   05  FILLER REDEFINES WS-WORK-INPUT.                            UTWOSCMP
003700     10  WS-WORK-INPUT-BYTE-1                PIC X(1).            UTWOSCMP
003800     10  WS-WORK-INPUT-BYTE-2                PIC X(1).            UTWOSCMP
003900                                                                  UTWOSCMP
004000   05  WS-WORK-OUTPUT.                                            UTWOSCMP
004100     10  WS-WORK-OUTPUT-N                    PIC 9(4) COMP.       UTWOSCMP
004200   05  FILLER REDEFINES WS-WORK-OUTPUT.                           UTWOSCMP
004300     10  WS-WORK-OUTPUT-BYTE-1               PIC X(1).            UTWOSCMP
004400     10  WS-WORK-OUTPUT-BYTE-2               PIC X(1).            UTWOSCMP
004500                                                                  UTWOSCMP
004600 LINKAGE SECTION.                                                 UTWOSCMP
004700 01  LK-TWOS-CMP-LEN                         PIC S9(4) COMP.      UTWOSCMP
004800 01  LK-TWOS-CMP-INPUT                       PIC X(256).          UTWOSCMP
004900 01  LK-TWOS-CMP-OUTPUT                      PIC X(256).          UTWOSCMP
005000                                                                  UTWOSCMP
005100 PROCEDURE DIVISION USING LK-TWOS-CMP-LEN                         UTWOSCMP
005200                          LK-TWOS-CMP-INPUT                       UTWOSCMP
005300                          LK-TWOS-CMP-OUTPUT.                     UTWOSCMP
005400     PERFORM VARYING WS-LEN FROM 1 BY 1                           UTWOSCMP
005500       UNTIL WS-LEN > LK-TWOS-CMP-LEN                             UTWOSCMP
005600       MOVE 0 TO WS-WORK-INPUT-N                                  UTWOSCMP
005700       MOVE LK-TWOS-CMP-INPUT(WS-LEN:1) TO WS-WORK-INPUT-BYTE-2   UTWOSCMP
005800       MOVE 255 TO WS-WORK-OUTPUT-N                               UTWOSCMP
005900       SUBTRACT WS-WORK-INPUT-N FROM WS-WORK-OUTPUT-N             UTWOSCMP
006000       MOVE WS-WORK-OUTPUT-BYTE-2 TO LK-TWOS-CMP-OUTPUT(WS-LEN:1) UTWOSCMP
006100     END-PERFORM.                                                 UTWOSCMP
006200                                                                  UTWOSCMP
006300     GOBACK.                                                      UTWOSCMP
006400                                                                  UTWOSCMP
006500* $ Version 5.98b sequenced on Thursday 5 Feb 2009 at 11:00pm     UTWOSCMP
