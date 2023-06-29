000100***************************************************************** UBNKPLT2
000200*                                                               * UBNKPLT2
000300*   Copyright (C) 1998-2008 Micro Focus. All Rights Reserved.   * UBNKPLT2
000400*   This demonstration program is provided for use by users     * UBNKPLT2
000500*   of Micro Focus products and may be used, modified and       * UBNKPLT2
000600*   distributed as part of your application provided that       * UBNKPLT2
000700*   you properly acknowledge the copyright of Micro Focus       * UBNKPLT2
000800*   in this material.                                           * UBNKPLT2
000900*                                                               * UBNKPLT2
001000***************************************************************** UBNKPLT2
001100                                                                  UBNKPLT2
001200***************************************************************** UBNKPLT2
001300* Program:     UBNKPLT2.CBL (CICS Version)                      * UBNKPLT2
001400* Layer:       System level                                     * UBNKPLT2
001500* Function:    PLTI Processing (ES/MTO Startup)                 * UBNKPLT2
001600*---------------------------------------------------------------* UBNKPLT2
001700* PLT Initialisation (PLTI) can run one or more programs once   * UBNKPLT2
001800* at system startup or at the initialisation of every SEP.      * UBNKPLT2
001900* This is a dummy process to illustrate the "run in each SEP"   * UBNKPLT2
002000* scenario                                                      * UBNKPLT2
002100***************************************************************** UBNKPLT2
002200                                                                  UBNKPLT2
002300 IDENTIFICATION DIVISION.                                         UBNKPLT2
002400 PROGRAM-ID.                                                      UBNKPLT2
002500     UBNKPLT2.                                                    UBNKPLT2
002600 DATE-WRITTEN.                                                    UBNKPLT2
002700     September 2002.                                              UBNKPLT2
002800 DATE-COMPILED.                                                   UBNKPLT2
002900     Today.                                                       UBNKPLT2
003000                                                                  UBNKPLT2
003100 ENVIRONMENT DIVISION.                                            UBNKPLT2
003200                                                                  UBNKPLT2
003300 DATA DIVISION.                                                   UBNKPLT2
003400 WORKING-STORAGE SECTION.                                         UBNKPLT2
003500 01  WS-MISC-STORAGE.                                             UBNKPLT2
003600   05  WS-PROGRAM-ID                         PIC X(8)             UBNKPLT2
003700       VALUE 'UBNKPLT2'.                                          UBNKPLT2
003800   05  WS-WTO-DATA.                                               UBNKPLT2
003900     10  FILLER                              PIC X(4)             UBNKPLT2
004000         VALUE 'INT '.                                            UBNKPLT2
004100     10  FILLER                              PIC X(7)             UBNKPLT2
004200         VALUE 'Termid:'.                                         UBNKPLT2
004300     10  WS-WTO-TERM                         PIC X(4).            UBNKPLT2
004400     10  FILLER                              PIC X(9)             UBNKPLT2
004500         VALUE ', Tranid:'.                                       UBNKPLT2
004600     10  WS-WTO-TRAN                         PIC X(4).            UBNKPLT2
004700     10  FILLER                              PIC X(10)            UBNKPLT2
004800         VALUE ', Program:'.                                      UBNKPLT2
004900     10  WS-WTO-PROG                         PIC X(8).            UBNKPLT2
005000                                                                  UBNKPLT2
005100 LINKAGE SECTION.                                                 UBNKPLT2
005200                                                                  UBNKPLT2
005300 PROCEDURE DIVISION.                                              UBNKPLT2
005400***************************************************************** UBNKPLT2
005500* Display the msg                                               * UBNKPLT2
005600***************************************************************** UBNKPLT2
005700     MOVE z'UBNKPLT2 Complete' TO WS-WTO-DATA.                    UBNKPLT2
005800     EXEC CICS WRITE                                              UBNKPLT2
005900               OPERATOR                                           UBNKPLT2
006000               TEXT(WS-WTO-DATA)                                  UBNKPLT2
006100               TEXTLENGTH(LENGTH OF WS-WTO-DATA)                  UBNKPLT2
006200     END-EXEC.                                                    UBNKPLT2
006300                                                                  UBNKPLT2
006400     EXEC CICS WRITEQ TD                                          UBNKPLT2
006500               QUEUE('CSMT')                                      UBNKPLT2
006600               FROM(WS-WTO-DATA)                                  UBNKPLT2
006700               LENGTH(LENGTH OF WS-WTO-DATA)                      UBNKPLT2
006800     END-EXEC.                                                    UBNKPLT2
006900                                                                  UBNKPLT2
007000***************************************************************** UBNKPLT2
007100* Now we have to have finished and can return to our invoker.   * UBNKPLT2
007200***************************************************************** UBNKPLT2
007300     EXEC CICS                                                    UBNKPLT2
007400          RETURN                                                  UBNKPLT2
007500     END-EXEC.                                                    UBNKPLT2
007600     GOBACK.                                                      UBNKPLT2
007700                                                                  UBNKPLT2
007800* $ Version 5.98b sequenced on Thursday 5 Feb 2009 at 11:00pm     UBNKPLT2
