000100***************************************************************** UBNKPLT1
000200*                                                               * UBNKPLT1
000300*   Copyright (C) 1998-2008 Micro Focus. All Rights Reserved.   * UBNKPLT1
000400*   This demonstration program is provided for use by users     * UBNKPLT1
000500*   of Micro Focus products and may be used, modified and       * UBNKPLT1
000600*   distributed as part of your application provided that       * UBNKPLT1
000700*   you properly acknowledge the copyright of Micro Focus       * UBNKPLT1
000800*   in this material.                                           * UBNKPLT1
000900*                                                               * UBNKPLT1
001000***************************************************************** UBNKPLT1
001100                                                                  UBNKPLT1
001200***************************************************************** UBNKPLT1
001300* Program:     UBNKPLT1.CBL (CICS Version)                      * UBNKPLT1
001400* Layer:       System level                                     * UBNKPLT1
001500* Function:    PLTI Processing (ES/MTO Startup)                 * UBNKPLT1
001600*---------------------------------------------------------------* UBNKPLT1
001700* PLT Initialisation (PLTI) can run one or more programs once   * UBNKPLT1
001800* at system startup or at the initialisation of every SEP.      * UBNKPLT1
001900* This is a dummy process to illustrate the "run once" scenario * UBNKPLT1
002000***************************************************************** UBNKPLT1
002100                                                                  UBNKPLT1
002200 IDENTIFICATION DIVISION.                                         UBNKPLT1
002300 PROGRAM-ID.                                                      UBNKPLT1
002400     UBNKPLT1.                                                    UBNKPLT1
002500 DATE-WRITTEN.                                                    UBNKPLT1
002600     September 2002.                                              UBNKPLT1
002700 DATE-COMPILED.                                                   UBNKPLT1
002800     Today.                                                       UBNKPLT1
002900                                                                  UBNKPLT1
003000 ENVIRONMENT DIVISION.                                            UBNKPLT1
003100                                                                  UBNKPLT1
003200 DATA DIVISION.                                                   UBNKPLT1
003300 WORKING-STORAGE SECTION.                                         UBNKPLT1
003400 01  WS-MISC-STORAGE.                                             UBNKPLT1
003500   05  WS-PROGRAM-ID                         PIC X(8)             UBNKPLT1
003600       VALUE 'UBNKPLT1'.                                          UBNKPLT1
003700   05  WS-WTO-DATA.                                               UBNKPLT1
003800     10  FILLER                              PIC X(4)             UBNKPLT1
003900         VALUE 'INT '.                                            UBNKPLT1
004000     10  FILLER                              PIC X(7)             UBNKPLT1
004100         VALUE 'Termid:'.                                         UBNKPLT1
004200     10  WS-WTO-TERM                         PIC X(4).            UBNKPLT1
004300     10  FILLER                              PIC X(9)             UBNKPLT1
004400         VALUE ', Tranid:'.                                       UBNKPLT1
004500     10  WS-WTO-TRAN                         PIC X(4).            UBNKPLT1
004600     10  FILLER                              PIC X(10)            UBNKPLT1
004700         VALUE ', Program:'.                                      UBNKPLT1
004800     10  WS-WTO-PROG                         PIC X(8).            UBNKPLT1
004900                                                                  UBNKPLT1
005000 LINKAGE SECTION.                                                 UBNKPLT1
005100                                                                  UBNKPLT1
005200 PROCEDURE DIVISION.                                              UBNKPLT1
005300***************************************************************** UBNKPLT1
005400* Display the msg                                               * UBNKPLT1
005500***************************************************************** UBNKPLT1
005600     MOVE z'UBNKPLT1 Complete' TO WS-WTO-DATA.                    UBNKPLT1
005700     EXEC CICS WRITE                                              UBNKPLT1
005800               OPERATOR                                           UBNKPLT1
005900               TEXT(WS-WTO-DATA)                                  UBNKPLT1
006000               TEXTLENGTH(LENGTH OF WS-WTO-DATA)                  UBNKPLT1
006100     END-EXEC.                                                    UBNKPLT1
006200                                                                  UBNKPLT1
006300     EXEC CICS WRITEQ TD                                          UBNKPLT1
006400               QUEUE('CSMT')                                      UBNKPLT1
006500               FROM(WS-WTO-DATA)                                  UBNKPLT1
006600               LENGTH(LENGTH OF WS-WTO-DATA)                      UBNKPLT1
006700     END-EXEC.                                                    UBNKPLT1
006800                                                                  UBNKPLT1
006900***************************************************************** UBNKPLT1
007000* Now we have to have finished and can return to our invoker.   * UBNKPLT1
007100***************************************************************** UBNKPLT1
007200     EXEC CICS                                                    UBNKPLT1
007300          RETURN                                                  UBNKPLT1
007400     END-EXEC.                                                    UBNKPLT1
007500     GOBACK.                                                      UBNKPLT1
007600                                                                  UBNKPLT1
007700* $ Version 5.98b sequenced on Thursday 5 Feb 2009 at 11:00pm     UBNKPLT1
