000100***************************************************************** dbankiop
000200*                                                               * dbankiop
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * dbankiop
000400*   This demonstration program is provided for use by users     * dbankiop
000500*   of Micro Focus products and may be used, modified and       * dbankiop
000600*   distributed as part of your application provided that       * dbankiop
000700*   you properly acknowledge the copyright of Micro Focus       * dbankiop
000800*   in this material.                                           * dbankiop
000900*                                                               * dbankiop
001000***************************************************************** dbankiop
001100                                                                  dbankiop
001200***************************************************************** dbankiop
001300* Program:     DBANKIOP.CBL                                     * dbankiop
001400* Function:    Return data access method                        * dbankiop
001500*              VSAM version                                     * dbankiop
001600***************************************************************** dbankiop
001700                                                                  dbankiop
001800 IDENTIFICATION DIVISION.                                         dbankiop
001900 PROGRAM-ID.                                                      dbankiop
002000     DBANKIOP.                                                    dbankiop
002100 DATE-WRITTEN.                                                    dbankiop
002200     September 2002.                                              dbankiop
002300 DATE-COMPILED.                                                   dbankiop
002400     Today.                                                       dbankiop
002500                                                                  dbankiop
002600 ENVIRONMENT DIVISION.                                            dbankiop
002700                                                                  dbankiop
002800 DATA DIVISION.                                                   dbankiop
002900                                                                  dbankiop
003000 WORKING-STORAGE SECTION.                                         dbankiop
003100 01  WS-MISC-STORAGE.                                             dbankiop
003200   05  WS-PROGRAM-ID                         PIC X(8)             dbankiop
003300       VALUE 'DBANKIOP'.                                          dbankiop
003400                                                                  dbankiop
003500 LINKAGE SECTION.                                                 dbankiop
003600 01  LK-PASS-AREA                            PIC X(6).            dbankiop
003700                                                                  dbankiop
003800 PROCEDURE DIVISION USING LK-PASS-AREA.                           dbankiop
003900***************************************************************** dbankiop
004000* Move the data to the passed area                              * dbankiop
004100***************************************************************** dbankiop
004200     MOVE 'VSM   ' TO LK-PASS-AREA.                               dbankiop
004300                                                                  dbankiop
004400***************************************************************** dbankiop
004500* Return to our caller                                          * dbankiop
004600***************************************************************** dbankiop
004700     GOBACK.                                                      dbankiop
004800                                                                  dbankiop
004900* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     dbankiop
