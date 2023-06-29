000100***************************************************************** SCUSTOMP
000200*                                                               * SCUSTOMP
000300*   Copyright (C) 1998-2008 Micro Focus. All Rights Reserved.   * SCUSTOMP
000400*   This demonstration program is provided for use by users     * SCUSTOMP
000500*   of Micro Focus products and may be used, modified and       * SCUSTOMP
000600*   distributed as part of your application provided that       * SCUSTOMP
000700*   you properly acknowledge the copyright of Micro Focus       * SCUSTOMP
000800*   in this material.                                           * SCUSTOMP
000900*                                                               * SCUSTOMP
001000***************************************************************** SCUSTOMP
001100                                                                  SCUSTOMP
001200***************************************************************** SCUSTOMP
001300* Program:     SCUSTOMP.CBL                                     * SCUSTOMP
001400* Layer:       Screen handling                                  * SCUSTOMP
001500* Function:    Populate screen titles                           * SCUSTOMP
001600***************************************************************** SCUSTOMP
001700                                                                  SCUSTOMP
001800 IDENTIFICATION DIVISION.                                         SCUSTOMP
001900 PROGRAM-ID.                                                      SCUSTOMP
002000     SCUSTOMP.                                                    SCUSTOMP
002100 DATE-WRITTEN.                                                    SCUSTOMP
002200     September 2002.                                              SCUSTOMP
002300 DATE-COMPILED.                                                   SCUSTOMP
002400     Today.                                                       SCUSTOMP
002500                                                                  SCUSTOMP
002600 ENVIRONMENT DIVISION.                                            SCUSTOMP
002700                                                                  SCUSTOMP
002800 DATA DIVISION.                                                   SCUSTOMP
002900 WORKING-STORAGE SECTION.                                         SCUSTOMP
003000***************************************************************** SCUSTOMP
003100* Headings for screens                                          * SCUSTOMP
003200*---------------------------------------------------------------* SCUSTOMP
003300* The screens have space for two titles, one on the top line,   * SCUSTOMP
003400* one on the second line. Each is 50 bytes long and is centered * SCUSTOMP
003500* on the line.                                                  * SCUSTOMP
003600***************************************************************** SCUSTOMP
003700 01  SCREEN-TITLES.                                               SCUSTOMP
003800   05  SCREEN-TITLE1                         PIC X(50)            SCUSTOMP
003900       VALUE '  Micro Focus Enterprise Solution Demonstration   '.SCUSTOMP
004000*             00000000011111111112222222222333333333344444444445'.SCUSTOMP
004100*      VALUE '12345678901234567890123456789012345678901234567890'.SCUSTOMP
004200   05  SCREEN-TITLE2                         PIC X(50)            SCUSTOMP
004300       VALUE '  *********************************************   '.SCUSTOMP
004400*             00000000011111111112222222222333333333344444444445'.SCUSTOMP
004500*      VALUE '12345678901234567890123456789012345678901234567890'.SCUSTOMP
004600                                                                  SCUSTOMP
004700 LINKAGE SECTION.                                                 SCUSTOMP
004800 01  LK-SCREEN-TITLES.                                            SCUSTOMP
004900   05  LK-SCREEN-TITLE1                      PIC X(50).           SCUSTOMP
005000   05  LK-SCREEN-TITLE2                      PIC X(50).           SCUSTOMP
005100                                                                  SCUSTOMP
005200 PROCEDURE DIVISION USING LK-SCREEN-TITLES.                       SCUSTOMP
005300***************************************************************** SCUSTOMP
005400* Move the titles from our area to the passed area              * SCUSTOMP
005500***************************************************************** SCUSTOMP
005600     MOVE SCREEN-TITLES TO LK-SCREEN-TITLES.                      SCUSTOMP
005700     GOBACK.                                                      SCUSTOMP
005800                                                                  SCUSTOMP
005900* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     SCUSTOMP
