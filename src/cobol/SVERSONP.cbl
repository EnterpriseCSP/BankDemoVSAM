000100***************************************************************** sversonp
000200*                                                               * sversonp
000300*   Copyright (C) 1998-2010 Micro Focus. All Rights Reserved.   * sversonp
000400*   This demonstration program is provided for use by users     * sversonp
000500*   of Micro Focus products and may be used, modified and       * sversonp
000600*   distributed as part of your application provided that       * sversonp
000700*   you properly acknowledge the copyright of Micro Focus       * sversonp
000800*   in this material.                                           * sversonp
000900*                                                               * sversonp
001000***************************************************************** sversonp
001100                                                                  sversonp
001200***************************************************************** sversonp
001300* Program:     SVERSONP.CBL                                     * sversonp
001400* Layer:       Screen handling                                  * sversonp
001500* Function:    Populate screen titles                           * sversonp
001600***************************************************************** sversonp
001700                                                                  sversonp
001800 IDENTIFICATION DIVISION.                                         sversonp
001900 PROGRAM-ID.                                                      sversonp
002000     SVERSONP.                                                    sversonp
002100 DATE-WRITTEN.                                                    sversonp
002200     September 2002.                                              sversonp
002300 DATE-COMPILED.                                                   sversonp
002400     Today.                                                       sversonp
002500                                                                  sversonp
002600 ENVIRONMENT DIVISION.                                            sversonp
002700                                                                  sversonp
002800 DATA DIVISION.                                                   sversonp
002900 WORKING-STORAGE SECTION.                                         sversonp
003000***************************************************************** sversonp
003100* Version to show on screens                                    * sversonp
003200***************************************************************** sversonp
003300 01  WS-VERSION                              PIC X(7)             sversonp
003400     VALUE ' V5.99c'.                                             sversonp
003500                                                                  sversonp
003600 LINKAGE SECTION.                                                 sversonp
003700 01  LK-VERSION                              PIC X(7).            sversonp
003800                                                                  sversonp
003900 PROCEDURE DIVISION USING LK-VERSION.                             sversonp
004000***************************************************************** sversonp
004100* Move the version from our area to the passed area             * sversonp
004200***************************************************************** sversonp
004300     MOVE WS-VERSION TO LK-VERSION.                               sversonp
004400     GOBACK.                                                      sversonp
004500                                                                  sversonp
004600* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     sversonp
004700                                                                  sversonp
