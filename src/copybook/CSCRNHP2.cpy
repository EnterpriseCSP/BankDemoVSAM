000100***************************************************************** cscrnhp2
000200*                                                               * cscrnhp2
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cscrnhp2
000400*                                                               * cscrnhp2
000500***************************************************************** cscrnhp2
000600                                                                  cscrnhp2
000700***************************************************************** cscrnhp2
000800* CSCRNHP2.CPY                                                  * cscrnhp2
000900*---------------------------------------------------------------* cscrnhp2
001000* Procedure code to populate screen titles                      * cscrnhp2
001100***************************************************************** cscrnhp2
001200     CALL 'SCUSTOMP' USING SCREEN-TITLES.                         cscrnhp2
001300     MOVE SCREEN-TITLE1 TO AHEAD1O IN <<SCRN>>.                   cscrnhp2
001400     MOVE SCREEN-TITLE2 TO AHEAD2O IN <<SCRN>>.                   cscrnhp2
001500     CALL 'SVERSONP' USING WS-VERSION.                            cscrnhp2
001600*    MOVE WS-VERSION TO AVERO IN <<SCRN>>.                        cscrnhp2
001700     MOVE WS-TRAN-ID TO ATRANO IN <<SCRN>>.                       cscrnhp2
001800     MOVE DD-TIME-OUTPUT TO ATIMEO IN <<SCRN>>.                   cscrnhp2
001900     MOVE DDO-DATA TO ADATEO IN <<SCRN>>.                         cscrnhp2
002000* Move in any error message                                       cscrnhp2
002100* Move in screen specific fields                                  cscrnhp2
002200        MOVE :OPTN:-HELP-LINE (01) TO AHLP01O IN <<SCRN>>.        cscrnhp2
002300        MOVE :OPTN:-HELP-LINE (02) TO AHLP02O IN <<SCRN>>.        cscrnhp2
002400        MOVE :OPTN:-HELP-LINE (03) TO AHLP03O IN <<SCRN>>.        cscrnhp2
002500        MOVE :OPTN:-HELP-LINE (04) TO AHLP04O IN <<SCRN>>.        cscrnhp2
002600        MOVE :OPTN:-HELP-LINE (05) TO AHLP05O IN <<SCRN>>.        cscrnhp2
002700        MOVE :OPTN:-HELP-LINE (06) TO AHLP06O IN <<SCRN>>.        cscrnhp2
002800        MOVE :OPTN:-HELP-LINE (07) TO AHLP07O IN <<SCRN>>.        cscrnhp2
002900        MOVE :OPTN:-HELP-LINE (08) TO AHLP08O IN <<SCRN>>.        cscrnhp2
003000        MOVE :OPTN:-HELP-LINE (09) TO AHLP09O IN <<SCRN>>.        cscrnhp2
003100        MOVE :OPTN:-HELP-LINE (10) TO AHLP10O IN <<SCRN>>.        cscrnhp2
003200        MOVE :OPTN:-HELP-LINE (11) TO AHLP11O IN <<SCRN>>.        cscrnhp2
003300        MOVE :OPTN:-HELP-LINE (12) TO AHLP12O IN <<SCRN>>.        cscrnhp2
003400        MOVE :OPTN:-HELP-LINE (13) TO AHLP13O IN <<SCRN>>.        cscrnhp2
003500        MOVE :OPTN:-HELP-LINE (14) TO AHLP14O IN <<SCRN>>.        cscrnhp2
003600        MOVE :OPTN:-HELP-LINE (15) TO AHLP15O IN <<SCRN>>.        cscrnhp2
003700        MOVE :OPTN:-HELP-LINE (16) TO AHLP16O IN <<SCRN>>.        cscrnhp2
003800        MOVE :OPTN:-HELP-LINE (17) TO AHLP17O IN <<SCRN>>.        cscrnhp2
003900        MOVE :OPTN:-HELP-LINE (18) TO AHLP18O IN <<SCRN>>.        cscrnhp2
004000        MOVE :OPTN:-HELP-LINE (19) TO AHLP19O IN <<SCRN>>.        cscrnhp2
004100* Turn colour off if required                                     cscrnhp2
004200     IF COLOUR-OFF                                                cscrnhp2
004300        MOVE DFHGREEN TO ATXT01C IN <<SCRN>>                      cscrnhp2
004400        MOVE DFHGREEN TO ASCRNC IN <<SCRN>>                       cscrnhp2
004500        MOVE DFHGREEN TO AHEAD1C IN <<SCRN>>                      cscrnhp2
004600        MOVE DFHGREEN TO ADATEC IN <<SCRN>>                       cscrnhp2
004700        MOVE DFHGREEN TO ATXT02C IN <<SCRN>>                      cscrnhp2
004800        MOVE DFHGREEN TO ATRANC IN <<SCRN>>                       cscrnhp2
004900        MOVE DFHGREEN TO AHEAD2C IN <<SCRN>>                      cscrnhp2
005000        MOVE DFHGREEN TO ATIMEC IN <<SCRN>>                       cscrnhp2
005100        MOVE DFHGREEN TO AHLP01C IN <<SCRN>>                      cscrnhp2
005200        MOVE DFHGREEN TO AHLP02C IN <<SCRN>>                      cscrnhp2
005300        MOVE DFHGREEN TO AHLP03C IN <<SCRN>>                      cscrnhp2
005400        MOVE DFHGREEN TO AHLP04C IN <<SCRN>>                      cscrnhp2
005500        MOVE DFHGREEN TO AHLP05C IN <<SCRN>>                      cscrnhp2
005600        MOVE DFHGREEN TO AHLP06C IN <<SCRN>>                      cscrnhp2
005700        MOVE DFHGREEN TO AHLP07C IN <<SCRN>>                      cscrnhp2
005800        MOVE DFHGREEN TO AHLP08C IN <<SCRN>>                      cscrnhp2
005900        MOVE DFHGREEN TO AHLP09C IN <<SCRN>>                      cscrnhp2
006000        MOVE DFHGREEN TO AHLP10C IN <<SCRN>>                      cscrnhp2
006100        MOVE DFHGREEN TO AHLP11C IN <<SCRN>>                      cscrnhp2
006200        MOVE DFHGREEN TO AHLP12C IN <<SCRN>>                      cscrnhp2
006300        MOVE DFHGREEN TO AHLP13C IN <<SCRN>>                      cscrnhp2
006400        MOVE DFHGREEN TO AHLP14C IN <<SCRN>>                      cscrnhp2
006500        MOVE DFHGREEN TO AHLP15C IN <<SCRN>>                      cscrnhp2
006600        MOVE DFHGREEN TO AHLP16C IN <<SCRN>>                      cscrnhp2
006700        MOVE DFHGREEN TO AHLP17C IN <<SCRN>>                      cscrnhp2
006800        MOVE DFHGREEN TO AHLP18C IN <<SCRN>>                      cscrnhp2
006900        MOVE DFHGREEN TO AHLP19C IN <<SCRN>>                      cscrnhp2
007000        MOVE DFHGREEN TO ATXT03C IN <<SCRN>>                      cscrnhp2
007100*       MOVE DFHGREEN TO AVERC IN <<SCRN>>                        cscrnhp2
007200     END-IF.                                                      cscrnhp2
007300                                                                  cscrnhp2
007400* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cscrnhp2
