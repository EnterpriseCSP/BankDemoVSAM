000100***************************************************************** cscrnhp1
000200*                                                               * cscrnhp1
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cscrnhp1
000400*                                                               * cscrnhp1
000500***************************************************************** cscrnhp1
000600                                                                  cscrnhp1
000700***************************************************************** cscrnhp1
000800* CSCRNHP1.CPY                                                  * cscrnhp1
000900*---------------------------------------------------------------* cscrnhp1
001000* Procedure code to populate screen titles                      * cscrnhp1
001100***************************************************************** cscrnhp1
001200     CALL 'SCUSTOMP' USING SCREEN-TITLES.                         cscrnhp1
001300     MOVE SCREEN-TITLE1 TO HEAD1O IN <<SCRN>>.                    cscrnhp1
001400     MOVE SCREEN-TITLE2 TO HEAD2O IN <<SCRN>>.                    cscrnhp1
001500                                                                  cscrnhp1
001600* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cscrnhp1
