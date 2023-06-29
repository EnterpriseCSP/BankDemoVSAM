000100***************************************************************** cversnp1
000200*                                                               * cversnp1
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cversnp1
000400*                                                               * cversnp1
000500***************************************************************** cversnp1
000600                                                                  cversnp1
000700***************************************************************** cversnp1
000800* CVERSNP1.CPY                                                  * cversnp1
000900*---------------------------------------------------------------* cversnp1
001000* Procedure code to populate version                            * cversnp1
001100***************************************************************** cversnp1
001200     CALL 'SVERSONP' USING VERSION.                               cversnp1
001300     MOVE VERSION TO VERO IN <<SCRN>>.                            cversnp1
001400                                                                  cversnp1
001500* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cversnp1
