000100***************************************************************** cbreturn
000200*                                                               * cbreturn
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbreturn
000400*                                                               * cbreturn
000500***************************************************************** cbreturn
000600                                                                  cbreturn
000700****************************************************************  cbreturn
000800* CBRETURN.CPY (CICS Version)                                  *  cbreturn
000900*--------------------------------------------------------------*  cbreturn
001000* This copybook is used as the RETURN point from data access   *  cbreturn
001100* routines. It is used as there are different requirements     *  cbreturn
001200* depending on how the module is called (EXEC CICS LINK or     *  cbreturn
001300* COBOL CALL statement).                                       *  cbreturn
001400****************************************************************  cbreturn
001500     EXEC CICS RETURN                                             cbreturn
001600     END-EXEC.                                                    cbreturn
001700     GOBACK.                                                      cbreturn
001800                                                                  cbreturn
001900* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbreturn
