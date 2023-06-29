000100***************************************************************** cbentry
000200*                                                               * cbentry
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbentry
000400*                                                               * cbentry
000500***************************************************************** cbentry
000600                                                                  cbentry
000700****************************************************************  cbentry
000800* CBENTRY.CPY (CICS version)                                   *  cbentry
000900*--------------------------------------------------------------*  cbentry
001000* This copybook is used as the ENTRY point to data access      *  cbentry
001100* routines. It is used as there are different requirements     *  cbentry
001200* depending on how the module is called (EXEC CICS LINK or     *  cbentry
001300* COBOL CALL statement).                                       *  cbentry
001400****************************************************************  cbentry
001500 PROCEDURE DIVISION USING DFHCOMMAREA.                            cbentry
001600                                                                  cbentry
001700* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbentry
