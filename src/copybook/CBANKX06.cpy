000100***************************************************************** cbankx06
000200*                                                               * cbankx06
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbankx06
000400*                                                               * cbankx06
000500***************************************************************** cbankx06
000600                                                                  cbankx06
000700***************************************************************** cbankx06
000800* CBANKX06.CPY (CICS Version)                                   * cbankx06
000900*---------------------------------------------------------------* cbankx06
001000* This copybook is used to provide an common means of calling   * cbankx06
001100* data access module DBANK06P so that the that module using     * cbankx06
001200* this copy book is insensitive to it environment.              * cbankx06
001300* There are different versions for CICS, IMS and INET.          * cbankx06
001400***************************************************************** cbankx06
001500* by default use CICS commands to call the module                 cbankx06
001600     EXEC CICS LINK PROGRAM('DBANK06P')                           cbankx06
001700                    COMMAREA(CD06-DATA)                           cbankx06
001800                    LENGTH(LENGTH OF CD06-DATA)                   cbankx06
001900     END-EXEC                                                     cbankx06
002000*    CALL 'DBANK06P' USING CD06-DATA                              cbankx06
002100                                                                  cbankx06
002200* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbankx06
