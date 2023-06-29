000100***************************************************************** cbankx11
000200*                                                               * cbankx11
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbankx11
000400*                                                               * cbankx11
000500***************************************************************** cbankx11
000600                                                                  cbankx11
000700***************************************************************** cbankx11
000800* CBANKX11.CPY (CICS Version)                                   * cbankx11
000900*---------------------------------------------------------------* cbankx11
001000* This copybook is used to provide an common means of calling   * cbankx11
001100* data access module DBANK11P so that the that module using     * cbankx11
001200* this copy book is insensitive to it environment.              * cbankx11
001300* There are different versions for CICS, IMS and INET.          * cbankx11
001400***************************************************************** cbankx11
001500* by default use CICS commands to call the module                 cbankx11
001600     EXEC CICS LINK PROGRAM('DBANK11P')                           cbankx11
001700                    COMMAREA(CD11-DATA)                           cbankx11
001800                    LENGTH(LENGTH OF CD11-DATA)                   cbankx11
001900     END-EXEC                                                     cbankx11
002000*    CALL 'DBANK11P' USING CD11-DATA                              cbankx11
002100                                                                  cbankx11
002200* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbankx11
