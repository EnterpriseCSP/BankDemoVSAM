000100***************************************************************** cbankx07
000200*                                                               * cbankx07
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbankx07
000400*                                                               * cbankx07
000500***************************************************************** cbankx07
000600                                                                  cbankx07
000700***************************************************************** cbankx07
000800* CBANKX07.CPY (CICS Version)                                   * cbankx07
000900*---------------------------------------------------------------* cbankx07
001000* This copybook is used to provide an common means of calling   * cbankx07
001100* data access module DBANK07P so that the that module using     * cbankx07
001200* this copy book is insensitive to it environment.              * cbankx07
001300* There are different versions for CICS, IMS and INET.          * cbankx07
001400***************************************************************** cbankx07
001500* by default use CICS commands to call the module                 cbankx07
001600     EXEC CICS LINK PROGRAM('DBANK07P')                           cbankx07
001700                    COMMAREA(CD07-DATA)                           cbankx07
001800                    LENGTH(LENGTH OF CD07-DATA)                   cbankx07
001900     END-EXEC                                                     cbankx07
002000*    CALL 'DBANK07P' USING CD07-DATA                              cbankx07
002100                                                                  cbankx07
002200* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbankx07
