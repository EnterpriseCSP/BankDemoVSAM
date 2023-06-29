000100***************************************************************** cbankx04
000200*                                                               * cbankx04
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbankx04
000400*                                                               * cbankx04
000500***************************************************************** cbankx04
000600                                                                  cbankx04
000700***************************************************************** cbankx04
000800* CBANKX04.CPY (CICS Version)                                   * cbankx04
000900*---------------------------------------------------------------* cbankx04
001000* This copybook is used to provide an common means of calling   * cbankx04
001100* data access module DBANK04P so that the that module using     * cbankx04
001200* this copy book is insensitive to it environment.              * cbankx04
001300* There are different versions for CICS, IMS and INET.          * cbankx04
001400***************************************************************** cbankx04
001500* by default use CICS commands to call the module                 cbankx04
001600     EXEC CICS LINK PROGRAM('DBANK04P')                           cbankx04
001700                    COMMAREA(CD04-DATA)                           cbankx04
001800                    LENGTH(LENGTH OF CD04-DATA)                   cbankx04
001900     END-EXEC                                                     cbankx04
002000*    CALL 'DBANK04P' USING CD04-DATA                              cbankx04
002100                                                                  cbankx04
002200* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbankx04
