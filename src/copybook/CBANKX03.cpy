000100***************************************************************** cbankx03
000200*                                                               * cbankx03
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbankx03
000400*                                                               * cbankx03
000500***************************************************************** cbankx03
000600                                                                  cbankx03
000700***************************************************************** cbankx03
000800* CBANKX03.CPY (CICS Version)                                   * cbankx03
000900*---------------------------------------------------------------* cbankx03
001000* This copybook is used to provide an common means of calling   * cbankx03
001100* data access module DBANK03P so that the that module using     * cbankx03
001200* this copy book is insensitive to it environment.              * cbankx03
001300* There are different versions for CICS, IMS and INET.          * cbankx03
001400***************************************************************** cbankx03
001500* by default use CICS commands to call the module                 cbankx03
001600     EXEC CICS LINK PROGRAM('DBANK03P')                           cbankx03
001700                    COMMAREA(CD03-DATA)                           cbankx03
001800                    LENGTH(LENGTH OF CD03-DATA)                   cbankx03
001900     END-EXEC                                                     cbankx03
002000*    CALL 'DBANK03P' USING CD01-DATA                              cbankx03
002100                                                                  cbankx03
002200* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbankx03
