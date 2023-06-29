000100***************************************************************** cbankx09
000200*                                                               * cbankx09
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbankx09
000400*                                                               * cbankx09
000500***************************************************************** cbankx09
000600                                                                  cbankx09
000700***************************************************************** cbankx09
000800* CBANKX09.CPY (CICS Version)                                   * cbankx09
000900*---------------------------------------------------------------* cbankx09
001000* This copybook is used to provide an common means of calling   * cbankx09
001100* data access module DBANK09P so that the that module using     * cbankx09
001200* this copy book is insensitive to it environment.              * cbankx09
001300* There are different versions for CICS, IMS and INET.          * cbankx09
001400***************************************************************** cbankx09
001500* by default use CICS commands to call the module                 cbankx09
001600     EXEC CICS LINK PROGRAM('DBANK09P')                           cbankx09
001700                    COMMAREA(CD09-DATA)                           cbankx09
001800                    LENGTH(LENGTH OF CD09-DATA)                   cbankx09
001900     END-EXEC                                                     cbankx09
002000*    CALL 'DBANK09P' USING CD09-DATA                              cbankx09
002100                                                                  cbankx09
002200* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbankx09
