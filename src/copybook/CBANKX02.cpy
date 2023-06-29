000100***************************************************************** cbankx02
000200*                                                               * cbankx02
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbankx02
000400*                                                               * cbankx02
000500***************************************************************** cbankx02
000600                                                                  cbankx02
000700***************************************************************** cbankx02
000800* CBANKX02.CPY (CICS Version)                                   * cbankx02
000900*---------------------------------------------------------------* cbankx02
001000* This copybook is used to provide an common means of calling   * cbankx02
001100* data access module DBANK02P so that the that module using     * cbankx02
001200* this copy book is insensitive to it environment.              * cbankx02
001300* There are different versions for CICS, IMS and INET.          * cbankx02
001400***************************************************************** cbankx02
001500* by default use CICS commands to call the module                 cbankx02
001600     EXEC CICS LINK PROGRAM('DBANK02P')                           cbankx02
001700                    COMMAREA(CD02-DATA)                           cbankx02
001800                    LENGTH(LENGTH OF CD02-DATA)                   cbankx02
001900     END-EXEC                                                     cbankx02
002000*    CALL 'DBANK02P' USING CD02-DATA                              cbankx02
002100                                                                  cbankx02
002200* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbankx02
