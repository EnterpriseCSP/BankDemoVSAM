000100***************************************************************** cbankx08
000200*                                                               * cbankx08
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbankx08
000400*                                                               * cbankx08
000500***************************************************************** cbankx08
000600                                                                  cbankx08
000700***************************************************************** cbankx08
000800* CBANKX08.CPY (CICS Version)                                   * cbankx08
000900*---------------------------------------------------------------* cbankx08
001000* This copybook is used to provide an common means of calling   * cbankx08
001100* data access module DBANK03P so that the that module using     * cbankx08
001200* this copy book is insensitive to it environment.              * cbankx08
001300* There are different versions for CICS, IMS and INET.          * cbankx08
001400***************************************************************** cbankx08
001500* by default use CICS commands to call the module                 cbankx08
001600     EXEC CICS LINK PROGRAM('DBANK08P')                           cbankx08
001700                    COMMAREA(CD08-DATA)                           cbankx08
001800                    LENGTH(LENGTH OF CD08-DATA)                   cbankx08
001900     END-EXEC                                                     cbankx08
002000*    CALL 'DBANK08P' USING CD08-DATA                              cbankx08
002100                                                                  cbankx08
002200* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbankx08
