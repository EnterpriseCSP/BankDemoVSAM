000100***************************************************************** cbankx05
000200*                                                               * cbankx05
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cbankx05
000400*                                                               * cbankx05
000500***************************************************************** cbankx05
000600                                                                  cbankx05
000700***************************************************************** cbankx05
000800* CBANKX05.CPY (CICS Version)                                   * cbankx05
000900*---------------------------------------------------------------* cbankx05
001000* This copybook is used to provide an common means of calling   * cbankx05
001100* data access module DBANK05P so that the that module using     * cbankx05
001200* this copy book is insensitive to it environment.              * cbankx05
001300* There are different versions for CICS, IMS and INET.          * cbankx05
001400***************************************************************** cbankx05
001500* by default use CICS commands to call the module                 cbankx05
001600     EXEC CICS LINK PROGRAM('DBANK05P')                           cbankx05
001700                    COMMAREA(CD05-DATA)                           cbankx05
001800                    LENGTH(LENGTH OF CD05-DATA)                   cbankx05
001900     END-EXEC                                                     cbankx05
002000*    CALL 'DBANK05P' USING CD05-DATA                              cbankx05
002100                                                                  cbankx05
002200* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cbankx05
