000100***************************************************************** ccashx01
000200*                                                               * ccashx01
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * ccashx01
000400*                                                               * ccashx01
000500***************************************************************** ccashx01
000600                                                                  ccashx01
000700***************************************************************** ccashx01
000800* CCASHX01.CPY (CICS Version)                                   * ccashx01
000900*---------------------------------------------------------------* ccashx01
001000* This copybook is used to provide an common means of calling   * ccashx01
001100* data access module DCASH01P so that the that module using     * ccashx01
001200* this copy book is insensitive to it environment.              * ccashx01
001300* There are different versions for CICS, IMS and INET.          * ccashx01
001400***************************************************************** ccashx01
001500* by default use CICS commands to call the module                 ccashx01
001600     EXEC CICS LINK PROGRAM('DCASH01P')                           ccashx01
001700                    COMMAREA(CD01-DATA)                           ccashx01
001800                    LENGTH(LENGTH OF CD01-DATA)                   ccashx01
001900     END-EXEC                                                     ccashx01
002000*    CALL 'DCASH01P' USING CD01-DATA                              ccashx01
002100                                                                  ccashx01
002200* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     ccashx01
