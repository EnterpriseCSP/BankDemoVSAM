000100***************************************************************** ccashx02
000200*                                                               * ccashx02
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * ccashx02
000400*                                                               * ccashx02
000500***************************************************************** ccashx02
000600                                                                  ccashx02
000700***************************************************************** ccashx02
000800* CCASHX02.CPY (CICS Version)                                   * ccashx02
000900*---------------------------------------------------------------* ccashx02
001000* This copybook is used to provide an common means of calling   * ccashx02
001100* data access module DCASH02P so that the that module using     * ccashx02
001200* this copy book is insensitive to it environment.              * ccashx02
001300* There are different versions for CICS, IMS and INET.          * ccashx02
001400***************************************************************** ccashx02
001500* by default use CICS commands to call the module                 ccashx02
001600     EXEC CICS LINK PROGRAM('DCASH02P')                           ccashx02
001700                    COMMAREA(CD02-DATA)                           ccashx02
001800                    LENGTH(LENGTH OF CD02-DATA)                   ccashx02
001900     END-EXEC                                                     ccashx02
002000*    CALL 'DCASH02P' USING CD02-DATA                              ccashx02
002100                                                                  ccashx02
002200* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     ccashx02
