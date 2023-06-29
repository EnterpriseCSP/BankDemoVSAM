000100***************************************************************** chelpx01
000200*                                                               * chelpx01
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * chelpx01
000400*                                                               * chelpx01
000500***************************************************************** chelpx01
000600                                                                  chelpx01
000700***************************************************************** chelpx01
000800* CHELPX01.CPY (CICS Version)                                   * chelpx01
000900*---------------------------------------------------------------* chelpx01
001000* This copybook is used to provide an common means of calling   * chelpx01
001100* data access module DHELP01P so that the that module using     * chelpx01
001200* this copy book is insensitive to it environment.              * chelpx01
001300* There are different versions for CICS, IMS and INET.          * chelpx01
001400***************************************************************** chelpx01
001500* by default use CICS commands to call the module                 chelpx01
001600     EXEC CICS LINK PROGRAM('DHELP01P')                           chelpx01
001700                    COMMAREA(HELP01-DATA)                         chelpx01
001800                    LENGTH(LENGTH OF HELP01-DATA)                 chelpx01
001900     END-EXEC                                                     chelpx01
002000*    CALL 'DHELP01P' USING HELP01-DATA                            chelpx01
002100                                                                  chelpx01
002200* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     chelpx01
