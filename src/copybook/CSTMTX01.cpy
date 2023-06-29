000100***************************************************************** cstmtx01
000200*                                                               * cstmtx01
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * cstmtx01
000400*                                                               * cstmtx01
000500***************************************************************** cstmtx01
000600                                                                  cstmtx01
000700***************************************************************** cstmtx01
000800* CSTMTX01.CPY (CICS Version)                                   * cstmtx01
000900*---------------------------------------------------------------* cstmtx01
001000* This copybook is used to provide an common means of calling   * cstmtx01
001100* data module SSTMT01P so that the that module using            * cstmtx01
001200* this copy book is insensitive to it environment.              * cstmtx01
001300* There are different versions for CICS, IMS and INET.          * cstmtx01
001400***************************************************************** cstmtx01
001500* by default use CICS commands to call the module                 cstmtx01
001600     EXEC CICS LINK PROGRAM('SSTMT01P')                           cstmtx01
001700                    COMMAREA(CSTMTD01-DATA)                       cstmtx01
001800                    LENGTH(LENGTH OF CSTMTD01-DATA)               cstmtx01
001900     END-EXEC                                                     cstmtx01
002000*    CALL 'SSTMT01P' USING CPRT-DATA                              cstmtx01
002100                                                                  cstmtx01
002200* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     cstmtx01
