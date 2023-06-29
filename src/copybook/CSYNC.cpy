000100***************************************************************** csync
000200*                                                               * csync
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * csync
000400*                                                               * csync
000500***************************************************************** csync
000600                                                                  csync
000700***************************************************************** csync
000800* CSYNC.CPY (CICS Version)                                      * csync
000900*---------------------------------------------------------------* csync
001000* This copybook is used to call create a SYNC point so that we  * csync
001100* know where we are in a database etc.                          * csync
001200* There are different versions for CICS, IMS and INET.          * csync
001300***************************************************************** csync
001400     EXEC CICS SYNCPOINT                                          csync
001500     END-EXEC.                                                    csync
001600                                                                  csync
001700* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     csync
