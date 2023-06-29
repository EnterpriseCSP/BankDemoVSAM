000100***************************************************************** ciofuncs
000200*                                                               * ciofuncs
000300*  Copyright(C) 1998-2010 Micro Focus. All Rights Reserved.     * ciofuncs
000400*                                                               * ciofuncs
000500***************************************************************** ciofuncs
000600                                                                  ciofuncs
000700***************************************************************** ciofuncs
000800* CIOFUNCS.CPY                                                  * ciofuncs
000900*---------------------------------------------------------------* ciofuncs
001000* I/O Request definitions (request functions and status codes)  * ciofuncs
001100***************************************************************** ciofuncs
001200   05  IO-REQUEST-AREAS.                                          ciofuncs
001300     10  IO-REQUEST-FUNCTION                 PIC X(8).            ciofuncs
001400       88  IO-REQUEST-FUNCTION-OPEN          VALUE 'OPEN    '.    ciofuncs
001500       88  IO-REQUEST-FUNCTION-READ          VALUE 'READ    '.    ciofuncs
001600       88  IO-REQUEST-FUNCTION-CLOSE         VALUE 'CLOSE   '.    ciofuncs
001700     10  IO-REQUEST-STATUS                   PIC X(8).            ciofuncs
001800       88  IO-REQUEST-STATUS-OK              VALUE 'OK      '.    ciofuncs
001900       88  IO-REQUEST-STATUS-EOF             VALUE 'EOF     '.    ciofuncs
002000       88  IO-REQUEST-STATUS-ERROR           VALUE 'ERROR   '.    ciofuncs
002100                                                                  ciofuncs
002200* $ Version 5.99c sequenced on Wednesday 3 Mar 2011 at 1:00pm     ciofuncs
