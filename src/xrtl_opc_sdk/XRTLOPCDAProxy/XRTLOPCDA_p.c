
#pragma warning( disable: 4049 )  /* more than 64k source lines */

/* this ALWAYS GENERATED file contains the proxy stub code */


 /* File created by MIDL compiler version 6.00.0347 */
/* at Fri Nov 01 18:23:40 2002
 */
/* Compiler settings for XRTLOPCDA.idl:
    Oicf, W1, Zp8, env=Win32 (32b run)
    protocol : dce , ms_ext, c_ext
    error checks: allocation ref bounds_check enum stub_data 
    VC __declspec() decoration level: 
         __declspec(uuid()), __declspec(selectany), __declspec(novtable)
         DECLSPEC_UUID(), MIDL_INTERFACE()
*/
//@@MIDL_FILE_HEADING(  )

#if !defined(_M_IA64) && !defined(_M_AMD64)
#define USE_STUBLESS_PROXY


/* verify that the <rpcproxy.h> version is high enough to compile this file*/
#ifndef __REDQ_RPCPROXY_H_VERSION__
#define __REQUIRED_RPCPROXY_H_VERSION__ 440
#endif


#include "rpcproxy.h"
#ifndef __RPCPROXY_H_VERSION__
#error this stub requires an updated version of <rpcproxy.h>
#endif // __RPCPROXY_H_VERSION__


#include "XRTLOPCDA.h"

#define TYPE_FORMAT_STRING_SIZE   1299                              
#define PROC_FORMAT_STRING_SIZE   1211                              
#define TRANSMIT_AS_TABLE_SIZE    0            
#define WIRE_MARSHAL_TABLE_SIZE   1            

typedef struct _MIDL_TYPE_FORMAT_STRING
    {
    short          Pad;
    unsigned char  Format[ TYPE_FORMAT_STRING_SIZE ];
    } MIDL_TYPE_FORMAT_STRING;

typedef struct _MIDL_PROC_FORMAT_STRING
    {
    short          Pad;
    unsigned char  Format[ PROC_FORMAT_STRING_SIZE ];
    } MIDL_PROC_FORMAT_STRING;


static RPC_SYNTAX_IDENTIFIER  _RpcTransferSyntax = 
{{0x8A885D04,0x1CEB,0x11C9,{0x9F,0xE8,0x08,0x00,0x2B,0x10,0x48,0x60}},{2,0}};


extern const MIDL_TYPE_FORMAT_STRING __MIDL_TypeFormatString;
extern const MIDL_PROC_FORMAT_STRING __MIDL_ProcFormatString;


extern const MIDL_STUB_DESC Object_StubDesc;


extern const MIDL_SERVER_INFO IXRTLOPCDA20Server_ServerInfo;
extern const MIDL_STUBLESS_PROXY_INFO IXRTLOPCDA20Server_ProxyInfo;


extern const MIDL_STUB_DESC Object_StubDesc;


extern const MIDL_SERVER_INFO IXRTLOPCDA20Group_ServerInfo;
extern const MIDL_STUBLESS_PROXY_INFO IXRTLOPCDA20Group_ProxyInfo;


extern const MIDL_STUB_DESC Object_StubDesc;


extern const MIDL_SERVER_INFO IXRTLOPCDANameSpace_ServerInfo;
extern const MIDL_STUBLESS_PROXY_INFO IXRTLOPCDANameSpace_ProxyInfo;


extern const MIDL_STUB_DESC Object_StubDesc;


extern const MIDL_SERVER_INFO IXRTLOPCDANameSpaceItem_ServerInfo;
extern const MIDL_STUBLESS_PROXY_INFO IXRTLOPCDANameSpaceItem_ProxyInfo;


extern const MIDL_STUB_DESC Object_StubDesc;


extern const MIDL_SERVER_INFO IXRTLOPCDADataSource_ServerInfo;
extern const MIDL_STUBLESS_PROXY_INFO IXRTLOPCDADataSource_ProxyInfo;


extern const USER_MARSHAL_ROUTINE_QUADRUPLE UserMarshalRoutines[ WIRE_MARSHAL_TABLE_SIZE ];

#if !defined(__RPC_WIN32__)
#error  Invalid build platform for this stub.
#endif

#if !(TARGET_IS_NT40_OR_LATER)
#error You need a Windows NT 4.0 or later to run this stub because it uses these features:
#error   -Oif or -Oicf, [wire_marshal] or [user_marshal] attribute.
#error However, your C/C++ compilation flags indicate you intend to run this app on earlier systems.
#error This app will die there with the RPC_X_WRONG_STUB_VERSION error.
#endif


static const MIDL_PROC_FORMAT_STRING __MIDL_ProcFormatString =
    {
        0,
        {

	/* Procedure GetNameSpace */

			0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/*  2 */	NdrFcLong( 0x0 ),	/* 0 */
/*  6 */	NdrFcShort( 0x3 ),	/* 3 */
/*  8 */	NdrFcShort( 0xc ),	/* x86 Stack size/offset = 12 */
/* 10 */	NdrFcShort( 0x0 ),	/* 0 */
/* 12 */	NdrFcShort( 0x8 ),	/* 8 */
/* 14 */	0x5,		/* Oi2 Flags:  srv must size, has return, */
			0x2,		/* 2 */

	/* Parameter ppNameSpace */

/* 16 */	NdrFcShort( 0x13 ),	/* Flags:  must size, must free, out, */
/* 18 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 20 */	NdrFcShort( 0x2 ),	/* Type Offset=2 */

	/* Return value */

/* 22 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 24 */	NdrFcShort( 0x8 ),	/* x86 Stack size/offset = 8 */
/* 26 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure SetNameSpace */

/* 28 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 30 */	NdrFcLong( 0x0 ),	/* 0 */
/* 34 */	NdrFcShort( 0x4 ),	/* 4 */
/* 36 */	NdrFcShort( 0xc ),	/* x86 Stack size/offset = 12 */
/* 38 */	NdrFcShort( 0x0 ),	/* 0 */
/* 40 */	NdrFcShort( 0x8 ),	/* 8 */
/* 42 */	0x6,		/* Oi2 Flags:  clt must size, has return, */
			0x2,		/* 2 */

	/* Parameter pNameSpace */

/* 44 */	NdrFcShort( 0xb ),	/* Flags:  must size, must free, in, */
/* 46 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 48 */	NdrFcShort( 0x6 ),	/* Type Offset=6 */

	/* Return value */

/* 50 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 52 */	NdrFcShort( 0x8 ),	/* x86 Stack size/offset = 8 */
/* 54 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure GetDataSource */

/* 56 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 58 */	NdrFcLong( 0x0 ),	/* 0 */
/* 62 */	NdrFcShort( 0x5 ),	/* 5 */
/* 64 */	NdrFcShort( 0x10 ),	/* x86 Stack size/offset = 16 */
/* 66 */	NdrFcShort( 0x6 ),	/* 6 */
/* 68 */	NdrFcShort( 0x8 ),	/* 8 */
/* 70 */	0x5,		/* Oi2 Flags:  srv must size, has return, */
			0x3,		/* 3 */

	/* Parameter dwSource */

/* 72 */	NdrFcShort( 0x48 ),	/* Flags:  in, base type, */
/* 74 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 76 */	0xd,		/* FC_ENUM16 */
			0x0,		/* 0 */

	/* Parameter ppDataSource */

/* 78 */	NdrFcShort( 0x13 ),	/* Flags:  must size, must free, out, */
/* 80 */	NdrFcShort( 0x8 ),	/* x86 Stack size/offset = 8 */
/* 82 */	NdrFcShort( 0x18 ),	/* Type Offset=24 */

	/* Return value */

/* 84 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 86 */	NdrFcShort( 0xc ),	/* x86 Stack size/offset = 12 */
/* 88 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure SetDataSource */

/* 90 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 92 */	NdrFcLong( 0x0 ),	/* 0 */
/* 96 */	NdrFcShort( 0x6 ),	/* 6 */
/* 98 */	NdrFcShort( 0x10 ),	/* x86 Stack size/offset = 16 */
/* 100 */	NdrFcShort( 0x6 ),	/* 6 */
/* 102 */	NdrFcShort( 0x8 ),	/* 8 */
/* 104 */	0x6,		/* Oi2 Flags:  clt must size, has return, */
			0x3,		/* 3 */

	/* Parameter dwSource */

/* 106 */	NdrFcShort( 0x48 ),	/* Flags:  in, base type, */
/* 108 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 110 */	0xd,		/* FC_ENUM16 */
			0x0,		/* 0 */

	/* Parameter pDataSource */

/* 112 */	NdrFcShort( 0xb ),	/* Flags:  must size, must free, in, */
/* 114 */	NdrFcShort( 0x8 ),	/* x86 Stack size/offset = 8 */
/* 116 */	NdrFcShort( 0x1c ),	/* Type Offset=28 */

	/* Return value */

/* 118 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 120 */	NdrFcShort( 0xc ),	/* x86 Stack size/offset = 12 */
/* 122 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure SetDeleted */

/* 124 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 126 */	NdrFcLong( 0x0 ),	/* 0 */
/* 130 */	NdrFcShort( 0x3 ),	/* 3 */
/* 132 */	NdrFcShort( 0xc ),	/* x86 Stack size/offset = 12 */
/* 134 */	NdrFcShort( 0x8 ),	/* 8 */
/* 136 */	NdrFcShort( 0x8 ),	/* 8 */
/* 138 */	0x4,		/* Oi2 Flags:  has return, */
			0x2,		/* 2 */

	/* Parameter bFlag */

/* 140 */	NdrFcShort( 0x48 ),	/* Flags:  in, base type, */
/* 142 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 144 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Return value */

/* 146 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 148 */	NdrFcShort( 0x8 ),	/* x86 Stack size/offset = 8 */
/* 150 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure GetDeleted */

/* 152 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 154 */	NdrFcLong( 0x0 ),	/* 0 */
/* 158 */	NdrFcShort( 0x4 ),	/* 4 */
/* 160 */	NdrFcShort( 0xc ),	/* x86 Stack size/offset = 12 */
/* 162 */	NdrFcShort( 0x0 ),	/* 0 */
/* 164 */	NdrFcShort( 0x24 ),	/* 36 */
/* 166 */	0x4,		/* Oi2 Flags:  has return, */
			0x2,		/* 2 */

	/* Parameter pbFlag */

/* 168 */	NdrFcShort( 0x2150 ),	/* Flags:  out, base type, simple ref, srv alloc size=8 */
/* 170 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 172 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Return value */

/* 174 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 176 */	NdrFcShort( 0x8 ),	/* x86 Stack size/offset = 8 */
/* 178 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure GetOrganization */

/* 180 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 182 */	NdrFcLong( 0x0 ),	/* 0 */
/* 186 */	NdrFcShort( 0x3 ),	/* 3 */
/* 188 */	NdrFcShort( 0xc ),	/* x86 Stack size/offset = 12 */
/* 190 */	NdrFcShort( 0x0 ),	/* 0 */
/* 192 */	NdrFcShort( 0x22 ),	/* 34 */
/* 194 */	0x4,		/* Oi2 Flags:  has return, */
			0x2,		/* 2 */

	/* Parameter pNameSpaceType */

/* 196 */	NdrFcShort( 0x2010 ),	/* Flags:  out, srv alloc size=8 */
/* 198 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 200 */	NdrFcShort( 0x32 ),	/* Type Offset=50 */

	/* Return value */

/* 202 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 204 */	NdrFcShort( 0x8 ),	/* x86 Stack size/offset = 8 */
/* 206 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure SetOrganization */

/* 208 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 210 */	NdrFcLong( 0x0 ),	/* 0 */
/* 214 */	NdrFcShort( 0x4 ),	/* 4 */
/* 216 */	NdrFcShort( 0xc ),	/* x86 Stack size/offset = 12 */
/* 218 */	NdrFcShort( 0x6 ),	/* 6 */
/* 220 */	NdrFcShort( 0x8 ),	/* 8 */
/* 222 */	0x4,		/* Oi2 Flags:  has return, */
			0x2,		/* 2 */

	/* Parameter NameSpaceType */

/* 224 */	NdrFcShort( 0x48 ),	/* Flags:  in, base type, */
/* 226 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 228 */	0xd,		/* FC_ENUM16 */
			0x0,		/* 0 */

	/* Return value */

/* 230 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 232 */	NdrFcShort( 0x8 ),	/* x86 Stack size/offset = 8 */
/* 234 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure CheckPath */

/* 236 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 238 */	NdrFcLong( 0x0 ),	/* 0 */
/* 242 */	NdrFcShort( 0x5 ),	/* 5 */
/* 244 */	NdrFcShort( 0x18 ),	/* x86 Stack size/offset = 24 */
/* 246 */	NdrFcShort( 0x0 ),	/* 0 */
/* 248 */	NdrFcShort( 0x8 ),	/* 8 */
/* 250 */	0x6,		/* Oi2 Flags:  clt must size, has return, */
			0x2,		/* 2 */

	/* Parameter vItemIDChunks */

/* 252 */	NdrFcShort( 0x8b ),	/* Flags:  must size, must free, in, by val, */
/* 254 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 256 */	NdrFcShort( 0x416 ),	/* Type Offset=1046 */

	/* Return value */

/* 258 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 260 */	NdrFcShort( 0x14 ),	/* x86 Stack size/offset = 20 */
/* 262 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure AddItem */

/* 264 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 266 */	NdrFcLong( 0x0 ),	/* 0 */
/* 270 */	NdrFcShort( 0x6 ),	/* 6 */
/* 272 */	NdrFcShort( 0x24 ),	/* x86 Stack size/offset = 36 */
/* 274 */	NdrFcShort( 0x6 ),	/* 6 */
/* 276 */	NdrFcShort( 0x8 ),	/* 8 */
/* 278 */	0x7,		/* Oi2 Flags:  srv must size, clt must size, has return, */
			0x5,		/* 5 */

	/* Parameter vItemIDChunks */

/* 280 */	NdrFcShort( 0x8b ),	/* Flags:  must size, must free, in, by val, */
/* 282 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 284 */	NdrFcShort( 0x416 ),	/* Type Offset=1046 */

	/* Parameter pszDescription */

/* 286 */	NdrFcShort( 0x10b ),	/* Flags:  must size, must free, in, simple ref, */
/* 288 */	NdrFcShort( 0x14 ),	/* x86 Stack size/offset = 20 */
/* 290 */	NdrFcShort( 0x422 ),	/* Type Offset=1058 */

	/* Parameter vtDataType */

/* 292 */	NdrFcShort( 0x48 ),	/* Flags:  in, base type, */
/* 294 */	NdrFcShort( 0x18 ),	/* x86 Stack size/offset = 24 */
/* 296 */	0x6,		/* FC_SHORT */
			0x0,		/* 0 */

	/* Parameter ppItem */

/* 298 */	NdrFcShort( 0x13 ),	/* Flags:  must size, must free, out, */
/* 300 */	NdrFcShort( 0x1c ),	/* x86 Stack size/offset = 28 */
/* 302 */	NdrFcShort( 0x424 ),	/* Type Offset=1060 */

	/* Return value */

/* 304 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 306 */	NdrFcShort( 0x20 ),	/* x86 Stack size/offset = 32 */
/* 308 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure GetItem */

/* 310 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 312 */	NdrFcLong( 0x0 ),	/* 0 */
/* 316 */	NdrFcShort( 0x7 ),	/* 7 */
/* 318 */	NdrFcShort( 0x1c ),	/* x86 Stack size/offset = 28 */
/* 320 */	NdrFcShort( 0x0 ),	/* 0 */
/* 322 */	NdrFcShort( 0x8 ),	/* 8 */
/* 324 */	0x7,		/* Oi2 Flags:  srv must size, clt must size, has return, */
			0x3,		/* 3 */

	/* Parameter vItemIDChunks */

/* 326 */	NdrFcShort( 0x8b ),	/* Flags:  must size, must free, in, by val, */
/* 328 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 330 */	NdrFcShort( 0x416 ),	/* Type Offset=1046 */

	/* Parameter ppItem */

/* 332 */	NdrFcShort( 0x13 ),	/* Flags:  must size, must free, out, */
/* 334 */	NdrFcShort( 0x14 ),	/* x86 Stack size/offset = 20 */
/* 336 */	NdrFcShort( 0x424 ),	/* Type Offset=1060 */

	/* Return value */

/* 338 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 340 */	NdrFcShort( 0x18 ),	/* x86 Stack size/offset = 24 */
/* 342 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure HasItem */

/* 344 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 346 */	NdrFcLong( 0x0 ),	/* 0 */
/* 350 */	NdrFcShort( 0x8 ),	/* 8 */
/* 352 */	NdrFcShort( 0x1c ),	/* x86 Stack size/offset = 28 */
/* 354 */	NdrFcShort( 0x0 ),	/* 0 */
/* 356 */	NdrFcShort( 0x24 ),	/* 36 */
/* 358 */	0x6,		/* Oi2 Flags:  clt must size, has return, */
			0x3,		/* 3 */

	/* Parameter vItemIDChunks */

/* 360 */	NdrFcShort( 0x8b ),	/* Flags:  must size, must free, in, by val, */
/* 362 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 364 */	NdrFcShort( 0x416 ),	/* Type Offset=1046 */

	/* Parameter pbResult */

/* 366 */	NdrFcShort( 0x2150 ),	/* Flags:  out, base type, simple ref, srv alloc size=8 */
/* 368 */	NdrFcShort( 0x14 ),	/* x86 Stack size/offset = 20 */
/* 370 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Return value */

/* 372 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 374 */	NdrFcShort( 0x18 ),	/* x86 Stack size/offset = 24 */
/* 376 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure RemoveItem */

/* 378 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 380 */	NdrFcLong( 0x0 ),	/* 0 */
/* 384 */	NdrFcShort( 0x9 ),	/* 9 */
/* 386 */	NdrFcShort( 0x18 ),	/* x86 Stack size/offset = 24 */
/* 388 */	NdrFcShort( 0x0 ),	/* 0 */
/* 390 */	NdrFcShort( 0x8 ),	/* 8 */
/* 392 */	0x6,		/* Oi2 Flags:  clt must size, has return, */
			0x2,		/* 2 */

	/* Parameter vItemIDChunks */

/* 394 */	NdrFcShort( 0x8b ),	/* Flags:  must size, must free, in, by val, */
/* 396 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 398 */	NdrFcShort( 0x416 ),	/* Type Offset=1046 */

	/* Return value */

/* 400 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 402 */	NdrFcShort( 0x14 ),	/* x86 Stack size/offset = 20 */
/* 404 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure Clear */

/* 406 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 408 */	NdrFcLong( 0x0 ),	/* 0 */
/* 412 */	NdrFcShort( 0xa ),	/* 10 */
/* 414 */	NdrFcShort( 0x8 ),	/* x86 Stack size/offset = 8 */
/* 416 */	NdrFcShort( 0x0 ),	/* 0 */
/* 418 */	NdrFcShort( 0x8 ),	/* 8 */
/* 420 */	0x4,		/* Oi2 Flags:  has return, */
			0x1,		/* 1 */

	/* Return value */

/* 422 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 424 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 426 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure CreateItemEnumerator */

/* 428 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 430 */	NdrFcLong( 0x0 ),	/* 0 */
/* 434 */	NdrFcShort( 0xb ),	/* 11 */
/* 436 */	NdrFcShort( 0x2c ),	/* x86 Stack size/offset = 44 */
/* 438 */	NdrFcShort( 0x14 ),	/* 20 */
/* 440 */	NdrFcShort( 0x8 ),	/* 8 */
/* 442 */	0x7,		/* Oi2 Flags:  srv must size, clt must size, has return, */
			0x7,		/* 7 */

	/* Parameter vRootItemIDChunks */

/* 444 */	NdrFcShort( 0x8b ),	/* Flags:  must size, must free, in, by val, */
/* 446 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 448 */	NdrFcShort( 0x416 ),	/* Type Offset=1046 */

	/* Parameter dwBrowseFilterType */

/* 450 */	NdrFcShort( 0x48 ),	/* Flags:  in, base type, */
/* 452 */	NdrFcShort( 0x14 ),	/* x86 Stack size/offset = 20 */
/* 454 */	0xd,		/* FC_ENUM16 */
			0x0,		/* 0 */

	/* Parameter szFilterCriteria */

/* 456 */	NdrFcShort( 0x10b ),	/* Flags:  must size, must free, in, simple ref, */
/* 458 */	NdrFcShort( 0x18 ),	/* x86 Stack size/offset = 24 */
/* 460 */	NdrFcShort( 0x422 ),	/* Type Offset=1058 */

	/* Parameter vtDataTypeFilter */

/* 462 */	NdrFcShort( 0x48 ),	/* Flags:  in, base type, */
/* 464 */	NdrFcShort( 0x1c ),	/* x86 Stack size/offset = 28 */
/* 466 */	0x6,		/* FC_SHORT */
			0x0,		/* 0 */

	/* Parameter dwAccessRightsFilter */

/* 468 */	NdrFcShort( 0x48 ),	/* Flags:  in, base type, */
/* 470 */	NdrFcShort( 0x20 ),	/* x86 Stack size/offset = 32 */
/* 472 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Parameter ppIEnumString */

/* 474 */	NdrFcShort( 0x13 ),	/* Flags:  must size, must free, out, */
/* 476 */	NdrFcShort( 0x24 ),	/* x86 Stack size/offset = 36 */
/* 478 */	NdrFcShort( 0x43a ),	/* Type Offset=1082 */

	/* Return value */

/* 480 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 482 */	NdrFcShort( 0x28 ),	/* x86 Stack size/offset = 40 */
/* 484 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure GetItemID */


	/* Procedure GetItemIDSeparator */

/* 486 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 488 */	NdrFcLong( 0x0 ),	/* 0 */
/* 492 */	NdrFcShort( 0xc ),	/* 12 */
/* 494 */	NdrFcShort( 0xc ),	/* x86 Stack size/offset = 12 */
/* 496 */	NdrFcShort( 0x0 ),	/* 0 */
/* 498 */	NdrFcShort( 0x8 ),	/* 8 */
/* 500 */	0x5,		/* Oi2 Flags:  srv must size, has return, */
			0x2,		/* 2 */

	/* Parameter pszItemID */


	/* Parameter pszItemIDSeparator */

/* 502 */	NdrFcShort( 0x2013 ),	/* Flags:  must size, must free, out, srv alloc size=8 */
/* 504 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 506 */	NdrFcShort( 0x450 ),	/* Type Offset=1104 */

	/* Return value */


	/* Return value */

/* 508 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 510 */	NdrFcShort( 0x8 ),	/* x86 Stack size/offset = 8 */
/* 512 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure SetItemIDSeparator */

/* 514 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 516 */	NdrFcLong( 0x0 ),	/* 0 */
/* 520 */	NdrFcShort( 0xd ),	/* 13 */
/* 522 */	NdrFcShort( 0xc ),	/* x86 Stack size/offset = 12 */
/* 524 */	NdrFcShort( 0x0 ),	/* 0 */
/* 526 */	NdrFcShort( 0x8 ),	/* 8 */
/* 528 */	0x6,		/* Oi2 Flags:  clt must size, has return, */
			0x2,		/* 2 */

	/* Parameter szItemIDSeparator */

/* 530 */	NdrFcShort( 0x10b ),	/* Flags:  must size, must free, in, simple ref, */
/* 532 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 534 */	NdrFcShort( 0x422 ),	/* Type Offset=1058 */

	/* Return value */

/* 536 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 538 */	NdrFcShort( 0x8 ),	/* x86 Stack size/offset = 8 */
/* 540 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure CombineItemID */

/* 542 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 544 */	NdrFcLong( 0x0 ),	/* 0 */
/* 548 */	NdrFcShort( 0xe ),	/* 14 */
/* 550 */	NdrFcShort( 0x1c ),	/* x86 Stack size/offset = 28 */
/* 552 */	NdrFcShort( 0x0 ),	/* 0 */
/* 554 */	NdrFcShort( 0x8 ),	/* 8 */
/* 556 */	0x7,		/* Oi2 Flags:  srv must size, clt must size, has return, */
			0x3,		/* 3 */

	/* Parameter vItemIDChunks */

/* 558 */	NdrFcShort( 0x8b ),	/* Flags:  must size, must free, in, by val, */
/* 560 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 562 */	NdrFcShort( 0x416 ),	/* Type Offset=1046 */

	/* Parameter pszItemID */

/* 564 */	NdrFcShort( 0x2013 ),	/* Flags:  must size, must free, out, srv alloc size=8 */
/* 566 */	NdrFcShort( 0x14 ),	/* x86 Stack size/offset = 20 */
/* 568 */	NdrFcShort( 0x450 ),	/* Type Offset=1104 */

	/* Return value */

/* 570 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 572 */	NdrFcShort( 0x18 ),	/* x86 Stack size/offset = 24 */
/* 574 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure SplitItemID */

/* 576 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 578 */	NdrFcLong( 0x0 ),	/* 0 */
/* 582 */	NdrFcShort( 0xf ),	/* 15 */
/* 584 */	NdrFcShort( 0x10 ),	/* x86 Stack size/offset = 16 */
/* 586 */	NdrFcShort( 0x0 ),	/* 0 */
/* 588 */	NdrFcShort( 0x8 ),	/* 8 */
/* 590 */	0x7,		/* Oi2 Flags:  srv must size, clt must size, has return, */
			0x3,		/* 3 */

	/* Parameter szItemID */

/* 592 */	NdrFcShort( 0x10b ),	/* Flags:  must size, must free, in, simple ref, */
/* 594 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 596 */	NdrFcShort( 0x422 ),	/* Type Offset=1058 */

	/* Parameter pvItemIDChunks */

/* 598 */	NdrFcShort( 0x4113 ),	/* Flags:  must size, must free, out, simple ref, srv alloc size=16 */
/* 600 */	NdrFcShort( 0x8 ),	/* x86 Stack size/offset = 8 */
/* 602 */	NdrFcShort( 0x460 ),	/* Type Offset=1120 */

	/* Return value */

/* 604 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 606 */	NdrFcShort( 0xc ),	/* x86 Stack size/offset = 12 */
/* 608 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure IsLeaf */

/* 610 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 612 */	NdrFcLong( 0x0 ),	/* 0 */
/* 616 */	NdrFcShort( 0x10 ),	/* 16 */
/* 618 */	NdrFcShort( 0x1c ),	/* x86 Stack size/offset = 28 */
/* 620 */	NdrFcShort( 0x0 ),	/* 0 */
/* 622 */	NdrFcShort( 0x24 ),	/* 36 */
/* 624 */	0x6,		/* Oi2 Flags:  clt must size, has return, */
			0x3,		/* 3 */

	/* Parameter vItemIDChunks */

/* 626 */	NdrFcShort( 0x8b ),	/* Flags:  must size, must free, in, by val, */
/* 628 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 630 */	NdrFcShort( 0x416 ),	/* Type Offset=1046 */

	/* Parameter pbResult */

/* 632 */	NdrFcShort( 0x2150 ),	/* Flags:  out, base type, simple ref, srv alloc size=8 */
/* 634 */	NdrFcShort( 0x14 ),	/* x86 Stack size/offset = 20 */
/* 636 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Return value */

/* 638 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 640 */	NdrFcShort( 0x18 ),	/* x86 Stack size/offset = 24 */
/* 642 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure IsBranch */

/* 644 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 646 */	NdrFcLong( 0x0 ),	/* 0 */
/* 650 */	NdrFcShort( 0x11 ),	/* 17 */
/* 652 */	NdrFcShort( 0x1c ),	/* x86 Stack size/offset = 28 */
/* 654 */	NdrFcShort( 0x0 ),	/* 0 */
/* 656 */	NdrFcShort( 0x24 ),	/* 36 */
/* 658 */	0x6,		/* Oi2 Flags:  clt must size, has return, */
			0x3,		/* 3 */

	/* Parameter vItemIDChunks */

/* 660 */	NdrFcShort( 0x8b ),	/* Flags:  must size, must free, in, by val, */
/* 662 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 664 */	NdrFcShort( 0x416 ),	/* Type Offset=1046 */

	/* Parameter pbResult */

/* 666 */	NdrFcShort( 0x2150 ),	/* Flags:  out, base type, simple ref, srv alloc size=8 */
/* 668 */	NdrFcShort( 0x14 ),	/* x86 Stack size/offset = 20 */
/* 670 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Return value */

/* 672 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 674 */	NdrFcShort( 0x18 ),	/* x86 Stack size/offset = 24 */
/* 676 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure QueryAvailableProperties */

/* 678 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 680 */	NdrFcLong( 0x0 ),	/* 0 */
/* 684 */	NdrFcShort( 0x3 ),	/* 3 */
/* 686 */	NdrFcShort( 0x18 ),	/* x86 Stack size/offset = 24 */
/* 688 */	NdrFcShort( 0x0 ),	/* 0 */
/* 690 */	NdrFcShort( 0x24 ),	/* 36 */
/* 692 */	0x5,		/* Oi2 Flags:  srv must size, has return, */
			0x5,		/* 5 */

	/* Parameter pdwCount */

/* 694 */	NdrFcShort( 0x2150 ),	/* Flags:  out, base type, simple ref, srv alloc size=8 */
/* 696 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 698 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Parameter ppPropertyIDs */

/* 700 */	NdrFcShort( 0x2013 ),	/* Flags:  must size, must free, out, srv alloc size=8 */
/* 702 */	NdrFcShort( 0x8 ),	/* x86 Stack size/offset = 8 */
/* 704 */	NdrFcShort( 0x46a ),	/* Type Offset=1130 */

	/* Parameter ppDescriptions */

/* 706 */	NdrFcShort( 0x2013 ),	/* Flags:  must size, must free, out, srv alloc size=8 */
/* 708 */	NdrFcShort( 0xc ),	/* x86 Stack size/offset = 12 */
/* 710 */	NdrFcShort( 0x47c ),	/* Type Offset=1148 */

	/* Parameter ppvtDataTypes */

/* 712 */	NdrFcShort( 0x2013 ),	/* Flags:  must size, must free, out, srv alloc size=8 */
/* 714 */	NdrFcShort( 0x10 ),	/* x86 Stack size/offset = 16 */
/* 716 */	NdrFcShort( 0x4a2 ),	/* Type Offset=1186 */

	/* Return value */

/* 718 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 720 */	NdrFcShort( 0x14 ),	/* x86 Stack size/offset = 20 */
/* 722 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure GetItemProperties */

/* 724 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 726 */	NdrFcLong( 0x0 ),	/* 0 */
/* 730 */	NdrFcShort( 0x4 ),	/* 4 */
/* 732 */	NdrFcShort( 0x18 ),	/* x86 Stack size/offset = 24 */
/* 734 */	NdrFcShort( 0x8 ),	/* 8 */
/* 736 */	NdrFcShort( 0x8 ),	/* 8 */
/* 738 */	0x7,		/* Oi2 Flags:  srv must size, clt must size, has return, */
			0x5,		/* 5 */

	/* Parameter dwCount */

/* 740 */	NdrFcShort( 0x48 ),	/* Flags:  in, base type, */
/* 742 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 744 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Parameter pdwPropertyIDs */

/* 746 */	NdrFcShort( 0x10b ),	/* Flags:  must size, must free, in, simple ref, */
/* 748 */	NdrFcShort( 0x8 ),	/* x86 Stack size/offset = 8 */
/* 750 */	NdrFcShort( 0x4b8 ),	/* Type Offset=1208 */

	/* Parameter ppvData */

/* 752 */	NdrFcShort( 0x2013 ),	/* Flags:  must size, must free, out, srv alloc size=8 */
/* 754 */	NdrFcShort( 0xc ),	/* x86 Stack size/offset = 12 */
/* 756 */	NdrFcShort( 0x4c2 ),	/* Type Offset=1218 */

	/* Parameter ppErrors */

/* 758 */	NdrFcShort( 0x2013 ),	/* Flags:  must size, must free, out, srv alloc size=8 */
/* 760 */	NdrFcShort( 0x10 ),	/* x86 Stack size/offset = 16 */
/* 762 */	NdrFcShort( 0x4dc ),	/* Type Offset=1244 */

	/* Return value */

/* 764 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 766 */	NdrFcShort( 0x14 ),	/* x86 Stack size/offset = 20 */
/* 768 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure LookupItemIDs */

/* 770 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 772 */	NdrFcLong( 0x0 ),	/* 0 */
/* 776 */	NdrFcShort( 0x5 ),	/* 5 */
/* 778 */	NdrFcShort( 0x18 ),	/* x86 Stack size/offset = 24 */
/* 780 */	NdrFcShort( 0x8 ),	/* 8 */
/* 782 */	NdrFcShort( 0x8 ),	/* 8 */
/* 784 */	0x7,		/* Oi2 Flags:  srv must size, clt must size, has return, */
			0x5,		/* 5 */

	/* Parameter dwCount */

/* 786 */	NdrFcShort( 0x48 ),	/* Flags:  in, base type, */
/* 788 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 790 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Parameter pdwPropertyIDs */

/* 792 */	NdrFcShort( 0x10b ),	/* Flags:  must size, must free, in, simple ref, */
/* 794 */	NdrFcShort( 0x8 ),	/* x86 Stack size/offset = 8 */
/* 796 */	NdrFcShort( 0x4b8 ),	/* Type Offset=1208 */

	/* Parameter ppszNewItemIDs */

/* 798 */	NdrFcShort( 0x2013 ),	/* Flags:  must size, must free, out, srv alloc size=8 */
/* 800 */	NdrFcShort( 0xc ),	/* x86 Stack size/offset = 12 */
/* 802 */	NdrFcShort( 0x4e4 ),	/* Type Offset=1252 */

	/* Parameter ppErrors */

/* 804 */	NdrFcShort( 0x2013 ),	/* Flags:  must size, must free, out, srv alloc size=8 */
/* 806 */	NdrFcShort( 0x10 ),	/* x86 Stack size/offset = 16 */
/* 808 */	NdrFcShort( 0x4dc ),	/* Type Offset=1244 */

	/* Return value */

/* 810 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 812 */	NdrFcShort( 0x14 ),	/* x86 Stack size/offset = 20 */
/* 814 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure GetAccessRights */

/* 816 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 818 */	NdrFcLong( 0x0 ),	/* 0 */
/* 822 */	NdrFcShort( 0x6 ),	/* 6 */
/* 824 */	NdrFcShort( 0xc ),	/* x86 Stack size/offset = 12 */
/* 826 */	NdrFcShort( 0x0 ),	/* 0 */
/* 828 */	NdrFcShort( 0x24 ),	/* 36 */
/* 830 */	0x4,		/* Oi2 Flags:  has return, */
			0x2,		/* 2 */

	/* Parameter pdwAccessRights */

/* 832 */	NdrFcShort( 0x2150 ),	/* Flags:  out, base type, simple ref, srv alloc size=8 */
/* 834 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 836 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Return value */

/* 838 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 840 */	NdrFcShort( 0x8 ),	/* x86 Stack size/offset = 8 */
/* 842 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure SetAccessRights */

/* 844 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 846 */	NdrFcLong( 0x0 ),	/* 0 */
/* 850 */	NdrFcShort( 0x7 ),	/* 7 */
/* 852 */	NdrFcShort( 0xc ),	/* x86 Stack size/offset = 12 */
/* 854 */	NdrFcShort( 0x8 ),	/* 8 */
/* 856 */	NdrFcShort( 0x8 ),	/* 8 */
/* 858 */	0x4,		/* Oi2 Flags:  has return, */
			0x2,		/* 2 */

	/* Parameter dwAccessRights */

/* 860 */	NdrFcShort( 0x48 ),	/* Flags:  in, base type, */
/* 862 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 864 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Return value */

/* 866 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 868 */	NdrFcShort( 0x8 ),	/* x86 Stack size/offset = 8 */
/* 870 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure GetDataType */

/* 872 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 874 */	NdrFcLong( 0x0 ),	/* 0 */
/* 878 */	NdrFcShort( 0x8 ),	/* 8 */
/* 880 */	NdrFcShort( 0xc ),	/* x86 Stack size/offset = 12 */
/* 882 */	NdrFcShort( 0x0 ),	/* 0 */
/* 884 */	NdrFcShort( 0x22 ),	/* 34 */
/* 886 */	0x4,		/* Oi2 Flags:  has return, */
			0x2,		/* 2 */

	/* Parameter pvtDataType */

/* 888 */	NdrFcShort( 0x2150 ),	/* Flags:  out, base type, simple ref, srv alloc size=8 */
/* 890 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 892 */	0x6,		/* FC_SHORT */
			0x0,		/* 0 */

	/* Return value */

/* 894 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 896 */	NdrFcShort( 0x8 ),	/* x86 Stack size/offset = 8 */
/* 898 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure SetDataType */

/* 900 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 902 */	NdrFcLong( 0x0 ),	/* 0 */
/* 906 */	NdrFcShort( 0x9 ),	/* 9 */
/* 908 */	NdrFcShort( 0xc ),	/* x86 Stack size/offset = 12 */
/* 910 */	NdrFcShort( 0x6 ),	/* 6 */
/* 912 */	NdrFcShort( 0x8 ),	/* 8 */
/* 914 */	0x4,		/* Oi2 Flags:  has return, */
			0x2,		/* 2 */

	/* Parameter vtDataType */

/* 916 */	NdrFcShort( 0x48 ),	/* Flags:  in, base type, */
/* 918 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 920 */	0x6,		/* FC_SHORT */
			0x0,		/* 0 */

	/* Return value */

/* 922 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 924 */	NdrFcShort( 0x8 ),	/* x86 Stack size/offset = 8 */
/* 926 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure CanRead */

/* 928 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 930 */	NdrFcLong( 0x0 ),	/* 0 */
/* 934 */	NdrFcShort( 0xa ),	/* 10 */
/* 936 */	NdrFcShort( 0xc ),	/* x86 Stack size/offset = 12 */
/* 938 */	NdrFcShort( 0x0 ),	/* 0 */
/* 940 */	NdrFcShort( 0x24 ),	/* 36 */
/* 942 */	0x4,		/* Oi2 Flags:  has return, */
			0x2,		/* 2 */

	/* Parameter pbResult */

/* 944 */	NdrFcShort( 0x2150 ),	/* Flags:  out, base type, simple ref, srv alloc size=8 */
/* 946 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 948 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Return value */

/* 950 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 952 */	NdrFcShort( 0x8 ),	/* x86 Stack size/offset = 8 */
/* 954 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure CanWrite */

/* 956 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 958 */	NdrFcLong( 0x0 ),	/* 0 */
/* 962 */	NdrFcShort( 0xb ),	/* 11 */
/* 964 */	NdrFcShort( 0xc ),	/* x86 Stack size/offset = 12 */
/* 966 */	NdrFcShort( 0x0 ),	/* 0 */
/* 968 */	NdrFcShort( 0x24 ),	/* 36 */
/* 970 */	0x4,		/* Oi2 Flags:  has return, */
			0x2,		/* 2 */

	/* Parameter pbResult */

/* 972 */	NdrFcShort( 0x2150 ),	/* Flags:  out, base type, simple ref, srv alloc size=8 */
/* 974 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 976 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Return value */

/* 978 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 980 */	NdrFcShort( 0x8 ),	/* x86 Stack size/offset = 8 */
/* 982 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure Read */

/* 984 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 986 */	NdrFcLong( 0x0 ),	/* 0 */
/* 990 */	NdrFcShort( 0x3 ),	/* 3 */
/* 992 */	NdrFcShort( 0x18 ),	/* x86 Stack size/offset = 24 */
/* 994 */	NdrFcShort( 0x0 ),	/* 0 */
/* 996 */	NdrFcShort( 0x4e ),	/* 78 */
/* 998 */	0x7,		/* Oi2 Flags:  srv must size, clt must size, has return, */
			0x5,		/* 5 */

	/* Parameter szItemID */

/* 1000 */	NdrFcShort( 0x10b ),	/* Flags:  must size, must free, in, simple ref, */
/* 1002 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 1004 */	NdrFcShort( 0x422 ),	/* Type Offset=1058 */

	/* Parameter pValue */

/* 1006 */	NdrFcShort( 0x4113 ),	/* Flags:  must size, must free, out, simple ref, srv alloc size=16 */
/* 1008 */	NdrFcShort( 0x8 ),	/* x86 Stack size/offset = 8 */
/* 1010 */	NdrFcShort( 0x460 ),	/* Type Offset=1120 */

	/* Parameter pQuality */

/* 1012 */	NdrFcShort( 0x2150 ),	/* Flags:  out, base type, simple ref, srv alloc size=8 */
/* 1014 */	NdrFcShort( 0xc ),	/* x86 Stack size/offset = 12 */
/* 1016 */	0x6,		/* FC_SHORT */
			0x0,		/* 0 */

	/* Parameter pftTimeStamp */

/* 1018 */	NdrFcShort( 0x2112 ),	/* Flags:  must free, out, simple ref, srv alloc size=8 */
/* 1020 */	NdrFcShort( 0x10 ),	/* x86 Stack size/offset = 16 */
/* 1022 */	NdrFcShort( 0x390 ),	/* Type Offset=912 */

	/* Return value */

/* 1024 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 1026 */	NdrFcShort( 0x14 ),	/* x86 Stack size/offset = 20 */
/* 1028 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure Write */

/* 1030 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 1032 */	NdrFcLong( 0x0 ),	/* 0 */
/* 1036 */	NdrFcShort( 0x4 ),	/* 4 */
/* 1038 */	NdrFcShort( 0x28 ),	/* x86 Stack size/offset = 40 */
/* 1040 */	NdrFcShort( 0x1e ),	/* 30 */
/* 1042 */	NdrFcShort( 0x8 ),	/* 8 */
/* 1044 */	0x6,		/* Oi2 Flags:  clt must size, has return, */
			0x5,		/* 5 */

	/* Parameter szItemID */

/* 1046 */	NdrFcShort( 0x10b ),	/* Flags:  must size, must free, in, simple ref, */
/* 1048 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 1050 */	NdrFcShort( 0x422 ),	/* Type Offset=1058 */

	/* Parameter Value */

/* 1052 */	NdrFcShort( 0x8b ),	/* Flags:  must size, must free, in, by val, */
/* 1054 */	NdrFcShort( 0x8 ),	/* x86 Stack size/offset = 8 */
/* 1056 */	NdrFcShort( 0x416 ),	/* Type Offset=1046 */

	/* Parameter Quality */

/* 1058 */	NdrFcShort( 0x48 ),	/* Flags:  in, base type, */
/* 1060 */	NdrFcShort( 0x18 ),	/* x86 Stack size/offset = 24 */
/* 1062 */	0x6,		/* FC_SHORT */
			0x0,		/* 0 */

	/* Parameter ftTimeStamp */

/* 1064 */	NdrFcShort( 0x8a ),	/* Flags:  must free, in, by val, */
/* 1066 */	NdrFcShort( 0x1c ),	/* x86 Stack size/offset = 28 */
/* 1068 */	NdrFcShort( 0x390 ),	/* Type Offset=912 */

	/* Return value */

/* 1070 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 1072 */	NdrFcShort( 0x24 ),	/* x86 Stack size/offset = 36 */
/* 1074 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure CreateItemEnumerator */

/* 1076 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 1078 */	NdrFcLong( 0x0 ),	/* 0 */
/* 1082 */	NdrFcShort( 0x5 ),	/* 5 */
/* 1084 */	NdrFcShort( 0xc ),	/* x86 Stack size/offset = 12 */
/* 1086 */	NdrFcShort( 0x0 ),	/* 0 */
/* 1088 */	NdrFcShort( 0x8 ),	/* 8 */
/* 1090 */	0x5,		/* Oi2 Flags:  srv must size, has return, */
			0x2,		/* 2 */

	/* Parameter ppIEnumString */

/* 1092 */	NdrFcShort( 0x13 ),	/* Flags:  must size, must free, out, */
/* 1094 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 1096 */	NdrFcShort( 0x43a ),	/* Type Offset=1082 */

	/* Return value */

/* 1098 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 1100 */	NdrFcShort( 0x8 ),	/* x86 Stack size/offset = 8 */
/* 1102 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure Update */

/* 1104 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 1106 */	NdrFcLong( 0x0 ),	/* 0 */
/* 1110 */	NdrFcShort( 0x6 ),	/* 6 */
/* 1112 */	NdrFcShort( 0xc ),	/* x86 Stack size/offset = 12 */
/* 1114 */	NdrFcShort( 0x0 ),	/* 0 */
/* 1116 */	NdrFcShort( 0x8 ),	/* 8 */
/* 1118 */	0x6,		/* Oi2 Flags:  clt must size, has return, */
			0x2,		/* 2 */

	/* Parameter pDataSource */

/* 1120 */	NdrFcShort( 0xb ),	/* Flags:  must size, must free, in, */
/* 1122 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 1124 */	NdrFcShort( 0x1c ),	/* Type Offset=28 */

	/* Return value */

/* 1126 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 1128 */	NdrFcShort( 0x8 ),	/* x86 Stack size/offset = 8 */
/* 1130 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure AddItem */

/* 1132 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 1134 */	NdrFcLong( 0x0 ),	/* 0 */
/* 1138 */	NdrFcShort( 0x7 ),	/* 7 */
/* 1140 */	NdrFcShort( 0xc ),	/* x86 Stack size/offset = 12 */
/* 1142 */	NdrFcShort( 0x0 ),	/* 0 */
/* 1144 */	NdrFcShort( 0x8 ),	/* 8 */
/* 1146 */	0x6,		/* Oi2 Flags:  clt must size, has return, */
			0x2,		/* 2 */

	/* Parameter szItemID */

/* 1148 */	NdrFcShort( 0x10b ),	/* Flags:  must size, must free, in, simple ref, */
/* 1150 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 1152 */	NdrFcShort( 0x422 ),	/* Type Offset=1058 */

	/* Return value */

/* 1154 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 1156 */	NdrFcShort( 0x8 ),	/* x86 Stack size/offset = 8 */
/* 1158 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure RemoveItem */

/* 1160 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 1162 */	NdrFcLong( 0x0 ),	/* 0 */
/* 1166 */	NdrFcShort( 0x8 ),	/* 8 */
/* 1168 */	NdrFcShort( 0xc ),	/* x86 Stack size/offset = 12 */
/* 1170 */	NdrFcShort( 0x0 ),	/* 0 */
/* 1172 */	NdrFcShort( 0x8 ),	/* 8 */
/* 1174 */	0x6,		/* Oi2 Flags:  clt must size, has return, */
			0x2,		/* 2 */

	/* Parameter szItemID */

/* 1176 */	NdrFcShort( 0x10b ),	/* Flags:  must size, must free, in, simple ref, */
/* 1178 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 1180 */	NdrFcShort( 0x422 ),	/* Type Offset=1058 */

	/* Return value */

/* 1182 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 1184 */	NdrFcShort( 0x8 ),	/* x86 Stack size/offset = 8 */
/* 1186 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

	/* Procedure ClearItems */

/* 1188 */	0x33,		/* FC_AUTO_HANDLE */
			0x6c,		/* Old Flags:  object, Oi2 */
/* 1190 */	NdrFcLong( 0x0 ),	/* 0 */
/* 1194 */	NdrFcShort( 0x9 ),	/* 9 */
/* 1196 */	NdrFcShort( 0x8 ),	/* x86 Stack size/offset = 8 */
/* 1198 */	NdrFcShort( 0x0 ),	/* 0 */
/* 1200 */	NdrFcShort( 0x8 ),	/* 8 */
/* 1202 */	0x4,		/* Oi2 Flags:  has return, */
			0x1,		/* 1 */

	/* Return value */

/* 1204 */	NdrFcShort( 0x70 ),	/* Flags:  out, return, base type, */
/* 1206 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 1208 */	0x8,		/* FC_LONG */
			0x0,		/* 0 */

			0x0
        }
    };

static const MIDL_TYPE_FORMAT_STRING __MIDL_TypeFormatString =
    {
        0,
        {
			NdrFcShort( 0x0 ),	/* 0 */
/*  2 */	
			0x11, 0x10,	/* FC_RP [pointer_deref] */
/*  4 */	NdrFcShort( 0x2 ),	/* Offset= 2 (6) */
/*  6 */	
			0x2f,		/* FC_IP */
			0x5a,		/* FC_CONSTANT_IID */
/*  8 */	NdrFcLong( 0xef4b587b ),	/* -280274821 */
/* 12 */	NdrFcShort( 0x7426 ),	/* 29734 */
/* 14 */	NdrFcShort( 0x43c7 ),	/* 17351 */
/* 16 */	0x9c,		/* 156 */
			0x6f,		/* 111 */
/* 18 */	0xa5,		/* 165 */
			0x13,		/* 19 */
/* 20 */	0xe1,		/* 225 */
			0x36,		/* 54 */
/* 22 */	0x71,		/* 113 */
			0x98,		/* 152 */
/* 24 */	
			0x11, 0x10,	/* FC_RP [pointer_deref] */
/* 26 */	NdrFcShort( 0x2 ),	/* Offset= 2 (28) */
/* 28 */	
			0x2f,		/* FC_IP */
			0x5a,		/* FC_CONSTANT_IID */
/* 30 */	NdrFcLong( 0x5c389437 ),	/* 1547211831 */
/* 34 */	NdrFcShort( 0x7a4d ),	/* 31309 */
/* 36 */	NdrFcShort( 0x4db0 ),	/* 19888 */
/* 38 */	0x8e,		/* 142 */
			0x2,		/* 2 */
/* 40 */	0x15,		/* 21 */
			0xfe,		/* 254 */
/* 42 */	0x25,		/* 37 */
			0x23,		/* 35 */
/* 44 */	0xd9,		/* 217 */
			0xb8,		/* 184 */
/* 46 */	
			0x11, 0xc,	/* FC_RP [alloced_on_stack] [simple_pointer] */
/* 48 */	0x8,		/* FC_LONG */
			0x5c,		/* FC_PAD */
/* 50 */	
			0x11, 0xc,	/* FC_RP [alloced_on_stack] [simple_pointer] */
/* 52 */	0xd,		/* FC_ENUM16 */
			0x5c,		/* FC_PAD */
/* 54 */	
			0x12, 0x0,	/* FC_UP */
/* 56 */	NdrFcShort( 0x3ca ),	/* Offset= 970 (1026) */
/* 58 */	
			0x2b,		/* FC_NON_ENCAPSULATED_UNION */
			0x9,		/* FC_ULONG */
/* 60 */	0x7,		/* Corr desc: FC_USHORT */
			0x0,		/*  */
/* 62 */	NdrFcShort( 0xfff8 ),	/* -8 */
/* 64 */	NdrFcShort( 0x2 ),	/* Offset= 2 (66) */
/* 66 */	NdrFcShort( 0x10 ),	/* 16 */
/* 68 */	NdrFcShort( 0x2f ),	/* 47 */
/* 70 */	NdrFcLong( 0x14 ),	/* 20 */
/* 74 */	NdrFcShort( 0x800b ),	/* Simple arm type: FC_HYPER */
/* 76 */	NdrFcLong( 0x3 ),	/* 3 */
/* 80 */	NdrFcShort( 0x8008 ),	/* Simple arm type: FC_LONG */
/* 82 */	NdrFcLong( 0x11 ),	/* 17 */
/* 86 */	NdrFcShort( 0x8001 ),	/* Simple arm type: FC_BYTE */
/* 88 */	NdrFcLong( 0x2 ),	/* 2 */
/* 92 */	NdrFcShort( 0x8006 ),	/* Simple arm type: FC_SHORT */
/* 94 */	NdrFcLong( 0x4 ),	/* 4 */
/* 98 */	NdrFcShort( 0x800a ),	/* Simple arm type: FC_FLOAT */
/* 100 */	NdrFcLong( 0x5 ),	/* 5 */
/* 104 */	NdrFcShort( 0x800c ),	/* Simple arm type: FC_DOUBLE */
/* 106 */	NdrFcLong( 0xb ),	/* 11 */
/* 110 */	NdrFcShort( 0x8006 ),	/* Simple arm type: FC_SHORT */
/* 112 */	NdrFcLong( 0xa ),	/* 10 */
/* 116 */	NdrFcShort( 0x8008 ),	/* Simple arm type: FC_LONG */
/* 118 */	NdrFcLong( 0x6 ),	/* 6 */
/* 122 */	NdrFcShort( 0xe8 ),	/* Offset= 232 (354) */
/* 124 */	NdrFcLong( 0x7 ),	/* 7 */
/* 128 */	NdrFcShort( 0x800c ),	/* Simple arm type: FC_DOUBLE */
/* 130 */	NdrFcLong( 0x8 ),	/* 8 */
/* 134 */	NdrFcShort( 0xe2 ),	/* Offset= 226 (360) */
/* 136 */	NdrFcLong( 0xd ),	/* 13 */
/* 140 */	NdrFcShort( 0xf4 ),	/* Offset= 244 (384) */
/* 142 */	NdrFcLong( 0x9 ),	/* 9 */
/* 146 */	NdrFcShort( 0x100 ),	/* Offset= 256 (402) */
/* 148 */	NdrFcLong( 0x2000 ),	/* 8192 */
/* 152 */	NdrFcShort( 0x10c ),	/* Offset= 268 (420) */
/* 154 */	NdrFcLong( 0x24 ),	/* 36 */
/* 158 */	NdrFcShort( 0x31a ),	/* Offset= 794 (952) */
/* 160 */	NdrFcLong( 0x4024 ),	/* 16420 */
/* 164 */	NdrFcShort( 0x314 ),	/* Offset= 788 (952) */
/* 166 */	NdrFcLong( 0x4011 ),	/* 16401 */
/* 170 */	NdrFcShort( 0x312 ),	/* Offset= 786 (956) */
/* 172 */	NdrFcLong( 0x4002 ),	/* 16386 */
/* 176 */	NdrFcShort( 0x310 ),	/* Offset= 784 (960) */
/* 178 */	NdrFcLong( 0x4003 ),	/* 16387 */
/* 182 */	NdrFcShort( 0x30e ),	/* Offset= 782 (964) */
/* 184 */	NdrFcLong( 0x4014 ),	/* 16404 */
/* 188 */	NdrFcShort( 0x30c ),	/* Offset= 780 (968) */
/* 190 */	NdrFcLong( 0x4004 ),	/* 16388 */
/* 194 */	NdrFcShort( 0x30a ),	/* Offset= 778 (972) */
/* 196 */	NdrFcLong( 0x4005 ),	/* 16389 */
/* 200 */	NdrFcShort( 0x308 ),	/* Offset= 776 (976) */
/* 202 */	NdrFcLong( 0x400b ),	/* 16395 */
/* 206 */	NdrFcShort( 0x2f2 ),	/* Offset= 754 (960) */
/* 208 */	NdrFcLong( 0x400a ),	/* 16394 */
/* 212 */	NdrFcShort( 0x2f0 ),	/* Offset= 752 (964) */
/* 214 */	NdrFcLong( 0x4006 ),	/* 16390 */
/* 218 */	NdrFcShort( 0x2fa ),	/* Offset= 762 (980) */
/* 220 */	NdrFcLong( 0x4007 ),	/* 16391 */
/* 224 */	NdrFcShort( 0x2f0 ),	/* Offset= 752 (976) */
/* 226 */	NdrFcLong( 0x4008 ),	/* 16392 */
/* 230 */	NdrFcShort( 0x2f2 ),	/* Offset= 754 (984) */
/* 232 */	NdrFcLong( 0x400d ),	/* 16397 */
/* 236 */	NdrFcShort( 0x2f0 ),	/* Offset= 752 (988) */
/* 238 */	NdrFcLong( 0x4009 ),	/* 16393 */
/* 242 */	NdrFcShort( 0x2ee ),	/* Offset= 750 (992) */
/* 244 */	NdrFcLong( 0x6000 ),	/* 24576 */
/* 248 */	NdrFcShort( 0x2ec ),	/* Offset= 748 (996) */
/* 250 */	NdrFcLong( 0x400c ),	/* 16396 */
/* 254 */	NdrFcShort( 0x2ea ),	/* Offset= 746 (1000) */
/* 256 */	NdrFcLong( 0x10 ),	/* 16 */
/* 260 */	NdrFcShort( 0x8002 ),	/* Simple arm type: FC_CHAR */
/* 262 */	NdrFcLong( 0x12 ),	/* 18 */
/* 266 */	NdrFcShort( 0x8006 ),	/* Simple arm type: FC_SHORT */
/* 268 */	NdrFcLong( 0x13 ),	/* 19 */
/* 272 */	NdrFcShort( 0x8008 ),	/* Simple arm type: FC_LONG */
/* 274 */	NdrFcLong( 0x15 ),	/* 21 */
/* 278 */	NdrFcShort( 0x800b ),	/* Simple arm type: FC_HYPER */
/* 280 */	NdrFcLong( 0x16 ),	/* 22 */
/* 284 */	NdrFcShort( 0x8008 ),	/* Simple arm type: FC_LONG */
/* 286 */	NdrFcLong( 0x17 ),	/* 23 */
/* 290 */	NdrFcShort( 0x8008 ),	/* Simple arm type: FC_LONG */
/* 292 */	NdrFcLong( 0xe ),	/* 14 */
/* 296 */	NdrFcShort( 0x2c8 ),	/* Offset= 712 (1008) */
/* 298 */	NdrFcLong( 0x400e ),	/* 16398 */
/* 302 */	NdrFcShort( 0x2cc ),	/* Offset= 716 (1018) */
/* 304 */	NdrFcLong( 0x4010 ),	/* 16400 */
/* 308 */	NdrFcShort( 0x2ca ),	/* Offset= 714 (1022) */
/* 310 */	NdrFcLong( 0x4012 ),	/* 16402 */
/* 314 */	NdrFcShort( 0x286 ),	/* Offset= 646 (960) */
/* 316 */	NdrFcLong( 0x4013 ),	/* 16403 */
/* 320 */	NdrFcShort( 0x284 ),	/* Offset= 644 (964) */
/* 322 */	NdrFcLong( 0x4015 ),	/* 16405 */
/* 326 */	NdrFcShort( 0x282 ),	/* Offset= 642 (968) */
/* 328 */	NdrFcLong( 0x4016 ),	/* 16406 */
/* 332 */	NdrFcShort( 0x278 ),	/* Offset= 632 (964) */
/* 334 */	NdrFcLong( 0x4017 ),	/* 16407 */
/* 338 */	NdrFcShort( 0x272 ),	/* Offset= 626 (964) */
/* 340 */	NdrFcLong( 0x0 ),	/* 0 */
/* 344 */	NdrFcShort( 0x0 ),	/* Offset= 0 (344) */
/* 346 */	NdrFcLong( 0x1 ),	/* 1 */
/* 350 */	NdrFcShort( 0x0 ),	/* Offset= 0 (350) */
/* 352 */	NdrFcShort( 0xffffffff ),	/* Offset= -1 (351) */
/* 354 */	
			0x15,		/* FC_STRUCT */
			0x7,		/* 7 */
/* 356 */	NdrFcShort( 0x8 ),	/* 8 */
/* 358 */	0xb,		/* FC_HYPER */
			0x5b,		/* FC_END */
/* 360 */	
			0x12, 0x0,	/* FC_UP */
/* 362 */	NdrFcShort( 0xc ),	/* Offset= 12 (374) */
/* 364 */	
			0x1b,		/* FC_CARRAY */
			0x1,		/* 1 */
/* 366 */	NdrFcShort( 0x2 ),	/* 2 */
/* 368 */	0x9,		/* Corr desc: FC_ULONG */
			0x0,		/*  */
/* 370 */	NdrFcShort( 0xfffc ),	/* -4 */
/* 372 */	0x6,		/* FC_SHORT */
			0x5b,		/* FC_END */
/* 374 */	
			0x17,		/* FC_CSTRUCT */
			0x3,		/* 3 */
/* 376 */	NdrFcShort( 0x8 ),	/* 8 */
/* 378 */	NdrFcShort( 0xfffffff2 ),	/* Offset= -14 (364) */
/* 380 */	0x8,		/* FC_LONG */
			0x8,		/* FC_LONG */
/* 382 */	0x5c,		/* FC_PAD */
			0x5b,		/* FC_END */
/* 384 */	
			0x2f,		/* FC_IP */
			0x5a,		/* FC_CONSTANT_IID */
/* 386 */	NdrFcLong( 0x0 ),	/* 0 */
/* 390 */	NdrFcShort( 0x0 ),	/* 0 */
/* 392 */	NdrFcShort( 0x0 ),	/* 0 */
/* 394 */	0xc0,		/* 192 */
			0x0,		/* 0 */
/* 396 */	0x0,		/* 0 */
			0x0,		/* 0 */
/* 398 */	0x0,		/* 0 */
			0x0,		/* 0 */
/* 400 */	0x0,		/* 0 */
			0x46,		/* 70 */
/* 402 */	
			0x2f,		/* FC_IP */
			0x5a,		/* FC_CONSTANT_IID */
/* 404 */	NdrFcLong( 0x20400 ),	/* 132096 */
/* 408 */	NdrFcShort( 0x0 ),	/* 0 */
/* 410 */	NdrFcShort( 0x0 ),	/* 0 */
/* 412 */	0xc0,		/* 192 */
			0x0,		/* 0 */
/* 414 */	0x0,		/* 0 */
			0x0,		/* 0 */
/* 416 */	0x0,		/* 0 */
			0x0,		/* 0 */
/* 418 */	0x0,		/* 0 */
			0x46,		/* 70 */
/* 420 */	
			0x12, 0x10,	/* FC_UP [pointer_deref] */
/* 422 */	NdrFcShort( 0x2 ),	/* Offset= 2 (424) */
/* 424 */	
			0x12, 0x0,	/* FC_UP */
/* 426 */	NdrFcShort( 0x1fc ),	/* Offset= 508 (934) */
/* 428 */	
			0x2a,		/* FC_ENCAPSULATED_UNION */
			0x49,		/* 73 */
/* 430 */	NdrFcShort( 0x18 ),	/* 24 */
/* 432 */	NdrFcShort( 0xa ),	/* 10 */
/* 434 */	NdrFcLong( 0x8 ),	/* 8 */
/* 438 */	NdrFcShort( 0x58 ),	/* Offset= 88 (526) */
/* 440 */	NdrFcLong( 0xd ),	/* 13 */
/* 444 */	NdrFcShort( 0x78 ),	/* Offset= 120 (564) */
/* 446 */	NdrFcLong( 0x9 ),	/* 9 */
/* 450 */	NdrFcShort( 0x94 ),	/* Offset= 148 (598) */
/* 452 */	NdrFcLong( 0xc ),	/* 12 */
/* 456 */	NdrFcShort( 0xbc ),	/* Offset= 188 (644) */
/* 458 */	NdrFcLong( 0x24 ),	/* 36 */
/* 462 */	NdrFcShort( 0x114 ),	/* Offset= 276 (738) */
/* 464 */	NdrFcLong( 0x800d ),	/* 32781 */
/* 468 */	NdrFcShort( 0x130 ),	/* Offset= 304 (772) */
/* 470 */	NdrFcLong( 0x10 ),	/* 16 */
/* 474 */	NdrFcShort( 0x148 ),	/* Offset= 328 (802) */
/* 476 */	NdrFcLong( 0x2 ),	/* 2 */
/* 480 */	NdrFcShort( 0x160 ),	/* Offset= 352 (832) */
/* 482 */	NdrFcLong( 0x3 ),	/* 3 */
/* 486 */	NdrFcShort( 0x178 ),	/* Offset= 376 (862) */
/* 488 */	NdrFcLong( 0x14 ),	/* 20 */
/* 492 */	NdrFcShort( 0x190 ),	/* Offset= 400 (892) */
/* 494 */	NdrFcShort( 0xffffffff ),	/* Offset= -1 (493) */
/* 496 */	
			0x1b,		/* FC_CARRAY */
			0x3,		/* 3 */
/* 498 */	NdrFcShort( 0x4 ),	/* 4 */
/* 500 */	0x19,		/* Corr desc:  field pointer, FC_ULONG */
			0x0,		/*  */
/* 502 */	NdrFcShort( 0x0 ),	/* 0 */
/* 504 */	
			0x4b,		/* FC_PP */
			0x5c,		/* FC_PAD */
/* 506 */	
			0x48,		/* FC_VARIABLE_REPEAT */
			0x49,		/* FC_FIXED_OFFSET */
/* 508 */	NdrFcShort( 0x4 ),	/* 4 */
/* 510 */	NdrFcShort( 0x0 ),	/* 0 */
/* 512 */	NdrFcShort( 0x1 ),	/* 1 */
/* 514 */	NdrFcShort( 0x0 ),	/* 0 */
/* 516 */	NdrFcShort( 0x0 ),	/* 0 */
/* 518 */	0x12, 0x0,	/* FC_UP */
/* 520 */	NdrFcShort( 0xffffff6e ),	/* Offset= -146 (374) */
/* 522 */	
			0x5b,		/* FC_END */

			0x8,		/* FC_LONG */
/* 524 */	0x5c,		/* FC_PAD */
			0x5b,		/* FC_END */
/* 526 */	
			0x16,		/* FC_PSTRUCT */
			0x3,		/* 3 */
/* 528 */	NdrFcShort( 0x8 ),	/* 8 */
/* 530 */	
			0x4b,		/* FC_PP */
			0x5c,		/* FC_PAD */
/* 532 */	
			0x46,		/* FC_NO_REPEAT */
			0x5c,		/* FC_PAD */
/* 534 */	NdrFcShort( 0x4 ),	/* 4 */
/* 536 */	NdrFcShort( 0x4 ),	/* 4 */
/* 538 */	0x11, 0x0,	/* FC_RP */
/* 540 */	NdrFcShort( 0xffffffd4 ),	/* Offset= -44 (496) */
/* 542 */	
			0x5b,		/* FC_END */

			0x8,		/* FC_LONG */
/* 544 */	0x8,		/* FC_LONG */
			0x5b,		/* FC_END */
/* 546 */	
			0x21,		/* FC_BOGUS_ARRAY */
			0x3,		/* 3 */
/* 548 */	NdrFcShort( 0x0 ),	/* 0 */
/* 550 */	0x19,		/* Corr desc:  field pointer, FC_ULONG */
			0x0,		/*  */
/* 552 */	NdrFcShort( 0x0 ),	/* 0 */
/* 554 */	NdrFcLong( 0xffffffff ),	/* -1 */
/* 558 */	0x4c,		/* FC_EMBEDDED_COMPLEX */
			0x0,		/* 0 */
/* 560 */	NdrFcShort( 0xffffff50 ),	/* Offset= -176 (384) */
/* 562 */	0x5c,		/* FC_PAD */
			0x5b,		/* FC_END */
/* 564 */	
			0x1a,		/* FC_BOGUS_STRUCT */
			0x3,		/* 3 */
/* 566 */	NdrFcShort( 0x8 ),	/* 8 */
/* 568 */	NdrFcShort( 0x0 ),	/* 0 */
/* 570 */	NdrFcShort( 0x6 ),	/* Offset= 6 (576) */
/* 572 */	0x8,		/* FC_LONG */
			0x36,		/* FC_POINTER */
/* 574 */	0x5c,		/* FC_PAD */
			0x5b,		/* FC_END */
/* 576 */	
			0x11, 0x0,	/* FC_RP */
/* 578 */	NdrFcShort( 0xffffffe0 ),	/* Offset= -32 (546) */
/* 580 */	
			0x21,		/* FC_BOGUS_ARRAY */
			0x3,		/* 3 */
/* 582 */	NdrFcShort( 0x0 ),	/* 0 */
/* 584 */	0x19,		/* Corr desc:  field pointer, FC_ULONG */
			0x0,		/*  */
/* 586 */	NdrFcShort( 0x0 ),	/* 0 */
/* 588 */	NdrFcLong( 0xffffffff ),	/* -1 */
/* 592 */	0x4c,		/* FC_EMBEDDED_COMPLEX */
			0x0,		/* 0 */
/* 594 */	NdrFcShort( 0xffffff40 ),	/* Offset= -192 (402) */
/* 596 */	0x5c,		/* FC_PAD */
			0x5b,		/* FC_END */
/* 598 */	
			0x1a,		/* FC_BOGUS_STRUCT */
			0x3,		/* 3 */
/* 600 */	NdrFcShort( 0x8 ),	/* 8 */
/* 602 */	NdrFcShort( 0x0 ),	/* 0 */
/* 604 */	NdrFcShort( 0x6 ),	/* Offset= 6 (610) */
/* 606 */	0x8,		/* FC_LONG */
			0x36,		/* FC_POINTER */
/* 608 */	0x5c,		/* FC_PAD */
			0x5b,		/* FC_END */
/* 610 */	
			0x11, 0x0,	/* FC_RP */
/* 612 */	NdrFcShort( 0xffffffe0 ),	/* Offset= -32 (580) */
/* 614 */	
			0x1b,		/* FC_CARRAY */
			0x3,		/* 3 */
/* 616 */	NdrFcShort( 0x4 ),	/* 4 */
/* 618 */	0x19,		/* Corr desc:  field pointer, FC_ULONG */
			0x0,		/*  */
/* 620 */	NdrFcShort( 0x0 ),	/* 0 */
/* 622 */	
			0x4b,		/* FC_PP */
			0x5c,		/* FC_PAD */
/* 624 */	
			0x48,		/* FC_VARIABLE_REPEAT */
			0x49,		/* FC_FIXED_OFFSET */
/* 626 */	NdrFcShort( 0x4 ),	/* 4 */
/* 628 */	NdrFcShort( 0x0 ),	/* 0 */
/* 630 */	NdrFcShort( 0x1 ),	/* 1 */
/* 632 */	NdrFcShort( 0x0 ),	/* 0 */
/* 634 */	NdrFcShort( 0x0 ),	/* 0 */
/* 636 */	0x12, 0x0,	/* FC_UP */
/* 638 */	NdrFcShort( 0x184 ),	/* Offset= 388 (1026) */
/* 640 */	
			0x5b,		/* FC_END */

			0x8,		/* FC_LONG */
/* 642 */	0x5c,		/* FC_PAD */
			0x5b,		/* FC_END */
/* 644 */	
			0x1a,		/* FC_BOGUS_STRUCT */
			0x3,		/* 3 */
/* 646 */	NdrFcShort( 0x8 ),	/* 8 */
/* 648 */	NdrFcShort( 0x0 ),	/* 0 */
/* 650 */	NdrFcShort( 0x6 ),	/* Offset= 6 (656) */
/* 652 */	0x8,		/* FC_LONG */
			0x36,		/* FC_POINTER */
/* 654 */	0x5c,		/* FC_PAD */
			0x5b,		/* FC_END */
/* 656 */	
			0x11, 0x0,	/* FC_RP */
/* 658 */	NdrFcShort( 0xffffffd4 ),	/* Offset= -44 (614) */
/* 660 */	
			0x2f,		/* FC_IP */
			0x5a,		/* FC_CONSTANT_IID */
/* 662 */	NdrFcLong( 0x2f ),	/* 47 */
/* 666 */	NdrFcShort( 0x0 ),	/* 0 */
/* 668 */	NdrFcShort( 0x0 ),	/* 0 */
/* 670 */	0xc0,		/* 192 */
			0x0,		/* 0 */
/* 672 */	0x0,		/* 0 */
			0x0,		/* 0 */
/* 674 */	0x0,		/* 0 */
			0x0,		/* 0 */
/* 676 */	0x0,		/* 0 */
			0x46,		/* 70 */
/* 678 */	
			0x1b,		/* FC_CARRAY */
			0x0,		/* 0 */
/* 680 */	NdrFcShort( 0x1 ),	/* 1 */
/* 682 */	0x19,		/* Corr desc:  field pointer, FC_ULONG */
			0x0,		/*  */
/* 684 */	NdrFcShort( 0x4 ),	/* 4 */
/* 686 */	0x1,		/* FC_BYTE */
			0x5b,		/* FC_END */
/* 688 */	
			0x1a,		/* FC_BOGUS_STRUCT */
			0x3,		/* 3 */
/* 690 */	NdrFcShort( 0x10 ),	/* 16 */
/* 692 */	NdrFcShort( 0x0 ),	/* 0 */
/* 694 */	NdrFcShort( 0xa ),	/* Offset= 10 (704) */
/* 696 */	0x8,		/* FC_LONG */
			0x8,		/* FC_LONG */
/* 698 */	0x4c,		/* FC_EMBEDDED_COMPLEX */
			0x0,		/* 0 */
/* 700 */	NdrFcShort( 0xffffffd8 ),	/* Offset= -40 (660) */
/* 702 */	0x36,		/* FC_POINTER */
			0x5b,		/* FC_END */
/* 704 */	
			0x12, 0x0,	/* FC_UP */
/* 706 */	NdrFcShort( 0xffffffe4 ),	/* Offset= -28 (678) */
/* 708 */	
			0x1b,		/* FC_CARRAY */
			0x3,		/* 3 */
/* 710 */	NdrFcShort( 0x4 ),	/* 4 */
/* 712 */	0x19,		/* Corr desc:  field pointer, FC_ULONG */
			0x0,		/*  */
/* 714 */	NdrFcShort( 0x0 ),	/* 0 */
/* 716 */	
			0x4b,		/* FC_PP */
			0x5c,		/* FC_PAD */
/* 718 */	
			0x48,		/* FC_VARIABLE_REPEAT */
			0x49,		/* FC_FIXED_OFFSET */
/* 720 */	NdrFcShort( 0x4 ),	/* 4 */
/* 722 */	NdrFcShort( 0x0 ),	/* 0 */
/* 724 */	NdrFcShort( 0x1 ),	/* 1 */
/* 726 */	NdrFcShort( 0x0 ),	/* 0 */
/* 728 */	NdrFcShort( 0x0 ),	/* 0 */
/* 730 */	0x12, 0x0,	/* FC_UP */
/* 732 */	NdrFcShort( 0xffffffd4 ),	/* Offset= -44 (688) */
/* 734 */	
			0x5b,		/* FC_END */

			0x8,		/* FC_LONG */
/* 736 */	0x5c,		/* FC_PAD */
			0x5b,		/* FC_END */
/* 738 */	
			0x1a,		/* FC_BOGUS_STRUCT */
			0x3,		/* 3 */
/* 740 */	NdrFcShort( 0x8 ),	/* 8 */
/* 742 */	NdrFcShort( 0x0 ),	/* 0 */
/* 744 */	NdrFcShort( 0x6 ),	/* Offset= 6 (750) */
/* 746 */	0x8,		/* FC_LONG */
			0x36,		/* FC_POINTER */
/* 748 */	0x5c,		/* FC_PAD */
			0x5b,		/* FC_END */
/* 750 */	
			0x11, 0x0,	/* FC_RP */
/* 752 */	NdrFcShort( 0xffffffd4 ),	/* Offset= -44 (708) */
/* 754 */	
			0x1d,		/* FC_SMFARRAY */
			0x0,		/* 0 */
/* 756 */	NdrFcShort( 0x8 ),	/* 8 */
/* 758 */	0x1,		/* FC_BYTE */
			0x5b,		/* FC_END */
/* 760 */	
			0x15,		/* FC_STRUCT */
			0x3,		/* 3 */
/* 762 */	NdrFcShort( 0x10 ),	/* 16 */
/* 764 */	0x8,		/* FC_LONG */
			0x6,		/* FC_SHORT */
/* 766 */	0x6,		/* FC_SHORT */
			0x4c,		/* FC_EMBEDDED_COMPLEX */
/* 768 */	0x0,		/* 0 */
			NdrFcShort( 0xfffffff1 ),	/* Offset= -15 (754) */
			0x5b,		/* FC_END */
/* 772 */	
			0x1a,		/* FC_BOGUS_STRUCT */
			0x3,		/* 3 */
/* 774 */	NdrFcShort( 0x18 ),	/* 24 */
/* 776 */	NdrFcShort( 0x0 ),	/* 0 */
/* 778 */	NdrFcShort( 0xa ),	/* Offset= 10 (788) */
/* 780 */	0x8,		/* FC_LONG */
			0x36,		/* FC_POINTER */
/* 782 */	0x4c,		/* FC_EMBEDDED_COMPLEX */
			0x0,		/* 0 */
/* 784 */	NdrFcShort( 0xffffffe8 ),	/* Offset= -24 (760) */
/* 786 */	0x5c,		/* FC_PAD */
			0x5b,		/* FC_END */
/* 788 */	
			0x11, 0x0,	/* FC_RP */
/* 790 */	NdrFcShort( 0xffffff0c ),	/* Offset= -244 (546) */
/* 792 */	
			0x1b,		/* FC_CARRAY */
			0x0,		/* 0 */
/* 794 */	NdrFcShort( 0x1 ),	/* 1 */
/* 796 */	0x19,		/* Corr desc:  field pointer, FC_ULONG */
			0x0,		/*  */
/* 798 */	NdrFcShort( 0x0 ),	/* 0 */
/* 800 */	0x1,		/* FC_BYTE */
			0x5b,		/* FC_END */
/* 802 */	
			0x16,		/* FC_PSTRUCT */
			0x3,		/* 3 */
/* 804 */	NdrFcShort( 0x8 ),	/* 8 */
/* 806 */	
			0x4b,		/* FC_PP */
			0x5c,		/* FC_PAD */
/* 808 */	
			0x46,		/* FC_NO_REPEAT */
			0x5c,		/* FC_PAD */
/* 810 */	NdrFcShort( 0x4 ),	/* 4 */
/* 812 */	NdrFcShort( 0x4 ),	/* 4 */
/* 814 */	0x12, 0x0,	/* FC_UP */
/* 816 */	NdrFcShort( 0xffffffe8 ),	/* Offset= -24 (792) */
/* 818 */	
			0x5b,		/* FC_END */

			0x8,		/* FC_LONG */
/* 820 */	0x8,		/* FC_LONG */
			0x5b,		/* FC_END */
/* 822 */	
			0x1b,		/* FC_CARRAY */
			0x1,		/* 1 */
/* 824 */	NdrFcShort( 0x2 ),	/* 2 */
/* 826 */	0x19,		/* Corr desc:  field pointer, FC_ULONG */
			0x0,		/*  */
/* 828 */	NdrFcShort( 0x0 ),	/* 0 */
/* 830 */	0x6,		/* FC_SHORT */
			0x5b,		/* FC_END */
/* 832 */	
			0x16,		/* FC_PSTRUCT */
			0x3,		/* 3 */
/* 834 */	NdrFcShort( 0x8 ),	/* 8 */
/* 836 */	
			0x4b,		/* FC_PP */
			0x5c,		/* FC_PAD */
/* 838 */	
			0x46,		/* FC_NO_REPEAT */
			0x5c,		/* FC_PAD */
/* 840 */	NdrFcShort( 0x4 ),	/* 4 */
/* 842 */	NdrFcShort( 0x4 ),	/* 4 */
/* 844 */	0x12, 0x0,	/* FC_UP */
/* 846 */	NdrFcShort( 0xffffffe8 ),	/* Offset= -24 (822) */
/* 848 */	
			0x5b,		/* FC_END */

			0x8,		/* FC_LONG */
/* 850 */	0x8,		/* FC_LONG */
			0x5b,		/* FC_END */
/* 852 */	
			0x1b,		/* FC_CARRAY */
			0x3,		/* 3 */
/* 854 */	NdrFcShort( 0x4 ),	/* 4 */
/* 856 */	0x19,		/* Corr desc:  field pointer, FC_ULONG */
			0x0,		/*  */
/* 858 */	NdrFcShort( 0x0 ),	/* 0 */
/* 860 */	0x8,		/* FC_LONG */
			0x5b,		/* FC_END */
/* 862 */	
			0x16,		/* FC_PSTRUCT */
			0x3,		/* 3 */
/* 864 */	NdrFcShort( 0x8 ),	/* 8 */
/* 866 */	
			0x4b,		/* FC_PP */
			0x5c,		/* FC_PAD */
/* 868 */	
			0x46,		/* FC_NO_REPEAT */
			0x5c,		/* FC_PAD */
/* 870 */	NdrFcShort( 0x4 ),	/* 4 */
/* 872 */	NdrFcShort( 0x4 ),	/* 4 */
/* 874 */	0x12, 0x0,	/* FC_UP */
/* 876 */	NdrFcShort( 0xffffffe8 ),	/* Offset= -24 (852) */
/* 878 */	
			0x5b,		/* FC_END */

			0x8,		/* FC_LONG */
/* 880 */	0x8,		/* FC_LONG */
			0x5b,		/* FC_END */
/* 882 */	
			0x1b,		/* FC_CARRAY */
			0x7,		/* 7 */
/* 884 */	NdrFcShort( 0x8 ),	/* 8 */
/* 886 */	0x19,		/* Corr desc:  field pointer, FC_ULONG */
			0x0,		/*  */
/* 888 */	NdrFcShort( 0x0 ),	/* 0 */
/* 890 */	0xb,		/* FC_HYPER */
			0x5b,		/* FC_END */
/* 892 */	
			0x16,		/* FC_PSTRUCT */
			0x3,		/* 3 */
/* 894 */	NdrFcShort( 0x8 ),	/* 8 */
/* 896 */	
			0x4b,		/* FC_PP */
			0x5c,		/* FC_PAD */
/* 898 */	
			0x46,		/* FC_NO_REPEAT */
			0x5c,		/* FC_PAD */
/* 900 */	NdrFcShort( 0x4 ),	/* 4 */
/* 902 */	NdrFcShort( 0x4 ),	/* 4 */
/* 904 */	0x12, 0x0,	/* FC_UP */
/* 906 */	NdrFcShort( 0xffffffe8 ),	/* Offset= -24 (882) */
/* 908 */	
			0x5b,		/* FC_END */

			0x8,		/* FC_LONG */
/* 910 */	0x8,		/* FC_LONG */
			0x5b,		/* FC_END */
/* 912 */	
			0x15,		/* FC_STRUCT */
			0x3,		/* 3 */
/* 914 */	NdrFcShort( 0x8 ),	/* 8 */
/* 916 */	0x8,		/* FC_LONG */
			0x8,		/* FC_LONG */
/* 918 */	0x5c,		/* FC_PAD */
			0x5b,		/* FC_END */
/* 920 */	
			0x1b,		/* FC_CARRAY */
			0x3,		/* 3 */
/* 922 */	NdrFcShort( 0x8 ),	/* 8 */
/* 924 */	0x7,		/* Corr desc: FC_USHORT */
			0x0,		/*  */
/* 926 */	NdrFcShort( 0xffd8 ),	/* -40 */
/* 928 */	0x4c,		/* FC_EMBEDDED_COMPLEX */
			0x0,		/* 0 */
/* 930 */	NdrFcShort( 0xffffffee ),	/* Offset= -18 (912) */
/* 932 */	0x5c,		/* FC_PAD */
			0x5b,		/* FC_END */
/* 934 */	
			0x1a,		/* FC_BOGUS_STRUCT */
			0x3,		/* 3 */
/* 936 */	NdrFcShort( 0x28 ),	/* 40 */
/* 938 */	NdrFcShort( 0xffffffee ),	/* Offset= -18 (920) */
/* 940 */	NdrFcShort( 0x0 ),	/* Offset= 0 (940) */
/* 942 */	0x6,		/* FC_SHORT */
			0x6,		/* FC_SHORT */
/* 944 */	0x8,		/* FC_LONG */
			0x8,		/* FC_LONG */
/* 946 */	0x4c,		/* FC_EMBEDDED_COMPLEX */
			0x0,		/* 0 */
/* 948 */	NdrFcShort( 0xfffffdf8 ),	/* Offset= -520 (428) */
/* 950 */	0x5c,		/* FC_PAD */
			0x5b,		/* FC_END */
/* 952 */	
			0x12, 0x0,	/* FC_UP */
/* 954 */	NdrFcShort( 0xfffffef6 ),	/* Offset= -266 (688) */
/* 956 */	
			0x12, 0x8,	/* FC_UP [simple_pointer] */
/* 958 */	0x1,		/* FC_BYTE */
			0x5c,		/* FC_PAD */
/* 960 */	
			0x12, 0x8,	/* FC_UP [simple_pointer] */
/* 962 */	0x6,		/* FC_SHORT */
			0x5c,		/* FC_PAD */
/* 964 */	
			0x12, 0x8,	/* FC_UP [simple_pointer] */
/* 966 */	0x8,		/* FC_LONG */
			0x5c,		/* FC_PAD */
/* 968 */	
			0x12, 0x8,	/* FC_UP [simple_pointer] */
/* 970 */	0xb,		/* FC_HYPER */
			0x5c,		/* FC_PAD */
/* 972 */	
			0x12, 0x8,	/* FC_UP [simple_pointer] */
/* 974 */	0xa,		/* FC_FLOAT */
			0x5c,		/* FC_PAD */
/* 976 */	
			0x12, 0x8,	/* FC_UP [simple_pointer] */
/* 978 */	0xc,		/* FC_DOUBLE */
			0x5c,		/* FC_PAD */
/* 980 */	
			0x12, 0x0,	/* FC_UP */
/* 982 */	NdrFcShort( 0xfffffd8c ),	/* Offset= -628 (354) */
/* 984 */	
			0x12, 0x10,	/* FC_UP [pointer_deref] */
/* 986 */	NdrFcShort( 0xfffffd8e ),	/* Offset= -626 (360) */
/* 988 */	
			0x12, 0x10,	/* FC_UP [pointer_deref] */
/* 990 */	NdrFcShort( 0xfffffda2 ),	/* Offset= -606 (384) */
/* 992 */	
			0x12, 0x10,	/* FC_UP [pointer_deref] */
/* 994 */	NdrFcShort( 0xfffffdb0 ),	/* Offset= -592 (402) */
/* 996 */	
			0x12, 0x10,	/* FC_UP [pointer_deref] */
/* 998 */	NdrFcShort( 0xfffffdbe ),	/* Offset= -578 (420) */
/* 1000 */	
			0x12, 0x10,	/* FC_UP [pointer_deref] */
/* 1002 */	NdrFcShort( 0x2 ),	/* Offset= 2 (1004) */
/* 1004 */	
			0x12, 0x0,	/* FC_UP */
/* 1006 */	NdrFcShort( 0x14 ),	/* Offset= 20 (1026) */
/* 1008 */	
			0x15,		/* FC_STRUCT */
			0x7,		/* 7 */
/* 1010 */	NdrFcShort( 0x10 ),	/* 16 */
/* 1012 */	0x6,		/* FC_SHORT */
			0x1,		/* FC_BYTE */
/* 1014 */	0x1,		/* FC_BYTE */
			0x8,		/* FC_LONG */
/* 1016 */	0xb,		/* FC_HYPER */
			0x5b,		/* FC_END */
/* 1018 */	
			0x12, 0x0,	/* FC_UP */
/* 1020 */	NdrFcShort( 0xfffffff4 ),	/* Offset= -12 (1008) */
/* 1022 */	
			0x12, 0x8,	/* FC_UP [simple_pointer] */
/* 1024 */	0x2,		/* FC_CHAR */
			0x5c,		/* FC_PAD */
/* 1026 */	
			0x1a,		/* FC_BOGUS_STRUCT */
			0x7,		/* 7 */
/* 1028 */	NdrFcShort( 0x20 ),	/* 32 */
/* 1030 */	NdrFcShort( 0x0 ),	/* 0 */
/* 1032 */	NdrFcShort( 0x0 ),	/* Offset= 0 (1032) */
/* 1034 */	0x8,		/* FC_LONG */
			0x8,		/* FC_LONG */
/* 1036 */	0x6,		/* FC_SHORT */
			0x6,		/* FC_SHORT */
/* 1038 */	0x6,		/* FC_SHORT */
			0x6,		/* FC_SHORT */
/* 1040 */	0x4c,		/* FC_EMBEDDED_COMPLEX */
			0x0,		/* 0 */
/* 1042 */	NdrFcShort( 0xfffffc28 ),	/* Offset= -984 (58) */
/* 1044 */	0x5c,		/* FC_PAD */
			0x5b,		/* FC_END */
/* 1046 */	0xb4,		/* FC_USER_MARSHAL */
			0x83,		/* 131 */
/* 1048 */	NdrFcShort( 0x0 ),	/* 0 */
/* 1050 */	NdrFcShort( 0x10 ),	/* 16 */
/* 1052 */	NdrFcShort( 0x0 ),	/* 0 */
/* 1054 */	NdrFcShort( 0xfffffc18 ),	/* Offset= -1000 (54) */
/* 1056 */	
			0x11, 0x8,	/* FC_RP [simple_pointer] */
/* 1058 */	
			0x25,		/* FC_C_WSTRING */
			0x5c,		/* FC_PAD */
/* 1060 */	
			0x11, 0x10,	/* FC_RP [pointer_deref] */
/* 1062 */	NdrFcShort( 0x2 ),	/* Offset= 2 (1064) */
/* 1064 */	
			0x2f,		/* FC_IP */
			0x5a,		/* FC_CONSTANT_IID */
/* 1066 */	NdrFcLong( 0x3c914d07 ),	/* 1016155399 */
/* 1070 */	NdrFcShort( 0x3fe2 ),	/* 16354 */
/* 1072 */	NdrFcShort( 0x4339 ),	/* 17209 */
/* 1074 */	0xb5,		/* 181 */
			0x62,		/* 98 */
/* 1076 */	0x6d,		/* 109 */
			0x84,		/* 132 */
/* 1078 */	0x91,		/* 145 */
			0x77,		/* 119 */
/* 1080 */	0xf7,		/* 247 */
			0x87,		/* 135 */
/* 1082 */	
			0x11, 0x10,	/* FC_RP [pointer_deref] */
/* 1084 */	NdrFcShort( 0x2 ),	/* Offset= 2 (1086) */
/* 1086 */	
			0x2f,		/* FC_IP */
			0x5a,		/* FC_CONSTANT_IID */
/* 1088 */	NdrFcLong( 0x101 ),	/* 257 */
/* 1092 */	NdrFcShort( 0x0 ),	/* 0 */
/* 1094 */	NdrFcShort( 0x0 ),	/* 0 */
/* 1096 */	0xc0,		/* 192 */
			0x0,		/* 0 */
/* 1098 */	0x0,		/* 0 */
			0x0,		/* 0 */
/* 1100 */	0x0,		/* 0 */
			0x0,		/* 0 */
/* 1102 */	0x0,		/* 0 */
			0x46,		/* 70 */
/* 1104 */	
			0x11, 0x14,	/* FC_RP [alloced_on_stack] [pointer_deref] */
/* 1106 */	NdrFcShort( 0x2 ),	/* Offset= 2 (1108) */
/* 1108 */	
			0x13, 0x8,	/* FC_OP [simple_pointer] */
/* 1110 */	
			0x25,		/* FC_C_WSTRING */
			0x5c,		/* FC_PAD */
/* 1112 */	
			0x11, 0x4,	/* FC_RP [alloced_on_stack] */
/* 1114 */	NdrFcShort( 0x6 ),	/* Offset= 6 (1120) */
/* 1116 */	
			0x13, 0x0,	/* FC_OP */
/* 1118 */	NdrFcShort( 0xffffffa4 ),	/* Offset= -92 (1026) */
/* 1120 */	0xb4,		/* FC_USER_MARSHAL */
			0x83,		/* 131 */
/* 1122 */	NdrFcShort( 0x0 ),	/* 0 */
/* 1124 */	NdrFcShort( 0x10 ),	/* 16 */
/* 1126 */	NdrFcShort( 0x0 ),	/* 0 */
/* 1128 */	NdrFcShort( 0xfffffff4 ),	/* Offset= -12 (1116) */
/* 1130 */	
			0x11, 0x14,	/* FC_RP [alloced_on_stack] [pointer_deref] */
/* 1132 */	NdrFcShort( 0x2 ),	/* Offset= 2 (1134) */
/* 1134 */	
			0x13, 0x0,	/* FC_OP */
/* 1136 */	NdrFcShort( 0x2 ),	/* Offset= 2 (1138) */
/* 1138 */	
			0x1b,		/* FC_CARRAY */
			0x3,		/* 3 */
/* 1140 */	NdrFcShort( 0x4 ),	/* 4 */
/* 1142 */	0x29,		/* Corr desc:  parameter, FC_ULONG */
			0x54,		/* FC_DEREFERENCE */
/* 1144 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 1146 */	0x8,		/* FC_LONG */
			0x5b,		/* FC_END */
/* 1148 */	
			0x11, 0x14,	/* FC_RP [alloced_on_stack] [pointer_deref] */
/* 1150 */	NdrFcShort( 0x2 ),	/* Offset= 2 (1152) */
/* 1152 */	
			0x13, 0x0,	/* FC_OP */
/* 1154 */	NdrFcShort( 0x2 ),	/* Offset= 2 (1156) */
/* 1156 */	
			0x1b,		/* FC_CARRAY */
			0x3,		/* 3 */
/* 1158 */	NdrFcShort( 0x4 ),	/* 4 */
/* 1160 */	0x29,		/* Corr desc:  parameter, FC_ULONG */
			0x54,		/* FC_DEREFERENCE */
/* 1162 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 1164 */	
			0x4b,		/* FC_PP */
			0x5c,		/* FC_PAD */
/* 1166 */	
			0x48,		/* FC_VARIABLE_REPEAT */
			0x49,		/* FC_FIXED_OFFSET */
/* 1168 */	NdrFcShort( 0x4 ),	/* 4 */
/* 1170 */	NdrFcShort( 0x0 ),	/* 0 */
/* 1172 */	NdrFcShort( 0x1 ),	/* 1 */
/* 1174 */	NdrFcShort( 0x0 ),	/* 0 */
/* 1176 */	NdrFcShort( 0x0 ),	/* 0 */
/* 1178 */	0x13, 0x8,	/* FC_OP [simple_pointer] */
/* 1180 */	
			0x25,		/* FC_C_WSTRING */
			0x5c,		/* FC_PAD */
/* 1182 */	
			0x5b,		/* FC_END */

			0x8,		/* FC_LONG */
/* 1184 */	0x5c,		/* FC_PAD */
			0x5b,		/* FC_END */
/* 1186 */	
			0x11, 0x14,	/* FC_RP [alloced_on_stack] [pointer_deref] */
/* 1188 */	NdrFcShort( 0x2 ),	/* Offset= 2 (1190) */
/* 1190 */	
			0x13, 0x0,	/* FC_OP */
/* 1192 */	NdrFcShort( 0x2 ),	/* Offset= 2 (1194) */
/* 1194 */	
			0x1b,		/* FC_CARRAY */
			0x1,		/* 1 */
/* 1196 */	NdrFcShort( 0x2 ),	/* 2 */
/* 1198 */	0x29,		/* Corr desc:  parameter, FC_ULONG */
			0x54,		/* FC_DEREFERENCE */
/* 1200 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 1202 */	0x6,		/* FC_SHORT */
			0x5b,		/* FC_END */
/* 1204 */	
			0x11, 0x0,	/* FC_RP */
/* 1206 */	NdrFcShort( 0x2 ),	/* Offset= 2 (1208) */
/* 1208 */	
			0x1b,		/* FC_CARRAY */
			0x3,		/* 3 */
/* 1210 */	NdrFcShort( 0x4 ),	/* 4 */
/* 1212 */	0x29,		/* Corr desc:  parameter, FC_ULONG */
			0x0,		/*  */
/* 1214 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 1216 */	0x8,		/* FC_LONG */
			0x5b,		/* FC_END */
/* 1218 */	
			0x11, 0x14,	/* FC_RP [alloced_on_stack] [pointer_deref] */
/* 1220 */	NdrFcShort( 0x2 ),	/* Offset= 2 (1222) */
/* 1222 */	
			0x13, 0x0,	/* FC_OP */
/* 1224 */	NdrFcShort( 0x2 ),	/* Offset= 2 (1226) */
/* 1226 */	
			0x21,		/* FC_BOGUS_ARRAY */
			0x3,		/* 3 */
/* 1228 */	NdrFcShort( 0x0 ),	/* 0 */
/* 1230 */	0x29,		/* Corr desc:  parameter, FC_ULONG */
			0x0,		/*  */
/* 1232 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 1234 */	NdrFcLong( 0xffffffff ),	/* -1 */
/* 1238 */	0x4c,		/* FC_EMBEDDED_COMPLEX */
			0x0,		/* 0 */
/* 1240 */	NdrFcShort( 0xffffff88 ),	/* Offset= -120 (1120) */
/* 1242 */	0x5c,		/* FC_PAD */
			0x5b,		/* FC_END */
/* 1244 */	
			0x11, 0x14,	/* FC_RP [alloced_on_stack] [pointer_deref] */
/* 1246 */	NdrFcShort( 0x2 ),	/* Offset= 2 (1248) */
/* 1248 */	
			0x13, 0x0,	/* FC_OP */
/* 1250 */	NdrFcShort( 0xffffffd6 ),	/* Offset= -42 (1208) */
/* 1252 */	
			0x11, 0x14,	/* FC_RP [alloced_on_stack] [pointer_deref] */
/* 1254 */	NdrFcShort( 0x2 ),	/* Offset= 2 (1256) */
/* 1256 */	
			0x13, 0x0,	/* FC_OP */
/* 1258 */	NdrFcShort( 0x2 ),	/* Offset= 2 (1260) */
/* 1260 */	
			0x1b,		/* FC_CARRAY */
			0x3,		/* 3 */
/* 1262 */	NdrFcShort( 0x4 ),	/* 4 */
/* 1264 */	0x29,		/* Corr desc:  parameter, FC_ULONG */
			0x0,		/*  */
/* 1266 */	NdrFcShort( 0x4 ),	/* x86 Stack size/offset = 4 */
/* 1268 */	
			0x4b,		/* FC_PP */
			0x5c,		/* FC_PAD */
/* 1270 */	
			0x48,		/* FC_VARIABLE_REPEAT */
			0x49,		/* FC_FIXED_OFFSET */
/* 1272 */	NdrFcShort( 0x4 ),	/* 4 */
/* 1274 */	NdrFcShort( 0x0 ),	/* 0 */
/* 1276 */	NdrFcShort( 0x1 ),	/* 1 */
/* 1278 */	NdrFcShort( 0x0 ),	/* 0 */
/* 1280 */	NdrFcShort( 0x0 ),	/* 0 */
/* 1282 */	0x13, 0x8,	/* FC_OP [simple_pointer] */
/* 1284 */	
			0x25,		/* FC_C_WSTRING */
			0x5c,		/* FC_PAD */
/* 1286 */	
			0x5b,		/* FC_END */

			0x8,		/* FC_LONG */
/* 1288 */	0x5c,		/* FC_PAD */
			0x5b,		/* FC_END */
/* 1290 */	
			0x11, 0xc,	/* FC_RP [alloced_on_stack] [simple_pointer] */
/* 1292 */	0x6,		/* FC_SHORT */
			0x5c,		/* FC_PAD */
/* 1294 */	
			0x11, 0x4,	/* FC_RP [alloced_on_stack] */
/* 1296 */	NdrFcShort( 0xfffffe80 ),	/* Offset= -384 (912) */

			0x0
        }
    };

static const USER_MARSHAL_ROUTINE_QUADRUPLE UserMarshalRoutines[ WIRE_MARSHAL_TABLE_SIZE ] = 
        {
            
            {
            VARIANT_UserSize
            ,VARIANT_UserMarshal
            ,VARIANT_UserUnmarshal
            ,VARIANT_UserFree
            }

        };



/* Standard interface: __MIDL_itf_XRTLOPCDA_0000, ver. 0.0,
   GUID={0x00000000,0x0000,0x0000,{0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00}} */


/* Object interface: IUnknown, ver. 0.0,
   GUID={0x00000000,0x0000,0x0000,{0xC0,0x00,0x00,0x00,0x00,0x00,0x00,0x46}} */


/* Object interface: IXRTLOPCDA20Server, ver. 0.0,
   GUID={0xC9108B6D,0xB9A7,0x4396,{0xBF,0x9F,0x55,0xF2,0x48,0x49,0xE9,0x20}} */

#pragma code_seg(".orpc")
static const unsigned short IXRTLOPCDA20Server_FormatStringOffsetTable[] =
    {
    0,
    28,
    56,
    90
    };

static const MIDL_STUBLESS_PROXY_INFO IXRTLOPCDA20Server_ProxyInfo =
    {
    &Object_StubDesc,
    __MIDL_ProcFormatString.Format,
    &IXRTLOPCDA20Server_FormatStringOffsetTable[-3],
    0,
    0,
    0
    };


static const MIDL_SERVER_INFO IXRTLOPCDA20Server_ServerInfo = 
    {
    &Object_StubDesc,
    0,
    __MIDL_ProcFormatString.Format,
    &IXRTLOPCDA20Server_FormatStringOffsetTable[-3],
    0,
    0,
    0,
    0};
CINTERFACE_PROXY_VTABLE(7) _IXRTLOPCDA20ServerProxyVtbl = 
{
    &IXRTLOPCDA20Server_ProxyInfo,
    &IID_IXRTLOPCDA20Server,
    IUnknown_QueryInterface_Proxy,
    IUnknown_AddRef_Proxy,
    IUnknown_Release_Proxy ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDA20Server::GetNameSpace */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDA20Server::SetNameSpace */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDA20Server::GetDataSource */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDA20Server::SetDataSource */
};

const CInterfaceStubVtbl _IXRTLOPCDA20ServerStubVtbl =
{
    &IID_IXRTLOPCDA20Server,
    &IXRTLOPCDA20Server_ServerInfo,
    7,
    0, /* pure interpreted */
    CStdStubBuffer_METHODS
};


/* Object interface: IXRTLOPCDA20Group, ver. 0.0,
   GUID={0xD2ADCF12,0x1553,0x4040,{0xAA,0x0F,0xB7,0x4E,0xDA,0x6F,0xE1,0xD2}} */

#pragma code_seg(".orpc")
static const unsigned short IXRTLOPCDA20Group_FormatStringOffsetTable[] =
    {
    124,
    152
    };

static const MIDL_STUBLESS_PROXY_INFO IXRTLOPCDA20Group_ProxyInfo =
    {
    &Object_StubDesc,
    __MIDL_ProcFormatString.Format,
    &IXRTLOPCDA20Group_FormatStringOffsetTable[-3],
    0,
    0,
    0
    };


static const MIDL_SERVER_INFO IXRTLOPCDA20Group_ServerInfo = 
    {
    &Object_StubDesc,
    0,
    __MIDL_ProcFormatString.Format,
    &IXRTLOPCDA20Group_FormatStringOffsetTable[-3],
    0,
    0,
    0,
    0};
CINTERFACE_PROXY_VTABLE(5) _IXRTLOPCDA20GroupProxyVtbl = 
{
    &IXRTLOPCDA20Group_ProxyInfo,
    &IID_IXRTLOPCDA20Group,
    IUnknown_QueryInterface_Proxy,
    IUnknown_AddRef_Proxy,
    IUnknown_Release_Proxy ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDA20Group::SetDeleted */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDA20Group::GetDeleted */
};

const CInterfaceStubVtbl _IXRTLOPCDA20GroupStubVtbl =
{
    &IID_IXRTLOPCDA20Group,
    &IXRTLOPCDA20Group_ServerInfo,
    5,
    0, /* pure interpreted */
    CStdStubBuffer_METHODS
};


/* Object interface: IXRTLOPCDANameSpace, ver. 0.0,
   GUID={0xEF4B587B,0x7426,0x43C7,{0x9C,0x6F,0xA5,0x13,0xE1,0x36,0x71,0x98}} */

#pragma code_seg(".orpc")
static const unsigned short IXRTLOPCDANameSpace_FormatStringOffsetTable[] =
    {
    180,
    208,
    236,
    264,
    310,
    344,
    378,
    406,
    428,
    486,
    514,
    542,
    576,
    610,
    644
    };

static const MIDL_STUBLESS_PROXY_INFO IXRTLOPCDANameSpace_ProxyInfo =
    {
    &Object_StubDesc,
    __MIDL_ProcFormatString.Format,
    &IXRTLOPCDANameSpace_FormatStringOffsetTable[-3],
    0,
    0,
    0
    };


static const MIDL_SERVER_INFO IXRTLOPCDANameSpace_ServerInfo = 
    {
    &Object_StubDesc,
    0,
    __MIDL_ProcFormatString.Format,
    &IXRTLOPCDANameSpace_FormatStringOffsetTable[-3],
    0,
    0,
    0,
    0};
CINTERFACE_PROXY_VTABLE(18) _IXRTLOPCDANameSpaceProxyVtbl = 
{
    &IXRTLOPCDANameSpace_ProxyInfo,
    &IID_IXRTLOPCDANameSpace,
    IUnknown_QueryInterface_Proxy,
    IUnknown_AddRef_Proxy,
    IUnknown_Release_Proxy ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDANameSpace::GetOrganization */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDANameSpace::SetOrganization */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDANameSpace::CheckPath */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDANameSpace::AddItem */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDANameSpace::GetItem */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDANameSpace::HasItem */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDANameSpace::RemoveItem */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDANameSpace::Clear */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDANameSpace::CreateItemEnumerator */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDANameSpace::GetItemIDSeparator */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDANameSpace::SetItemIDSeparator */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDANameSpace::CombineItemID */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDANameSpace::SplitItemID */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDANameSpace::IsLeaf */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDANameSpace::IsBranch */
};

const CInterfaceStubVtbl _IXRTLOPCDANameSpaceStubVtbl =
{
    &IID_IXRTLOPCDANameSpace,
    &IXRTLOPCDANameSpace_ServerInfo,
    18,
    0, /* pure interpreted */
    CStdStubBuffer_METHODS
};


/* Object interface: IXRTLOPCDANameSpaceItem, ver. 0.0,
   GUID={0x3C914D07,0x3FE2,0x4339,{0xB5,0x62,0x6D,0x84,0x91,0x77,0xF7,0x87}} */

#pragma code_seg(".orpc")
static const unsigned short IXRTLOPCDANameSpaceItem_FormatStringOffsetTable[] =
    {
    678,
    724,
    770,
    816,
    844,
    872,
    900,
    928,
    956,
    486
    };

static const MIDL_STUBLESS_PROXY_INFO IXRTLOPCDANameSpaceItem_ProxyInfo =
    {
    &Object_StubDesc,
    __MIDL_ProcFormatString.Format,
    &IXRTLOPCDANameSpaceItem_FormatStringOffsetTable[-3],
    0,
    0,
    0
    };


static const MIDL_SERVER_INFO IXRTLOPCDANameSpaceItem_ServerInfo = 
    {
    &Object_StubDesc,
    0,
    __MIDL_ProcFormatString.Format,
    &IXRTLOPCDANameSpaceItem_FormatStringOffsetTable[-3],
    0,
    0,
    0,
    0};
CINTERFACE_PROXY_VTABLE(13) _IXRTLOPCDANameSpaceItemProxyVtbl = 
{
    &IXRTLOPCDANameSpaceItem_ProxyInfo,
    &IID_IXRTLOPCDANameSpaceItem,
    IUnknown_QueryInterface_Proxy,
    IUnknown_AddRef_Proxy,
    IUnknown_Release_Proxy ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDANameSpaceItem::QueryAvailableProperties */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDANameSpaceItem::GetItemProperties */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDANameSpaceItem::LookupItemIDs */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDANameSpaceItem::GetAccessRights */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDANameSpaceItem::SetAccessRights */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDANameSpaceItem::GetDataType */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDANameSpaceItem::SetDataType */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDANameSpaceItem::CanRead */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDANameSpaceItem::CanWrite */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDANameSpaceItem::GetItemID */
};

const CInterfaceStubVtbl _IXRTLOPCDANameSpaceItemStubVtbl =
{
    &IID_IXRTLOPCDANameSpaceItem,
    &IXRTLOPCDANameSpaceItem_ServerInfo,
    13,
    0, /* pure interpreted */
    CStdStubBuffer_METHODS
};


/* Object interface: IXRTLOPCDADataSource, ver. 0.0,
   GUID={0x5C389437,0x7A4D,0x4DB0,{0x8E,0x02,0x15,0xFE,0x25,0x23,0xD9,0xB8}} */

#pragma code_seg(".orpc")
static const unsigned short IXRTLOPCDADataSource_FormatStringOffsetTable[] =
    {
    984,
    1030,
    1076,
    1104,
    1132,
    1160,
    1188
    };

static const MIDL_STUBLESS_PROXY_INFO IXRTLOPCDADataSource_ProxyInfo =
    {
    &Object_StubDesc,
    __MIDL_ProcFormatString.Format,
    &IXRTLOPCDADataSource_FormatStringOffsetTable[-3],
    0,
    0,
    0
    };


static const MIDL_SERVER_INFO IXRTLOPCDADataSource_ServerInfo = 
    {
    &Object_StubDesc,
    0,
    __MIDL_ProcFormatString.Format,
    &IXRTLOPCDADataSource_FormatStringOffsetTable[-3],
    0,
    0,
    0,
    0};
CINTERFACE_PROXY_VTABLE(10) _IXRTLOPCDADataSourceProxyVtbl = 
{
    &IXRTLOPCDADataSource_ProxyInfo,
    &IID_IXRTLOPCDADataSource,
    IUnknown_QueryInterface_Proxy,
    IUnknown_AddRef_Proxy,
    IUnknown_Release_Proxy ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDADataSource::Read */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDADataSource::Write */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDADataSource::CreateItemEnumerator */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDADataSource::Update */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDADataSource::AddItem */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDADataSource::RemoveItem */ ,
    (void *) (INT_PTR) -1 /* IXRTLOPCDADataSource::ClearItems */
};

const CInterfaceStubVtbl _IXRTLOPCDADataSourceStubVtbl =
{
    &IID_IXRTLOPCDADataSource,
    &IXRTLOPCDADataSource_ServerInfo,
    10,
    0, /* pure interpreted */
    CStdStubBuffer_METHODS
};

static const MIDL_STUB_DESC Object_StubDesc = 
    {
    0,
    NdrOleAllocate,
    NdrOleFree,
    0,
    0,
    0,
    0,
    0,
    __MIDL_TypeFormatString.Format,
    1, /* -error bounds_check flag */
    0x20000, /* Ndr library version */
    0,
    0x600015b, /* MIDL Version 6.0.347 */
    0,
    UserMarshalRoutines,
    0,  /* notify & notify_flag routine table */
    0x1, /* MIDL flag */
    0, /* cs routines */
    0,   /* proxy/server info */
    0   /* Reserved5 */
    };

const CInterfaceProxyVtbl * _XRTLOPCDA_ProxyVtblList[] = 
{
    ( CInterfaceProxyVtbl *) &_IXRTLOPCDANameSpaceItemProxyVtbl,
    ( CInterfaceProxyVtbl *) &_IXRTLOPCDA20GroupProxyVtbl,
    ( CInterfaceProxyVtbl *) &_IXRTLOPCDADataSourceProxyVtbl,
    ( CInterfaceProxyVtbl *) &_IXRTLOPCDA20ServerProxyVtbl,
    ( CInterfaceProxyVtbl *) &_IXRTLOPCDANameSpaceProxyVtbl,
    0
};

const CInterfaceStubVtbl * _XRTLOPCDA_StubVtblList[] = 
{
    ( CInterfaceStubVtbl *) &_IXRTLOPCDANameSpaceItemStubVtbl,
    ( CInterfaceStubVtbl *) &_IXRTLOPCDA20GroupStubVtbl,
    ( CInterfaceStubVtbl *) &_IXRTLOPCDADataSourceStubVtbl,
    ( CInterfaceStubVtbl *) &_IXRTLOPCDA20ServerStubVtbl,
    ( CInterfaceStubVtbl *) &_IXRTLOPCDANameSpaceStubVtbl,
    0
};

PCInterfaceName const _XRTLOPCDA_InterfaceNamesList[] = 
{
    "IXRTLOPCDANameSpaceItem",
    "IXRTLOPCDA20Group",
    "IXRTLOPCDADataSource",
    "IXRTLOPCDA20Server",
    "IXRTLOPCDANameSpace",
    0
};


#define _XRTLOPCDA_CHECK_IID(n)	IID_GENERIC_CHECK_IID( _XRTLOPCDA, pIID, n)

int __stdcall _XRTLOPCDA_IID_Lookup( const IID * pIID, int * pIndex )
{
    IID_BS_LOOKUP_SETUP

    IID_BS_LOOKUP_INITIAL_TEST( _XRTLOPCDA, 5, 4 )
    IID_BS_LOOKUP_NEXT_TEST( _XRTLOPCDA, 2 )
    IID_BS_LOOKUP_NEXT_TEST( _XRTLOPCDA, 1 )
    IID_BS_LOOKUP_RETURN_RESULT( _XRTLOPCDA, 5, *pIndex )
    
}

const ExtendedProxyFileInfo XRTLOPCDA_ProxyFileInfo = 
{
    (PCInterfaceProxyVtblList *) & _XRTLOPCDA_ProxyVtblList,
    (PCInterfaceStubVtblList *) & _XRTLOPCDA_StubVtblList,
    (const PCInterfaceName * ) & _XRTLOPCDA_InterfaceNamesList,
    0, // no delegation
    & _XRTLOPCDA_IID_Lookup, 
    5,
    2,
    0, /* table of [async_uuid] interfaces */
    0, /* Filler1 */
    0, /* Filler2 */
    0  /* Filler3 */
};


#endif /* !defined(_M_IA64) && !defined(_M_AMD64)*/

