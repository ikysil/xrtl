
#pragma warning( disable: 4049 )  /* more than 64k source lines */

/* this ALWAYS GENERATED file contains the definitions for the interfaces */


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


/* verify that the <rpcndr.h> version is high enough to compile this file*/
#ifndef __REQUIRED_RPCNDR_H_VERSION__
#define __REQUIRED_RPCNDR_H_VERSION__ 440
#endif

#include "rpc.h"
#include "rpcndr.h"

#ifndef __RPCNDR_H_VERSION__
#error this stub requires an updated version of <rpcndr.h>
#endif // __RPCNDR_H_VERSION__

#ifndef COM_NO_WINDOWS_H
#include "windows.h"
#include "ole2.h"
#endif /*COM_NO_WINDOWS_H*/

#ifndef __XRTLOPCDA_h__
#define __XRTLOPCDA_h__

#if defined(_MSC_VER) && (_MSC_VER >= 1020)
#pragma once
#endif

/* Forward Declarations */ 

#ifndef __IXRTLOPCDA20Server_FWD_DEFINED__
#define __IXRTLOPCDA20Server_FWD_DEFINED__
typedef interface IXRTLOPCDA20Server IXRTLOPCDA20Server;
#endif 	/* __IXRTLOPCDA20Server_FWD_DEFINED__ */


#ifndef __IXRTLOPCDA20Group_FWD_DEFINED__
#define __IXRTLOPCDA20Group_FWD_DEFINED__
typedef interface IXRTLOPCDA20Group IXRTLOPCDA20Group;
#endif 	/* __IXRTLOPCDA20Group_FWD_DEFINED__ */


#ifndef __IXRTLOPCDANameSpace_FWD_DEFINED__
#define __IXRTLOPCDANameSpace_FWD_DEFINED__
typedef interface IXRTLOPCDANameSpace IXRTLOPCDANameSpace;
#endif 	/* __IXRTLOPCDANameSpace_FWD_DEFINED__ */


#ifndef __IXRTLOPCDANameSpaceItem_FWD_DEFINED__
#define __IXRTLOPCDANameSpaceItem_FWD_DEFINED__
typedef interface IXRTLOPCDANameSpaceItem IXRTLOPCDANameSpaceItem;
#endif 	/* __IXRTLOPCDANameSpaceItem_FWD_DEFINED__ */


#ifndef __IXRTLOPCDADataSource_FWD_DEFINED__
#define __IXRTLOPCDADataSource_FWD_DEFINED__
typedef interface IXRTLOPCDADataSource IXRTLOPCDADataSource;
#endif 	/* __IXRTLOPCDADataSource_FWD_DEFINED__ */


#ifndef __IXRTLOPCDA20Server_FWD_DEFINED__
#define __IXRTLOPCDA20Server_FWD_DEFINED__
typedef interface IXRTLOPCDA20Server IXRTLOPCDA20Server;
#endif 	/* __IXRTLOPCDA20Server_FWD_DEFINED__ */


#ifndef __IXRTLOPCDA20Group_FWD_DEFINED__
#define __IXRTLOPCDA20Group_FWD_DEFINED__
typedef interface IXRTLOPCDA20Group IXRTLOPCDA20Group;
#endif 	/* __IXRTLOPCDA20Group_FWD_DEFINED__ */


#ifndef __IXRTLOPCDADataSource_FWD_DEFINED__
#define __IXRTLOPCDADataSource_FWD_DEFINED__
typedef interface IXRTLOPCDADataSource IXRTLOPCDADataSource;
#endif 	/* __IXRTLOPCDADataSource_FWD_DEFINED__ */


/* header files for imported files */
#include "oaidl.h"
#include "opcda.h"

#ifdef __cplusplus
extern "C"{
#endif 

void * __RPC_USER MIDL_user_allocate(size_t);
void __RPC_USER MIDL_user_free( void * ); 

/* interface __MIDL_itf_XRTLOPCDA_0000 */
/* [local] */ 






typedef IXRTLOPCDANameSpace *LPXRTLOPCDANameSpace;

typedef IXRTLOPCDANameSpaceItem *LPXRTLOPCDANameSpaceItem;

typedef IXRTLOPCDADataSource *LPXRTLOPCDADataSource;



extern RPC_IF_HANDLE __MIDL_itf_XRTLOPCDA_0000_v0_0_c_ifspec;
extern RPC_IF_HANDLE __MIDL_itf_XRTLOPCDA_0000_v0_0_s_ifspec;

#ifndef __IXRTLOPCDA20Server_INTERFACE_DEFINED__
#define __IXRTLOPCDA20Server_INTERFACE_DEFINED__

/* interface IXRTLOPCDA20Server */
/* [unique][uuid][object] */ 


EXTERN_C const IID IID_IXRTLOPCDA20Server;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C9108B6D-B9A7-4396-BF9F-55F24849E920")
    IXRTLOPCDA20Server : public IUnknown
    {
    public:
        virtual HRESULT STDMETHODCALLTYPE GetNameSpace( 
            /* [out] */ LPXRTLOPCDANameSpace *ppNameSpace) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE SetNameSpace( 
            /* [in] */ LPXRTLOPCDANameSpace pNameSpace) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE GetDataSource( 
            /* [in] */ OPCDATASOURCE dwSource,
            /* [out] */ LPXRTLOPCDADataSource *ppDataSource) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE SetDataSource( 
            /* [in] */ OPCDATASOURCE dwSource,
            /* [in] */ LPXRTLOPCDADataSource pDataSource) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IXRTLOPCDA20ServerVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IXRTLOPCDA20Server * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IXRTLOPCDA20Server * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IXRTLOPCDA20Server * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetNameSpace )( 
            IXRTLOPCDA20Server * This,
            /* [out] */ LPXRTLOPCDANameSpace *ppNameSpace);
        
        HRESULT ( STDMETHODCALLTYPE *SetNameSpace )( 
            IXRTLOPCDA20Server * This,
            /* [in] */ LPXRTLOPCDANameSpace pNameSpace);
        
        HRESULT ( STDMETHODCALLTYPE *GetDataSource )( 
            IXRTLOPCDA20Server * This,
            /* [in] */ OPCDATASOURCE dwSource,
            /* [out] */ LPXRTLOPCDADataSource *ppDataSource);
        
        HRESULT ( STDMETHODCALLTYPE *SetDataSource )( 
            IXRTLOPCDA20Server * This,
            /* [in] */ OPCDATASOURCE dwSource,
            /* [in] */ LPXRTLOPCDADataSource pDataSource);
        
        END_INTERFACE
    } IXRTLOPCDA20ServerVtbl;

    interface IXRTLOPCDA20Server
    {
        CONST_VTBL struct IXRTLOPCDA20ServerVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IXRTLOPCDA20Server_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IXRTLOPCDA20Server_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IXRTLOPCDA20Server_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IXRTLOPCDA20Server_GetNameSpace(This,ppNameSpace)	\
    (This)->lpVtbl -> GetNameSpace(This,ppNameSpace)

#define IXRTLOPCDA20Server_SetNameSpace(This,pNameSpace)	\
    (This)->lpVtbl -> SetNameSpace(This,pNameSpace)

#define IXRTLOPCDA20Server_GetDataSource(This,dwSource,ppDataSource)	\
    (This)->lpVtbl -> GetDataSource(This,dwSource,ppDataSource)

#define IXRTLOPCDA20Server_SetDataSource(This,dwSource,pDataSource)	\
    (This)->lpVtbl -> SetDataSource(This,dwSource,pDataSource)

#endif /* COBJMACROS */


#endif 	/* C style interface */



HRESULT STDMETHODCALLTYPE IXRTLOPCDA20Server_GetNameSpace_Proxy( 
    IXRTLOPCDA20Server * This,
    /* [out] */ LPXRTLOPCDANameSpace *ppNameSpace);


void __RPC_STUB IXRTLOPCDA20Server_GetNameSpace_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDA20Server_SetNameSpace_Proxy( 
    IXRTLOPCDA20Server * This,
    /* [in] */ LPXRTLOPCDANameSpace pNameSpace);


void __RPC_STUB IXRTLOPCDA20Server_SetNameSpace_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDA20Server_GetDataSource_Proxy( 
    IXRTLOPCDA20Server * This,
    /* [in] */ OPCDATASOURCE dwSource,
    /* [out] */ LPXRTLOPCDADataSource *ppDataSource);


void __RPC_STUB IXRTLOPCDA20Server_GetDataSource_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDA20Server_SetDataSource_Proxy( 
    IXRTLOPCDA20Server * This,
    /* [in] */ OPCDATASOURCE dwSource,
    /* [in] */ LPXRTLOPCDADataSource pDataSource);


void __RPC_STUB IXRTLOPCDA20Server_SetDataSource_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IXRTLOPCDA20Server_INTERFACE_DEFINED__ */


#ifndef __IXRTLOPCDA20Group_INTERFACE_DEFINED__
#define __IXRTLOPCDA20Group_INTERFACE_DEFINED__

/* interface IXRTLOPCDA20Group */
/* [unique][uuid][object] */ 


EXTERN_C const IID IID_IXRTLOPCDA20Group;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("D2ADCF12-1553-4040-AA0F-B74EDA6FE1D2")
    IXRTLOPCDA20Group : public IUnknown
    {
    public:
        virtual HRESULT STDMETHODCALLTYPE SetDeleted( 
            /* [in] */ BOOL bFlag) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE GetDeleted( 
            /* [out] */ BOOL *pbFlag) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IXRTLOPCDA20GroupVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IXRTLOPCDA20Group * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IXRTLOPCDA20Group * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IXRTLOPCDA20Group * This);
        
        HRESULT ( STDMETHODCALLTYPE *SetDeleted )( 
            IXRTLOPCDA20Group * This,
            /* [in] */ BOOL bFlag);
        
        HRESULT ( STDMETHODCALLTYPE *GetDeleted )( 
            IXRTLOPCDA20Group * This,
            /* [out] */ BOOL *pbFlag);
        
        END_INTERFACE
    } IXRTLOPCDA20GroupVtbl;

    interface IXRTLOPCDA20Group
    {
        CONST_VTBL struct IXRTLOPCDA20GroupVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IXRTLOPCDA20Group_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IXRTLOPCDA20Group_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IXRTLOPCDA20Group_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IXRTLOPCDA20Group_SetDeleted(This,bFlag)	\
    (This)->lpVtbl -> SetDeleted(This,bFlag)

#define IXRTLOPCDA20Group_GetDeleted(This,pbFlag)	\
    (This)->lpVtbl -> GetDeleted(This,pbFlag)

#endif /* COBJMACROS */


#endif 	/* C style interface */



HRESULT STDMETHODCALLTYPE IXRTLOPCDA20Group_SetDeleted_Proxy( 
    IXRTLOPCDA20Group * This,
    /* [in] */ BOOL bFlag);


void __RPC_STUB IXRTLOPCDA20Group_SetDeleted_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDA20Group_GetDeleted_Proxy( 
    IXRTLOPCDA20Group * This,
    /* [out] */ BOOL *pbFlag);


void __RPC_STUB IXRTLOPCDA20Group_GetDeleted_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IXRTLOPCDA20Group_INTERFACE_DEFINED__ */


#ifndef __IXRTLOPCDANameSpace_INTERFACE_DEFINED__
#define __IXRTLOPCDANameSpace_INTERFACE_DEFINED__

/* interface IXRTLOPCDANameSpace */
/* [unique][uuid][object] */ 


EXTERN_C const IID IID_IXRTLOPCDANameSpace;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("EF4B587B-7426-43C7-9C6F-A513E1367198")
    IXRTLOPCDANameSpace : public IUnknown
    {
    public:
        virtual HRESULT STDMETHODCALLTYPE GetOrganization( 
            /* [out] */ OPCNAMESPACETYPE *pNameSpaceType) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE SetOrganization( 
            /* [in] */ OPCNAMESPACETYPE NameSpaceType) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE CheckPath( 
            /* [in] */ VARIANT vItemIDChunks) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE AddItem( 
            /* [in] */ VARIANT vItemIDChunks,
            /* [string][in] */ LPOLESTR pszDescription,
            /* [in] */ VARTYPE vtDataType,
            /* [out] */ LPXRTLOPCDANameSpaceItem *ppItem) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE GetItem( 
            /* [in] */ VARIANT vItemIDChunks,
            /* [out] */ LPXRTLOPCDANameSpaceItem *ppItem) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE HasItem( 
            /* [in] */ VARIANT vItemIDChunks,
            /* [out] */ BOOL *pbResult) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE RemoveItem( 
            /* [in] */ VARIANT vItemIDChunks) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE Clear( void) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE CreateItemEnumerator( 
            /* [in] */ VARIANT vRootItemIDChunks,
            /* [in] */ OPCBROWSETYPE dwBrowseFilterType,
            /* [string][in] */ LPOLESTR szFilterCriteria,
            /* [in] */ VARTYPE vtDataTypeFilter,
            /* [in] */ DWORD dwAccessRightsFilter,
            /* [out] */ LPENUMSTRING *ppIEnumString) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE GetItemIDSeparator( 
            /* [string][out] */ LPWSTR *pszItemIDSeparator) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE SetItemIDSeparator( 
            /* [string][in] */ LPWSTR szItemIDSeparator) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE CombineItemID( 
            /* [in] */ VARIANT vItemIDChunks,
            /* [string][out] */ LPWSTR *pszItemID) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE SplitItemID( 
            /* [string][in] */ LPWSTR szItemID,
            /* [out] */ VARIANT *pvItemIDChunks) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE IsLeaf( 
            /* [in] */ VARIANT vItemIDChunks,
            /* [out] */ BOOL *pbResult) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE IsBranch( 
            /* [in] */ VARIANT vItemIDChunks,
            /* [out] */ BOOL *pbResult) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IXRTLOPCDANameSpaceVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IXRTLOPCDANameSpace * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IXRTLOPCDANameSpace * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IXRTLOPCDANameSpace * This);
        
        HRESULT ( STDMETHODCALLTYPE *GetOrganization )( 
            IXRTLOPCDANameSpace * This,
            /* [out] */ OPCNAMESPACETYPE *pNameSpaceType);
        
        HRESULT ( STDMETHODCALLTYPE *SetOrganization )( 
            IXRTLOPCDANameSpace * This,
            /* [in] */ OPCNAMESPACETYPE NameSpaceType);
        
        HRESULT ( STDMETHODCALLTYPE *CheckPath )( 
            IXRTLOPCDANameSpace * This,
            /* [in] */ VARIANT vItemIDChunks);
        
        HRESULT ( STDMETHODCALLTYPE *AddItem )( 
            IXRTLOPCDANameSpace * This,
            /* [in] */ VARIANT vItemIDChunks,
            /* [string][in] */ LPOLESTR pszDescription,
            /* [in] */ VARTYPE vtDataType,
            /* [out] */ LPXRTLOPCDANameSpaceItem *ppItem);
        
        HRESULT ( STDMETHODCALLTYPE *GetItem )( 
            IXRTLOPCDANameSpace * This,
            /* [in] */ VARIANT vItemIDChunks,
            /* [out] */ LPXRTLOPCDANameSpaceItem *ppItem);
        
        HRESULT ( STDMETHODCALLTYPE *HasItem )( 
            IXRTLOPCDANameSpace * This,
            /* [in] */ VARIANT vItemIDChunks,
            /* [out] */ BOOL *pbResult);
        
        HRESULT ( STDMETHODCALLTYPE *RemoveItem )( 
            IXRTLOPCDANameSpace * This,
            /* [in] */ VARIANT vItemIDChunks);
        
        HRESULT ( STDMETHODCALLTYPE *Clear )( 
            IXRTLOPCDANameSpace * This);
        
        HRESULT ( STDMETHODCALLTYPE *CreateItemEnumerator )( 
            IXRTLOPCDANameSpace * This,
            /* [in] */ VARIANT vRootItemIDChunks,
            /* [in] */ OPCBROWSETYPE dwBrowseFilterType,
            /* [string][in] */ LPOLESTR szFilterCriteria,
            /* [in] */ VARTYPE vtDataTypeFilter,
            /* [in] */ DWORD dwAccessRightsFilter,
            /* [out] */ LPENUMSTRING *ppIEnumString);
        
        HRESULT ( STDMETHODCALLTYPE *GetItemIDSeparator )( 
            IXRTLOPCDANameSpace * This,
            /* [string][out] */ LPWSTR *pszItemIDSeparator);
        
        HRESULT ( STDMETHODCALLTYPE *SetItemIDSeparator )( 
            IXRTLOPCDANameSpace * This,
            /* [string][in] */ LPWSTR szItemIDSeparator);
        
        HRESULT ( STDMETHODCALLTYPE *CombineItemID )( 
            IXRTLOPCDANameSpace * This,
            /* [in] */ VARIANT vItemIDChunks,
            /* [string][out] */ LPWSTR *pszItemID);
        
        HRESULT ( STDMETHODCALLTYPE *SplitItemID )( 
            IXRTLOPCDANameSpace * This,
            /* [string][in] */ LPWSTR szItemID,
            /* [out] */ VARIANT *pvItemIDChunks);
        
        HRESULT ( STDMETHODCALLTYPE *IsLeaf )( 
            IXRTLOPCDANameSpace * This,
            /* [in] */ VARIANT vItemIDChunks,
            /* [out] */ BOOL *pbResult);
        
        HRESULT ( STDMETHODCALLTYPE *IsBranch )( 
            IXRTLOPCDANameSpace * This,
            /* [in] */ VARIANT vItemIDChunks,
            /* [out] */ BOOL *pbResult);
        
        END_INTERFACE
    } IXRTLOPCDANameSpaceVtbl;

    interface IXRTLOPCDANameSpace
    {
        CONST_VTBL struct IXRTLOPCDANameSpaceVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IXRTLOPCDANameSpace_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IXRTLOPCDANameSpace_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IXRTLOPCDANameSpace_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IXRTLOPCDANameSpace_GetOrganization(This,pNameSpaceType)	\
    (This)->lpVtbl -> GetOrganization(This,pNameSpaceType)

#define IXRTLOPCDANameSpace_SetOrganization(This,NameSpaceType)	\
    (This)->lpVtbl -> SetOrganization(This,NameSpaceType)

#define IXRTLOPCDANameSpace_CheckPath(This,vItemIDChunks)	\
    (This)->lpVtbl -> CheckPath(This,vItemIDChunks)

#define IXRTLOPCDANameSpace_AddItem(This,vItemIDChunks,pszDescription,vtDataType,ppItem)	\
    (This)->lpVtbl -> AddItem(This,vItemIDChunks,pszDescription,vtDataType,ppItem)

#define IXRTLOPCDANameSpace_GetItem(This,vItemIDChunks,ppItem)	\
    (This)->lpVtbl -> GetItem(This,vItemIDChunks,ppItem)

#define IXRTLOPCDANameSpace_HasItem(This,vItemIDChunks,pbResult)	\
    (This)->lpVtbl -> HasItem(This,vItemIDChunks,pbResult)

#define IXRTLOPCDANameSpace_RemoveItem(This,vItemIDChunks)	\
    (This)->lpVtbl -> RemoveItem(This,vItemIDChunks)

#define IXRTLOPCDANameSpace_Clear(This)	\
    (This)->lpVtbl -> Clear(This)

#define IXRTLOPCDANameSpace_CreateItemEnumerator(This,vRootItemIDChunks,dwBrowseFilterType,szFilterCriteria,vtDataTypeFilter,dwAccessRightsFilter,ppIEnumString)	\
    (This)->lpVtbl -> CreateItemEnumerator(This,vRootItemIDChunks,dwBrowseFilterType,szFilterCriteria,vtDataTypeFilter,dwAccessRightsFilter,ppIEnumString)

#define IXRTLOPCDANameSpace_GetItemIDSeparator(This,pszItemIDSeparator)	\
    (This)->lpVtbl -> GetItemIDSeparator(This,pszItemIDSeparator)

#define IXRTLOPCDANameSpace_SetItemIDSeparator(This,szItemIDSeparator)	\
    (This)->lpVtbl -> SetItemIDSeparator(This,szItemIDSeparator)

#define IXRTLOPCDANameSpace_CombineItemID(This,vItemIDChunks,pszItemID)	\
    (This)->lpVtbl -> CombineItemID(This,vItemIDChunks,pszItemID)

#define IXRTLOPCDANameSpace_SplitItemID(This,szItemID,pvItemIDChunks)	\
    (This)->lpVtbl -> SplitItemID(This,szItemID,pvItemIDChunks)

#define IXRTLOPCDANameSpace_IsLeaf(This,vItemIDChunks,pbResult)	\
    (This)->lpVtbl -> IsLeaf(This,vItemIDChunks,pbResult)

#define IXRTLOPCDANameSpace_IsBranch(This,vItemIDChunks,pbResult)	\
    (This)->lpVtbl -> IsBranch(This,vItemIDChunks,pbResult)

#endif /* COBJMACROS */


#endif 	/* C style interface */



HRESULT STDMETHODCALLTYPE IXRTLOPCDANameSpace_GetOrganization_Proxy( 
    IXRTLOPCDANameSpace * This,
    /* [out] */ OPCNAMESPACETYPE *pNameSpaceType);


void __RPC_STUB IXRTLOPCDANameSpace_GetOrganization_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDANameSpace_SetOrganization_Proxy( 
    IXRTLOPCDANameSpace * This,
    /* [in] */ OPCNAMESPACETYPE NameSpaceType);


void __RPC_STUB IXRTLOPCDANameSpace_SetOrganization_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDANameSpace_CheckPath_Proxy( 
    IXRTLOPCDANameSpace * This,
    /* [in] */ VARIANT vItemIDChunks);


void __RPC_STUB IXRTLOPCDANameSpace_CheckPath_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDANameSpace_AddItem_Proxy( 
    IXRTLOPCDANameSpace * This,
    /* [in] */ VARIANT vItemIDChunks,
    /* [string][in] */ LPOLESTR pszDescription,
    /* [in] */ VARTYPE vtDataType,
    /* [out] */ LPXRTLOPCDANameSpaceItem *ppItem);


void __RPC_STUB IXRTLOPCDANameSpace_AddItem_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDANameSpace_GetItem_Proxy( 
    IXRTLOPCDANameSpace * This,
    /* [in] */ VARIANT vItemIDChunks,
    /* [out] */ LPXRTLOPCDANameSpaceItem *ppItem);


void __RPC_STUB IXRTLOPCDANameSpace_GetItem_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDANameSpace_HasItem_Proxy( 
    IXRTLOPCDANameSpace * This,
    /* [in] */ VARIANT vItemIDChunks,
    /* [out] */ BOOL *pbResult);


void __RPC_STUB IXRTLOPCDANameSpace_HasItem_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDANameSpace_RemoveItem_Proxy( 
    IXRTLOPCDANameSpace * This,
    /* [in] */ VARIANT vItemIDChunks);


void __RPC_STUB IXRTLOPCDANameSpace_RemoveItem_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDANameSpace_Clear_Proxy( 
    IXRTLOPCDANameSpace * This);


void __RPC_STUB IXRTLOPCDANameSpace_Clear_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDANameSpace_CreateItemEnumerator_Proxy( 
    IXRTLOPCDANameSpace * This,
    /* [in] */ VARIANT vRootItemIDChunks,
    /* [in] */ OPCBROWSETYPE dwBrowseFilterType,
    /* [string][in] */ LPOLESTR szFilterCriteria,
    /* [in] */ VARTYPE vtDataTypeFilter,
    /* [in] */ DWORD dwAccessRightsFilter,
    /* [out] */ LPENUMSTRING *ppIEnumString);


void __RPC_STUB IXRTLOPCDANameSpace_CreateItemEnumerator_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDANameSpace_GetItemIDSeparator_Proxy( 
    IXRTLOPCDANameSpace * This,
    /* [string][out] */ LPWSTR *pszItemIDSeparator);


void __RPC_STUB IXRTLOPCDANameSpace_GetItemIDSeparator_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDANameSpace_SetItemIDSeparator_Proxy( 
    IXRTLOPCDANameSpace * This,
    /* [string][in] */ LPWSTR szItemIDSeparator);


void __RPC_STUB IXRTLOPCDANameSpace_SetItemIDSeparator_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDANameSpace_CombineItemID_Proxy( 
    IXRTLOPCDANameSpace * This,
    /* [in] */ VARIANT vItemIDChunks,
    /* [string][out] */ LPWSTR *pszItemID);


void __RPC_STUB IXRTLOPCDANameSpace_CombineItemID_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDANameSpace_SplitItemID_Proxy( 
    IXRTLOPCDANameSpace * This,
    /* [string][in] */ LPWSTR szItemID,
    /* [out] */ VARIANT *pvItemIDChunks);


void __RPC_STUB IXRTLOPCDANameSpace_SplitItemID_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDANameSpace_IsLeaf_Proxy( 
    IXRTLOPCDANameSpace * This,
    /* [in] */ VARIANT vItemIDChunks,
    /* [out] */ BOOL *pbResult);


void __RPC_STUB IXRTLOPCDANameSpace_IsLeaf_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDANameSpace_IsBranch_Proxy( 
    IXRTLOPCDANameSpace * This,
    /* [in] */ VARIANT vItemIDChunks,
    /* [out] */ BOOL *pbResult);


void __RPC_STUB IXRTLOPCDANameSpace_IsBranch_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IXRTLOPCDANameSpace_INTERFACE_DEFINED__ */


#ifndef __IXRTLOPCDANameSpaceItem_INTERFACE_DEFINED__
#define __IXRTLOPCDANameSpaceItem_INTERFACE_DEFINED__

/* interface IXRTLOPCDANameSpaceItem */
/* [unique][uuid][object] */ 


EXTERN_C const IID IID_IXRTLOPCDANameSpaceItem;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("3C914D07-3FE2-4339-B562-6D849177F787")
    IXRTLOPCDANameSpaceItem : public IUnknown
    {
    public:
        virtual HRESULT STDMETHODCALLTYPE QueryAvailableProperties( 
            /* [out] */ DWORD *pdwCount,
            /* [size_is][size_is][out] */ DWORD **ppPropertyIDs,
            /* [size_is][size_is][out] */ LPWSTR **ppDescriptions,
            /* [size_is][size_is][out] */ VARTYPE **ppvtDataTypes) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE GetItemProperties( 
            /* [in] */ DWORD dwCount,
            /* [size_is][in] */ DWORD *pdwPropertyIDs,
            /* [size_is][size_is][out] */ VARIANT **ppvData,
            /* [size_is][size_is][out] */ HRESULT **ppErrors) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE LookupItemIDs( 
            /* [in] */ DWORD dwCount,
            /* [size_is][in] */ DWORD *pdwPropertyIDs,
            /* [size_is][size_is][string][out] */ LPWSTR **ppszNewItemIDs,
            /* [size_is][size_is][out] */ HRESULT **ppErrors) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE GetAccessRights( 
            /* [out] */ DWORD *pdwAccessRights) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE SetAccessRights( 
            /* [in] */ DWORD dwAccessRights) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE GetDataType( 
            /* [out] */ VARTYPE *pvtDataType) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE SetDataType( 
            /* [in] */ VARTYPE vtDataType) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE CanRead( 
            /* [out] */ BOOL *pbResult) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE CanWrite( 
            /* [out] */ BOOL *pbResult) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE GetItemID( 
            /* [string][out] */ LPWSTR *pszItemID) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IXRTLOPCDANameSpaceItemVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IXRTLOPCDANameSpaceItem * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IXRTLOPCDANameSpaceItem * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IXRTLOPCDANameSpaceItem * This);
        
        HRESULT ( STDMETHODCALLTYPE *QueryAvailableProperties )( 
            IXRTLOPCDANameSpaceItem * This,
            /* [out] */ DWORD *pdwCount,
            /* [size_is][size_is][out] */ DWORD **ppPropertyIDs,
            /* [size_is][size_is][out] */ LPWSTR **ppDescriptions,
            /* [size_is][size_is][out] */ VARTYPE **ppvtDataTypes);
        
        HRESULT ( STDMETHODCALLTYPE *GetItemProperties )( 
            IXRTLOPCDANameSpaceItem * This,
            /* [in] */ DWORD dwCount,
            /* [size_is][in] */ DWORD *pdwPropertyIDs,
            /* [size_is][size_is][out] */ VARIANT **ppvData,
            /* [size_is][size_is][out] */ HRESULT **ppErrors);
        
        HRESULT ( STDMETHODCALLTYPE *LookupItemIDs )( 
            IXRTLOPCDANameSpaceItem * This,
            /* [in] */ DWORD dwCount,
            /* [size_is][in] */ DWORD *pdwPropertyIDs,
            /* [size_is][size_is][string][out] */ LPWSTR **ppszNewItemIDs,
            /* [size_is][size_is][out] */ HRESULT **ppErrors);
        
        HRESULT ( STDMETHODCALLTYPE *GetAccessRights )( 
            IXRTLOPCDANameSpaceItem * This,
            /* [out] */ DWORD *pdwAccessRights);
        
        HRESULT ( STDMETHODCALLTYPE *SetAccessRights )( 
            IXRTLOPCDANameSpaceItem * This,
            /* [in] */ DWORD dwAccessRights);
        
        HRESULT ( STDMETHODCALLTYPE *GetDataType )( 
            IXRTLOPCDANameSpaceItem * This,
            /* [out] */ VARTYPE *pvtDataType);
        
        HRESULT ( STDMETHODCALLTYPE *SetDataType )( 
            IXRTLOPCDANameSpaceItem * This,
            /* [in] */ VARTYPE vtDataType);
        
        HRESULT ( STDMETHODCALLTYPE *CanRead )( 
            IXRTLOPCDANameSpaceItem * This,
            /* [out] */ BOOL *pbResult);
        
        HRESULT ( STDMETHODCALLTYPE *CanWrite )( 
            IXRTLOPCDANameSpaceItem * This,
            /* [out] */ BOOL *pbResult);
        
        HRESULT ( STDMETHODCALLTYPE *GetItemID )( 
            IXRTLOPCDANameSpaceItem * This,
            /* [string][out] */ LPWSTR *pszItemID);
        
        END_INTERFACE
    } IXRTLOPCDANameSpaceItemVtbl;

    interface IXRTLOPCDANameSpaceItem
    {
        CONST_VTBL struct IXRTLOPCDANameSpaceItemVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IXRTLOPCDANameSpaceItem_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IXRTLOPCDANameSpaceItem_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IXRTLOPCDANameSpaceItem_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IXRTLOPCDANameSpaceItem_QueryAvailableProperties(This,pdwCount,ppPropertyIDs,ppDescriptions,ppvtDataTypes)	\
    (This)->lpVtbl -> QueryAvailableProperties(This,pdwCount,ppPropertyIDs,ppDescriptions,ppvtDataTypes)

#define IXRTLOPCDANameSpaceItem_GetItemProperties(This,dwCount,pdwPropertyIDs,ppvData,ppErrors)	\
    (This)->lpVtbl -> GetItemProperties(This,dwCount,pdwPropertyIDs,ppvData,ppErrors)

#define IXRTLOPCDANameSpaceItem_LookupItemIDs(This,dwCount,pdwPropertyIDs,ppszNewItemIDs,ppErrors)	\
    (This)->lpVtbl -> LookupItemIDs(This,dwCount,pdwPropertyIDs,ppszNewItemIDs,ppErrors)

#define IXRTLOPCDANameSpaceItem_GetAccessRights(This,pdwAccessRights)	\
    (This)->lpVtbl -> GetAccessRights(This,pdwAccessRights)

#define IXRTLOPCDANameSpaceItem_SetAccessRights(This,dwAccessRights)	\
    (This)->lpVtbl -> SetAccessRights(This,dwAccessRights)

#define IXRTLOPCDANameSpaceItem_GetDataType(This,pvtDataType)	\
    (This)->lpVtbl -> GetDataType(This,pvtDataType)

#define IXRTLOPCDANameSpaceItem_SetDataType(This,vtDataType)	\
    (This)->lpVtbl -> SetDataType(This,vtDataType)

#define IXRTLOPCDANameSpaceItem_CanRead(This,pbResult)	\
    (This)->lpVtbl -> CanRead(This,pbResult)

#define IXRTLOPCDANameSpaceItem_CanWrite(This,pbResult)	\
    (This)->lpVtbl -> CanWrite(This,pbResult)

#define IXRTLOPCDANameSpaceItem_GetItemID(This,pszItemID)	\
    (This)->lpVtbl -> GetItemID(This,pszItemID)

#endif /* COBJMACROS */


#endif 	/* C style interface */



HRESULT STDMETHODCALLTYPE IXRTLOPCDANameSpaceItem_QueryAvailableProperties_Proxy( 
    IXRTLOPCDANameSpaceItem * This,
    /* [out] */ DWORD *pdwCount,
    /* [size_is][size_is][out] */ DWORD **ppPropertyIDs,
    /* [size_is][size_is][out] */ LPWSTR **ppDescriptions,
    /* [size_is][size_is][out] */ VARTYPE **ppvtDataTypes);


void __RPC_STUB IXRTLOPCDANameSpaceItem_QueryAvailableProperties_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDANameSpaceItem_GetItemProperties_Proxy( 
    IXRTLOPCDANameSpaceItem * This,
    /* [in] */ DWORD dwCount,
    /* [size_is][in] */ DWORD *pdwPropertyIDs,
    /* [size_is][size_is][out] */ VARIANT **ppvData,
    /* [size_is][size_is][out] */ HRESULT **ppErrors);


void __RPC_STUB IXRTLOPCDANameSpaceItem_GetItemProperties_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDANameSpaceItem_LookupItemIDs_Proxy( 
    IXRTLOPCDANameSpaceItem * This,
    /* [in] */ DWORD dwCount,
    /* [size_is][in] */ DWORD *pdwPropertyIDs,
    /* [size_is][size_is][string][out] */ LPWSTR **ppszNewItemIDs,
    /* [size_is][size_is][out] */ HRESULT **ppErrors);


void __RPC_STUB IXRTLOPCDANameSpaceItem_LookupItemIDs_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDANameSpaceItem_GetAccessRights_Proxy( 
    IXRTLOPCDANameSpaceItem * This,
    /* [out] */ DWORD *pdwAccessRights);


void __RPC_STUB IXRTLOPCDANameSpaceItem_GetAccessRights_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDANameSpaceItem_SetAccessRights_Proxy( 
    IXRTLOPCDANameSpaceItem * This,
    /* [in] */ DWORD dwAccessRights);


void __RPC_STUB IXRTLOPCDANameSpaceItem_SetAccessRights_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDANameSpaceItem_GetDataType_Proxy( 
    IXRTLOPCDANameSpaceItem * This,
    /* [out] */ VARTYPE *pvtDataType);


void __RPC_STUB IXRTLOPCDANameSpaceItem_GetDataType_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDANameSpaceItem_SetDataType_Proxy( 
    IXRTLOPCDANameSpaceItem * This,
    /* [in] */ VARTYPE vtDataType);


void __RPC_STUB IXRTLOPCDANameSpaceItem_SetDataType_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDANameSpaceItem_CanRead_Proxy( 
    IXRTLOPCDANameSpaceItem * This,
    /* [out] */ BOOL *pbResult);


void __RPC_STUB IXRTLOPCDANameSpaceItem_CanRead_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDANameSpaceItem_CanWrite_Proxy( 
    IXRTLOPCDANameSpaceItem * This,
    /* [out] */ BOOL *pbResult);


void __RPC_STUB IXRTLOPCDANameSpaceItem_CanWrite_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDANameSpaceItem_GetItemID_Proxy( 
    IXRTLOPCDANameSpaceItem * This,
    /* [string][out] */ LPWSTR *pszItemID);


void __RPC_STUB IXRTLOPCDANameSpaceItem_GetItemID_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IXRTLOPCDANameSpaceItem_INTERFACE_DEFINED__ */


#ifndef __IXRTLOPCDADataSource_INTERFACE_DEFINED__
#define __IXRTLOPCDADataSource_INTERFACE_DEFINED__

/* interface IXRTLOPCDADataSource */
/* [unique][uuid][object] */ 


EXTERN_C const IID IID_IXRTLOPCDADataSource;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("5C389437-7A4D-4DB0-8E02-15FE2523D9B8")
    IXRTLOPCDADataSource : public IUnknown
    {
    public:
        virtual HRESULT STDMETHODCALLTYPE Read( 
            /* [string][in] */ LPWSTR szItemID,
            /* [out] */ VARIANT *pValue,
            /* [out] */ WORD *pQuality,
            /* [out] */ FILETIME *pftTimeStamp) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE Write( 
            /* [string][in] */ LPWSTR szItemID,
            /* [in] */ VARIANT Value,
            /* [in] */ WORD Quality,
            /* [in] */ FILETIME ftTimeStamp) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE CreateItemEnumerator( 
            /* [out] */ LPENUMSTRING *ppIEnumString) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE Update( 
            /* [in] */ LPXRTLOPCDADataSource pDataSource) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE AddItem( 
            /* [string][in] */ LPWSTR szItemID) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE RemoveItem( 
            /* [string][in] */ LPWSTR szItemID) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE ClearItems( void) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IXRTLOPCDADataSourceVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IXRTLOPCDADataSource * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void **ppvObject);
        
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IXRTLOPCDADataSource * This);
        
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IXRTLOPCDADataSource * This);
        
        HRESULT ( STDMETHODCALLTYPE *Read )( 
            IXRTLOPCDADataSource * This,
            /* [string][in] */ LPWSTR szItemID,
            /* [out] */ VARIANT *pValue,
            /* [out] */ WORD *pQuality,
            /* [out] */ FILETIME *pftTimeStamp);
        
        HRESULT ( STDMETHODCALLTYPE *Write )( 
            IXRTLOPCDADataSource * This,
            /* [string][in] */ LPWSTR szItemID,
            /* [in] */ VARIANT Value,
            /* [in] */ WORD Quality,
            /* [in] */ FILETIME ftTimeStamp);
        
        HRESULT ( STDMETHODCALLTYPE *CreateItemEnumerator )( 
            IXRTLOPCDADataSource * This,
            /* [out] */ LPENUMSTRING *ppIEnumString);
        
        HRESULT ( STDMETHODCALLTYPE *Update )( 
            IXRTLOPCDADataSource * This,
            /* [in] */ LPXRTLOPCDADataSource pDataSource);
        
        HRESULT ( STDMETHODCALLTYPE *AddItem )( 
            IXRTLOPCDADataSource * This,
            /* [string][in] */ LPWSTR szItemID);
        
        HRESULT ( STDMETHODCALLTYPE *RemoveItem )( 
            IXRTLOPCDADataSource * This,
            /* [string][in] */ LPWSTR szItemID);
        
        HRESULT ( STDMETHODCALLTYPE *ClearItems )( 
            IXRTLOPCDADataSource * This);
        
        END_INTERFACE
    } IXRTLOPCDADataSourceVtbl;

    interface IXRTLOPCDADataSource
    {
        CONST_VTBL struct IXRTLOPCDADataSourceVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IXRTLOPCDADataSource_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IXRTLOPCDADataSource_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IXRTLOPCDADataSource_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IXRTLOPCDADataSource_Read(This,szItemID,pValue,pQuality,pftTimeStamp)	\
    (This)->lpVtbl -> Read(This,szItemID,pValue,pQuality,pftTimeStamp)

#define IXRTLOPCDADataSource_Write(This,szItemID,Value,Quality,ftTimeStamp)	\
    (This)->lpVtbl -> Write(This,szItemID,Value,Quality,ftTimeStamp)

#define IXRTLOPCDADataSource_CreateItemEnumerator(This,ppIEnumString)	\
    (This)->lpVtbl -> CreateItemEnumerator(This,ppIEnumString)

#define IXRTLOPCDADataSource_Update(This,pDataSource)	\
    (This)->lpVtbl -> Update(This,pDataSource)

#define IXRTLOPCDADataSource_AddItem(This,szItemID)	\
    (This)->lpVtbl -> AddItem(This,szItemID)

#define IXRTLOPCDADataSource_RemoveItem(This,szItemID)	\
    (This)->lpVtbl -> RemoveItem(This,szItemID)

#define IXRTLOPCDADataSource_ClearItems(This)	\
    (This)->lpVtbl -> ClearItems(This)

#endif /* COBJMACROS */


#endif 	/* C style interface */



HRESULT STDMETHODCALLTYPE IXRTLOPCDADataSource_Read_Proxy( 
    IXRTLOPCDADataSource * This,
    /* [string][in] */ LPWSTR szItemID,
    /* [out] */ VARIANT *pValue,
    /* [out] */ WORD *pQuality,
    /* [out] */ FILETIME *pftTimeStamp);


void __RPC_STUB IXRTLOPCDADataSource_Read_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDADataSource_Write_Proxy( 
    IXRTLOPCDADataSource * This,
    /* [string][in] */ LPWSTR szItemID,
    /* [in] */ VARIANT Value,
    /* [in] */ WORD Quality,
    /* [in] */ FILETIME ftTimeStamp);


void __RPC_STUB IXRTLOPCDADataSource_Write_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDADataSource_CreateItemEnumerator_Proxy( 
    IXRTLOPCDADataSource * This,
    /* [out] */ LPENUMSTRING *ppIEnumString);


void __RPC_STUB IXRTLOPCDADataSource_CreateItemEnumerator_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDADataSource_Update_Proxy( 
    IXRTLOPCDADataSource * This,
    /* [in] */ LPXRTLOPCDADataSource pDataSource);


void __RPC_STUB IXRTLOPCDADataSource_Update_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDADataSource_AddItem_Proxy( 
    IXRTLOPCDADataSource * This,
    /* [string][in] */ LPWSTR szItemID);


void __RPC_STUB IXRTLOPCDADataSource_AddItem_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDADataSource_RemoveItem_Proxy( 
    IXRTLOPCDADataSource * This,
    /* [string][in] */ LPWSTR szItemID);


void __RPC_STUB IXRTLOPCDADataSource_RemoveItem_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IXRTLOPCDADataSource_ClearItems_Proxy( 
    IXRTLOPCDADataSource * This);


void __RPC_STUB IXRTLOPCDADataSource_ClearItems_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IXRTLOPCDADataSource_INTERFACE_DEFINED__ */



#ifndef __XRTLOPCDA_LIBRARY_DEFINED__
#define __XRTLOPCDA_LIBRARY_DEFINED__

/* library XRTLOPCDA */
/* [helpstring][version][uuid] */ 







EXTERN_C const IID LIBID_XRTLOPCDA;
#endif /* __XRTLOPCDA_LIBRARY_DEFINED__ */

/* Additional Prototypes for ALL interfaces */

unsigned long             __RPC_USER  VARIANT_UserSize(     unsigned long *, unsigned long            , VARIANT * ); 
unsigned char * __RPC_USER  VARIANT_UserMarshal(  unsigned long *, unsigned char *, VARIANT * ); 
unsigned char * __RPC_USER  VARIANT_UserUnmarshal(unsigned long *, unsigned char *, VARIANT * ); 
void                      __RPC_USER  VARIANT_UserFree(     unsigned long *, VARIANT * ); 

/* end of Additional Prototypes */

#ifdef __cplusplus
}
#endif

#endif


