unit OPCDA20Client_MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, COMObj, ActiveX,
  xrtl_opc_DA, xrtl_opc_sdk_DA;

const
  CLASS_OPCDA20ServerDemo: TGUID = '{3EDA7514-F446-4927-8152-AA60E7CDB559}';

type
  TMainForm = class(TForm)
    ConnectBtn: TButton;
    procedure ConnectBtnClick(Sender: TObject);
  private
    procedure DefineItem(vtDataType: DWORD; Name: WideString);
    procedure InitializeNameSpace;
  public
    ServerUnk: IUnknown;
    Server: IOPCServer;
    XRTLServer: IXRTLOPCDAServer;
    NameSpace: IXRTLOPCDANameSpace;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.DefineItem(vtDataType: DWORD; Name: WideString);
var
  Item: IXRTLOPCDANameSpaceItem;
begin
  OLECheck(NameSpace.AddItem(VarArrayOf([Name]), PWideChar(Name), VT_EMPTY, Item));
  OLECheck(NameSpace.AddItem(VarArrayOf([Name, 'R']), PWideChar(Name + ' (read only)'), vtDataType, Item));
  OLECheck(Item.SetAccessRights(OPC_READABLE));
  OLECheck(NameSpace.AddItem(VarArrayOf([Name, 'W']), PWideChar(Name + ' (write only)'), vtDataType, Item));
  OLECheck(Item.SetAccessRights(OPC_WRITEABLE));
  OLECheck(NameSpace.AddItem(VarArrayOf([Name, 'R/W']), PWideChar(Name), vtDataType, Item));
  OLECheck(NameSpace.AddItem(VarArrayOf([Name + ' array']), PWideChar(Name + ' array'), VT_EMPTY, Item));
  OLECheck(NameSpace.AddItem(VarArrayOf([Name + ' array', 'R']), PWideChar(Name + ' array' + ' (read only)'), vtDataType or VT_ARRAY, Item));
  OLECheck(Item.SetAccessRights(OPC_READABLE));
  OLECheck(NameSpace.AddItem(VarArrayOf([Name + ' array', 'W']), PWideChar(Name + ' array' + ' (write only)'), vtDataType or VT_ARRAY, Item));
  OLECheck(Item.SetAccessRights(OPC_WRITEABLE));
  OLECheck(NameSpace.AddItem(VarArrayOf([Name + ' array', 'R/W']), PWideChar(Name + ' array'), vtDataType or VT_ARRAY, Item));
end;

procedure TMainForm.InitializeNameSpace;
begin
  OLECheck(NameSpace.SetOrganization(OPC_NS_HIERARCHIAL));
  DefineItem(VT_BOOL, 'Boolean');
  DefineItem(VT_I1,   'ShortInt');
  DefineItem(VT_UI1,  'Byte');
  DefineItem(VT_I2,   'SmallInt');
  DefineItem(VT_UI2,  'Word');
  DefineItem(VT_I4,   'Integer');
  DefineItem(VT_UI4,  'DWord');
  DefineItem(VT_R4,   'Real4');
  DefineItem(VT_R8,   'Real8');
  DefineItem(VT_DATE, 'Date');
  DefineItem(VT_BSTR, 'String');
  DefineItem(VT_CY,   'Currency');
end;

procedure TMainForm.ConnectBtnClick(Sender: TObject);
begin
  ServerUnk:= CreateComObject(CLASS_OPCDA20ServerDemo);
  Server:= ServerUnk as IOPCServer;
  XRTLServer:= ServerUnk as IXRTLOPCDAServer;
  OLECheck(XRTLServer.GetNameSpace(NameSpace));
  InitializeNameSpace; 
end;

end.
