unit xrtl_io_ObjectStream;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils, Contnrs,
  xrtl_io_DataStream, xrtl_io_Exception;

type
  EXRTLObjectStreamException    = class(EXRTLIOException);
  EXRTLDuplicateClassRegistered = class(EXRTLObjectStreamException);
  EXRTLUnknownClass             = class(EXRTLObjectStreamException);
  EXRTLUnsupportedVersion       = class(EXRTLObjectStreamException);

  TXRTLObjectInputStream = class(TXRTLDataInputStream)
  public
    function   ReadObject(Instance: TObject): TObject;
  end;

  TXRTLObjectOutputStream = class(TXRTLDataOutputStream)
  public
    procedure  WriteObject(AObject: TObject);
  end;

  TXRTLDataStreamObjectBuilder = class
    function   CreateObject(const Data: Pointer): TObject; virtual; abstract;
    procedure  WriteObject(const AObject: TObject; const AStream: TXRTLObjectOutputStream); virtual; abstract;
    procedure  ReadObject(const AObject: TObject; const AStream: TXRTLObjectInputStream;
                          const AVersion: Integer); virtual; abstract;
  end;

  TXRTLDataStreamReadObjectProc  = procedure(const AObject: TObject;
                                             const AStream: TXRTLObjectInputStream;
                                             const AVersion: Integer);

  TXRTLDataStreamWriteObjectProc = procedure(const AObject: TObject;
                                             const AStream: TXRTLObjectOutputStream;
                                             var ACallForParent: Boolean);

  TXRTLDataStreamFactoryProc = function: TObject;

  TXRTLDataStreamClassInfo = class
  private
    FWriteObjectProc: TXRTLDataStreamWriteObjectProc;
    FReadObjectProc: TXRTLDataStreamReadObjectProc;
    FVersion: Cardinal;
    FFactoryProc: TXRTLDataStreamFactoryProc;
    FClass: TClass;
  public
    property   WriteObjectProc: TXRTLDataStreamWriteObjectProc read FWriteObjectProc;
    property   ReadObjectProc: TXRTLDataStreamReadObjectProc read FReadObjectProc;
    property   Version: Cardinal read FVersion;
    property   FactoryProc: TXRTLDataStreamFactoryProc read FFactoryProc;
  end;

procedure XRTLRegisterClass(const AClass: TClass;
                            AWriteObjectProc: TXRTLDataStreamWriteObjectProc;
                            AReadObjectProc: TXRTLDataStreamReadObjectProc;
                            AFactoryProc: TXRTLDataStreamFactoryProc;
                            const AVersion: Cardinal = 1);
function  XRTLGetClassInfo(const AClass: TClass): TXRTLDataStreamClassInfo; overload;
function  XRTLGetClassInfo(const AClassName: WideString): TXRTLDataStreamClassInfo; overload;

implementation

uses
  xrtl_util_Map,
  xrtl_util_Synchronizer,
  xrtl_util_Container;

var
  FDataStreamClassInfoMap: TXRTLMap = nil;
  FSynchronizer: TXRTLSynchronizer;

procedure XRTLRegisterClass(const AClass: TClass;
                            AWriteObjectProc: TXRTLDataStreamWriteObjectProc;
                            AReadObjectProc: TXRTLDataStreamReadObjectProc;
                            AFactoryProc: TXRTLDataStreamFactoryProc;
                            const AVersion: Cardinal = 1);
var
  ClassInfo: TXRTLDataStreamClassInfo;
  ClassName: WideString;
begin
  try
    FSynchronizer.BeginRead;
    ClassName:= AClass.ClassName;
    ClassInfo:= FDataStreamClassInfoMap.Get(@ClassName);
    if Assigned(ClassInfo) then
      raise EXRTLDuplicateClassRegistered.Create('Duplicate class registered');
    try
      FSynchronizer.BeginWrite;
      ClassInfo:= TXRTLDataStreamClassInfo.Create;
      ClassInfo.FWriteObjectProc:= AWriteObjectProc;
      ClassInfo.FReadObjectProc:=  AReadObjectProc;
      ClassInfo.FFactoryProc:=     AFactoryProc;
      ClassInfo.FVersion:=         AVersion;
      ClassInfo.FClass:=           AClass;
      FDataStreamClassInfoMap.Put(@ClassName, ClassInfo);
    finally
      FSynchronizer.EndWrite;
    end;
  finally
    FSynchronizer.EndRead;
  end;
end;

function XRTLGetClassInfo(const AClass: TClass): TXRTLDataStreamClassInfo;
begin
  Result:= XRTLGetClassInfo(AClass.ClassName);
end;

function XRTLGetClassInfo(const AClassName: WideString): TXRTLDataStreamClassInfo;
begin
  try
    FSynchronizer.BeginRead;
    Result:= FDataStreamClassInfoMap.Get(@AClassName);
  finally
    FSynchronizer.EndRead;
  end;
end;

const
  XRTLNilObjectName = '<nil>';

{ TXRTLObjectInputStream }

function TXRTLObjectInputStream.ReadObject(Instance: TObject): TObject;
var
  ClassName: WideString;
  ClassInfo: TXRTLDataStreamClassInfo;
  Version: Cardinal;
begin
  Result:= Instance;
  ClassName:= ReadWideString;
  while WideCompareStr(XRTLNilObjectName, ClassName) <> 0 do
  begin
    ClassInfo:= XRTLGetClassInfo(ClassName);
    if not Assigned(ClassInfo) or not Assigned(ClassInfo.ReadObjectProc) or
       not Assigned(ClassInfo.FactoryProc) then
      raise EXRTLUnknownClass.CreateFmt('Can''t read, class %s is not registered', [ClassName]);
    Version:= ReadCardinal;
    if ClassInfo.Version < Version then
      raise EXRTLUnsupportedVersion.CreateFmt('Can''t read, class %s registered with version %.8x, but version %.8x should be read', [ClassName, ClassInfo.Version, Version]);
    if not Assigned(Result) then
      Result:= ClassInfo.FactoryProc;
    ClassInfo.ReadObjectProc(Result, Self, Version);
    ClassName:= ReadWideString;
  end;
end;

{ TXRTLObjectOutputStream }

procedure TXRTLObjectOutputStream.WriteObject(AObject: TObject);
var
  ClassName: WideString;
  ClassInfo: TXRTLDataStreamClassInfo;
  LObject: TObject;
  LObjectClass: TClass;
  CallParent: Boolean;
  LStack: TStack;
begin
  if not Assigned(AObject) then
  begin
    WriteWideString(XRTLNilObjectName);
    Exit;
  end;
  LStack:= nil;
  try
    LStack:= TStack.Create;
    LObject:= AObject;
    LObjectClass:= LObject.ClassType;
    CallParent:= True;
    repeat
      ClassName:= LObjectClass.ClassName;
      ClassInfo:= XRTLGetClassInfo(ClassName);
      if Assigned(ClassInfo) then
        LStack.Push(ClassInfo);
      LObjectClass:= LObjectClass.ClassParent;
    until (LObjectClass = nil) or not CallParent;
    if LStack.Count = 0 then
      raise EXRTLUnknownClass.CreateFmt('Can''t write, class %s is not registered', [LObject.ClassName]);
    while LStack.Count > 0 do
    begin
      ClassInfo:= LStack.Pop;
      ClassName:= ClassInfo.FClass.ClassName;
      if not Assigned(ClassInfo.WriteObjectProc) then
        raise EXRTLUnknownClass.CreateFmt('Can''t write, class %s is not registered', [LObject.ClassName]);
      WriteWideString(ClassName);
      WriteCardinal(ClassInfo.Version);
      ClassInfo.WriteObjectProc(AObject, Self, CallParent);
    end;
    WriteWideString(XRTLNilObjectName);
  finally
    FreeAndNil(LStack);
  end;
end;

initialization
begin
  FSynchronizer:= TXRTLCriticalSectionSynchronizer.Create;
  FDataStreamClassInfoMap:= TXRTLMap.Create(TXRTLWideStringContainerAdapter.Create,
                              TXRTLObjectContainerAdapter.Create(True));
end;

finalization
begin
  FreeAndNil(FDataStreamClassInfoMap);
  FreeAndNil(FSynchronizer);
end;

end.
