unit xrtl_io_object_Serializer;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_util_Type, xrtl_util_Exception,
  xrtl_io_Stream;

type
  EXRTLSerializerException = class(EXRTLException);

  IXRTLSerializer = interface
  ['{C255D455-F2C1-44FA-A47F-C37771143BF4}']
    procedure  WriteInterface(const Stream: TXRTLOutputStream; const Obj: IInterface;
                              const Shared: Boolean = True);
    procedure  WriteClass(const Stream: TXRTLOutputStream; const Obj: TClass;
                          const Shared: Boolean = True);
    procedure  WriteObject(const Stream: TXRTLOutputStream; const Obj: TObject;
                           const Shared: Boolean = True);
  end;

  IXRTLDeserializer = interface
  ['{C255D456-F2C1-44FA-A47F-C37771143BF4}']
    function   ReadInterface(const Stream: TXRTLInputStream; const Obj: IInterface;
                             const Shared: Boolean = True): IInterface;
    function   ReadClass(const Stream: TXRTLInputStream; const Obj: TClass;
                         const Shared: Boolean = True): TClass;
    function   ReadObject(const Stream: TXRTLInputStream; const Obj: TObject;
                          const Shared: Boolean = True): TObject;
  end;

  IXRTLCustomDataWriter = interface
  ['{C255D457-F2C1-44FA-A47F-C37771143BF4}']
    procedure  WriteCustomData(const Stream: TXRTLOutputStream; const Serializer: IXRTLSerializer;
                               const Obj: TObject);
  end;

  IXRTLCustomDataReader = interface
  ['{C255D458-F2C1-44FA-A47F-C37771143BF4}']
    procedure  ReadCustomData(const Stream: TXRTLInputStream; const Deserializer: IXRTLDeserializer;
                              const Obj: TObject);
  end;

implementation

end.
