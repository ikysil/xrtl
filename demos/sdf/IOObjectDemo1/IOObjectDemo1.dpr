program IOObjectDemo1;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  IOD1_Classes in 'IOD1_Classes.pas',
  xrtl_io_Stream, xrtl_io_FileStream,
  xrtl_sdf_Serializer, xrtl_sdf_binary_Serializer,
  xrtl_util_Value;

procedure Demo;
var
  O1, I1: TBase;
  O2, I2: TDerived1;
  O3, I3: TDerived2;
  LOS: TXRTLOutputStream;
  LIS: TXRTLInputStream;
  LSer: IXRTLSerializer;
  LDeser: IXRTLDeserializer;
begin
  O1:= nil;
  O2:= nil;
  O3:= nil;
  I1:= nil;
  I2:= nil;
  I3:= nil;
  LOS:= nil;
  LSer:= nil;
  LDeser:= nil;
  try
    O1:= TBase.Create;
    O2:= TDerived1.Create(O1);
    O3:= TDerived2.Create;
    LOS:= TXRTLFileOutputStream.Create('c:\test.ser', False);
    LSer:= TXRTLBinarySerializer.Create;
    LSer.WriteObject(LOS, nil);
//    LSer.WriteObject(LOS, O2);
    LSer.WriteObject(LOS, O1);
    LSer.WriteObject(LOS, O3);
    LOS.Close;
    LIS:= TXRTLFileInputStream.Create('c:\test.ser', False);
    LDeser:= TXRTLBinaryDeserializer.Create;
    I2:= LDeser.ReadObject(LIS, I2) as TDerived1;
    I1:= LDeser.ReadObject(LIS, I1) as TBase;
    I3:= LDeser.ReadObject(LIS, I3) as TDerived2;
  finally
    LDeser:= nil;
    LSer:= nil;
    FreeAndNil(LIS);
    FreeAndNil(LOS);
    FreeAndNil(I3);
    FreeAndNil(I2);
    FreeAndNil(I1);
    FreeAndNil(O3);
    FreeAndNil(O2);
    FreeAndNil(O1);
  end;
end;

begin
  try
    Demo;
  except
    on E: Exception do
    begin
      WriteLn(Format('Exception: %s', [E.Message]));
      ReadLn;
    end;
  end;
end.
