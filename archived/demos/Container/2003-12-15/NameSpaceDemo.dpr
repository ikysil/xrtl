program NameSpaceDemo;

{$APPTYPE CONSOLE}

uses
  SysUtils, Windows, 
  xrtl_util_NameSpace, xrtl_util_NameSpacePath,
  xrtl_util_Container;

var
  NameSpace: TXRTLNameSpace;
  Enum: TXRTLNameSpaceEnumerator;
  PPath: Pointer;
  Path: TXRTLNameSpacePath;
  Count, I: Integer;

begin
  NameSpace:= TXRTLNameSpace.Create(TXRTLPointerContainerAdapter.Create);
  Enum:= nil;
  try
    NameSpace.Put(XRTLNameSpacePathCreate(['1', '2', '3']), nil);
    WriteLn(NameSpace.Contains(XRTLNameSpacePathCreate(['1', '2', '3'])));
    WriteLn(NameSpace.Contains(XRTLNameSpacePathCreate(['1', '2'])));
    WriteLn(NameSpace.Contains(XRTLNameSpacePathCreate(['1'])));
    Enum:= NameSpace.CreateEnumerator(XRTLNameSpacePathCreate([]), nsEnumerateRecursive);
    while Succeeded(Enum.Next(1, PPath, @Count)) and (Count > 0) do
    begin
      Path:= TXRTLNameSpacePath(PPath^);
      for I:= 0 to XRTLNameSpacePathGetLength(Path) - 1 do
      begin
        Write(Path[I] + '.');
      end;
      WriteLn;
    end;  
    NameSpace.Remove(XRTLNameSpacePathCreate(['1', '2', '3']));
    WriteLn(NameSpace.Contains(XRTLNameSpacePathCreate(['1', '2', '3'])));
    WriteLn(NameSpace.Contains(XRTLNameSpacePathCreate(['1', '2'])));
    WriteLn(NameSpace.Contains(XRTLNameSpacePathCreate(['1'])));
    NameSpace.Remove(XRTLNameSpacePathCreate(['1']));
    WriteLn(NameSpace.Contains(XRTLNameSpacePathCreate(['1', '2', '3'])));
    WriteLn(NameSpace.Contains(XRTLNameSpacePathCreate(['1', '2'])));
    WriteLn(NameSpace.Contains(XRTLNameSpacePathCreate(['1'])));
    WriteLn(NameSpace.Contains(XRTLNameSpacePathCreate([])));
  except
    on E: Exception do
      WriteLn(E.Message);
  end;
  ReadLn;
  FreeAndNil(NameSpace);
  FreeAndNil(Enum);
end.
