program MAPDemo2;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  xrtl_util_Container, xrtl_util_Map;

var
  Con: TXRTLMap;
  I, K: Integer;
  Str1, Str2, Str3, Str4: WideString;
begin
  for I := 0 to 0{0000} do
  begin
    Con := TXRTLMap.Create(TXRTLWideStringContainerAdapter.Create, TXRTLObjectContainerAdapter.Create(True));
    try
      Con.SetSorted(True);
      Str1:= '123';//IntToStr(I);
      Str2:= IntToStr(I+1);
      Str3:= IntToStr(I+2);
      Str4:= IntToStr(I+3);
      TObject(Con.Put(@(Str1), TObject.Create)).Free;
      TObject(Con.Put(@(Str2), TObject.Create)).Free;
      TObject(Con.Put(@(Str3), TObject.Create)).Free;
      TObject(Con.Put(@(Str4), TObject.Create)).Free;
      for K:= 0 to Con.GetCount - 1 do
      begin
        WriteLn(WideString(Con.GetKey(K)^));
      end;
    finally
      FreeAndNil(Con);
    end;
  end;
  ReadLn;
end.
