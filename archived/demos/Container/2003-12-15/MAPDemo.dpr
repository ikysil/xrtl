program MAPDemo;

{$APPTYPE CONSOLE}

uses
  SysUtils, xrtl_util_Map, xrtl_util_Container;

var
  Map: TXRTLMap;
  Key1, Key2: WideString;
begin
  Key1:= '123';
  Key2:= '12';
  Map:= nil;
  try
    Map:= TXRTLMap.Create(TXRTLWideStringContainerAdapter.Create, TXRTLPointerContainerAdapter.Create);
    Map.SetSorted(True);
    Map.Put(@Key1, Pointer(0));
    WriteLn(Map.ContainsKey(@Key1));
    WriteLn(Map.ContainsValue(Pointer(0)));
    WriteLn(Integer(Map.Get(@Key1)));
    Map.Put(@Key2, Pointer(23));
    WriteLn(Map.ContainsKey(@Key2));
    WriteLn(Integer(Map.GetValue(0)));
    WriteLn(Integer(Map.GetValue(1)));
    Map.Put(@Key2, Pointer(230));
    WriteLn(Integer(Map.GetValue(0)));
    WriteLn(Integer(Map.GetValue(1)));
    Map.Remove(@Key2);
    WriteLn(Map.ContainsKey(@Key2));
    WriteLn(Map.ContainsValue(Pointer(230)));
    WriteLn(Map.GetCount);
    Key2:= '';
    Map.Put(@Key2, Pointer(230));
    WriteLn(Map.ContainsKey(@Key2));
    WriteLn(Map.GetCount);
    Map.Put(nil, Pointer(230));
  except
    on E: Exception do
      WriteLn(E.Message);
  end;
  ReadLn;
  FreeAndNil(Map);
end.
