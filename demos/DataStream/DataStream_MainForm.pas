unit DataStream_MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm2 = class(TForm)
    Button2: TButton;
    Button3: TButton;
    Memo1: TMemo;
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  xrtl_io_Stream, xrtl_io_FileStream,
  xrtl_io_DataStream, xrtl_io_TaggedDataStream;

{$R *.dfm}

procedure TForm2.Button2Click(Sender: TObject);
var
 aa:Variant;
 S: TXRTLDataOutputStream;
begin
  aa:=VarArrayCreate([0,3],varVariant);
  aa[0]:=1;
  aa[1]:='abc';
  aa[2]:=now;
  aa[3]:=Null;
  S:= nil;
  try
    S:= TXRTLTaggedDataOutputStream.Create(TXRTLFileOutputStream.Create('c:\XRTL.REP'));
    S.WriteVariant(aa);
  finally
    FreeAndNil(S);
  end;
end;

procedure TForm2.Button3Click(Sender: TObject);

  function toStrNull(V: Variant): string;
  begin
    if VarIsNull(V) then
      Result:= 'Null'
    else
      if VarIsEmpty(V) then
        Result:= 'Empty'
      else
        Result:= V;
  end;

var
  S: TXRTLDataInputStream;
  Replicator:Variant;
  i:Integer;
begin
  S:= nil;
  VarClear(Replicator);
  try
    S:= TXRTLTaggedDataInputStream.Create(TXRTLFileInputStream.Create('c:\XRTL.REP'));
    Replicator:=S.ReadVariant;
  finally
    FreeAndNil(S);
  end;
  for i:=VarArrayLowBound(Replicator,1) to VarArrayHighBound(Replicator, 1) do
    Memo1.Lines.Add(toStrNull(Replicator[i]));
end;

end.
