unit xrtl_crypt_md_SHA1;

{$INCLUDE xrtl.inc}

interface

uses
  xrtl_io_StreamProcessor,
  xrtl_crypt_MessageDigest, xrtl_crypt_md_SHA;

type
  TXRTLSHA1 = class(TXRTLSHA)
  private
  protected
    procedure engineExpand(var W: TXRTLSHAWork); override;
  public
    class function GetDisplayName: string; override;
  end;

implementation

uses
  xrtl_util_CPUUtils;

procedure TXRTLSHA1.engineExpand(var W: TXRTLSHAWork);
var
  I: Integer;
begin
  for I:= 16 to 79 do
  begin
    W[I]:= XRTLROL32(W[I - 16] xor W[I - 14] xor W[I - 8] xor W[I - 3], 1);
  end;
end;

class function TXRTLSHA1.GetDisplayName: string;
begin
  Result:= 'SHA-1';
end;

end.