unit secDemo1_Classes;

interface

uses
  Windows, SysUtils,
  xrtl_io_StreamProcessor,
  xrtl_crypt_BlockCipher;

type
  TXRTLIdentityBlockCipher = Class(TXRTLBlockCipher)
  protected
    function   engineGetBlockSize: Integer; override;
  public
    procedure  engineUpdateEncipherBlockECB(InBuffer, OutBuffer: PByteArray); override;
    procedure  engineUpdateDecipherBlockECB(InBuffer, OutBuffer: PByteArray); override;
  end;

implementation

uses
  xrtl_util_CPUUtils,
  xrtl_crypt_DDP;

{ TXRTLIdentityBlockCipher }

function TXRTLIdentityBlockCipher.engineGetBlockSize: Integer;
begin
  Result:= 8;
end;

procedure TXRTLIdentityBlockCipher.engineUpdateEncipherBlockECB(InBuffer, OutBuffer: PByteArray);
begin
  PInteger(OutBuffer)^:= XRTLDDPR3Fixed(PInteger(InBuffer)^);
  PInteger(@OutBuffer[4])^:= XRTLDDPR3Fixed(PInteger(@InBuffer[4])^);
end;

procedure TXRTLIdentityBlockCipher.engineUpdateDecipherBlockECB(InBuffer, OutBuffer: PByteArray);
begin
  PInteger(OutBuffer)^:= XRTLDDPR3Fixed(PInteger(InBuffer)^);
  PInteger(@OutBuffer[4])^:= XRTLDDPR3Fixed(PInteger(@InBuffer[4])^);
end;

end.
