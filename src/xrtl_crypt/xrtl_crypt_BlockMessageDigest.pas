unit xrtl_crypt_BlockMessageDigest;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils,
  xrtl_io_StreamProcessor,
  xrtl_crypt_MessageDigest;

type
  TXRTLBlockMessageDigest = class(TXRTLMessageDigest)
  private
  protected
    FBlockSize: Integer;
    procedure  engineUpdateDigest(var InBuffer: PByteArray;
                                  var InAvail: Integer;
                                  const Operation: TXRTLStreamProcessorOperation); override;
    procedure  engineUpdateDigestBlock(InBuffer: PByteArray); virtual; abstract;
    procedure  engineUpdateDigestBlockFinal(InBuffer: PByteArray;
                                            InAvail: Integer); virtual; abstract;
  public
    constructor Create(ASize, ABlockSize: Integer);
    function   GetBlockSize: Integer;
  end;

implementation

uses
  xrtl_util_CPUUtils;

{ TXRTLBlockMessageDigest }

constructor TXRTLBlockMessageDigest.Create(ASize, ABlockSize: Integer);
begin
  inherited Create(ASize);
  FBlockSize:= ABlockSize;
end;

function TXRTLBlockMessageDigest.GetBlockSize: Integer;
begin
  Result:= FBlockSize;
end;

procedure TXRTLBlockMessageDigest.engineUpdateDigest(
  var InBuffer: PByteArray; var InAvail: Integer;
  const Operation: TXRTLStreamProcessorOperation);
begin
  while InAvail >= FBlockSize do
  begin
    engineUpdateDigestBlock(InBuffer);
    InBuffer:= XRTLPointerAdd(InBuffer, FBlockSize);
    Dec(InAvail, FBlockSize);
  end;
  if Operation = spoFinish then
  begin
    engineUpdateDigestBlockFinal(InBuffer, InAvail);
    InBuffer:= XRTLPointerAdd(InBuffer, InAvail);
    InAvail:= 0;
    Reset;
  end;
end;

end.
