unit xrtl_crypt_CipherKey;

{$INCLUDE xrtl.inc}

interface

uses
  SysUtils;

type
  TXRTLCipherKey = class
  private
  protected
    function   GetKeyData: PByteArray; virtual; abstract;
    function   GetKeySize: Integer; virtual; abstract;
  public
    property   KeyData: PByteArray read GetKeyData;
    property   KeySize: Integer read GetKeySize;
  end;

  TXRTLStringCipherKey = class(TXRTLCipherKey)
  private
    FKey: WideString;
  protected
    function   GetKeyData: PByteArray; override;
    function   GetKeySize: Integer; override;
  public
    constructor Create(const AKey: WideString = '');
    property   Key: WideString read FKey write FKey; 
  end;

implementation

{ TXRTLStringCipherKey }

constructor TXRTLStringCipherKey.Create(const AKey: WideString = '');
begin
  inherited Create;
  FKey:= AKey;
end;

function TXRTLStringCipherKey.GetKeyData: PByteArray;
begin
  Result:= @FKey[1];
end;

function TXRTLStringCipherKey.GetKeySize: Integer;
begin
  Result:= Length(FKey);
end;

end.
