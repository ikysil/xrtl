unit xrtl_crypt_cipher_AES;

{$INCLUDE xrtl.inc}

interface

uses
  xrtl_crypt_cipher_Rijndael;

type
  TXRTLAESCipher = class(TXRTLRijndael128Cipher)
  private
  protected
  public
    class function GetDisplayName: string; override;
  end;

implementation

{ TXRTLAESCipher }

class function TXRTLAESCipher.GetDisplayName: string;
begin
  Result:= 'AES';
end;

end.
