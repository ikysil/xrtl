unit xrtl_net_URIUtils;

{$INCLUDE xrtl.inc}

interface

function  XRTLURLDecode(const ASrc: WideString): WideString;
function  XRTLURLEncode(const ASrc: WideString): WideString;
function  XRTLURINormalize(const AURI: WideString): WideString;
procedure XRTLURIParse(const AURI: WideString;
                       var VProtocol, VHost, VPath, VDocument, VPort,
                       VBookmark, VUserName, VPassword: WideString);

implementation

uses
  SysUtils,
  xrtl_util_StrUtils;

function XRTLURLDecode(const ASrc: WideString): WideString;
var
  I: integer;
  ESC: string[2];
  CharCode: integer;
  LSrc: WideString;
begin
  Result:= '';
  LSrc:= StringReplace(ASrc, '+', ' ', [rfReplaceAll]);  {do not localize}
  I:= 1;
  while I <= Length(LSrc) do
  begin
    if LSrc[I] <> '%' then  {do not localize}
    begin
      Result:= Result + LSrc[I];
    end
    else
    begin
      Inc(I); // skip the % char
      ESC:= Copy(LSrc, I, 2); // Copy the escape code
      Inc(I, 1); // Then skip it.
      try
        CharCode:= StrToInt('$' + ESC);  {do not localize}
        if (CharCode > 0) and (CharCode < 256) then
        begin
          Result:= Result + Char(CharCode);
        end;
      except
      end;
    end;
    Inc(I);
  end;
end;

function XRTLURLEncode(const ASrc: WideString): WideString;
const
  UnsafeChars = ['*', '#', '%', '<', '>'];  {do not localize}
var
  I: Integer;
begin
  Result:= '';
  for I:= 1 to Length(ASrc) do
  begin
    if ASrc[I] = ' ' then {do not localize}
    begin
      Result:= Result + '+'; {do not localize}
    end
    else
    begin
      if (Char(ASrc[I]) in UnsafeChars) or (ASrc[I] >= #$80) then
      begin
        Result:= Result + '%' + IntToHex(Ord(ASrc[I]), 2);  {do not localize}
      end
      else
      begin
        Result:= Result + ASrc[I];
      end;
    end;
  end;
end;

function XRTLURINormalize(const AURI: WideString): WideString;
begin
// Normalize the directory delimiters to follow the UNIX syntax
  Result:= StringReplace(AURI, '\', '/', [rfReplaceAll]);
end;

procedure XRTLURIParse(const AURI: WideString;
                       var VProtocol, VHost, VPath, VDocument, VPort,
                       VBookmark, VUserName, VPassword: WideString);
var
  LBuffer: WideString;
  LTokenPos: Integer;
  LURI: WideString;
begin
  LURI:= XRTLURINormalize(AURI);
  VHost:= '';
  VProtocol:= '';
  VPath:= '';
  VDocument:= '';
  if XRTLPos('://', LURI) > 0 then
  begin
    // absolute URI
    // What to do when data don't match configuration ??
    // Get the protocol
    LTokenPos:= XRTLPos('://', LURI);
    VProtocol:= Copy(LURI, 1, LTokenPos  - 1);
    Delete(LURI, 1, LTokenPos + 2);
    // Get the user name, password, host and the port number
    LBuffer:= XRTLFetch(LURI, '/', True);
    // Get username and password
    LTokenPos:= XRTLPos('@', LURI);
    VPassword := Copy(LBuffer, 1, LTokenPos  - 1);
    if LTokenPos > 0 then
      Delete(LBuffer, 1, LTokenPos + 2);
    VUserName := XRTLFetch(VPassword, ':', True);
    // Ignore cases where there is only password (http://:password@host/pat/doc)
    if Length(VUserName) = 0 then VPassword := '';
    // Get the host and the port number
    VHost := XRTLFetch(LBuffer, ':', True);
    VPort := LBuffer;
    // Get the path
    LTokenPos := XRTLRPos('/', LURI, -1);
    VPath := '/' + Copy(LURI, 1, LTokenPos);
    Delete(LURI, 1, LTokenPos);
    // Get the document
    VDocument := LURI;
  end
  else
  begin
    // received an absolute path, not an URI
    // Get the path
    LTokenPos:= XRTLRPos('/', LURI, -1);
    VPath := Copy(LURI, 1, LTokenPos);
    Delete(LURI, 1, LTokenPos);
    // Get the document
    VDocument := LURI;
  end;

  VPath := XRTLURLDecode(VPath);
  // Parse the # bookmark from the document
  VBookmark := VDocument;
  VDocument := XRTLURLDecode(XRTLFetch(VBookmark, '#'));
end;

end.
