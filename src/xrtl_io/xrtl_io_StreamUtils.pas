unit xrtl_io_StreamUtils;

{$INCLUDE xrtl.inc}

interface

uses
  Windows, SysUtils, Math,
  xrtl_io_Stream, xrtl_io_ResourceStrings, xrtl_io_Exception;

type
  TXRTLCopyStreamProgressProc = procedure(const Position: Int64; const Step: Integer) of object;
{
  Description: Copies InputStream to OutputStream
  Parameters:
    InputStream  - source
    OutputStream - destination
    ProgressProc - if assigned is called after each block has been copied
    Size         - size of stream to copy (bytes)
    BlockSize    -
  Returns:
    Copied bytes count
  Exceptions:
    EXRTLEndOfStreamException if end of stream reached before Size bytes copied (Size > 0).
}
function  XRTLCopyStream(const InputStream: TXRTLInputStream;
                         const OutputStream: TXRTLOutputStream;
                         ProgressProc: TXRTLCopyStreamProgressProc = nil;
                         Size: Int64 = -1; BlockSize: Integer = 4096): Int64;

function  XRTLGetTempFileName(ADirectory: string = ''; APrefix: string = ''): string;

implementation

function  XRTLCopyStream(const InputStream: TXRTLInputStream;
                         const OutputStream: TXRTLOutputStream;
                         ProgressProc: TXRTLCopyStreamProgressProc = nil;
                         Size: Int64 = -1; BlockSize: Integer = 4096): Int64;
var
  RSize, RCount: Integer;
  Buffer: Pointer;
begin
  Result:= 0;
  BlockSize:= Max(1024, BlockSize);
  Buffer:= nil;
  try
    GetMem(Buffer, BlockSize);
    while True do
    begin
      if Size > 0 then
        RSize:= Min(Size - Result, BlockSize)
      else
        RSize:= BlockSize;
      if RSize = 0 then
        Break;
      RCount:= InputStream.ReadBuffer(Buffer^, RSize);
      case RCount of
        XRTLEndOfStreamValue:
        begin
          if (Size > 0) and (Result < Size) then
            raise EXRTLEndOfStreamException.CreateFmt(SXRTLEndOfStreamException, [InputStream.ClassName])
          else
            Break;
        end;
        0:
        begin
          raise EXRTLIOException.CreateFmt('%s.ReadBuffer returned 0(zero)', [InputStream.ClassName]);
        end;
      else
        OutputStream.WriteBuffer(Buffer^, RCount);
        Inc(Result, RCount);
        if Assigned(ProgressProc) then
          ProgressProc(Result, RCount);
      end;
    end;
  finally
    if Assigned(Buffer) then
      FreeMem(Buffer);
  end;
end;

function  XRTLGetTempFileName(ADirectory: string = ''; APrefix: string = ''): string;
var
  TempDir, Prefix: string;
begin
  Prefix:= Trim(APrefix);
  if Prefix = '' then
    Prefix:= 'TFS';
  TempDir:= ADirectory;
  if TempDir = '' then
  begin
    SetLength(TempDir, MAX_PATH);
    SetLength(TempDir, GetTempPath(MAX_PATH, PChar(TempDir)));
  end;
{$IFDEF COMPILER6_UP}
  TempDir:= IncludeTrailingPathDelimiter(TempDir);
{$ELSE}
  TempDir:= IncludeTrailingBackslash(TempDir);
{$ENDIF}
  SetLength(Result, MAX_PATH);
  GetTempFileName(PChar(TempDir), PChar(Prefix), 0, PChar(Result));
  SetLength(Result, StrLen(PChar(Result)));
end;

end.
