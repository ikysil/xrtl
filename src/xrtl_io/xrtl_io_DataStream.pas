unit xrtl_io_DataStream;

{$INCLUDE xrtl.inc}

interface

uses
  Windows, ActiveX, {$IFDEF HAS_UNIT_VARIANTS}Variants,{$ENDIF}
  xrtl_io_Stream;

type
  TXRTLDataInputStream = class(TXRTLFilterInputStream)
  private
  public
    function   ReadBoolean: Boolean; virtual;
    function   ReadByte: Byte; virtual;
    function   ReadCardinal: Cardinal; virtual;
    function   ReadCurrency: Currency; virtual;
    function   ReadDateTime: TDateTime; virtual;
    function   ReadDouble: Double; virtual;
    function   ReadInt64: Int64; virtual;
    function   ReadInteger: Integer; virtual;
    function   ReadShortInt: ShortInt; virtual;
    function   ReadSingle: Single; virtual;
    function   ReadSmallInt: SmallInt; virtual;
    function   ReadString: string; virtual;
    function   ReadUTF8String: WideString; virtual;
    function   ReadVariant: Variant; virtual;
    function   ReadWideString: WideString; virtual;
    function   ReadWordBool: WordBool; virtual;
  end;

  TXRTLDataOutputStream = class(TXRTLFilterOutputStream)
  private
  public
    procedure  WriteBoolean(AValue: Boolean); virtual;
    procedure  WriteByte(AValue: Byte); virtual;
    procedure  WriteCardinal(AValue: Cardinal); virtual;
    procedure  WriteCurrency(AValue: Currency); virtual;
    procedure  WriteDateTime(AValue: TDateTime); virtual;
    procedure  WriteDouble(AValue: Double); virtual;
    procedure  WriteInt64(AValue: Int64); virtual;
    procedure  WriteInteger(AValue: Integer); virtual;
    procedure  WriteShortInt(AValue: ShortInt); virtual;
    procedure  WriteSingle(AValue: Single); virtual;
    procedure  WriteSmallInt(AValue: SmallInt); virtual;
    procedure  WriteString(AValue: string); virtual;
    procedure  WriteUTF8String(AValue: WideString); virtual;
    procedure  WriteVariant(AValue: Variant); virtual;
    procedure  WriteWideString(AValue: WideString); virtual;
    procedure  WriteWordBool(AValue: WordBool); virtual;
  end;

implementation

{ TXRTLDataInputStream }

function TXRTLDataInputStream.ReadBoolean: Boolean;
begin
  ReadBufferFully(Result, SizeOf(Result));
end;

function TXRTLDataInputStream.ReadByte: Byte;
begin
  ReadBufferFully(Result, SizeOf(Result));
end;

function TXRTLDataInputStream.ReadCardinal: Cardinal;
begin
  ReadBufferFully(Result, SizeOf(Result));
end;

function TXRTLDataInputStream.ReadCurrency: Currency;
begin
  ReadBufferFully(Result, SizeOf(Result));
end;

function TXRTLDataInputStream.ReadDateTime: TDateTime;
begin
  ReadBufferFully(Result, SizeOf(Result));
end;

function TXRTLDataInputStream.ReadDouble: Double;
begin
  ReadBufferFully(Result, SizeOf(Result));
end;

function TXRTLDataInputStream.ReadInt64: Int64;
begin
  ReadBufferFully(Result, SizeOf(Result));
end;

function TXRTLDataInputStream.ReadInteger: Integer;
begin
  ReadBufferFully(Result, SizeOf(Result));
end;

function TXRTLDataInputStream.ReadShortInt: ShortInt;
begin
  ReadBufferFully(Result, SizeOf(Result));
end;

function TXRTLDataInputStream.ReadSingle: Single;
begin
  ReadBufferFully(Result, SizeOf(Result));
end;

function TXRTLDataInputStream.ReadSmallInt: SmallInt;
begin
  ReadBufferFully(Result, SizeOf(Result));
end;

function TXRTLDataInputStream.ReadString: string;
var
  L: Integer;
begin
  L:= ReadInteger;
  SetLength(Result, L);
  ReadBufferFully(PChar(Result)^, L);
end;

function TXRTLDataInputStream.ReadUTF8String: WideString;
begin
  Result:= UTF8Decode(ReadString);
end;

function TXRTLDataInputStream.ReadVariant: Variant;
var
  VD: TVarData;
  VarType: Integer;
  I, DimCount, ElemCount, DimElemCount, ElemSize: Integer;
  ArrayData: Pointer;
  ArrayDims: array of Integer;
begin
  VarType:= ReadInteger;
  if (VarType and varArray) <> 0 then
  begin
    DimCount:= ReadByte;
    SetLength(ArrayDims, DimCount * 2);
    ElemCount:= 1;
    for I:= 0 to DimCount - 1 do
    begin
      DimElemCount:= ReadInteger;
      ElemCount:= ElemCount * DimElemCount;
      ArrayDims[I * 2]:= 0;
      ArrayDims[I * 2 + 1]:= DimElemCount - 1;
    end;
    if ElemCount = 0 then
    begin
      VarClear(Result);
      Exit;
    end;
    Result:= VarArrayCreate(ArrayDims, VarType and varTypeMask);
    VD:= TVarData(Result);
    try
      ArrayData:= VarArrayLock(Result);
      for I:= 0 to ElemCount - 1 do
      begin
        ElemSize:= 0;
        case VarType and varTypeMask of
          VT_I1, VT_UI1:
          begin
            ElemSize:= SizeOf(Byte);
            PByte(ArrayData)^:= ReadByte;
          end;
          VT_I2, VT_UI2:
          begin
            ElemSize:= SizeOf(SmallInt);
            PSmallInt(ArrayData)^:= ReadSmallInt;
          end;
          VT_I4, VT_UI4:
          begin
            ElemSize:= SizeOf(Integer);
            PInteger(ArrayData)^:= ReadInteger;
          end;
          VT_R4:
          begin
            ElemSize:= SizeOf(Single);
            PSingle(ArrayData)^:= ReadSingle;
          end;
          VT_R8:
          begin
            ElemSize:= SizeOf(Double);
            PDouble(ArrayData)^:= ReadDouble;
          end;
          VT_CY:
          begin
            ElemSize:= SizeOf(Currency);
            Currency(ArrayData^):= ReadCurrency;
          end;
          VT_DATE:
          begin
            ElemSize:= SizeOf(TDateTime);
            TDateTime(ArrayData^):= ReadDateTime;
          end;
          VT_BSTR:
          begin
            ElemSize:= SizeOf(PWideChar);
            WideString(ArrayData^):= ReadWideString;
          end;
          VT_BOOL:
          begin
            ElemSize:= SizeOf(WordBool);
            WordBool(ArrayData^):= ReadWordBool;
          end;
          VT_DECIMAL:
          begin
            ElemSize:= 16;
            ReadBufferFully(ArrayData^, 16);
          end;
          VT_VARIANT:
          begin
            ElemSize:= SizeOf(TVarData);
            Variant(PVarData(ArrayData)^):= ReadVariant;
          end;
        else
        end;
        ArrayData:= Pointer(Integer(ArrayData) + ElemSize);
      end;
    finally
      VarArrayUnlock(Result);
    end;
  end
  else
  begin
    case VarType and varTypeMask of
      VT_EMPTY:
        Result:= Unassigned;
      VT_NULL:
        Result:= Null;
      VT_I1:
        Result:= ReadShortInt;
      VT_UI1:
        Result:= Byte(ReadByte);
      VT_I2:
        Result:= ReadSmallInt;
      VT_UI2:
        Result:= Word(ReadSmallInt);
      VT_I4:
        Result:= ReadInteger;
      VT_UI4:
      begin
        Result:= Integer(ReadCardinal);
        VD.VType:= VT_UI4;
      end;
      VT_R4:
        Result:= ReadSingle;
      VT_R8:
        Result:= ReadDouble;
      VT_CY:
        Result:= ReadCurrency;
      VT_DATE:
        Result:= ReadDateTime;
      VT_BSTR:
        Result:= ReadWideString;
      VT_BOOL:
        Result:= ReadWordBool;
      VT_DECIMAL:
        ReadBufferFully(Result, 16);
    else
    end;
  end;
end;

function TXRTLDataInputStream.ReadWideString: WideString;
var
  L: Integer;
begin
  L:= ReadInteger;
  SetLength(Result, L);
  ReadBuffer(Pointer(Result)^, L * SizeOf(WideChar));
end;

function TXRTLDataInputStream.ReadWordBool: WordBool;
begin
  ReadBufferFully(Result, SizeOf(Result));
end;

{ TXRTLDataOutputStream }

procedure TXRTLDataOutputStream.WriteBoolean(AValue: Boolean);
begin
  WriteBuffer(AValue, SizeOf(AValue));
end;

procedure TXRTLDataOutputStream.WriteByte(AValue: Byte);
begin
  WriteBuffer(AValue, SizeOf(AValue));
end;

procedure TXRTLDataOutputStream.WriteCardinal(AValue: Cardinal);
begin
  WriteBuffer(AValue, SizeOf(AValue));
end;

procedure TXRTLDataOutputStream.WriteCurrency(AValue: Currency);
begin
  WriteBuffer(AValue, SizeOf(AValue));
end;

procedure TXRTLDataOutputStream.WriteDateTime(AValue: TDateTime);
begin
  WriteBuffer(AValue, SizeOf(AValue));
end;

procedure TXRTLDataOutputStream.WriteDouble(AValue: Double);
begin
  WriteBuffer(AValue, SizeOf(AValue));
end;

procedure TXRTLDataOutputStream.WriteInt64(AValue: Int64);
begin
  WriteBuffer(AValue, SizeOf(AValue));
end;

procedure TXRTLDataOutputStream.WriteInteger(AValue: Integer);
begin
  WriteBuffer(AValue, SizeOf(AValue));
end;

procedure TXRTLDataOutputStream.WriteShortInt(AValue: ShortInt);
begin
  WriteBuffer(AValue, SizeOf(AValue));
end;

procedure TXRTLDataOutputStream.WriteSingle(AValue: Single);
begin
  WriteBuffer(AValue, SizeOf(AValue));
end;

procedure TXRTLDataOutputStream.WriteSmallInt(AValue: SmallInt);
begin
  WriteBuffer(AValue, SizeOf(AValue));
end;

procedure TXRTLDataOutputStream.WriteString(AValue: string);
begin
  WriteInteger(Length(AValue));
  WriteBuffer(PChar(AValue)^, Length(AValue));
end;

procedure TXRTLDataOutputStream.WriteUTF8String(AValue: WideString);
begin
  WriteString(UTF8Encode(AValue));
end;

procedure TXRTLDataOutputStream.WriteVariant(AValue: Variant);
var
  VD: TVarData;
  I, DimCount, ElemCount, DimElemCount, ElemSize: Integer;
  ArrayData: Pointer;
begin
  WriteInteger(VarType(AValue));
  VD:= TVarData(AValue);
  if VarIsArray(AValue) then
  begin
    DimCount:= VarArrayDimCount(AValue);
    WriteByte(DimCount);
    ElemCount:= 1;
    for I:= 0 to DimCount - 1 do
    begin
      DimElemCount:= VarArrayHighBound(AValue, I + 1) - VarArrayLowBound(AValue, I + 1) + 1;
      ElemCount:= ElemCount * DimElemCount;
      WriteInteger(DimElemCount);
    end;
    try
      ArrayData:= VarArrayLock(AValue);
      for I:= 0 to ElemCount - 1 do
      begin
        ElemSize:= 0;
        case VarType(AValue) and varTypeMask of
          VT_I1, VT_UI1:
          begin
            ElemSize:= SizeOf(Byte);
            WriteByte(PByte(ArrayData)^);
          end;
          VT_I2, VT_UI2:
          begin
            ElemSize:= SizeOf(SmallInt);
            WriteSmallInt(PSmallInt(ArrayData)^);
          end;
          VT_I4, VT_UI4:
          begin
            ElemSize:= SizeOf(Integer);
            WriteInteger(PInteger(ArrayData)^);
          end;
          VT_R4:
          begin
            ElemSize:= SizeOf(Single);
            WriteSingle(PSingle(ArrayData)^);
          end;
          VT_R8:
          begin
            ElemSize:= SizeOf(Double);
            WriteDouble(PDouble(ArrayData)^);
          end;
          VT_CY:
          begin
            ElemSize:= SizeOf(Currency);
            WriteCurrency(PCurrency(ArrayData)^);
          end;
          VT_DATE:
          begin
            ElemSize:= SizeOf(TDateTime);
            WriteDateTime(PDateTime(ArrayData)^);
          end;
          VT_BSTR:
          begin
            ElemSize:= SizeOf(PWideChar);
            WriteWideString(WideString(ArrayData^));
          end;
          VT_BOOL:
          begin
            ElemSize:= SizeOf(WordBool);
            WriteWordBool(PWordBool(ArrayData)^);
          end;
          VT_DECIMAL:
          begin
            ElemSize:= 16;
            WriteBuffer(ArrayData^, 16);
          end;
          VT_VARIANT:
          begin
            ElemSize:= SizeOf(TVarData);
            WriteVariant(Variant(PVarData(ArrayData)^));
          end;
        else
        end;
        ArrayData:= Pointer(Integer(ArrayData) + ElemSize);
      end;
    finally
      VarArrayUnlock(AValue);
    end;
  end
  else
  begin
    case VarType(AValue) of
      VT_I1, VT_UI1:
        WriteByte(VD.VByte);
      VT_I2, VT_UI2:
        WriteSmallInt(VD.VSmallInt);
      VT_I4, VT_UI4:
        WriteInteger(VD.VInteger);
      VT_R4:
        WriteSingle(VD.VSingle);
      VT_R8:
        WriteDouble(VD.VDouble);
      VT_CY:
        WriteCurrency(VD.VCurrency);
      VT_DATE:
        WriteDateTime(VD.VDate);
      VT_BSTR:
        WriteWideString(VD.VOleStr);
      VT_BOOL:
        WriteWordBool(VD.VBoolean);
      VT_DECIMAL:
        WriteBuffer(VD, 16);
    else
    end;
  end;
end;

procedure TXRTLDataOutputStream.WriteWideString(AValue: WideString);
begin
  WriteInteger(Length(AValue));
  WriteBuffer(Pointer(AValue)^, Length(AValue) * SizeOf(WideChar));
end;

procedure TXRTLDataOutputStream.WriteWordBool(AValue: WordBool);
begin
  WriteBuffer(AValue, SizeOf(AValue));
end;

end.
