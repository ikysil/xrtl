unit xrtl_io_ResourceStrings;

{$INCLUDE xrtl.inc}

interface

resourcestring
  SXRTLReadExceptionFmt     = '%s can''t read';
  SXRTLWriteExceptionFmt    = '%s can''t write';
  SXRTLFlushExceptionFmt    = '%s can''t flush';
  SXRTLSeekExceptionFmt     = '%s can''t seek';
  SXRTLMarkExceptionFmt     = '%s can''t mark';
  SXRTLSkipExceptionFmt     = '%s can''t skip';
  SXRTLRestoreExceptionFmt  = '%s can''t restore';
  SXRTLEndOfStreamException = 'End of stream in %s';
  SXRTLGetByteRange         = 'Call to %s.GetByte [property Bytes] with index <> [0..%d]';
  SXRTLInvalidDataTag       = 'Invalid data tag read [%d] while expecting [%d]';

implementation

end.
