unit xrtl_io_Exception;

{$INCLUDE xrtl.inc}

interface

uses
  xrtl_util_Exception;

type
  EXRTLIOException          = class(EXRTLException);

  EXRTLSeekException        = class(EXRTLIOException);
  EXRTLReadException        = class(EXRTLIOException);
  EXRTLWriteException       = class(EXRTLIOException);
  EXRTLFlushException       = class(EXRTLIOException);
  EXRTLMarkException        = class(EXRTLIOException);
  EXRTLSkipException        = class(EXRTLIOException);
  EXRTLRestoreException     = class(EXRTLIOException);
  EXRTLEndOfStreamException = class(EXRTLIOException);

implementation

end.
