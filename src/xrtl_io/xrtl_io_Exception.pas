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

{ @abstract(Signals that an end of file or end of stream has been reached
            unexpectedly during input.)
  This exception is mainly used by data input streams to signal end of stream.
  Note that many other input operations return a special value
  (@link(XRTLEndOfStreamValue)) on end of stream rather than throwing an exception.
}
  EXRTLEndOfStreamException = class(EXRTLIOException);

implementation

end.
