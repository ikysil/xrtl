unit xrtl_opc_sdk_ResourceStrings;

{$INCLUDE xrtl.inc}

interface

resourcestring
  SShutdown        = 'Shutdown';

// OPC return value messages
  SOPC_E_INVALIDHANDLE     = 'The value of the handle is invalid.';
  SOPC_E_BADTYPE           = 'The server cannot convert the data between the requested data type and the canonical data type.';
  SOPC_E_PUBLIC            = 'The requested operation cannot be done on a public group.';
  SOPC_E_BADRIGHTS         = 'The Items AccessRights do not allow the operation.';
  SOPC_E_UNKNOWNITEMID     = 'The item is no longer available in the server address space.';
  SOPC_E_INVALIDITEMID     = 'The item definition does not conform to the server''s syntax.';
  SOPC_E_INVALIDFILTER     = 'The filter string was not valid.';
  SOPC_E_UNKNOWNPATH       = 'The item''s access path is not known to the server.';
  SOPC_E_RANGE             = 'The value was out of range.';
  SOPC_E_DUPLICATENAME     = 'Duplicate name not allowed.';
  SOPC_S_UNSUPPORTEDRATE   = 'The server does not support the requested data rate but will use the closest available rate.';
  SOPC_S_CLAMP             = 'A value passed to WRITE was accepted but the output was clamped.';
  SOPC_S_INUSE             = 'The operation cannot be completed because the object still has references that exist.';
  SOPC_E_INVALIDCONFIGFILE = 'The server''s configuration file is an invalid format.';
  SOPC_E_NOTFOUND          = 'The server could not locate the requested object.';
  SOPC_E_INVALID_PID       = 'The server does not recognise the passed property ID.';
  SOPC_S_ALREADYACKED      = 'The condition has already been acknowleged.';
  SOPC_S_INVALIDBUFFERTIME = 'The buffer time parameter was invalid.';
  SOPC_S_INVALIDMAXSIZE    = 'The max size parameter was invalid.';
  SOPC_E_INVALIDBRANCHNAME = 'The string was not recognized as an area name.';
  SOPC_E_INVALIDTIME       = 'The time does not match the latest active time.';
  SOPC_E_BUSY              = 'A refresh is currently in progress.';
  SOPC_E_NOINFO            = 'Information is not available.';

  SOPCDA20Group_ClassName   = 'OPCDA20Group';
  SOPCDA20Group_Description = 'xrtl OPC DA 2.0 Group';

implementation

end.
