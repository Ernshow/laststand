unit ExceptionHandler;

interface

{$ifdef FPC}
	uses Scriptcore;
{$endif}

const
    EH_EXCEPTION_FILE_PATH = '~/exceptions.log';

procedure EH_LogException(msg: string);

implementation

{$ifndef FPC}
var
    EH_ExceptionFile: TStringList;

  {$endif}
procedure EH_LogException(msg: string);
begin
    {$ifndef FPC}
    EH_ExceptionFile.Append(msg);
    EH_ExceptionFile.SaveToFile(EH_EXCEPTION_FILE_PATH);
  {$endif}
end;

function EH_ExceptionHandler(ErrorCode: TErrorType;
    Message, UnitName, FunctionName: string; Row, Col: cardinal): boolean;
var
    msg: string;
begin
    //if Message = '' then Message := IntToStr(cardinal(TErrorType));
    msg := UnitName + '(' + IntToStr(Row) + ',' + IntToStr(Col) + ') [' +
        FunctionName + ']: ' + Message;
    WriteLn('Unhandled exception occured:');
    WriteLn(msg);
    EH_LogException(msg);
    Result := True;
end;

initialization
{$ifndef FPC}
    if file.Exists(EH_EXCEPTION_FILE_PATH) then
        EH_ExceptionFile := file.CreateStringListFromFile(EH_EXCEPTION_FILE_PATH)
    else
        EH_ExceptionFile := file.CreateStringList();
    Script.OnUnhandledException := @EH_ExceptionHandler;
{$endif}
finalization
{$ifndef FPC}
    EH_ExceptionFile.Free;
{$endif}
end.
