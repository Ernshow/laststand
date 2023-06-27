unit Prelude;

interface

function Show(x: Variant): Variant;
function EscapeQuotesBackslash(str: String): String;
function RightPad(str: String; chara: String; num: Integer): String;
function LeftPad(str: String; chara: String; num: Integer): String;

implementation

// Convert a primitive to a string
function Show(x: Variant): Variant;
begin
	case VarType(x) of
		varString:
			Result := x;
		varEmpty:
      Result := 'Empty';
    varNull:
			Result := 'Null';
		varSingle, varDouble, varByte, varWord, varLongWord,
		varInteger, varShortInt, varSmallInt, varInt64:
			Result := IntToStr(x);
		varBoolean:
			if x = True then
				Result := 'true'
			else
				Result := 'false'
	else
		Result := NULL;
	end;
end;

function EscapeQuotesBackslash(str: String): String;
begin
	Result := ReplaceRegExpr('\\', str, '\\', False);
	Result := ReplaceRegExpr('"', Result, '\"', False);
	Result := ReplaceRegExpr('''', Result, '''\\''', False);
end;

function RightPad(str: String; chara: String; num: Integer): String;
var
	i: Integer;
	len: Integer;
begin
	len := Length(str);
	Result := str;
	if len <= num then
		for i := 0 to (num - len) do
			Result := Result + chara;
end;

function LeftPad(str: String; chara: String; num: Integer): String;
var
	i: Integer;
	len: Integer;
begin
	len := Length(str);
	Result := str;
	if len <= num then
		for i := (num - len) downto 0 do
			Result := chara + Result;
end;

end.
