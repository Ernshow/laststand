unit Rules;

interface

uses
	Scriptcore,
	Botwizard,
	Constants,
	Globals,
	LSPlayers,
	MersenneTwister,
	Misc,
	maths,
	gamemodes,
	PlacePlayer;

type
  TKey = string;
  TValue = byte;

  TEntry = record
    Key: TKey;
    Value: TValue;
  end;

const
  TABLE_SIZE = 100; // Size of the hash table

var
  HashTable: array[0..TABLE_SIZE-1] of TEntry;

function Rules_HashFunction(const Key: TKey): integer;
procedure Rules_InitializeDefaultRules();
procedure _protected_SetRuleValue(const Key: TKey; const Value: TValue);
procedure Rules_SetRuleValue(const Key: TKey; const Value: TValue);
procedure Rules_ResetToDefaultRules();
function Rules_GetRuleValue(const Key: TKey): TValue;
procedure Rules_PrintRules(ID: byte);

implementation

function Rules_HashFunction(const Key: TKey): integer;
var
  HashValue: integer;
  Ch: Char;
  i: integer;
begin
  HashValue := 0;
  for i := 1 to length(Key) - 1 do
  begin
    Ch := Key[i];
    HashValue := (HashValue + Ord(Ch)) mod TABLE_SIZE;
  end;
  Result := HashValue;
end;

procedure Rules_InitializeDefaultRules();
var
  i: Integer;
begin
  for i := 0 to TABLE_SIZE - 1 do
  begin
    HashTable[i].Key := '';
    HashTable[i].Value := 0;
  end;
  // Set the default rules
  _protected_SetRuleValue('role.doc.allow_revive', 1);
  _protected_SetRuleValue('role.doc.allow_heal', 1);
  _protected_SetRuleValue('role.mech.allow_wire', 1);
  _protected_SetRuleValue('role.mech.allow_sg', 1);
  _protected_SetRuleValue('role.cop.allow_heli', 1);
  _protected_SetRuleValue('role.demo.allow_mine', 1);
  _protected_SetRuleValue('role.demo.allow_charge', 1);
end;

procedure _protected_SetRuleValue(const Key: TKey; const Value: TValue);
var
  HashIndex: Integer;
begin
  HashIndex := Rules_HashFunction(Key);
  HashTable[HashIndex].Key := Key;
  HashTable[HashIndex].Value := Value;
end;

procedure Rules_SetRuleValue(const Key: TKey; const Value: TValue);
var
  HashIndex: Integer;
begin
  HashIndex := Rules_HashFunction(Key);
  if HashTable[HashIndex].Key = Key then begin
    HashTable[HashIndex].Key := Key;
    HashTable[HashIndex].Value := Value;
    WriteMessage(0, 'Game rule has been changed: ' + Key + ' => ' + IntToStr(Value), RED);
  end;
end;

procedure Rules_ResetToDefaultRules();
begin
  Rules_InitializeDefaultRules();
end;

function Rules_GetRuleValue(const Key: TKey): TValue;
var
  HashIndex: Integer;
begin
  HashIndex := Rules_HashFunction(Key);
  if (HashTable[HashIndex].Key = Key) then
    Result := HashTable[HashIndex].Value
  else
  begin
    Result := 0;
  end;
end;

procedure Rules_PrintRules(ID: byte);
var
  Entry: TEntry;
  i: byte;
begin
  WriteMessage(ID, 'Currently active rules:', WHITE);
  for i := 0 to Length(HashTable) - 1 do
  begin
    Entry := HashTable[i];
    if Entry.Key <> '' then
      WriteMessage(ID, Entry.Key +' => ' + IntToStr(Entry.Value), WHITE);
  end;
end;

end.
