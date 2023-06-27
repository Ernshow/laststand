unit Chaos;

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

const
  CHAOS_DURATION = 30; // in seconds
  NEXT_CHAOS_TIMER = 30; // in seconds

var
	ChaosModeEnabled: boolean;
	Countdown: integer;


procedure Chaos_SetChaosModeEnabled(Enabled: boolean);
procedure Chaos_OnTick();

implementation

procedure Chaos_SetChaosModeEnabled(Enabled: Boolean);
begin
	ChaosModeEnabled := Enabled;
end;

procedure Chaos_OnTick();
begin
	if ChaosModeEnabled then begin
		//WriteMessage(0, 'Chaos mode tick', RED);
		Countdown := Countdown + 1;
		if (Countdown mod NEXT_CHAOS_TIMER = 0) then begin
			WriteMessage(0, 'Will select chaos mode', RED);
			Countdown := 0;
		end;
	end;
end;

end.
