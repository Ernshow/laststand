unit Scriptcore;

// This unit provides most of Soldat's Scriptcore2 and Scriptcore3 functions,
// constants, variables and classes in form of empty definitions.
// By "using" it in your script files you can develop your scripts under
// Pascal IDE like Lazaurs.
// Note: the unit must be included with preprocessor, so it stays visible for
// the IDE and the compiler and invisible for SoldatServer. Define SCRIPT
// in Soldat defines.
//
// Some definitions may be missing since we could forget about something.
//
// By tk and Falcon.

interface

{$ifndef SCRIPT}
type
	// //////////////////////////// SC3 classes //////////////////////////////////
	TActiveBullet = class;
	TActiveFlag = class;
	TActiveMapObject = class;
	TActivePlayer = class;
	TActiveSpawnPoint = class;
	TBanLists = class;
	TFile = class;
	TGame = class;
	TGlobal = class;
	TIniFile = class;
	TMap = class;
	TMapsList = class;
	TNewMapObject = class;
	TNewWeapon = class;
	TPlayer = class;
	TPlayers = class;
	TPlayerWeapon = class;
	TScript = class;
	TSpawnPoint = class;
	TStream = class;
	TStringList = class;
	TTeam = class;
	TThing = class;
	TWeapon = class;

	TStringArray = array of string;

	TVector = record
		x, y: single;
	end;

	TValueSign = -1..1;
	TRoundToRange = -37..37;

	// //////////////////////////// SC3 enums ////////////////////////////////////
	TJoinType = (TJoinNormal, TJoinSilent);
	TKickReason = (TKickNoResponse, TKickNoCheatResponse,
		TKickChangeTeam, TKickPing, TKickFlooding, TKickConsole,
		TKickConnectionCheat, TKickCheat, TKickLeft, TKickVoted,
		TKickAC, TKickSilent
	);

	// //////////////////////////// SC3 events ///////////////////////////////////
	// Team
	TOnJoinTeam = procedure(Player: TActivePlayer; Team: TTeam);
	TOnLeaveTeam = procedure(Player: TActivePlayer; Team: TTeam; Kicked: Boolean);
	TOnClockTick = procedure(Ticks: Integer);
	TOnJoin = procedure(Player: TActivePlayer; Team: TTeam);
	TOnLeave = procedure(Player: TActivePlayer; Kicked: Boolean);

	// Game
	TOnRequest = function(
		Ip, Hw: string;
		Port: Word; State: Byte;
		Forwarded: Boolean;
		Password: string
	): Integer;
	TOnAdminCommand = function(Player: TActivePlayer; Command: string): Boolean;
	TOnTCPMessage = procedure(Ip: string; Port: Word; Message: string);
	TOnTCPCommand = function(Ip: string; Port: Word; Command: string): Boolean;
	TOnAdminConnect = procedure(Ip: string; Port: Word);
	TOnAdminDisconnect = procedure(Ip: string; Port: Word);
	TOnBeforeMapChange = procedure(Next: string);
	TOnAfterMapChange = procedure(Next: string);

	// ActivePlayer
	TOnFlagGrab = procedure(Player: TActivePlayer; TFlag: TActiveFlag;
		Team: Byte; GrabbedInBase: Boolean);
	TOnFlagReturn = procedure(Player: TActivePlayer; Flag: TActiveFlag;
		Team: Byte);
	TOnFlagScore = procedure(Player: TActivePlayer; Flag: TActiveFlag;
		Team: Byte);
	TOnFlagDrop = procedure(Player: TActivePlayer; Flag: TActiveFlag;
		Team: Byte; Thrown: Boolean);
	TOnKitPickup = procedure(Player: TActivePlayer;
		Kit: TActiveMapObject);
	TOnBeforeRespawn = function(Player: TActivePlayer): Byte;
	TOnAfterRespawn = procedure(Player: TActivePlayer);
	TOnDamage = function(Shooter, Victim: TActivePlayer; Damage: Integer;
		BulletID: Byte): Integer;
	TOnKill = procedure(Killer, Victim: TActivePlayer;
		WeaponType: Byte);
	TOnWeaponChange = procedure(Player: TActivePlayer;
		Primary, Secondary: TPlayerWeapon);
	TOnVoteMapStart = function(Player: TActivePlayer;
		Map: string): Boolean;
	TOnVoteKickStart = function(Player, Victim: TActivePlayer;
		Reason: string): Boolean;
	TOnVoteMap = procedure(Player: TActivePlayer; Map: string);
	TOnVoteKick = procedure(Player, Victim: TActivePlayer);
	TOnSpeak = procedure(Player: TActivePlayer; Text: string);
	TOnCommand = function(Player: TActivePlayer; Command: string): Boolean;

	// Script
	TErrorType =
			(ErNoError, erCannotImport, erInvalidType, ErInternalError,
			erInvalidHeader, erInvalidOpcode, erInvalidOpcodeParameter, erNoMainProc,
			erOutOfGlobalVarsRange, erOutOfProcRange, ErOutOfRange, erOutOfStackRange,
			ErTypeMismatch, erUnexpectedEof, erVersionError, ErDivideByZero, ErMathError,
			erCouldNotCallProc, erOutofRecordRange, erOutOfMemory, erException,
			erNullPointerException, erNullVariantError, erInterfaceNotSupported, erCustomError);

	TOnException = function(ErrorCode: TErrorType; Message, UnitName, FunctionName: string;
		Row, Col: Cardinal): Boolean;


	////////////////////////////// SC3 classes ///////////////////////////////////
	// ------------------------------- Bullet ------------------------------------
	TActiveBullet = class(TObject)
	protected
		FID: Byte;
		function GetActive: Boolean; abstract;
		function GetID: Byte; abstract;
		function GetStyle: Byte; abstract;
		function GetX: Single; abstract;
		function GetY: Single; abstract;
		function GetVelX: Single; abstract;
		function GetVelY: Single; abstract;
		function GetOwner: Byte; abstract;
	public
		//constructor CreateActive(ID: Byte; var Bul: TBullet);
		function GetOwnerWeaponId: Integer; abstract;
		property Active: Boolean read GetActive;
		property ID: Byte read GetID;
		property Style: Byte read GetStyle;
		property X: Single read GetX;
		property Y: Single read GetY;
		property VelX: Single read GetVelX;
		property VelY: Single read GetVelY;
		property Owner: Byte read GetOwner;
	end;

	// ----------------------------- BanList -------------------------------------
	TBanHW = class(TObject)
		HW: string; Time: Integer; Reason: string;
	end;

	TBanIP = class(TObject)
		IP: string; Time: Integer; Reason: string;
	end;

	TBanLists = class(TObject)
	private
		function GetBannedHW(Num: Integer): TBanHW; abstract;
		function GetBannedIP(Num: Integer): TBanIP; abstract;
		function GetBannedHWCount: Integer; abstract;
		function GetBannedIPCount: Integer; abstract;
	public
		procedure AddHWBan(HW, Reason: string; Duration: Integer); abstract;
		procedure AddIPBan(IP: string; Reason: string; Duration: Integer); abstract;
		function DelHWBan(HW: string): Boolean; abstract;
		function DelIPBan(IP: string): Boolean; abstract;
		function IsBannedHW(HW: string): Boolean; abstract;
		function IsBannedIP(IP: string): Boolean; abstract;
		function GetHWBanId(HW: string): Integer; abstract;
		function GetIPBanId(IP: string): Integer; abstract;
		property HW[i: Integer]: TBanHW read GetBannedHW;
		property IP[i: Integer]: TBanIP read GetBannedIP;
		property BannedHWCount: Integer read GetBannedHWCount;
		property BannedIPCount: Integer read GetBannedIPCount;
	end;

	// --------------------------- File API --------------------------------------
	TStringList = class(TObject)
	public
	end;

	TIniFile = class(TObject)
	public
	end;

	TStream = class(TObject)
	public
	end;

	TFile = class(TObject)
	public
		//constructor Create(API: TScriptFileAPI);
		function CheckAccess(const FilePath: string): Boolean; abstract;
		function CreateFileStream(): TStream; abstract;
		function CreateFileStreamFromFile(const Path: string): TStream; abstract;
		function CreateStringList(): TStringList; abstract;
		function CreateStringListFromFile(const Path: string): TStringList; abstract;
		function CreateINI(const Path: string): TIniFile; abstract;
		function Exists(const Path: string): Boolean; abstract;
		function Copy(const Source, Destination: string): Boolean; abstract;
		function Move(const Source, Destination: string): Boolean; abstract;
		function Delete(const Path: string): Boolean; abstract;
	end;

	// -------------------------------- Game -------------------------------------
	TGame = class(TObject)
		private
			//FTeams: array [0..5] of TTeam;
			//FMapsList: TMapsList;
			//FBanLists: TBanLists;
			FOnClockTick: TOnClockTick;
			FOnJoin: TOnJoin;
			FOnLeave: TOnLeave;
			FOnRequest: TOnRequest;
			FOnAdminCommand: TOnAdminCommand;
			FOnTCPMessage: TOnTCPMessage;
			FOnTCPCommand: TOnTCPCommand;
			FOnAdminConnect: TOnAdminConnect;
			FOnAdminDisconnect: TOnAdminDisconnect;
			FTickThreshold: Longint;
			function GetGameStyle: Byte; abstract;
			procedure SetGameStyle(Style: Byte); abstract;
			function GetMaxPlayers: Byte; abstract;
			procedure SetMaxPlayers(Max: Byte); abstract;
			function GetNextMap: string; abstract;
			function GetCurrentMap: string; abstract;
			function GetNumBots: Byte; abstract;
			function GetNumPlayers: Byte; abstract;
			function GetSpectators: Byte; abstract;
			function GetScoreLimit: Word; abstract;
			procedure SetScoreLimit(Limit: Word); abstract;
			function GetServerIP: string; abstract;
			function GetServerName: string; abstract;
			function GetServerPort: Word; abstract;
			function GetServerVersion: string; abstract;
			function GetServerInfo: string; abstract;
			function GetGravity: Single; abstract;
			procedure SetGravity(Grav: Single); abstract;
			function GetPaused: Boolean; abstract;
			procedure SetPaused(Paused: Boolean); abstract;
			function GetRespawnTime: Integer; abstract;
			procedure SetRespawnTime(Time: Integer); abstract;
			function GetMinRespawnTime: Integer; abstract;
			procedure SetMinRespawnTime(Time: Integer); abstract;
			function GetMaxRespawnTime: Integer; abstract;
			procedure SetMaxRespawnTime(Time: Integer); abstract;
			function GetMaxGrenades: Byte; abstract;
			procedure SetMaxGrenades(Num: Byte); abstract;
			function GetBonus: Byte; abstract;
			procedure SetBonus(Num: Byte); abstract;
			function GetTimeLimit: Longint; abstract;
			procedure SetTimeLimit(Num: Longint); abstract;
			function GetTimeLeft: Longint; abstract;
			function GetFriendlyFire: Boolean; abstract;
			procedure SetFriendlyFire(Enabled: Boolean); abstract;
			function GetPassword: string; abstract;
			procedure SetPassword(Pass: string); abstract;
			function GetAdminPassword: string; abstract;
			procedure SetAdminPassword(Pass: string); abstract;
			function GetVotePercent: Byte; abstract;
			procedure SetVotePercent(Percent: Byte); abstract;
			function GetRealistic: Boolean; abstract;
			procedure SetRealistic(Enabled: Boolean); abstract;
			function GetSurvival: Boolean; abstract;
			procedure SetSurvival(Enabled: Boolean); abstract;
			function GetAdvance: Boolean; abstract;
			procedure SetAdvance(Enabled: Boolean); abstract;
			function GetBalance: Boolean; abstract;
			procedure SetBalance(Enabled: Boolean); abstract;
			function GetTickCount: Longint; abstract;
			function GetTeam(ID: Byte): TTeam; abstract;
			function GetMapsList: TMapsList; abstract;
			function GetBanLists: TBanLists; abstract;
		public
			constructor Create; abstract;
			procedure Shutdown; abstract;
			procedure StartVoteKick(ID: Byte; Reason: string); abstract;
			procedure StartVoteMap(Name: string); abstract;
			procedure Restart; abstract;
			function LoadWeap(WeaponMod: string): Boolean; abstract;
			function LoadCon(ConfigFile: string): Boolean; abstract;
			function LoadList(MapsList: string): Boolean; abstract;
			function LobbyRegister: Boolean; abstract;
			property GameStyle: Byte read GetGameStyle write SetGameStyle;
			property MaxPlayers: Byte read GetMaxPlayers write SetMaxPlayers;
			property NextMap: string read GetNextMap;
			property CurrentMap: string read GetCurrentMap;
			property NumBots: Byte read GetNumBots;
			property NumPlayers: Byte read GetNumPlayers;
			property Spectators: Byte read GetSpectators;
			property ScoreLimit: Word read GetScoreLimit write SetScoreLimit;
			property ServerIP: string read GetServerIP;
			property ServerName: string read GetServerName;
			property ServerPort: Word read GetServerPort;
			property ServerVersion: string read GetServerVersion;
			property ServerInfo: string read GetServerInfo;
			property Gravity: Single read GetGravity write SetGravity;
			property Paused: Boolean read GetPaused write SetPaused;
			property RespawnTime: Longint read GetRespawnTime write SetRespawnTime;
			property MinRespawnTime: Longint read GetMinRespawnTime write SetMinRespawnTime;
			property MaxRespawnTime: Longint read GetMaxRespawnTime write SetMaxRespawnTime;
			property MaxGrenades: Byte read GetMaxGrenades write SetMaxGrenades;
			property Bonus: Byte read GetBonus write SetBonus;
			property TimeLimit: Longint read GetTimeLimit write SetTimeLimit;
			property TimeLeft: Longint read GetTimeLeft;
			property FriendlyFire: Boolean read GetFriendlyFire write SetFriendlyFire;
			property Password: string read GetPassword write SetPassword;
			property AdminPassword: string read GetAdminPassword write SetAdminPassword;
			property VotePercent: Byte read GetVotePercent write SetVotePercent;
			property Realistic: Boolean read GetRealistic write SetRealistic;
			property Survival: Boolean read GetSurvival write SetSurvival;
			property Advance: Boolean read GetAdvance write SetAdvance;
			property Balance: Boolean read GetBalance write SetBalance;
			property TickThreshold: Longint read FTickThreshold write FTickThreshold;
			property TickCount: Longint read GetTickCount;
			property Teams[ID: Byte]: TTeam read GetTeam;
			property ScriptMapsList: TMapsList read GetMapsList;
			property ScriptBanLists: TBanLists read GetBanLists;
			property OnClockTick: TOnClockTick read FOnClockTick write FOnClockTick;
			property OnJoin: TOnJoin read FOnJoin write FOnJoin;
			property OnLeave: TOnLeave read FOnLeave write FOnLeave;
			property OnRequest: TOnRequest read FOnRequest write FOnRequest;
			property OnAdminCommand: TOnAdminCommand read FOnAdminCommand write FOnAdminCommand;
			property OnTCPMessage: TOnTCPMessage read FOnTCPMessage write FOnTCPMessage;
			property OnTCPCommand: TOnTCPCommand read FOnTCPCommand write FOnTCPCommand;
			property OnAdminConnect: TOnAdminConnect read FOnAdminConnect write FOnAdminConnect;
			property OnAdminDisconnect: TOnAdminDisconnect read FOnAdminDisconnect write FOnAdminDisconnect;
		end;

	// -------------------------------- Global -----------------------------------
	TGlobal = class(TObject)
	private
		function GetDateSeparator: Char; abstract;
		procedure SetDateSeparator(Separator: Char); abstract;
		function GetShortDateFormat: string; abstract;
		procedure SetShortDateFormat(Format: string); abstract;
	public
		property ScriptDateSeparator: Char read GetDateSeparator write SetDateSeparator;
		property ScriptShortDateFormat: string read GetShortDateFormat write SetShortDateFormat;
	end;

	// --------------------------------- Map -------------------------------------
	TMap = class(TObject)
		private
			//FObjects: array [1..128] of TActiveMapObject;
			//FBullets: array [1..256] of TActiveBullet;
			//FSpawnpoints: array [1..128] of TActiveSpawnPoint;
			//FLastFlagObjs: array [1..3] of TActiveFlag;
			FOnBeforeMapChange: TOnBeforeMapChange;
			FOnAfterMapChange: TOnAfterMapChange;
			function GetObject(ID: Byte): TActiveMapObject; abstract;
			function GetBullet(ID: Byte): TActiveBullet; abstract;
			function GetSpawn(ID: Byte): TSpawnPoint; abstract;
			procedure SetSpawn(ID: Byte; const Spawn: TSpawnPoint); abstract;
			function GetName: string; abstract;
		public
			constructor Create; abstract;
			// function GetFlag(ID: Integer): TActiveFlag; abstract;
			function RayCast(x1, y1, x2, y2: Single; Player: Boolean = False;
				Flag: Boolean = False; Bullet: Boolean = True; CheckCollider: Boolean = False;
				Team: Byte = 0): Boolean; abstract;
			function RayCastVector(A, B: TVector; Player: Boolean = False;
				Flag: Boolean = False; Bullet: Boolean = True; CheckCollider: Boolean = False;
				Team: Byte = 0): Boolean; abstract;
			function CreateBulletVector(A, B: TVector; HitM: Single; sStyle: Byte;
				Owner: TActivePlayer): Integer; abstract;
			function CreateBullet(X, Y, VelX, VelY, HitM: Single; sStyle: Byte; Owner: TActivePlayer): Integer; abstract;
			function AddObject(Obj: TNewMapObject): TActiveMapObject; abstract;
			function AddSpawnPoint(Spawn: TSpawnPoint): TActiveSpawnPoint; abstract;
			procedure NextMap; abstract;
			procedure SetMap(NewMap: string); abstract;
			property Objects[ID: Byte]: TActiveMapObject read GetObject;
			property Bullets[ID: Byte]: TActiveBullet read GetBullet;
			property Spawns[ID: Byte]: TSpawnPoint read GetSpawn write SetSpawn;
			property Name: string read GetName;
			property RedFlag: TActiveFlag index 1 read GetFlag;
			property BlueFlag: TActiveFlag index 2 read GetFlag;
			property YellowFlag: TActiveFlag index 3 read GetFlag;
			property OnBeforeMapChange: TOnBeforeMapChange
				read FOnBeforeMapChange write FOnBeforeMapChange;
			property OnAfterMapChange: TOnAfterMapChange
				read FOnAfterMapChange write FOnAfterMapChange;
		end;

	// ----------------------------- Maplist -------------------------------------
	TMapsList = class(TObject)
	private
		function GetMap(Num: Integer): string; abstract;
		function GetCurrentMapId: Integer; abstract;
		procedure SetCurrentMapId(NewNum: Integer); abstract;
		function GetMapsCount: Integer; abstract;
	public
		procedure AddMap(Name: string); abstract;
		procedure RemoveMap(Name: string); abstract;
		function GetMapIdByName(Name: string): Integer; abstract;
		property Map[i: Integer]: string read GetMap; default;
		property CurrentMapId: Integer read GetCurrentMapId write SetCurrentMapId;
		property MapsCount: Integer read GetMapsCount;
	end;

	// ------------------------------- Math --------------------------------------
	TMath = class
	private
		function GetE: Extended; abstract;
		function GetPi: Extended; abstract;
	public
		function Sin(A: Extended): Extended; abstract;
		function Cos(A: Extended): Extended; abstract;
		function Tan(A: Extended): Extended; abstract;
		function Cotan(A: Extended): Extended; abstract;
		function Pow(A, B: Extended): Extended; abstract;
		function LogN(A, B: Extended): Extended; abstract;
		function Ln(A: Extended): Extended; abstract;
		function ArcSin(A: Extended): Extended; abstract;
		function ArcCos(A: Extended): Extended; abstract;
		function ArcTan(A: Extended): Extended; abstract;
		function ArcCotan(A: Extended): Extended; abstract;
		function ArcTan2(A, B: Extended): Extended; abstract;
		function Min(A, B: Extended): Extended; abstract;
		function Max(A, B: Extended): Extended; abstract;
		function Abs(A: Extended): Extended; abstract;
		function Exp(A: Extended): Extended; abstract;
		function Sign(A: Extended): TValueSign; abstract;
		function IsNaN(A: Extended): Boolean; abstract;
		function Round(A: Extended): Integer; abstract;
		function RoundTo(A: Extended; B: TRoundToRange): Extended; abstract;
		function DegToRad(A: Extended): Extended; abstract;
		function RadToDeg(A: Extended): Extended; abstract;
		function DegNormalize(A: Extended): Extended; abstract;
		function InRange(const A, B, C: Double): Boolean; abstract;
		function EnsureRange(const AValue, AMin, AMax: Double): Double; abstract;
		function Random(Min, Max: Extended): Extended; abstract;
		property E: Extended read GetE;
		property Pi: Extended read GetPi;
	end;

	// ------------------------------ Object -------------------------------------
	TThing = class(TObject)
	protected
		//FObj: PThing;
		function GetStyle: Byte; virtual; abstract;
		function GetX: Single; abstract;
		function GetY: Single; abstract;
	public
		property Style: Byte read GetStyle;
		property X: Single read GetX;
		property Y: Single read GetY;
	end;

	TNewMapObject = class(TThing)
	protected
		function GetStyle: Byte; override; abstract;
		procedure SetStyle(Style: Byte); abstract;
		procedure SetX(X: Single); abstract;
		procedure SetY(Y: Single); abstract;
	public
		constructor Create; abstract;
		destructor Destroy; override; abstract;
		property Style: Byte read GetStyle write SetStyle;
		property X: Single read GetX write SetX;
		property Y: Single read GetY write SetY;
	end;

	TActiveMapObject = class(TThing)
	protected
		FID: Byte;
		function GetActive: Boolean; abstract;
		function GetID: Byte; abstract;
		function GetStyle: Byte; override; abstract;
	public
		//constructor CreateActive(ID: Byte; var Obj: TThing);
		procedure Kill; abstract;
		property Active: Boolean read GetActive;
		property ID: Byte read GetID;
	end;

	TActiveFlag = class(TActiveMapObject)
	private
		function GetInBase: Boolean; abstract;
	public
		property InBase: Boolean read GetInBase;
	end;

	// ------------------------------ Player -------------------------------------
	TPlayer = class(TObject)
		protected
			FPrimary: TWeapon;
			FSecondary: TWeapon;
			//function GetSprite: TSprite;
			function GetTeam: Byte; abstract;
			function GetName: string; abstract;
			function GetAlive: Boolean; abstract;
			function GetHealth: Single; abstract;
			procedure SetHealth(Health: Single); abstract;
			function GetVest: Single; abstract;
			procedure SetVest(Vest: Single); abstract;
			function GetPrimary: TPlayerWeapon; abstract;
			function GetSecondary: TPlayerWeapon; abstract;
			function GetShirtColor: Longword; abstract;
			function GetPantsColor: Longword; abstract;
			function GetSkinColor: Longword; abstract;
			function GetHairColor: Longword; abstract;
			function GetFavouriteWeapon: string; abstract;
			procedure SetFavouriteWeapon(Weapon: string); abstract;
			function GetChosenSecondaryWeapon: Byte; abstract;
			function GetFriend: string; abstract;
			procedure SetFriend(Friend: string); abstract;
			function GetAccuracy: Byte; abstract;
			procedure SetAccuracy(Accuracy: Byte); abstract;
			function GetShootDead: Boolean; abstract;
			procedure SetShootDead(ShootDead: Boolean); abstract;
			function GetGrenadeFrequency: Byte; abstract;
			procedure SetGrenadeFrequency(Frequency: Byte); abstract;
			function GetCamping: Boolean; abstract;
			procedure SetCamping(Camping: Boolean); abstract;
			function GetOnStartUse: Byte; abstract;
			procedure SetOnStartUse(Thing: Byte); abstract;
			function GetHairStyle: Byte; abstract;
			function GetHeadgear: Byte; abstract;
			function GetChain: Byte; abstract;
			function GetChatFrequency: Byte; abstract;
			procedure SetChatFrequency(Frequency: Byte); abstract;
			function GetChatKill: string; abstract;
			procedure SetChatKill(Message: string); abstract;
			function GetChatDead: string; abstract;
			procedure SetChatDead(Message: string); abstract;
			function GetChatLowHealth: string; abstract;
			procedure SetChatLowHealth(Message: string); abstract;
			function GetChatSeeEnemy: string; abstract;
			procedure SetChatSeeEnemy(Message: string); abstract;
			function GetChatWinning: string; abstract;
			procedure SetChatWinning(Message: string); abstract;
			function GetAdmin: Boolean; abstract;
			procedure SetAdmin(SetAsAdmin: Boolean); abstract;
			function GetDummy: Boolean; abstract;
			procedure SetDummy(Dummy: Boolean); abstract;
		public
			destructor Destroy; override; abstract;
			//Not exported
			//property Sprite: TSprite read GetSprite;
			property Team: Byte read GetTeam;
			property Name: string read GetName;
			property Alive: Boolean read GetAlive;
			property Health: Single read GetHealth write SetHealth;
			property Vest: Single read GetVest write SetVest;
			property Primary: TPlayerWeapon read GetPrimary;
			property Secondary: TPlayerWeapon read GetSecondary;
			property ShirtColor: Longword read GetShirtColor;
			property PantsColor: Longword read GetPantsColor;
			property SkinColor: Longword read GetSkinColor;
			property HairColor: Longword read GetHairColor;
			property FavouriteWeapon: string read GetFavouriteWeapon write SetFavouriteWeapon;
			property ChosenSecondaryWeapon: Byte read GetChosenSecondaryWeapon;
			property Friend: string read GetFriend write SetFriend;
			property Accuracy: Byte read GetAccuracy write SetAccuracy;
			property ShootDead: Boolean read GetShootDead write SetShootDead;
			property GrenadeFrequency: Byte read GetGrenadeFrequency write SetGrenadeFrequency;
			property Camping: Boolean read GetCamping write SetCamping;
			property OnStartUse: Byte read GetOnStartUse write SetOnStartUse;
			property HairStyle: Byte read GetHairStyle;
			property Headgear: Byte read GetHeadgear;
			property Chain: Byte read GetChain;
			property ChatFrequency: Byte read GetChatFrequency write SetChatFrequency;
			property ChatKill: string read GetChatKill write SetChatKill;
			property ChatDead: string read GetChatDead write SetChatDead;
			property ChatLowHealth: string read GetChatLowHealth write SetChatLowHealth;
			property ChatSeeEnemy: string read GetChatSeeEnemy write SetChatSeeEnemy;
			property ChatWinning: string read GetChatWinning write SetChatWinning;
			property IsAdmin: Boolean read GetAdmin write SetAdmin;
			property Dummy: Boolean read GetDummy write SetDummy;
		end;

		TNewPlayer = class(TPlayer)
		protected
			procedure SetName(Name: string); abstract;
			procedure SetTeam(Team: Byte); abstract;
			procedure SetHealth(Health: Single); abstract;
			function GetPrimary: TWeapon; abstract;
			procedure SetPrimary(Primary: TWeapon); abstract;
			function GetSecondary: TWeapon; abstract;
			procedure SetSecondary(Secondary: TWeapon); abstract;
			procedure SetShirtColor(Color: Longword); abstract;
			procedure SetPantsColor(Color: Longword); abstract;
			procedure SetSkinColor(Color: Longword); abstract;
			procedure SetHairColor(Color: Longword); abstract;
			procedure SetHairStyle(Style: Byte); abstract;
			procedure SetHeadgear(Headgear: Byte); abstract;
			procedure SetChain(Chain: Byte); abstract;
			function GetDummy: Boolean; abstract;
			procedure SetDummy(Dummy: Boolean); abstract;
		public
			constructor Create; abstract;
			property Name: string read GetName write SetName;
			property Team: Byte read GetTeam write SetTeam;
			property Health: Single read GetHealth write SetHealth;
			property Primary: TWeapon read GetPrimary; write SetPrimary;
			property Secondary: TWeapon read GetSecondary write SetSecondary;
			property ShirtColor: Longword read GetShirtColor write SetShirtColor;
			property PantsColor: Longword read GetPantsColor write SetPantsColor;
			property SkinColor: Longword read GetSkinColor write SetSkinColor;
			property HairColor: Longword read GetHairColor write SetHairColor;
			property HairStyle: Byte read GetHairStyle write SetHairStyle;
			property Headgear: Byte read GetHeadgear write SetHeadgear;
			property Chain: Byte read GetChain write SetChain;
			property Dummy: Boolean read GetDummy write SetDummy;
		end;

		TActivePlayer = class(TPlayer)
		private
			FID: Byte;
			FOnFlagGrab: TOnFlagGrab;
			FOnFlagReturn: TOnFlagReturn;
			FOnFlagScore: TOnFlagScore;
			FOnFlagDrop: TOnFlagDrop;
			FOnKitPickup: TOnKitPickup;
			FOnBeforeRespawn: TOnBeforeRespawn;
			FOnAfterRespawn: TOnAfterRespawn;
			FOnDamage: TOnDamage;
			FOnKill: TOnKill;
			FOnWeaponChange: TOnWeaponChange;
			FOnVoteMapStart: TOnVoteMapStart;
			FOnVoteKickStart: TOnVoteKickStart;
			FOnVoteMap: TOnVoteMap;
			FOnVoteKick: TOnVoteKick;
			FOnSpeak: TOnSpeak;
			FOnCommand: TOnCommand;
		protected
			function GetKills: Integer; abstract;
			procedure SetKills(Kills: Integer); abstract;
			function GetDeaths: Integer; abstract;
			function GetPing: Integer; abstract;
			procedure SetTeam(Team: Byte); abstract;
			function GetActive: Boolean; abstract;
			procedure SetAlive(Alive: Boolean); abstract;
			function GetIP: string; abstract;
			function GetPort: Word; abstract;
			function GetVelX: Single; abstract;
			function GetVelY: Single; abstract;
			function GetMuted: Boolean; abstract;
			procedure SetMuted(Muted: Boolean); abstract;
			function GetJets: Integer; abstract;
			function GetGrenades: Byte; abstract;
			function GetX: Single; abstract;
			function GetY: Single; abstract;
			function GetMouseAimX: SmallInt; abstract;
			procedure SetMouseAimX(AimX: SmallInt); abstract;
			function GetMouseAimY: SmallInt; abstract;
			procedure SetMouseAimY(AimY: SmallInt); abstract;
			function GetFlagger: Boolean; abstract;
			function GetTime: Integer; abstract;
			function GetOnGround: Boolean; abstract;
			function GetProne: Boolean; abstract;
			function GetHuman: Boolean; abstract;
			function GetDirection: Shortint; abstract;
			function GetFlags: Byte; abstract;
			function GetHWID: string; abstract;
			function GetKeyUp: Boolean; abstract;
			procedure SetKeyUp(Pressed: Boolean); abstract;
			function GetKeyLeft: Boolean; abstract;
			procedure SetKeyLeft(Pressed: Boolean); abstract;
			function GetKeyRight: Boolean; abstract;
			procedure SetKeyRight(Pressed: Boolean); abstract;
			function GetKeyShoot: Boolean; abstract;
			procedure SetKeyShoot(Pressed: Boolean); abstract;
			function GetKeyJetpack: Boolean; abstract;
			procedure SetKeyJetpack(Pressed: Boolean); abstract;
			function GetKeyGrenade: Boolean; abstract;
			procedure SetKeyGrenade(Pressed: Boolean); abstract;
			function GetKeyChangeWeap: Boolean; abstract;
			procedure SetKeyChangeWeap(Pressed: Boolean); abstract;
			function GetKeyThrow: Boolean; abstract;
			procedure SetKeyThrow(Pressed: Boolean); abstract;
			function GetKeyReload: Boolean; abstract;
			procedure SetKeyReload(Pressed: Boolean); abstract;
			function GetKeyCrouch: Boolean; abstract;
			procedure SetKeyCrouch(Pressed: Boolean); abstract;
			function GetKeyProne: Boolean; abstract;
			procedure SetKeyProne(Pressed: Boolean); abstract;
			function GetKeyFlagThrow: Boolean; abstract;
			procedure SetKeyFlagThrow(Pressed: Boolean); abstract;
			procedure SetWeaponActive(ID: Byte; Active: Boolean); abstract;
		public
			//constructor Create(var Sprite: TSprite; ID: Byte); overload;
			function Ban(Time: Integer; Reason: string): Boolean; abstract;
			procedure Say(Text: string); abstract;
			procedure Damage(Shooter: Byte; Damage: Integer); abstract;
			procedure BigText(Layer: Byte; Text: string; Delay: Integer;
				Color: Longint; Scale: Single; X, Y: Integer); abstract;
			procedure WorldText(Layer: Byte; Text: string; Delay: Integer;
				Color: Longint; Scale, X, Y: Single); abstract;
			procedure ForceWeapon(Primary, Secondary: TNewWeapon); abstract;
			procedure ForwardTo(TargetIP: string; TargetPort: Word; Message: string); abstract;
			procedure GiveBonus(BType: Byte); abstract;
			function Kick(reason: TKickReason): Boolean; abstract;
			procedure ChangeTeam(NewTeam: Byte; JoinType: TJoinType); abstract;
			procedure Move(X, Y: Single); abstract;
			procedure SetVelocity(VelX, VelY: Single); abstract;
			procedure Tell(Text: string); abstract;
			procedure WriteConsole(Text: string; Color: Longint); abstract;
			property ID: Byte read FID;
			property Team: Byte read GetTeam write SetTeam;
			property Alive: Boolean read GetAlive;
			property Kills: Integer read GetKills write SetKills;
			property Deaths: Integer read GetDeaths;
			property Ping: Integer read GetPing;
			property Active: Boolean read GetActive;
			property IP: string read GetIP;
			property Port: Word read GetPort;
			property VelX: Single read GetVelX;
			property VelY: Single read GetVelY;
			property Muted: Boolean read GetMuted write SetMuted;
			property Jets: Integer read GetJets;
			property Grenades: Byte read GetGrenades;
			property X: Single read GetX;
			property Y: Single read GetY;
			property MouseAimX: SmallInt read GetMouseAimX write SetMouseAimX;
			property MouseAimY: SmallInt read GetMouseAimY write SetMouseAimY;
			property Flagger: Boolean read GetFlagger;
			property Time: Integer read GetTime;
			property OnGround: Boolean read GetOnGround;
			property IsProne: Boolean read GetProne;
			property Human: Boolean read GetHuman;
			property Direction: Shortint read GetDirection;
			property Flags: Byte read GetFlags;
			property HWID: string read GetHWID;
			property KeyUp: Boolean read GetKeyUp write SetKeyUp;
			property KeyLeft: Boolean read GetKeyLeft write SetKeyLeft;
			property KeyRight: Boolean read GetKeyRight write SetKeyRight;
			property KeyShoot: Boolean read GetKeyShoot write SetKeyShoot;
			property KeyJetpack: Boolean read GetKeyJetpack write SetKeyJetpack;
			property KeyGrenade: Boolean read GetKeyGrenade write SetKeyGrenade;
			property KeyChangeWeap: Boolean read GetKeyChangeWeap write SetKeyChangeWeap;
			property KeyThrow: Boolean read GetKeyThrow write SetKeyThrow;
			property KeyReload: Boolean read GetKeyReload write SetKeyReload;
			property KeyCrouch: Boolean read GetKeyCrouch write SetKeyCrouch;
			property KeyProne: Boolean read GetKeyProne write SetKeyProne;
			property KeyFlagThrow: Boolean read GetKeyFlagThrow write SetKeyFlagThrow;
			property WeaponActive[ID: Byte]: Boolean write SetWeaponActive;
			property OnFlagGrab: TOnFlagGrab read FOnFlagGrab write FOnFlagGrab;
			property OnFlagReturn: TOnFlagReturn read FOnFlagReturn write FOnFlagReturn;
			property OnFlagScore: TOnFlagScore read FOnFlagScore write FOnFlagScore;
			property OnFlagDrop: TOnFlagDrop read FOnFlagDrop write FOnFlagDrop;
			property OnKitPickup: TOnKitPickup read FOnKitPickup write FOnKitPickup;
			property OnBeforeRespawn: TOnBeforeRespawn
				read FOnBeforeRespawn write FOnBeforeRespawn;
			property OnAfterRespawn: TOnAfterRespawn read FOnAfterRespawn write FOnAfterRespawn;
			property OnDamage: TOnDamage read FOnDamage write FOnDamage;
			property OnKill: TOnKill read FOnKill write FOnKill;
			property OnWeaponChange: TOnWeaponChange read FOnWeaponChange write FOnWeaponChange;
			property OnVoteMapStart: TOnVoteMapStart read FOnVoteMapStart write FOnVoteMapStart;
			property OnVoteKickStart: TOnVoteKickStart
				read FOnVoteKickStart write FOnVoteKickStart;
			property OnVoteMap: TOnVoteMap read FOnVoteMap write FOnVoteMap;
			property OnVoteKick: TOnVoteKick read FOnVoteKick write FOnVoteKick;
			property OnSpeak: TOnSpeak read FOnSpeak write FOnSpeak;
			property OnCommand: TOnCommand read FOnCommand write FOnCommand;
		end;

	// ------------------------------ Players ------------------------------------
	TPlayers = class(TObject)
		private
			//FPlayers: array [1..32] of TActivePlayer;
			function GetPlayer(ID: Byte): TActivePlayer; abstract;
		public
			constructor Create; abstract;
			destructor Destroy; override; abstract;
			function Add(Player: TNewPlayer; jointype: TJoinType): TActivePlayer; abstract;
			procedure WriteConsole(Text: string; Color: Longint); abstract;
			procedure BigText(Layer: Byte; Text: string; Delay: Integer;
				Color: Longint; Scale: Single; X, Y: Integer); abstract;
			procedure WorldText(Layer: Byte; Text: string; Delay: Integer;
				Color: Longint; Scale, X, Y: Single); abstract;
			function GetByName(Name: string): TActivePlayer; abstract;
			function GetByIP(IP: string): TActivePlayer; abstract;
			procedure Tell(Text: string); abstract;

			property Player[ID: Byte]: TActivePlayer read GetPlayer; default;
		end;

	// -------------------------------- Script -----------------------------------
	TScript = class(TObject)
	private
		FOnException: TOnException;
		function GetName: string; abstract;
		function GetVersion: string; abstract;
		function GetDir: string; abstract;
		function GetDebugMode: Boolean; abstract;
	public
		//constructor Create(Script: TScript);
		procedure Recompile(Force: Boolean); abstract;
		procedure Unload; abstract;
		property Name: string read GetName;
		property Version: string read GetVersion;
		property Dir: string read GetDir;
		property DebugMode: Boolean read GetDebugMode;
		property OnException: TOnException read FOnException write FOnException;
	end;

	// ---------------------------- SpawnPoint -----------------------------------
	TSpawnPoint = class(TObject)
	protected
		function GetActive: Boolean; abstract;
		procedure SetActive(Active: Boolean); abstract;
		function GetX: Longint; abstract;
		procedure SetX(X: Longint); abstract;
		function GetY: Longint; abstract;
		procedure SetY(Y: Longint); abstract;
		function GetStyle: Byte; abstract;
		procedure SetStyle(Style: Byte); abstract;
	public
		property Active: Boolean read GetActive write SetActive;
		property X: Longint read GetX write SetX;
		property Y: Longint read GetY write SetY;
		property Style: Byte read GetStyle write SetStyle;
	end;

	TNewSpawnPoint = class(TSpawnPoint)
	public
		constructor Create; abstract;
		destructor Destroy; override; abstract;
	end;

	TActiveSpawnPoint = class(TSpawnPoint)
	private
		FID: Byte;
	public
		//constructor CreateActive(ID: Byte; var Spawn: TMapSpawnPoint);
		property ID: Byte read FID;
	end;

	// ------------------------------- Team --------------------------------------
	TTeam = class(TObject)
	private
		//FPlayers: TList;
		FID: Byte;
		FOnJoin: TOnJoinTeam;
		FOnLeave: TOnLeaveTeam;
		function GetScore: Byte; abstract;
		procedure SetScore(Score: Byte); abstract;
		function GetPlayer(Num: Byte): TPlayer; abstract;
		function GetCount: Byte; abstract;
	public
		constructor Create(ID: Byte); abstract;
		destructor Destroy; override; abstract;
		procedure AddPlayer(Player: TPlayer); abstract;
		procedure RemovePlayer(Player: TPlayer); abstract;

		procedure Add(Player: TPlayer); abstract;
		property Score: Byte read GetScore write SetScore;
		property Player[i: Byte]: TPlayer read GetPlayer; default;
		property Count: Byte read GetCount;
		property ID: Byte read FID;
		property OnJoin: TOnJoinTeam read FOnJoin write FOnJoin;
		property OnLeave: TOnLeaveTeam read FOnLeave write FOnLeave;
	end;

	// -------------------------------- Weapon -----------------------------------
	TWeapon = class(TObject)
	protected
		//FWeapon: PGun;
		//function GetGun: TGun;
		function GetType: Byte; abstract;
		function GetName: string; abstract;
		function GetBulletStyle: Byte; abstract;
		function GetAmmo: Byte; abstract;
		procedure SetAmmo(Ammo: Byte); virtual; abstract;
	public
		//property Gun: TGun read GetGun;

		property WType: Byte read GetType;
		property Name: string read GetName;
		property BulletStyle: Byte read GetBulletStyle;
		property Ammo: Byte read GetAmmo write SetAmmo;
	end;

	TPlayerWeapon = class(TWeapon)
	protected
		procedure SetAmmo(Ammo: Byte); override; abstract;
	public
		//constructor Create(var Sprite: TSprite); override;
	end;

	TNewWeapon = class(TWeapon)
	private
		procedure SetType(WType: Byte); abstract;
	protected
		procedure SetAmmo(Ammo: Byte); override; abstract;
	public
		constructor Create; abstract;
		destructor Destroy; override; abstract;
		property WType: Byte read GetType write SetType;
	end;

const
	// These number don't have to be correct, it's just to satisfy the FPC.
	WTYPE_EAGLE = 0;
	WTYPE_MP5 = 1;
	WTYPE_AK74 = 2;
	WTYPE_STEYRAUG = 3;
	WTYPE_SPAS12 = 4;
	WTYPE_RUGER77 = 5;
	WTYPE_M79 = 6;
	WTYPE_BARRETT = 7;
	WTYPE_M249 = 8;
	WTYPE_MINIGUN = 9;
	WTYPE_USSOCOM = 10;
	WTYPE_KNIFE = 11;
	WTYPE_CHAINSAW = 12;
	WTYPE_LAW = 13;
	WTYPE_FLAMER = 14;
	WTYPE_BOW = 15;
	WTYPE_BOW2 = 16;
	WTYPE_M2 = 17;
	WTYPE_NOWEAPON = 18;
	WTYPE_FRAGGRENADE = 19;
	WTYPE_CLUSTERGRENADE = 20;
	WTYPE_CLUSTER = 21;
	WTYPE_THROWNKNIFE = 22;

var
	// SC3
	Map: TMap;
	//File: TFile;
	Game: TGame;
	Global: TGlobal;
	Math: TMath;
	Players: TPlayers;
	Script: TScript;

	// SC2
{$ifdef SC2}
	CoreVersion: string;
	ScriptName: string;
	SafeMode: Byte;
	MaxPlayers: Byte;
	NumPlayers: Byte;
	NumBots: Byte;
	CurrentMap: string;
	NextMap: string;
	TimeLimit: Integer;
	TimeLeft: Integer;
	ScoreLimit: Integer;
	GameStyle: Byte;
	Version: string;
	ServerVersion: string;
	ServerName: string;
	ServerIP: string;
	ServerPort: Integer;
	DeathmatchPlayers: Byte;
	Spectators: Byte;
	AlphaPlayers: Byte;
	BravoPlayers: Byte;
	CharliePlayers: Byte;
	DeltaPlayers: Byte;
	AlphaScore: Byte;
	BravoScore: Byte;
	CharlieScore: Byte;
	DeltaScore: Byte;
	ReqPort: Word;
	Paused: Boolean;
	Password: string;
	DisableScript: Boolean;
	AppOnIdleTimer: LongWord;
	ExceptionType, ExceptionParam: integer;
{$endif}

///////////////////////////////  Functions /////////////////////////////////////
// SC3 functions as according to the wiki

// Writes a message to the server console.
procedure WriteLn(const Text: string); external;
function StrToInt(Text: String): Integer; external;
function iif(Condition: Boolean; IfTrue, IfFalse: Variant): Variant; external;
function Random(Min, Max: Integer): Integer; external;
function RGB(R, G, B: Byte): LongWord; external;
function ExecRegExpr(const ARegExpr, AInputStr: string): boolean; external;
procedure SplitRegExpr(const ARegExpr, AInputStr: string; APieces: TStrings); external;
function ReplaceRegExpr(const ARegExpr, AInputStr, AReplaceStr: string; AUseSubstitution: boolean): string; external;
function QuoteRegExprMetaChars(const AStr: string): string; external;
function RegExprSubExpressions(const ARegExpr: string; ASubExprs : TStrings; AExtendedSyntax: boolean): integer; external;
function EncodeDate(Year, Month, Day: Word): TDateTime; external;
function EncodeTime(Hour, Min, Sec, MSec: Word): TDateTime; external;
function TryEncodeDate(Year, Month, Day: Word; var Date: TDateTime): Boolean; external;
function TryEncodeTime(Hour, Min, Sec, MSec: Word; var Time: TDateTime): Boolean; external;
procedure DecodeDate(const DateTime: TDateTime; var Year, Month, Day: Word); external;
procedure DecodeTime(const DateTime: TDateTime; var Hour, Min, Sec, MSec: Word); external;
function DayOfWeek(const DateTime: TDateTime): Word; external;
function Date: TDateTime; external;
function Time: TDateTime; external;
function Now: TDateTime; external;
function DateTimeToUnix(D: TDateTime): Int64; external;
function UnixToDateTime(U: Int64): TDateTime; external;
function DateToStr(D: TDateTime): String; external;
// function IntToStr(x: integer): string; external;
function StrToDate(const s: String): TDateTime; external;
function FormatDateTime(const fmt: String; D: TDateTime): String; external;
procedure UnloadDll(s: string); external;
function DLLGetLastError: Longint; external;
function Distance(X1, Y1, X2, Y2: Single): Single; external;
function menu2obj(Style: Byte): Byte; external;
function menu2weap(Style: Byte): Byte; external;
function obj2menu(Style: Byte): Byte; external;
function obj2weap(Style: Byte): Byte; external;
function weap2menu(Style: Byte): Byte; external;
function weap2obj(Style: Byte): Byte; external;

// SC2 functions?
{$ifdef SC2}
function FileExists(FileName: String): Boolean; external;
function GetURL(Url: String): String; external;
procedure DrawTextEx(
	Id, Num: Byte; Text: String; Delay: Integer;
	Colour: Longint; Scale: Single; X, Y: Integer;
); external;

procedure DrawText(
	Id: Byte; Text: String; Delay: Integer; Colour: Longint;
	Scale: Single; X, Y: Integer;
); external;

function ReadFile(FileName: String): String; external;
function RayCast(
	P1X, P1Y, P2X, P2Y: Single; var Distance: Single;
	MaxDist: Single;
): Boolean; external;

function RayCastEx(
	P1X, P1Y, P2X, P2Y: Single; var Distance: Single;
	MaxDist: Single; Player, Flag, Bullet, Collider: Boolean; Team: Byte;
): Boolean; external;

function ArrayHigh(Arr: array of String): Integer; external;
function WeaponNameByNum(Num: Integer): string; external;
procedure ForceWeapon(A, B, C, D: Byte); external;
procedure ForceWeaponEx(A, B, C, D, E: Byte); external;
procedure SetScore(Id: Byte; Score: Integer); external;
procedure SetTeamScore(Team: Byte; Score: Integer); external;
procedure KickPlayer(Num: Byte); external;
function GetTickCount: Cardinal; external;
function arctan(Num: Extended): Extended; external;
function LogN(base, x: double): double; external;
function CreateBullet(X, Y, VelX, VelY, HitM: Single; sStyle, Owner: Byte): Integer; external;
function CreateObject(X, Y: Single; BType: Byte): Integer; external;
procedure KillObject(Id: Integer); external;
procedure GiveBonus(Id, Bonus: Byte); external;
function RegExpMatch(RegularExpression, Source: String): Boolean; external;
function RegExpReplace(RegularExpression, Source, ReplaceText: String): String; external;
function FormatDate(Format: String): String; external;
function IDToIP(Id: Byte): String; external;
function IDToHW(Id: Byte): String; external;
procedure BanPlayer(Num: Byte; Time: Integer); external;
procedure BanPlayerReason(Num: Byte; Time: Integer; Reason: String); external;
function Command(Cmd: String): Variant; external;
procedure SendStringToPlayer(Id: Byte; Text: String); external;
procedure BotChat(Id: Byte; Text: String); external;
procedure Sleep(Milliseconds: Cardinal); external;
function GetSystem: String; external;
function GetPlayerStat(Id: Byte; Stat: String): Variant; external;
function GetKeyPress(Id: Byte; Key: String): Boolean; external;
function GetObjectStat(Id: Byte; Stat: String): Variant; external;
function GetSpawnStat(Id: Byte; Stat: String): Variant; external;
procedure SetSpawnStat(Id: Byte; Stat: String; Value: Variant); external;
procedure DoDamage(Id: Byte; Damage: Integer); external;
procedure DoDamageBy(Id, Shooter: Byte; Damage: Integer); external;
procedure GetPlayerXY(Id: Byte; var X, Y: Single); external;
procedure GetFlagsXY(var BlueFlagX, BlueFlagY, RedFlagX, RedFlagY: Single); external;
procedure GetFlagsSpawnXY(var BlueFlagX, BlueFlagY, RedFlagX, RedFlagY: Single); external;
procedure StartVoteKick(Target: Byte; Reason: String); external;
procedure StartVoteMap(Mapname, Reason: String); external;
function Sqrt(Number: Extended): Extended; external;
function Round(const Number: Variant): Int64; external;
function RoundTo(const AValue: Extended; ADigit: Integer): Extended; external;
function GetPiece(const Input, Splitter: String; Index: Integer): String; external;
function WriteFile(FileName: String; SData: String): Boolean; external;
//function Length(S: String): Integer; external;
procedure Inc(var X: Integer; N: Integer); external;
procedure Dec(var X: Integer; N: Integer); external;
function CrossFunc(const Params: array of Variant; ProcName: String): Variant; external;
procedure WriteConsole(ID: byte; Text: string; Color: Cardinal); external;
procedure SetArrayLength(arr: array of string; len: integer);  overload; external;
function GetArrayLength(arr: array of string): integer;  overload; external;
function StrToIntDef(s: string; default: integer): integer; external;
function ContainsString(haystack, needle: string): boolean; external;
procedure RaiseException(code: integer; msg: string); external;
function FloatToStr(x: Extended): string; external;
function StrPos(const A, S: string): Integer; external;
function StrReplace(const A, B, C: string): string; external;
function HTTPEncode(A: string): string; external;
function HTTPDecode(A: string): string; external;
function shell_exec(A: string): Integer; external;
procedure PlaySound(ID: byte; name: string; x, y: single); external;
function FormatFloat(format: string; float: double): string; external;
procedure ServerModifier(what: string; param: variant); external;
procedure MovePlayer(ID: byte; x, y: single); external;
function  ExceptionToString(ex_type, ex_param: integer): string; external;
procedure ShutDown(); external;
function GetStringIndex(arg1: string; arg2: array of string): Integer; external;
{$endif}
{$endif}

implementation
end.
