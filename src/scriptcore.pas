unit Scriptcore;

// This unit provides most of Soldat's Scriptcore2 and Scriptcore3 functions,
// constants, variables and classes in form of empty definitions.
// By "using" it in your script files you can develop your scripts under
// Pascal IDE like Lazaurs.
// Note: the unit must be included with preprocessor, so it stays visible for
// the IDE and the compiler and invisible for SoldatServer, for example:

// > {$ifdef FPC}
// > uses
// >     Scriptcore;
// > {$endif}

// Some definitions may be missing since we could forget about something.

// By tk and Falcon.

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

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
    TOnLeaveTeam = procedure(Player: TActivePlayer; Team: TTeam; Kicked: boolean);
    TOnClockTick = procedure(Ticks: integer);
    TOnJoin = procedure(Player: TActivePlayer; Team: TTeam);
    TOnLeave = procedure(Player: TActivePlayer; Kicked: boolean);

    // Game
    TOnRequest = function(Ip, Hw: string; Port: word; State: byte;
        Forwarded: boolean; Password: string): integer;
    TOnAdminCommand = function(Player: TActivePlayer; Command: string): boolean;
    TOnTCPMessage = procedure(Ip: string; Port: word; Message: string);
    TOnTCPCommand = function(Ip: string; Port: word; Command: string): boolean;
    TOnAdminConnect = procedure(Ip: string; Port: word);
    TOnAdminDisconnect = procedure(Ip: string; Port: word);
    TOnBeforeMapChange = procedure(Next: string);
    TOnAfterMapChange = procedure(Next: string);

    // ActivePlayer
    TOnFlagGrab = procedure(Player: TActivePlayer; TFlag: TActiveFlag;
        Team: byte; GrabbedInBase: boolean);
    TOnFlagReturn = procedure(Player: TActivePlayer; Flag: TActiveFlag; Team: byte);
    TOnFlagScore = procedure(Player: TActivePlayer; Flag: TActiveFlag; Team: byte);
    TOnFlagDrop = procedure(Player: TActivePlayer; Flag: TActiveFlag;
        Team: byte; Thrown: boolean);
    TOnKitPickup = procedure(Player: TActivePlayer; Kit: TActiveMapObject);
    TOnBeforeRespawn = function(Player: TActivePlayer): byte;
    TOnAfterRespawn = procedure(Player: TActivePlayer);
    TOnDamage = function(Shooter, Victim: TActivePlayer; Damage: integer;
        BulletID: byte): integer;
    TOnKill = procedure(Killer, Victim: TActivePlayer; WeaponType: byte);
    TOnWeaponChange = procedure(Player: TActivePlayer;
        Primary, Secondary: TPlayerWeapon);
    TOnVoteMapStart = function(Player: TActivePlayer; Map: string): boolean;
    TOnVoteKickStart = function(Player, Victim: TActivePlayer; Reason: string): boolean;
    TOnVoteMap = procedure(Player: TActivePlayer; Map: string);
    TOnVoteKick = procedure(Player, Victim: TActivePlayer);
    TOnSpeak = procedure(Player: TActivePlayer; Text: string);
    TOnCommand = function(Player: TActivePlayer; Command: string): boolean;

    // Script
    TErrorType =
        (ErNoError, erCannotImport, erInvalidType, ErInternalError,
        erInvalidHeader, erInvalidOpcode, erInvalidOpcodeParameter, erNoMainProc,
        erOutOfGlobalVarsRange, erOutOfProcRange, ErOutOfRange, erOutOfStackRange,
        ErTypeMismatch, erUnexpectedEof, erVersionError, ErDivideByZero, ErMathError,
        erCouldNotCallProc, erOutofRecordRange, erOutOfMemory, erException,
        erNullPointerException, erNullVariantError, erInterfaceNotSupported,
        erCustomError);

    TOnException = function(ErrorCode: TErrorType; Message, UnitName, FunctionName: string;
        Row, Col: cardinal): boolean;


    ////////////////////////////// SC3 classes ///////////////////////////////////
    // ------------------------------- Bullet ------------------------------------
    TActiveBullet = class(TObject)
    protected
        FID: byte;
        function GetActive: boolean;
        function GetID: byte;
        function GetStyle: byte;
        function GetX: single;
        function GetY: single;
        function GetVelX: single;
        function GetVelY: single;
        function GetOwner: byte;
    public
        //constructor CreateActive(ID: Byte; var Bul: TBullet);
        function GetOwnerWeaponId: integer;
        property Active: boolean read GetActive;
        property ID: byte read GetID;
        property Style: byte read GetStyle;
        property X: single read GetX;
        property Y: single read GetY;
        property VelX: single read GetVelX;
        property VelY: single read GetVelY;
        property Owner: byte read GetOwner;
    end;

    // ----------------------------- BanList -------------------------------------
    TBanHW = class(TObject)
        HW: string;
        Time: integer;
        Reason: string;
    end;

    TBanIP = class(TObject)
        IP: string;
        Time: integer;
        Reason: string;
    end;

    TBanLists = class(TObject)
    private
        function GetBannedHW(Num: integer): TBanHW;
        function GetBannedIP(Num: integer): TBanIP;
        function GetBannedHWCount: integer;
        function GetBannedIPCount: integer;
    public
        procedure AddHWBan(HW, Reason: string; Duration: integer);
        procedure AddIPBan(IP: string; Reason: string; Duration: integer);
        function DelHWBan(HW: string): boolean;
        function DelIPBan(IP: string): boolean;
        function IsBannedHW(HW: string): boolean;
        function IsBannedIP(IP: string): boolean;
        function GetHWBanId(HW: string): integer;
        function GetIPBanId(IP: string): integer;
        property HW[i: integer]: TBanHW read GetBannedHW;
        property IP[i: integer]: TBanIP read GetBannedIP;
        property BannedHWCount: integer read GetBannedHWCount;
        property BannedIPCount: integer read GetBannedIPCount;
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
        function CheckAccess(const FilePath: string): boolean;
        function CreateFileStream(): TStream;
        function CreateFileStreamFromFile(const Path: string): TStream;
        function CreateStringList(): TStringList;
        function CreateStringListFromFile(const Path: string): TStringList;
        function CreateINI(const Path: string): TIniFile;
        function Exists(const Path: string): boolean;
        function Copy(const Source, Destination: string): boolean;
        function Move(const Source, Destination: string): boolean;
        function Delete(const Path: string): boolean;
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
        FTickThreshold: longint;
        function GetGameStyle: byte;
        procedure SetGameStyle(Style: byte);
        function GetMaxPlayers: byte;
        procedure SetMaxPlayers(Max: byte);
        function GetNextMap: string;
        function GetCurrentMap: string;
        function GetNumBots: byte;
        function GetNumPlayers: byte;
        function GetSpectators: byte;
        function GetScoreLimit: word;
        procedure SetScoreLimit(Limit: word);
        function GetServerIP: string;
        function GetServerName: string;
        function GetServerPort: word;
        function GetServerVersion: string;
        function GetServerInfo: string;
        function GetGravity: single;
        procedure SetGravity(Grav: single);
        function GetPaused: boolean;
        procedure SetPaused(Paused: boolean);
        function GetRespawnTime: integer;
        procedure SetRespawnTime(Time: integer);
        function GetMinRespawnTime: integer;
        procedure SetMinRespawnTime(Time: integer);
        function GetMaxRespawnTime: integer;
        procedure SetMaxRespawnTime(Time: integer);
        function GetMaxGrenades: byte;
        procedure SetMaxGrenades(Num: byte);
        function GetBonus: byte;
        procedure SetBonus(Num: byte);
        function GetTimeLimit: longint;
        procedure SetTimeLimit(Num: longint);
        function GetTimeLeft: longint;
        function GetFriendlyFire: boolean;
        procedure SetFriendlyFire(Enabled: boolean);
        function GetPassword: string;
        procedure SetPassword(Pass: string);
        function GetAdminPassword: string;
        procedure SetAdminPassword(Pass: string);
        function GetVotePercent: byte;
        procedure SetVotePercent(Percent: byte);
        function GetRealistic: boolean;
        procedure SetRealistic(Enabled: boolean);
        function GetSurvival: boolean;
        procedure SetSurvival(Enabled: boolean);
        function GetAdvance: boolean;
        procedure SetAdvance(Enabled: boolean);
        function GetBalance: boolean;
        procedure SetBalance(Enabled: boolean);
        function GetTickCount: longint;
        function GetTeam(ID: byte): TTeam;
        function GetMapsList: TMapsList;
        function GetBanLists: TBanLists;
    public
        constructor Create;
        procedure Shutdown;
        procedure StartVoteKick(ID: byte; Reason: string);
        procedure StartVoteMap(Name: string);
        procedure Restart;
        function LoadWeap(WeaponMod: string): boolean;
        function LoadCon(ConfigFile: string): boolean;
        function LoadList(MapsList: string): boolean;
        function LobbyRegister: boolean;
        property GameStyle: byte read GetGameStyle write SetGameStyle;
        property MaxPlayers: byte read GetMaxPlayers write SetMaxPlayers;
        property NextMap: string read GetNextMap;
        property CurrentMap: string read GetCurrentMap;
        property NumBots: byte read GetNumBots;
        property NumPlayers: byte read GetNumPlayers;
        property Spectators: byte read GetSpectators;
        property ScoreLimit: word read GetScoreLimit write SetScoreLimit;
        property ServerIP: string read GetServerIP;
        property ServerName: string read GetServerName;
        property ServerPort: word read GetServerPort;
        property ServerVersion: string read GetServerVersion;
        property ServerInfo: string read GetServerInfo;
        property Gravity: single read GetGravity write SetGravity;
        property Paused: boolean read GetPaused write SetPaused;
        property RespawnTime: longint read GetRespawnTime write SetRespawnTime;
        property MinRespawnTime: longint read GetMinRespawnTime write SetMinRespawnTime;
        property MaxRespawnTime: longint read GetMaxRespawnTime write SetMaxRespawnTime;
        property MaxGrenades: byte read GetMaxGrenades write SetMaxGrenades;
        property Bonus: byte read GetBonus write SetBonus;
        property TimeLimit: longint read GetTimeLimit write SetTimeLimit;
        property TimeLeft: longint read GetTimeLeft;
        property FriendlyFire: boolean read GetFriendlyFire write SetFriendlyFire;
        property Password: string read GetPassword write SetPassword;
        property AdminPassword: string read GetAdminPassword write SetAdminPassword;
        property VotePercent: byte read GetVotePercent write SetVotePercent;
        property Realistic: boolean read GetRealistic write SetRealistic;
        property Survival: boolean read GetSurvival write SetSurvival;
        property Advance: boolean read GetAdvance write SetAdvance;
        property Balance: boolean read GetBalance write SetBalance;
        property TickThreshold: longint read FTickThreshold write FTickThreshold;
        property TickCount: longint read GetTickCount;
        property Teams[ID: byte]: TTeam read GetTeam;
        property ScriptMapsList: TMapsList read GetMapsList;
        property ScriptBanLists: TBanLists read GetBanLists;
        property OnClockTick: TOnClockTick read FOnClockTick write FOnClockTick;
        property OnJoin: TOnJoin read FOnJoin write FOnJoin;
        property OnLeave: TOnLeave read FOnLeave write FOnLeave;
        property OnRequest: TOnRequest read FOnRequest write FOnRequest;
        property OnAdminCommand: TOnAdminCommand
            read FOnAdminCommand write FOnAdminCommand;
        property OnTCPMessage: TOnTCPMessage read FOnTCPMessage write FOnTCPMessage;
        property OnTCPCommand: TOnTCPCommand read FOnTCPCommand write FOnTCPCommand;
        property OnAdminConnect: TOnAdminConnect
            read FOnAdminConnect write FOnAdminConnect;
        property OnAdminDisconnect: TOnAdminDisconnect
            read FOnAdminDisconnect write FOnAdminDisconnect;
    end;

    // -------------------------------- Global -----------------------------------
    TGlobal = class(TObject)
    private
        function GetDateSeparator: char;
        procedure SetDateSeparator(Separator: char);
        function GetShortDateFormat: string;
        procedure SetShortDateFormat(Format: string);
    public
        property ScriptDateSeparator: char read GetDateSeparator write SetDateSeparator;
        property ScriptShortDateFormat: string read GetShortDateFormat
            write SetShortDateFormat;
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
        function GetObject(ID: byte): TActiveMapObject;
        function GetBullet(ID: byte): TActiveBullet;
        function GetSpawn(ID: byte): TSpawnPoint;
        procedure SetSpawn(ID: byte; const Spawn: TSpawnPoint);
        function GetName: string;
    public
        constructor Create;
        function GetFlag(ID: integer): TActiveFlag;
        function RayCast(x1, y1, x2, y2: single; Player: boolean = False;
            Flag: boolean = False; Bullet: boolean = True;
            CheckCollider: boolean = False; Team: byte = 0): boolean;
        function RayCastVector(A, B: TVector; Player: boolean = False;
            Flag: boolean = False; Bullet: boolean = True;
            CheckCollider: boolean = False; Team: byte = 0): boolean;
        function CreateBulletVector(A, B: TVector; HitM: single;
            sStyle: byte; Owner: TActivePlayer): integer;
        function CreateBullet(X, Y, VelX, VelY, HitM: single; sStyle: byte;
            Owner: TActivePlayer): integer;
        function AddObject(Obj: TNewMapObject): TActiveMapObject;
        function AddSpawnPoint(Spawn: TSpawnPoint): TActiveSpawnPoint;
        procedure NextMap;
        procedure SetMap(NewMap: string);
        property Objects[ID: byte]: TActiveMapObject read GetObject;
        property Bullets[ID: byte]: TActiveBullet read GetBullet;
        property Spawns[ID: byte]: TSpawnPoint read GetSpawn write SetSpawn;
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
        function GetMap(Num: integer): string;
        function GetCurrentMapId: integer;
        procedure SetCurrentMapId(NewNum: integer);
        function GetMapsCount: integer;
    public
        procedure AddMap(Name: string);
        procedure RemoveMap(Name: string);
        function GetMapIdByName(Name: string): integer;
        property Map[i: integer]: string read GetMap; default;
        property CurrentMapId: integer read GetCurrentMapId write SetCurrentMapId;
        property MapsCount: integer read GetMapsCount;
    end;

    // ------------------------------- Math --------------------------------------
    TMath = class
    private
        function GetE: extended;
        function GetPi: extended;
    public
        function Sin(A: extended): extended;
        function Cos(A: extended): extended;
        function Tan(A: extended): extended;
        function Cotan(A: extended): extended;
        function Pow(A, B: extended): extended;
        function LogN(A, B: extended): extended;
        function Ln(A: extended): extended;
        function ArcSin(A: extended): extended;
        function ArcCos(A: extended): extended;
        function ArcTan(A: extended): extended;
        function ArcCotan(A: extended): extended;
        function ArcTan2(A, B: extended): extended;
        function Min(A, B: extended): extended;
        function Max(A, B: extended): extended;
        function Abs(A: extended): extended;
        function Exp(A: extended): extended;
        function Sign(A: extended): TValueSign;
        function IsNaN(A: extended): boolean;
        function Round(A: extended): integer;
        function RoundTo(A: extended; B: TRoundToRange): extended;
        function DegToRad(A: extended): extended;
        function RadToDeg(A: extended): extended;
        function DegNormalize(A: extended): extended;
        function InRange(const A, B, C: double): boolean;
        function EnsureRange(const AValue, AMin, AMax: double): double;
        function Random(Min, Max: extended): extended;
        property E: extended read GetE;
        property Pi: extended read GetPi;
    end;

    // ------------------------------ Object -------------------------------------
    TThing = class(TObject)
    protected
        //FObj: PThing;
        function GetStyle: byte; virtual; abstract;
        function GetX: single;
        function GetY: single;
    public
        property Style: byte read GetStyle;
        property X: single read GetX;
        property Y: single read GetY;
    end;

    TNewMapObject = class(TThing)
    protected
        function GetStyle: byte; override;
        procedure SetStyle(Style: byte);
        procedure SetX(X: single);
        procedure SetY(Y: single);
    public
        constructor Create;
        destructor Destroy; override;
        property Style: byte read GetStyle write SetStyle;
        property X: single read GetX write SetX;
        property Y: single read GetY write SetY;
    end;

    TActiveMapObject = class(TThing)
    protected
        FID: byte;
        function GetActive: boolean;
        function GetID: byte;
        function GetStyle: byte; override;
    public
        //constructor CreateActive(ID: Byte; var Obj: TThing);
        procedure Kill;
        property Active: boolean read GetActive;
        property ID: byte read GetID;
    end;

    TActiveFlag = class(TActiveMapObject)
    private
        function GetInBase: boolean;
    public
        property InBase: boolean read GetInBase;
    end;

    // ------------------------------ Player -------------------------------------
    TPlayer = class(TObject)
    protected
        FPrimary: TWeapon;
        FSecondary: TWeapon;
        //function GetSprite: TSprite;
        function GetTeam: byte;
        function GetName: string;
        function GetAlive: boolean;
        function GetHealth: single;
        procedure SetHealth(Health: single);
        function GetVest: single;
        procedure SetVest(Vest: single);
        function GetPrimary: TPlayerWeapon;
        function GetSecondary: TPlayerWeapon;
        function GetShirtColor: longword;
        function GetPantsColor: longword;
        function GetSkinColor: longword;
        function GetHairColor: longword;
        function GetFavouriteWeapon: string;
        procedure SetFavouriteWeapon(Weapon: string);
        function GetChosenSecondaryWeapon: byte;
        function GetFriend: string;
        procedure SetFriend(Friend: string);
        function GetAccuracy: byte;
        procedure SetAccuracy(Accuracy: byte);
        function GetShootDead: boolean;
        procedure SetShootDead(ShootDead: boolean);
        function GetGrenadeFrequency: byte;
        procedure SetGrenadeFrequency(Frequency: byte);
        function GetCamping: boolean;
        procedure SetCamping(Camping: boolean);
        function GetOnStartUse: byte;
        procedure SetOnStartUse(Thing: byte);
        function GetHairStyle: byte;
        function GetHeadgear: byte;
        function GetChain: byte;
        function GetChatFrequency: byte;
        procedure SetChatFrequency(Frequency: byte);
        function GetChatKill: string;
        procedure SetChatKill(Message: string);
        function GetChatDead: string;
        procedure SetChatDead(Message: string);
        function GetChatLowHealth: string;
        procedure SetChatLowHealth(Message: string);
        function GetChatSeeEnemy: string;
        procedure SetChatSeeEnemy(Message: string);
        function GetChatWinning: string;
        procedure SetChatWinning(Message: string);
        function GetAdmin: boolean;
        procedure SetAdmin(SetAsAdmin: boolean);
        function GetDummy: boolean;
        procedure SetDummy(Dummy: boolean);
    public
        destructor Destroy; override;
        //Not exported
        //property Sprite: TSprite read GetSprite;
        property Team: byte read GetTeam;
        property Name: string read GetName;
        property Alive: boolean read GetAlive;
        property Health: single read GetHealth write SetHealth;
        property Vest: single read GetVest write SetVest;
        property Primary: TPlayerWeapon read GetPrimary;
        property Secondary: TPlayerWeapon read GetSecondary;
        property ShirtColor: longword read GetShirtColor;
        property PantsColor: longword read GetPantsColor;
        property SkinColor: longword read GetSkinColor;
        property HairColor: longword read GetHairColor;
        property FavouriteWeapon: string read GetFavouriteWeapon write SetFavouriteWeapon;
        property ChosenSecondaryWeapon: byte read GetChosenSecondaryWeapon;
        property Friend: string read GetFriend write SetFriend;
        property Accuracy: byte read GetAccuracy write SetAccuracy;
        property ShootDead: boolean read GetShootDead write SetShootDead;
        property GrenadeFrequency: byte read GetGrenadeFrequency write SetGrenadeFrequency;
        property Camping: boolean read GetCamping write SetCamping;
        property OnStartUse: byte read GetOnStartUse write SetOnStartUse;
        property HairStyle: byte read GetHairStyle;
        property Headgear: byte read GetHeadgear;
        property Chain: byte read GetChain;
        property ChatFrequency: byte read GetChatFrequency write SetChatFrequency;
        property ChatKill: string read GetChatKill write SetChatKill;
        property ChatDead: string read GetChatDead write SetChatDead;
        property ChatLowHealth: string read GetChatLowHealth write SetChatLowHealth;
        property ChatSeeEnemy: string read GetChatSeeEnemy write SetChatSeeEnemy;
        property ChatWinning: string read GetChatWinning write SetChatWinning;
        property IsAdmin: boolean read GetAdmin write SetAdmin;
        property Dummy: boolean read GetDummy write SetDummy;
    end;

    TNewPlayer = class(TPlayer)
    protected
        procedure SetName(Name: string);
        procedure SetTeam(Team: byte);
        procedure SetHealth(Health: single);
        function GetPrimary: TWeapon;
        procedure SetPrimary(Primary: TWeapon);
        function GetSecondary: TWeapon;
        procedure SetSecondary(Secondary: TWeapon);
        procedure SetShirtColor(Color: longword);
        procedure SetPantsColor(Color: longword);
        procedure SetSkinColor(Color: longword);
        procedure SetHairColor(Color: longword);
        procedure SetHairStyle(Style: byte);
        procedure SetHeadgear(Headgear: byte);
        procedure SetChain(Chain: byte);
        function GetDummy: boolean;
        procedure SetDummy(Dummy: boolean);
    public
        constructor Create;
        property Name: string read GetName write SetName;
        property Team: byte read GetTeam write SetTeam;
        property Health: single read GetHealth write SetHealth;
        property Primary: TWeapon read GetPrimary write SetPrimary;
        property Secondary: TWeapon read GetSecondary write SetSecondary;
        property ShirtColor: longword read GetShirtColor write SetShirtColor;
        property PantsColor: longword read GetPantsColor write SetPantsColor;
        property SkinColor: longword read GetSkinColor write SetSkinColor;
        property HairColor: longword read GetHairColor write SetHairColor;
        property HairStyle: byte read GetHairStyle write SetHairStyle;
        property Headgear: byte read GetHeadgear write SetHeadgear;
        property Chain: byte read GetChain write SetChain;
        property Dummy: boolean read GetDummy write SetDummy;
    end;

    TActivePlayer = class(TPlayer)
    private
        FID: byte;
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
        function GetKills: integer;
        procedure SetKills(Kills: integer);
        function GetDeaths: integer;
        function GetPing: integer;
        procedure SetTeam(Team: byte);
        function GetActive: boolean;
        procedure SetAlive(Alive: boolean);
        function GetIP: string;
        function GetPort: word;
        function GetVelX: single;
        function GetVelY: single;
        function GetMuted: boolean;
        procedure SetMuted(Muted: boolean);
        function GetJets: integer;
        function GetGrenades: byte;
        function GetX: single;
        function GetY: single;
        function GetMouseAimX: smallint;
        procedure SetMouseAimX(AimX: smallint);
        function GetMouseAimY: smallint;
        procedure SetMouseAimY(AimY: smallint);
        function GetFlagger: boolean;
        function GetTime: integer;
        function GetOnGround: boolean;
        function GetProne: boolean;
        function GetHuman: boolean;
        function GetDirection: shortint;
        function GetFlags: byte;
        function GetHWID: string;
        function GetKeyUp: boolean;
        procedure SetKeyUp(Pressed: boolean);
        function GetKeyLeft: boolean;
        procedure SetKeyLeft(Pressed: boolean);
        function GetKeyRight: boolean;
        procedure SetKeyRight(Pressed: boolean);
        function GetKeyShoot: boolean;
        procedure SetKeyShoot(Pressed: boolean);
        function GetKeyJetpack: boolean;
        procedure SetKeyJetpack(Pressed: boolean);
        function GetKeyGrenade: boolean;
        procedure SetKeyGrenade(Pressed: boolean);
        function GetKeyChangeWeap: boolean;
        procedure SetKeyChangeWeap(Pressed: boolean);
        function GetKeyThrow: boolean;
        procedure SetKeyThrow(Pressed: boolean);
        function GetKeyReload: boolean;
        procedure SetKeyReload(Pressed: boolean);
        function GetKeyCrouch: boolean;
        procedure SetKeyCrouch(Pressed: boolean);
        function GetKeyProne: boolean;
        procedure SetKeyProne(Pressed: boolean);
        function GetKeyFlagThrow: boolean;
        procedure SetKeyFlagThrow(Pressed: boolean);
        procedure SetWeaponActive(ID: byte; Active: boolean);
    public
        //constructor Create(var Sprite: TSprite; ID: Byte); overload;
        function Ban(Time: integer; Reason: string): boolean;
        procedure Say(Text: string);
        procedure Damage(Shooter: byte; Damage: integer);
        procedure BigText(Layer: byte; Text: string; Delay: integer;
            Color: longint; Scale: single; X, Y: integer);
        procedure WorldText(Layer: byte; Text: string; Delay: integer;
            Color: longint; Scale, X, Y: single);
        procedure ForceWeapon(Primary, Secondary: TNewWeapon);
        procedure ForwardTo(TargetIP: string; TargetPort: word; Message: string);
        procedure GiveBonus(BType: byte);
        function Kick(reason: TKickReason): boolean;
        procedure ChangeTeam(NewTeam: byte; JoinType: TJoinType);
        procedure Move(X, Y: single);
        procedure SetVelocity(VelX, VelY: single);
        procedure Tell(Text: string);
        procedure WriteConsole(Text: string; Color: longint);
        property ID: byte read FID;
        property Team: byte read GetTeam write SetTeam;
        property Alive: boolean read GetAlive;
        property Kills: integer read GetKills write SetKills;
        property Deaths: integer read GetDeaths;
        property Ping: integer read GetPing;
        property Active: boolean read GetActive;
        property IP: string read GetIP;
        property Port: word read GetPort;
        property VelX: single read GetVelX;
        property VelY: single read GetVelY;
        property Muted: boolean read GetMuted write SetMuted;
        property Jets: integer read GetJets;
        property Grenades: byte read GetGrenades;
        property X: single read GetX;
        property Y: single read GetY;
        property MouseAimX: smallint read GetMouseAimX write SetMouseAimX;
        property MouseAimY: smallint read GetMouseAimY write SetMouseAimY;
        property Flagger: boolean read GetFlagger;
        property Time: integer read GetTime;
        property OnGround: boolean read GetOnGround;
        property IsProne: boolean read GetProne;
        property Human: boolean read GetHuman;
        property Direction: shortint read GetDirection;
        property Flags: byte read GetFlags;
        property HWID: string read GetHWID;
        property KeyUp: boolean read GetKeyUp write SetKeyUp;
        property KeyLeft: boolean read GetKeyLeft write SetKeyLeft;
        property KeyRight: boolean read GetKeyRight write SetKeyRight;
        property KeyShoot: boolean read GetKeyShoot write SetKeyShoot;
        property KeyJetpack: boolean read GetKeyJetpack write SetKeyJetpack;
        property KeyGrenade: boolean read GetKeyGrenade write SetKeyGrenade;
        property KeyChangeWeap: boolean read GetKeyChangeWeap write SetKeyChangeWeap;
        property KeyThrow: boolean read GetKeyThrow write SetKeyThrow;
        property KeyReload: boolean read GetKeyReload write SetKeyReload;
        property KeyCrouch: boolean read GetKeyCrouch write SetKeyCrouch;
        property KeyProne: boolean read GetKeyProne write SetKeyProne;
        property KeyFlagThrow: boolean read GetKeyFlagThrow write SetKeyFlagThrow;
        property WeaponActive[ID: byte]: boolean write SetWeaponActive;
        property OnFlagGrab: TOnFlagGrab read FOnFlagGrab write FOnFlagGrab;
        property OnFlagReturn: TOnFlagReturn read FOnFlagReturn write FOnFlagReturn;
        property OnFlagScore: TOnFlagScore read FOnFlagScore write FOnFlagScore;
        property OnFlagDrop: TOnFlagDrop read FOnFlagDrop write FOnFlagDrop;
        property OnKitPickup: TOnKitPickup read FOnKitPickup write FOnKitPickup;
        property OnBeforeRespawn: TOnBeforeRespawn
            read FOnBeforeRespawn write FOnBeforeRespawn;
        property OnAfterRespawn: TOnAfterRespawn
            read FOnAfterRespawn write FOnAfterRespawn;
        property OnDamage: TOnDamage read FOnDamage write FOnDamage;
        property OnKill: TOnKill read FOnKill write FOnKill;
        property OnWeaponChange: TOnWeaponChange
            read FOnWeaponChange write FOnWeaponChange;
        property OnVoteMapStart: TOnVoteMapStart
            read FOnVoteMapStart write FOnVoteMapStart;
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
        function GetPlayer(ID: byte): TActivePlayer;
    public
        constructor Create;
        destructor Destroy; override;
        function Add(Player: TNewPlayer; jointype: TJoinType): TActivePlayer;
        procedure WriteConsole(Text: string; Color: longint);
        procedure BigText(Layer: byte; Text: string; Delay: integer;
            Color: longint; Scale: single; X, Y: integer);
        procedure WorldText(Layer: byte; Text: string; Delay: integer;
            Color: longint; Scale, X, Y: single);
        function GetByName(Name: string): TActivePlayer;
        function GetByIP(IP: string): TActivePlayer;
        procedure Tell(Text: string);

        property Player[ID: byte]: TActivePlayer read GetPlayer; default;
    end;

    // -------------------------------- Script -----------------------------------
    TScript = class(TObject)
    private
        FOnException: TOnException;
        function GetName: string;
        function GetVersion: string;
        function GetDir: string;
        function GetDebugMode: boolean;
    public
        //constructor Create(Script: TScript);
        procedure Recompile(Force: boolean);
        procedure Unload;
        property Name: string read GetName;
        property Version: string read GetVersion;
        property Dir: string read GetDir;
        property DebugMode: boolean read GetDebugMode;
        property OnException: TOnException read FOnException write FOnException;
    end;

    // ---------------------------- SpawnPoint -----------------------------------
    TSpawnPoint = class(TObject)
    protected
        function GetActive: boolean;
        procedure SetActive(Active: boolean);
        function GetX: longint;
        procedure SetX(X: longint);
        function GetY: longint;
        procedure SetY(Y: longint);
        function GetStyle: byte;
        procedure SetStyle(Style: byte);
    public
        property Active: boolean read GetActive write SetActive;
        property X: longint read GetX write SetX;
        property Y: longint read GetY write SetY;
        property Style: byte read GetStyle write SetStyle;
    end;

    TNewSpawnPoint = class(TSpawnPoint)
    public
        constructor Create;
        destructor Destroy; override;
    end;

    TActiveSpawnPoint = class(TSpawnPoint)
    private
        FID: byte;
    public
        //constructor CreateActive(ID: Byte; var Spawn: TMapSpawnPoint);
        property ID: byte read FID;
    end;

    // ------------------------------- Team --------------------------------------
    TTeam = class(TObject)
    private
        //FPlayers: TList;
        FID: byte;
        FOnJoin: TOnJoinTeam;
        FOnLeave: TOnLeaveTeam;
        function GetScore: byte;
        procedure SetScore(Score: byte);
        function GetPlayer(Num: byte): TPlayer;
        function GetCount: byte;
    public
        constructor Create(ID: byte);
        destructor Destroy; override;
        procedure AddPlayer(Player: TPlayer);
        procedure RemovePlayer(Player: TPlayer);

        procedure Add(Player: TPlayer);
        property Score: byte read GetScore write SetScore;
        property Player[i: byte]: TPlayer read GetPlayer; default;
        property Count: byte read GetCount;
        property ID: byte read FID;
        property OnJoin: TOnJoinTeam read FOnJoin write FOnJoin;
        property OnLeave: TOnLeaveTeam read FOnLeave write FOnLeave;
    end;

    // -------------------------------- Weapon -----------------------------------
    TWeapon = class(TObject)
    protected
        //FWeapon: PGun;
        //function GetGun: TGun;
        function GetType: byte;
        function GetName: string;
        function GetBulletStyle: byte;
        function GetAmmo: byte;
        procedure SetAmmo(Ammo: byte); virtual; abstract;
    public
        //property Gun: TGun read GetGun;

        property WType: byte read GetType;
        property Name: string read GetName;
        property BulletStyle: byte read GetBulletStyle;
        property Ammo: byte read GetAmmo write SetAmmo;
    end;

    TPlayerWeapon = class(TWeapon)
    protected
        procedure SetAmmo(Ammo: byte); override;
    public
        //constructor Create(var Sprite: TSprite); override;
    end;

    TNewWeapon = class(TWeapon)
    private
        procedure SetType(WType: byte);
    protected
        procedure SetAmmo(Ammo: byte); override;
    public
        constructor Create;
        destructor Destroy; override;
        property WType: byte read GetType write SetType;
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
    CoreVersion: string;
    ScriptName: string;
    SafeMode: byte;
    MaxPlayers: byte;
    NumPlayers: byte;
    NumBots: byte;
    CurrentMap: string;
    NextMap: string;
    TimeLimit: integer;
    TimeLeft: integer;
    ScoreLimit: integer;
    GameStyle: byte;
    Version: string;
    ServerVersion: string;
    ServerName: string;
    ServerIP: string;
    ServerPort: integer;
    DeathmatchPlayers: byte;
    Spectators: byte;
    AlphaPlayers: byte;
    BravoPlayers: byte;
    CharliePlayers: byte;
    DeltaPlayers: byte;
    AlphaScore: byte;
    BravoScore: byte;
    CharlieScore: byte;
    DeltaScore: byte;
    ReqPort: word;
    Paused: boolean;
    Password: string;
    DisableScript: boolean;
    AppOnIdleTimer: longword;
    ExceptionType, ExceptionParam: integer;

///////////////////////////////  Functions /////////////////////////////////////

function weap2obj(Style: byte): byte;

function menu2obj(Style: byte): byte;

function obj2weap(Style: byte): byte;

function obj2menu(Style: byte): byte;

function weap2menu(Style: byte): byte;

function menu2weap(Style: byte): byte;

function FileExists(FileName: string): boolean;

function GetURL(Url: string): string;

procedure DrawTextEx(Id, Num: byte; Text: string; Delay: integer;
    Colour: longint; Scale: single; X, Y: integer);

procedure DrawText(Id: byte; Text: string; Delay: integer; Colour: longint;
    Scale: single; X, Y: integer);

function ReadFile(FileName: string): string;

function RGB(R, G, B: byte): cardinal;

function RayCast(P1X, P1Y, P2X, P2Y: single; var Distance: single;
    MaxDist: single): boolean;

function RayCastEx(P1X, P1Y, P2X, P2Y: single; var Distance: single;
    MaxDist: single; Player, Flag, Bullet, Collider: boolean; Team: byte): boolean;

function ArrayHigh(Arr: array of string): integer;

function WeaponNameByNum(Num: integer): string;

procedure ForceWeapon(A, B, C, D: byte);

procedure ForceWeaponEx(A, B, C, D, E: byte);

procedure SetScore(Id: byte; Score: integer);

procedure SetTeamScore(Team: byte; Score: integer);

procedure KickPlayer(Num: byte);

function GetTickCount: cardinal;

function arctan(Num: extended): extended;

function LogN(base, x: double): double;

function CreateBullet(X, Y, VelX, VelY, HitM: single; sStyle, Owner: byte): integer;

function CreateObject(X, Y: single; BType: byte): integer;

procedure KillObject(Id: integer);

procedure GiveBonus(Id, Bonus: byte);

function Random(Min, Max: integer): integer;

function RegExpMatch(RegularExpression, Source: string): boolean;

function RegExpReplace(RegularExpression, Source, ReplaceText: string): string;

function FormatDate(Format: string): string;

function IDToIP(Id: byte): string;

function IDToHW(Id: byte): string;

procedure BanPlayer(Num: byte; Time: integer);

procedure BanPlayerReason(Num: byte; Time: integer; Reason: string);

function Command(Cmd: string): variant;

procedure SendStringToPlayer(Id: byte; Text: string);

procedure BotChat(Id: byte; Text: string);

procedure Sleep(Milliseconds: cardinal);

function GetSystem: string;

function GetPlayerStat(Id: byte; Stat: string): variant;

function GetKeyPress(Id: byte; Key: string): boolean;

function GetObjectStat(Id: byte; Stat: string): variant;

function GetSpawnStat(Id: byte; Stat: string): variant;

procedure SetSpawnStat(Id: byte; Stat: string; Value: variant);

procedure DoDamage(Id: byte; Damage: integer);

procedure DoDamageBy(Id, Shooter: byte; Damage: integer);

procedure GetPlayerXY(Id: byte; var X, Y: single);

procedure GetFlagsXY(var BlueFlagX, BlueFlagY, RedFlagX, RedFlagY: single);

procedure GetFlagsSpawnXY(var BlueFlagX, BlueFlagY, RedFlagX, RedFlagY: single);

procedure StartVoteKick(Target: byte; Reason: string);

procedure StartVoteMap(Mapname, Reason: string);

function Sqrt(Number: extended): extended;

function Round(const Number: variant): int64;

function RoundTo(const AValue: extended; ADigit: integer): extended;

function GetPiece(const Input, Splitter: string; Index: integer): string;

function WriteFile(FileName: string; SData: string): boolean;

procedure WriteLn(const Data: string);

//function Length(S: String): Integer;

procedure Inc(var X: integer; N: integer);

procedure Dec(var X: integer; N: integer);

function CrossFunc(const Params: array of variant; ProcName: string): variant;

procedure WriteConsole(ID: byte; Text: string; Color: cardinal);

procedure SetArrayLength(arr: array of string; len: integer); overload;

function GetArrayLength(arr: array of string): integer; overload;

function StrToInt(s: string): integer;

function StrToIntDef(s: string; default: integer): integer;

function ContainsString(haystack, needle: string): boolean;

procedure RaiseException(code: integer; msg: string);

function IntToStr(x: integer): string;

function FloatToStr(x: extended): string;

function StrPos(const A, S: string): integer;

function StrReplace(const A, B, C: string): string;

function HTTPEncode(A: string): string;

function HTTPDecode(A: string): string;

function shell_exec(A: string): integer;

function Distance(x1, y1, x2, y2: single): single;

function iif(cond: boolean; t, f: variant): variant;

procedure PlaySound(ID: byte; Name: string; x, y: single);

function FormatFloat(format: string; float: double): string;

procedure ServerModifier(what: string; param: variant);

procedure MovePlayer(ID: byte; x, y: single);

function ExceptionToString(ex_type, ex_param: integer): string;

procedure ShutDown();

function GetStringIndex(arg1: string; arg2: array of string): integer;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

implementation

function weap2obj(Style: byte): byte;
begin
    Result := 0;
end;

function menu2obj(Style: byte): byte;
begin
    Result := 0;
end;

function obj2weap(Style: byte): byte;
begin
    Result := 0;
end;

function obj2menu(Style: byte): byte;
begin
    Result := 0;
end;

function weap2menu(Style: byte): byte;
begin
    Result := 0;
end;

function menu2weap(Style: byte): byte;
begin
    Result := 0;
end;

function FileExists(FileName: string): boolean;
begin
    Result := False;
end;

function GetURL(Url: string): string;
begin
    Result := '';
end;

procedure DrawTextEx(Id, Num: byte; Text: string; Delay: integer;
    Colour: longint; Scale: single; X, Y: integer);
begin
end;

procedure DrawText(Id: byte; Text: string; Delay: integer; Colour: longint;
    Scale: single; X, Y: integer);
begin
end;

function ReadFile(FileName: string): string;
begin
    Result := '';
end;

function RGB(R, G, B: byte): cardinal;
begin
    Result := 0;
end;

function RayCast(P1X, P1Y, P2X, P2Y: single; var Distance: single;
    MaxDist: single): boolean;
begin
    Result := False;
end;

function RayCastEx(P1X, P1Y, P2X, P2Y: single; var Distance: single;
    MaxDist: single; Player, Flag, Bullet, Collider: boolean; Team: byte): boolean;
begin
    Result := False;
end;

function ArrayHigh(Arr: array of string): integer;
begin
    Result := 0;
end;

function WeaponNameByNum(Num: integer): string;
begin
    Result := '';
end;

procedure ForceWeapon(A, B, C, D: byte);
begin
end;

procedure ForceWeaponEx(A, B, C, D, E: byte);
begin
end;

procedure SetScore(Id: byte; Score: integer);
begin
end;

procedure SetTeamScore(Team: byte; Score: integer);
begin
end;

procedure KickPlayer(Num: byte);
begin
end;

function GetTickCount: cardinal;
begin
    Result := 0;
end;

function arctan(Num: extended): extended;
begin
    Result := 0;
end;

function LogN(base, x: double): double;
begin
    Result := 0;
end;

function CreateBullet(X, Y, VelX, VelY, HitM: single; sStyle, Owner: byte): integer;
begin
    Result := 0;
end;

function CreateObject(X, Y: single; BType: byte): integer;
begin
    Result := 0;
end;

procedure KillObject(Id: integer);
begin
end;

procedure GiveBonus(Id, Bonus: byte);
begin
end;

function Random(Min, Max: integer): integer;
begin
    Result := 0;
end;

function RegExpMatch(RegularExpression, Source: string): boolean;
begin
    Result := False;
end;

function RegExpReplace(RegularExpression, Source, ReplaceText: string): string;
begin
    Result := '';
end;

function FormatDate(Format: string): string;
begin
    Result := '';
end;

function IDToIP(Id: byte): string;
begin
    Result := '';
end;

function IDToHW(Id: byte): string;
begin
    Result := '';
end;

procedure BanPlayer(Num: byte; Time: integer);
begin
end;

procedure BanPlayerReason(Num: byte; Time: integer; Reason: string);
begin
end;

function Command(Cmd: string): variant;
begin
    Result := nil;
end;

procedure SendStringToPlayer(Id: byte; Text: string);
begin
end;

procedure BotChat(Id: byte; Text: string);
begin
end;

procedure Sleep(Milliseconds: cardinal);
begin
end;

function GetSystem: string;
begin
    Result := '';
end;

function GetPlayerStat(Id: byte; Stat: string): variant;
begin
    Result := nil;
end;

function GetKeyPress(Id: byte; Key: string): boolean;
begin
    Result := False;
end;

function GetObjectStat(Id: byte; Stat: string): variant;
begin
    Result := nil;
end;

function GetSpawnStat(Id: byte; Stat: string): variant;
begin
    Result := nil;
end;

procedure SetSpawnStat(Id: byte; Stat: string; Value: variant);
begin
end;

procedure DoDamage(Id: byte; Damage: integer);
begin
end;

procedure DoDamageBy(Id, Shooter: byte; Damage: integer);
begin
end;

procedure GetPlayerXY(Id: byte; var X, Y: single);
begin
    X := 0;
    Y := 0;
end;

procedure GetFlagsXY(var BlueFlagX, BlueFlagY, RedFlagX, RedFlagY: single);
begin
end;

procedure GetFlagsSpawnXY(var BlueFlagX, BlueFlagY, RedFlagX, RedFlagY: single);
begin
end;

procedure StartVoteKick(Target: byte; Reason: string);
begin
end;

procedure StartVoteMap(Mapname, Reason: string);
begin
end;

function Sqrt(Number: extended): extended;
begin
    Result := 0;
end;

function Round(const Number: variant): int64;
begin
    Result := 0;
end;

function RoundTo(const AValue: extended; ADigit: integer): extended;
begin
    Result := 0;
end;

function GetPiece(const Input, Splitter: string; Index: integer): string;
begin
    Result := '';
end;

function WriteFile(FileName: string; SData: string): boolean;
begin
    Result := False;
end;

procedure WriteLn(const Data: string);
begin
end;

//function Length(S: String): Integer;
//begin
//  Result := 0;
//end;

procedure Inc(var X: integer; N: integer);
begin
end;

procedure Dec(var X: integer; N: integer);
begin
end;

function CrossFunc(const Params: array of variant; ProcName: string): variant;
begin
    Result := nil;
end;

procedure WriteConsole(ID: byte; Text: string; Color: cardinal);
begin
end;

procedure SetArrayLength(arr: array of string; len: integer); overload;
begin
end;

function GetArrayLength(arr: array of string): integer; overload;
begin
    Result := 0;
end;

function StrToInt(s: string): integer;
begin
    Result := 0;
end;

function StrToIntDef(s: string; default: integer): integer;
begin
    Result := 0;
end;

function ContainsString(haystack, needle: string): boolean;
begin
    Result := False;
end;

procedure RaiseException(code: integer; msg: string);
begin
end;

function IntToStr(x: integer): string;
begin
    Result := '';
end;

function FloatToStr(x: extended): string;
begin
    Result := '';
end;

function StrPos(const A, S: string): integer;
begin
    Result := 0;
end;

function StrReplace(const A, B, C: string): string;
begin
    Result := '';
end;

function HTTPEncode(A: string): string;
begin
    Result := '';
end;

function HTTPDecode(A: string): string;
begin
    Result := '';
end;

function shell_exec(A: string): integer;
begin
    Result := 0;
end;

function Distance(x1, y1, x2, y2: single): single;
begin
    Result := 0;
end;

function iif(cond: boolean; t, f: variant): variant;
begin
    Result := f;
end;

procedure PlaySound(ID: byte; Name: string; x, y: single);
begin
end;

procedure ServerModifier(what: string; param: variant);
begin
end;

function FormatFloat(format: string; float: double): string;
begin
    Result := '';
end;

procedure MovePlayer(ID: byte; x, y: single);
begin
end;

function ExceptionToString(ex_type, ex_param: integer): string;
begin
    Result := '';
end;

procedure ShutDown();
begin

end;

function GetStringIndex(arg1: string; arg2: array of string): integer;
begin
    Result := 0;
end;

////////////////////////////////// METHODS /////////////////////////////////////

procedure TBanLists.AddHWBan(HW, Reason: string; Duration: integer);
begin
end;

procedure TBanLists.AddIPBan(IP: string; Reason: string; Duration: integer);
begin
end;

function TBanLists.DelHWBan(HW: string): boolean;
begin
    Result := False;
end;

function TBanLists.DelIPBan(IP: string): boolean;
begin
    Result := False;
end;

function TBanLists.IsBannedHW(HW: string): boolean;
begin
    Result := False;
end;

function TBanLists.IsBannedIP(IP: string): boolean;
begin
    Result := False;
end;

function TBanLists.GetHWBanId(HW: string): integer;
begin
    Result := 0;
end;

function TBanLists.GetIPBanId(IP: string): integer;
begin
    Result := 0;
end;

function TBanLists.GetBannedHW(Num: integer): TBanHW;
begin
    Result := nil;
end;

function TBanLists.GetBannedIP(Num: integer): TBanIP;
begin
    Result := nil;
end;

function TBanLists.GetBannedHWCount: integer;
begin
    Result := 0;
end;

function TBanLists.GetBannedIPCount: integer;
begin
    Result := 0;
end;

// -------------------------------- Bullet -------------------------------------
function TActiveBullet.GetOwnerWeaponId: integer;
begin
    Result := 0;
end;

function TActiveBullet.GetActive: boolean;
begin
    Result := False;
end;

function TActiveBullet.GetID: byte;
begin
    Result := 0;
end;

function TActiveBullet.GetStyle: byte;
begin
    Result := 0;
end;

function TActiveBullet.GetX: single;
begin
    Result := 0;
end;

function TActiveBullet.GetY: single;
begin
    Result := 0;
end;

function TActiveBullet.GetVelX: single;
begin
    Result := 0;
end;

function TActiveBullet.GetVelY: single;
begin
    Result := 0;
end;

function TActiveBullet.GetOwner: byte;
begin
    Result := 0;
end;

// -------------------------------- File API -----------------------------------
//constructor TFile.Create(API: TScriptFileAPI);
//begin
//  Self.FAPI := API;
//end;

function TFile.CheckAccess(const FilePath: string): boolean;
begin
    Result := False;
end;

function TFile.CreateFileStream(): TStream;
begin
    Result := nil;
end;

function TFile.CreateFileStreamFromFile(const Path: string): TStream;
begin
    Result := nil;
end;

function TFile.CreateStringList(): TStringList;
begin
    Result := nil;
end;

function TFile.CreateStringListFromFile(const Path: string): TStringList;
begin
    Result := nil;
end;

function TFile.CreateINI(const Path: string): TIniFile;
begin
    Result := nil;
end;

function TFile.Exists(const Path: string): boolean;
begin
    Result := False;
end;

function TFile.Copy(const Source, Destination: string): boolean;
begin
    Result := False;
end;

function TFile.Move(const Source, Destination: string): boolean;
begin
    Result := False;
end;

function TFile.Delete(const Path: string): boolean;
begin
    Result := False;
end;

// ---------------------------------- Game -------------------------------------
function TGame.GetGameStyle: byte;
begin
    Result := 0;
end;

procedure TGame.SetGameStyle(Style: byte);
begin
end;

function TGame.GetMaxPlayers: byte;
begin
    Result := 0;
end;

procedure TGame.SetMaxPlayers(Max: byte);
begin
end;

function TGame.GetNextMap: string;
begin
    Result := '';
end;

function TGame.GetCurrentMap: string;
begin
    Result := '';
end;

function TGame.GetNumBots: byte;
begin
    Result := 0;
end;

function TGame.GetNumPlayers: byte;
begin
    Result := 0;
end;

function TGame.GetSpectators: byte;
begin
    Result := 0;
end;

function TGame.GetScoreLimit: word;
begin
    Result := 0;
end;

procedure TGame.SetScoreLimit(Limit: word);
begin
end;

function TGame.GetServerIP: string;
begin
    Result := '';
end;

function TGame.GetServerName: string;
begin
    Result := '';
end;

function TGame.GetServerPort: word;
begin
    Result := 0;
end;

function TGame.GetServerVersion: string;
begin
    Result := '';
end;

function TGame.GetServerInfo: string;
begin
    Result := '';
end;

function TGame.GetGravity: single;
begin
    Result := 0;
end;

procedure TGame.SetGravity(Grav: single);
begin
end;

function TGame.GetPaused: boolean;
begin
    Result := False;
end;

procedure TGame.SetPaused(Paused: boolean);
begin
end;

function TGame.GetRespawnTime: integer;
begin
    Result := 0;
end;

procedure TGame.SetRespawnTime(Time: integer);
begin
end;

function TGame.GetMinRespawnTime: integer;
begin
    Result := 0;
end;

procedure TGame.SetMinRespawnTime(Time: integer);
begin
end;

function TGame.GetMaxRespawnTime: integer;
begin
    Result := 0;
end;

procedure TGame.SetMaxRespawnTime(Time: integer);
begin
end;

function TGame.GetMaxGrenades: byte;
begin
    Result := 0;
end;

procedure TGame.SetMaxGrenades(Num: byte);
begin
end;

function TGame.GetBonus: byte;
begin
    Result := 0;
end;

procedure TGame.SetBonus(Num: byte);
begin
end;

function TGame.GetTimeLimit: longint;
begin
    Result := 0;
end;

procedure TGame.SetTimeLimit(Num: longint);
begin
end;

function TGame.GetTimeLeft: longint;
begin
    Result := 0;
end;

function TGame.GetFriendlyFire: boolean;
begin
    Result := False;
end;

procedure TGame.SetFriendlyFire(Enabled: boolean);
begin
end;

function TGame.GetPassword: string;
begin
    Result := '';
end;

procedure TGame.SetPassword(Pass: string);
begin
end;

function TGame.GetAdminPassword: string;
begin
    Result := '';
end;

procedure TGame.SetAdminPassword(Pass: string);
begin
end;

function TGame.GetVotePercent: byte;
begin
    Result := 0;
end;

procedure TGame.SetVotePercent(Percent: byte);
begin
end;

function TGame.GetRealistic: boolean;
begin
    Result := False;
end;

procedure TGame.SetRealistic(Enabled: boolean);
begin
end;

function TGame.GetSurvival: boolean;
begin
    Result := False;
end;

procedure TGame.SetSurvival(Enabled: boolean);
begin
end;

function TGame.GetAdvance: boolean;
begin
    Result := False;
end;

procedure TGame.SetAdvance(Enabled: boolean);
begin
end;

function TGame.GetBalance: boolean;
begin
    Result := False;
end;

procedure TGame.SetBalance(Enabled: boolean);
begin
end;

function TGame.GetTickCount: longint;
begin
    Result := 0;
end;

function TGame.GetTeam(ID: byte): TTeam;
begin
    Result := nil;
end;

function TGame.GetMapsList: TMapsList;
begin
    Result := nil;
end;

function TGame.GetBanLists: TBanLists;
begin
    Result := nil;
end;

procedure TGame.Restart;
begin
end;

function TGame.LoadWeap(WeaponMod: string): boolean;
begin
    Result := False;
end;

function TGame.LoadCon(ConfigFile: string): boolean;
begin
    Result := False;
end;

function TGame.LoadList(MapsList: string): boolean;
begin
    Result := False;
end;

function TGame.LobbyRegister: boolean;
begin
    Result := False;
end;

constructor TGame.Create;
begin
end;

procedure TGame.Shutdown;
begin
end;

procedure TGame.StartVoteKick(ID: byte; Reason: string);
begin
end;

procedure TGame.StartVoteMap(Name: string);
begin
end;

// -------------------------------- Global -------------------------------------
function TGlobal.GetDateSeparator: char;
begin
    Result := #0;
end;

procedure TGlobal.SetDateSeparator(Separator: char);
begin
end;

function TGlobal.GetShortDateFormat: string;
begin
    Result := '';
end;

procedure TGlobal.SetShortDateFormat(Format: string);
begin
end;

// --------------------------------- Map ---------------------------------------
constructor TMap.Create;
begin
end;

function TMap.GetObject(ID: byte): TActiveMapObject;
begin
    Result := nil;
end;

function TMap.GetBullet(ID: byte): TActiveBullet;
begin
    Result := nil;
end;

function TMap.GetSpawn(ID: byte): TSpawnPoint;
begin
    Result := nil;
end;

procedure TMap.SetSpawn(ID: byte; const Spawn: TSpawnPoint);
begin
end;

function TMap.GetName: string;
begin
    Result := '';
end;

function TMap.GetFlag(ID: integer): TActiveFlag;
begin
    Result := nil;
end;

function TMap.RayCast(x1, y1, x2, y2: single; Player: boolean = False;
    Flag: boolean = False; Bullet: boolean = True; CheckCollider: boolean = False;
    Team: byte = 0): boolean;
begin
    Result := False;
end;

function TMap.RayCastVector(A, B: TVector; Player: boolean = False;
    Flag: boolean = False; Bullet: boolean = True; CheckCollider: boolean = False;
    Team: byte = 0): boolean;
begin
    Result := False;
end;

function TMap.CreateBulletVector(A, B: TVector; HitM: single; sStyle: byte;
    Owner: TActivePlayer): integer;
begin
    Result := 0;
end;

function TMap.CreateBullet(X, Y, VelX, VelY, HitM: single; sStyle: byte;
    Owner: TActivePlayer): integer;
begin
    Result := 0;
end;

function TMap.AddObject(Obj: TNewMapObject): TActiveMapObject;
begin
    Result := nil;
end;

function TMap.AddSpawnPoint(Spawn: TSpawnPoint): TActiveSpawnPoint;
begin
    Result := nil;
end;

procedure TMap.NextMap;
begin
end;

procedure TMap.SetMap(NewMap: string);
begin
end;

// ------------------------------- Mapslist ------------------------------------
procedure TMapsList.AddMap(Name: string);
begin
end;

procedure TMapsList.RemoveMap(Name: string);
begin
end;

function TMapsList.GetMapIdByName(Name: string): integer;
begin
    Result := 0;
end;

function TMapsList.GetMap(Num: integer): string;
begin
    Result := '';
end;

function TMapsList.GetCurrentMapId: integer;
begin
    Result := 0;
end;

procedure TMapsList.SetCurrentMapId(NewNum: integer);
begin
end;

function TMapsList.GetMapsCount: integer;
begin
    Result := 0;
end;

// -------------------------------- Math ---------------------------------------
function TMath.GetE: extended;
begin
    Result := 0.0;
end;

function TMath.GetPi: extended;
begin
    Result := 0.0;
end;

function TMath.Sin(A: extended): extended;
begin
    Result := 0.0;
end;

function TMath.Cos(A: extended): extended;
begin
    Result := 0.0;
end;

function TMath.Tan(A: extended): extended;
begin
    Result := 0.0;
end;

function TMath.Cotan(A: extended): extended;
begin
    Result := 0.0;
end;

function TMath.Pow(A, B: extended): extended;
begin
    Result := 0.0;
end;

function TMath.LogN(A, B: extended): extended;
begin
    Result := 0.0;
end;

function TMath.Ln(A: extended): extended;
begin
    Result := 0.0;
end;

function TMath.ArcSin(A: extended): extended;
begin
    Result := 0.0;
end;

function TMath.ArcCos(A: extended): extended;
begin
    Result := 0.0;
end;

function TMath.ArcTan(A: extended): extended;
begin
    Result := 0.0;
end;

function TMath.ArcCotan(A: extended): extended;
begin
    Result := 0.0;
end;

function TMath.ArcTan2(A, B: extended): extended;
begin
    Result := 0.0;
end;

function TMath.Min(A, B: extended): extended;
begin
    Result := 0.0;
end;

function TMath.Max(A, B: extended): extended;
begin
    Result := 0.0;
end;

function TMath.Abs(A: extended): extended;
begin
    Result := 0.0;
end;

function TMath.Exp(A: extended): extended;
begin
    Result := 0.0;
end;

function TMath.Sign(A: extended): TValueSign;
begin
    Result := 0;
end;

function TMath.IsNaN(A: extended): boolean;
begin
    Result := False;
end;

function TMath.Round(A: extended): integer;
begin
    Result := 0;
end;

function TMath.RoundTo(A: extended; B: TRoundToRange): extended;
begin
    Result := 0.0;
end;

function TMath.DegToRad(A: extended): extended;
begin
    Result := 0.0;
end;

function TMath.RadToDeg(A: extended): extended;
begin
    Result := 0.0;
end;

function TMath.DegNormalize(A: extended): extended;
begin
    Result := 0.0;
end;

function TMath.InRange(const A, B, C: double): boolean;
begin
    Result := False;
end;

function TMath.EnsureRange(const AValue, AMin, AMax: double): double;
begin
    Result := 0.0;
end;

function TMath.Random(Min, Max: extended): extended;
begin
    Result := 0.0;
end;

// -------------------------------- Object -------------------------------------
function TThing.GetX: single;
begin
    Result := 0;
end;

function TThing.GetY: single;
begin
    Result := 0;
end;

function TNewMapObject.GetStyle: byte;
begin
    Result := 0;
end;

constructor TNewMapObject.Create;
begin
end;

destructor TNewMapObject.Destroy;
begin
end;

procedure TNewMapObject.SetStyle(Style: byte);
begin
end;

procedure TNewMapObject.SetX(X: single);
begin
end;

procedure TNewMapObject.SetY(Y: single);
begin
end;

function TActiveMapObject.GetActive: boolean;
begin
    Result := False;
end;

function TActiveMapObject.GetID: byte;
begin
    Result := 0;
end;

function TActiveMapObject.GetStyle: byte;
begin
    Result := 0;
end;

procedure TActiveMapObject.Kill;
begin
end;

function TActiveFlag.GetInBase: boolean;
begin
    Result := False;
end;

// --------------------------------- Player ------------------------------------
constructor TNewPlayer.Create;
begin
end;

destructor TPlayer.Destroy;
begin
end;

//function TPlayer.GetSprite: TSprite;
//begin
//  Result := nil;
//end;

function TPlayer.GetTeam: byte;
begin
    Result := 0;
end;

function TPlayer.GetName: string;
begin
    Result := '';
    ;
end;

function TPlayer.GetAlive: boolean;
begin
    Result := False;
end;

function TPlayer.GetHealth: single;
begin
    Result := 0;
end;

procedure TPlayer.SetHealth(Health: single);
begin
end;

function TPlayer.GetVest: single;
begin
    Result := 0;
end;

procedure TPlayer.SetVest(Vest: single);
begin
end;

function TPlayer.GetPrimary: TPlayerWeapon;
begin
    Result := nil;
end;

function TPlayer.GetSecondary: TPlayerWeapon;
begin
    Result := nil;
end;

function TPlayer.GetShirtColor: longword;
begin
    Result := 0;
end;

function TPlayer.GetPantsColor: longword;
begin
    Result := 0;
end;

function TPlayer.GetSkinColor: longword;
begin
    Result := 0;
end;

function TPlayer.GetHairColor: longword;
begin
    Result := 0;
end;

function TPlayer.GetFavouriteWeapon: string;
begin
    Result := '';
end;

procedure TPlayer.SetFavouriteWeapon(Weapon: string);
begin
end;

function TPlayer.GetChosenSecondaryWeapon: byte;
begin
    Result := 0;
end;

function TPlayer.GetFriend: string;
begin
    Result := '';
end;

procedure TPlayer.SetFriend(Friend: string);
begin
end;

function TPlayer.GetAccuracy: byte;
begin
    Result := 0;
end;

procedure TPlayer.SetAccuracy(Accuracy: byte);
begin
end;

function TPlayer.GetShootDead: boolean;
begin
    Result := False;
end;

procedure TPlayer.SetShootDead(ShootDead: boolean);
begin
end;

function TPlayer.GetGrenadeFrequency: byte;
begin
    Result := 0;
end;

procedure TPlayer.SetGrenadeFrequency(Frequency: byte);
begin
end;

function TPlayer.GetCamping: boolean;
begin
    Result := False;
end;

procedure TPlayer.SetCamping(Camping: boolean);
begin
end;

function TPlayer.GetOnStartUse: byte;
begin
    Result := 0;
end;

procedure TPlayer.SetOnStartUse(Thing: byte);
begin
end;

function TPlayer.GetHairStyle: byte;
begin
    Result := 0;
end;

function TPlayer.GetHeadgear: byte;
begin
    Result := 0;
end;

function TPlayer.GetChain: byte;
begin
    Result := 0;
end;

function TPlayer.GetChatFrequency: byte;
begin
    Result := 0;
end;

procedure TPlayer.SetChatFrequency(Frequency: byte);
begin
end;

function TPlayer.GetChatKill: string;
begin
    Result := '';
end;

procedure TPlayer.SetChatKill(Message: string);
begin
end;

function TPlayer.GetChatDead: string;
begin
    Result := '';
end;

procedure TPlayer.SetChatDead(Message: string);
begin
end;

function TPlayer.GetChatLowHealth: string;
begin
    Result := '';
end;

procedure TPlayer.SetChatLowHealth(Message: string);
begin
end;

function TPlayer.GetChatSeeEnemy: string;
begin
    Result := '';
end;

procedure TPlayer.SetChatSeeEnemy(Message: string);
begin
end;

function TPlayer.GetChatWinning: string;
begin
    Result := '';
end;

procedure TPlayer.SetChatWinning(Message: string);
begin
end;

function TPlayer.GetAdmin: boolean;
begin
    Result := False;
end;

procedure TPlayer.SetAdmin(SetAsAdmin: boolean);
begin
end;

function TPlayer.GetDummy: boolean;
begin
    Result := False;
end;

procedure TPlayer.SetDummy(Dummy: boolean);
begin
end;

//constructor TActivePlayer.Create(var Sprite: TSprite; ID: Byte);
//begin
//end;

function TActivePlayer.GetKills: integer;
begin
    Result := 0;
end;

procedure TActivePlayer.SetKills(Kills: integer);
begin
end;

function TActivePlayer.GetDeaths: integer;
begin
    Result := 0;
end;

function TActivePlayer.GetPing: integer;
begin
    Result := 0;
end;

function TActivePlayer.GetActive: boolean;
begin
    Result := False;
end;

function TActivePlayer.GetIP: string;
begin
    Result := '';
end;

function TActivePlayer.GetPort: word;
begin
    Result := 0;
end;

procedure TActivePlayer.SetAlive(Alive: boolean);
begin
end;

function TActivePlayer.GetJets: integer;
begin
    Result := 0;
end;

function TActivePlayer.GetGrenades: byte;
begin
    Result := 0;
end;

function TActivePlayer.GetX: single;
begin
    Result := 0;
end;

function TActivePlayer.GetY: single;
begin
    Result := 0;
end;

function TActivePlayer.GetMouseAimX: smallint;
begin
    Result := 0;
end;

procedure TActivePlayer.SetMouseAimX(AimX: smallint);
begin
end;

function TActivePlayer.GetMouseAimY: smallint;
begin
    Result := 0;
end;

procedure TActivePlayer.SetMouseAimY(AimY: smallint);
begin
end;

function TActivePlayer.GetFlagger: boolean;
begin
    Result := False;
end;

function TActivePlayer.GetTime: integer;
begin
    Result := 0;
end;

function TActivePlayer.GetOnGround: boolean;
begin
    Result := False;
end;

function TActivePlayer.GetProne: boolean;
begin
    Result := False;
end;

function TActivePlayer.GetHuman: boolean;
begin
    Result := False;
end;

function TActivePlayer.GetDirection: shortint;
begin
    Result := 0;
end;

function TActivePlayer.GetFlags: byte;
begin
    Result := 0;
end;

function TActivePlayer.GetHWID: string;
begin
    Result := '';
end;

function TActivePlayer.GetKeyUp: boolean;
begin
    Result := False;
end;

procedure TActivePlayer.SetKeyUp(Pressed: boolean);
begin
end;

function TActivePlayer.GetKeyLeft: boolean;
begin
    Result := False;
end;

procedure TActivePlayer.SetKeyLeft(Pressed: boolean);
begin
end;

function TActivePlayer.GetKeyRight: boolean;
begin
    Result := False;
end;

procedure TActivePlayer.SetKeyRight(Pressed: boolean);
begin
end;

function TActivePlayer.GetKeyShoot: boolean;
begin
    Result := False;
end;

procedure TActivePlayer.SetKeyShoot(Pressed: boolean);
begin
end;

function TActivePlayer.GetKeyJetpack: boolean;
begin
    Result := False;
end;

procedure TActivePlayer.SetKeyJetpack(Pressed: boolean);
begin
end;

function TActivePlayer.GetKeyGrenade: boolean;
begin
    Result := False;
end;

procedure TActivePlayer.SetKeyGrenade(Pressed: boolean);
begin
end;

function TActivePlayer.GetKeyChangeWeap: boolean;
begin
    Result := False;
end;

procedure TActivePlayer.SetKeyChangeWeap(Pressed: boolean);
begin
end;

function TActivePlayer.GetKeyThrow: boolean;
begin
    Result := False;
end;

procedure TActivePlayer.SetKeyThrow(Pressed: boolean);
begin
    ;
end;

function TActivePlayer.GetKeyReload: boolean;
begin
    Result := False;
end;

procedure TActivePlayer.SetKeyReload(Pressed: boolean);
begin
end;

function TActivePlayer.GetKeyCrouch: boolean;
begin
    Result := False;
end;

procedure TActivePlayer.SetKeyCrouch(Pressed: boolean);
begin
end;

function TActivePlayer.GetKeyProne: boolean;
begin
    Result := False;
end;

procedure TActivePlayer.SetKeyProne(Pressed: boolean);
begin
end;

function TActivePlayer.GetKeyFlagThrow: boolean;
begin
    Result := False;
end;

procedure TActivePlayer.SetKeyFlagThrow(Pressed: boolean);
begin
end;


function TActivePlayer.Ban(Time: integer; Reason: string): boolean;
begin
    Result := False;
end;

procedure TActivePlayer.Say(Text: string);
begin
end;

procedure TActivePlayer.Damage(Shooter: byte; Damage: integer);
begin
end;

procedure TActivePlayer.BigText(Layer: byte; Text: string; Delay: integer;
    Color: longint; Scale: single; X, Y: integer);
begin
end;

procedure TActivePlayer.WorldText(Layer: byte; Text: string; Delay: integer;
    Color: longint; Scale, X, Y: single);
begin
end;

procedure TActivePlayer.ForceWeapon(Primary, Secondary: TNewWeapon);
begin
end;

procedure TActivePlayer.ForwardTo(TargetIP: string; TargetPort: word; Message: string);
begin
end;

procedure TActivePlayer.GiveBonus(BType: byte);
begin
end;

function TActivePlayer.Kick(reason: TKickReason): boolean;
begin
    Result := False;
end;

procedure TActivePlayer.ChangeTeam(NewTeam: byte; JoinType: TJoinType);
begin
end;

procedure TActivePlayer.Move(X, Y: single);
begin
end;

procedure TActivePlayer.SetVelocity(VelX, VelY: single);
begin
end;

procedure TActivePlayer.Tell(Text: string);
begin
end;

procedure TActivePlayer.WriteConsole(Text: string; Color: longint);
begin
end;

procedure TNewPlayer.SetTeam(Team: byte);
begin
end;

procedure TNewPlayer.SetName(Name: string);
begin
end;

procedure TNewPlayer.SetHealth(Health: single);
begin
end;

function TNewPlayer.GetPrimary: TWeapon;
begin
    Result := nil;
end;

procedure TNewPlayer.SetPrimary(Primary: TWeapon);
begin
end;

function TNewPlayer.GetSecondary: TWeapon;
begin
    Result := nil;
end;

procedure TNewPlayer.SetSecondary(Secondary: TWeapon);
begin
end;

procedure TNewPlayer.SetShirtColor(Color: longword);
begin
end;

procedure TNewPlayer.SetPantsColor(Color: longword);
begin
end;

procedure TNewPlayer.SetSkinColor(Color: longword);
begin
end;

procedure TNewPlayer.SetHairColor(Color: longword);
begin
end;

procedure TNewPlayer.SetHairStyle(Style: byte);
begin
end;

procedure TNewPlayer.SetHeadgear(Headgear: byte);
begin
end;

procedure TNewPlayer.SetChain(Chain: byte);
begin
end;

function TNewPlayer.GetDummy(): boolean;
begin
    Result := False;
end;

procedure TNewPlayer.SetDummy(Dummy: boolean);
begin
end;

procedure TActivePlayer.SetTeam(Team: byte);
begin
end;

function TActivePlayer.GetVelX: single;
begin
    Result := 0;
end;

function TActivePlayer.GetVelY: single;
begin
    Result := 0;
end;

function TActivePlayer.GetMuted: boolean;
begin
    Result := False;
end;

procedure TActivePlayer.SetWeaponActive(ID: byte; Active: boolean);
begin
end;

procedure TActivePlayer.SetMuted(Muted: boolean);
begin
end;

// -------------------------------- Players ------------------------------------
constructor TPlayers.Create;
begin
end;

destructor TPlayers.Destroy;
begin
end;

function TPlayers.GetPlayer(ID: byte): TActivePlayer;
begin
    Result := nil;
end;

function TPlayers.Add(Player: TNewPlayer; jointype: TJoinType): TActivePlayer;
begin
    Result := nil;
end;

procedure TPlayers.WriteConsole(Text: string; Color: longint);
begin
end;

procedure TPlayers.BigText(Layer: byte; Text: string; Delay: integer;
    Color: longint; Scale: single; X, Y: integer);
begin
end;

procedure TPlayers.WorldText(Layer: byte; Text: string; Delay: integer;
    Color: longint; Scale, X, Y: single);
begin
end;

function TPlayers.GetByName(Name: string): TActivePlayer;
begin
    Result := nil;
end;

function TPlayers.GetByIP(IP: string): TActivePlayer;
begin
    Result := nil;
end;

procedure TPlayers.Tell(Text: string);
begin
end;

// -------------------------------- Script -------------------------------------
function TScript.GetName: string;
begin
    Result := '';
end;

function TScript.GetVersion: string;
begin
    Result := '';
end;

function TScript.GetDir: string;
begin
    Result := '';
end;

function TScript.GetDebugMode: boolean;
begin
    Result := False;
end;


procedure TScript.Recompile(Force: boolean);
begin
end;

procedure TScript.Unload;
begin
end;

// ------------------------------ Spawnpoint -----------------------------------
function TSpawnPoint.GetActive: boolean;
begin
    Result := False;
end;

procedure TSpawnPoint.SetActive(Active: boolean);
begin
end;

function TSpawnPoint.GetX: longint;
begin
    Result := 0;
end;

procedure TSpawnPoint.SetX(X: longint);
begin
end;

function TSpawnPoint.GetY: longint;
begin
    Result := 0;
end;

procedure TSpawnPoint.SetY(Y: longint);
begin
end;

function TSpawnPoint.GetStyle: byte;
begin
    Result := 0;
end;

procedure TSpawnPoint.SetStyle(Style: byte);
begin
end;

constructor TNewSpawnPoint.Create;
begin
end;

destructor TNewSpawnPoint.Destroy;
begin
end;

//constructor TScriptActiveSpawnPoint.CreateActive(ID: Byte; var Spawn: TMapSpawnPoint);
//begin
//end;

// --------------------------------- Team --------------------------------------
constructor TTeam.Create(ID: byte);
begin
end;

destructor TTeam.Destroy;
begin
end;

procedure TTeam.AddPlayer(Player: TPlayer);
begin
end;

procedure TTeam.RemovePlayer(Player: TPlayer);
begin
end;

function TTeam.GetScore: byte;
begin
    Result := 0;
end;

procedure TTeam.SetScore(Score: byte);
begin
end;

function TTeam.GetPlayer(Num: byte): TPlayer;
begin
    Result := nil;
end;

function TTeam.GetCount: byte;
begin
    Result := 0;
end;

procedure TTeam.Add(Player: TPlayer);
begin
end;

// ------------------------------- Weapons -------------------------------------
constructor TNewWeapon.Create;
begin
end;

destructor TNewWeapon.Destroy;
begin
end;

function TWeapon.GetType: byte;
begin
    Result := 0;
end;

function TWeapon.GetName: string;
begin
    Result := '';
end;

function TWeapon.GetBulletStyle: byte;
begin
    Result := 0;
end;

function TWeapon.GetAmmo: byte;
begin
    Result := 0;
end;

procedure TPlayerWeapon.SetAmmo(Ammo: byte);
begin
end;


procedure TNewWeapon.SetAmmo(Ammo: byte);
begin
end;

procedure TNewWeapon.SetType(WType: byte);
begin
end;

end.
