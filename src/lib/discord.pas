unit Discord;

interface
uses
	Scriptcore,
	Prelude;

type
	StrPair = record
		fst: String;
		snd: String;
	end;

	EmbedFooter = record
		text: String;
		icon_url: String;
	end;

	EmbedFieldItem = record
		name: String;
		value: String;
		inln: Boolean;
	end;

	DiscordEmbed = record
		title: String;
		title_url: String;
		description: String;
		colour: Integer;
		thumbnail_url: String;
		image_url: String;
		fields: Array of EmbedFieldItem;
	end;

function JsonKV(key: String; value: Variant): String;
function makeFieldItem(name: String; value: String; inln: Boolean): EmbedFieldItem;
function Discord_EmbedFieldItemToJson(fieldItem: EmbedFieldItem): String;
function Discord_EmbedFieldsToJson(fields: Array of EmbedFieldItem): String;
function Discord_EmbedToJson(embed: DiscordEmbed): String;
procedure Discord_SendWebhookMessage(
	username: String;
	content: String;
	embed: DiscordEmbed;
	link: String
);

implementation

function JsonKV(key: String; value: Variant): String;
var
	str: String;
begin
	// Result := '"' + key + '": "' + EscapeQuotesBackslash(value) + '"';
	str := Show(value);
	WriteDebug(6, 'JsonKV() value = ' + str);

	if str = Nil then
		// Can't raise in PascalScript??
		// raise Exception.Create('JsonKV called with something unserialisable')
		Result := '"' + key + '": ""'
	else
		Result := '"' + key + '": "' + str + '"';
end;

function makeFieldItem(name: String; value: String; inln: Boolean): EmbedFieldItem;
begin
	Result.name := name;
	Result.value := value;
	Result.inln := inln;
end;

function Discord_EmbedFieldItemToJson(fieldItem: EmbedFieldItem): String;
begin
	Result := '{'
		+ JsonKV('name', fieldItem.name) + ','
		+ JsonKV('value', fieldItem.value) + ','
		+ JsonKV('inline', iif(fieldItem.inln, 'true', 'false'))
		+ '}';
end;

function Discord_EmbedFieldsToJson(fields: Array of EmbedFieldItem): String;
var
	i: Integer;
	last: Integer;
begin
	Result := '[';
	last := high(fields);
	for i := low(fields) to last do
	begin
		Result := Result + Discord_EmbedFieldItemToJson(fields[i]);
		if i <> last then Result := Result + ','
	end;
	Result := Result + ']';
end;

function Discord_EmbedToJson(embed: DiscordEmbed): String;
begin
	Result := '{'
		+ JsonKV('type', 'rich') + ','
		+ JsonKV('title', embed.title) + ','
		+ JsonKV('url', embed.title_url) + ','
		+ JsonKV('description', embed.description) + ','
		+ '"thumbnail": {' + JsonKV('url', embed.thumbnail_url) + '},'
		+ JsonKV('color', embed.colour) + ','
		+ '"fields": ' + Discord_EmbedFieldsToJson(embed.fields)
		+ '}';
end;

procedure Discord_SendWebhookMessage(
	username: String;
	content: String;
	embed: DiscordEmbed;
	link: String
);
var
	shellCmd: String;
	i: Integer;
begin
	shellCmd := 'curl -X POST -H "Content-Type: application/json" -d ''{'
		+ JsonKV('username', username) + ','
		+ JsonKV('content', content) + ','
		+ '"embeds": [' + Discord_EmbedToJson(embed) + ']'
		+ '}'' '
		+ link
		+ ' &';
	// WriteDebug(6, shellCmd);
	shell_exec(shellCmd);
end;

end.
