unit uHelper;

interface

uses IdHTTP, System.JSON, SysUtils, Variants, Classes;

const
    FCard = ['J', 'Q', 'K', 'A'];
    srvUrl = 'http://durakgame.s3.amazonaws.com/public/servers.json';

type
    TServer = record
        name: String;
        host: String;
        port: Integer;
    end;

    TServers = array [0..6] of TServer;

    TCard = record
        cType: char;
        cVal: String;
        use: boolean;
    end;

    THandCards = array of TCard;

function getHttp(url: String): String;
function getCommand(data: String): String;
function getJson(json: String): TJsonValue;
function getJsonParam(param: String; default: Variant; field: TJSONObject): Variant;
function getCard(value: String): TCard;
function getCards(hand: TJSONArray): THandCards;
function getRelevantCard(hand: THandCards; card: TCard; vc: TCard): TCard;
function getAnyCard(hand: THandCards; card: TCard): TCard;
function getNextCard(hand: THandCards; card: TCard): TCard;
function getMyCards(hand: THandCards): String;

procedure splitCard(var hand: THandCards; card: TCard);
procedure setUsesCard(var hand: THandCards; c: TCard; value: boolean = true);
procedure setUnUsesCard(var hand: THandCards);

implementation

procedure splitCard(var hand: THandCards; card: TCard);
begin
    SetLength(hand, Length(hand));
    hand[Length(hand) - 1].use := false;
end;

function getMyCards(hand: THandCards): String;
var
    i: integer;
begin
    Result := '';

    for i := 0 to Length(hand) - 1 do
        if NOT hand[i].use then
        Result := Result + hand[i].cType + hand[i].cVal + ' | ';


end;

procedure setUnUsesCard(var hand: THandCards);
var
    i: integer;
begin
    for i := 0 to Length(hand) - 1 do
        hand[i].use := false;
end;

procedure setUsesCard(var hand: THandCards; c: TCard; value: boolean = true);
var
    i: integer;
begin
    for i := 0 to Length(hand) - 1 do
        if (hand[i].cType = c.cType) AND (hand[i].cVal = c.cVal) then begin
            hand[i].use := value;
            break;
        end;
end;

function getCardIndex(v: String): Integer;
begin
    result := 0;
    if v = 'J' then result := 1;
    if v = 'Q' then result := 2;
    if v = 'K' then result := 3;
    if v = 'A' then result := 4;
end;

function canSetCard(a, b, c: TCard; cuse: boolean = false): boolean;
begin
    Result := false;
    if (b.use) then Exit;
    if (a.cType <> b.cType) AND (b.cType <> c.cType) then Exit;

    if (cuse) AND (b.cType = c.cType) AND (a.cType <> c.cType) then begin
        Result := true;
        Exit;
    end;

    if (a.cVal[1] in FCard) AND (NOT (b.cVal[1] in FCard)) then Exit;
    if (NOT (a.cVal[1] in FCard)) AND (b.cVal[1] in FCard) then Result := true;

    if (a.cVal[1] in FCard) AND (b.cVal[1] in FCard) then
        if getCardIndex(a.cVal) < getCardIndex(b.cVal) then
            Result := true;
end;

function getRelevantCard(hand: THandCards; card: TCard; vc: TCard): TCard;
var
    i: integer;
begin
    Result.cType := card.cType;

    // Norm
    for i := 0 to length(hand) - 1 do begin
        if canSetCard(card, hand[i], vc) then begin
            Result := hand[i];
            Exit;
        end;
    end;

    // Max
    for i := 0 to length(hand) - 1 do begin
        if canSetCard(card, hand[i], vc, true) then begin
            Result := hand[i];
            Exit;
        end;
    end;
end;

function getMinCard(hand: THandCards): TCard;
var
    i: integer;
    c: TCard;
begin
    for i := 0 to length(hand) - 1 do begin
        if hand[i].use then continue;
        
        if (c.cVal = '') then begin
            c := hand[i];
            continue;
        end;

        if (c.cVal[1] in FCard) AND (hand[i].cVal[1] in FCard) then
            if getCardIndex(c.cVal) > getCardIndex(hand[i].cVal) then begin
                c := hand[i];
                continue;
            end;

        if NOT (hand[i].cVal[1] in FCard) AND (c.cVal[1] in FCard) then begin
            c := hand[i];
            continue;
        end;

        if NOT (hand[i].cVal[1] in FCard) AND NOT (c.cVal[1] in FCard) then
            if (StrToInt(c.cVal) > StrToInt(hand[i].cVal)) then begin
              c := hand[i];
              continue;
            end;
    end;

    c.cVal := Trim(c.cVal);
    Result := c;
end;

function getAnyCard(hand: THandCards; card: TCard): TCard;
begin
    Result := getMinCard(hand);
end;

function getNextCard(hand: THandCards; card: TCard): TCard;
var
    i,j: Integer;
    c: TCard;
begin
    Result.cVal := '';

    for i := 0 to length(hand) - 1 do begin
        if (hand[i].use) then begin
            for j := 0 to length(hand) - 1 do begin
                if (i = j) then continue;

                if (hand[j].cVal = hand[i].cVal) AND (NOT hand[i].use) then begin
                    Result := hand[i];
                    Exit;
                end;
            end;
        end;

        if (hand[i].cVal = card.cVal) AND (NOT hand[i].use) then begin
            Result := hand[i];
            Exit;
        end;
    end;

    Result.cVal := Trim(Result.cVal);
end;

function getCard(value: String): TCard;
begin
    if (value = '') then exit;

    Result.cType := value[1];
    value[1] := ' ';
    Result.cVal := trim(value);
    Result.use := false;
end;

function getCards(hand: TJSONArray): THandCards;
var
    i: integer;
begin
    SetLength(Result, hand.Count);
    for i := 0 to hand.Count - 1 do
        Result[i] := getCard(hand.Items[i].Value);

    i := 0;
end;

function getHttp(url: String): String;
var
    lHTTP: TIdHTTP;
begin
    lHTTP := TIdHTTP.Create(nil);
    try
        try
            Result := lHTTP.Get(url);
        except end;
    finally
        lHTTP.Free;
    end;
end;


function getCommand(data: String): String;
var
    s: string;
    i: integer;
begin
    for i := 1 to Length(data) do begin
        if data[i] = '{' then break;
        s := s + data[i];
    end;

    result := s;
end;

function getJson(json: String): TJsonValue;
var
    data: string;
    jps, aps: integer;
begin
    data := json;

    if ((data[1] <> '{') or (data[1] <> '[')) then begin
        jps := pos('{', data);
        aps := pos('[', data);

        if ((aps <> 0) and (aps < jps)) then
            jps := aps;

        data := Copy(data, jps, length(data));
    end;

    try
    if data <> '' then
        result := TJSONObject.ParseJSONValue(data);
    except
        result := nil;
    end;
end;

function getJsonParam(param: String; default: Variant; field: TJSONObject): Variant;
begin
    try
    if (field.Values[param] <> nil) then Result := field.Values[param].Value else Result := default;
    except
        Result := default;
    end;
end;

end.
