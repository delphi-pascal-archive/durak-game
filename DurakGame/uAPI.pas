unit uAPI;

interface

uses IdTCPClient, IdGlobal, SysUtils, Variants, Classes, System.JSON, uHelper;

const
    GM_LOSE = 0;
    GM_TAKE = 1;
    GM_GAME = 2;

type
    TGameMode = (lose, take, game);
    TLogOut = procedure (answer: String) of object ;

    TGameBot = class
        log: TLogOut;

        host: String;
        port: Integer;

        conn: TIdTCPClient;

        token: String;
        gameId: String;
        gameMode: TGameMode;

        hand: THandCards;
        card: TCard;

        upddesc: boolean;

        login: boolean;

        constructor Create(host: String; port: Integer); overload;
    private

    public
        procedure Connect();

        procedure doStart;
        procedure doRegister(name: String);
        procedure doLogin(token: String = '');

        procedure doNewGame(pass: String; amount: Integer; mode: TGameMode; deck: Integer; players: Integer);
        procedure doReady;
        procedure doLeaveGame;
        procedure doGameOver;
        procedure doTake;
        procedure doSmile(id: Integer);
        procedure doDone;

        procedure doPutCard;
        procedure doSetCard(b: TCard);
        procedure doPutNextCard(rt: TCard);

        procedure sendCmd(cmd: String);
    end;

    TAnswer = class (TThread)
    private
        bot: TGameBot;
        answer: WideString;

        procedure Output;
        procedure PrintLog(log: String);
    protected
        procedure Execute; override;
    public
        constructor Create(_bot: TGameBot);
    end;

implementation

constructor TAnswer.Create(_bot: TGameBot);
begin
    self.bot := _bot;

    inherited Create();
end;

procedure TAnswer.PrintLog(log: String);
begin
    if (@bot.log <> nil) then
        bot.log(log);
end;

procedure TAnswer.Output();
var
    json: TJSONValue;
    k, v, cmd: string;

begin
    cmd := getCommand(self.answer);
    json := getJson(self.answer);

    if (cmd = 'set_token') then begin
        bot.token := getJsonParam('token', '', json as TJSONObject);
        bot.doLogin(bot.token);
        PrintLog('Получили токен: ' + bot.token);
    end;

    if (cmd = 'game') then begin
        bot.gameid := getJsonParam('id', '', json as TJSONObject);
        SetLength(bot.hand, 0);
        PrintLog('Создана игра с ID: ' + bot.gameId);
    end;

    if (cmd = 'btn_ready_on') then begin
        bot.doReady;
        bot.doSmile(random(24));
        PrintLog('Готов к игре!');
    end;

    if (cmd = 'game_start') then begin
        if (bot.gameMode = lose) then bot.doGameOver;
        bot.doSmile(random(24));
        bot.upddesc := true;
        PrintLog('Игра запущена!');
    end;

    if (cmd = 'mode') then begin
        k := getJsonParam('0', '', json as TJSONObject);
        v := getJsonParam('1', '', json as TJSONObject);

        if (k = '9') AND (v = '2') AND (bot.gameMode = take) then bot.doTake;
        if (k = '1') AND (v = '8') AND (bot.gameMode = take) then bot.doGameOver;

        if (k = '0') AND (v = '7') AND (bot.gameMode = game) then bot.doDone;
        if (k = '9') AND (v = '2') AND (bot.gameMode = game) then begin end;
        if (k = '1') AND (v = '8') AND (bot.gameMode = game) then begin
            bot.doPutCard;
        end;
    end;

    if (cmd = 'uu') then begin
        k := getJsonParam('k', '', json as TJSONObject);
        v := getJsonParam('v', '', json as TJSONObject);

        if (k = 'points') then
            PrintLog('Баланс: ' + v);
    end;

    if (cmd = 'p') then begin
        PrintLog('Подключился пользователь: ' + getJsonParam('name', '', (json as TJsonObject).Values['user'] as TJsonObject));
    end;

    // Обновляем карты на руках
    if (cmd = 'hand') then begin
        bot.hand := getCards((json as TJSONObject).Values['cards'] as TJSONArray);
    end;

    if (cmd = 'b') then begin
        bot.doPutNextCard(getCard(getJsonParam('b', '', json as TJsonObject)));
    end;

    if (cmd = 'rt') then begin
        setUsesCard(bot.hand, getCard(getJsonParam('b', '', json as TJsonObject)), false);
        bot.doDone;
    end;

    if (cmd = 't') then begin
        // Бой
        if (getJsonParam('id', '', json as TJsonObject) <> '') then
            bot.doSetCard(getCard(getJsonParam('c', '', json as TJsonObject)));
        //
    end;

    if (cmd = 'turn') then begin
        bot.card := getCard(getJsonParam('trump', '', json as TJsonObject));

        if (getJsonParam('deck', '0', json as TJsonObject) = '0') then
            bot.upddesc := false else bot.upddesc := true;
    end;

    if (cmd = 'authorized') then begin
        bot.login := true;
        PrintLog('Авторизовались с ID: ' + getJsonParam('id', '', json as TJsonObject));
    end;
end;

procedure TAnswer.Execute;
begin
    inherited;

    while self.bot.conn.Connected do begin
        self.answer := self.bot.conn.IOHandler.ReadLn(IndyTextEncoding_UTF8);
        Synchronize(Output);
    end;
end;

{******************************************************************************}

constructor TGameBot.Create(host: String; port: Integer);
begin
    {inherited;}

    self.host := host;
    self.port := port;

    self.conn := TIdTCPClient.Create(nil);
    self.conn.Host := host;
    self.conn.Port := port;

    self.login := false;

    Connect;
end;

procedure TGameBot.sendCmd(cmd: String);
begin
    self.conn.IOHandler.WriteLn(cmd + #$0A, IndyTextEncoding_UTF8);
end;

procedure TGameBot.Connect();
begin
    self.conn.Connect;
    TAnswer.Create(self);
end;

procedure TGameBot.doStart;
begin
    self.sendCmd('client{"platform":"android","protocol":1,"tz":"+00:00","lang":"en","version":"1.0.4","name":"durak.android"}');
end;

procedure TGameBot.doRegister(name: String);
begin
    doStart();
    self.sendCmd('register{"name":"' + name + '"}');
end;

procedure TGameBot.doLogin(token: String = '');
begin
    doStart();

    if (token = '') then
        self.sendCmd('auth{"token":"' + self.token + '"}')
        else
        self.sendCmd('auth{"token":"' + token + '"}')
end;

procedure TGameBot.doNewGame(pass: String; amount: Integer; mode: TGameMode; deck: Integer; players: Integer);
begin
    self.gameMode := mode;
    self.sendCmd('create{"ch":false,"nb":true,"deck":'+IntToStr(deck)+',"password":"' + pass + '","sw":false,"players":'+IntToStr(players)+',"bet":' + IntToStr(amount) + '}');
end;

procedure TGameBot.doReady;
begin
    self.sendCmd('ready');
    doSmile(random(24));
end;

procedure TGameBot.doTake;
begin
    self.sendCmd('take');
    doSmile(random(24));
end;

procedure TGameBot.doGameOver;
begin
    self.sendCmd('surrender');
    doSmile(random(24));
end;

procedure TGameBot.doLeaveGame;
begin
    self.sendCmd('leave{"id":' + self.gameid + '}');
end;

procedure TGameBot.doPutNextCard(rt: TCard);
var
    i: integer;
    c: TCard;
begin
    c := getNextCard(hand, rt);

    if (c.cVal <> '') then begin
        setUsesCard(hand, c);
        sendCmd('t{"c":"'+c.cType+c.cVal+'"}');

        for i := 0 to Length(hand) do
            if (hand[i].cVal = c.cVal) AND (hand[i].cType <> c.cType) then begin
                setUsesCard(hand, hand[i]);
                sendCmd('t{"c":"'+hand[i].cType+hand[i].cVal+'"}');
            end;
    end else begin
        if gameMode = lose then doGameOver else doDone;
    end;

end;

procedure TGameBot.doPutCard();
var
    i: integer;
    c,b: TCard;
begin
    c := getAnyCard(hand, card);

    if (c.cVal <> '') then begin
        setUsesCard(hand, c);
        sendCmd('t{"c":"'+c.cType+c.cVal+'"}');

        for i := 0 to Length(hand) do
            if (hand[i].cVal = c.cVal) AND (hand[i].cType <> c.cType) then begin
                setUsesCard(hand, hand[i]);
                sendCmd('t{"c":"'+hand[i].cType+hand[i].cVal+'"}');
            end;
    end else begin
        if gameMode = lose then doGameOver else doDone;
    end;

    doSmile(random(24));
end;

procedure TGameBot.doSetCard(b: TCard);
var
    c: TCard;
begin
    c := getRelevantCard(hand, b, card);

    if (c.cVal <> '') then
        sendCmd('b{"b":"'+c.cType+c.cVal+'","c":"'+b.cType+b.cVal+'"}')
        else begin
            doTake;
        end;

    setUsesCard(hand, c);

    if (NOT upddesc) then
        splitCard(hand, b);

    doSmile(random(24));
end;

procedure TGameBot.doDone;
begin
    sendCmd('done');
    sendCmd('pass');
end;

procedure TGameBot.doSmile(id: Integer);
begin
    sendCmd('smile{"id":'+IntToStr(id)+'}');
end;

end.
