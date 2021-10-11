unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uAPI, uHelper, Vcl.StdCtrls, System.JSON;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    RegName: TEdit;
    Amount: TEdit;
    Button9: TButton;
    ServersList: TComboBox;
    GameMode: TComboBox;
    GamePass: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Deck: TComboBox;
    Players: TComboBox;
    Label6: TLabel;
    Label7: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ServersListChange(Sender: TObject);
    procedure Button9Click(Sender: TObject);

  private
    { Private declarations }
    procedure logout(answer: String);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

  bot: TGameBot;
  servers: TServers;

  host: String;
  port: Integer;
implementation

{$R *.dfm}

procedure getServers();
var
    i : integer;
    data: String;
    json: TJSONValue;
    srv : TJSONValue;
begin
    data := getHttp(srvUrl);
    json := getJson(data);

    try
        json := (json as TJSONObject).Values['user'];

        for i := 1 to 6 do begin
            srv := (json as TJSONObject).Values['u'+IntToStr(i)];
            servers[i-1].host := getJsonParam('host', '', srv as TJSONObject);
            servers[i-1].port := StrToInt(getJsonParam('port', '0', srv as TJSONObject));
            servers[i-1].name := getJsonParam('en', '', (srv as TJSONObject).Values['name'] as TJSONObject);

            Form1.ServersList.Items.Add(servers[i-1].name);
        end;

        Form1.ServersList.ItemIndex := 0;
        host := servers[0].host;
        port := servers[0].port;
    finally
    end;
end;

procedure TForm1.logout(answer: String);
begin
    Form1.Memo1.Lines.Add(answer);
end;

procedure TForm1.ServersListChange(Sender: TObject);
begin
    host := servers[ServersList.ItemIndex].host;
    port := servers[ServersList.ItemIndex].port;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
    bot := TGameBot.Create(host, port);
    bot.log := logout;
    bot.doRegister(RegName.Text);

    Memo1.Lines.Clear;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
    gm: TGameMode;
    pl: Integer;
    dc: Integer;
begin
    case GameMode.ItemIndex of
        0 : gm := lose;
        1 : gm := take;
        2 : gm := game;
    end;

    case Players.ItemIndex of
        0 : pl := 2;
        1 : pl := 3;
        2 : pl := 4;
    end;

    case Deck.ItemIndex of
        0 : dc := 24;
        1 : dc := 36;
        2 : dc := 54;
    end;

    bot.doNewGame(GamePass.Text, StrToInt(Amount.Text), gm, dc, pl);
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
    Application.Terminate;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    getServers;
end;

end.
