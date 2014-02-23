unit IRC;

{$mode objfpc}{$H+}

interface

uses Sockets, Winsock2, SysUtils, Classes, Contnrs, SyncOBJS;

type
  TAddress = array[0..3] of byte;

  TMessage = record
    Sender: String;
    Content: String;
  end;

  PMessage = ^TMessage;

  TDataReadThread = class(TThread)
  protected
      procedure Execute; override;

    public
      Datas: TQueue;
      constructor Create(CreateSuspended: Boolean; Sock: LongInt; var aSin, aSout: Text);

    private
      Socket: LongInt;
      Sin, Sout: Text;
	end;

  TIRCClient = class
		public
      _Server: TAddress;
      _Port: Integer;
      PassWord: String;
      Nick: String;
      _altNick: String;
      irc: LongInt;
      inputLine: String;
      Sin, Sout: Text;
			DRT: TDataReadThread;
      MessageQueue: TQueue;

      constructor Create(Server: TAddress; Port: Integer);
      constructor Create(Server: TAddress);

      //Flags
      function Connected: Boolean;
      function HasMessages: Boolean;

      procedure Listen;
      procedure Connect;

      //Irc stuff
      procedure Join(Channel: String);
      procedure Part(Channel: String);
      procedure SendMessage(Message: String; Channel: String);
      function GetMessage: TMessage;

    private
      procedure Send(Message: String);
      procedure ParseData(Data: String);
  end;

function IPFromHostName(HostName: String): TAddress;
procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings);
function RestAfter(Strs: TStringList; Pos: Integer): String;

var
  CriticalSection: TCriticalSection;

implementation

constructor TIRCClient.Create(Server: TAddress; Port: Integer);
begin
  _Server := Server;
  _Port := Port;

  MessageQueue := TQueue.Create;
end;

constructor TIRCClient.Create(Server: TAddress);
begin
  Create(Server, 6667);
end;

function TIRCClient.Connected: Boolean;
begin
  Connected := True;
end;

procedure TIRCClient.Send(Message: String);
var
  Buffer: String;
begin
  Buffer := Message;
  WriteLN(Sout, Buffer);
  Flush(Sout);
end;

procedure TIRCClient.Listen;
var
  Dat: PChar;

begin
  CriticalSection.Acquire;
  try
    while (DRT.Datas.Count > 0) do begin
      Dat := PChar(DRT.Datas.Pop);
      WriteLN(Dat);
      ParseData(Dat);
      StrDispose(Dat);
    end;

  finally
    CriticalSection.Release;
  end;
end;

procedure TIRCClient.ParseData(Data: String);
var
  IRCData, PrivMsgData: TStringList;
  StrBuf: String;
  Message: PMessage;

begin
  IRCData := TStringList.Create;
  Split(' ', Data, IRCData);

	New(Message);

  if (Length(Data) > 4) then begin
    StrBuf := Copy(Data, 0, 4);
    if StrBuf = 'PING' then begin
      Send('PONG ' + IRCData[1]);
      Exit;
    end;
end;

  case IRCData[1] of
  	'001': Send('MODE ' + Nick + '+B');
    'PRIVMSG': begin
      if LowerCase(IRCData[2]) <> LowerCase(Nick) then begin
				Message^.Content := RestAfter(IRCData, 3);
        PrivMsgData := TStringList.Create;
        Split('!', Data, PrivMsgData);
        Message^.Sender := Copy(PrivMsgData[0], 2, Length(PrivMsgData[0])-2);
				MessageQueue.Push(Message);
      end;
    end;
  end;

  IRCData.Destroy;
end;

procedure TIRCClient.Connect;
var
  SAddr: TInetSockAddr;

begin
  IRC := fpSocket(AF_INET, SOCK_STREAM, 0);
  SAddr.sin_family := AF_INET;
  SAddr.sin_port := htons(_port);
  move(_Server, SAddr.Sin_addr,4);
  //SAddr.sin_addr.s_addr := _Server;
  if not Sockets.Connect(IRC, SAddr, Sin, Sout) then WritelN('error');
  Reset(sin);
  ReWrite(sout);

	DRT := TDataReadThread.Create(True, IRC, Sin, Sout);
  DRT.Start;

  if PassWord <> '' then Send('PASS ' + PassWord);
  Send('NICK ' + Nick);
  Send('USER ' + Nick + ' 0 * :' + Nick);
end;

function TIRCClient.HasMessages: Boolean;
begin
  HasMessages := MessageQueue.Count > 0;
end;

{IRC FUNCTIONS}
procedure TIRCClient.Join(Channel: String);
begin
  Send('JOIN ' + Channel);
end;

procedure TIRCClient.Part(Channel: String);
begin
  Send('PART ' + Channel);
end;

function TIRCClient.GetMessage: TMessage;
var
  Message: PMessage;
  Message2: TMessage;
begin
  Message := MessageQueue.Pop;
	Message2 := Message^;
  Dispose(Message);
  GetMessage := Message2;
end;

procedure TIRCClient.SendMessage(Message: String; Channel: String);
begin
  Send('PRIVMSG ' + Channel + ' :' + Message);
end;

{HELPERS}

function IPFromHostName(HostName: String): TAddress;
var
  HostChar: PChar;
  AddrBuf: TAddress;
  Hint: pHostEnt;
begin
  HostChar := PChar(HostName);
  Hint := gethostbyname(HostChar);

  move(Hint^.H_Addr^^, AddrBuf,4);

  IPFromHostName := AddrBuf;
end;

procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings) ;
begin
   ListOfStrings.Clear;
   ListOfStrings.Delimiter     := Delimiter;
   ListOfStrings.DelimitedText := Str;
end;

function RestAfter(Strs: TStringList; Pos: Integer): String;
var
  I: Integer;
  Buf: String;
begin
  Buf := '';

	for I := Pos to Strs.Count-1 do
		Buf += Strs[I] + ' ';

  RestAfter := Copy(Buf, 2, Length(Buf)-2);
end;

{DATA READ THREAD}
constructor TDataReadThread.Create(CreateSuspended: Boolean; Sock: LongInt; var aSin, aSout: Text);
begin
  Datas := TQueue.Create;

  FreeOnTerminate := True;
  Socket := Sock;

  Sin := aSin;
  Sout := aSout;
  inherited Create(CreateSuspended);
end;

procedure TDataReadThread.Execute;
var
  Buffer: String;

begin
  while (true) do begin
		ReadLN(Sin, Buffer);
    CriticalSection.Acquire;
    try
      Writeln('Putting ' + buffer);
      Datas.Push(StrNew(PChar(Buffer)));
    finally
      CriticalSection.Release;
    end;
  end;
end;

begin
  CriticalSection := TCriticalSection.Create;
end.

