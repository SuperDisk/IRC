program Project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, Sockets, IRC, Winsock;

var
  Client: TIRCClient;
  Msg: String;

begin
  //MyAddrBuf := IpFromHostName(URL);

  Client := TIRCClient.Create(IpFromHostName('irc.freenode.net'), 6667);
  Client.Nick := 'Nickelodeon';
  Client.Connect;
  Client.Join('#Blergity');
  Client.SendMessage('Hi everyone', '#Blergity');

  while true do begin
		Client.Listen;
		Msg := Client.GetMessage;
    if Msg <> '' then WriteLN(Msg);
  end;

  readln;
end.

