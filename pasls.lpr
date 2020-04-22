program pasls;

{$mode objfpc}{$H+}

uses
  SysUtils, fpjson, jsonparser, jsonscanner,
  lsp, general, synchronization, completion;

const
  ContentType = 'application/vscode-jsonrpc; charset=utf-8';

var
  Dispatcher: TLSPDispatcher;
  Header, Name, Value, Content: string;
  I, Length: Integer;
  Request, Response: TJSONData;
begin
  Dispatcher := TLSPDispatcher.Create(nil);
  TJSONData.CompressedJSON := True;
  SetTextLineEnding(Input, #13#10);
  SetTextLineEnding(Output, #13#10);

  while not EOF do
  begin
    ReadLn(Header);
    while Header <> '' do
    begin
      I := Pos(':', Header);
      Name := Copy(Header, 1, I - 1);
      Delete(Header, 1, i);
      Value := Trim(Header);
      if Name = 'Content-Length' then Length := StrToInt(Value);
      ReadLn(Header);
    end;

    Content := '';
    SetLength(Content, Length);
    I := 1;
    while I <= Length do
    begin
      Read(Content[I]);
      Inc(I);
    end;

    Request := TJSONParser.Create(Content, DefaultOptions).Parse;
    Response := Dispatcher.Execute(Request);
    if Assigned(Response) then
    begin
      Content := Response.AsJSON;
      WriteLn('Content-Type: ', ContentType);
      WriteLn('Content-Length: ', Content.Length);
      WriteLn;
      Write(Content);
      Flush(Output);
    end;
  end;
end.
