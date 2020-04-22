unit general;

{$mode objfpc}{$H+}

interface

uses
  Classes, CodeToolManager, CodeToolsConfig, URIParser, LazUTF8,
  lsp, capabilities;

type

  { TVoidParams }

  TVoidParams = class(TPersistent);

  { TInitializeParams }

  TInitializeParams = class(TPersistent)
  private
    //fProcessId: string;
    fRootUri: string;
    fCapabilities: TClientCapabilities;
  published
    //property processId: string read fProcessId write fProcessId;
    property rootUri: string read fRootUri write fRootUri;
    property capabilities: TClientCapabilities read fCapabilities write fCapabilities;
  end;

  { TInitializeResult }

  TInitializeResult = class(TPersistent)
  private
    fCapabilities: TServerCapabilities;
  published
    property capabilities: TServerCapabilities read fCapabilities write fCapabilities;
  end;

  { TInitialize }

  TInitialize = class(specialize TLSPRequest<TInitializeParams, TInitializeResult>)
    function Process(var Params : TInitializeParams): TInitializeResult; override;
  end;

  { TInitialized }

  TInitialized = class(specialize TLSPNotification<TVoidParams>)
    procedure Process(var Params : TVoidParams); override;
  end;

  { TCancelParams }

  TCancelParams = class(TPersistent)
  private
    fId: Integer;
  published
    property id: Integer read fId write fId;
  end;

  { TShutdown }

  TShutdown = class(specialize TLSPRequest<TVoidParams, TPersistent>)
    function Process(var Params : TVoidParams): TPersistent; override;
  end;

  { TExit }

  TExit = class(specialize TLSPNotification<TVoidParams>)
    procedure Process(var Params : TVoidParams); override;
  end;

  { TCancel }

  TCancel = class(specialize TLSPNotification<TCancelParams>)
    procedure Process(var Params : TCancelParams); override;
  end;

implementation

{ TInitialize }

function TInitialize.Process(var Params : TInitializeParams): TInitializeResult;
var
  CodeToolsOptions: TCodeToolsOptions;
begin with Params do
  begin
    CodeToolsOptions := TCodeToolsOptions.Create;
    with CodeToolsOptions do
    begin
      InitWithEnvironmentVariables;
      ProjectDir := ParseURI(rootUri).Path;
    end;
    with CodeToolBoss do
    begin
      Init(CodeToolsOptions);
      IdentifierList.SortForHistory := True;
      IdentifierList.SortForScope := True;
    end;

    Result := TInitializeResult.Create;
    Result.capabilities := TServerCapabilities.Create;
  end;
end;

{ TInitialized }

procedure TInitialized.Process(var Params : TVoidParams);
begin
  // do nothing
end;

{ TShutdown }

function TShutdown.Process(var Params : TVoidParams): TPersistent;
begin
  // do nothing
end;

{ TExit }

procedure TExit.Process(var Params : TVoidParams);
begin
  Halt(0);
end;

{ TCancel }

procedure TCancel.Process(var Params : TCancelParams);
begin
  // not supported
end;

initialization
  LSPHandlerManager.RegisterHandler('initialize', TInitialize);
  LSPHandlerManager.RegisterHandler('initialized', TInitialized);
  LSPHandlerManager.RegisterHandler('shutdown', TShutdown);
  LSPHandlerManager.RegisterHandler('exit', TExit);
  LSPHandlerManager.RegisterHandler('$/cancelRequest', TCancel);
end.

