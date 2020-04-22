unit capabilities;

{$mode objfpc}{$H+}

interface

uses
  Classes, options;

type

  { TWorkspaceClientCapabilities }

  TWorkspaceClientCapabilities = class(TPersistent)
  private
    fApplyEdit: Boolean;
  published
    property applyEdit: Boolean read fApplyEdit write fApplyEdit;
  end;

  { TTextDocumentClientCapabilities }

  TTextDocumentClientCapabilities = class(TPersistent)
  private
  published
  end;

  { TClientCapabilities }

  TClientCapabilities = class(TPersistent)
  private
    fWorkspace: TWorkspaceClientCapabilities;
    fTextDocument: TTextDocumentClientCapabilities;
  published
    property workspace: TWorkspaceClientCapabilities read fWorkspace write fWorkspace;
    property textDocument: TTextDocumentClientCapabilities read fTextDocument write fTextDocument;
  end;

  { TServerCapabilities }

  TServerCapabilities = class(TPersistent)
  private
    fTextDocumentSync: TTextDocumentSyncOptions;
    fCompletionProvider: TCompletionOptions;
  public
    constructor Create;
  published
    property textDocumentSync: TTextDocumentSyncOptions read fTextDocumentSync write fTextDocumentSync;
    property completionProvider: TCompletionOptions read fCompletionProvider write fCompletionProvider;
  end;

implementation

{ TServerCapabilities }

constructor TServerCapabilities.Create;
begin
  textDocumentSync := TTextDocumentSyncOptions.Create;
  completionProvider := TCompletionOptions.Create;
end;

end.

