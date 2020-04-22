// Pascal Language Server
// Copyright 2020 Arjan Adriaanse

// This file is part of Pascal Language Server.

// Pascal Language Server is free software: you can redistribute it
// and/or modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.

// Pascal Language Server is distributed in the hope that it will be
// useful, but WITHOUT ANY WARRANTY; without even the implied warranty
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Pascal Language Server.  If not, see
// <https://www.gnu.org/licenses/>.

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

