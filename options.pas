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

unit options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

type

  { TTextDocumentSyncKind }

  // Defines how the host (editor) should sync document changes to the
  // language server.
  TTextDocumentSyncKind = (
    // Documents should not be synced at all.
    None = 0,
    // Documents are synced by always sending the full content
    Full = 1,
    // Documents are synced by sending the full content on open.
    // After that only incremental updates to the document are send.
    Incremental = 2);

  { TTextDocumentSyncOptions }

  TTextDocumentSyncOptions = class(TPersistent)
  private
    fOpenClose: Boolean;
    fChange: TTextDocumentSyncKind;
  public
    constructor Create;
  published
    // Open and close notifications are sent to the server. If omitted
    // open close notification should not be sent.
    property openClose: Boolean read fOpenClose write fOpenClose;
    // Change notifications are sent to the server. See
    // TextDocumentSyncKind.None, TextDocumentSyncKind.Full and
    // TextDocumentSyncKind.Incremental. If omitted it defaults to
    // TextDocumentSyncKind.None.
    property change: TTextDocumentSyncKind read fChange write fChange;
  end;

  { TCompletionOptions }

  // Completion options.
  TCompletionOptions = class(TPersistent)
  private
    fTriggerCharacters: TStrings;
    fAllCommitCharacters: TStrings;
    fResolveProvider: Boolean;
  public
    constructor Create;
  published
    // Most tools trigger completion request automatically without
    // explicitly requesting it using a keyboard shortcut
    // (e.g. Ctrl+Space). Typically they do so when the user starts to
    // type an identifier. For example if the user types `c` in a
    // JavaScript file code complete will automatically pop up present
    // `console` besides others as a completion item. Characters that
    // make up identifiers don't need to be listed here.
    //
    // If code complete should automatically be trigger on characters
    // not being valid inside an identifier (for example `.` in
    // JavaScript) list them in `triggerCharacters`.
    property triggerCharacters: TStrings read fTriggerCharacters write fTriggerCharacters;
    // The list of all possible characters that commit a
    // completion. This field can be used if clients don't support
    // individual commit characters per completion item. See
    // `ClientCapabilities.textDocument.completion.completionItem.commitCharactersSupport`.
    //
    // If a server provides both `allCommitCharacters` and commit
    // characters on an individual completion item the ones on the
    // completion item win.
    //
    // @since 3.2.0
    property allCommitCharacters: TStrings read fAllCommitCharacters write fAllCommitCharacters;
    // The server provides support to resolve additional information
    // for a completion item.
    property resolveProvider: Boolean read fResolveProvider write fResolveProvider;
  end;

implementation

{ TTextDocumentSyncOptions}

constructor TTextDocumentSyncOptions.Create;
begin
  openClose := True;
  change := TTextDocumentSyncKind.Full;
end;

{ TCompletionOptions }

constructor TCompletionOptions.Create;
begin
  resolveProvider := False;
end;

end.

