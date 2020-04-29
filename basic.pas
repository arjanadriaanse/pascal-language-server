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

unit basic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TOptional }

  generic TOptional<T> = class
  private
    fHasValue: Boolean;
    fValue: T;
    function GetValue: T;
    procedure SetValue(AValue: T);
  public
    property HasValue: Boolean read fHasValue;
    property Value: T read GetValue write SetValue;
    procedure Clear;
  end;

  { TOptionalVariantBase }

  TOptionalVariantBase = class(specialize TOptional<Variant>);

  { TOptionalVariant }

  generic TOptionalVariant<T> = class(TOptionalVariantBase)
  private
    function GetValue: T;
    procedure SetValue(AValue: T);
  public
    constructor Create;
    constructor Create(AValue: T);
    property Value: T read GetValue write SetValue;
  end;

  { TOptionalObjectBase }

  TOptionalObjectBase = class(specialize TOptional<TObject>)
  public
    function ValueClass: TClass; virtual; abstract;
  end;

  { TOptionalObject }

  generic TOptionalObject<T> = class(TOptionalObjectBase)
  private
    function GetValue: T;
    procedure SetValue(AValue: T);
  public
    constructor Create;
    constructor Create(AValue: T);
    function ValueClass: TClass; override;
    property Value: T read GetValue write SetValue;
  end;

  { TGenericCollection }

  generic TGenericCollection<T> = class(TCollection)
    constructor Create;
  end;

  { TDocumentUri }

  TDocumentUri = string;

  { TPosition }

  TPosition = class(TPersistent)
  private
    fLine: Integer;
    fCharacter: Integer;
  published
    // Line position in a document (zero-based).
    property line: Integer read fLine write fLine;
    // Character offset on a line in a document (zero-based). Assuming
    // that the line is represented as a string, the `character` value
    // represents the gap between the `character` and `character + 1`.
    //
    // If the character value is greater than the line length it
    // defaults back to the line length.
    property character: Integer read fCharacter write fCharacter;
  end;

  { TRange }

  TRange = class(TPersistent)
  private
    fStart: TPosition;
    fEnd: TPosition;
  published
    // The range's start position.
    property start: TPosition read fStart write fStart;
    // The range's end position.
    property end_: TPosition read fEnd write fEnd;
  end;

  { TLocation }

  TLocation = class(TPersistent)
  private
    fUri: TDocumentUri;
    fRange: TRange;
  published
    property uri: TDocumentUri read fUri write fUri;
    property range: TRange read fRange write fRange;
  end;

  { TLocationLink }

  TLocationLink = class(TPersistent)
  private
    fOriginSelectionRange: TRange;
    fTargetUri: TDocumentUri;
    fTargetRange: TRange;
    fTargetSelectionRange: TRange;
  published
    // Span of the origin of this link.
    //
    // Used as the underlined span for mouse interaction. Defaults to
    // the word range at the mouse position.
    property originSelectionRange: TRange read fOriginSelectionRange write fOriginSelectionRange;
    // The target resource identifier of this link.
    property targetUri: TDocumentUri read fTargetUri write fTargetUri;
    // The full target range of this link. If the target for example
    // is a symbol then target range is the range enclosing this
    // symbol not including leading/trailing whitespace but everything
    // else like comments. This information is typically used to
    // highlight the range in the editor.
    property targetRange: TRange read fTargetRange write fTargetRange;
    // The range that should be selected and revealed when this link
    // is being followed, e.g the name of a function.  Must be
    // contained by the the `targetRange`. See also
    // `DocumentSymbol#range`
    property targetSelectionRange: TRange read fTargetSelectionRange write fTargetSelectionRange;
  end;

  { TTextDocumentIdentifier }

  TTextDocumentIdentifier = class(TPersistent)
  private
    fUri: TDocumentUri;
  published
    property uri: TDocumentUri read fUri write fUri;
  end;

  { TVersionedTextDocumentIdentifier }

  TVersionedTextDocumentIdentifier = class(TTextDocumentIdentifier)
  private
    fVersion: string;
  published
    // The version number of this document. If a versioned text
    // document identifier is sent from the server to the client and
    // the file is not open in the editor (the server has not received
    // an open notification before) the server can send `null` to
    // indicate that the version is known and the content on disk is
    // the master (as speced with document content ownership).
    //
    // The version number of a document will increase after each
    // change, including undo/redo. The number doesn't need to be
    // consecutive.
    property version: string read fVersion write fVersion;
  end;

  { TTextEdit }

  TTextEdit = class(TCollectionItem)
  private
    fRange: TRange;
    fNewText: string;
  published
    // The range of the text document to be manipulated. To insert
    // text into a document create a range where start === end.
    property range: TRange read fRange write fRange;
    // The string to be inserted. For delete operations use an empty
    // string.
    property newText: string read fNewText write fNewText;
  end;

  TTextEdits = specialize TGenericCollection<TTextEdit>;

  { TTextDocumentEdit }

  TTextDocumentEdit = class(TPersistent)
  private
    fTextDocument: TVersionedTextDocumentIdentifier;
    fEdits: TTextEdits;
  published
    // The text document to change.
    property textDocument: TVersionedTextDocumentIdentifier read fTextDocument write fTextDocument;
    // The edits to be applied.
    property edits: TTextEdits read fEdits write fEdits;
  end;

  { TTextDocumentItem }

  TTextDocumentItem = class(TPersistent)
  private
    fUri: TDocumentUri;
    fLanguageId: string;
    fVersion: Integer;
    fText: string;
  published
    // The text document's URI.
    property uri: TDocumentUri read fUri write fUri;
    // The text document's language identifier.
    property languageId: string read fLanguageId write fLanguageId;
    // The version number of this document (it will increase after
    // each change, including undo/redo).
    property version: Integer read fVersion write fVersion;
    // The content of the opened text document.
    property text: string read fText write fText;
  end;

  { TTextDocumentPositionParams }

  TTextDocumentPositionParams = class(TPersistent)
  private
    fTextDocument: TTextDocumentIdentifier;
    fPosition: TPosition;
  published
    // The text document.
    property textDocument: TTextDocumentIdentifier read fTextDocument write fTextDocument;
    // The position inside the text document.
    property position: TPosition read fPosition write fPosition;
  end;

  { TMarkupKind }

  // Describes the content type that a client supports in various
  // result literals like `Hover`, `ParameterInfo` or
  // `CompletionItem`.
  //
  // Please note that `MarkupKinds` must not start with a `$`. This
  // kinds are reserved for internal usage.
  TMarkupKind = string;

  { TMarkupContent }

  TMarkupContent = class(TPersistent)
  private
    fKind: TMarkupKind;
    fValue: string;
  published
    // The type of the Markup
    property kind: TMarkupKind read fKind write fKind;
    // The content itself
    property value: string read fValue write fValue;
  end;

implementation

{ TOptional }

function TOptional.GetValue: T;
begin
  if fHasValue then Result := fValue
  else Exception.Create('no value');
end;

procedure TOptional.SetValue(AValue: T);
begin
  fValue := AValue;
  fHasValue := True;
end;

procedure TOptional.Clear;
begin
  fHasValue := False;
end;

{ TOptionalVariant }

function TOptionalVariant.GetValue: T;
begin
  Result := T(inherited Value);
end;

procedure TOptionalVariant.SetValue(AValue: T);
begin
  inherited Value := AValue;
end;

constructor TOptionalVariant.Create;
begin
  inherited Create;
end;

constructor TOptionalVariant.Create(AValue: T);
begin
  Create;
  SetValue(AValue);
end;

{ TOptionalObject }

function TOptionalObject.GetValue: T;
begin
  Result := T(inherited Value);
end;

procedure TOptionalObject.SetValue(AValue: T);
begin
  inherited Value := AValue;
end;

constructor TOptionalObject.Create;
begin
  inherited Create;
end;

constructor TOptionalObject.Create(AValue: T);
begin
  Create;
  SetValue(AValue);
end;

function TOptionalObject.ValueClass: TClass;
begin
  Result := T;
end;

{ TGenericCollection }

constructor TGenericCollection.Create;
begin
  inherited Create(T);
end;

end.
