unit synchronization;

{$mode objfpc}{$H+}

interface

uses
  Classes, URIParser, CodeToolManager, CodeCache,
  lsp, basic;

type

  { TDidOpenTextDocumentParams }

  TDidOpenTextDocumentParams = class(TPersistent)
  private
    fTextDocument: TTextDocumentItem;
  published
    // The document that was opened.
    property textDocument: TTextDocumentItem read fTextDocument write fTextDocument;
  end;

  { TDidOpenTextDocument }

  TDidOpenTextDocument = class(specialize TLSPNotification<TDidOpenTextDocumentParams>)
    procedure Process(var Params : TDidOpenTextDocumentParams); override;
  end;

  { TTextDocumentContentChangeEvent }

  // An event describing a change to a text document. If range and
  // rangeLength are omitted the new text is considered to be the full
  // content of the document.
  TTextDocumentContentChangeEvent = class(TCollectionItem)
  private
    fText: string;
  published
    // The new text of the whole document.
    property text: string read fText write fText;
  end;

  { TDidChangeTextDocumentParams }

  TDidChangeTextDocumentParams = class(TPersistent)
  private
    fTextDocument: TVersionedTextDocumentIdentifier;
    fContentChanges: TCollection;
  public
    constructor Create;
  published
    // The document that did change. The version number points to the
    // version after all provided content changes have been applied.
    property textDocument: TVersionedTextDocumentIdentifier read fTextDocument write fTextDocument;
    // The actual content changes. The content changes describe single
    // state changes to the document. So if there are two content
    // changes c1 (at array index 0) and c2 (at array index 1) for a
    // document in state S then c1 moves the document from S to S' and
    // c2 from S' to S''. So c1 is computed on the state S and c2 is
    // computed on the state S'.
    //
    // To mirror the content of a document using change events use the
    // following approach:
    // - start with the same initial content
    // - apply the 'textDocument/didChange' notifications in the order
    //   you recevie them.
    // - apply the `TextDocumentContentChangeEvent`s in a single
    //   notification in the order you receive them.
    property contentChanges: TCollection read fContentChanges write fContentChanges;
  end;

  { TDidChangeTextDocument }

  TDidChangeTextDocument = class(specialize TLSPNotification<TDidChangeTextDocumentParams>)
    procedure Process(var Params : TDidChangeTextDocumentParams); override;
  end;

implementation

{ TDidChangeTextDocumentParams }

constructor TDidChangeTextDocumentParams.Create;
begin
  contentChanges := TCollection.Create(TTextDocumentContentChangeEvent);
end;

{ TDidOpenTextDocument }

procedure TDidOpenTextDocument.Process(var Params : TDidOpenTextDocumentParams);
var
  URI: TURI;
  Code: TCodeBuffer;
begin with Params do
  begin
    URI := ParseURI(textDocument.uri);
    Code := CodeToolBoss.LoadFile(URI.Path + URI.Document, false, false);
    Code.Source := textDocument.text;
  end;
end;

{ TDidChangeTextDocument }

procedure TDidChangeTextDocument.Process(var Params : TDidChangeTextDocumentParams);
var
  URI: TURI;
  Code: TCodeBuffer;
  Change: TCollectionItem;
begin with Params do
  begin
    URI := ParseURI(textDocument.uri);
    Code := CodeToolBoss.FindFile(URI.Path + URI.Document);
    for Change in contentChanges do
    begin
      Code.Source := TTextDocumentContentChangeEvent(Change).text;
    end;
  end;
end;

initialization
  LSPHandlerManager.RegisterHandler('textDocument/didOpen', TDidOpenTextDocument);
  LSPHandlerManager.RegisterHandler('textDocument/didChange', TDidChangeTextDocument);
end.
