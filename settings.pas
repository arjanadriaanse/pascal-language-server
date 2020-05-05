// Pascal Language Server
// Copyright 2020 Ryan Joseph

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

unit settings;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$scopedenums on}

interface

type
  TServerOption = (
      InsertCompletionsAsSnippets,        // Procedure completions with parameters are inserted as snippets
      InsertCompletionProcedureBrackets   // Procedure completions with parameters (non-snippet) insert
                                          // empty brackets (and insert as snippet)
    );
  TServerOptions = set of TServerOption;
  
  TServerSettings = record
    Options: TServerOptions;
  end;

var
  ServerSettings: TServerSettings;

implementation

end.