unit UAVL_Borradores;

{$mode objfpc}{$H+}

interface
uses Classes, SysUtils;


type
  TDraft = record
    Id: LongInt;
    Remitente, Destinatario, Asunto, Mensaje: string;
  end;

  PAVL = ^TAVL;
  TAVL = record
    Key : LongInt;
    Data: TDraft;
    H   : Integer;
    L, R: PAVL;
  end;

procedure AVL_Init(var Root: PAVL);
procedure AVL_Insert_WIP(var Root: PAVL; const D: TDraft); // TODO: reemplazar por AVL real
procedure AVL_ToDot(Root: PAVL; const FilePath: string);

implementation

procedure AVL_Init(var Root: PAVL); begin Root := nil; end;

procedure AVL_Insert_WIP(var Root: PAVL; const D: TDraft);
var
  n: PAVL;
begin
  New(n);
  n^.Key := D.Id;
  n^.Data := D;
  n^.H := 1;
  n^.L := nil;
  n^.R := nil;

  if Root = nil then
    Root := n
  else
  begin
    n^.R := Root;
    Root := n;
  end;
end;


procedure AVL_ToDot(Root: PAVL; const FilePath: string);
var SL: TStringList;
  procedure walk(N: PAVL);
  begin
    if N=nil then exit;
    SL.Add(Format('  "%d" [label="ID=%d|%s"];',[N^.Key,N^.Key,N^.Data.Asunto]));
    if N^.L<>nil then SL.Add(Format('  "%d" -> "%d";',[N^.Key,N^.L^.Key]));
    if N^.R<>nil then SL.Add(Format('  "%d" -> "%d";',[N^.Key,N^.R^.Key]));
    walk(N^.L); walk(N^.R);
  end;
begin
  SL := TStringList.Create;
  try
    SL.Add('digraph AVL_Borradores { node [shape=record, fillcolor="#E8F5E9", style=filled];');
    if Root=nil then SL.Add('  empty [label="(sin borradores)"];') else walk(Root);
    SL.Add('}');
    SL.SaveToFile(FilePath);
  finally
    SL.Free;
  end;
end;

end.
