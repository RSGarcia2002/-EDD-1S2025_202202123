unit UBTree_Favoritos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // Registro del favorito (usado tanto en lista como en reporte)
  TMailFav = record
    Id          : Int64;
    Remitente   : string;
    Destinatario: string;
    Asunto      : string;
    Mensaje     : string;
    Fecha       : string;
  end;

  // Callback para recorridos
  TVisitMailFav = procedure (const M: TMailFav) of object;

  // Nodo (bloque) — simulación de B-Tree
  PBNode = ^TBNode;
  TBNode = record
    Count : Integer;               // cuántos elementos válidos (1..4)
    Keys  : array[1..4] of TMailFav; // cada clave guarda su mensaje completo
    Next  : PBNode;                // siguiente bloque
  end;

procedure BTree_Init(var Head: PBNode);
procedure BTree_Insert_WIP(var Head: PBNode; const M: TMailFav);
procedure BTree_ToDot_WIP(Head: PBNode; const FilePath: string);
procedure BTree_ForEach_WIP(Root: PBNode; Visit: TVisitMailFav);
procedure BTree_Remove_WIP(var Head: PBNode; const Id: Int64; out Removed: Boolean);

implementation

// ------------------------------------------------------------------
// Inicialización del árbol
// ------------------------------------------------------------------
procedure BTree_Init(var Head: PBNode);
begin
  Head := nil;
end;

// ------------------------------------------------------------------
// Inserta nuevo registro en bloques de máximo 4
// ------------------------------------------------------------------
procedure BTree_Insert_WIP(var Head: PBNode; const M: TMailFav);
var
  n: PBNode;
begin
  if (Head = nil) or (Head^.Count = 4) then
  begin
    New(n);
    n^.Count := 0;
    n^.Next  := Head;
    Head     := n;
  end;

  Inc(Head^.Count);
  Head^.Keys[Head^.Count] := M;
end;

// ------------------------------------------------------------------
// Escapador de caracteres HTML (para Graphviz)
// ------------------------------------------------------------------
function HtmlEsc(const S: string): string;
begin
  Result := StringReplace(S, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;',  [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;',  [rfReplaceAll]);
end;

// ------------------------------------------------------------------
// Genera el archivo .dot con formato gráfico similar al ejemplo
// ------------------------------------------------------------------
procedure BTree_ToDot_WIP(Head: PBNode; const FilePath: string);
var
  SL : TStringList;
  cur: PBNode;
  i, idx: Integer;
  cell, htmlText: string;

  function HtmlEsc(const S: string): string;
  begin
    Result := StringReplace(S, '&', '&amp;', [rfReplaceAll]);
    Result := StringReplace(Result, '<', '&lt;',  [rfReplaceAll]);
    Result := StringReplace(Result, '>', '&gt;',  [rfReplaceAll]);
  end;

begin
  SL := TStringList.Create;
  try
    SL.Add('digraph BTree_Favoritos {');
    SL.Add('  rankdir=TB;');
    SL.Add('  node  [shape=record, style="filled", fillcolor="#A5D6A7", fontname="Helvetica"];');
    SL.Add('  edge  [color="#90A4AE"];');

    if Head = nil then
      SL.Add('  empty [label="(sin favoritos)"];')
    else
    begin
      idx := 0;
      cur := Head;
      while cur <> nil do
      begin
        Inc(idx);
        cell := '';

        for i := 1 to cur^.Count do
        begin
          if i > 1 then
            cell := cell + '|';

          with cur^.Keys[i] do
          begin
            htmlText :=
              '< <B>ID:</B> ' + IntToStr(Id) + '\l' +
              '<B>De:</B> ' + HtmlEsc(Remitente) + '\l' +
              '<B>Para:</B> ' + HtmlEsc(Destinatario) + '\l' +
              '<B>Asunto:</B> ' + HtmlEsc(Asunto) + '\l' +
              '<B>Fecha:</B> ' + HtmlEsc(Fecha) + '\l' +
              '<B>Mensaje:</B> ' + HtmlEsc(Mensaje) + '\l';

            cell := cell + '{' + htmlText + '}';
          end;
        end;

        // Crea el bloque con el contenido formateado
        SL.Add(Format('  blk%d [label="{%s}"];', [idx, cell]));

        // Enlaza bloques
        if cur^.Next <> nil then
          SL.Add(Format('  blk%d -> blk%d;', [idx, idx + 1]));

        cur := cur^.Next;
      end;
    end;

    SL.Add('}');
    ForceDirectories(ExtractFilePath(FilePath));
    SL.SaveToFile(FilePath);
  finally
    SL.Free;
  end;
end;

// ------------------------------------------------------------------
// Recorre todos los nodos del árbol
// ------------------------------------------------------------------
procedure BTree_ForEach_WIP(Root: PBNode; Visit: TVisitMailFav);
var
  cur: PBNode;
  i  : Integer;
begin
  if not Assigned(Visit) then Exit;
  cur := Root;
  while cur <> nil do
  begin
    for i := 1 to cur^.Count do
      Visit(cur^.Keys[i]);
    cur := cur^.Next;
  end;
end;

// ------------------------------------------------------------------
// Elimina un correo favorito por ID
// ------------------------------------------------------------------
procedure BTree_Remove_WIP(var Head: PBNode; const Id: Int64; out Removed: Boolean);
var
  cur, prev: PBNode;
  i, k: Integer;
begin
  Removed := False;
  prev := nil;
  cur  := Head;

  while cur <> nil do
  begin
    k := 0;
    for i := 1 to cur^.Count do
      if cur^.Keys[i].Id = Id then
      begin
        k := i;
        Break;
      end;

    if k > 0 then
    begin
      // Compactar las claves del bloque
      for i := k to cur^.Count - 1 do
        cur^.Keys[i] := cur^.Keys[i + 1];
      Dec(cur^.Count);
      Removed := True;

      // Eliminar bloque vacío
      if cur^.Count = 0 then
      begin
        if prev = nil then
          Head := cur^.Next
        else
          prev^.Next := cur^.Next;
        Dispose(cur);
      end;
      Exit;
    end;

    prev := cur;
    cur  := cur^.Next;
  end;
end;

end.

