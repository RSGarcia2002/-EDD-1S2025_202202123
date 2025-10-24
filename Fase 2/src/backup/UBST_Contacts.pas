unit UBST_Contacts;

{$mode objfpc}{$H+}

interface

uses SysUtils, Classes;

type
  TContacto = record
    Nombre   : string;
    Email    : string;
    Telefono : string;
  end;

  PBSNode = ^TBSNode;
  TBSNode = record
    Key   : string;   // Email como clave
    Data  : TContacto;
    Left  : PBSNode;
    Right : PBSNode;
  end;
type
  TVisitContacto = procedure(const C: TContacto) of object;  // ← para recorridos


function BST_SearchByEmail(Root: PBSNode; const Email: string; out C: TContacto): Boolean;
function BST_DeleteByEmail(var Root: PBSNode; const Email: string): Boolean;
procedure BST_InOrder(Root: PBSNode; Visit: TVisitContacto);
procedure BST_Init(var Root: PBSNode);
procedure BST_Insert(var Root: PBSNode; const C: TContacto);
function  BST_FindEmail(Root: PBSNode; const Email: string; out C: TContacto): boolean;
procedure BST_ToDot(Root: PBSNode; const FilePath: string);
procedure BST_Dispose(var Root: PBSNode);

implementation

procedure BST_Init(var Root: PBSNode);
begin
  Root := nil;
end;

procedure BST_Insert(var Root: PBSNode; const C: TContacto);
var
  cur, parent, node: PBSNode;
  cmp: integer;
begin
  New(node);
  node^.Key := C.Email;
  node^.Data := C;
  node^.Left := nil; node^.Right := nil;

  if Root = nil then begin
    Root := node; exit;
  end;

  cur := Root; parent := nil;
  while cur <> nil do
  begin
    parent := cur;
    cmp := CompareText(node^.Key, cur^.Key);
    if cmp < 0 then cur := cur^.Left
    else cur := cur^.Right;
  end;

  if CompareText(node^.Key, parent^.Key) < 0 then parent^.Left := node
  else parent^.Right := node;
end;

function BST_FindEmail(Root: PBSNode; const Email: string; out C: TContacto): boolean;
var cur: PBSNode; cmp: integer;
begin
  Result := False; cur := Root;
  while cur <> nil do
  begin
    cmp := CompareText(Email, cur^.Key);
    if cmp = 0 then begin C := cur^.Data; exit(True); end;
    if cmp < 0 then cur := cur^.Left else cur := cur^.Right;
  end;
end;

procedure _Dot(Root: PBSNode; var SL: TStringList);
begin
  if Root = nil then exit;
  // Etiqueta
  SL.Add(Format('  "%s" [label="%s"];', [Root^.Key, Root^.Data.Nombre+' | '+Root^.Key]));
  if Root^.Left <> nil then
  begin
    SL.Add(Format('  "%s" -> "%s";', [Root^.Key, Root^.Left^.Key]));
    _Dot(Root^.Left, SL);
  end;
  if Root^.Right <> nil then
  begin
    SL.Add(Format('  "%s" -> "%s";', [Root^.Key, Root^.Right^.Key]));
    _Dot(Root^.Right, SL);
  end;
end;

procedure BST_ToDot(Root: PBSNode; const FilePath: string);
var SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add('digraph BST_Contactos {');
    SL.Add('  node [shape=record, style=filled, fillcolor="#E3F2FD"];');
    SL.Add('  rankdir=TB;');
    if Root=nil then
      SL.Add('  empty [label="(sin contactos)"];')
    else
      _Dot(Root, SL);
    SL.Add('}');
    SL.SaveToFile(FilePath);
  finally
    SL.Free;
  end;
end;


function BST_SearchByEmail(Root: PBSNode; const Email: string; out C: TContacto): Boolean;
var cur: PBSNode; cmp: Integer;
begin
  Result := False; cur := Root;
  while cur<>nil do
  begin
    cmp := CompareText(Email, cur^.Data.Email);
    if cmp=0 then begin C := cur^.Data; Exit(True); end
    else if cmp<0 then cur := cur^.L else cur := cur^.R;
  end;
end;
procedure BST_InOrder(Root: PBSNode; Visit: TVisitContacto);
begin
  if Root=nil then Exit;
  BST_InOrder(Root^.Left, Visit);
  if Assigned(Visit) then Visit(Root^.Data);
  BST_InOrder(Root^.Right, Visit);
end;

function BST_DeleteByEmail(var Root: PBSNode; const Email: string): Boolean;

  function MinNode(N: PBSNode): PBSNode; inline;
  begin
    while (N<>nil) and (N^.Left<>nil) do N := N^.Left;
    Result := N;
  end;

  function _Del(var N: PBSNode; const Key: string): Boolean;
  var cmp: Integer; tmp, m: PBSNode;
  begin
    if N=nil then Exit(False);
    cmp := CompareText(Key, N^.Data.Email);
    if cmp<0 then Result := _Del(N^.Left, Key)
    else if cmp>0 then Result := _Del(N^.Right, Key)
    else
    begin
      if (N^.Left=nil) or (N^.Right=nil) then
      begin
        tmp := N^.Left; if tmp=nil then tmp := N^.Right;
        if tmp=nil then begin Dispose(N); N:=nil; end
        else begin
          N^.Data := tmp^.Data; N^.Left := tmp^.Left; N^.Right := tmp^.Right; Dispose(tmp);
        end;
      end
      else
      begin
        m := MinNode(N^.Right);
        N^.Data := m^.Data;
        _Del(N^.Right, m^.Data.Email);
      end;
      Exit(True);
    end;
  end;

begin
  Result := _Del(Root, Email);
end;



procedure _Free(var N: PBSNode);
begin
  if N=nil then exit;
  _Free(N^.Left); _Free(N^.Right);
  Dispose(N); N:=nil;
end;

procedure BST_Dispose(var Root: PBSNode);
begin
  _Free(Root);
end;

end.
