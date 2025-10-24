unit UBST_Communities;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TComunidad = record
    Nombre       : string;
    FechaCreacion: TDateTime;
    NumMensajes  : Integer;
  end;

  PMsg = ^TMsg;
  TMsg = record
    Correo   : string;
    Texto    : string;
    FechaPub : string; // 'yyyy-mm-dd hh:nn'
    Next     : PMsg;
  end;

  PCBST = ^TCBST;
  TCBST = record
    Key     : string;   // Nombre de la comunidad (clave BST)
    Data    : TComunidad;
    MsgHead : PMsg;     // lista simple (push al inicio = más reciente primero)
    L, R    : PCBST;
  end;

{ ===== API ===== }

procedure CBST_Init(var Root: PCBST);
function  CBST_Find(Root: PCBST; const Nombre: string): PCBST;
function  CBST_Insert(var Root: PCBST; const Nombre: string): PCBST;

// Por compatibilidad con tu código; delega a CBST_Insert
procedure CBST_Insert_WIP(var Root: PCBST; const Nombre: string);

// Mensajes
procedure CBST_AddMessage(Root: PCBST; const Community, AutorEmail, Texto, Fecha: string; out Ok: Boolean);
procedure CBST_MessagesToStrings(Root: PCBST; const Community: string; L: TStrings);

// Reporte DOT
procedure CBST_ToDot(Root: PCBST; const FilePath: string);

// (Helpers opcionales)
function  CBST_MessageCount(C: PCBST): Integer;

implementation

{ ===== Helpers internos ===== }

procedure CBST_Init(var Root: PCBST);
begin
  Root := nil;
end;

function CBST_Find(Root: PCBST; const Nombre: string): PCBST;
var
  cmp: Integer;
begin
  while Root <> nil do
  begin
    cmp := AnsiCompareText(Nombre, Root^.Key);
    if cmp = 0 then Exit(Root)
    else if cmp < 0 then Root := Root^.L
    else Root := Root^.R;
  end;
  Result := nil;
end;

function CBST_Insert(var Root: PCBST; const Nombre: string): PCBST;
var
  cur, parent: PCBST;
  cmp: Integer;
begin
  if Root = nil then
  begin
    New(Root);
    FillChar(Root^, SizeOf(Root^), 0);
    Root^.Key                := Nombre;
    Root^.Data.Nombre        := Nombre;
    Root^.Data.FechaCreacion := Now;
    Root^.Data.NumMensajes   := 0;
    Result := Root;
    Exit;
  end;

  cur := Root; parent := nil;
  while cur <> nil do
  begin
    parent := cur;
    cmp := AnsiCompareText(Nombre, cur^.Key);
    if cmp = 0 then
    begin
      Result := cur; // ya existe
      Exit;
    end
    else if cmp < 0 then
      cur := cur^.L
    else
      cur := cur^.R;
  end;

  New(cur);
  FillChar(cur^, SizeOf(cur^), 0);
  cur^.Key                := Nombre;
  cur^.Data.Nombre        := Nombre;
  cur^.Data.FechaCreacion := Now;
  cur^.Data.NumMensajes   := 0;

  if AnsiCompareText(Nombre, parent^.Key) < 0 then
    parent^.L := cur
  else
    parent^.R := cur;

  Result := cur;
end;

procedure CBST_Insert_WIP(var Root: PCBST; const Nombre: string);
begin
  CBST_Insert(Root, Nombre);
end;

function CBST_MessageCount(C: PCBST): Integer;
var p: PMsg;
begin
  Result := 0;
  if C = nil then Exit;
  p := C^.MsgHead;
  while p <> nil do
  begin
    Inc(Result);
    p := p^.Next;
  end;
end;

{ ===== Mensajes ===== }

procedure CBST_AddMessage(Root: PCBST; const Community, AutorEmail, Texto, Fecha: string; out Ok: Boolean);
var
  C: PCBST;
  n: PMsg;
begin
  Ok := False;
  if (Root = nil) or (Community = '') or (Texto = '') then Exit;

  C := CBST_Find(Root, Community);
  if C = nil then Exit;

  New(n);
  n^.Correo   := AutorEmail;
  n^.Texto    := Texto;
  n^.FechaPub := Fecha;
  n^.Next     := C^.MsgHead;     // push (más reciente primero)
  C^.MsgHead  := n;

  Inc(C^.Data.NumMensajes);
  Ok := True;
end;

procedure CBST_MessagesToStrings(Root: PCBST; const Community: string; L: TStrings);
var
  C: PCBST;
  p: PMsg;
  i: Integer;
begin
  if L = nil then Exit;
  L.Clear;

  C := CBST_Find(Root, Community);
  if C = nil then Exit;

  i := 0; p := C^.MsgHead;
  while p <> nil do
  begin
    Inc(i);
    L.Add(Format('%d) [%s] %s: %s', [i, p^.FechaPub, p^.Correo, p^.Texto]));
    p := p^.Next;
  end;
end;

{ ===== DOT ===== }

procedure CBST_ToDot(Root: PCBST; const FilePath: string);

  function Esc(const S: string): string;
  begin
    Result := StringReplace(S, '"', '\"', [rfReplaceAll]);
  end;

  procedure EmitNode(SL: TStrings; C: PCBST);
  var
    labelTxt: string;
  begin
    labelTxt := Format('%s|creada: %s|mensajes: %d',
                       [C^.Data.Nombre,
                        FormatDateTime('yyyy-mm-dd hh:nn', C^.Data.FechaCreacion),
                        C^.Data.NumMensajes]);
    SL.Add(Format('  "%s" [label="{%s}"];',
      [Esc(C^.Key), Esc(labelTxt)]));
  end;

  procedure Walk(SL: TStrings; C: PCBST);
  begin
    if C = nil then Exit;
    // in-order para mejor orden en la vista
    Walk(SL, C^.L);
    EmitNode(SL, C);
    if C^.L <> nil then
      SL.Add(Format('  "%s" -> "%s";', [Esc(C^.Key), Esc(C^.L^.Key)]));
    if C^.R <> nil then
      SL.Add(Format('  "%s" -> "%s";', [Esc(C^.Key), Esc(C^.R^.Key)]));
    Walk(SL, C^.R);
  end;

var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add('digraph BST_Comunidades {');
    SL.Add('  node [shape=record, fillcolor="#E3F2FD", style=filled];');
    SL.Add('  rankdir=TB;');
    if Root = nil then
      SL.Add('  empty [label="(sin comunidades)"];')
    else
      Walk(SL, Root);
    SL.Add('}');
    SL.SaveToFile(FilePath);
  finally
    SL.Free;
  end;
end;

end.

