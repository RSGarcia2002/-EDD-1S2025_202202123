unit UDataCore;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, DateUtils, fpjson, jsonparser,  UDomain, UAVL_Borradores, UBTree_Favoritos;

{======================  USUARIOS: lista simple  ======================}

type
  PUserNode = ^TUserNode;
  TUserNode = record
    Id        : string;
    Nombre    : string;
    Usuario   : string;
    Email     : string;
    Telefono  : string;
    Password  : string;
    Next      : PUserNode;
    DemoSeeded: boolean;
  end;

var
  UsersHead: PUserNode = nil;

procedure UserList_Clear;
procedure UserList_AddOrUpdate(const AId, ANombre, AUsuario, AEmail, ATel, APass: string);
function  User_ValidarLogin(const AEmail, APass: string;
  out Nombre, Usuario, Tel: string): boolean;
function  User_FindByEmail(const AEmail: string): PUserNode;
procedure EnsureDemoInboxFor(const Email: string);
function User_ExistsEmail(const AEmail: string): boolean;
function User_ExistsUsuario(const AUsuario: string): boolean;

{======================  BANDEJA: lista doble  =======================}

type
  PMailNode = ^TMailNode;
  TMailNode = record
    Id          : string;
    Remitente   : string;
    Destinatario: string;
    Asunto      : string;
    Fecha       : string;
    Mensaje     : string;
    Estado      : string;  // 'NL' no leído, 'L' leído
    Prev, Next  : PMailNode;
  end;

var
  InboxHead: PMailNode = nil;
  InboxTail: PMailNode = nil;

procedure Inbox_Clear;
procedure Inbox_PushBack(const AId, ARem, ADest, AAsunto, AFecha, AMensaje: string);
function  Inbox_Count: Integer;
procedure Inbox_ToStrings(const L: TStrings);
function  Inbox_RemoveAt(AIndex: Integer; out AId, ARem, ADest, AAsunto, AFecha, AMsg, AEstado: string): boolean;
// Bandeja: helpers filtrados por usuario
procedure Inbox_ToStringsFor(const L: TStrings; const Email: string);
function  Inbox_GetNthFor(const Email: string; AIndex: Integer): PMailNode; // 1-based
function  Inbox_RemoveNode(Node: PMailNode; out AId, ARem, ADest, AAsunto, AFecha, AMsg, AEstado: string): boolean;
function  Inbox_CountFor(const Email: string): Integer;
function Inbox_CountReadFor(const Destinatario: string): Integer;
function Inbox_LoadFromFile(const AFile: string; out Loaded, Skipped: Integer): Boolean;
{======================  PAPELERA: pila  =======================}

type
  PTrashNode = ^TTrashNode;
  TTrashNode = record
    Id, Remitente, Destinatario, Asunto, Fecha, Mensaje, Estado: string;
    Next: PTrashNode;
  end;

var
  TrashTop: PTrashNode = nil;

procedure Trash_Clear;
procedure Trash_Push(const AId, ARem, ADest, AAsunto, AFecha, AMensaje, AEstado: string);
function  Trash_Pop(out AId, ARem, ADest, AAsunto, AFecha, AMensaje, AEstado: string): boolean;
function  Trash_Count: Integer;
procedure Trash_ToStrings(const L: TStrings);
function  Trash_RemoveAt(AIndex: Integer): boolean; // 1-based
procedure Trash_ToStringsFiltered(const L: TStrings; const Filter: string);


{======================  CONTACTOS: lista circular =======================}

type
  PContactNode = ^TContactNode;
  TContactNode = record
    Email  : string;
    Nombre : string;
    Owner  : string;
    Next   : PContactNode; // lista circular
  end;

var
  ContactsTail: PContactNode = nil; // sigue siendo una sola lista circular global
type
  TAddLineProc = procedure(const S: string) of object;
procedure Contacts_Clear;
function  Contacts_ExistsFor(const Owner, AEmail: string): boolean;
function  Contacts_AddFor(const Owner, AEmail, ANombre: string): boolean; // False si ya existe
procedure Contacts_ToStringsFor(const Owner: string; const L: TStrings);
function Contacts_RemoveFor(const ownerEmail, contactEmail: string): Boolean;
procedure Contacts_ForEachLine(const ownerEmail: string; const AddLine: TAddLineProc);




{======================  PROGRAMADOS (cola)  ======================}

type
  PProgNode = ^TProgNode;
  TProgNode = record
    Id, Remitente, Destinatario, Asunto, FechaProg, Mensaje: string;
    Next: PProgNode;
  end;

var
  ProgHead: PProgNode = nil;
  ProgTail: PProgNode = nil;

procedure Prog_Clear;
procedure Prog_Enqueue(const AId, ARem, ADest, AAsunto, AFechaProg, AMensaje: string);
function  Prog_Dequeue(out AId, ARem, ADest, AAsunto, AFechaProg, AMensaje: string): boolean;
function  Prog_Count: Integer;
procedure Prog_ToStrings(const L: TStrings);
procedure Prog_RemoveAt(AIndex: Integer); // 1-based
procedure Prog_SendAllNow; // pasa todos a la Bandeja


{======================  REPORTES (DOT)  ======================}

// Devuelve la carpeta de reportes: {usuario}-Reportes (la crea si no existe)
function UserReportsDir: string;

// Generan los archivos DOT y devuelven el path del .dot creado
function Report_Inbox_DOT: string;
function Report_Trash_DOT: string;
function Report_Programados_DOT: string;
function Report_Contactos_DOT: string;
function Report_Borradores_DOT: string;
function Report_Favoritos_DOT: string;

procedure Relations_GenerateDot(const OutFile: string);
function RootReportsDir: string;
procedure Users_GenerateDot(const OutFile: string);

implementation
 // ---- Fallbacks seguros ----
function ParseDateDef(const S: string; Def: TDateTime): TDateTime;
var
  FS: TFormatSettings;
begin
  // Esperamos "yyyy-mm-dd hh:nn" (si no, usamos Now)
  FS := DefaultFormatSettings;
  FS.DateSeparator := '-';
  FS.TimeSeparator := ':';
  FS.ShortDateFormat := 'yyyy-mm-dd';
  FS.LongTimeFormat  := 'hh:nn';
  if not TryStrToDateTime(S, Result, FS) then
    Result := Def;
end;

// Split CSV seguro (usa SplitString si existe; si no, fallback)
function SplitCSVLine(const L: string): TStringArray;
var
  p, startPos: SizeInt;
  tmp: array of string;
  s: string;
begin
  if @SplitString <> nil then
    Exit(SplitString(L, ','));

  SetLength(tmp, 0);
  startPos := 1;
  for p := 1 to Length(L) do
    if L[p] = ',' then
    begin
      s := Copy(L, startPos, p - startPos);
      SetLength(tmp, Length(tmp)+1);
      tmp[High(tmp)] := s;
      startPos := p + 1;
    end;
  s := Copy(L, startPos, MaxInt);
  SetLength(tmp, Length(tmp)+1);
  tmp[High(tmp)] := s;
  Result := tmp;
end;

// Lee texto tolerante a claves en mayúsculas/minúsculas
function JGetStrDef(d: TJSONData; const Keys: array of string; const Def: string): string;
var
  k: string; n: TJSONData;
begin
  for k in Keys do
  begin
    n := d.FindPath(k);
    if (n = nil) and (Length(k) > 0) then
      n := d.FindPath(UpperCase(k[1]) + Copy(k,2,MaxInt));
    if (n <> nil) and (n.JSONType in [jtString, jtNumber, jtBoolean]) then
      Exit(n.AsString);
  end;
  Result := Def;
end;

function JGetArray(d: TJSONData; const Keys: array of string): TJSONData;
var
  k: string; n: TJSONData;
begin
  for k in Keys do
  begin
    n := d.FindPath(k);
    if (n <> nil) and (n.JSONType = jtArray) then Exit(n);
  end;
  Result := nil;
end;


// Id auto si viene vacío
function EnsureId(const S: string): string;
begin
  if Trim(S) <> '' then Exit(S);
  Result := FormatDateTime('yyyymmddhhnnsszzz', Now);
end;
{======================  USUARIOS  ======================}

procedure UserList_Clear;
var
  cur, nx: PUserNode;
begin
  cur := UsersHead;
  while cur <> nil do
  begin
    nx := cur^.Next;
    Dispose(cur);
    cur := nx;
  end;
  UsersHead := nil;
end;

procedure UserList_AddOrUpdate(const AId, ANombre, AUsuario, AEmail, ATel, APass: string);
var
  cur: PUserNode;
begin
  cur := UsersHead;
  while cur <> nil do
  begin
    if cur^.Id = AId then
    begin
      cur^.Nombre   := ANombre;
      cur^.Usuario  := AUsuario;
      cur^.Email    := AEmail;
      cur^.Telefono := ATel;
      cur^.Password := APass;
      cur^.DemoSeeded := False;
      Exit;
    end;
    cur := cur^.Next;
  end;

  New(cur);
  cur^.Id        := AId;
  cur^.Nombre    := ANombre;
  cur^.Usuario   := AUsuario;
  cur^.Email     := AEmail;
  cur^.Telefono  := ATel;
  cur^.Password  := APass;
  cur^.Next      := UsersHead;
  UsersHead      := cur;
end;

function User_ValidarLogin(const AEmail, APass: string;
  out Nombre, Usuario, Tel: string): boolean;
var
  cur: PUserNode;
begin
  Result  := False;
  Nombre  := '';
  Usuario := '';
  Tel     := '';
  cur := UsersHead;
  while cur <> nil do
  begin
    if SameText(cur^.Email, AEmail) and (cur^.Password = APass) then
    begin
      Nombre  := cur^.Nombre;
      Usuario := cur^.Usuario;
      Tel     := cur^.Telefono;
      Exit(True);
    end;
    cur := cur^.Next;
  end;
end;

function User_FindByEmail(const AEmail: string): PUserNode;
var
  cur: PUserNode;
begin
  Result := nil;
  cur := UsersHead;
  while cur <> nil do
  begin
    if SameText(cur^.Email, AEmail) then
      Exit(cur);
    cur := cur^.Next;
  end;
end;
function User_FindByUsuario(const AUsuario: string): PUserNode;
var
  cur: PUserNode;
begin
  Result := nil;
  cur := UsersHead;
  while cur <> nil do
  begin
    if SameText(cur^.Usuario, AUsuario) then
      Exit(cur);
    cur := cur^.Next;
  end;
end;

function User_ExistsEmail(const AEmail: string): boolean;
begin
  Result := (User_FindByEmail(AEmail) <> nil);
end;

function User_ExistsUsuario(const AUsuario: string): boolean;
begin
  Result := (User_FindByUsuario(AUsuario) <> nil);
end;


{======================  BANDEJA  ======================}

procedure Inbox_Clear;
var
  cur, nx: PMailNode;
begin
  cur := InboxHead;
  while cur <> nil do
  begin
    nx := cur^.Next;
    Dispose(cur);
    cur := nx;
  end;
  InboxHead := nil;
  InboxTail := nil;
end;

procedure Inbox_PushBack(const AId, ARem, ADest, AAsunto, AFecha, AMensaje: string);
var
  N: PMailNode;
begin
  New(N);
  N^.Id           := AId;
  N^.Remitente    := ARem;
  N^.Destinatario := ADest;
  N^.Asunto       := AAsunto;
  N^.Fecha        := AFecha;
  N^.Mensaje      := AMensaje;
  N^.Estado       := 'NL';
  N^.Prev         := InboxTail;
  N^.Next         := nil;

  if InboxTail <> nil then
    InboxTail^.Next := N
  else
    InboxHead := N;

  InboxTail := N;
end;

function Inbox_Count: Integer;
var
  cur: PMailNode;
begin
  Result := 0;
  cur := InboxHead;
  while cur <> nil do
  begin
    Inc(Result);
    cur := cur^.Next;
  end;
end;

procedure Inbox_ToStrings(const L: TStrings);
var
  cur: PMailNode;
  i: Integer;
begin
  L.Clear;
  i := 1;
  cur := InboxHead;
  while cur <> nil do
  begin
    L.Add(Format('%d) [%s] %s - %s (%s)', [
      i, cur^.Estado, cur^.Remitente, cur^.Asunto, cur^.Fecha]));
    Inc(i);
    cur := cur^.Next;
  end;
end;

function Inbox_RemoveAt(AIndex: Integer; out AId, ARem, ADest, AAsunto, AFecha, AMsg, AEstado: string): boolean;
var
  cur: PMailNode;
  i  : Integer;
begin
  Result := False;
  AId:=''; ARem:=''; ADest:=''; AAsunto:=''; AFecha:=''; AMsg:=''; AEstado:='';

  if AIndex <= 0 then Exit;

  cur := InboxHead;
  i := 1;
  while (cur <> nil) and (i < AIndex) do
  begin
    cur := cur^.Next;
    Inc(i);
  end;

  if cur = nil then Exit;

  AId     := cur^.Id;
  ARem    := cur^.Remitente;
  ADest   := cur^.Destinatario;
  AAsunto := cur^.Asunto;
  AFecha  := cur^.Fecha;
  AMsg    := cur^.Mensaje;
  AEstado := cur^.Estado;

  if cur^.Prev <> nil then
    cur^.Prev^.Next := cur^.Next
  else
    InboxHead := cur^.Next;

  if cur^.Next <> nil then
    cur^.Next^.Prev := cur^.Prev
  else
    InboxTail := cur^.Prev;

  Dispose(cur);
  Result := True;
end;

{======================  PAPELERA (PILA)  ======================}

procedure Trash_Clear;
var
  n, nx: PTrashNode;
begin
  n := TrashTop;
  while n <> nil do
  begin
    nx := n^.Next;
    Dispose(n);
    n := nx;
  end;
  TrashTop := nil;
end;

procedure Trash_Push(const AId, ARem, ADest, AAsunto, AFecha, AMensaje, AEstado: string);
var
  n: PTrashNode;
begin
  New(n);
  n^.Id := AId;
  n^.Remitente := ARem;
  n^.Destinatario := ADest;
  n^.Asunto := AAsunto;
  n^.Fecha := AFecha;
  n^.Mensaje := AMensaje;
  n^.Estado := AEstado;
  n^.Next := TrashTop;
  TrashTop := n;
end;

function Trash_Pop(out AId, ARem, ADest, AAsunto, AFecha, AMensaje, AEstado: string): boolean;
var
  n: PTrashNode;
begin
  Result := False;
  if TrashTop = nil then Exit;

  n := TrashTop;
  AId := n^.Id;
  ARem := n^.Remitente;
  ADest := n^.Destinatario;
  AAsunto := n^.Asunto;
  AFecha := n^.Fecha;
  AMensaje := n^.Mensaje;
  AEstado := n^.Estado;

  TrashTop := n^.Next;
  Dispose(n);
  Result := True;
end;

function Trash_Count: Integer;
var
  n: PTrashNode;
begin
  Result := 0;
  n := TrashTop;
  while n <> nil do
  begin
    Inc(Result);
    n := n^.Next;
  end;
end;

procedure Trash_ToStrings(const L: TStrings);
var
  n: PTrashNode;
  i: Integer;
begin
  L.Clear;
  i := 1;
  n := TrashTop;
  while n <> nil do
  begin
    L.Add(Format('%d) [TOP] %s - %s (%s)', [
      i, n^.Remitente, n^.Asunto, n^.Fecha]));
    Inc(i);
    n := n^.Next;
  end;
  if i=1 then
    L.Add('(Papelera vacía)');
end;
function Trash_RemoveAt(AIndex: Integer): boolean;
var
  cur, prev: PTrashNode;
  i: Integer;
begin
  Result := False;
  if (AIndex <= 0) or (TrashTop = nil) then Exit;

  // como es pila, recorremos desde TOP hasta índice
  prev := nil;
  cur := TrashTop;
  i := 1;
  while (cur <> nil) and (i < AIndex) do
  begin
    prev := cur;
    cur := cur^.Next;
    Inc(i);
  end;

  if cur = nil then Exit; // fuera de rango

  if prev = nil then
    TrashTop := cur^.Next     // eliminando el TOP
  else
    prev^.Next := cur^.Next;  // unlink medio/final

  Dispose(cur);
  Result := True;
end;

procedure Trash_ToStringsFiltered(const L: TStrings; const Filter: string);
var
  n: PTrashNode;
  i: Integer;
  f: string;
begin
  L.Clear;
  f := LowerCase(Trim(Filter));
  i := 1;
  n := TrashTop;
  while n <> nil do
  begin
    if (f='') or (Pos(f, LowerCase(n^.Remitente))>0) or (Pos(f, LowerCase(n^.Asunto))>0) then
    begin
      L.Add(Format('%d) [TOP] %s - %s (%s)', [i, n^.Remitente, n^.Asunto, n^.Fecha]));
    end;
    Inc(i);
    n := n^.Next;
  end;
  if L.Count=0 then
    L.Add('(Sin resultados)');
end;



{======================  CONTACTOS (CIRCULAR)  ======================}
procedure Contacts_Clear;
var
  cur, start: PContactNode;
begin
  if ContactsTail = nil then Exit;
  start := ContactsTail^.Next;
  cur := start;
  repeat
    start := cur^.Next;
    Dispose(cur);
    cur := start;
  until cur = ContactsTail^.Next;
  ContactsTail := nil;
end;

function Contacts_ExistsFor(const Owner, AEmail: string): boolean;
var
  cur, start: PContactNode;
begin
  Result := False;
  if ContactsTail = nil then Exit;
  start := ContactsTail^.Next;
  cur := start;
  repeat
    if SameText(cur^.Owner, Owner) and SameText(cur^.Email, AEmail) then
      Exit(True);
    cur := cur^.Next;
  until cur = start;
end;
function Contacts_AddFor(const Owner, AEmail, ANombre: string): boolean;
var
  n: PContactNode;
begin
  // No duplicar dentro del mismo propietario
  if Contacts_ExistsFor(Owner, AEmail) then Exit(False);

  New(n);
  n^.Email  := AEmail;
  n^.Nombre := ANombre;
  n^.Owner  := Owner;

  if ContactsTail = nil then
  begin
    n^.Next   := n;      // primer nodo apunta a sí mismo
    ContactsTail := n;
  end
  else
  begin
    n^.Next := ContactsTail^.Next; // después de tail (cabeza)
    ContactsTail^.Next := n;
    ContactsTail := n;             // nuevo último
  end;
  Result := True;
end;

procedure Contacts_ToStringsFor(const Owner: string; const L: TStrings);
var
  cur, start: PContactNode;
  idx: Integer;
begin
  L.Clear;
  if ContactsTail = nil then Exit;

  start := ContactsTail^.Next;
  cur := start; idx := 1;
  repeat
    if SameText(cur^.Owner, Owner) then
    begin
      L.Add(Format('%d) %s <%s>', [idx, cur^.Nombre, cur^.Email]));
      Inc(idx);
    end;
    cur := cur^.Next;
  until cur = start;

  if idx = 1 then
    L.Add('(Sin contactos)');
end;
function Contacts_RemoveFor(const ownerEmail, contactEmail: string): Boolean;
var
  prev, cur, start: PContactNode;
begin
  Result := False;
  if ContactsTail = nil then Exit;

  // lista circular: tail->next es la “cabeza”
  prev  := ContactsTail;
  cur   := ContactsTail^.Next;
  start := cur;

  repeat
    if SameText(cur^.Owner, ownerEmail) and SameText(cur^.Email, contactEmail) then
    begin
      // quitar nodo cur
      prev^.Next := cur^.Next;

      // si el que quitamos es el tail
      if cur = ContactsTail then
      begin
        if cur^.Next = cur then
          ContactsTail := nil                // era el único nodo
        else
          ContactsTail := prev;              // prev pasa a ser el nuevo tail
      end;

      Dispose(cur);
      Exit(True);
    end;

    prev := cur;
    cur  := cur^.Next;
  until cur = start;
end;

procedure Contacts_ForEachLine(const ownerEmail: string; const AddLine: TAddLineProc);
var
  cur, start: PContactNode;
begin
  if (ContactsTail = nil) or (not Assigned(AddLine)) then Exit;

  start := ContactsTail^.Next;
  cur   := start;
  repeat
    if SameText(cur^.Owner, ownerEmail) then
      AddLine(Format('%s <%s>', [cur^.Nombre, cur^.Email])); // ajusta campos si difieren
    cur := cur^.Next;
  until cur = start;
end;



{======================  PROGRAMADOS (COLA)  ======================}

procedure Prog_Clear;
var cur, nx: PProgNode;
begin
  cur := ProgHead;
  while cur <> nil do begin nx := cur^.Next; Dispose(cur); cur := nx; end;
  ProgHead := nil; ProgTail := nil;
end;

procedure Prog_Enqueue(const AId, ARem, ADest, AAsunto, AFechaProg, AMensaje: string);
var n: PProgNode;
begin
  New(n);
  n^.Id := AId; n^.Remitente := ARem; n^.Destinatario := ADest;
  n^.Asunto := AAsunto; n^.FechaProg := AFechaProg; n^.Mensaje := AMensaje;
  n^.Next := nil;
  if ProgTail <> nil then ProgTail^.Next := n else ProgHead := n;
  ProgTail := n;
end;

function Prog_Dequeue(out AId, ARem, ADest, AAsunto, AFechaProg, AMensaje: string): boolean;
var n: PProgNode;
begin
  Result := False; if ProgHead = nil then Exit;
  n := ProgHead;
  AId := n^.Id; ARem := n^.Remitente; ADest := n^.Destinatario;
  AAsunto := n^.Asunto; AFechaProg := n^.FechaProg; AMensaje := n^.Mensaje;
  ProgHead := n^.Next; if ProgHead = nil then ProgTail := nil;
  Dispose(n); Result := True;
end;

function Prog_Count: Integer;
var cur: PProgNode; begin Result := 0; cur := ProgHead; while cur <> nil do begin Inc(Result); cur := cur^.Next; end end;

procedure Prog_ToStrings(const L: TStrings);
var cur: PProgNode; i: Integer;
begin
  L.Clear; i := 1; cur := ProgHead;
  while cur <> nil do begin
    L.Add(Format('%d) %s -> %s | %s (%s)', [i, cur^.Remitente, cur^.Destinatario, cur^.Asunto, cur^.FechaProg]));
    Inc(i); cur := cur^.Next;
  end;
  if i = 1 then L.Add('(Sin programados)');
end;

procedure Prog_RemoveAt(AIndex: Integer);
var cur, prev: PProgNode; i: Integer;
begin
  if (AIndex <= 0) or (ProgHead = nil) then Exit;
  prev := nil; cur := ProgHead; i := 1;
  while (cur <> nil) and (i < AIndex) do begin prev := cur; cur := cur^.Next; Inc(i); end;
  if cur = nil then Exit;
  if prev = nil then ProgHead := cur^.Next else prev^.Next := cur^.Next;
  if cur = ProgTail then ProgTail := prev;
  Dispose(cur);
end;

procedure Prog_SendAllNow;
var id,rem,dst,asu,fec,msg: string;
begin
  while Prog_Dequeue(id,rem,dst,asu,fec,msg) do
    Inbox_PushBack(id, rem, dst, asu, fec, msg);
end;
{======================  REPORTES (DOT)  ======================}

function UserReportsDir: string;
var
  base, dir, usuario: string;
  u: PUserNode;
begin
  // Carpeta junto al ejecutable: {usuario}-Reportes
  base := ExtractFilePath(ParamStr(0));
  u := User_FindByEmail(Domain_GetCurrentUser);
  if u <> nil then usuario := u^.Usuario else usuario := 'usuario';
  dir := IncludeTrailingPathDelimiter(base) + usuario + '-Reportes';
  if not DirectoryExists(dir) then
    CreateDir(dir);
  Result := dir;
end;

function Report_Inbox_DOT: string;
var
  sl: TStringList;
  cur: PMailNode;
  idx: Integer;
  outPath: string;
begin
  sl := TStringList.Create;
  try
    sl.Add('digraph Inbox {');
    sl.Add('  rankdir=LR;');
    sl.Add('  node [shape=box, style=rounded, fontsize=10];');

    idx := 1; cur := InboxHead;
    while cur <> nil do
    begin
      sl.Add(Format('  n%d [label="[%s] %s\n%s\n%s"];',
        [idx, cur^.Estado, cur^.Remitente, cur^.Asunto, cur^.Fecha]));
      if cur^.Next <> nil then
        sl.Add(Format('  n%d -> n%d;', [idx, idx+1]));
      Inc(idx);
      cur := cur^.Next;
    end;

    if InboxHead = nil then
      sl.Add('  vacío [label="Bandeja vacía"];');

    sl.Add('}');

    outPath := IncludeTrailingPathDelimiter(UserReportsDir) + 'inbox.dot';
    sl.SaveToFile(outPath);
    Result := outPath;
  finally
    sl.Free;
  end;
end;

function Report_Trash_DOT: string;
var
  sl: TStringList;
  n: PTrashNode;
  idx: Integer;
  outPath: string;
begin
  sl := TStringList.Create;
  try
    sl.Add('digraph Trash {');
    sl.Add('  rankdir=TB;');
    sl.Add('  node [shape=box, style=rounded, fontsize=10];');
    sl.Add('  top [label="TOP", shape=ellipse, style=bold];');

    idx := 1; n := TrashTop;
    if n = nil then
      sl.Add('  vacio [label="Papelera vacía"];')
    else
    begin
      while n <> nil do
      begin
        sl.Add(Format('  t%d [label="%s\n%s\n%s"];', [idx, n^.Remitente, n^.Asunto, n^.Fecha]));
        if idx = 1 then
          sl.Add('  top -> t1;')
        else
          sl.Add(Format('  t%d -> t%d;', [idx-1, idx]));
        Inc(idx);
        n := n^.Next;
      end;
    end;

    sl.Add('}');

    outPath := IncludeTrailingPathDelimiter(UserReportsDir) + 'papelera.dot';
    sl.SaveToFile(outPath);
    Result := outPath;
  finally
    sl.Free;
  end;
end;

function Report_Programados_DOT: string;
var
  sl: TStringList;
  cur: PProgNode;
  idx: Integer;
  outPath: string;
begin
  sl := TStringList.Create;
  try
    sl.Add('digraph Programados {');
    sl.Add('  rankdir=LR;');
    sl.Add('  node [shape=box, style=rounded, fontsize=10];');
    sl.Add('  head [label="HEAD", shape=ellipse, style=bold];');

    idx := 1; cur := ProgHead;
    if cur = nil then
      sl.Add('  vacio [label="Sin programados"];')
    else
    begin
      sl.Add('  head -> p1;');
      while cur <> nil do
      begin
        sl.Add(Format('  p%d [label="%s -> %s\n%s\n%s"];',
          [idx, cur^.Remitente, cur^.Destinatario, cur^.Asunto, cur^.FechaProg]));
        if cur^.Next <> nil then
          sl.Add(Format('  p%d -> p%d;', [idx, idx+1]));
        Inc(idx);
        cur := cur^.Next;
      end;
    end;

    sl.Add('}');

    outPath := IncludeTrailingPathDelimiter(UserReportsDir) + 'programados.dot';
    sl.SaveToFile(outPath);
    Result := outPath;
  finally
    sl.Free;
  end;
end;

function Report_Contactos_DOT: string;
var
  sl: TStringList;
  cur: PContactNode;
  idx: Integer;
  outPath: string;
begin
  sl := TStringList.Create;
  try
    sl.Add('digraph Contactos {');
    sl.Add('  rankdir=LR;');
    sl.Add('  node [shape=box, style=rounded, fontsize=10];');

    if ContactsTail = nil then
      sl.Add('  vacio [label="Sin contactos"];')
    else
    begin
      // imprimir en círculo
      idx := 1;
      cur := ContactsTail^.Next; // head
      repeat
        sl.Add(Format('  c%d [label="%s\n<%s>"];', [idx, cur^.Nombre, cur^.Email]));
        // flecha al siguiente (o al primero si es el último)
        if cur^.Next = ContactsTail^.Next then
          sl.Add(Format('  c%d -> c1;', [idx]))
        else
          sl.Add(Format('  c%d -> c%d;', [idx, idx+1]));
        Inc(idx);
        cur := cur^.Next;
      until cur = ContactsTail^.Next;
    end;

    sl.Add('}');

    outPath := IncludeTrailingPathDelimiter(UserReportsDir) + 'contactos.dot';
    sl.SaveToFile(outPath);
    Result := outPath;
  finally
    sl.Free;
  end;
end;
procedure Relations_GenerateDot(const OutFile: string);
var
  emails: TStringList;
  dot   : TStringList;
  counts: array of array of Integer;
  i, j  : Integer;
  cur   : PMailNode;
  uUsr  : PUserNode;  // <- declarar aquí

  function IndexOfEmail(const E: string): Integer;
  begin
    Result := emails.IndexOf(E);
    if Result < 0 then
      Result := emails.Add(E);
  end;
begin
  emails := TStringList.Create;
  dot    := TStringList.Create;
  try
    // 1) Reunimos todos los emails presentes en correos
    cur := InboxHead;
    while cur <> nil do
    begin
      IndexOfEmail(cur^.Remitente);
      IndexOfEmail(cur^.Destinatario);
      cur := cur^.Next;
    end;

    // Si no hay correos aún, al menos meter los correos de los usuarios
    if emails.Count = 0 then
    begin
      uUsr := UsersHead;           // <- asignar aquí
      while uUsr <> nil do
      begin
        IndexOfEmail(uUsr^.Email);
        uUsr := uUsr^.Next;
      end;
    end;

    // 2) Matriz NxN con conteos
    SetLength(counts, emails.Count);
    for i := 0 to emails.Count-1 do
    begin
      SetLength(counts[i], emails.Count);
      for j := 0 to emails.Count-1 do counts[i][j] := 0;
    end;

    // 3) Acumular
    cur := InboxHead;
    while cur <> nil do
    begin
      i := emails.IndexOf(cur^.Remitente);
      j := emails.IndexOf(cur^.Destinatario);
      if (i >= 0) and (j >= 0) then
        Inc(counts[i][j]);
      cur := cur^.Next;
    end;

    // 4) Escribir DOT
    dot.Add('digraph Relaciones {');
    dot.Add('  rankdir=LR;');
    dot.Add('  labelloc="t";');
    dot.Add('  label="Relaciones de envío (matriz dispersa)";');
    dot.Add('  node [shape=box, style="rounded,filled", fillcolor="#eef7ff"];');

    for i := 0 to emails.Count-1 do
      dot.Add(Format('  "%s";', [emails[i]]));

    for i := 0 to emails.Count-1 do
      for j := 0 to emails.Count-1 do
        if counts[i][j] > 0 then
          dot.Add(Format('  "%s" -> "%s" [label="%d"];',
                   [emails[i], emails[j], counts[i][j]]));

    dot.Add('}');

    // 5) Guardar
    ForceDirectories(ExtractFileDir(OutFile));
    dot.SaveToFile(OutFile);
  finally
    dot.Free;
    emails.Free;
  end;
end;
function Report_Borradores_DOT: string;
var
  sl: TStringList;

  function Safe(const S: string): string;
  begin
    // escape de comillas y saltos a \l (left-justified) para Graphviz
    Result := StringReplace(S, '"', '''', [rfReplaceAll]);
    Result := StringReplace(Result, #13#10, '\l', [rfReplaceAll]);
    Result := StringReplace(Result, #10, '\l', [rfReplaceAll]);
  end;

  procedure Walk(N: PAVL);
  var
    idStr, card: string;
  begin
    if N = nil then Exit;

    idStr := 'n' + IntToStr(N^.Key);

    // tarjeta con HTML-like label
    card :=
      '<' +
      '<TABLE BORDER="0" CELLBORDER="0" CELLPADDING="2" BGCOLOR="#FFFDE7">' +
      '<TR><TD><B>ID:</B> ' + IntToStr(N^.Data.Id) + '</TD></TR>' +
      '<TR><TD><B>Remitente:</B> ' + Safe(N^.Data.Remitente) + '</TD></TR>' +
      '<TR><TD><B>Estado:</B> ' + 'no leído' + '</TD></TR>' + // si manejas estado, cámbialo aquí
      '<TR><TD><B>Asunto:</B> ' + Safe(N^.Data.Asunto) + '</TD></TR>' +
      '<TR><TD><B>Mensaje:</B> ' + Safe(N^.Data.Mensaje) + '\l</TD></TR>' +
      '</TABLE>' +
      '>';

    sl.Add(Format('  %s [label=%s, shape=box, style="rounded,filled", fillcolor="#FFF9C4", fontname="Helvetica"];',
      [idStr, card]));

    if N^.L <> nil then
    begin
      sl.Add(Format('  %s -> n%d;', [idStr, N^.L^.Key]));
      Walk(N^.L);
    end;
    if N^.R <> nil then
    begin
      sl.Add(Format('  %s -> n%d;', [idStr, N^.R^.Key]));
      Walk(N^.R);
    end;
  end;

var
  outPath: string;
begin
  sl := TStringList.Create;
  try
    sl.Add('digraph "Reporte de Borradores (Árbol AVL)" {');
    sl.Add('  rankdir=TB;');
    sl.Add('  node [fontname="Helvetica"];');
    sl.Add('  edge [color="#90A4AE"];');

    if GlobalDrafts = nil then
      sl.Add('  vacio [label="(sin borradores)"];')
    else
      Walk(GlobalDrafts);

    sl.Add('}');

    outPath := IncludeTrailingPathDelimiter(UserReportsDir) + 'borradores.dot';
    sl.SaveToFile(outPath);
    Result := outPath;
  finally
    sl.Free;
  end;
end;

function Report_Favoritos_DOT: string;
var
  sl: TStringList;

  function Safe(const S: string): string;
  begin
    Result := StringReplace(S, '"', '''', [rfReplaceAll]);
    Result := StringReplace(Result, #13#10, '\l', [rfReplaceAll]);
    Result := StringReplace(Result, #10, '\l', [rfReplaceAll]);
  end;

  function FavCard(const M: TMailFav): string;
  begin
    Result :=
      '<' +
      '<B>ID:</B> ' + IntToStr(M.Id) + '\l' +
      '<B>De:</B> ' + Safe(M.Remitente) + '\l' +
      '<B>Para:</B> ' + Safe(M.Destinatario) + '\l' +
      '<B>Asunto:</B> ' + Safe(M.Asunto) + '\l' +
      '<B>Fecha:</B> ' + Safe(M.Fecha) + '\l' +
      '<B>Mensaje:</B> ' + Safe(M.Mensaje) + '\l' +
      '>';
  end;

  procedure EmitBlocks(Head: PBNode);
  var
    idx, i: Integer;
    cur: PBNode;
    labelLine: string;
  begin
    if Head = nil then
    begin
      sl.Add('  vacio [label="(sin favoritos)"];');
      Exit;
    end;

    idx := 0;
    cur := Head;
    while cur <> nil do
    begin
      Inc(idx);
      labelLine := '';

      // hasta 4 “celdas” por bloque
      for i := 1 to cur^.Count do
      begin
        if i > 1 then
          labelLine += '|';
        labelLine += '{' + FavCard(cur^.Keys[i]) + '}';
      end;

      sl.Add(Format('  blk%d [label="{{%s}}", shape=record, style="filled", fillcolor="#A5D6A7", fontname="Helvetica"];',
        [idx, labelLine]));

      if cur^.Next <> nil then
        sl.Add(Format('  blk%d -> blk%d;', [idx, idx + 1]));

      cur := cur^.Next;
    end;
  end;

var
  outPath: string;
begin
  sl := TStringList.Create;
  try
    sl.Add('digraph BTree_Favoritos {');
    sl.Add('  rankdir=TB;');
    sl.Add('  node  [shape=record, style="filled", fillcolor="#A5D6A7", fontname="Helvetica"];');
    sl.Add('  edge  [color="#90A4AE"];');

    EmitBlocks(GlobalFavs);

    sl.Add('}');

    outPath := IncludeTrailingPathDelimiter(UserReportsDir) + 'favoritos.dot';
    sl.SaveToFile(outPath);
    Result := outPath;
  finally
    sl.Free;
  end;
end;

function RootReportsDir: string;
var
  base, dir: string;
begin
  base := ExtractFilePath(ParamStr(0));
  dir  := IncludeTrailingPathDelimiter(base) + 'Root-Reportes';
  ForceDirectories(dir);
  Result := dir;
end;

procedure Users_GenerateDot(const OutFile: string);
var
  dot: TStringList;
  u  : PUserNode;
  function Esc(const S: string): string;
  begin
    // escapado simple para DOT
    Result := StringReplace(S, '"', '\"', [rfReplaceAll]);
  end;
begin
  dot := TStringList.Create;
  try
    dot.Add('digraph Usuarios {');
    dot.Add('  labelloc="t";');
    dot.Add('  label="Usuarios registrados";');
    dot.Add('  fontname="Helvetica";');
    dot.Add('  node [shape=plaintext, fontname="Helvetica"];');

    // Tabla
    dot.Add('  usuarios_table [label=<');
    dot.Add('  <table border="1" cellborder="1" cellspacing="0" cellpadding="6">');
    dot.Add('    <tr bgcolor="#e8f1ff"><td><b>ID</b></td><td><b>Nombre</b></td><td><b>Usuario</b></td><td><b>Email</b></td><td><b>Teléfono</b></td></tr>');

    u := UsersHead;
    while u <> nil do
    begin
      dot.Add(Format('    <tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>',
        [Esc(u^.Id), Esc(u^.Nombre), Esc(u^.Usuario), Esc(u^.Email), Esc(u^.Telefono)]));
      u := u^.Next;
    end;

    dot.Add('  </table>');
    dot.Add('  >];');
    dot.Add('}');

    ForceDirectories(ExtractFileDir(OutFile));
    dot.SaveToFile(OutFile);
  finally
    dot.Free;
  end;
end;


{======================  Bandeja (DOT)  ======================}
procedure Inbox_ToStringsFor(const L: TStrings; const Email: string);
var cur: PMailNode; i: Integer;
begin
  L.Clear; i := 1; cur := InboxHead;
  while cur <> nil do
  begin
    if SameText(cur^.Destinatario, Email) then
    begin
      L.Add(Format('%d) [%s] %s - %s (%s)', [i, cur^.Estado, cur^.Remitente, cur^.Asunto, cur^.Fecha]));
      Inc(i);
    end;
    cur := cur^.Next;
  end;
  if i=1 then L.Add('(Bandeja vacía)');
end;

function Inbox_GetNthFor(const Email: string; AIndex: Integer): PMailNode;
var cur: PMailNode; k: Integer;
begin
  Result := nil; if AIndex<=0 then Exit;
  k:=0; cur:=InboxHead;
  while cur<>nil do
  begin
    if SameText(cur^.Destinatario, Email) then
    begin
      Inc(k); if k=AIndex then Exit(cur);
    end;
    cur := cur^.Next;
  end;
end;

function Inbox_RemoveNode(Node: PMailNode; out AId, ARem, ADest, AAsunto, AFecha, AMsg, AEstado: string): boolean;
begin
  Result := False; if Node=nil then Exit;
  AId:=Node^.Id; ARem:=Node^.Remitente; ADest:=Node^.Destinatario;
  AAsunto:=Node^.Asunto; AFecha:=Node^.Fecha; AMsg:=Node^.Mensaje; AEstado:=Node^.Estado;

  if Node^.Prev<>nil then Node^.Prev^.Next:=Node^.Next else InboxHead:=Node^.Next;
  if Node^.Next<>nil then Node^.Next^.Prev:=Node^.Prev else InboxTail:=Node^.Prev;
  Dispose(Node); Result:=True;
end;
function Inbox_CountFor(const Email: string): Integer;
var
  cur: PMailNode;
begin
  Result := 0;
  cur := InboxHead;
  while cur <> nil do
  begin
    if SameText(cur^.Destinatario, Email) then
      Inc(Result);
    cur := cur^.Next;
  end;
end;
procedure EnsureDemoInboxFor(const Email: string);
var
  u: PUserNode;
  nowStr: string;
begin
  u := User_FindByEmail(Email);
  if u = nil then Exit;

  if u^.DemoSeeded then Exit; // ya se sembró antes

  nowStr := FormatDateTime('yyyy-mm-dd hh:nn', Now);
  Inbox_PushBack('d1', 'admin@edd.com',  Email,
                 'Bienvenido a EDDMail', nowStr, '¡Hola! Este es un correo de bienvenida.');
  Inbox_PushBack('d2', 'soporte@edd.com', Email,
                 'Tu cuenta fue creada',  nowStr, 'Prueba de bandeja inicial.');

  u^.DemoSeeded := True;
end;


// RENOMBRADA para no chocar con la tuya
function EnsureIdAuto(const S: string): string;
begin
  if Trim(S) <> '' then Exit(S);
  Result := FormatDateTime('yyyymmddhhnnsszzz', Now);
end;


function Inbox_LoadFromJSON(const FN: string; out Loaded, Skipped: Integer): Boolean;
var
  root, arr, it: TJSONData;
  i: Integer;
  idS, remi, dest, asu, est, msg, fec: string;
begin
  Loaded := 0; Skipped := 0;
  Result := False;
  try
    root := GetJSON(TFileStream.Create(FN, fmOpenRead or fmShareDenyNone), True);
    try
      if root.JSONType = jtArray then
        arr := root
      else
        arr := JGetArray(root, ['Correos','correos']);
      if arr = nil then Exit(False);

      for i := 0 to arr.Count - 1 do
      begin
        it   := arr.Items[i];

        idS  := JGetStrDef(it, ['Id','id'], '');
        remi := Trim(JGetStrDef(it, ['Remitente','remitente'], ''));
        dest := Trim(JGetStrDef(it, ['Destinatario','destinatario'], ''));
        asu  := JGetStrDef(it, ['Asunto','asunto'], '');
        msg  := JGetStrDef(it, ['Mensaje','mensaje'], '');
        est  := UpperCase(Trim(JGetStrDef(it, ['Estado','estado'], 'NL')));
        if (est='LEÍDO') or (est='LEIDO') or (est='L') then est := 'L' else est := 'NL';

        if (remi='') or (dest='') then begin Inc(Skipped); Continue; end;
        if (User_FindByEmail(remi)=nil) or (User_FindByEmail(dest)=nil) then begin Inc(Skipped); Continue; end;

        fec := FormatDateTime('yyyy-mm-dd hh:nn', Now);
        idS := EnsureIdAuto(idS);

        // *** 6 parámetros ***
        Inbox_PushBack(idS, remi, dest, asu, fec, msg);

        Inc(Loaded);
      end;
      Result := True;
    finally
      root.Free;
    end;
  except
    Result := False;
  end;
end;


function Inbox_LoadFromCSV(const FN: string; out Loaded, Skipped: Integer): Boolean;
var
  SL: TStringList;
  i: Integer;
  L, idS, remi, dest, asu, fecS, msg, fec: string;
  parts: TStringArray;
begin
  Loaded := 0; Skipped := 0;
  Result := False;
  SL := TStringList.Create;
  try
    SL.LoadFromFile(FN);
    if SL.Count = 0 then Exit(True);

    // Id,Remitente,Destinatario,Asunto,Fecha,Mensaje
    i := 0;
    if Pos('remitente', LowerCase(SL[0])) > 0 then i := 1;

    for i := i to SL.Count - 1 do
    begin
      L := Trim(SL[i]);
      if L = '' then Continue;

      parts := SplitCSVLine(L);
      if Length(parts) < 3 then begin Inc(Skipped); Continue; end;

      if Length(parts) < 6 then
      begin
        idS  := '';
        remi := Trim(parts[0]);
        dest := Trim(parts[1]);
        asu  := parts[2];
        fecS := '';
        msg  := '';
      end
      else
      begin
        idS  := parts[0];
        remi := Trim(parts[1]);
        dest := Trim(parts[2]);
        asu  := parts[3];
        fecS := parts[4];
        msg  := parts[5];
      end;

      if (remi='') or (dest='') then begin Inc(Skipped); Continue; end;
      if (User_FindByEmail(remi)=nil) or (User_FindByEmail(dest)=nil) then begin Inc(Skipped); Continue; end;

      idS := EnsureIdAuto(idS);
      fec := FormatDateTime('yyyy-mm-dd hh:nn', ParseDateDef(fecS, Now));

      // *** 6 parámetros ***
      Inbox_PushBack(idS, remi, dest, asu, fec, msg);

      Inc(Loaded);
    end;

    Result := True;
  finally
    SL.Free;
  end;
end;


function Inbox_LoadFromFile(const AFile: string; out Loaded, Skipped: Integer): Boolean;
var
  ext: string;
begin
  ext := LowerCase(ExtractFileExt(AFile));
  if (ext = '.json') then
    Result := Inbox_LoadFromJSON(AFile, Loaded, Skipped)
  else
    Result := Inbox_LoadFromCSV(AFile, Loaded, Skipped);
end;
function Inbox_CountReadFor(const Destinatario: string): Integer;
var
  cur: PMailNode;
  cnt: Integer;
  who: string;
begin
  who := LowerCase(Trim(Destinatario));
  cnt := 0;
  cur := InboxHead;
  while cur <> nil do
  begin
    if SameText(cur^.Destinatario, who) and SameText(cur^.Estado, 'L')then
    Inc(cnt);
    cur := cur^.Next;
    end;
  Result := cnt;
  end;
end.





end.

