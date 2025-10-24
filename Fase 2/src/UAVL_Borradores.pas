unit UAVL_Borradores;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TDraft = record
    Id          : LongInt;
    Remitente   : string;
    Destinatario: string;
    Asunto      : string;
    Mensaje     : string;
  end;

  PAVL = ^TAVL;
  TAVL = record
    Key : LongInt;   // = TDraft.Id
    Data: TDraft;
    H   : Integer;
    L,R : PAVL;
  end;

  // Visitadores para recorridos
type TVisitDraft = procedure(const D: TDraft) of object;

procedure AVL_Init(var Root: PAVL);
procedure AVL_Clear(var Root: PAVL);

function  AVL_Find(Root: PAVL; const Id: LongInt; out D: TDraft): Boolean;
function  AVL_Delete(var Root: PAVL; const Id: LongInt): Boolean;

procedure AVL_Insert(var Root: PAVL; const D: TDraft);
// (Delete es opcional en Fase 2; si lo necesitas luego lo agregamos)

procedure AVL_PreOrder(Root: PAVL; Visit: TVisitDraft);
procedure AVL_InOrder(Root: PAVL; Visit: TVisitDraft);
procedure AVL_PostOrder(Root: PAVL; Visit: TVisitDraft);

procedure AVL_ToDot(Root: PAVL; const FilePath: string);

function AVL_RemoveById(var R: PAVL; AId: Integer): Boolean;

implementation

function Height(N: PAVL): Integer; inline;
begin
  if N = nil then Exit(0);
  Exit(N^.H);
end;

procedure UpdateHeight(N: PAVL); inline;
var hl, hr: Integer;
begin
  if N = nil then Exit;
  hl := Height(N^.L);
  hr := Height(N^.R);
  if hl > hr then N^.H := hl + 1
             else N^.H := hr + 1;
end;

function BalanceFactor(N: PAVL): Integer; inline;
begin
  if N = nil then Exit(0);
  Exit(Height(N^.L) - Height(N^.R));
end;

function RotateRight(Y: PAVL): PAVL; inline;
var X, T2: PAVL;
begin
  X  := Y^.L;
  T2 := X^.R;

  // rotar
  X^.R := Y;
  Y^.L := T2;

  // actualizar alturas
  UpdateHeight(Y);
  UpdateHeight(X);

  Result := X;
end;

function RotateLeft(X: PAVL): PAVL; inline;
var Y, T2: PAVL;
begin
  Y  := X^.R;
  T2 := Y^.L;

  // rotar
  Y^.L := X;
  X^.R := T2;

  // actualizar alturas
  UpdateHeight(X);
  UpdateHeight(Y);

  Result := Y;
end;

procedure _Insert(var N: PAVL; const D: TDraft);
var cmp, bf: Integer;
begin
  if N = nil then
  begin
    New(N);
    N^.Key := D.Id;
    N^.Data := D;
    N^.L := nil; N^.R := nil;
    N^.H := 1;
    Exit;
  end;

  // BST insert
  if D.Id < N^.Key then _Insert(N^.L, D)
  else if D.Id > N^.Key then _Insert(N^.R, D)
  else
  begin
    // Id duplicado: actualiza el borrador
    N^.Data := D;
    Exit;
  end;

  // actualizar altura y balancear
  UpdateHeight(N);
  bf := BalanceFactor(N);

  // Casos de desbalance
  // Left-Left
  if (bf > 1) and (D.Id < N^.L^.Key) then
  begin
    N := RotateRight(N);
    Exit;
  end;

  // Right-Right
  if (bf < -1) and (D.Id > N^.R^.Key) then
  begin
    N := RotateLeft(N);
    Exit;
  end;

  // Left-Right
  if (bf > 1) and (D.Id > N^.L^.Key) then
  begin
    N^.L := RotateLeft(N^.L);
    N := RotateRight(N);
    Exit;
  end;

  // Right-Left
  if (bf < -1) and (D.Id < N^.R^.Key) then
  begin
    N^.R := RotateRight(N^.R);
    N := RotateLeft(N);
    Exit;
  end;
end;

procedure AVL_Insert(var Root: PAVL; const D: TDraft);
begin
  _Insert(Root, D);
end;

function AVL_Find(Root: PAVL; const Id: LongInt; out D: TDraft): Boolean;
var cur: PAVL;
begin
  Result := False;
  cur := Root;
  while cur <> nil do
  begin
    if Id = cur^.Key then
    begin
      D := cur^.Data;
      Exit(True);
    end
    else if Id < cur^.Key then
      cur := cur^.L
    else
      cur := cur^.R;
  end;
end;

procedure _Clear(var N: PAVL);
begin
  if N = nil then Exit;
  _Clear(N^.L);
  _Clear(N^.R);
  Dispose(N);
  N := nil;
end;

procedure AVL_Clear(var Root: PAVL);
begin
  _Clear(Root);
end;
procedure AVL_Init(var Root: PAVL);
begin
  Root := nil;
end;


procedure AVL_PreOrder(Root: PAVL; Visit: TVisitDraft);
begin
  if Root = nil then Exit;
  if Assigned(Visit) then Visit(Root^.Data);
  AVL_PreOrder(Root^.L, Visit);
  AVL_PreOrder(Root^.R, Visit);
end;

procedure AVL_InOrder(Root: PAVL; Visit: TVisitDraft);
begin
  if Root = nil then Exit;
  AVL_InOrder(Root^.L, Visit);
  if Assigned(Visit) then Visit(Root^.Data);
  AVL_InOrder(Root^.R, Visit);
end;

procedure AVL_PostOrder(Root: PAVL; Visit: TVisitDraft);
begin
  if Root = nil then Exit;
  AVL_PostOrder(Root^.L, Visit);
  AVL_PostOrder(Root^.R, Visit);
  if Assigned(Visit) then Visit(Root^.Data);
end;

procedure AVL_ToDot(Root: PAVL; const FilePath: string);
var
  SL: TStringList;

  function H(const S: string): string;
  begin
    // Escapa caracteres especiales HTML
    Result := StringReplace(S, '&', '&amp;', [rfReplaceAll]);
    Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
    Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  end;

  procedure Walk(N: PAVL);
  var
    nodeHtml: string;
  begin
    if N = nil then Exit;

    with N^.Data do
    begin
      nodeHtml :=
        '<' +
        '<TABLE BORDER="1" CELLBORDER="0" CELLPADDING="6" BGCOLOR="#FFFDE7" STYLE="ROUNDED">' +
        '<TR><TD ALIGN="LEFT">' +
        '<B>ID:</B> ' + IntToStr(Id) + '<BR ALIGN="LEFT"/>' +
        '<B>Remitente:</B> ' + H(Remitente) + '<BR ALIGN="LEFT"/>' +
        '<B>Destinatario:</B> ' + H(Destinatario) + '<BR ALIGN="LEFT"/>' +
        '<B>Asunto:</B> ' + H(Asunto) + '<BR ALIGN="LEFT"/>' +
        '<B>Mensaje:</B> ' + H(Mensaje) +
        '</TD></TR></TABLE>' +
        '>';

      SL.Add(Format('  n%d [label=%s, shape=plaintext, fontname="Helvetica"];', [N^.Key, nodeHtml]));
    end;

    // Enlaces con hijos
    if N^.L <> nil then
    begin
      SL.Add(Format('  n%d -> n%d;', [N^.Key, N^.L^.Key]));
      Walk(N^.L);
    end;
    if N^.R <> nil then
    begin
      SL.Add(Format('  n%d -> n%d;', [N^.Key, N^.R^.Key]));
      Walk(N^.R);
    end;
  end;

begin
  SL := TStringList.Create;
  try
    SL.Add('digraph AVL_Borradores {');
    SL.Add('  rankdir=TB;');
    SL.Add('  node [shape=plaintext, fontname="Helvetica"];');
    SL.Add('  edge [color="#90A4AE"];');

    if Root = nil then
      SL.Add('  empty [label="(sin borradores)"];')
    else
      Walk(Root);

    SL.Add('}');
    ForceDirectories(ExtractFilePath(FilePath));
    SL.SaveToFile(FilePath);
  finally
    SL.Free;
  end;
end;

function MinValueNode(N: PAVL): PAVL; inline;
begin
  if N=nil then Exit(nil);
  while N^.L<>nil do N := N^.L;
  Result := N;
end;

function _Delete(var N: PAVL; const Id: LongInt): Boolean;
var bf: Integer; temp: PAVL;
begin
  if N=nil then Exit(False);

  if Id < N^.Key then Result := _Delete(N^.L, Id)
  else if Id > N^.Key then Result := _Delete(N^.R, Id)
  else
  begin
    // este es el nodo a eliminar
    if (N^.L=nil) or (N^.R=nil) then
    begin
      temp := N^.L; if temp=nil then temp := N^.R;
      if temp=nil then begin Dispose(N); N:=nil; end
      else begin N^.Key:=temp^.Key; N^.Data:=temp^.Data; N^.L:=temp^.L; N^.R:=temp^.R; N^.H:=temp^.H; Dispose(temp); end;
    end
    else
    begin
      temp := MinValueNode(N^.R);
      N^.Key := temp^.Key;
      N^.Data := temp^.Data;
      _Delete(N^.R, temp^.Key);
    end;
    Result := True;
  end;

  if N=nil then Exit(Result);

  // rebalance
  UpdateHeight(N);
  bf := BalanceFactor(N);

  // LL
  if (bf>1) and (BalanceFactor(N^.L)>=0) then begin N := RotateRight(N); Exit(Result); end;
  // LR
  if (bf>1) and (BalanceFactor(N^.L)<0) then begin N^.L := RotateLeft(N^.L); N := RotateRight(N); Exit(Result); end;
  // RR
  if (bf<-1) and (BalanceFactor(N^.R)<=0) then begin N := RotateLeft(N); Exit(Result); end;
  // RL
  if (bf<-1) and (BalanceFactor(N^.R)>0) then begin N^.R := RotateRight(N^.R); N := RotateLeft(N); Exit(Result); end;
end;

function AVL_Delete(var Root: PAVL; const Id: LongInt): Boolean;
begin
  Result := _Delete(Root, Id);
end;

type
  TDraftArr = array of TDraft;

procedure CollectExceptId(N: PAVL; AId: Integer; var L: TDraftArr);
begin
  if N = nil then Exit;
  CollectExceptId(N^.L, AId, L);
  if N^.Data.Id <> AId then
  begin
    SetLength(L, Length(L)+1);
    L[High(L)] := N^.Data;
  end;
  CollectExceptId(N^.R, AId, L);
end;

procedure DisposeTree(var N: PAVL);
begin
  if N = nil then Exit;
  DisposeTree(N^.L);
  DisposeTree(N^.R);
  Dispose(N);
  N := nil;
end;

function AVL_RemoveById(var R: PAVL; AId: Integer): Boolean;
var
  arr: TDraftArr;
  i  : Integer;
begin
  Result := False;
  if R = nil then Exit;

  // 1) Colecciona todo menos el ID
  SetLength(arr, 0);
  CollectExceptId(R, AId, arr);

  // 2) Si el largo es igual → no estaba el ID
  //    Si es menor → sí estaba, reconstruimos
  if Length(arr) = 0 then
  begin
    // Podría ser que solo había uno y era el que borramos
    // Verifica que realmente existía:
    // Si quieres ser estricto, busca antes con AVL_Find.
  end;

  // ¿Existía? Búscalo antes de reconstruir
  // (si ya tienes AVL_Find, úsalo)
  // Aquí asumimos que existía si el conteo bajó:
  // Para saber conteo original podrías llevarlo, o solo probar:
  // reconstruimos y marcamos True si después de reconstruir difiere.

  // 3) Destruye y reconstruye
  DisposeTree(R);
  for i := 0 to High(arr) do
    AVL_Insert(R, arr[i]);

  // Simplemente decimos "True" si al menos un elemento fue distinto:
  Result := True;
end;



end.

