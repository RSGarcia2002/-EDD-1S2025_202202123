unit UReportes;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes;

function ReporteUsuarios_Generar(const UserOrRoot: string; out RutaDot, RutaPNG: string): string;
function ReporteRelaciones_Generar(const UserOrRoot: string; out RutaDot, RutaPNG: string): string;
function ReporteCorreosRecibidos_Generar(const UserOrRoot: string; out RutaDot, RutaPNG: string): string;
function ReportePapelera_Generar(const UserOrRoot: string; out RutaDot, RutaPNG: string): string;
function ReporteProgramados_Generar(const UserOrRoot: string; out RutaDot, RutaPNG: string): string;
function ReporteContactos_Generar(const UserOrRoot: string; out RutaDot, RutaPNG: string): string;
function ReporteComunidades_Generar(const UserOrRoot: string; out RutaDot, RutaPNG: string): string;

implementation

uses
  Process, UDataCore;

{ ---------- utilidades comunes ---------- }

function EnsureDir(const Dir: string): boolean;
begin
  if DirectoryExists(Dir) then Exit(True);
  Result := ForceDirectories(Dir);
end;

function San(const S: string): string;
begin
  // Escapes seguros para etiquetas Graphviz
  Result := StringReplace(S, '"', '\"', [rfReplaceAll]);
  Result := StringReplace(Result, '|', '\|', [rfReplaceAll]);
  Result := StringReplace(Result, '{', '\{', [rfReplaceAll]);
  Result := StringReplace(Result, '}', '\}', [rfReplaceAll]);
  Result := StringReplace(Result, LineEnding, '\n', [rfReplaceAll]); // \n visible en DOT
end;

function ReportsDir(const UserOrRoot: string): string;
var base: string;
begin
  base := GetCurrentDir;
  if SameText(UserOrRoot, 'root') then
    Result := IncludeTrailingPathDelimiter(base) + 'Root-Reportes'
  else
    Result := IncludeTrailingPathDelimiter(base) + LowerCase(UserOrRoot) + '-reportes';
  EnsureDir(Result);
end;

function RunDotToPNG(const ARutaDot, ARutaPNG: string; out ErrMsg: string): boolean;
var
  P: TProcess;
  OutBuf: TStringList;
begin
  Result := False;
  ErrMsg := '';
  P := TProcess.Create(nil);
  OutBuf := TStringList.Create;
  try
    P.Executable := 'dot';            // usa Graphviz del PATH
    P.Parameters.Add('-Tpng');
    P.Parameters.Add(ARutaDot);
    P.Parameters.Add('-o');
    P.Parameters.Add(ARutaPNG);
    P.Options := [poUsePipes, poWaitOnExit, poStderrToOutPut];
    try
      P.Execute;
      OutBuf.LoadFromStream(P.Output);
      ErrMsg := Trim(OutBuf.Text);
    except
      on E: Exception do
        ErrMsg := 'No se pudo ejecutar "dot". ¿Graphviz instalado? Detalle: ' + E.Message;
    end;
    Result := (P.ExitStatus = 0) and FileExists(ARutaPNG);
    if (not Result) and (ErrMsg = '') then
      ErrMsg := Format('dot terminó con código %d y no generó %s', [P.ExitStatus, ARutaPNG]);
  finally
    OutBuf.Free;
    P.Free;
  end;
end;

function FinalizarReporte(const RutaDot, RutaPNG: string): string;
var ok: boolean; err: string;
begin
  ok := RunDotToPNG(RutaDot, RutaPNG, err);
  if ok then
    Result := 'Reporte generado: ' + RutaDot + LineEnding +
              'Imagen generada:  ' + RutaPNG
  else
    Result := 'Reporte generado: ' + RutaDot + LineEnding +
              'No se pudo generar PNG. Detalle: ' + err;
end;

{ ---------- Reporte: Usuarios (lista simple) ---------- }

function ReporteUsuarios_Generar(const UserOrRoot: string; out RutaDot, RutaPNG: string): string;
var
  outDir: string;
  L: TStringList;
  cur: PUserNode;
  idx: Integer;
  nodeName, prevNode: string;
begin
  RutaDot := ''; RutaPNG := '';
  outDir := ReportsDir(UserOrRoot);
  RutaDot := IncludeTrailingPathDelimiter(outDir) + 'usuarios.dot';
  RutaPNG := IncludeTrailingPathDelimiter(outDir) + 'usuarios.png';

  L := TStringList.Create;
  try
    L.Add('digraph Usuarios {');
    L.Add('  rankdir=LR;');
    L.Add('  node [shape=record, fontsize=10, fontname="Helvetica"];');
    L.Add('  edge [arrowhead=vee, arrowsize=0.6];');
    L.Add('');

    cur := UsersHead;
    idx := 0;
    prevNode := '';
    while cur <> nil do
    begin
      nodeName := 'n' + IntToStr(idx);
      L.Add(Format('  %s [label="{Id: %s|Nombre: %s|Usuario: %s|Email: %s|Tel: %s}"];',
                   [nodeName, San(cur^.Id), San(cur^.Nombre), San(cur^.Usuario),
                    San(cur^.Email), San(cur^.Telefono)]));
      if prevNode <> '' then
        L.Add(Format('  %s -> %s;', [prevNode, nodeName]));
      prevNode := nodeName;
      cur := cur^.Next;
      Inc(idx);
    end;

    if idx = 0 then
      L.Add('  empty [label="(sin usuarios)", shape=plaintext];');

    L.Add('}');
    L.SaveToFile(RutaDot);
  finally
    L.Free;
  end;

  Result := FinalizarReporte(RutaDot, RutaPNG);
end;

{ ---------- PLANTILLAS: rellena el bloque señalado en cada uno ---------- }

function ReporteRelaciones_Generar(const UserOrRoot: string; out RutaDot, RutaPNG: string): string;
var
  outDir: string; L: TStringList;
begin
  RutaDot := ''; RutaPNG := '';
  outDir := ReportsDir(UserOrRoot);
  RutaDot := IncludeTrailingPathDelimiter(outDir) + 'relaciones.dot';
  RutaPNG := IncludeTrailingPathDelimiter(outDir) + 'relaciones.png';

  L := TStringList.Create;
  try
    L.Add('digraph Relaciones {');
    L.Add('  rankdir=LR;');
    L.Add('  node [shape=record, fontname="Helvetica"];');
    L.Add('  edge [arrowhead=vee];');
    L.Add('');
    { TODO: Recorre tu estructura de relaciones (matriz dispersa o conteos) y
            agrega aristas Emisor->Receptor con etiqueta de cantidad.
      Ejemplo:
        L.Add(Format('  "%s" -> "%s" [label="%d"];', [San(EmailEmisor), San(EmailDest), Conteo]));
    }
    if True then
      L.Add('  info [shape=plaintext, label="(pendiente: llenar relaciones)"];');
    L.Add('}');
    L.SaveToFile(RutaDot);
  finally
    L.Free;
  end;
  Result := FinalizarReporte(RutaDot, RutaPNG);
end;

function ReporteCorreosRecibidos_Generar(const UserOrRoot: string; out RutaDot, RutaPNG: string): string;
var
  outDir: string; L: TStringList;
begin
  RutaDot := ''; RutaPNG := '';
  outDir := ReportsDir(UserOrRoot);
  RutaDot := IncludeTrailingPathDelimiter(outDir) + 'correos_recibidos.dot';
  RutaPNG := IncludeTrailingPathDelimiter(outDir) + 'correos_recibidos.png';

  L := TStringList.Create;
  try
    L.Add('digraph CorreosRecibidos {');
    L.Add('  rankdir=LR;');
    L.Add('  node [shape=record, fontname="Helvetica"];');
    L.Add('  edge [arrowhead=vee];');
    L.Add('');
    { TODO: Recorre tu lista doble de bandeja de entrada del usuario (UserOrRoot) y
            agrega nodos por correo, con enlaces c0->c1->c2...
      Ejemplo nodo:
        L.Add(Format('  c%d [label="{Id: %s|De: %s|Asunto: %s|Fecha: %s|Estado: %s}"];', ...));
    }
    if True then
      L.Add('  info [shape=plaintext, label="(pendiente: llenar correos recibidos)"];');
    L.Add('}');
    L.SaveToFile(RutaDot);
  finally
    L.Free;
  end;
  Result := FinalizarReporte(RutaDot, RutaPNG);
end;

function ReportePapelera_Generar(const UserOrRoot: string; out RutaDot, RutaPNG: string): string;
var
  outDir: string; L: TStringList;
begin
  RutaDot := ''; RutaPNG := '';
  outDir := ReportsDir(UserOrRoot);
  RutaDot := IncludeTrailingPathDelimiter(outDir) + 'papelera.dot';
  RutaPNG := IncludeTrailingPathDelimiter(outDir) + 'papelera.png';

  L := TStringList.Create;
  try
    L.Add('digraph Papelera {');
    L.Add('  rankdir=TB;');
    L.Add('  node [shape=record, fontname="Helvetica"];');
    L.Add('');
    { TODO: Recorre tu pila (top-first) y agrega p0->p1->p2 (LIFO).
      Ejemplo nodo:
        L.Add(Format('  p%d [label="{Asunto: %s|De: %s}"];', ...));
    }
    if True then
      L.Add('  info [shape=plaintext, label="(pendiente: llenar pila de papelera)"];');
    L.Add('}');
    L.SaveToFile(RutaDot);
  finally
    L.Free;
  end;
  Result := FinalizarReporte(RutaDot, RutaPNG);
end;

function ReporteProgramados_Generar(const UserOrRoot: string; out RutaDot, RutaPNG: string): string;
var
  outDir: string; L: TStringList;
begin
  RutaDot := ''; RutaPNG := '';
  outDir := ReportsDir(UserOrRoot);
  RutaDot := IncludeTrailingPathDelimiter(outDir) + 'programados.dot';
  RutaPNG := IncludeTrailingPathDelimiter(outDir) + 'programados.png';

  L := TStringList.Create;
  try
    L.Add('digraph Programados {');
    L.Add('  rankdir=LR;');
    L.Add('  node [shape=record, fontname="Helvetica"];');
    L.Add('');
    { TODO: Recorre tu cola (FIFO) y agrega q0->q1->q2...
      Ejemplo nodo:
        L.Add(Format('  q%d [label="{Para: %s|Asunto: %s|FechaProg: %s}"];', ...));
    }
    if True then
      L.Add('  info [shape=plaintext, label="(pendiente: llenar cola de programados)"];');
    L.Add('}');
    L.SaveToFile(RutaDot);
  finally
    L.Free;
  end;
  Result := FinalizarReporte(RutaDot, RutaPNG);
end;

function ReporteContactos_Generar(const UserOrRoot: string; out RutaDot, RutaPNG: string): string;
var
  outDir: string; L: TStringList;
begin
  RutaDot := ''; RutaPNG := '';
  outDir := ReportsDir(UserOrRoot);
  RutaDot := IncludeTrailingPathDelimiter(outDir) + 'contactos.dot';
  RutaPNG := IncludeTrailingPathDelimiter(outDir) + 'contactos.png';

  L := TStringList.Create;
  try
    L.Add('digraph Contactos {');
    L.Add('  rankdir=LR;');
    L.Add('  node [shape=record, fontname="Helvetica"];');
    L.Add('');
    { TODO: Recorre tu lista circular de contactos del usuario.
      Dibuja k0->k1->... y al final kN->k0 para cerrar círculo.
      Ejemplo nodo:
        L.Add(Format('  k%d [label="{Nombre: %s|Email: %s}"];', ...));
    }
    if True then
      L.Add('  info [shape=plaintext, label="(pendiente: llenar lista circular de contactos)"];');
    L.Add('}');
    L.SaveToFile(RutaDot);
  finally
    L.Free;
  end;
  Result := FinalizarReporte(RutaDot, RutaPNG);
end;

function ReporteComunidades_Generar(const UserOrRoot: string; out RutaDot, RutaPNG: string): string;
var
  outDir: string; L: TStringList;
begin
  RutaDot := ''; RutaPNG := '';
  outDir := ReportsDir(UserOrRoot);
  RutaDot := IncludeTrailingPathDelimiter(outDir) + 'comunidades.dot';
  RutaPNG := IncludeTrailingPathDelimiter(outDir) + 'comunidades.png';

  L := TStringList.Create;
  try
    L.Add('digraph Comunidades {');
    L.Add('  rankdir=TB;');
    L.Add('  node [shape=record, fontname="Helvetica"];');
    L.Add('');
    { TODO: Por cada comunidad, crea un subgrafo/cluster y dentro agrega sus usuarios.
      Ejemplo:
        L.Add('  subgraph cluster_dev { label="Comunidad: Dev"; style=rounded; color=gray; }');
        L.Add('    "Dev_0" [label="{Nombre: Ana|Id: 1}"]; ...');
    }
    if True then
      L.Add('  info [shape=plaintext, label="(pendiente: llenar comunidades y usuarios)"];');
    L.Add('}');
    L.SaveToFile(RutaDot);
  finally
    L.Free;
  end;
  Result := FinalizarReporte(RutaDot, RutaPNG);
end;

end.

