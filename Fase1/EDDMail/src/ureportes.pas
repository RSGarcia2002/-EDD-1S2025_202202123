unit UReportes;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils;

function ReporteUsuarios_Generar(out RutaDot: string; out RutaPNG: string): string;

implementation

uses
  Classes, UDataCore
  {$IFDEF UNIX}, Process{$ENDIF}; // para ejecutar 'dot' opcionalmente

function EnsureDir(const Dir: string): boolean;
begin
  if DirectoryExists(Dir) then Exit(True);
  Result := ForceDirectories(Dir);
end;

function San(const S: string): string;
begin
  // Escapa comillas y pipes para etiquetas Graphviz
  Result := StringReplace(S, '"', '\"', [rfReplaceAll]);
  Result := StringReplace(Result, '|', '\|', [rfReplaceAll]);
  Result := StringReplace(Result, '{', '\{', [rfReplaceAll]);
  Result := StringReplace(Result, '}', '\}', [rfReplaceAll]);
  // Graphviz entiende \n como salto
  Result := StringReplace(Result, LineEnding, '\n', [rfReplaceAll]);
end;

function ReporteUsuarios_Generar(out RutaDot: string; out RutaPNG: string): string;
var
  outDir: string;
  L: TStringList;
  cur: PUserNode;
  idx: Integer;
  nodeName, prevNode: string;
  dot: string;
  okExec: boolean;
{$IFDEF UNIX}
  P: TProcess;
{$ENDIF}
begin
  RutaDot := '';
  RutaPNG := '';
  outDir  := IncludeTrailingPathDelimiter(GetCurrentDir) + 'Root-Reportes';
  if not EnsureDir(outDir) then
    Exit('No se pudo crear la carpeta Root-Reportes en: ' + outDir);

  RutaDot := IncludeTrailingPathDelimiter(outDir) + 'usuarios.dot';
  RutaPNG := IncludeTrailingPathDelimiter(outDir) + 'usuarios.png';

  L := TStringList.Create;
  try
    // Cabecera DOT
    L.Add('digraph Usuarios {');
    L.Add('  rankdir=LR;');
    L.Add('  node [shape=record, fontsize=10, fontname="Helvetica"];');
    L.Add('  edge [arrowhead=vee, arrowsize=0.6];');
    L.Add('');

    // Generar nodos y enlaces como lista simple
    cur := UsersHead;
    idx := 0;
    prevNode := '';
    while cur <> nil do
    begin
      nodeName := 'n' + IntToStr(idx);
      dot := Format('  %s [label="{Id: %s|Nombre: %s|Usuario: %s|Email: %s|Tel: %s}"];',
                    [nodeName,
                     San(cur^.Id), San(cur^.Nombre), San(cur^.Usuario),
                     San(cur^.Email), San(cur^.Telefono)]);
      L.Add(dot);

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

  // (Opcional) Generar PNG con Graphviz si está disponible (solo UNIX)
  okExec := False;
{$IFDEF UNIX}
  try
    if FileExists('/usr/bin/dot') or FileExists('/bin/dot') then
    begin
      P := TProcess.Create(nil);
      try
        P.Executable := 'dot';
        P.Parameters.Add('-Tpng');
        P.Parameters.Add(RutaDot);
        P.Parameters.Add('-o');
        P.Parameters.Add(RutaPNG);
        P.Options := [poUsePipes, poWaitOnExit];
        P.Execute;
        okExec := (P.ExitStatus = 0) and FileExists(RutaPNG);
      finally
        P.Free;
      end;
    end;
  except
    okExec := False;
  end;
{$ENDIF}

  if okExec then
    Result := 'Reporte generado: ' + RutaDot + LineEnding +
              'Imagen generada:  ' + RutaPNG
  else
    Result := 'Reporte generado: ' + RutaDot + LineEnding +
              '(Instala Graphviz para generar PNG con dot)';
end;



end.

