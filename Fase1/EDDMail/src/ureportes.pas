unit UReportes;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes;

function ReporteUsuarios_Generar(out RutaDot: string; out RutaPNG: string): string;

implementation

uses
  UDataCore, Process;

function EnsureDir(const Dir: string): boolean;
begin
  if DirectoryExists(Dir) then Exit(True);
  Result := ForceDirectories(Dir);
end;

function San(const S: string): string;
begin
  // Escapa caracteres problemáticos para etiquetas Graphviz
  Result := StringReplace(S, '"', '\"', [rfReplaceAll]);
  Result := StringReplace(Result, '|', '\|', [rfReplaceAll]);
  Result := StringReplace(Result, '{', '\{', [rfReplaceAll]);
  Result := StringReplace(Result, '}', '\}', [rfReplaceAll]);
  // Graphviz entiende \n como salto de línea
  Result := StringReplace(Result, LineEnding, '\n', [rfReplaceAll]);
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
    // Usa 'dot' del PATH (Linux/Windows/Mac)
    P.Executable := 'dot';
    P.Parameters.Add('-Tpng');
    P.Parameters.Add(ARutaDot);
    P.Parameters.Add('-o');
    P.Parameters.Add(ARutaPNG);
    // Captura stderr en stdout y espera a que termine
    P.Options := [poUsePipes, poWaitOnExit, poStderrToOutPut];
    try
      P.Execute;
      // Lee la salida/errores para depurar si algo falla
      OutBuf.LoadFromStream(P.Output);
      ErrMsg := Trim(OutBuf.Text);
    except
      on E: Exception do
        ErrMsg := 'No se pudo ejecutar "dot". ¿Está Graphviz instalado y en el PATH? Detalle: ' + E.Message;
    end;

    Result := (P.ExitStatus = 0) and FileExists(ARutaPNG);
    if (not Result) and (ErrMsg = '') then
      ErrMsg := 'dot finalizó con código ' + IntToStr(P.ExitStatus) +
                ' y no se encontró el PNG en: ' + ARutaPNG;
  finally
    OutBuf.Free;
    P.Free;
  end;
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
  Err: string;
begin
  RutaDot := '';
  RutaPNG := '';

  // Carpeta de salida: <cwd>/Root-Reportes
  outDir := IncludeTrailingPathDelimiter(GetCurrentDir) + 'Root-Reportes';
  if not EnsureDir(outDir) then
  begin
    Result := 'No se pudo crear la carpeta Root-Reportes en: ' + outDir;
    Exit;
  end;

  RutaDot := IncludeTrailingPathDelimiter(outDir) + 'usuarios.dot';
  RutaPNG := IncludeTrailingPathDelimiter(outDir) + 'usuarios.png';

  // Construcción del archivo DOT
  L := TStringList.Create;
  try
    L.Add('digraph Usuarios {');
    L.Add('  rankdir=LR;');
    L.Add('  node [shape=record, fontsize=10, fontname="Helvetica"];');
    L.Add('  edge [arrowhead=vee, arrowsize=0.6];');
    L.Add('');

    // Lista simple de usuarios
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

  // Intentar generar el PNG con Graphviz
  okExec := RunDotToPNG(RutaDot, RutaPNG, Err);

  if okExec then
    Result := 'Reporte generado: ' + RutaDot + LineEnding +
              'Imagen generada:  ' + RutaPNG
  else
  begin
    // Mensaje claro si falta Graphviz o si dot falló
    if Err = '' then
      Err := 'Instala Graphviz (paquete "graphviz") o verifica que "dot" esté en el PATH.';
    Result := 'Reporte generado: ' + RutaDot + LineEnding +
              'No se pudo generar el PNG.' + LineEnding +
              'Detalle: ' + Err;
  end;
end;

end.

