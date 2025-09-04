unit UnitMenuRoot;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls;

type

  { TFormMenuRoot }

  TFormMenuRoot = class(TForm)
    btnCargaMasiva: TButton;
    btnReporteUsuarios: TButton;
    btnReporteRelaciones: TButton;
    btnCerrarSesion: TButton;
    btnComuCrear: TButton;
    btnComuAgregarUsuario: TButton;
    btnComuReporte: TButton;
    procedure btnCerrarSesionClick(Sender: TObject);
    procedure btnComuCrearClick(Sender: TObject);
    procedure btnComuReporteClick(Sender: TObject);
    procedure btnComuAgregarUsuarioClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCargaMasivaClick(Sender: TObject);
    procedure btnReporteUsuariosClick(Sender: TObject);
    procedure btnReporteRelacionesClick(Sender: TObject);
  private
  public
  end;

implementation

{$R *.lfm}

uses Dialogs, UJSONLoader, UReportes, UDataCore, UComunidades, UComunidadesAdapters ;



procedure TFormMenuRoot.btnCerrarSesionClick(Sender: TObject);
begin
  ModalResult := mrClose;
end;

procedure TFormMenuRoot.btnComuCrearClick(Sender: TObject);
var
  nom: string;
begin
  nom := ''; // valor por defecto para InputQuery
  if not InputQuery('Crear comunidad', 'Nombre de comunidad:', nom) then Exit;
  nom := Trim(nom);
  if nom = '' then Exit;

  // CrearComunidad devuelve PCommunity -> comparar contra nil
  if CrearComunidad(nom) <> nil then
    MessageDlg('Comunidad creada: ' + nom, mtInformation, [mbOK], 0)
  else
    MessageDlg('Ya existe una comunidad con ese nombre.', mtWarning, [mbOK], 0);
end;


procedure TFormMenuRoot.btnComuReporteClick(Sender: TObject);
var
  dir, dotFile, pngFile: string;
begin
  dir     := RootReportsDir; // helper que ya tienes
  ExportarReporteComunidadesDOT(dir); // genera Root-Reportes/Reporte_Comunidades.dot

  // (opcional) si tienes Graphviz, intentar PNG
  dotFile := IncludeTrailingPathDelimiter(dir) + 'Reporte_Comunidades.dot';
  pngFile := IncludeTrailingPathDelimiter(dir) + 'Reporte_Comunidades.png';
  TryGeneratePNGFromDOT(dotFile, pngFile);

  MessageDlg('Reporte generado en: ' + LineEnding + dotFile, mtInformation, [mbOK], 0);
end;

procedure TFormMenuRoot.btnComuAgregarUsuarioClick(Sender: TObject);
var
  comu, email: string;
  u: PUserNode;
begin
  comu  := '';
  email := '';
  if not InputQuery('Agregar a comunidad', 'Nombre de comunidad:', comu) then Exit;
  if not InputQuery('Agregar a comunidad', 'Email del usuario:', email) then Exit;

  comu  := Trim(comu);
  email := Trim(email);
  if (comu = '') or (email = '') then Exit;

  u := User_FindByEmail(email);
  if u = nil then
  begin
    MessageDlg('Ese email no existe como usuario.', mtError, [mbOK], 0);
    Exit;
  end;

  // AddUserToCommunity_StrId devuelve Boolean
  if AddUserToCommunity_StrId(comu, u^.Id, u^.Usuario) then
    MessageDlg(Format('Se agregó %s (%s) a %s', [u^.Usuario, u^.Id, comu]),
               mtInformation, [mbOK], 0)
  else
    MessageDlg('No se pudo agregar (no existe la comunidad o ya está dentro).',
               mtWarning, [mbOK], 0);
end;

procedure TFormMenuRoot.btnCargaMasivaClick(Sender: TObject);
var folder, resumen: string;
begin
  if SelectDirectory('Selecciona la carpeta con users.json', '', folder) then
  begin
    resumen := CargarMasivaDesdeCarpeta(folder);
    MessageDlg('Resultado Carga Masiva', resumen, mtInformation, [mbOK], 0);
  end;
end;

procedure TFormMenuRoot.btnReporteUsuariosClick(Sender: TObject);
var dot, png, msg: string;
begin
  msg := ReporteUsuarios_Generar('root', dot, png);
  ShowMessage(msg);
   RunCommand('xdg-open "'+png+'"', ...);
end;

procedure TFormMenuRoot.btnReporteRelacionesClick(Sender: TObject);
var
  dir, fileDot: string;
begin
  // Carpeta de reportes del root (junto al ejecutable)
  dir     := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)) + 'Root-Reportes');
  fileDot := dir + 'relaciones.dot';

  Relations_GenerateDot(fileDot);

  MessageDlg('Reporte generado:' + LineEnding + fileDot + LineEnding +
             '(Usa Graphviz para exportar a PNG:' + LineEnding +
             'dot -Tpng -O "' + fileDot + '")',
             mtInformation, [mbOK], 0);
end;

procedure TFormMenuRoot.FormCreate(Sender: TObject);
begin
  Caption := 'Menú Root';
  InitComunidades;
end;

end.

