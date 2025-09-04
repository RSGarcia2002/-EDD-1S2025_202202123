unit UnitMenuUsuario;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  { TFormMenuUsuario }
  TFormMenuUsuario = class(TForm)
    btnBandeja: TButton;
    btnPapelera: TButton;
    btnContactos: TButton;
    btnEnviar: TButton;
    btnSalir: TButton;
    btnPerfil: TButton;
    btnProgramar: TButton;
    btnProgramados: TButton;
    btnReportes: TButton;
    lblBienvenida: TLabel;
    procedure btnBandejaClick(Sender: TObject);
    procedure btnContactosClick(Sender: TObject);
    procedure btnEnviarClick(Sender: TObject);
    procedure btnPapeleraClick(Sender: TObject);
    procedure btnPerfilClick(Sender: TObject);
    procedure btnProgramadosClick(Sender: TObject);
    procedure btnProgramarClick(Sender: TObject);
    procedure btnReportesClick(Sender: TObject);
    procedure btnSalirClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    procedure Configurar(const Nombre, Usuario: string);
  end;

var
  FormMenuUsuario: TFormMenuUsuario;

implementation

{$R *.lfm}

uses
  UnitBandeja, UnitPapelera, UnitContactos, UnitEnviarCorreo, UDataCore, UnitPerfil,
  UnitProgramarCorreo, UnitProgramados, UnitReportesUsuario;

procedure TFormMenuUsuario.FormCreate(Sender: TObject);
begin
  Caption := 'Menú Usuario';
end;

procedure TFormMenuUsuario.Configurar(const Nombre, Usuario: string);
begin
  lblBienvenida.Caption := 'Bienvenido, ' + Nombre + ' (' + Usuario + ')';
end;

procedure TFormMenuUsuario.btnBandejaClick(Sender: TObject);
begin
  EnsureDemoInboxFor(CurrentUserEmail);

  if not Assigned(FormBandeja) then
    Application.CreateForm(TFormBandeja, FormBandeja);

  Inbox_ToStringsFor(FormBandeja.lbBandeja.Items, CurrentUserEmail);
  FormBandeja.Position := poScreenCenter;
  FormBandeja.ShowModal;
end;

procedure TFormMenuUsuario.btnPapeleraClick(Sender: TObject);
begin
  if not Assigned(FormPapelera) then
    Application.CreateForm(TFormPapelera, FormPapelera);
  Trash_ToStrings(FormPapelera.lbPapelera.Items);
  FormPapelera.ShowModal;
end;

procedure TFormMenuUsuario.btnPerfilClick(Sender: TObject);
begin
  if not Assigned(FormPerfil) then
    Application.CreateForm(TFormPerfil, FormPerfil);
    FormPerfil.Refrescar;
  FormPerfil.ShowModal;
end;

procedure TFormMenuUsuario.btnProgramadosClick(Sender: TObject);
begin
  if not Assigned(FormProgramados) then
    Application.CreateForm(TFormProgramados, FormProgramados);
  FormProgramados.ShowModal;
end;

procedure TFormMenuUsuario.btnProgramarClick(Sender: TObject);
begin
  if not Assigned(FormProgramarCorreo) then
    Application.CreateForm(TFormProgramarCorreo, FormProgramarCorreo);
  FormProgramarCorreo.ShowModal;
end;

procedure TFormMenuUsuario.btnReportesClick(Sender: TObject);
begin
  if not Assigned(FormReportesUsuario) then
    Application.CreateForm(TFormReportesUsuario, FormReportesUsuario);
  FormReportesUsuario.ShowModal;
end;

procedure TFormMenuUsuario.btnContactosClick(Sender: TObject);
begin
  if not Assigned(FormContactos) then
    Application.CreateForm(TFormContactos, FormContactos);

  FormContactos.Position := poScreenCenter;
  FormContactos.Refrescar;
  FormContactos.ShowModal;
end;

procedure TFormMenuUsuario.btnEnviarClick(Sender: TObject);
begin
  if not Assigned(FormEnviarCorreo) then
    Application.CreateForm(TFormEnviarCorreo, FormEnviarCorreo);
  // sugerencia: autocompletar destinatario con el último contacto seleccionado
  FormEnviarCorreo.ShowModal;
end;

procedure TFormMenuUsuario.btnSalirClick(Sender: TObject);
begin
  CurrentUserEmail := '';
  Close;
end;

end.

