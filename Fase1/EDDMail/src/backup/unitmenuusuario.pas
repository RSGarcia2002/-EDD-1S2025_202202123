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
    btnBorradores: TButton;
    btnFavoritos: TButton;
    lblBienvenida: TLabel;
    procedure btnBandejaClick(Sender: TObject);
    procedure btnBandejaEnClick(Sender: TObject);
    procedure btnBorradoresClick(Sender: TObject);
    procedure btnContactosClick(Sender: TObject);
    procedure btnEnviarClick(Sender: TObject);
    procedure btnFavoritosClick(Sender: TObject);
    procedure btnPapeleraClick(Sender: TObject);
    procedure btnPerfilClick(Sender: TObject);
    procedure btnProgramadosClick(Sender: TObject);
    procedure btnProgramarClick(Sender: TObject);
    procedure btnReportesClick(Sender: TObject);
    procedure btnSalirClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
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
  UnitProgramarCorreo, UnitProgramados, UnitReportesUsuario,UnitBorradoresVista, UnitInbox, UDomain, UnitFavoritos ;

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
  EnsureDemoInboxFor(Domain_GetCurrentUser);

  if not Assigned(FormBandeja) then
    Application.CreateForm(TFormBandeja, FormBandeja);

  Inbox_ToStringsFor(FormBandeja.lbBandeja.Items, Domain_GetCurrentUser);
  FormBandeja.Position := poScreenCenter;
  FormBandeja.ShowModal;
end;

procedure TFormMenuUsuario.btnBandejaEnClick(Sender: TObject);
begin
  if not Assigned(FormInbox) then
    Application.CreateForm(TFormInbox, FormInbox);
  FormInbox.Position := poScreenCenter;
  FormInbox.ShowModal;

end;

procedure TFormMenuUsuario.btnBorradoresClick(Sender: TObject);
begin
  if not Assigned(FormBorradoresVista) then
    Application.CreateForm(TFormBorradoresVista, FormBorradoresVista);
  FormBorradoresVista.Position := poScreenCenter;
  FormBorradoresVista.ShowModal; // solo visualización
end;

procedure TFormMenuUsuario.btnPapeleraClick(Sender: TObject);
begin
  if not Assigned(FormPapelera) then
    Application.CreateForm(TFormPapelera, FormPapelera);
  FormPapelera.Refrescar;
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
  if not Assigned(FormPerfil) then
    Application.CreateForm(TFormContactos, FormContactos);
  FormContactos.ShowModal;
end;

procedure TFormMenuUsuario.btnEnviarClick(Sender: TObject);
begin
  if not Assigned(FormEnviarCorreo) then
    Application.CreateForm(TFormEnviarCorreo, FormEnviarCorreo);
  // sugerencia: autocompletar destinatario con el último contacto seleccionado
  FormEnviarCorreo.ShowModal;
end;

procedure TFormMenuUsuario.btnFavoritosClick(Sender: TObject);
begin
  if not Assigned(FormFavoritos) then
    Application.CreateForm(TFormFavoritos, FormFavoritos);
  FormFavoritos.Position := poScreenCenter;
  FormFavoritos.ShowModal;
end;

procedure TFormMenuUsuario.btnSalirClick(Sender: TObject);
begin
  Domain_ClearCurrentUser;
  Close;
end;

procedure TFormMenuUsuario.Button1Click(Sender: TObject);
begin

end;

end.

