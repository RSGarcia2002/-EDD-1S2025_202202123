unit UnitReportesUsuario;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TFormReportesUsuario = class(TForm)
    btnCorreos: TButton;
    btnPapelera: TButton;
    btnProgramados: TButton;
    btnContactos: TButton;
    btnAbrirCarpeta: TButton;
    procedure btnAbrirCarpetaClick(Sender: TObject);
    procedure btnContactosClick(Sender: TObject);
    procedure btnCorreosClick(Sender: TObject);
    procedure btnPapeleraClick(Sender: TObject);
    procedure btnProgramadosClick(Sender: TObject);
    procedure btnCerrarClick(Sender: TObject);
  private
    procedure AvisoOk(const PathDot: string);
  public
  end;

var
  FormReportesUsuario: TFormReportesUsuario;

implementation

{$R *.lfm}

uses UDataCore, LCLIntf; // LCLIntf para OpenURL

procedure TFormReportesUsuario.AvisoOk(const PathDot: string);
begin
  MessageDlg('Reporte generado:' + LineEnding + PathDot + LineEnding + LineEnding +
    '(Abre el .dot con Graphviz o usa: dot -Tpng archivo.dot -o archivo.png)',
    mtInformation, [mbOK], 0);
end;

procedure TFormReportesUsuario.btnCorreosClick(Sender: TObject);
begin
  AvisoOk(Report_Inbox_DOT);
end;

procedure TFormReportesUsuario.btnPapeleraClick(Sender: TObject);
begin
  AvisoOk(Report_Trash_DOT);
end;

procedure TFormReportesUsuario.btnProgramadosClick(Sender: TObject);
begin
  AvisoOk(Report_Programados_DOT);
end;

procedure TFormReportesUsuario.btnContactosClick(Sender: TObject);
begin
  AvisoOk(Report_Contactos_DOT);
end;

procedure TFormReportesUsuario.btnAbrirCarpetaClick(Sender: TObject);
begin
  // Abre la carpeta en el explorador
  OpenURL('file:///' + UserReportsDir);
end;

procedure TFormReportesUsuario.btnCerrarClick(Sender: TObject);
begin
  Close;
end;

end.

