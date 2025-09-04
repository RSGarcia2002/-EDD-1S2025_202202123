unit UnitProgramados;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormProgramados }

  TFormProgramados = class(TForm)
    btnEnviarAhora: TButton;
    btnEliminarSel: TButton;
    btnCerrar: TButton;
    lbProgramados: TListBox;
    procedure FormShow(Sender: TObject);
    procedure btnCerrarClick(Sender: TObject);
    procedure btnEliminarSelClick(Sender: TObject);
    procedure btnEnviarAhoraClick(Sender: TObject);
    procedure lbProgramadosClick(Sender: TObject);
  private
    procedure Cargar;
  public
  end;

var
  FormProgramados: TFormProgramados;

implementation

{$R *.lfm}

uses UDataCore, UnitBandeja;

procedure TFormProgramados.Cargar;
begin
  Prog_ToStrings(lbProgramados.Items);
end;

procedure TFormProgramados.FormShow(Sender: TObject);
begin
  Cargar;
end;

procedure TFormProgramados.btnCerrarClick(Sender: TObject);
begin
  Close;
end;

procedure TFormProgramados.btnEliminarSelClick(Sender: TObject);
var idx: Integer;
begin
  if lbProgramados.ItemIndex<0 then
  begin
    MessageDlg('Selecciona un elemento.', mtWarning,[mbOK],0);
    Exit;
  end;
  idx := lbProgramados.ItemIndex + 1;
  Prog_RemoveAt(idx);
  Cargar;
end;

procedure TFormProgramados.btnEnviarAhoraClick(Sender: TObject);
begin
  if Prog_Count=0 then
  begin
    MessageDlg('No hay correos programados.', mtInformation,[mbOK],0);
    Exit;
  end;
  Prog_SendAllNow;
  Cargar;
  if Assigned(FormBandeja) then
    Inbox_ToStringsFor(FormBandeja.lbBandeja.Items, CurrentUserEmail);
  MessageDlg('Programados enviados (simulación).', mtInformation,[mbOK],0);
end;

procedure TFormProgramados.lbProgramadosClick(Sender: TObject);
begin

end;



end.

