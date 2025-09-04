unit UnitPapelera;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TFormPapelera = class(TForm)
    btnRestaurar: TButton;
    btnVaciar: TButton;
    btnCerrar: TButton;
    btnBuscar: TButton;
    btnEliminarSel: TButton;
    edtBuscar: TEdit;
    lbPapelera: TListBox;
    procedure FormShow(Sender: TObject);
    procedure btnCerrarClick(Sender: TObject);
    procedure btnRestaurarClick(Sender: TObject);
    procedure btnVaciarClick(Sender: TObject);
    procedure btnBuscarClick(Sender: TObject);
    procedure btnEliminarSelClick(Sender: TObject);
  private
    procedure CargarLista;
  public
    procedure Refrescar;
  end;

var
  FormPapelera: TFormPapelera;

implementation

{$R *.lfm}

uses
  UDataCore, UnitBandeja;

procedure TFormPapelera.CargarLista;
begin
  // lista completa sin filtro
  Trash_ToStrings(lbPapelera.Items);
end;

procedure TFormPapelera.Refrescar;
begin
  CargarLista;
end;

procedure TFormPapelera.FormShow(Sender: TObject);
begin
  CargarLista;
end;

procedure TFormPapelera.btnCerrarClick(Sender: TObject);
begin
  Close;
end;

procedure TFormPapelera.btnVaciarClick(Sender: TObject);
begin
  if Trash_Count = 0 then
  begin
    MessageDlg('La papelera ya está vacía.', mtInformation, [mbOK], 0);
    Exit;
  end;
  if MessageDlg('¿Vaciar papelera?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    Trash_Clear;
    CargarLista;
  end;
end;

procedure TFormPapelera.btnBuscarClick(Sender: TObject);
begin
  Trash_ToStringsFiltered(lbPapelera.Items, edtBuscar.Text);
end;

procedure TFormPapelera.btnEliminarSelClick(Sender: TObject);
var
  idx1: Integer;
begin
  if lbPapelera.ItemIndex < 0 then
  begin
    MessageDlg('Selecciona un elemento primero.', mtWarning, [mbOK], 0);
    Exit;
  end;

  // soporta tanto lista completa como filtrada porque nuestra numeración empieza en 1
  idx1 := lbPapelera.ItemIndex + 1;

  if MessageDlg('Eliminar permanentemente el elemento seleccionado?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    if Trash_RemoveAt(idx1) then
    begin
      // recarga lista; si hay filtro activo, respétalo
      if Trim(edtBuscar.Text) = '' then
        CargarLista
      else
        Trash_ToStringsFiltered(lbPapelera.Items, edtBuscar.Text);
    end
    else
      MessageDlg('No se pudo eliminar (índice fuera de rango).', mtError, [mbOK], 0);
  end;
end;

procedure TFormPapelera.btnRestaurarClick(Sender: TObject);
var
  id, rem, dest, asu, fec, msg, est: string;
begin
  if not Trash_Pop(id, rem, dest, asu, fec, msg, est) then
  begin
    MessageDlg('No hay correos en la papelera.', mtInformation, [mbOK], 0);
    Exit;
  end;

  // restaurar al final de la bandeja
  Inbox_PushBack(id, rem, dest, asu, fec, msg);

  // refrescar listas
  if Trim(edtBuscar.Text) = '' then
    CargarLista
  else
    Trash_ToStringsFiltered(lbPapelera.Items, edtBuscar.Text);

  if Assigned(FormBandeja) then
    Inbox_ToStrings(FormBandeja.lbBandeja.Items);
end;

end.

