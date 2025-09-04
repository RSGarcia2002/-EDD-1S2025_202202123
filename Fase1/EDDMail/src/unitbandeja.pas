unit UnitBandeja;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TFormBandeja = class(TForm)
    btnCerrar: TButton;
    btnEliminar: TButton;
    btnOrdenar: TButton;
    lbBandeja: TListBox;
    procedure FormShow(Sender: TObject);
    procedure btnCerrarClick(Sender: TObject);
    procedure btnEliminarClick(Sender: TObject);
    procedure btnOrdenarClick(Sender: TObject);
    procedure lbBandejaDblClick(Sender: TObject);
  private
    procedure CargarLista;
  public
  end;

var
  FormBandeja: TFormBandeja;

implementation

{$R *.lfm}

uses
  UDataCore, UnitPapelera;

procedure TFormBandeja.CargarLista;
begin
  // SIEMPRE filtrado por el usuario logueado
  Inbox_ToStringsFor(lbBandeja.Items, CurrentUserEmail);
end;

procedure TFormBandeja.FormShow(Sender: TObject);
begin
  CargarLista;
end;

procedure TFormBandeja.btnCerrarClick(Sender: TObject);
begin
  Close;
end;

procedure TFormBandeja.btnEliminarClick(Sender: TObject);
var
  idx1: Integer;
  node: PMailNode;
  id, rem, dest, asu, fec, msg, est: string;
begin
  if lbBandeja.ItemIndex < 0 then
  begin
    MessageDlg('Selecciona un correo primero.', mtWarning, [mbOK], 0);
    Exit;
  end;

  idx1 := lbBandeja.ItemIndex + 1;               // índice 1-based del subconjunto
  node := Inbox_GetNthFor(CurrentUserEmail, idx1);

  if (node <> nil) and Inbox_RemoveNode(node, id, rem, dest, asu, fec, msg, est) then
  begin
    Trash_Push(id, rem, dest, asu, fec, msg, est);
    // refrescar papelera si existe
    if Assigned(FormPapelera) then
      Trash_ToStrings(FormPapelera.lbPapelera.Items);
    CargarLista;
    MessageDlg('Correo movido a Papelera.', mtInformation, [mbOK], 0);
  end
  else
    MessageDlg('No se pudo eliminar el correo.', mtError, [mbOK], 0);
end;

procedure TFormBandeja.btnOrdenarClick(Sender: TObject);
var
  cur: PMailNode;
  tmp: TStringList;
  i: Integer;
  key, display, sep: string;
begin
  // Ordena SOLO lo del usuario actual (vista filtrada)
  tmp := TStringList.Create;
  try
    sep := '||';
    cur := InboxHead;
    while cur <> nil do
    begin
      if SameText(cur^.Destinatario, CurrentUserEmail) then
      begin
        key := LowerCase(cur^.Asunto);
        display := Format('[%s] %s - %s (%s)', [cur^.Estado, cur^.Remitente, cur^.Asunto, cur^.Fecha]);
        tmp.Add(key + sep + display);
      end;
      cur := cur^.Next;
    end;

    tmp.Sort;

    lbBandeja.Items.BeginUpdate;
    try
      lbBandeja.Items.Clear;
      for i := 0 to tmp.Count - 1 do
      begin
        display := Copy(tmp[i], Pos(sep, tmp[i]) + Length(sep), MaxInt);
        lbBandeja.Items.Add(Format('%d) %s', [i + 1, display]));
      end;
    finally
      lbBandeja.Items.EndUpdate;
    end;
  finally
    tmp.Free;
  end;
end;

procedure TFormBandeja.lbBandejaDblClick(Sender: TObject);
var
  idx1: Integer;
  node: PMailNode;
begin
  if lbBandeja.ItemIndex < 0 then Exit;

  idx1 := lbBandeja.ItemIndex + 1;
  node := Inbox_GetNthFor(CurrentUserEmail, idx1);
  if node = nil then Exit;

  if node^.Estado = 'NL' then node^.Estado := 'L' else node^.Estado := 'NL';
  CargarLista;
end;

end.

