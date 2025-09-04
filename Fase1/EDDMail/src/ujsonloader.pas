unit UJSONLoader;

{$mode ObjFPC}{$H+}

interface

uses Classes, SysUtils;

function CargarMasivaDesdeCarpeta(const AFolder: string): string;

implementation

uses fpjson, jsonparser, UDataCore;

function LoadJSONFromFile(const AFile: string): TJSONData;
var s: TStringStream;
begin
  Result := nil;
  if not FileExists(AFile) then Exit;
  s := TStringStream.Create('');
  try
    s.LoadFromFile(AFile);
    Result := GetJSON(s.DataString);
  finally
    s.Free;
  end;
end;

function JsonToString(const V: TJSONData): string;
begin
  if V = nil then Exit('');
  case V.JSONType of
    jtNull: Exit('');
    jtBoolean: Exit(BoolToStr(TJSONBoolean(V).AsBoolean, True));
    jtNumber: Exit(TJSONNumber(V).AsString);
    jtString: Exit(TJSONString(V).AsString);
  else
    Result := V.AsJSON;
  end;
end;

function CargarUsuarios(const AFile: string; out CantOK, CantErr: Integer): string;
var data, arrNode: TJSONData; arr: TJSONArray; i: Integer;
    obj: TJSONObject; id,nombre,usuario,email,tel,passw: string; errores: TStringList;
begin
  CantOK := 0; CantErr := 0;
  errores := TStringList.Create;
  data := LoadJSONFromFile(AFile);
  try
    if data = nil then Exit('No se encontró: ' + AFile);

    if data.JSONType = jtObject then
    begin
      arrNode := TJSONObject(data).Find('usuarios');
      if (arrNode=nil) or (arrNode.JSONType<>jtArray) then
        Exit('Raíz objeto pero falta arreglo "usuarios".');
      arr := TJSONArray(arrNode);
    end
    else if data.JSONType = jtArray then
      arr := TJSONArray(data)
    else
      Exit('JSON raíz debe ser objeto con "usuarios" o arreglo directo.');

    for i := 0 to arr.Count - 1 do
    begin
      if not (arr.Items[i] is TJSONObject) then begin
        Inc(CantErr); errores.Add(Format('Índice %d: no es objeto JSON',[i])); Continue;
      end;
      obj := TJSONObject(arr.Items[i]);

      id      := Trim(JsonToString(obj.Find('id')));
      nombre  := Trim(obj.Get('nombre',''));
      usuario := Trim(obj.Get('usuario',''));
      email   := Trim(obj.Get('email',''));
      tel     := Trim(obj.Get('telefono',''));
      passw   := Trim(obj.Get('password','')); // opcional

      if id='' then begin
        Inc(CantErr); errores.Add(Format('Índice %d: "id" vacío',[i])); Continue;
      end;

      UserList_AddOrUpdate(id, nombre, usuario, email, tel, passw);
      Inc(CantOK);
    end;

    if errores.Count>0 then
      Result := Format('Usuarios cargados=%d, errores=%d',[CantOK,CantErr]) + LineEnding +
                'Detalle:' + LineEnding + errores.Text
    else
      Result := 'Usuarios cargados correctamente: ' + IntToStr(CantOK);
  finally
    data.Free;
    errores.Free;
  end;
end;

function CargarMasivaDesdeCarpeta(const AFolder: string): string;
var fUsers: string; okU,errU: Integer; rU: string;
begin
  fUsers := IncludeTrailingPathDelimiter(AFolder) + 'users.json';
  rU := CargarUsuarios(fUsers, okU, errU);
  Result := 'Resumen de Carga Masiva' + LineEnding +
            '=======================' + LineEnding +
            rU;
end;

end.

