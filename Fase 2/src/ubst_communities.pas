unit UBST_Communities;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils;

type
  TComunidad = record
    Nombre: string;
    FechaCreacion: TDateTime;
    NumMensajes: Integer;
  end;

  PMsg = ^TMsg;
  TMsg = record
    Correo, Texto, FechaPub: string;
    Next: PMsg;
  end;

  PCBST = ^TCBST;
  TCBST = record
    Key : string; // Nombre comunidad
    Data: TComunidad;
    MsgHead: PMsg; // lista simple de mensajes
    L, R: PCBST;
  end;

procedure CBST_Init(var Root: PCBST);
procedure CBST_Insert_WIP(var Root: PCBST; const Nombre: string);
procedure CBST_ToDot(Root: PCBST; const FilePath: string);

implementation

procedure CBST_Init(var Root: PCBST); begin Root := nil; end;

procedure CBST_Insert_WIP(var Root: PCBST; const Nombre: string);
var n: PCBST;
begin
  New(n); n^.Key:=Nombre;
  n^.Data.Nombre:=Nombre; n^.Data.FechaCreacion:=Now; n^.Data.NumMensajes:=0;
  n^.MsgHead:=nil; n^.L:=nil; n^.R:=nil;
  if Root=nil then Root:=n else begin n^.R:=Root; Root:=n; end;
end;

procedure _walk(N: PCBST; SL: TStrings);
begin
  if N=nil then exit;
  SL.Add(Format('  "%s" [label="%s | %s | %d msg"];',
        [N^.Key, N^.Data.Nombre, DateTimeToStr(N^.Data.FechaCreacion), N^.Data.NumMensajes]));
  if N^.L<>nil then SL.Add(Format('  "%s" -> "%s";',[N^.Key,N^.L^.Key]));
  if N^.R<>nil then SL.Add(Format('  "%s" -> "%s";',[N^.Key,N^.R^.Key]));
  _walk(N^.L, SL); _walk(N^.R, SL);
end;

procedure CBST_ToDot(Root: PCBST; const FilePath: string);
var SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add('digraph BST_Comunidades { node [shape=record, fillcolor="#F3E5F5", style=filled];');
    if Root=nil then SL.Add('  empty [label="(sin comunidades)"];') else _walk(Root, SL);
    SL.Add('}');
    SL.SaveToFile(FilePath);
  finally
    SL.Free;
  end;
end;

end.

