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

// ...declaraciones de CBST_Init/Insert_WIP/ToDot aquí...

implementation

