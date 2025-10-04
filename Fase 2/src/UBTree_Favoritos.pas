unit UBTree_Favoritos;

{$mode objfpc}{$H+}

interface
uses Classes, SysUtils;

type
  TMailFav = record
    Id: LongInt; Remitente, Destinatario, Asunto, Mensaje, Fecha: string;
  end;

  // Stub de nodo B-Tree (orden 5) - por ahora solo almacenaremos lista plana para reporte WIP
  PBNode = ^TBNode;
  TBNode = record
    Count : Integer;
    Keys  : array[1..4] of LongInt; // IDs
    Next  : PBNode; // lista simple temporal para WIP
  end;

procedure BTree_Init(var Head: PBNode);
procedure BTree_Insert_WIP(var Head: PBNode; const M: TMailFav);
procedure BTree_ToDot_WIP(Head: PBNode; const FilePath: string);

implementation

procedure BTree_Init(var Head: PBNode); begin Head := nil; end;

procedure BTree_Insert_WIP(var Head: PBNode; const M: TMailFav);
var n: PBNode;
begin
  // WIP: apilar bloques de hasta 4 IDs en lista lineal
  if (Head=nil) or (Head^.Count=4) then begin
    New(n); n^.Count:=0; n^.Next:=Head; Head:=n;
  end;
  Inc(Head^.Count);
  Head^.Keys[Head^.Count] := M.Id;
end;

procedure BTree_ToDot_WIP(Head: PBNode; const FilePath: string);
var SL: TStringList; i, idx: Integer; cur: PBNode;
begin
  SL := TStringList.Create;
  try
    SL.Add('digraph BTree_Favoritos { node [shape=record, fillcolor="#FFF3E0", style=filled]; rankdir=TB;');
    if Head=nil then SL.Add('  empty [label="(sin favoritos)"];')
    else begin
      idx:=0; cur:=Head;
      while cur<>nil do begin
        Inc(idx);
        SL.Add(Format('  "blk%d" [label="', [idx]));
        for i:=1 to cur^.Count do
        begin
          if i>1 then SL[SL.Count-1] := SL[SL.Count-1] + '|';
          SL[SL.Count-1] := SL[SL.Count-1] + IntToStr(cur^.Keys[i]);
        end;
        SL[SL.Count-1] := SL[SL.Count-1] + '"];';
        if (cur^.Next<>nil) then SL.Add(Format('  "blk%d" -> "blk%d";',[idx, idx+1]));
        cur:=cur^.Next;
      end;
    end;
    SL.Add('}');
    SL.SaveToFile(FilePath);
  finally
    SL.Free;
  end;
end;

end.
