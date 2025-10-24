unit UDomain;

{$mode objfpc}{$H+}

interface

uses SysUtils, UAVL_Borradores,UBST_Contacts, UBTree_Favoritos,UBST_Communities;

var
  GlobalDrafts      : PAVL = nil;   // raíz del AVL de borradores
  NextDraftId       : LongInt = 1;  // contador simple de IDs
  CurrentUserEmail  : string = '';  // lo setearás al hacer login
  GlobalContacts : PBSNode = nil;
  GGlobalCommunities: PCBST;
  GlobalFavs: PBNode = nil;
  _CurrentUserEmail: string = '';

procedure Domain_InitDrafts; inline;
function  Domain_NewDraftId: LongInt; inline;
procedure Domain_InitContacts; inline;
procedure Domain_InitFavs; inline;

procedure Domain_SetCurrentUser(const Email: string);
function  Domain_GetCurrentUser: string;
procedure Domain_ClearCurrentUser;

procedure Domain_InitCommunities;


implementation

procedure Domain_InitDrafts;
begin
  AVL_Init(GlobalDrafts);
  NextDraftId := 1;
end;
procedure Domain_InitContacts;
begin
  BST_Init(GlobalContacts);
end;


function Domain_NewDraftId: LongInt;
begin
  Result := NextDraftId;
  Inc(NextDraftId);
end;
procedure Domain_InitFavs;
begin
  BTree_Init(GlobalFavs);
end;
procedure Domain_SetCurrentUser(const Email: string);
begin
  _CurrentUserEmail := LowerCase(Trim(Email));
end;
function Domain_GetCurrentUser: string;
begin
  Result := _CurrentUserEmail;
end;

procedure Domain_ClearCurrentUser;
begin
  _CurrentUserEmail := '';
end;

procedure Domain_InitCommunities;
begin
  CBST_Init(GlobalCommunities);
end;

end.

