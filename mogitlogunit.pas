unit mogitlogunit;
{$mode objfpc}{$H+}
interface
uses
  Classes, Sysutils,
  mui, amigados,
  MUIClass.Base, MUIClass.Window, MUIClass.Gadget,
  MUIClass.List, MUIClass.Group, MUIClass.Area,
  gitunit;

type
  TCommit = record
    Hash: string;
    FullHash: string;
    Date: string;
    Author: string;
    Message: string;
    Files: TStringArray;
  end;
  TCommits= array of TCommit;


  TLogWindow = class(TMUIWindow)
  private
    CommitList, MessageList, FileList: TMUIListView;
    Commits: TCommits;
  public
    constructor Create; override;
    procedure DisplayEvent(Sender: TObject; ToPrint: PPChar; Entry: PChar);
    procedure ButtonClick(Sender: TObject);
    procedure SelectEvent(Sender: TObject);
    procedure FileClick(Sender: TObject);
  end;
  
implementation

uses
  globalunit;

constructor TLogWindow.Create;

begin
  inherited;
  
  Width := MUIV_Window_Width_Visible(50);  
  OnShow := @ButtonClick; 
  
  Title := 'Git Log for "'+SrcPath+'"'; 
  
  with TMUIText.Create do
  begin
    Contents := SrcPath;
    Parent := self;
  end;
  
  CommitList := TMUIListView.Create;
  with CommitList do
  begin
    List := TMUIList.Create;
    List.OnDisplay := @DisplayEvent;
    List.Format := ',,,';
    List.Title := #1;
    List.OnActiveChange := @SelectEvent;
    Parent := Self;
  end;
  
  MessageList := TMUIListView.Create;
  with MessageList do
  begin
    List := TMUIFloatText.Create;
    Parent := Self;
  end;
  
  FileList := TMUIListView.Create;
  with FileList do
  begin
    List := TMUIList.Create;
    OnDoubleClick := @FileClick;
    Parent := Self;
  end;
  
end;

var
  Titles: array[0..3] of PChar = ('Hash', 'Date', 'Author', 'Message');

procedure TLogWindow.DisplayEvent(Sender: TObject; ToPrint: PPChar; Entry: PChar);
var
  Idx: Integer;
  p: PLongInt;
begin
  P := PLongInt(ToPrint);
  Dec(P);
  Idx := P^;
  if (Idx >= 0) and (Idx < Length(Commits)) and Assigned(Entry) then
  begin
    ToPrint[0] := PChar(Commits[Idx].Hash);
    ToPrint[1] := PChar(Commits[Idx].Date);
    ToPrint[2] := PChar(Commits[Idx].Author);
    ToPrint[3] := PChar(Commits[Idx].Message);
  end
  else
  begin
    ToPrint[0] := PChar(Titles[0]);
    ToPrint[1] := PChar(Titles[1]);
    ToPrint[2] := PChar(Titles[2]);
    ToPrint[3] := PChar(Titles[3]);
  end;
end;

procedure TLogWindow.ButtonClick(Sender: TObject);
var
  i: Integer;
  LE: TLogEntries;
begin
	LE := GitLog(SrcPath);
	SetLength(Commits, Length(LE));
	for i := 0 to High(LE) do
	begin
		Commits[i].Hash := LE[i].Hash;
		Commits[i].FullHash := LE[i].FullHash;
		Commits[i].Date := LE[i].Date;
		Commits[i].Author := LE[i].Author;
		Commits[i].Message := LE[i].Msg; 
	end; 
	CommitList.List.Quiet := True;
	// Clear Property and Event list
	while CommitList.List.Entries > 0 do
		CommitList.List.Remove(MUIV_List_Remove_Last);
	for i := 0 to High(Commits) do
		CommitList.List.InsertSingle(PChar(Commits[i].Hash), MUIV_List_Insert_Bottom);  
	CommitList.List.Quiet := False;
end;

procedure TLogWindow.SelectEvent(Sender: TObject);
var
  Idx, i: Integer;
begin
  TMUIFloatText(MessageList.List).Text := '';
  Idx := CommitList.List.Active;
  if (Idx >= 0) and (Idx <= High(Commits)) then
  begin
    if Length(Commits[Idx].Files) = 0 then
    begin
			Commits[Idx].Files := GitChangedFiles(SrcPath,Commits[Idx].FullHash);
			if Length(Commits[Idx].Files) = 0 then
			begin
			  SetLength(Commits[Idx].Files, 1);
			  Commits[Idx].Files[0] := ' ';
			end;
		end;
		//
		TMUIFloatText(MessageList.List).Text := Commits[Idx].Message;
		FileList.List.Quiet := True;
    // Clear Property and Event list
    while FileList.List.Entries > 0 do
      FileList.List.Remove(MUIV_List_Remove_Last);
    for i := 0 to High(Commits[Idx].Files) do
      FileList.List.InsertSingle(PChar(Commits[Idx].Files[i]), MUIV_List_Insert_Bottom);  
    FileList.List.Quiet := False;
  end;
end;

procedure TLogWindow.FileClick(Sender: TObject);
var
  IdxC, IdxF: Integer;
  Hash, FileN: string;
  FName: string;
begin
  IdxC := CommitList.List.Active;
  IdxF := FileList.List.Active;
  if (IdxC < 0) or (IdxC > High(Commits)) then
    Exit;
  if (IdxF < 0) or (IdxF > High(Commits[IdxC].Files)) then
    Exit;
  Hash := Commits[IdxC].FullHash;
  FileN := Commits[IdxC].Files[IdxF];
  //
  FName := GitDiff(SrcPath, FileN, Hash);
  if FName <> '' then
    SysUtils.ExecuteProcess(Editor, FName);
end;

end.
