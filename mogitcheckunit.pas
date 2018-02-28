unit mogitcheckunit;
{$mode objfpc}{$H+}
interface
uses
  Classes, Sysutils, StrUtils,
  mui, amigados,
  MUIClass.Base, MUIClass.Window, MUIClass.Gadget,
  MUIClass.List, MUIClass.Group, MUIClass.Area,
  MUIClass.Image, MUIClass.Dialog,
  globalunit, gitunit;
  
type
  TCheckWindow = class(TMUIWindow)
  private
    FileList: TFileStatusList;
    procedure UpdateLists;

  public
    ChangeList: TMUIListView;
    ShowUntracked: TMUICheckMark;
    RefreshButton, StageButton, UnStageButton, RevertButton: TMUIButton;
     
    constructor Create; override;
    procedure DisplayEvent(Sender: TObject; ToPrint: PPChar; Entry: PChar);
    procedure ShowEvent(Sender: TObject);
    procedure StageClick(Sender: TObject);
    procedure UnStageClick(Sender: TObject);
    procedure RevertClick(Sender: TObject);
    procedure SelectEvent(Sender: TObject);
    procedure DblClickEvent(Sender: TObject);
  end;
  
implementation
  
constructor TCheckWindow.Create;
var
  Grp1: TMUIGroup;
begin
  inherited;
  Title := 'Git Check for "'+SrcPath+'"'; 
  Width := MUIV_Window_Width_Visible(50);
  OnShow := @ShowEvent;
  
  Grp1 := TMUIGroup.Create;
  with Grp1 do
  begin
    Frame := MUIV_Frame_Group;
    FrameTitle := 'Changed Files';
    Parent := Self;
  end;
  
  ChangeList := TMUIListView.Create;
  with ChangeList do
  begin
    List := TMUIList.Create;
    List.Title := #1;
    List.Format := ',,';
    List.OnDisplay := @DisplayEvent;
    List.OnActiveChange := @SelectEvent;
    OnDoubleClick := @DblClickEvent;
    Parent := Grp1; 
  end;
  
  Grp1 := TMUIGroup.Create;
  with Grp1 do
  begin
    Horiz := True;
    Parent := Self;
  end;
  
  StageButton := TMUIButton.Create;
  with StageButton.Create('Refresh') do
  begin
    OnClick := @ShowEvent;
    Parent := Grp1;
  end;
  
  ShowUntracked := TMUICheckMark.Create;
  with ShowUntracked do
  begin
    Selected := globalunit.ShowUntracked;
    OnSelected := @ShowEvent;
    Parent := Grp1;
  end;
  with TMUIText.Create('Show Untracked Files') do
  begin
    Frame := MUIV_Frame_None;
    Parent := Grp1;
  end;
  
  TMUIHSpace.Create(10).Parent := Grp1;
  
  StageButton := TMUIButton.Create;
  with StageButton.Create('Stage/Add') do
  begin
    OnClick := @StageClick;
    Disabled := True;
    Parent := Grp1;
  end;
  UnStageButton := TMUIButton.Create;
  with UnStageButton.Create('Unstage/Remove') do
  begin
    OnClick := @UnStageClick;
    Disabled := True;
    Parent := Grp1;
  end;  
  
  RevertButton := TMUIButton.Create;
  with RevertButton.Create('Revert') do
  begin
    OnClick := @RevertClick;
    Disabled := True;
    Parent := Grp1;
  end;  
   
end;

procedure TCheckWindow.StageClick(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := ChangeList.List.Active;
  if (Idx >= 0) and (Idx < Length(FileList)) then
  begin
    GitAdd(SrcPath, FileList[Idx].FileName);
    UpdateLists;
  end;
end;

procedure TCheckWindow.UnStageClick(Sender: TObject);
var
  Idx: Integer;
  p: Integer;
  FName: string;
begin
  Idx := ChangeList.List.Active;
  if (Idx >= 0) and (Idx < Length(FileList)) then
  begin
    FName := FileList[Idx].FileName;
    p := Pos('>', FName);
    if p > 0 then
    begin
      Delete(FName, 1, p);
      FName := Trim(FName);
    end;
    writeln(FName);
    GitUnAdd(SrcPath, FileList[Idx].FileName);
    UpdateLists;
  end;
end;

procedure TCheckWindow.RevertClick(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := ChangeList.List.Active;
  if (Idx >= 0) and (Idx < Length(FileList)) then
  begin
    if FileList[Idx].Staged then
      GitUnAdd(SrcPath, FileList[Idx].FileName);
    GitRevert(SrcPath, FileList[Idx].FileName);
    UpdateLists;
  end;
end;

procedure TCheckWindow.SelectEvent(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := ChangeList.List.Active;
  
  if (Idx >= 0) and (Idx < Length(FileList)) then
  begin  
    StageButton.Disabled := FileList[Idx].Staged;
    UnStageButton.Disabled := not StageButton.Disabled;
    RevertButton.Disabled := FileList[Idx].Status in [gsUnknown, gsUntracked, gsRenamed, gsAdded]; 
  end
  else
  begin
    StageButton.Disabled := True;
    UnStageButton.Disabled := True;
    RevertButton.Disabled := True;
  end;
end;

procedure TCheckWindow.DblClickEvent(Sender: TObject);
var
  Idx: Integer;
  FName: string;
begin
  Idx := ChangeList.List.Active;
  if (Idx >= 0) and (Idx < Length(FileList)) then
  begin
    if not (FileList[Idx].Status = gsModified) then
    begin
      ShowMessage('Diff viewing only works for modified Files');
      Exit;
    end;  
    writeln('diff of ', FileList[Idx].FileName);
    FName := GitDiff(SrcPath, FileList[Idx].FileName, '', FileList[Idx].Staged);
    writeln('diff filename: ', FName);
    if FName <> '' then
      SysUtils.ExecuteProcess(Editor, FName);
  end;
end;


procedure TCheckWindow.ShowEvent(Sender: TObject);
begin
  UpdateLists;
end;

procedure TCheckWindow.UpdateLists;
var
  i: Integer;
begin
  FileList := GitStatus(SrcPath, ShowUntracked.Selected);
  ChangeList.List.Quiet := True;
	// Clear Property and Event list
	while ChangeList.List.Entries > 0 do
		ChangeList.List.Remove(MUIV_List_Remove_Last);
	for i := 0 to High(FileList) do
		ChangeList.List.InsertSingle(PChar(FileList[i].FileName), MUIV_List_Insert_Bottom);  
	ChangeList.List.Quiet := False;
  
end;

const
  Titles: array[0..2] of string = ('Status', 'Staged', 'File'); 

procedure TCheckWindow.DisplayEvent(Sender: TObject; ToPrint: PPChar; Entry: PChar);
var
  Idx: Integer;
  p: PLongInt;
begin
  P := PLongInt(ToPrint);
  Dec(P);
  Idx := P^;
  if (Idx >= 0) and (Idx < Length(FileList)) and Assigned(Entry) then
  begin
    ToPrint[0] := PChar(GitStatusText[FileList[Idx].Status]);
    ToPrint[1] := PChar(StageText[FileList[Idx].Staged]);
    ToPrint[2] := PChar(FileList[Idx].FileName);
  end
  else
  begin
    ToPrint[0] := PChar(Titles[0]);
    ToPrint[1] := PChar(Titles[1]);
    ToPrint[2] := PChar(Titles[2]);
  end;  
end;



end.