unit gitunit;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils;

var
  GitPath: string = 'gg:usr/bin/git';
  
type
  TGitStatus = (gsUnknown, gsUntracked, gsModified, gsDeleted, gsAdded, gsRenamed, gsCopied);
const
  GitStatusText: array[TGitStatus] of string = 
               ('Unknown', 'Untracked', 'Modified', 'Deleted', 'Added', 'Renamed', 'Copied');
  StageText: array[Boolean] of string = 
    ('', 'Staged');

type  
  TFileStatus = record
    Filename: string;
    Status: TGitStatus;
    Staged: Boolean;
  end;
  TFileStatusList = array of TFileStatus;
  
  TLogEntry = record
    Hash: string;
    FullHash: string;
    Date: string;
    UDate: Int64;
    Author: string;
    Msg: string;
  end;
  TLogEntries = array of TLogEntry;
  TStringArray = array of string;


  
function GitTopLevel(Path: string): string;
  
function GitStatus(Path: string; IncludeUntracked: Boolean = True): TFileStatusList;
procedure GitAdd(Path: string; FileName: string);
procedure GitUnAdd(Path: string; FileName: string);
function GitDiff(Path, FileName: string; FullHash: string = ''; Cached: boolean = False): string;
function GitLog(Path: string; Num: Integer = -1): TLogEntries;
procedure GitRevert(Path: string; FileName: string);
function GitChangedFiles(Path: string; Hash: string): TStringArray;

implementation

var
  ToDelete: TStringList;

procedure DoGit(Path: string; Parameter: string; Res: TStringList);
var
  Temp: string;
begin
  Temp := GetTempFileName('T:', 'MoGit');
  Res.Clear;
  try
    SysUtils.SetCurrentDir(Path);
    SysUtils.ExecuteProcess(GitPath, Parameter + ' >' + Temp);
    Res.LoadFromFile(Temp);
    DeleteFile(Temp);
  except
    on E:Exception do
      writeln('exception ', E.Message);
  end;
end;

function GitTopLevel(Path: string): string;
var
  SL: TStringList;
  P: Integer;
begin
  SL := TStringList.Create;
  DoGit(Path, 'rev-parse --show-toplevel', SL);
  Result := Trim(SL.Text);
  if Copy(Result, 1, 1) = '/' then
  begin
    Delete(Result, 1, 1);
    P := Pos('/', Result);
    if P > 0 then
      Result[P] := ':';
  end;
  SL.Free;
end;

procedure GitAdd(Path: string; FileName: string);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  DoGit(Path, 'add -- ' + FileName, SL);
  SL.Free;
end;

procedure GitUnAdd(Path: string; FileName: string);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  DoGit(Path, 'reset HEAD -- ' + FileName, SL);
  SL.Free;
end;

function GitDiff(Path, FileName: string; FullHash: string = ''; Cached: boolean = False): string;
var
  SL: TStringList;
  Num: Integer;
  t: string;
begin
  SL := TStringList.Create;
  try
    if FullHash <> '' then
      Cached := False;
    if Cached then
      DoGit(Path, 'diff ' + ' --cached -- ' + FileName, SL)
    else
      DoGit(Path, 'diff ' + FullHash + ' -- ' + FileName, SL);
    if SL.Count > 0 then
    begin
      T := 'T:';      
      Num := 0;
      repeat
        Result := t + FullHash + 'diff_' + IntToStr(Num) + '.diff';
        Inc(Num);
      until not FileExists(Result);
      SL.SaveToFile(Result);
      ToDelete.Add(Result);
    end;
  finally
    SL.Free;
  end;
end;

function GitStatus(Path: string; IncludeUntracked: Boolean = True): TFileStatusList;
var
  SL: TStringList;
  i, Idx: Integer;
  Line: string;
  c1, c2: Char;
begin
  SL := TStringList.Create;
  try
    DoGit(Path, 'status --porcelain', SL);
    SetLength(Result, SL.Count);
    Idx := 0;
    for i := 0 to SL.Count - 1 do
    begin
      Line := SL[i];
      if Length(Line) > 2 then
      begin
        c1 := Line[1];
        c2 := Line[2];
        Result[Idx].Staged := (c1 <> ' ') and (c1 <> '?');
        if Result[Idx].Staged then
          c2 := c1;
        case c2 of
          'A': Result[Idx].Status := gsAdded;
          'M': Result[Idx].Status := gsModified;
          'D': Result[Idx].Status := gsDeleted;
          'R': Result[Idx].Status := gsRenamed;
          'C': Result[Idx].Status := gsCopied;
          '?': begin
            Result[Idx].Status := gsUntracked;
            if not IncludeUntracked then
              Continue;  
          end;
        end;
        Result[Idx].FileName := Copy(Line, 4, Length(Line));
        Inc(Idx);
      end;
    end;
    SetLength(Result, Idx);
  finally
    SL.Free;
  end;
end;

function GetNextString(var s: string): string;
var
  p: Integer;
begin
  Result := '';
  p := Pos(#9, s);
  if p > 0 then
  begin
    Result := Copy(s, 1, p - 1);
    Delete(s, 1, p);
  end;
end;

{TLogEntry = record
    Hash: string;       %h
    FullHash: string;   %H
    Date: string;       %aD
    UDate: Int64;       %at
    Author: string;     %an
    Msg: string;        %s
  end;}

function GitLog(Path: string; Num: Integer = -1): TLogEntries;
var
  SL: TStringList;
  Line: string;
  i: Integer;
begin
  SL := TStringList.Create;
  try
    DoGit(Path, 'log --format=%h%x09%H%x09%aD%x09%at%x09%an%x09%s', SL);
    SetLength(Result, SL.Count);
    for i := 0 to Sl.Count - 1 do
    begin
      Line := SL[i];
      Result[i].Hash := GetNextString(Line);
      Result[i].FullHash := GetNextString(Line);
      Result[i].Date := GetNextString(Line);
      Result[i].UDate := StrToIntDef(GetNextString(Line), -1);
      Result[i].Author := GetNextString(Line);
      Result[i].Msg := Line;
    end;
  finally
    SL.Free;
  end;
end;

procedure GitRevert(Path: string; FileName: string);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  DoGit(Path, 'checkout -- ' + FileName, SL);
  SL.Free;
end;

function GitChangedFiles(Path: string; Hash: string): TStringArray;
var
  SL: TStringList;
  i: Integer;
begin
  SL := TStringList.Create;
  DoGit(Path, 'show --format= --name-only ' + Hash, SL);
  SetLength(Result, SL.Count);
  for i := 0 to SL.Count - 1 do
    Result[i] := SL[i];
  SL.Free;
end;

procedure DeleteTempFiles;
var
  i: Integer;
begin
  for i := 0 to ToDelete.Count - 1 do
  begin
    try
      DeleteFile(ToDelete[i]);
    except
    end;
  end;
  ToDelete.Free;
end;

initialization
  ToDelete := TStringList.Create;
finalization
  DeleteTempFiles;
end.