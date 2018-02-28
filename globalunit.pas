unit globalunit;
{$mode objfpc}{$H+}
interface

uses
  IniFiles;
  
const
  VERSION = '$VER: MoGit 0.1 (27.02.2018)';  
  
type
  TStartModus = (smLog, smCheck);  
var
  StartModus: TStartModus = smLog;
  SrcPath: string = '';
  ShowUntracked: Boolean = True;
  GitPrg: string = 'System:Development/GG/usr/bin/git';
  Editor: string = 'sys:Applications/Scribble/Scribble';

implementation
uses
  GitUnit;

procedure ParseParameter;
var
  i: Integer;
begin
  if ParamCount > 0 then
  begin
    i := 1;
    writeln('Paramstr(1) ', ParamStr(1));
    if ParamStr(1) = 'log' then
    begin
      StartModus := smLog;
      i := 2;
    end
    else
    if ParamStr(1) = 'check' then
    begin
      StartModus := smCheck;
      i := 2;
    end;
    if ParamCount >= i then
    begin
      SrcPath := ParamStr(i);
    end;
    SrcPath := GitTopLevel(SrcPath);    
  end;
end;


procedure LoadSettings;
var
  Ini: TIniFile;
begin
  Ini := nil;
  try
    Ini := TIniFile.Create(ParamStr(0) + '.ini');
    GitPrg := Ini.ReadString('General', 'Git', GitPrg);
    GitPath := GitPrg;
    Editor := Ini.ReadString('General', 'DiffEditor', Editor);
    ShowUntracked := Ini.ReadBool('General', 'ShowUntracked', True);
  except
  end;
  Ini.Free;
end;

initialization
  LoadSettings;
  ParseParameter;

end.