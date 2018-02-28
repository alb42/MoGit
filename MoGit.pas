program Mogit;
uses
  MUIClass.Base,
  mogitlogunit, mogitcheckunit,
  globalunit;

begin
  if ParamStr(1) = 'help' then
	begin
		writeln(VERSION);
		writeln('Usage: MoGit [log|check|help] [path to repository]');
		writeln('    help  = this help text');
		writeln('    log   = show the commit log of the given repository ');
		writeln('    check = show the status of the given repository (changed files, staged, deleted, ...)');
		writeln('  if no parameter is given "log" and current directory is used as default.');
		Exit;
	end;

  case StartModus of
    smLog: TLogWindow.Create;
    smCheck: TCheckWindow.Create;
  end;
  
  MUIApp.Title := 'MoGit';
  MUIApp.Version := VERSION;
  MUIApp.Author := 'Marcus "ALB42" Sackrow';
  MUIApp.Description := 'GIT helper';
  MUIApp.Base := 'MOGIT';
  MUIApp.Run;  
end.