@echo %off
setlocal enabledelayedexpansion

set AppDir=%~dp0
set R="%AppDir%Application\R\bin\x64\R.exe"
set runfile="%AppDir%Application\run.R"
set appfolder="%AppDir%Application\app"

%R% --no-save --slave -f %runfile% --args %appfolder%
exit 0
