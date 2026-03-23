@echo off
set "XTDB_SCRIPT_PATH=scripts/xtdb-count-runner.clj"
call "%~dp0run-xtdb-windows.bat" %*
exit /b %ERRORLEVEL%
