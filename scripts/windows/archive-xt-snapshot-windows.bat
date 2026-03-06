@echo off
setlocal EnableExtensions

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"
for %%I in ("%SCRIPT_DIR%\..") do set "SCRIPTS_DIR=%%~fI"
for %%I in ("%SCRIPTS_DIR%\..") do set "REPO_ROOT=%%~fI"

where bash >nul 2>nul
if errorlevel 1 (
  1>&2 echo [archive-xt-snapshot-windows] ERROR: bash is required to run scripts/archive_xt_snapshot.sh
  exit /b 1
)

pushd "%REPO_ROOT%" >nul 2>nul
if errorlevel 1 (
  1>&2 echo [archive-xt-snapshot-windows] ERROR: unable to enter %REPO_ROOT%
  exit /b 1
)

bash scripts/archive_xt_snapshot.sh %*
set "RC=%ERRORLEVEL%"

popd >nul 2>nul
exit /b %RC%
