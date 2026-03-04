@echo off
setlocal EnableExtensions

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"
for %%I in ("%SCRIPT_DIR%\..") do set "SCRIPTS_DIR=%%~fI"
for %%I in ("%SCRIPTS_DIR%\..") do set "REPO_ROOT=%%~fI"

set "XTDB_SDEPS={:deps {org.clojure/clojure {:mvn/version ^"1.11.1^"} com.xtdb/xtdb-api {:mvn/version ^"1.24.3^"} com.xtdb/xtdb-rocksdb {:mvn/version ^"1.24.3^"} com.xtdb/xtdb-lmdb {:mvn/version ^"1.24.3^"}}}"

pushd "%REPO_ROOT%" >nul 2>nul
if errorlevel 1 (
  1>&2 echo [xtdb-peek-windows] ERROR: unable to enter %REPO_ROOT%
  exit /b 1
)

clojure -Sdeps "%XTDB_SDEPS%" scripts/xtdb-peek-runner.clj %*
set "RC=%ERRORLEVEL%"

popd >nul 2>nul
exit /b %RC%
