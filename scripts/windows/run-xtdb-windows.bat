@echo off
setlocal EnableExtensions

if not defined XTDB_SCRIPT_PATH (
  1>&2 echo [run-xtdb-windows] ERROR: XTDB_SCRIPT_PATH is not set
  exit /b 64
)

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"
for %%I in ("%SCRIPT_DIR%\..") do set "SCRIPTS_DIR=%%~fI"
for %%I in ("%SCRIPTS_DIR%\..") do set "REPO_ROOT=%%~fI"
for %%I in ("%REPO_ROOT%\..\futon3c\scripts\windows\run-clojure-windows.bat") do set "RUN_CLOJURE=%%~fI"

if not exist "%RUN_CLOJURE%" (
  1>&2 echo [run-xtdb-windows] ERROR: missing %RUN_CLOJURE%
  exit /b 1
)

set "RUN_CLOJURE_WORKDIR=%REPO_ROOT%"
set "RUN_CLOJURE_WINDOWS_LWJGL_NATIVE_PATCH=1"
set RUN_CLOJURE_EXTRA_ALIAS_BODY={:extra-paths ["."] :extra-deps {com.xtdb/xtdb-rocksdb {:mvn/version "1.24.3"} com.xtdb/xtdb-lmdb {:mvn/version "1.24.3"}}}

call "%RUN_CLOJURE%" -M:windows-lmdb:run-clojure-extra %XTDB_SCRIPT_PATH% %*
exit /b %ERRORLEVEL%
