@echo off
setlocal EnableExtensions EnableDelayedExpansion

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"
for %%I in ("%SCRIPT_DIR%\..\..") do set "REPO_ROOT=%%~fI"
set "RAW_DIR=%SCRIPT_DIR%\raw"
set "RECEIPT_PATH=%RAW_DIR%\latest-futon1-stack-tests-receipt.json"
if defined FUTON1_STACK_TESTS_RECEIPT set "RECEIPT_PATH=%FUTON1_STACK_TESTS_RECEIPT%"

set "FAILURES=0"
set "RECEIPT_ERROR=0"

call :utc_now_iso STACK_START_ISO
call :epoch_now STACK_START_SECONDS

set "CMD_CLIENT=%SCRIPT_DIR%\client-test-windows.bat %*"
set "CMD_GRAPH=%SCRIPT_DIR%\graph-memory-test-windows.bat %*"
set "CMD_NLP=%SCRIPT_DIR%\nlp-interface-test-windows.bat %*"

echo [futon1-stack-tests-windows] START apps/client determinism
call :utc_now_iso START_CLIENT_ISO
call :epoch_now START_CLIENT_SECONDS
call "%SCRIPT_DIR%\client-test-windows.bat" %*
set "RC_CLIENT=%ERRORLEVEL%"
call :epoch_now END_CLIENT_SECONDS
call :utc_now_iso END_CLIENT_ISO
call :duration_seconds "%START_CLIENT_SECONDS%" "%END_CLIENT_SECONDS%" DUR_CLIENT_SECONDS
if not "%RC_CLIENT%"=="0" set /a FAILURES+=1

echo [futon1-stack-tests-windows] START apps/graph-memory regression suite
call :utc_now_iso START_GRAPH_ISO
call :epoch_now START_GRAPH_SECONDS
call "%SCRIPT_DIR%\graph-memory-test-windows.bat" %*
set "RC_GRAPH=%ERRORLEVEL%"
call :epoch_now END_GRAPH_SECONDS
call :utc_now_iso END_GRAPH_ISO
call :duration_seconds "%START_GRAPH_SECONDS%" "%END_GRAPH_SECONDS%" DUR_GRAPH_SECONDS
if not "%RC_GRAPH%"=="0" set /a FAILURES+=1

echo [futon1-stack-tests-windows] START apps/nlp-interface pipeline tests
call :utc_now_iso START_NLP_ISO
call :epoch_now START_NLP_SECONDS
call "%SCRIPT_DIR%\nlp-interface-test-windows.bat" %*
set "RC_NLP=%ERRORLEVEL%"
call :epoch_now END_NLP_SECONDS
call :utc_now_iso END_NLP_ISO
call :duration_seconds "%START_NLP_SECONDS%" "%END_NLP_SECONDS%" DUR_NLP_SECONDS
if not "%RC_NLP%"=="0" set /a FAILURES+=1

call :utc_now_iso STACK_END_ISO
call :epoch_now STACK_END_SECONDS
call :duration_seconds "%STACK_START_SECONDS%" "%STACK_END_SECONDS%" STACK_DURATION_SECONDS

if "%DUR_CLIENT_SECONDS%"=="" set "DUR_CLIENT_SECONDS=0"
if "%DUR_GRAPH_SECONDS%"=="" set "DUR_GRAPH_SECONDS=0"
if "%DUR_NLP_SECONDS%"=="" set "DUR_NLP_SECONDS=0"
if "%STACK_DURATION_SECONDS%"=="" set "STACK_DURATION_SECONDS=0"

if %FAILURES% GTR 0 (
  set "TEST_EXIT_CODE=1"
  set "AGGREGATE_VERDICT=fail"
) else (
  set "TEST_EXIT_CODE=0"
  set "AGGREGATE_VERDICT=pass"
)

if "%STACK_START_ISO%"=="" call :utc_now_iso STACK_START_ISO
if "%STACK_START_ISO%"=="" set "STACK_START_ISO=%STACK_END_ISO%"

call :write_receipt
if errorlevel 1 (
  set "RECEIPT_ERROR=1"
  1>&2 echo [futon1-stack-tests-windows] ERROR unable to write receipt: %RECEIPT_PATH%
)

echo [futon1-stack-tests-windows] SUMMARY client=%RC_CLIENT% graph-memory=%RC_GRAPH% nlp-interface=%RC_NLP%
echo [futon1-stack-tests-windows] RECEIPT %RECEIPT_PATH%
if "%TEST_EXIT_CODE%"=="1" (
  1>&2 echo [futon1-stack-tests-windows] FAIL failures=%FAILURES%
  exit /b 1
)
if "%RECEIPT_ERROR%"=="1" (
  1>&2 echo [futon1-stack-tests-windows] FAIL receipt-write
  exit /b 2
)

echo [futon1-stack-tests-windows] PASS
exit /b 0

:write_receipt
if not exist "%RAW_DIR%" mkdir "%RAW_DIR%" >nul 2>nul
if errorlevel 1 exit /b 1

call :json_escape REPO_ROOT ESC_REPO_ROOT
call :json_escape RECEIPT_PATH ESC_RECEIPT_PATH
call :json_escape CMD_CLIENT ESC_CMD_CLIENT
call :json_escape CMD_GRAPH ESC_CMD_GRAPH
call :json_escape CMD_NLP ESC_CMD_NLP

> "%RECEIPT_PATH%" (
  echo {
  echo   "schema": "futon1-stack-tests-receipt/v1",
  echo   "generated_at_utc": "%STACK_END_ISO%",
  echo   "runner": "scripts/windows/futon1-stack-tests-windows.bat",
  echo   "repo_root": "%ESC_REPO_ROOT%",
  echo   "receipt_path": "%ESC_RECEIPT_PATH%",
  echo   "jobs": [
  echo     {
  echo       "name": "apps/client",
  echo       "command": "%ESC_CMD_CLIENT%",
  echo       "start_utc": "%START_CLIENT_ISO%",
  echo       "end_utc": "%END_CLIENT_ISO%",
  echo       "duration_seconds": %DUR_CLIENT_SECONDS%,
  echo       "exit_code": %RC_CLIENT%
  echo     },
  echo     {
  echo       "name": "apps/graph-memory",
  echo       "command": "%ESC_CMD_GRAPH%",
  echo       "start_utc": "%START_GRAPH_ISO%",
  echo       "end_utc": "%END_GRAPH_ISO%",
  echo       "duration_seconds": %DUR_GRAPH_SECONDS%,
  echo       "exit_code": %RC_GRAPH%
  echo     },
  echo     {
  echo       "name": "apps/nlp-interface",
  echo       "command": "%ESC_CMD_NLP%",
  echo       "start_utc": "%START_NLP_ISO%",
  echo       "end_utc": "%END_NLP_ISO%",
  echo       "duration_seconds": %DUR_NLP_SECONDS%,
  echo       "exit_code": %RC_NLP%
  echo     }
  echo   ],
  echo   "summary": {
  echo     "start_utc": "%STACK_START_ISO%",
  echo     "end_utc": "%STACK_END_ISO%",
  echo     "duration_seconds": %STACK_DURATION_SECONDS%,
  echo     "failures": %FAILURES%,
  echo     "aggregate_exit_code": %TEST_EXIT_CODE%,
  echo     "aggregate_verdict": "%AGGREGATE_VERDICT%"
  echo   }
  echo }
)
if errorlevel 1 exit /b 1
exit /b 0

:json_escape
set "ESC_VALUE=!%~1!"
set "ESC_VALUE=!ESC_VALUE:\=\\!"
set "ESC_VALUE=!ESC_VALUE:"=\"!"
set "%~2=!ESC_VALUE!"
exit /b 0

:utc_now_iso
set "%~1="
for /f "usebackq delims=" %%I in (`powershell -NoProfile -Command "Get-Date -Format o"`) do set "%~1=%%I"
if not defined %~1 set "%~1=unknown"
exit /b 0

:epoch_now
set "%~1="
for /f "usebackq delims=." %%I in (`powershell -NoProfile -Command "Get-Date -UFormat %%s"`) do set "%~1=%%I"
if not defined %~1 set "%~1=0"
exit /b 0

:duration_seconds
set "%~3=0"
if "%~1"=="" exit /b 0
if "%~2"=="" exit /b 0
set /a "%~3=%~2-%~1" >nul 2>nul
if errorlevel 1 set "%~3=0"
if !%~3! LSS 0 set "%~3=0"
exit /b 0
