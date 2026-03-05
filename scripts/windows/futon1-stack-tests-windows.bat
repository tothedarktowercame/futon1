@echo off
setlocal EnableExtensions

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"

set "FAILURES=0"

echo [futon1-stack-tests-windows] START apps/client determinism
call "%SCRIPT_DIR%\client-test-windows.bat" %*
set "RC_CLIENT=%ERRORLEVEL%"
if not "%RC_CLIENT%"=="0" set /a FAILURES+=1

echo [futon1-stack-tests-windows] START apps/graph-memory regression suite
call "%SCRIPT_DIR%\graph-memory-test-windows.bat" %*
set "RC_GRAPH=%ERRORLEVEL%"
if not "%RC_GRAPH%"=="0" set /a FAILURES+=1

echo [futon1-stack-tests-windows] START apps/nlp-interface pipeline tests
call "%SCRIPT_DIR%\nlp-interface-test-windows.bat" %*
set "RC_NLP=%ERRORLEVEL%"
if not "%RC_NLP%"=="0" set /a FAILURES+=1

echo [futon1-stack-tests-windows] SUMMARY client=%RC_CLIENT% graph-memory=%RC_GRAPH% nlp-interface=%RC_NLP%
if %FAILURES% GTR 0 (
  1>&2 echo [futon1-stack-tests-windows] FAIL failures=%FAILURES%
  exit /b 1
)

echo [futon1-stack-tests-windows] PASS
exit /b 0
