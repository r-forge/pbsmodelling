@ECHO OFF
if not defined PBS_SETLOCAL (
  SETLOCAL
  SET PBS_SETLOCAL=1 )

if "%1"=="" (
  echo ERROR - you must specify a package name
  echo example: %0 PBSmodelling
  goto end )

SET PBS_NO_PAUSE=1
call checkPaths.bat

if not defined PBSERROR (
  Rcmd build %1 )

:end