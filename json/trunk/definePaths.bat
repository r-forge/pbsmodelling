@ECHO OFF
if not defined PBS_SETLOCAL (
  SETLOCAL
  SET PBS_SETLOCAL=1 )

rem ***** Edit the six PATH variables listed below *****

set R_PATH=C:\Program Files\R\R-2.5.0\bin
set TOOLS_PATH=C:\rtools\bin
set PERL_PATH=C:\Perl\bin
set MINGW_PATH=C:\MinGW\bin
set TEX_PATH=C:\MiKTeX2.5\miktex\bin
set HTMLHELP_PATH=C:\Program Files\HTML Help Workshop