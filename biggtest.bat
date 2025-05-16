@echo off
:loop
type biggtest.alis | alis-parser.exe > nul
goto loop
