@echo off
title ZWCAD 工具包 - 云更新
"%SystemRoot%\System32\WindowsPowerShell\v1.0\powershell.exe" -NoProfile -ExecutionPolicy Bypass -File "%~dp0update.ps1"
echo.
pause
