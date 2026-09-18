$ErrorActionPreference='Stop'
$root=Split-Path $PSScriptRoot
$cad='C:\Program Files\ZWSOFT\ZWCAD 2020'
# Only Bridge.cs registers CAD commands. Health.cs is legacy and is not compiled.
& "$env:WINDIR\Microsoft.NET\Framework64\v4.0.30319\csc.exe" /nologo /target:library /platform:x64 "/out:$root\bin\ZWKit.Core.102.dll" "/reference:$cad\ZwManaged.dll" "/reference:$cad\ZwDatabaseMgd.dll" /reference:System.Drawing.dll /reference:System.Windows.Forms.dll /reference:Microsoft.CSharp.dll "/reference:$env:WINDIR\Microsoft.NET\Framework64\v4.0.30319\WPF\PresentationCore.dll" "/reference:$env:WINDIR\Microsoft.NET\Framework64\v4.0.30319\WPF\WindowsBase.dll" "$PSScriptRoot\Mouse.cs" "$PSScriptRoot\Bridge.cs"
if($LASTEXITCODE -ne 0){throw 'Build failed'}
