param([ValidateSet('Install','Load','Verify','Uninstall','Fonts')][string]$Mode='Install',[switch]$NoUI,[string]$Selection)
$ErrorActionPreference='Stop'
$kitRoot=$PSScriptRoot
$enc=[Text.Encoding]::GetEncoding(936)
$hookPath=Join-Path $env:APPDATA 'ZWSOFT\ZWCAD\2020\zh-CN\Support\zwcad.lsp'
$statePath=Join-Path $kitRoot 'install-state.json'
$logPath=Join-Path $kitRoot 'install.log'
function LispString([string]$s) { '"'+$s.Replace('\','/').Replace('"','\"')+'"' }
function StripHook([string]$s) { [regex]::Replace($s,'(?ms)^;; ZWKIT BEGIN\r?\n.*?^;; ZWKIT END\r?\n?','') }
function WriteLog([string]$s) { Add-Content -LiteralPath $logPath -Value ((Get-Date -Format s)+' '+$s) -Encoding UTF8; Write-Output $s }
function SendCad([string]$s) {
    if([int]$doc.GetVariable('CMDACTIVE') -ne 0){throw 'CAD 正在执行命令，请按 Esc 退出后重试。'}
    $doc.SendCommand($s+"`n")
}
function Install-KitFonts([string]$Only) {
    $fontDir=Join-Path $kitRoot 'fonts'
    if(-not(Test-Path -LiteralPath $fontDir)){return}
    $files=@(Get-ChildItem -LiteralPath $fontDir -File | Where-Object { $_.Extension -in '.ttf','.ttc','.otf','.shx' })
    if($Only){
        $names=@($Only.Split(',') | ForEach-Object {$_.Trim()})
        $files=@($files | Where-Object {$names -contains $_.Name})
        if(-not $files.Count){WriteLog '所选字体未在 fonts 文件夹中找到。';$script:fontFails=1;return}
    }
    if(-not $files.Count){$script:fontFails=0;return}
    # 写 C:\Windows\Fonts 和 HKLM 需要管理员权限；不是管理员时每个字体都会失败，
    # 必须把失败数报回去，不能让调用方显示「字体已安装」（审查发现原来退出码恒为 0）。
    $isAdmin=([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
    if(-not $isAdmin){WriteLog '提示：当前不是管理员，TrueType 字体无法写入系统字体目录。请右键「一键安装.cmd」→「以管理员身份运行」再装字体。'}
    $fails=0
    $sysFonts=Join-Path $env:WINDIR 'Fonts'
    $cadFonts=$null
    if(Test-Path -LiteralPath (Join-Path $env:ProgramFiles 'ZWSOFT\ZWCAD 2020\Fonts')){$cadFonts=Join-Path $env:ProgramFiles 'ZWSOFT\ZWCAD 2020\Fonts'}
    else {
        $cadExe=@(Get-Process ZWCAD -ErrorAction SilentlyContinue | Select-Object -First 1).Path
        if($cadExe){$cadFonts=Join-Path (Split-Path $cadExe) 'Fonts'}
    }
    $reg='HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Fonts'
    foreach($f in $files) {
        try {
            if($f.Extension -eq '.shx') {
                if(-not $cadFonts -or -not(Test-Path -LiteralPath $cadFonts)){WriteLog ('未找到 ZWCAD 字体目录，跳过 SHX：'+$f.Name);$fails++;continue}
                Copy-Item -LiteralPath $f.FullName -Destination (Join-Path $cadFonts $f.Name) -Force
                WriteLog ('已复制 SHX 字体到 ZWCAD：'+$f.Name)
            } else {
                $dest=Join-Path $sysFonts $f.Name
                if(Test-Path -LiteralPath $dest){WriteLog ('系统已有该字体，跳过：'+$f.Name);continue}
                Copy-Item -LiteralPath $f.FullName -Destination $dest
                New-ItemProperty -LiteralPath $reg -Name ([IO.Path]::GetFileNameWithoutExtension($f.Name)+' (TrueType)') -Value $f.Name -PropertyType String -Force | Out-Null
                WriteLog ('已安装字体：'+$f.Name+'（重启 CAD 后生效）')
            }
        } catch {
            WriteLog ('字体处理失败：'+$f.Name+'；'+$_.Exception.Message)
            $fails++
        }
    }
    if($files | Where-Object {$_.Name -match '^STXIHEI'}){Set-KitFontMap}
    WriteLog ('字体处理完成，共 ' + $files.Count + ' 个文件，失败 ' + $fails + ' 个。')
    $script:fontFails=$fails
}
function Set-KitFontMap {
    $lines=@('STXIHEI;simhei.ttf','STXIHEI.TTF;simhei.ttf')
    $fmps=@((Join-Path $env:APPDATA 'ZWSOFT\ZWCAD\2020\zh-CN\Support\zwcad.fmp'),(Join-Path $env:ProgramFiles 'ZWSOFT\ZWCAD 2020\UserDataCache\Support\zwcad.fmp'))
    foreach($f in $fmps) {
        try {
            if(-not(Test-Path -LiteralPath $f)){continue}
            $t=[IO.File]::ReadAllText($f,$enc)
            if($t -notmatch 'STXIHEI'){
                [IO.File]::WriteAllText($f,($t.TrimEnd()+"`r`n"+($lines -join "`r`n")+"`r`n"),$enc)
                WriteLog ('已添加字体映射：'+$f)
            }
        } catch {
            WriteLog ('字体映射失败：'+$f+'；'+$_.Exception.Message)
        }
    }
    try {
        New-ItemProperty -Path 'HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion\FontSubstitutes' -Name 'STXihei' -Value 'SimHei' -PropertyType String -Force | Out-Null
        WriteLog '已添加系统字体替代：STXihei -> SimHei'
    } catch {
        WriteLog ('系统字体替代失败：'+$_.Exception.Message)
    }
}
try {
    if($Mode -eq 'Fonts') {
        $script:fontFails=0
        Install-KitFonts $Selection
        if($script:fontFails -gt 0){
            if(-not $NoUI){Add-Type -AssemblyName System.Windows.Forms;[Windows.Forms.MessageBox]::Show(('有 '+$script:fontFails+' 个字体没装上（多半是没用管理员身份运行），详情见 install.log。'),'中望工具包：未完成') | Out-Null}
            exit 1
        }
        if(-not $NoUI){Add-Type -AssemblyName System.Windows.Forms;[Windows.Forms.MessageBox]::Show('字体处理完成，详情见 install.log。重启 CAD 后生效。','中望工具包') | Out-Null}
        exit 0
    }
    if($Selection) {
        $chosen=@($Selection.Split(',') | Select-Object -Unique)
        $validKeys=@('dimensions','trim','extend','tolerance','symbols','font','dimline','frame','tke')
        if(@($chosen | Where-Object {$_ -notin $validKeys}).Count){throw '无效的模块选择。'}
        $config='(setq zwk:enabled ''('+ (($chosen | ForEach-Object {'"'+$_+'"'}) -join ' ') + '))'
        [IO.File]::WriteAllText((Join-Path $kitRoot 'selection.lsp'),$config,$enc)
    }
    foreach($f in @('boot.lsp','modules.lsp','bin\ZWKit.Core.102.dll')) {
        if(-not(Test-Path -LiteralPath (Join-Path $kitRoot $f))){throw ('插件文件缺失：'+$f)}
    }
    $cad=$null; $doc=$null
    try { $cad=[Runtime.InteropServices.Marshal]::GetActiveObject('ZWCAD.Application'); $doc=$cad.ActiveDocument } catch {}
    if($cad -and -not ([string]$cad.Version).StartsWith('2020')) {throw '本工具包仅适用于中望 CAD 2020。'}
    if($Mode -eq 'Uninstall') {
        # 钩子不管有没有 install-state.json 都要去掉：原来只在状态文件存在时才去，
        # 状态文件丢了（换过包、删过）就什么都没做却记录「已移除」（审查发现）。
        if(Test-Path -LiteralPath $hookPath){$text=[IO.File]::ReadAllText($hookPath,$enc); [IO.File]::WriteAllText($hookPath,(StripHook $text),$enc)}
        if(Test-Path -LiteralPath $statePath) {
            $state=Get-Content -LiteralPath $statePath -Raw | ConvertFrom-Json
            if($null -ne $state.oldAcadLspAsDoc) {
                if($doc){$doc.SetVariable('ACADLSPASDOC',[int]$state.oldAcadLspAsDoc)}
                elseif($state.configKey -and (Test-Path -LiteralPath $state.configKey)){Set-ItemProperty -LiteralPath $state.configKey -Name ACADLSPASDOC -Value ([int]$state.oldAcadLspAsDoc)}
            }
        }
        WriteLog '已移除本工具包的自动加载入口。重启 CAD 后生效；原插件文件和图纸保留。'
    } else {
        if($Mode -eq 'Install') {
            $support=Split-Path $hookPath
            if(-not(Test-Path -LiteralPath $support)){throw '未找到中望 2020 中文版用户支持目录。'}
            $old=''; if(Test-Path -LiteralPath $hookPath){$old=[IO.File]::ReadAllText($hookPath,$enc)}
            if(-not(Test-Path -LiteralPath $statePath)) {
                $key='HKCU:\Software\ZWSOFT\ZWCAD\2020\zh-CN\Profiles\Default\Config'
                if($doc){$profile=[string]$doc.GetVariable('CPROFILE');$key='HKCU:\Software\ZWSOFT\ZWCAD\2020\zh-CN\Profiles\'+$profile+'\Config'}
                # 全新配置里可能还没有这个值（或整个键都没有）；Stop 模式下直接读会抛异常、整个安装失败。
                # 读不到就记 $null，卸载时跳过还原。
                $prior=$null
                try { $prior=(Get-ItemProperty -LiteralPath $key -Name ACADLSPASDOC -ErrorAction Stop).ACADLSPASDOC } catch {}
                if($old){[IO.File]::WriteAllText((Join-Path $kitRoot 'zwcad.lsp.before-install.bak'),$old,$enc)}
                @{oldAcadLspAsDoc=$prior;configKey=$key;hook=$hookPath;installedAt=(Get-Date -Format s)} | ConvertTo-Json | Set-Content -LiteralPath $statePath -Encoding UTF8
            }
            $state=Get-Content -LiteralPath $statePath -Raw | ConvertFrom-Json
            $hook=";; ZWKIT BEGIN`r`n(setq zwk:root "+(LispString $kitRoot)+")`r`n"+'(load (strcat zwk:root "/boot.lsp") nil)'+"`r`n;; ZWKIT END`r`n"
            [IO.File]::WriteAllText($hookPath,((StripHook $old).TrimEnd()+"`r`n"+$hook),$enc)
            if($doc -and [int]$doc.GetVariable('CMDACTIVE') -eq 0){$doc.SetVariable('ACADLSPASDOC',1)}
            # 键不存在时写不进去也不算失败：boot.lsp 启动时自己会 (setvar "ACADLSPASDOC" 1)
            try { Set-ItemProperty -LiteralPath $state.configKey -Name ACADLSPASDOC -Value 1 -ErrorAction Stop }
            catch { WriteLog ('注册表写 ACADLSPASDOC 失败（不影响使用，CAD 里会自动补上）：'+$_.Exception.Message) }
            WriteLog '自动加载入口已安装。每次启动或打开图纸将加载工具包并设置平滑度 20000。'
            Install-KitFonts
        }
        if(-not $doc) {
            if($Mode -ne 'Install'){throw '请先打开中望 CAD 2020 和图纸。'}
            WriteLog 'CAD 当前未运行；下次打开 CAD 时生效。'
        } elseif([int]$doc.GetVariable('CMDACTIVE') -ne 0) {
            throw '自动加载入口已准备好，但 CAD 正在执行命令。请退出命令后双击重新安装/加载。'
        } else {
            $nonce=[Guid]::NewGuid().ToString('N')
            $resultPath=Join-Path $env:TEMP ('zwkit-'+$nonce+'.txt')
            if($Mode -ne 'Verify') {
                SendCad ('(command "_.NETLOAD" '+(LispString (Join-Path $kitRoot 'bin\ZWKit.Core.102.dll'))+')')
                Start-Sleep -Milliseconds 400
                SendCad ('(progn (setq zwk:root '+(LispString $kitRoot)+') (load (strcat zwk:root "/boot.lsp")))')
                Start-Sleep -Milliseconds 400
            }
            $check='(progn (setq zwk:f (open '+(LispString $resultPath)+' "w")) (write-line (if (and (zwk:ready) (= (zwk:smooth-value) 20000) (zwk:selection-ok)) "OK" "FAILED") zwk:f) (close zwk:f))'
            SendCad $check
            $until=(Get-Date).AddSeconds(12)
            $answer=''
            do {
                if(Test-Path -LiteralPath $resultPath){$answer=[string](Get-Content -LiteralPath $resultPath -Raw)}
                if($answer -match '\S'){break}
                Start-Sleep -Milliseconds 200
            } while((Get-Date) -lt $until)
            if($answer -notmatch '\S'){throw '未收到完整 CAD 自检结果。若当前会话加载过旧版 DLL，请保存图纸并重启 CAD，再运行 Verify 或 ZWKCHECK。自动加载设置已保留；本次不能视为加载成功。'}
            if($answer -notmatch '^\s*OK\s*$'){throw '自检未通过：模块、命令或平滑度有一项未就绪。'}
            WriteLog '自检通过：所选模块、公共组件和平滑度 20000 已就绪。'
        }
    }
    if(-not $NoUI){Add-Type -AssemblyName System.Windows.Forms;[Windows.Forms.MessageBox]::Show('操作完成。详细结果见 install.log。','中望工具包') | Out-Null}
    exit 0
} catch {
    WriteLog ('失败：'+$_.Exception.Message+'；位置：'+$_.ScriptStackTrace)
    if(-not $NoUI){Add-Type -AssemblyName System.Windows.Forms;[Windows.Forms.MessageBox]::Show($_.Exception.Message,'中望工具包：未完成') | Out-Null}
    exit 1
}
