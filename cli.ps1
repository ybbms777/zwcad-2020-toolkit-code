param([ValidateSet('dimensions','trim','extend','tolerance','symbols','font','dimline','frame','all','fonts')][string]$Choice)
$ErrorActionPreference='Stop'
[Console]::OutputEncoding=[Text.UTF8Encoding]::new($false)
$names=@{dimensions='智能标注';trim='快速修剪';extend='快速延伸';tolerance='几何公差';symbols='机械符号';font='改字体';dimline='标注线属性';frame='标题框缩放'}
# CJK glyphs render 2 columns wide in the console; ASCII renders 1. Pad by
# visual width, not .NET string length, so centered text always lines up.
#
# NOTE (2026-09-18): in a GBK/CP936 console every double-byte character is
# 2 columns wide -- including box-drawing (U+2500-U+257F), U+25A0-U+25FF,
# U+00B7, U+2605, etc. Those are "East Asian Ambiguous" width and Windows
# Terminal renders them 1 column, so they are NOT safe for borders/alignment.
# Rule: use plain ASCII for anything that must line up (borders, bars,
# padding). Only CJK text goes through the width math below.
function Get-VisualWidth([string]$s) {
    $w=0
    foreach($ch in $s.ToCharArray()) {
        $cp=[int]$ch
        if(($cp -ge 0x1100 -and $cp -le 0x115F) -or ($cp -ge 0x2E80 -and $cp -le 0xA4CF) -or
           ($cp -ge 0xAC00 -and $cp -le 0xD7A3) -or ($cp -ge 0xF900 -and $cp -le 0xFAFF) -or
           ($cp -ge 0xFF00 -and $cp -le 0xFF60) -or ($cp -ge 0xFFE0 -and $cp -le 0xFFE6) -or
           ($cp -ge 0x2010 -and $cp -le 0x2027) -or ($cp -ge 0x2030 -and $cp -le 0x205E) -or
           ($cp -ge 0x2500 -and $cp -le 0x257F) -or ($cp -ge 0x25A0 -and $cp -le 0x25FF) -or
           ($cp -ge 0x2605 -and $cp -le 0x2606) -or $cp -eq 0x00B7) { $w+=2 } else { $w+=1 }
    }
    $w
}
function Center-Text([string]$s,[int]$width) {
    $pad=$width-(Get-VisualWidth $s)
    if($pad -lt 0){$pad=0}
    $left=[int][Math]::Floor($pad/2)
    (' '*$left)+$s+(' '*($pad-$left))
}
function Pad-Visual([string]$s,[int]$width) {
    $pad=$width-(Get-VisualWidth $s)
    if($pad -lt 0){$pad=0}
    $s+(' '*$pad)
}
# 版本号从 version.json 读，别再写死在标题框里（发布时会自动更新）
function Get-KitVersion {
    $p=Join-Path $PSScriptRoot 'version.json'
    if(Test-Path -LiteralPath $p){
        try{
            $v=(Get-Content -LiteralPath $p -Raw -Encoding UTF8 | ConvertFrom-Json)
            if($v.version){ return [string]$v.version }
        }catch{}
    }
    return '1.2'
}
$allModules=@('dimensions','trim','extend','tolerance','symbols','font','dimline','frame')
# Weights are cosmetic only -- they shape how long the progress bar runs,
# not the actual (near-instant) install work.
$weights=@{dimensions=3;trim=3;extend=3;tolerance=1;symbols=2;font=4;dimline=3;frame=1}
$stages=@(
    @{Max=12;Text='正在检查安装环境'},
    @{Max=32;Text='正在部署插件模块'},
    @{Max=55;Text='正在配置命令与快捷键'},
    @{Max=78;Text='正在写入自动加载配置'},
    @{Max=93;Text='正在校验安装结果'}
)
function Get-StageText([int]$Percent) {
    foreach($s in $stages){ if($Percent -le $s.Max){return $s.Text} }
    return $stages[-1].Text
}
function Get-InstallDuration([string[]]$Modules,[int]$FontCount=0) {
    if($FontCount -gt 0) { $base=1200+($FontCount*1500) }
    else {
        $w=0
        foreach($m in $Modules){ if($weights.ContainsKey($m)){$w+=$weights[$m]} }
        if($w -le 0){$w=1}
        $base=1800+($w*1900)
    }
    $jitter=[int]($base*0.3)
    $duration=$base+(Get-Random -Minimum (-$jitter) -Maximum $jitter)
    [Math]::Max(2500,[Math]::Min(30000,[int]$duration))
}
function Get-KitFonts {
    $fontDir=Join-Path $PSScriptRoot 'fonts'
    if(-not(Test-Path -LiteralPath $fontDir)){return @()}
    @(Get-ChildItem -LiteralPath $fontDir -File -ErrorAction SilentlyContinue | Where-Object { $_.Extension -in '.ttf','.ttc','.otf','.shx' })
}
# SHX shape fonts and symbol TTFs are only a few KB, so a plain "N1 MB"
# format renders them all as "0.0 MB" and looks broken. Pick the unit.
function Format-Size([long]$bytes) {
    if($bytes -ge 1MB){ return ('{0:N1} MB' -f ($bytes/1MB)) }
    if($bytes -ge 1KB){ return ('{0:N1} KB' -f ($bytes/1KB)) }
    return ('{0} B' -f $bytes)
}
function Invoke-KitInstall([string]$Mode,[string]$Selection,[int]$DurationMs) {
    $selArg=''
    if($Selection){$selArg=' -Selection "'+$Selection+'"'}
    $timer=[Diagnostics.Stopwatch]::StartNew()
    $duration=$DurationMs
    $info=New-Object Diagnostics.ProcessStartInfo
    $info.FileName="$env:WINDIR\System32\WindowsPowerShell\v1.0\powershell.exe"
    $info.Arguments='-NoProfile -ExecutionPolicy Bypass -File "'+(Join-Path $PSScriptRoot 'install.ps1')+'" -Mode '+$Mode+' -NoUI'+$selArg
    $info.UseShellExecute=$false
    $info.CreateNoWindow=$true
    $info.RedirectStandardOutput=$true
    $info.RedirectStandardError=$true
    $info.StandardOutputEncoding=[Text.Encoding]::GetEncoding(936)
    $info.StandardErrorEncoding=[Text.Encoding]::GetEncoding(936)
    $process=[Diagnostics.Process]::Start($info)
    $outTask=$process.StandardOutput.ReadToEndAsync()
    $errTask=$process.StandardError.ReadToEndAsync()
    do {
        $percent=[Math]::Min(95,[int](95*$timer.ElapsedMilliseconds/$duration))
        $filled=[int]($percent/5)
        $bar=('='*$filled)+('-'*(20-$filled))
        $stage=Pad-Visual (Get-StageText $percent) 22
        Write-Host ("`r  $stage [$bar] $percent%   ") -NoNewline -ForegroundColor Cyan
        Start-Sleep -Milliseconds 80
    } while(-not $process.HasExited -or $timer.ElapsedMilliseconds -lt $duration)
    $process.WaitForExit()
    $ok=$process.ExitCode -eq 0
    if($ok){Write-Host ("`r  "+(Pad-Visual '安装完成' 22)+" [====================] 100%   ") -ForegroundColor Green}
    else {Write-Host "`n  安装未完成，请查看下面的原因。" -ForegroundColor Red}
    Write-Host $outTask.Result
    if($errTask.Result){Write-Host $errTask.Result -ForegroundColor Red}
    $process.Dispose()
    return $ok
}
function Select-KitFonts {
    $fonts=@(Get-KitFonts)
    if(-not $fonts.Count){
        Write-Host '  fonts 文件夹里没有字体文件。请把 .ttf/.ttc/.otf/.shx 放进 fonts 文件夹后再试。' -ForegroundColor Yellow
        return @()
    }
    Write-Host "`n可安装的字体（永久写入系统字体库，重启不丢）：" -ForegroundColor Cyan
    for($i=0;$i -lt $fonts.Count;$i++){
        $f=$fonts[$i]
        Write-Host ("  {0} {1} {2,9}" -f ('['+($i+1)+']').PadRight(5),$f.Name.PadRight(16),(Format-Size $f.Length))
    }
    Write-Host '  [A] 全部安装    [0] 取消'
    do {
        $sel=Read-Host "`n输入编号（逗号分隔）"
        if($sel -eq '0'){return @()}
        if($sel -match '^\s*[Aa]\s*$'){return @($fonts | ForEach-Object {$_.Name})}
        $valid=$sel -match '^\s*[1-9](\s*[,，]\s*[1-9])*\s*$'
        if($valid){
            $idx=@($sel -split '[,，]' | ForEach-Object {[int]$_.Trim()} | Where-Object {$_ -ge 1 -and $_ -le $fonts.Count} | Select-Object -Unique)
            if($idx.Count){return @($idx | ForEach-Object {$fonts[$_-1].Name})}
        }
        Write-Host '输入有误，请重新输入。' -ForegroundColor Yellow
    } until($false)
}
function Show-Menu {
    Clear-Host
    $boxWidth=48
    Write-Host ''
    Write-Host ('   +'+('-'*$boxWidth)+'+') -ForegroundColor Cyan
    Write-Host ('   |'+(Center-Text '中望 CAD 2020  插件安装中心' $boxWidth)+'|') -ForegroundColor Cyan
    Write-Host ('   |'+(Center-Text ('v'+(Get-KitVersion)) $boxWidth)+'|') -ForegroundColor DarkCyan
    Write-Host ('   +'+('-'*$boxWidth)+'+') -ForegroundColor Cyan
    Write-Host ''
    Write-Host '   标注与绘图' -ForegroundColor White
    Write-Host '    [1] 智能标注        ' -NoNewline -ForegroundColor Yellow
    Write-Host 'ZD 系列，点选标注点/对象，仿 AutoCAD 快速标尺寸' -ForegroundColor Gray
    Write-Host '    [2] 快速修剪        ' -NoNewline -ForegroundColor Yellow
    Write-Host 'TR / 拖动修剪，划过多余线段松开即剪' -ForegroundColor Gray
    Write-Host '    [3] 快速延伸        ' -NoNewline -ForegroundColor Yellow
    Write-Host 'EX / Shift 拖动修剪，延伸与修剪一键切换' -ForegroundColor Gray
    Write-Host '    [4] 几何公差        ' -NoNewline -ForegroundColor Yellow
    Write-Host 'GC，调用原生公差对话框快速放置' -ForegroundColor Gray
    Write-Host ''
    Write-Host '   文字与标注线' -ForegroundColor White
    Write-Host '    [5] 改字体          ' -NoNewline -ForegroundColor Yellow
    Write-Host 'ZF / ZFD，宋体·华文细黑·Arial + 字高 + 颜色，支持批量框选' -ForegroundColor Gray
    Write-Host '    [6] 标注线属性      ' -NoNewline -ForegroundColor Yellow
    Write-Host 'BZ / BZD，标注线/箭头颜色·大小·线宽，支持批量框选' -ForegroundColor Gray
    Write-Host '    [7] 机械符号        ' -NoNewline -ForegroundColor Yellow
    Write-Host 'FH，沉孔·深度·直径·沉头，ZGDT 符号字体' -ForegroundColor Gray
    Write-Host ''
    Write-Host '   图纸整理' -ForegroundColor White
    Write-Host '    [8] 标题框缩放      ' -NoNewline -ForegroundColor Yellow
    Write-Host 'TK，图框按零件尺寸自动缩放并居中（可手输尺寸或框选自动量）' -ForegroundColor Gray
    Write-Host ''
    Write-Host '    [9] 全部安装        ' -NoNewline -ForegroundColor Green
    Write-Host '★ 推荐 — 一次装齐以上全部功能' -ForegroundColor Green
    Write-Host '    [10] 字体安装       ' -NoNewline -ForegroundColor Yellow
    Write-Host '宋体/黑体/仿宋/楷体/华文细黑等，独立于插件功能' -ForegroundColor Gray
    Write-Host '    [0] 退出' -ForegroundColor DarkGray
    Write-Host ''
    Write-Host '   可多选，用逗号分隔，例如 1,2,5 或 9,10；本次选择替换上次选择。' -ForegroundColor DarkGray
    Write-Host '   点选式 TR/EX/栏选删除(FE) 已内置于基础组件，无需单独选择；1-9 都含自动加载和平滑度 20000。' -ForegroundColor DarkGray
}
if(-not $Choice){
    Show-Menu
    do {
        $inputValue=Read-Host "`n请输入编号"
        if($inputValue -eq '0'){exit 0}
        $valid=$inputValue -match '^\s*(10|[1-9])(\s*[,，]\s*(10|[1-9]))*\s*$'
        if(-not $valid){Write-Host '请输入 1-10，或用逗号分隔多个编号。' -ForegroundColor Yellow}
    } until($valid)
    $numbers=@($inputValue -split '[,，]' | ForEach-Object {$_.Trim()})
    $fontMode=$numbers -contains '10'
    $modNumbers=@($numbers | Where-Object {$_ -ne '10'})
    # 编号必须和上面菜单显示一致：[5]改字体 [6]标注线属性 [7]机械符号 [8]标题框缩放
    # （原来 5 和 7 是反的，选 [5]改字体 实际装的是机械符号）
    $numberMap=@{'1'='dimensions';'2'='trim';'3'='extend';'4'='tolerance';'5'='font';'6'='dimline';'7'='symbols';'8'='frame'}
    if($modNumbers -contains '9'){$selected=@($allModules)}
    else {$selected=@($modNumbers | ForEach-Object {$numberMap[$_]} | Where-Object {$_} | Select-Object -Unique)}
    if($fontMode){
        $picked=@(Select-KitFonts)
        if($picked.Count){
            Write-Host ("`n即将安装字体："+($picked -join '、')) -ForegroundColor Cyan
            $dur=Get-InstallDuration -Modules @() -FontCount $picked.Count
            if(Invoke-KitInstall 'Fonts' ($picked -join ',') $dur){Write-Host '[OK] 字体已安装到系统，重启 CAD 后生效。' -ForegroundColor Green}
        } else {Write-Host '未安装字体。' -ForegroundColor DarkGray}
        if(-not $selected.Count){exit 0}
    }
}
if($Choice -eq 'fonts'){
    $picked=@(Get-KitFonts | ForEach-Object {$_.Name})
    if(-not $picked.Count){Write-Host '  fonts 文件夹里没有字体文件。' -ForegroundColor Yellow; exit 1}
    Write-Host ("`n即将安装字体："+($picked -join '、')) -ForegroundColor Cyan
    $dur=Get-InstallDuration -Modules @() -FontCount $picked.Count
    if(Invoke-KitInstall 'Fonts' ($picked -join ',') $dur){Write-Host '[OK] 字体已安装到系统，重启 CAD 后生效。' -ForegroundColor Green; exit 0} else {exit 1}
}
if($Choice -eq 'all'){$selected=@($allModules)}
elseif($Choice){$selected=@($Choice)}
if($selected -and $selected.Count){
    Write-Host ("`n即将安装："+(($selected | ForEach-Object {$names[$_]}) -join ' + ')) -ForegroundColor Cyan
    $dur=Get-InstallDuration -Modules $selected
    if(Invoke-KitInstall 'Install' ($selected -join ',') $dur){
        Write-Host '[OK] 已保存选择，以后打开 CAD 自动加载。' -ForegroundColor Green
        exit 0
    } else {exit 1}
}
exit 0
