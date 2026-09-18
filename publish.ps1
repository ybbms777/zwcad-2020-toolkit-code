# ============================================================
#  ZWCAD 2020 整合工具包 - 发布
#
#  做三件事：
#    1. 重新计算全部代码文件的 sha256 -> 生成 version.json
#    2. 同步代码到公开仓库工作目录（不含字体）
#    3. 提交并推送：公开仓库（更新源）+ 私有仓库（含字体全量备份）
#
#  用法：
#    .\publish.ps1                 版本号自动 +1（如 1.2.3 -> 1.2.4）
#    .\publish.ps1 -Version 1.3    指定版本号
#    .\publish.ps1 -Message "说明"  自定义提交说明
# ============================================================
param(
    [string]$Version,
    [string]$Message,
    [switch]$NoZip
)

# 注意：不能用 Stop —— git 往 stderr 写东西时会被当成终止错误，
# 导致失败信息还没打印脚本就崩了。所有 git 调用都显式检查退出码。
$ErrorActionPreference = 'Continue'
try { [Console]::OutputEncoding = [Text.UTF8Encoding]::new($false) } catch {}

$root    = $PSScriptRoot
$pubDir  = 'C:\Users\Administrator\zwcad-toolkit-publish'
$pubRepo = 'git@github.com:ybbms777/zwcad-2020-toolkit-code.git'
$zipPath = 'C:\Users\Administrator\Desktop\ZWCAD工具包1.2.zip'

$excludeDirs  = @('fonts', '.git', '_backup')
$excludeFiles = @('selection.lsp', 'install-state.json', 'install.log', 'version.json',
                  'zwcad.lsp.before-install.bak')

function Say([string]$t, [string]$c = 'Gray') { Write-Host $t -ForegroundColor $c }

# 不依赖 PATH —— 某些会话（自动化 / 精简环境）里 PATH 里没有 git
function Resolve-Git {
    $c = Get-Command git -ErrorAction SilentlyContinue
    if ($c) { return $c.Source }
    $cands = @()
    if ($env:ProgramFiles) { $cands += (Join-Path $env:ProgramFiles 'Git\cmd\git.exe') }
    if (${env:ProgramFiles(x86)}) { $cands += (Join-Path ${env:ProgramFiles(x86)} 'Git\cmd\git.exe') }
    if ($env:LOCALAPPDATA) { $cands += (Join-Path $env:LOCALAPPDATA 'Programs\Git\cmd\git.exe') }
    $cands += 'C:\Program Files\Git\cmd\git.exe'
    $cands += 'C:\Program Files (x86)\Git\cmd\git.exe'
    foreach ($p in $cands) {
        if ($p -and (Test-Path -LiteralPath $p)) { return $p }
    }
    return $null
}

function Run-Git([string]$dir, [string[]]$gitArgs) {
    $out = & $script:gitExe -C $dir @gitArgs 2>&1 | Out-String
    $code = $LASTEXITCODE
    return [pscustomobject]@{ Code = $code; Out = ([string]$out).Trim() }
}

function Get-PublishFiles {
    $list = @()
    Get-ChildItem -LiteralPath $root -Recurse -File -Force | ForEach-Object {
        $rel = $_.FullName.Substring($root.Length + 1) -replace '\\', '/'
        $top = ($rel -split '/')[0]
        if ($excludeDirs -contains $top) { return }
        if ($excludeFiles -contains $rel) { return }
        if ($rel -like '*.bak' -or $rel -like '*.new' -or $rel -like '*.tmp') { return }
        $list += $rel
    }
    return ($list | Sort-Object)
}

function Next-Version([string]$cur) {
    if (-not $cur) { return '1.2.0' }
    $p = $cur.Split('.')
    if ($p.Count -lt 3) { return ($cur + '.1') }
    $p[2] = ([int]$p[2] + 1).ToString()
    return ($p -join '.')
}

# 重打完整分发压缩包（含字体）。这是给"新同事首次安装"用的一次性完整包。
# 排除：.git、_backup、日志、状态、备份文件。
function New-KitZip([string]$outPath) {
    Add-Type -AssemblyName System.IO.Compression.FileSystem -ErrorAction Stop

    $skipDirs  = @('.git', '_backup')
    $skipFiles = @('install.log', 'install-state.json')

    $list = @()
    Get-ChildItem -LiteralPath $root -Recurse -File -Force | ForEach-Object {
        $rel = $_.FullName.Substring($root.Length + 1) -replace '\\', '/'
        $parts = $rel -split '/'
        if ($parts | Where-Object { $skipDirs -contains $_ }) { return }
        if ($skipFiles -contains $rel) { return }
        if ($rel -like '*.bak' -or $rel -like '*.new' -or $rel -like '*.tmp') { return }
        $list += $rel
    }
    $list = $list | Sort-Object

    $tmp = $outPath + '.tmp'
    if (Test-Path -LiteralPath $tmp) { Remove-Item -LiteralPath $tmp -Force }
    $zip = [IO.Compression.ZipFile]::Open($tmp, 'Create')
    try {
        foreach ($rel in $list) {
            $full = Join-Path $root ($rel -replace '/', '\')
            [IO.Compression.ZipFileExtensions]::CreateEntryFromFile(
                $zip, $full, $rel, [IO.Compression.CompressionLevel]::Optimal) | Out-Null
        }
    } finally {
        $zip.Dispose()
    }
    Move-Item -LiteralPath $tmp -Destination $outPath -Force
    return $list.Count
}

Write-Host ''
Write-Host '  ZWCAD 工具包 - 发布' -ForegroundColor Cyan
Write-Host '  ------------------------------------------------' -ForegroundColor DarkCyan
Write-Host ''

$gitExe = Resolve-Git
if (-not $gitExe) {
    Say '  找不到 git。请先安装 Git for Windows（https://git-scm.com/download/win）。' 'Red'
    exit 1
}
Say ('  git: ' + $gitExe) 'DarkGray'

# ---------- 1. 版本号 ----------
$localJson = Join-Path $root 'version.json'
$curVer = ''
$oldMap = @{}
if (Test-Path -LiteralPath $localJson) {
    try {
        $old = Get-Content -LiteralPath $localJson -Raw -Encoding UTF8 | ConvertFrom-Json
        $curVer = $old.version
        foreach ($p in $old.files.PSObject.Properties) { $oldMap[$p.Name] = [string]$p.Value }
    } catch {}
}
$versionGiven = [bool]$Version

# ---------- 2. 生成清单 ----------
Say '  [1/5] 计算文件指纹...' 'Cyan'
$files = Get-PublishFiles
$map = [ordered]@{}
foreach ($rel in $files) {
    $full = Join-Path $root ($rel -replace '/', '\')
    $map[$rel] = (Get-FileHash -LiteralPath $full -Algorithm SHA256).Hash.ToLower()
}

# 和上一版逐文件比对，判断代码是否真的变了
$sameAsBefore = $false
if ($curVer -and $oldMap.Count -eq $map.Count) {
    $same = $true
    foreach ($k in $map.Keys) {
        if (-not $oldMap.ContainsKey($k) -or $oldMap[$k] -ne $map[$k]) { $same = $false; break }
    }
    $sameAsBefore = $same
}

if (-not $versionGiven) {
    if ($sameAsBefore) { $Version = $curVer } else { $Version = Next-Version $curVer }
}
Say ('  版本: ' + $curVer + '  ->  ' + $Version + $(if ($sameAsBefore) { '   (代码无变化)' } else { '' })) 'Cyan'

if ($sameAsBefore -and -not $versionGiven) {
    Say ('        ' + $files.Count + ' 个文件与上一版完全一致，不重写 version.json') 'DarkGray'
} else {
    $manifest = [ordered]@{
        version = $Version
        date    = (Get-Date -Format 'yyyy-MM-dd HH:mm')
        source  = 'ybbms777/zwcad-2020-toolkit-code'
        branch  = 'main'
        files   = $map
    }
    [IO.File]::WriteAllText($localJson, ($manifest | ConvertTo-Json -Depth 5),
                            [Text.UTF8Encoding]::new($false))
    Say ('        ' + $files.Count + ' 个文件，清单已写入 version.json') 'Gray'
}

# ---------- 3. 同步到公开仓库目录 ----------
Say '  [2/5] 同步代码到公开仓库...' 'Cyan'
if (-not (Test-Path -LiteralPath $pubDir)) {
    Say '        公开仓库目录不存在，尝试 clone...' 'DarkGray'
    $out = & $gitExe clone $pubRepo $pubDir 2>&1 | Out-String
    if ($LASTEXITCODE -ne 0) {
        Say '        clone 失败。' 'Red'
        Say '        请先在 GitHub 上创建公开仓库: zwcad-2020-toolkit-code' 'Yellow'
        Say ('        git 输出: ' + ([string]$out).Trim()) 'DarkGray'
        exit 1
    }
} else {
    $g = Run-Git $pubDir @('fetch', 'origin')
    if ($g.Code -ne 0) { Say ('        fetch 警告: ' + $g.Out) 'DarkGray' }
    $g = Run-Git $pubDir @('reset', '--hard', 'origin/main')
    if ($g.Code -ne 0) { Say ('        reset 警告（首次发布可忽略）: ' + $g.Out) 'DarkGray' }
}

# 清掉旧代码（保留 .git）
Get-ChildItem -LiteralPath $pubDir -Force | Where-Object { $_.Name -ne '.git' } | ForEach-Object {
    Remove-Item -LiteralPath $_.FullName -Recurse -Force
}
foreach ($rel in $files) {
    $src = Join-Path $root ($rel -replace '/', '\')
    $dst = Join-Path $pubDir ($rel -replace '/', '\')
    $d = Split-Path $dst -Parent
    if (-not (Test-Path -LiteralPath $d)) { New-Item -ItemType Directory -Path $d -Force | Out-Null }
    Copy-Item -LiteralPath $src -Destination $dst -Force
}
Copy-Item -LiteralPath $localJson -Destination (Join-Path $pubDir 'version.json') -Force
Say ('        已同步 ' + $files.Count + ' 个文件（不含字体）') 'Gray'

# ---------- 4. 提交并推送 ----------
if (-not $Message) { $Message = '更新到 ' + $Version }

Say '  [3/5] 推送到公开仓库...' 'Cyan'
$g = Run-Git $pubDir @('add', '-A')
if ($g.Code -ne 0) { Say ('        git add 失败: ' + $g.Out) 'Red'; exit 1 }
$st = (& $gitExe -C $pubDir status --porcelain 2>&1 | Out-String).Trim()
if ($st) {
    $g = Run-Git $pubDir @('commit', '-q', '-m', $Message)
    if ($g.Code -ne 0) { Say ('        commit 失败: ' + $g.Out) 'Red'; exit 1 }
    $g = Run-Git $pubDir @('push', '-u', 'origin', 'HEAD:main')
    if ($g.Code -ne 0) { Say ('        push 失败: ' + $g.Out) 'Red'; exit 1 }
    Say '        公开仓库已更新' 'Green'
} else {
    Say '        公开仓库无变化' 'DarkGray'
}

Say '  [4/5] 推送到私有仓库（含字体全量备份）...' 'Cyan'
$g = Run-Git $root @('add', '-A')
if ($g.Code -ne 0) { Say ('        git add 失败: ' + $g.Out) 'Red'; exit 1 }
$st2 = (& $gitExe -C $root status --porcelain 2>&1 | Out-String).Trim()
if ($st2) {
    $g = Run-Git $root @('commit', '-q', '-m', $Message)
    if ($g.Code -ne 0) { Say ('        commit 失败: ' + $g.Out) 'Red'; exit 1 }
    $g = Run-Git $root @('push', 'origin', 'HEAD:main')
    if ($g.Code -ne 0) { Say ('        push 失败: ' + $g.Out) 'Red'; exit 1 }
    Say '        私有仓库已更新' 'Green'
} else {
    Say '        私有仓库无变化' 'DarkGray'
}

# ---------- 5. 重打完整分发压缩包 ----------
if ($NoZip) {
    Say '  [5/5] 已指定 -NoZip，跳过重打压缩包' 'DarkGray'
} else {
    Say '  [5/5] 重打完整压缩包（含字体，约需十几秒）...' 'Cyan'
    try {
        $n = New-KitZip $zipPath
        $mb = [Math]::Round((Get-Item -LiteralPath $zipPath).Length / 1MB, 1)
        Say ('        ' + $n + ' 个条目，' + $mb + ' MiB') 'Gray'
        Say ('        ' + $zipPath) 'Gray'
    } catch {
        Say ('        压缩包生成失败: ' + $_.Exception.Message) 'Red'
        Say '        其余步骤已完成，可稍后手工重打。' 'Yellow'
    }
}

Write-Host ''
Say ('  发布完成：' + $Version) 'Green'
Say '  同事/客户双击"更新.cmd"即可拿到新版。' 'Gray'
Say '  新同事拿压缩包做首次安装。' 'Gray'
Write-Host ''
exit 0
