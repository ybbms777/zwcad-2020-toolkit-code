# ============================================================
#  ZWCAD 2020 整合工具包 - 发布
#
#  一次做完 5 件事：
#    1. 重算全部代码文件的 sha256 -> version.json（含字体版本号、本版更新说明）
#    2. 同步「不含字体」的代码到公开仓库工作目录
#    3. 推送公开仓库（更新源，给已装好的同事用）
#    4. 推送私密仓库（含版权字体的完整版，做备份）
#    5. 在桌面重打完整分发包，文件名带字体版本号
#
#  用法：
#    .\publish.ps1                    版本号自动 +1，字体没变则字体版本号不变
#    .\publish.ps1 -Version 1.3       指定版本号
#    .\publish.ps1 -Note "修了xx","新增yy"   写明本次更新内容（不填会交互询问）
#    .\publish.ps1 -Message "提交说明"  自定义 git 提交说明
#    .\publish.ps1 -NoZip             跳过重打压缩包
#
#  规则（用户明确要求，别改）：
#    - 本地桌面统一保留「带字体」的完整包，文件名必须带字体版本号
#    - 私密仓库 = 带版权字体的完整版；公开仓库 = 不含字体的代码版
#    - 每次更新/修 BUG 都要写更新说明，写进 CHANGELOG.md 和 version.json
#    - 只有版本号真正更新时才追加更新说明（代码没变不写）
# ============================================================
param(
    [string]$Version,
    [string]$Message,
    [string[]]$Note,
    [switch]$NoZip
)

# 注意：不能用 Stop —— git 往 stderr 写东西时会被当成终止错误，
# 导致失败信息还没打印脚本就崩了。所有 git 调用都显式检查退出码。
$ErrorActionPreference = 'Continue'
try { [Console]::OutputEncoding = [Text.UTF8Encoding]::new($false) } catch {}

$root    = $PSScriptRoot
$pubDir  = 'C:\Users\Administrator\zwcad-toolkit-publish'
$pubRepo = 'git@github.com:ybbms777/zwcad-2020-toolkit-code.git'
$zipDir  = 'C:\Users\Administrator\Desktop'

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

# 字体目录整体指纹：只有它变了，字体版本号才跳
function Get-FontsHash {
    $fontDir = Join-Path $root 'fonts'
    if (-not (Test-Path -LiteralPath $fontDir)) { return '' }
    $items = @()
    Get-ChildItem -LiteralPath $fontDir -Recurse -File -Force | Sort-Object FullName | ForEach-Object {
        $rel = $_.FullName.Substring($fontDir.Length + 1) -replace '\\', '/'
        $items += ($rel + ':' + (Get-FileHash -LiteralPath $_.FullName -Algorithm SHA256).Hash.ToLower())
    }
    $sha = [Security.Cryptography.SHA256]::Create()
    $bytes = [Text.Encoding]::UTF8.GetBytes(($items -join "`n"))
    return (($sha.ComputeHash($bytes) | ForEach-Object { $_.ToString('x2') }) -join '')
}

function Next-Version([string]$cur) {
    if (-not $cur) { return '1.2.0' }
    $p = $cur.Split('.')
    if ($p.Count -lt 3) { return ($cur + '.1') }
    $p[2] = ([int]$p[2] + 1).ToString()
    return ($p -join '.')
}

# 重打完整分发包（含字体）。这是给"新同事首次安装"用的一次性完整包。
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

# 把本版更新说明插到 CHANGELOG.md 最上面（最新的在最前）
function Add-Changelog([string]$ver, [string[]]$notes) {
    $path = Join-Path $root 'CHANGELOG.md'
    $head = "# 更新记录`r`n`r`n最新在最上面。`r`n`r`n"
    $body = ''
    if (Test-Path -LiteralPath $path) {
        $t = [IO.File]::ReadAllText($path, [Text.UTF8Encoding]::new($false))
        $i = $t.IndexOf('## ')
        if ($i -ge 0) { $body = $t.Substring($i) }
    }
    $entry = '## ' + $ver + ' — ' + (Get-Date -Format 'yyyy-MM-dd') + "`r`n" +
             (($notes | ForEach-Object { '- ' + $_ }) -join "`r`n") + "`r`n`r`n"
    [IO.File]::WriteAllText($path, ($head + $entry + $body), [Text.UTF8Encoding]::new($false))
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

# ---------- 1. 读旧清单 ----------
$localJson = Join-Path $root 'version.json'
$curVer = ''; $curFontVer = ''; $curFontsHash = ''; $curNotes = @()
$oldMap = @{}
if (Test-Path -LiteralPath $localJson) {
    try {
        $old = Get-Content -LiteralPath $localJson -Raw -Encoding UTF8 | ConvertFrom-Json
        $curVer = $old.version
        $curFontVer = $old.fontVersion
        $curFontsHash = $old.fontsHash
        if ($old.notes) { $curNotes = @($old.notes) }
        foreach ($p in $old.files.PSObject.Properties) { $oldMap[$p.Name] = [string]$p.Value }
    } catch {}
}
$versionGiven = [bool]$Version

# ---------- 2. 计算指纹 ----------
Say '  [1/5] 计算文件指纹...' 'Cyan'
$files = Get-PublishFiles
$map = [ordered]@{}
foreach ($rel in $files) {
    $full = Join-Path $root ($rel -replace '/', '\')
    $map[$rel] = (Get-FileHash -LiteralPath $full -Algorithm SHA256).Hash.ToLower()
}

$sameAsBefore = $false
if ($curVer -and $oldMap.Count -eq $map.Count) {
    $same = $true
    foreach ($k in $map.Keys) {
        if (-not $oldMap.ContainsKey($k) -or $oldMap[$k] -ne $map[$k]) { $same = $false; break }
    }
    $sameAsBefore = $same
}

$fontsHash = Get-FontsHash
$fontsChanged = ($fontsHash -ne $curFontsHash)
$nothingChanged = ($sameAsBefore -and -not $fontsChanged)

# ---------- 3. 定版本号 ----------
if (-not $versionGiven) {
    if ($sameAsBefore) { $Version = $curVer } else { $Version = Next-Version $curVer }
}
if ($fontsChanged -or -not $curFontVer) {
    $fontVersion = (Get-Date -Format 'yyyy.MM.dd')
} else {
    $fontVersion = $curFontVer
}

Say ('  代码版本: ' + $curVer + '  ->  ' + $Version + $(if ($sameAsBefore) { '   (代码无变化)' } else { '' })) 'Cyan'
Say ('  字体版本: ' + $curFontVer + '  ->  ' + $fontVersion + $(if ($fontsChanged) { '   (字体有变化)' } else { '   (字体无变化)' })) 'Cyan'

# ---------- 4. 更新说明 ----------
$notes = $curNotes
if (-not $nothingChanged) {
    if ($Note -and $Note.Count) {
        $notes = @($Note)
    } elseif ($sameAsBefore -and $fontsChanged) {
        $notes = @('字体更新')
    } else {
        Write-Host ''
        Say '  本次是版本更新，请输入更新说明（每行一条，直接回车结束）:' 'Yellow'
        $lines = @()
        do {
            $l = Read-Host '  -'
            if ($l) { $lines += $l.Trim() }
        } while ($l)
        if (-not $lines.Count) { $lines = @('版本更新') }
        $notes = @($lines)
    }
    Say ('  本版更新内容: ' + $notes.Count + ' 条') 'Gray'
    foreach ($n in $notes) { Say ('    - ' + $n) 'DarkGray' }
}

# ---------- 5. 写 version.json ----------
if ($nothingChanged -and -not $versionGiven) {
    Say ('        ' + $files.Count + ' 个文件与上一版完全一致，不重写 version.json') 'DarkGray'
} else {
    $manifest = [ordered]@{
        version     = $Version
        date        = (Get-Date -Format 'yyyy-MM-dd HH:mm')
        fontVersion = $fontVersion
        fontsHash   = $fontsHash
        notes       = $notes
        source      = 'ybbms777/zwcad-2020-toolkit-code'
        branch      = 'main'
        files       = $map
    }
    [IO.File]::WriteAllText($localJson, ($manifest | ConvertTo-Json -Depth 5),
                            [Text.UTF8Encoding]::new($false))
    Say ('        ' + $files.Count + ' 个文件，清单已写入 version.json') 'Gray'
    if (-not $sameAsBefore -or $versionGiven) {
        Add-Changelog $Version $notes
        Say '        已追加更新说明到 CHANGELOG.md' 'Gray'
    }
}

# ---------- 6. 同步到公开仓库目录 ----------
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

# ---------- 7. 推送 ----------
if (-not $Message) { $Message = '更新到 ' + $Version }

Say '  [3/5] 推送到公开仓库（不含版权字体）...' 'Cyan'
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

Say '  [4/5] 推送到私密仓库（含版权字体，完整备份）...' 'Cyan'
$g = Run-Git $root @('add', '-A')
if ($g.Code -ne 0) { Say ('        git add 失败: ' + $g.Out) 'Red'; exit 1 }
$st2 = (& $gitExe -C $root status --porcelain 2>&1 | Out-String).Trim()
if ($st2) {
    $g = Run-Git $root @('commit', '-q', '-m', $Message)
    if ($g.Code -ne 0) { Say ('        commit 失败: ' + $g.Out) 'Red'; exit 1 }
    $g = Run-Git $root @('push', 'origin', 'HEAD:main')
    if ($g.Code -ne 0) { Say ('        push 失败: ' + $g.Out) 'Red'; exit 1 }
    Say '        私密仓库已更新' 'Green'
} else {
    Say '        私密仓库无变化' 'DarkGray'
}

# ---------- 8. 重打完整包 ----------
$zipName = 'ZWCAD工具包' + $Version + '-字体' + $fontVersion + '.zip'
$zipPath = Join-Path $zipDir $zipName
if ($NoZip) {
    Say '  [5/5] 已指定 -NoZip，跳过重打压缩包' 'DarkGray'
} else {
    Say '  [5/5] 重打完整包（含字体，约需十几秒）...' 'Cyan'
    try {
        $n = New-KitZip $zipPath
        $mb = [Math]::Round((Get-Item -LiteralPath $zipPath).Length / 1MB, 1)
        Say ('        ' + $n + ' 个条目，' + $mb + ' MiB') 'Gray'
        Say ('        ' + $zipPath) 'Gray'
        # 本地统一只保留带字体的最新完整包，清掉旧的
        Get-ChildItem -LiteralPath $zipDir -Filter 'ZWCAD工具包*.zip' -File | Where-Object {
            $_.Name -ne $zipName
        } | ForEach-Object {
            Remove-Item -LiteralPath $_.FullName -Force
            Say ('        已删除旧包 ' + $_.Name) 'DarkGray'
        }
    } catch {
        Say ('        压缩包生成失败: ' + $_.Exception.Message) 'Red'
        Say '        其余步骤已完成，可稍后手工重打。' 'Yellow'
    }
}

Write-Host ''
Say ('  发布完成：代码 ' + $Version + '，字体 ' + $fontVersion) 'Green'
Say ('  完整包: ' + $zipName) 'Gray'
Say '  同事/客户双击"更新.cmd"即可拿到新版（只下变化的文件）。' 'Gray'
Say '  新同事拿完整包做首次安装。' 'Gray'
Write-Host ''
exit 0
