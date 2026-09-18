# ============================================================
#  ZWCAD 2020 整合工具包 - 云更新
#
#  只下载发生变化的文件（通常 1-3 个、几 KB），
#  fonts\ 不参与更新，selection.lsp 等本机配置不会被覆盖。
#  目标机器无需安装 git。
# ============================================================
$ErrorActionPreference = 'Continue'
try { [Console]::OutputEncoding = [Text.UTF8Encoding]::new($false) } catch {}
try { [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12 } catch {}

$root = $PSScriptRoot
$repo = 'ybbms777/zwcad-2020-toolkit-code'
$sources = @(
    'https://raw.githubusercontent.com/' + $repo + '/main/',
    'https://cdn.jsdelivr.net/gh/' + $repo + '@main/'
)

# 这些永远不覆盖（本机状态 / 用户选择 / 大文件 / 备份）
$never = @('fonts/', 'selection.lsp', 'install-state.json', 'install.log', 'version.json', '_backup/')

function Say([string]$t, [string]$c = 'Gray') { Write-Host $t -ForegroundColor $c }
function Sha([string]$p) { (Get-FileHash -LiteralPath $p -Algorithm SHA256).Hash.ToLower() }

function Get-Remote([string]$rel, [string]$outFile) {
    foreach ($s in $sources) {
        try {
            $wc = New-Object Net.WebClient
            $wc.Headers.Add('User-Agent', 'ZWKit-Updater')
            $wc.Headers.Add('Cache-Control', 'no-cache')
            if ($outFile) { $wc.DownloadFile($s + $rel, $outFile) } else { return $wc.DownloadString($s + $rel) }
            return $true
        } catch { }
    }
    return $false
}

Write-Host ''
Write-Host '  ZWCAD 工具包 - 云更新' -ForegroundColor Cyan
Write-Host '  ------------------------------------------------' -ForegroundColor DarkCyan
Write-Host ''

# ---------- 1. 本地版本 ----------
$localVer = '(未记录)'
$localPath = Join-Path $root 'version.json'
if (Test-Path -LiteralPath $localPath) {
    try { $localVer = (Get-Content -LiteralPath $localPath -Raw -Encoding UTF8 | ConvertFrom-Json).version } catch {}
}

# ---------- 2. 拉云端清单 ----------
Say '  [1/4] 连接更新源...' 'Cyan'
$manifest = $null
$tmpJson = Join-Path $env:TEMP ('zwkit-ver-' + [Guid]::NewGuid().ToString('N') + '.json')
if (Get-Remote 'version.json' $tmpJson) {
    try { $manifest = Get-Content -LiteralPath $tmpJson -Raw -Encoding UTF8 | ConvertFrom-Json } catch {}
}
if (Test-Path -LiteralPath $tmpJson) { Remove-Item -LiteralPath $tmpJson -Force -ErrorAction SilentlyContinue }

if (-not $manifest) {
    Say '        无法连接更新源，请检查网络后重试。' 'Red'
    Say ('        更新源: ' + $sources[0]) 'DarkGray'
    exit 1
}
Say ('        本地版本 : ' + $localVer) 'Gray'
Say ('        云端版本 : ' + $manifest.version + '   (' + $manifest.date + ')') 'Gray'

$want = @{}
foreach ($p in $manifest.files.PSObject.Properties) { $want[$p.Name] = [string]$p.Value }

# ---------- 3. 比对 ----------
Say '  [2/4] 比对文件...' 'Cyan'
$changed = @()
$missing = @()
foreach ($rel in ($want.Keys | Sort-Object)) {
    if ($never | Where-Object { $rel.StartsWith($_) }) { continue }
    $full = Join-Path $root ($rel -replace '/', '\')
    if (Test-Path -LiteralPath $full) {
        if ((Sha $full) -ne $want[$rel]) { $changed += $rel }
    } else {
        $changed += $rel
        $missing += $rel
    }
}

if (-not $changed.Count) {
    Say '        已是最新版本，无需更新。' 'Green'
    Say ''
    exit 0
}
Say ('        需要更新 ' + $changed.Count + ' 个文件:') 'Yellow'
foreach ($rel in $changed) { Say ('          - ' + $rel + $(if ($missing -contains $rel) { '  (新增)' } else { '' })) 'DarkGray' }

# ---------- 4. CAD 占用检查 ----------
$cad = Get-Process ZWCAD -ErrorAction SilentlyContinue
if ($cad) {
    Say ''
    Say '  注意：中望 CAD 正在运行。' 'Yellow'
    Say '        插件 DLL 被占用时无法替换，建议先关闭 CAD 再更新。' 'Yellow'
}

# ---------- 5. 下载并替换 ----------
Say ''
Say '  [3/4] 下载并校验...' 'Cyan'
$stamp = Get-Date -Format 'yyyyMMdd-HHmmss'
$backupDir = Join-Path $root ('_backup\' + $stamp)
$done = 0; $failed = @()

foreach ($rel in $changed) {
    $full = Join-Path $root ($rel -replace '/', '\')
    $dir = Split-Path $full -Parent
    if (-not (Test-Path -LiteralPath $dir)) { New-Item -ItemType Directory -Path $dir -Force | Out-Null }

    $tmp = $full + '.new'
    if (Test-Path -LiteralPath $tmp) { Remove-Item -LiteralPath $tmp -Force -ErrorAction SilentlyContinue }
    if (-not (Get-Remote $rel $tmp)) {
        Say ('        x 下载失败  ' + $rel) 'Red'
        $failed += $rel
        continue
    }
    if ((Sha $tmp) -ne $want[$rel]) {
        Say ('        x 校验不通过（文件可能不完整）  ' + $rel) 'Red'
        Remove-Item -LiteralPath $tmp -Force -ErrorAction SilentlyContinue
        $failed += $rel
        continue
    }

    # 备份旧文件
    if (Test-Path -LiteralPath $full) {
        $bdir = Join-Path $backupDir (Split-Path $rel -Parent)
        if ($bdir -and -not (Test-Path -LiteralPath $bdir)) { New-Item -ItemType Directory -Path $bdir -Force | Out-Null }
        Copy-Item -LiteralPath $full -Destination (Join-Path $backupDir ($rel -replace '/', '\')) -Force
    }

    try {
        Move-Item -LiteralPath $tmp -Destination $full -Force
        Say ('        + ' + $rel) 'Green'
        $done++
    } catch {
        Say ('        x 替换失败（文件被占用？）  ' + $rel) 'Red'
        Say ('          ' + $_.Exception.Message) 'DarkGray'
        Remove-Item -LiteralPath $tmp -Force -ErrorAction SilentlyContinue
        $failed += $rel
    }
}

# ---------- 6. 写本地版本 ----------
Say ''
Say '  [4/4] 写入版本信息...' 'Cyan'
if (-not $failed.Count) {
    try {
        [IO.File]::WriteAllText($localPath, (Get-Remote 'version.json' $null), [Text.UTF8Encoding]::new($false))
        Say ('        本地版本已更新为 ' + $manifest.version) 'Gray'
    } catch {
        Say '        版本文件写入失败，下次运行会重新比对（不影响功能）。' 'DarkGray'
    }
}

Write-Host ''
if ($failed.Count) {
    Say ('  完成 ' + $done + ' 个，失败 ' + $failed.Count + ' 个。' ) 'Yellow'
    Say '  失败的文件请在关闭 CAD 后重新运行本更新。' 'Yellow'
    if (Test-Path -LiteralPath $backupDir) { Say ('  旧版备份: ' + $backupDir) 'DarkGray' }
    exit 1
}
Say ('  更新完成：' + $done + ' 个文件，已升级到 ' + $manifest.version) 'Green'
Say '  重新打开中望 CAD 即可生效。' 'Green'
if (Test-Path -LiteralPath $backupDir) { Say ('  旧版备份: ' + $backupDir) 'DarkGray' }
Write-Host ''
exit 0
