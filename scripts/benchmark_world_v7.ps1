param(
    [ValidateSet('7.0', '6.5.2')][string]$Version = '7.0',
    [switch]$Pan,
    [switch]$Preload,
    [ValidatePattern('^\d{1,2},\d{1,2}$')][string]$Center,
    [ValidateSet(1280, 1920, 2560)][int]$Width
)

$mode = if ($Pan) { 'world-v7-pan' } else { 'world-v7' }
$options = @("-J-Dsettlement.world.version=$Version", '-M', 'scripts/benchmark_ui_v1.clj', $mode)
if ($Preload) { $options += '--preload' }
if ($Center) { $options += "--center=$Center" }
if ($Width) { $options += "--width=$Width" }

# Captures launcher overhead before the JVM's own uptime measurement starts.
$env:SETTLEMENT_BENCH_LAUNCHED_AT = [DateTimeOffset]::UtcNow.ToUnixTimeMilliseconds().ToString()
try {
    clojure @options
    exit $LASTEXITCODE
} finally {
    Remove-Item Env:SETTLEMENT_BENCH_LAUNCHED_AT -ErrorAction SilentlyContinue
}
