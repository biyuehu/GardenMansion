import { watch } from 'node:fs'
import { type Subprocess, spawn } from 'bun'

const CMD = process.env.WATCH_CMD ?? 'spago run'
const PATTERNS = (process.env.WATCH_PATTERNS ?? '**/*.purs,**/*.yaml')
  .split(',')
  .map((s) => s.trim())
  .filter(Boolean)
const DEBOUNCE_MS = Number(process.env.WATCH_DEBOUNCE ?? 300)
const IS_WINDOWS = process.platform === 'win32'

let current: Subprocess | null = null
let restarting = false
let debounceTimer: ReturnType<typeof setTimeout> | null = null
let pendingRestart = false

function log(msg: string) {
  console.log(`[watch] ${msg}`)
}

async function killCurrentAndWait(): Promise<void> {
  if (!current) return
  const proc = current
  const pid = proc.pid
  current = null

  log(`killing process tree, pid=${pid}`)
  if (IS_WINDOWS) {
    const killer = spawn(['taskkill', '/PID', String(pid), '/T', '/F'], {
      stdout: 'inherit',
      stderr: 'inherit'
    })
    await killer.exited
  } else {
    try {
      process.kill(-pid, 'SIGKILL')
    } catch {
      process.kill(pid, 'SIGKILL')
    }
  }
  await Promise.race([proc.exited, new Promise((resolve) => setTimeout(resolve, 8000))])
  log('process tree killed')
}

function startNew(): void {
  log(`start: ${CMD}`)
  const [bin, ...args] = CMD.split(' ')
  current = spawn([bin, ...args], { stdout: 'inherit', stderr: 'inherit', stdin: 'inherit' })
  current.exited.then((code) => {
    if (!restarting) log(`process exited on its own, code=${code}`)
  })
}

async function restart(): Promise<void> {
  if (restarting) {
    pendingRestart = true
    return
  }
  restarting = true
  do {
    pendingRestart = false
    await killCurrentAndWait()
    startNew()
  } while (pendingRestart)
  restarting = false
}

async function shutdown(signal: string): Promise<void> {
  log(`received ${signal}, cleaning up`)
  await killCurrentAndWait()
  process.exit(0)
}

process.on('SIGINT', () => shutdown('SIGINT'))
process.on('SIGTERM', () => shutdown('SIGTERM'))

log(`watching: ${PATTERNS.join(', ')}`)
log(`command: ${CMD}`)
watch(process.cwd(), { recursive: true }, (_event, filename) => {
  if (!filename) return
  const normalized = filename.replace(/\\/g, '/')
  if (['node_modules', '.git', 'output', '.spago', 'dist', 'build'].some((d) => normalized.split('/').includes(d)))
    return
  if (
    PATTERNS.some((pattern) => {
      const regexStr =
        '^' +
        pattern
          .replace(/[.+^${}()|[\]\\]/g, '\\$&')
          .replace(/\*\*/g, '§DS§')
          .replace(/\*/g, '[^/]*')
          .replace(/§DS§/g, '.*') +
        '$'
      return new RegExp(regexStr).test(normalized.replace(/\\/g, '/'))
    })
  ) {
    log(`change detected (${normalized}), restart in ${DEBOUNCE_MS}ms`)
    if (debounceTimer) clearTimeout(debounceTimer)
    debounceTimer = setTimeout(() => restart(), DEBOUNCE_MS)
  }
})
startNew()
