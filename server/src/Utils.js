import { join } from 'node:path'

export const endsWith = (suffix) => (str) => str.endsWith(suffix)
export const startsWith = (prefix) => (str) => str.startsWith(prefix)
export const currentDir = process.cwd()
export const pathJoin = (a) => (b) => join(a, b)
export const encodeBase64 = btoa

export const decodeBase64Prim = (str) => (just) => (nothing) => {
  try {
    return just(atob(str))
  } catch {
    nothing
  }
}

export const onShutdownSignal = (cleanup) => () => {
  let called = false
  const handler = (signal) => () => {
    if (called) return
    called = true
    try {
      cleanup()
    } catch (e) {
      console.error('Failed to exit:', e)
    } finally {
      process.exit(0)
    }
  }
  process.on('SIGTERM', handler('SIGTERM'))
  process.on('SIGINT', handler('SIGINT'))
}
