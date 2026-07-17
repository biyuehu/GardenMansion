import { execFileSync } from 'node:child_process'
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

export const readDhallFilePrim = (filePath) => (left) => (right) => () => {
  try {
    return right(
      execFileSync('dhall-to-json', ['--file', filePath], {
        encoding: 'utf-8',
        maxBuffer: 10 * 1024 * 1024
      })
    )
  } catch (err) {
    let message
    if (err.code === 'ENOENT') {
      message = 'dhall-to-json binary not found in PATH. Is dhall-json installed?'
    } else {
      const stderr = err.stderr ? err.stderr.toString().trim() : ''
      message = stderr.length > 0 ? stderr : err.message
    }
    return left(message)
  }
}
