import { join } from 'node:path'

export const endsWith = (suffix) => (str) => str.endsWith(suffix)
export const startsWith = (prefix) => (str) => str.startsWith(prefix)
export const currentDir = process.cwd()
export const pathJoin = (a) => (b) => join(a, b)
