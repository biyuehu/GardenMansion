import { join } from 'node:path'

export const endsWith = (suffix) => (str) => str.endsWith(suffix)
export const startsWith = (prefix) => (str) => str.startsWith(prefix)
export const currentDir = process.cwd()
export const pathJoin = (a) => (b) => join(a, b)
export const encodeJson = JSON.stringify.bind(JSON)
export const encodeBase64 = btoa
export const decodeBase64 = atob
