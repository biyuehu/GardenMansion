import { createServer } from 'node:http'
import sirv from 'sirv'

export const createServerPrim = (callback) => (tuple) => (parseMethod) => () =>
  createServer((req, res) => {
    let body = ''
    req.on('data', (chunk) => {
      body += chunk.toString()
    })
    req.on('end', () => {
      const { searchParams, pathname } = new URL(req.url ?? '', `http://${req.headers.host}`)
      try {
        callback({
          query: Array.from(searchParams).map(([k, v]) => tuple(k)(v)),
          body,
          path: pathname,
          headers: Object.entries(req.headers).map(([k, v]) => tuple(k)(v ? (Array.isArray(v) ? v.join(',') : v) : '')),
          method: parseMethod(req.method ?? 'GET')
        })(req)(res)()
      } catch (error) {
        console.error('PureScript Error:', error)
        res.statusCode = 500
        res.end('Internal Server Error')
      }
    })
  })

export const createStaticHandler = (assetsDir) => (req) => (res) => () => {
  try {
    sirv(assetsDir)(req, res)
  } catch (error) {
    console.error('Error serving static assets:', error)
  }
}

export const listen = (server) => (port) => (callback) => () => server.listen(port, () => callback())
export const close = (server) => () => server.close()
