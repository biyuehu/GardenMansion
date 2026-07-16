import 'uno.css'
import './assets/style.css'
import { Elm } from './Main.elm'

if (process.env.NODE_ENV === 'development') {
  const ElmDebugTransform = await import('elm-debug-transformer')
  ElmDebugTransform.register({ simple_mode: true })
}

const root = document.querySelector('#app')
if (root === null) {
  throw new Error('Root element not found')
}

const app = Elm.Main.init({
  node: root,
  flags: localStorage.getItem('himeno_token') || null
})

app.ports.saveToken.subscribe((token: string | null) => {
  if (token === null) {
    localStorage.removeItem('himeno_token')
  } else {
    localStorage.setItem('himeno_token', token)
  }
})
