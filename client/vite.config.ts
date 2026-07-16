import UnoCSS from 'unocss/vite'
import { defineConfig } from 'vite'
import Elm from 'vite-plugin-elm'

export default defineConfig({
  plugins: [Elm(), UnoCSS()],
  server: {
    proxy: {
      '/api': {
        target: 'http://localhost:8080/',
        changeOrigin: true
      }
    }
  }
})
