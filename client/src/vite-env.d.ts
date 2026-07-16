declare module '*Main.elm' {
  export namespace Elm {
    export namespace Main {
      // biome-ignore lint/suspicious/noExplicitAny: *
      function init(options: object): any
    }
  }
}

declare const process: { env: { NODE_ENV: string } }

declare module '*.css' {
  const content: unknown
  export default content
}

declare module 'uno.css' {
  const content: unknown
  export default content
}
