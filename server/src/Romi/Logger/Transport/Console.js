import { Colors, TerminalAdapter } from '@kotori-bot/tools'
import dayjs from 'dayjs'

const colors = new Colors(new TerminalAdapter())

export const colorize = (msg) => colors.parse(msg)

const DEFAULT_LOGGER_LEVELS = {
  FATAL: ['<redBright><bold>FATAL</bold></redBright> ', 'redBright'],
  ERROR: ['<red>ERROR</red>', 'red'],
  WARN: ['<yellow>WARN</yellow>', 'yellowBright'],
  INFO: ['<green>INFO</green>'],
  RECORD: ['<cyan>LOG</cyan>'],
  DEBUG: ['<magenta>DEBUG</magenta>', 'magentaBright'],
  TRACE: ['<gray>TRACE</gray>', 'gray']
}

export const getBasicData = (timeFormat) => (time) => (msg) => (level) => {
  const levels = DEFAULT_LOGGER_LEVELS[level]
  return {
    time: dayjs(time).format(timeFormat),
    level: levels[0],
    msg: levels?.[1] ? `<${levels[1]}>${msg}</${levels[1]}>` : msg
  }
}
