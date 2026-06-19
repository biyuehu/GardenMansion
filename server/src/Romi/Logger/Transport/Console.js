import dayjs from 'dayjs'

export const renderTime = (template) => (time) => dayjs(time).format(template)
