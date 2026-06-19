export const pid = process.pid

export const handleShowTypeclass = (value) => (show) => (typeof value === 'string' ? value : show(value))
