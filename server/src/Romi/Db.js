import DB from '../../../db/build/exec/Main.js'

export const dbCreatePrim = DB.create
export const dbGetPrim = DB.get
export const dbPutPrim = DB.put
export const dbPutOrPrim = DB.putOr
// export const dbPutOrIfPrim = DB.putOrIf
export const dbDelPrim = DB.del
// export const dbDelOrIfPrim = DB.delOrIf
export const toBatchOpPutPrim = (k, v) => ({ type: 'put', key: k, value: v })
export const toBatchOpDelPrim = (k) => ({ type: 'del', key: k })
export const dbBatchPrim = DB.batch
export const dbClosePrim = DB.close
