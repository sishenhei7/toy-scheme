/**
 * parser 接受一个 st 字符串，然后返回一个 ast 结构
 * 注意：为了更好地扩展性，这里不涉及任何 let、define 等语法，这些语法在后面处理
 */

import tokenize from './token'
import parseToken from './data'

export function parse(st: string): ReturnType<typeof parseToken> {
  const tokenList = tokenize(st).filter((token) => !token.isIgnoreToken())
  console.log(tokenList)
  return parseToken(tokenList)
}
