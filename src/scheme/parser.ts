/**
 * parser 接受一个 st 字符串，然后返回一个 ast 结构
 * 注意：为了更好地扩展性，这里不涉及任何 let、define 等语法，这些语法在后面处理
 */

import { TokenType, NodeAtom, NodeContainer, INode } from './node';

const regexList: [TokenType, RegExp][] = [
  [TokenType.Quote, /^'/],
  [TokenType.LParen, /^\(/],
  [TokenType.RParen, /^\)/],
  [TokenType.Symbol, /^[^\s()',]+/],
  [TokenType.Number, /^[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?/],
  [TokenType.String, /^"([^\\"]+|\\.)*"/],
  [TokenType.WhiteSpace, /^\s*/],
  [TokenType.Boolean, /^#t|^#f/],
  [TokenType.Comment, /^;.*/]
]

interface TokenItem {
  token: TokenType
  value: string
  len: number
}

export function parse(st: string) {
  let index = 0
  const nodeList = tokenizer()
  let nodeCursor = 0
  return parseExpression()

  function getNextToken(): TokenItem {
    for (const [token, reg] of regexList) {
      const matched = reg.exec(st)
      const value = matched[0]

      if (matched) {
        return {
          token,
          value,
          len: value.length
        }
      }
    }

    return {
      token: TokenType.EOF,
      value: null,
      len: 0
    }
  }

  function advance(n: number): void {
    index += n
    st = st.substring(n)
  }

  function tokenizer(): INode[] {
    const stack = []

    while (true) {
      const { token, value, len } = getNextToken()

      if (INode.isContainerType(token)) {
        stack.push(new NodeContainer(token, index, index + len))
      } else {
        stack.push(new NodeAtom(token, index, index + len, value))
      }

      advance(len)

      if (token === TokenType.EOF) {
        break
      }
    }

    return stack
  }

  function parseExpression(): NodeAtom | NodeContainer {
    let lastNode = null
    let firstNode = null

    while (nodeCursor < nodeList.length) {
      const currentNode = nodeList[nodeCursor++]

      if (currentNode.isOpenToken()) {
        break
      }

      if (currentNode.isCloseToken()) {
        (currentNode as NodeContainer).body = parseExpression()
      }

      if (lastNode) {
        lastNode.next = currentNode
      }

      if (!firstNode) {
        firstNode = currentNode
      }

      lastNode = currentNode
    }

    return firstNode
  }
}
