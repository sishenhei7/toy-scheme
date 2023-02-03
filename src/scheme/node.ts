import { should } from '../../../vue-sourcecode/vue/packages/server-renderer/src/bundle-renderer/create-bundle-runner';
export enum TokenType {
  Quote,
  LParen,
  RParen,
  Symbol,
  Number,
  String,
  WhiteSpace,
  Boolean,
  Comment,
  EOF,
}

export class INode {
  type: TokenType
  start: number
  end: number

  curr = null
  next = null

  public setChain(curr: INode, next: INode): void {
    this.curr = curr
    this.next = next
  }

  public isOpenToken(): boolean {
    return [TokenType.LParen].includes(this.type)
  }

  public isCloseToken(): boolean {
    return [TokenType.RParen].includes(this.type)
  }

  public isIgnoreToken(): boolean {
    return [TokenType.WhiteSpace, TokenType.Comment].includes(this.type)
  }

  static isContainerType(token: TokenType): boolean {
    return [TokenType.LParen, TokenType.RParen].includes(token)
  }
}

export class NodeAtom extends INode {
  constructor(
    public type: TokenType,
    public start: number,
    public end: number,
    public value: string = '',
  ) {
    super()
  }
}

export class NodeContainer extends INode {
  constructor(
    public type: TokenType,
    public start: number,
    public end: number,
    public body: INode = null,
  ) {
    super()
  }
}
