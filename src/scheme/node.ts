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

export interface INode {
  type: TokenType
  start: number
  end: number
}

export class NodeAtom implements INode {
  constructor(
    public type: TokenType,
    public value: string,
    public start: number,
    public end: number,
  ) { }
}

export class NodeContainer implements INode {
  constructor(
    public type: TokenType,
    public body: INode,
    public start: number,
    public end: number,
  ) { }
}
