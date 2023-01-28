enum tokenKind {
  WhiteSpace,
  Boolean,
  LParen,
  RParen,
  Symbol,
  Number,
  String,
  Quote,
  Comment,
  EOF,
}

export class Token {
  constructor(public kind: tokenKind, public value: string){}
}

export class Parser {
  private readonly regexSymbol = /^[^\s()',]+/;
  private readonly regexNumber = /^[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?/;
  private readonly regexString = /^"([^\\"]+|\\.)*"/;
  private readonly regexWhiteSpace = /^\s*/;
  private readonly regexBoolean = /^#t|^#f/;
  private readonly regexComment = /^;.*/;

  private tokens: Token[] = [];
  private cursor = 0;

  public parse(sentence: string) {

  }
}
