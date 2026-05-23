package com.todormoskov

import scala.collection.mutable.ListBuffer

object Lexer {

  enum TokenKind:
    case
    // End of file
    EOF,
    // Newline
    NEWLINE,

    // Primary
    IDENTIFIER,
    INTEGER,
    STRING,

    // TLA+ Tokens
    // Module-Level Constructs
    MODULE,
    EXTENDS,
    CONSTANT,
    CONSTANTS,
    VARIABLE,
    VARIABLES,
    ASSUME,
    DEFINE,
    INSTANCE,
    WITH,
    LEFT_ARROW,
    THEOREM,
    LOCAL,

    // Symbols in module-level constructs
    COMMA,
    UNDERSCORE,
    LEFT_ROUND_BRACKET,
    RIGHT_ROUND_BRACKET,
    LEFT_SQUARE_BRACKET,
    RIGHT_SQUARE_BRACKET,
    LEFT_CURLY_BRACKET,
    RIGHT_CURLY_BRACKET,
    COLON,

    // Logical operators
    AND,
    OR,
    NOT,
    IMPLICATION,
    EQUIVALENCE,
    TRUE,
    FALSE,
    BOOLEAN,
    FORALL,
    EXISTS,
    CHOOSE,

    // Set operators
    EQ,
    NEQ,
    SET_IN,
    NOTIN,
    UNION,
    INTERSECT,
    SUBSET,
    SUBSETEQ,
    SET_DIFFERENCE,
    BIG_SUBSET,
    BIG_UNION,

    // Functions
    DOMAIN,
    MAPS_TO,
    RIGHT_ARROW,
    EXCEPT,
    BANG,

    // Records
    DOT,

    // Tuples
    LEFT_ANGLE_BRACKET,
    RIGHT_ANGLE_BRACKET,
    TIMES,

    // Miscellaneous constructs
    IF,
    THEN,
    ELSE,
    CASE,
    OTHER,
    LET,
    IN,

    // Actions
    PRIME,
    ENABLED,
    UNCHANGED,
    ACTION_COMPOSITION,

    // Temporal operators
    SQUARE,
    DIAMOND,
    WF,
    SF,
    LEADS_TO,

    // Operators in standard modules
    PLUS,
    MINUS,
    MUL,
    DIV,
    EXP,
    DOT_DOT,
    MOD,

    // Comparison Operators
    LT,
    GT,
    LE,
    GE,

    // Other stuff
    MINUS_SEPARATOR,
    EQUAL_SEPARATOR,
    BACKSLASH_OPERATOR,
    UNKNOWN_OPERATOR,
    AT,
    AT_AT,
    CIRCLE,
    OPLUS,
    OMINUS,
    INT_DIV,
    SQSUBSETEQ,
    COLON_GT,

    EPC_CONTEXT_OPEN,
    EPC_CONTEXT_CLOSE,
    EPC_VAR,
    EPC_ASSIGN,
    EPC_LOC_SCOPE_OPEN,
    EPC_LOC_SCOPE_CLOSE,
    EPC_LOC_MEMORY_CODE_SEP,
    EPC_LOC_COMPOSITION,
    EPC_THREAD_COMPOSITION,
    EPC_MULTISET_OPEN,
    EPC_MULTISET_CLOSE,
    EPC_CHAN,
    EPC_SUSPCHAN,
    EPC_IF,
    EPC_THEN,
    EPC_ELSE,
    EPC_FI,
    EPC_SKIP,
    EPC_DEF,
    EPC_UNDEFINED,
    EPC_INITIALIZED,
    QUESTION_MARK,
    SEMICOLON

    def display: String = this match {
      case TokenKind.EOF => "end of file"
      case TokenKind.NEWLINE => "newline"
      case TokenKind.IDENTIFIER => "identifier"
      case TokenKind.INTEGER => "integer"
      case TokenKind.STRING => "string"
      case TokenKind.MODULE => "keyword 'MODULE'"
      case TokenKind.EXTENDS => "keyword 'EXTENDS'"
      case TokenKind.CONSTANT => "keyword 'CONSTANT'"
      case TokenKind.CONSTANTS => "keyword 'CONSTANTS'"
      case TokenKind.VARIABLE => "keyword 'VARIABLE'"
      case TokenKind.VARIABLES => "keyword 'VARIABLES'"
      case TokenKind.ASSUME => "keyword 'ASSUME'"
      case TokenKind.DEFINE => "'=='"
      case TokenKind.INSTANCE => "keyword 'INSTANCE'"
      case TokenKind.WITH => "keyword 'WITH'"
      case TokenKind.LEFT_ARROW => "'<-'"
      case TokenKind.THEOREM => "keyword 'THEOREM'"
      case TokenKind.LOCAL => "keyword 'LOCAL'"
      case TokenKind.COMMA => "','"
      case TokenKind.UNDERSCORE => "'_'"
      case TokenKind.LEFT_ROUND_BRACKET => "'('"
      case TokenKind.RIGHT_ROUND_BRACKET => "')'"
      case TokenKind.LEFT_SQUARE_BRACKET => "'['"
      case TokenKind.RIGHT_SQUARE_BRACKET => "']'"
      case TokenKind.LEFT_CURLY_BRACKET => "'{'"
      case TokenKind.RIGHT_CURLY_BRACKET => "'}'"
      case TokenKind.COLON => "':'"
      case TokenKind.AND => "'/\\'"
      case TokenKind.OR => "'\\/'"
      case TokenKind.NOT => "'~'"
      case TokenKind.IMPLICATION => "'=>'"
      case TokenKind.EQUIVALENCE => "'<=>'"
      case TokenKind.TRUE => "'TRUE'"
      case TokenKind.FALSE => "'FALSE'"
      case TokenKind.BOOLEAN => "'BOOLEAN'"
      case TokenKind.FORALL => "'\\A'"
      case TokenKind.EXISTS => "'\\E'"
      case TokenKind.CHOOSE => "'CHOOSE'"
      case TokenKind.EQ => "'='"
      case TokenKind.NEQ => "'#'"
      case TokenKind.SET_IN => "'\\in'"
      case TokenKind.NOTIN => "'\\notin'"
      case TokenKind.UNION => "'\\union'"
      case TokenKind.INTERSECT => "'\\intersect'"
      case TokenKind.SUBSET => "'\\subset'"
      case TokenKind.SUBSETEQ => "'\\subseteq'"
      case TokenKind.SET_DIFFERENCE => "'\\'"
      case TokenKind.BIG_SUBSET => "'SUBSET'"
      case TokenKind.BIG_UNION => "'UNION'"
      case TokenKind.DOMAIN => "'DOMAIN'"
      case TokenKind.MAPS_TO => "'|->'"
      case TokenKind.RIGHT_ARROW => "'->'"
      case TokenKind.EXCEPT => "'EXCEPT'"
      case TokenKind.BANG => "'!'"
      case TokenKind.DOT => "'.'"
      case TokenKind.LEFT_ANGLE_BRACKET => "'<<'"
      case TokenKind.RIGHT_ANGLE_BRACKET => "'>>'"
      case TokenKind.TIMES => "'\\times'"
      case TokenKind.IF => "'IF'"
      case TokenKind.THEN => "'THEN'"
      case TokenKind.ELSE => "'ELSE'"
      case TokenKind.CASE => "'CASE'"
      case TokenKind.OTHER => "'OTHER'"
      case TokenKind.LET => "'LET'"
      case TokenKind.IN => "'IN'"
      case TokenKind.PRIME => "'"
      case TokenKind.ENABLED => "'ENABLED'"
      case TokenKind.UNCHANGED => "'UNCHANGED'"
      case TokenKind.ACTION_COMPOSITION => "'\\cdot'"
      case TokenKind.SQUARE => "'[]'"
      case TokenKind.DIAMOND => "'<>'"
      case TokenKind.WF => "'WF_'"
      case TokenKind.SF => "'SF_'"
      case TokenKind.LEADS_TO => "~>"
      case TokenKind.PLUS => "'+'"
      case TokenKind.MINUS => "'-'"
      case TokenKind.MUL => "'*'"
      case TokenKind.DIV => "'/'"
      case TokenKind.EXP => "'^'"
      case TokenKind.DOT_DOT => "'..'"
      case TokenKind.MOD => "'%'"
      case TokenKind.LT => "'<'"
      case TokenKind.GT => "'>'"
      case TokenKind.LE => "'<='"
      case TokenKind.GE => "'>="
      case TokenKind.MINUS_SEPARATOR => "'----'[-]*"
      case TokenKind.EQUAL_SEPARATOR => "'===='[=]*"
      case TokenKind.BACKSLASH_OPERATOR => "'\\'"
      case TokenKind.UNKNOWN_OPERATOR => "???"
      case TokenKind.AT => "'@'"
      case TokenKind.AT_AT => "'@@'"
      case TokenKind.CIRCLE => "\\o"
      case TokenKind.OPLUS => "'(+)'"
      case TokenKind.OMINUS => "'(-)'"
      case TokenKind.INT_DIV => "'\\div'"
      case TokenKind.SQSUBSETEQ => "'\\sqsubseteq'"
      case TokenKind.COLON_GT => "':>'"
      case TokenKind.EPC_CONTEXT_OPEN => "keyword '--EPC'"
      case TokenKind.EPC_CONTEXT_CLOSE => "'*)'"
      case TokenKind.EPC_VAR => "'var'"
      case TokenKind.EPC_ASSIGN => "':='"
      case TokenKind.EPC_LOC_SCOPE_OPEN => "'[['"
      case TokenKind.EPC_LOC_SCOPE_CLOSE => "']]'"
      case TokenKind.EPC_LOC_MEMORY_CODE_SEP => "'<|'"
      case TokenKind.EPC_LOC_COMPOSITION => "'||'"
      case TokenKind.EPC_THREAD_COMPOSITION => "'|'"
      case TokenKind.EPC_MULTISET_OPEN => "'{{'"
      case TokenKind.EPC_MULTISET_CLOSE => "'}}'"
      case TokenKind.EPC_CHAN => "keyword 'chan'"
      case TokenKind.EPC_SUSPCHAN => "keyword 'suspchan'"
      case TokenKind.EPC_IF => "'if'"
      case TokenKind.EPC_THEN => "'then'"
      case TokenKind.EPC_ELSE => "'else'"
      case TokenKind.EPC_FI => "'fi'"
      case TokenKind.EPC_SKIP => "'skip'"
      case TokenKind.EPC_DEF => "keyword 'def'"
      case TokenKind.EPC_UNDEFINED => "'Undefined'"
      case TokenKind.EPC_INITIALIZED => "'Initialized'"
      case TokenKind.QUESTION_MARK => "'?'"
      case TokenKind.SEMICOLON => "';'"
    }

  case class Token(kind: TokenKind, value: String, line: Int, column : Int)

  final class Scanner(input: String):
    // The whole input as a single string
    private val chars = input
    private val len = chars.length
    private var idx = 0
    // A buffer of one character
    private var buffer: Option[Char] = None
    private var column = 0
    private val lineBuffer = new StringBuilder

    def eof: Boolean = idx >= len

    def getColumn: Int = column
    def resetColumn(): Unit = column = 0
    def getLineBuffer: String = lineBuffer.toString()
    def clearLineBuffer(): Unit = lineBuffer.clear()

    def getLine(lineNum: Int): String = {
      chars.linesIterator.zipWithIndex.find(_._2 == lineNum - 1).map(_._1).getOrElse("")
    }

    /**
     * Return the next character from input without consuming it.
     *
     * @return The next character in the input stream or None at the end of the stream.
     */
    def peek(): Option[Char] = {
      if buffer.isEmpty then buffer = consume()
      buffer
    }

    /**
     * Consume and return the next character from input.
     *
     * @return The next character in the input stream or None at the end of the stream.
     */
    def consume(): Option[Char] =
      // If the buffer is full, we empty the buffer and return the character it contains.
      if (buffer.isDefined) {
        val c = buffer.get
        buffer = None
        Some(c)
      } else if (!eof) {
          val c = chars.charAt(idx)
          idx += 1
          column += 1
          lineBuffer += c
          Some(c)
      } else None

  final class Tokenizer(val scanner: Scanner):
    // A buffer of tokens
    private val buffer: ListBuffer[Token] = ListBuffer.empty
    private var lineNum = 1
    // State flag
    private var isEPCContext: Boolean = false

    /**
     * Peeks through the next k number of tokens
     *
     * This functions looks ahead the next k number of tokens,
     * and returns them as a list.
     * It uses a FIFO buffer to store tokens temporarily.
     * @param k number of tokens
     * @return one token or a list of tokens
     */
    def peek(k: Int = 1): Token | List[Token] = {
      // Fill buffer with at least one token if empty
      if buffer.isEmpty then
        buffer += consume()

      // Fill buffer up to k tokens if needed
      val bufferSize = buffer.size
      if bufferSize < k then
        for _ <- 0 until (k - bufferSize) do
          buffer += consume(keepBuffer = true)

      // Return a single token or a list with k tokens
      if k == 1 then buffer.head else buffer.take(k).toList
    }

    /**
     * Consumes one token and implements peeking through the next one.
     *
     * If we want to only peek and not consume, we set the keep_buffer flag.
     * This argument is passed to additional calls to next.
     * The idea is that we can peek through more than one tokens (by keeping them in the buffer),
     * but we consume tokens only one by one.
     * @param keepBuffer whether to keep the buffer intact, or consume a token from it
     * @return one token
     */
    def consume(keepBuffer: Boolean = false): Token = {

      if (buffer.nonEmpty && !keepBuffer) {
        val c = buffer.head
        buffer.remove(0)
        return c
      }

      var optionChar = scanner.peek()
      val col = scanner.getColumn
      if (optionChar.isDefined) {
        var c = optionChar.get
        // Whitespace handling (including new lines)
        if (c.isWhitespace) {
          if (c == '\n' || c == '\r') {
            // Line feed and carriage return handling
            scanner.consume()
            scanner.clearLineBuffer()
            scanner.resetColumn()
            lineNum += 1
            // Change on 28.11.2025: We start ignoring newlines,
            // and simply look for the next token (as with the other whitespace symbols)!
            // return Token(TokenKind.NEWLINE, null)
            return consume(keepBuffer = true)
          }
          // Consume whitespace
          scanner.consume()
          // Look recursively for the next token
          return consume(keepBuffer = true)
        }
        // Operators and symbols
        if (c == '+') {
          scanner.consume()
          return Token(TokenKind.PLUS, "+", lineNum, col)
        }
        if (c == '-') {
          scanner.consume()
          if (scanner.peek().isDefined && scanner.peek().get == '>') {
            scanner.consume()
            return Token(TokenKind.RIGHT_ARROW, "->", lineNum, col)
          }
          // Minus separator handling
          val sbValue = new StringBuilder("-")
          while (scanner.peek().isDefined && scanner.peek().get == '-') {
            sbValue += scanner.consume().get
          }
          if (sbValue.length >= 4) {
            return Token(TokenKind.MINUS_SEPARATOR, sbValue.toString, lineNum, col)
          }
          if (sbValue.length == 1) {
            return Token(TokenKind.MINUS, "-", lineNum, col)
          }
          // The operators -- and --- are unknown
          return Token(TokenKind.UNKNOWN_OPERATOR, sbValue.toString, lineNum, col)
        }
        if (c == '*') {
          scanner.consume()
          // If we are in an EPC context (after the --EPC keyword)
          // and we encounter the symbols "*)", we know that this is
          // the closing of the comment, in which the spec is defined.
          // In that case we close the EPC context, and we return a
          // token indicating that the EPC context has been closed.
          if (scanner.peek().isDefined && scanner.peek().get == ')') {
            scanner.consume()
            isEPCContext = false
            return Token(TokenKind.EPC_CONTEXT_CLOSE, "", lineNum, col)
          }
          return Token(TokenKind.MUL, "*", lineNum, col)
        }
        if (c == '^') {
          scanner.consume()
          return Token(TokenKind.EXP, "^", lineNum, col)
        }
        if (c == '%') {
          scanner.consume()
          return Token(TokenKind.MOD, "%", lineNum, col)
        }
        if (c == '.') {
          scanner.consume()
          if (scanner.peek().isDefined && scanner.peek().get == '.') {
            scanner.consume()
            return Token(TokenKind.DOT_DOT, "..", lineNum, col)
          }
          return Token(TokenKind.DOT, ".", lineNum, col)
        }
        if (c == ':') {
          scanner.consume()
          if (scanner.peek().isDefined && scanner.peek().get == '>') {
            scanner.consume()
            return Token(TokenKind.COLON_GT, ":>", lineNum, col)
          }
          if (scanner.peek().isDefined && scanner.peek().get == '=') {
            scanner.consume()
            return Token(TokenKind.EPC_ASSIGN, ":=", lineNum, col)
          }
          return Token(TokenKind.COLON, ":", lineNum, col)
        }
        if (c == ';') {
          scanner.consume()
          return Token(TokenKind.SEMICOLON, ";", lineNum, col)
        }
        if (c == ',') {
          scanner.consume()
          return Token(TokenKind.COMMA, ",", lineNum, col)
        }
        if (c == '!') {
          scanner.consume()
          return Token(TokenKind.BANG, "!", lineNum, col)
        }
        if (c == '?') {
          scanner.consume()
          return Token(TokenKind.QUESTION_MARK, "?", lineNum, col)
        }
        if (c == '_') {
          scanner.consume()
          return Token(TokenKind.UNDERSCORE, "_", lineNum, col)
        }
        if (c == '~') {
          scanner.consume()
          if (scanner.peek().isDefined && scanner.peek().get == '>') {
            scanner.consume()
            return Token(TokenKind.LEADS_TO, "~>", lineNum, col)
          }
          return Token(TokenKind.NOT, "~", lineNum, col)
        }
        if (c == '\'') {
          scanner.consume()
          return Token(TokenKind.PRIME, "'", lineNum, col)
        }
        // '(' or '(*' ... '*)' comments
        if (c == '(') {
          scanner.consume()
          if (scanner.peek().isDefined && scanner.peek().get == '*') {
            scanner.consume()
            // Throw away all characters before the next '*)'
            val sbValue = new StringBuilder
            while (scanner.peek().isDefined && !sbValue.endsWith("*)")) {
              // Line feed and carriage return handling
              if (scanner.peek().get == '\n' || scanner.peek().get == '\r') {
                scanner.clearLineBuffer()
                scanner.resetColumn()
                lineNum += 1
              }
              sbValue += scanner.consume().get
              optionChar = scanner.peek()

              // If this comment is opening an EPC spec (with the --EPC keyword)
              // we stop throwing away its contents, and we continue with the
              // recognition of the following tokens.
              // Additionally, we set the isEPCContext variable to true, in order
              // to be able to recognize that the symbols "*)" are closing the
              // EPC context and the current comment.
              if (sbValue.endsWith("--EPC")) {
                isEPCContext = true
                return Token(TokenKind.EPC_CONTEXT_OPEN, "--EPC", lineNum, col)
              }
            }
            return consume(keepBuffer = true)
          }
          if (scanner.peek().isDefined && scanner.peek().get == '+') {
            scanner.consume()
            if (scanner.peek().isDefined && scanner.peek().get == ')') {
              scanner.consume()
              return Token(TokenKind.OPLUS, "(+)", lineNum, col)
            }
            return Token(TokenKind.UNKNOWN_OPERATOR, "(+", lineNum, col)
          }
          if (scanner.peek().isDefined && scanner.peek().get == '-') {
            scanner.consume()
            if (scanner.peek().isDefined && scanner.peek().get == ')') {
              scanner.consume()
              return Token(TokenKind.OMINUS, "(-)", lineNum, col)
            }
            return Token(TokenKind.UNKNOWN_OPERATOR, "(-", lineNum, col)
          }
          return Token(TokenKind.LEFT_ROUND_BRACKET, "(", lineNum, col)
        }
        if (c == ')') {
          scanner.consume()
          return Token(TokenKind.RIGHT_ROUND_BRACKET, ")", lineNum, col)
        }
        if (c == '[') {
          scanner.consume()
          if (scanner.peek().isDefined && scanner.peek().get == ']') {
            scanner.consume()
            return Token(TokenKind.SQUARE, "[]", lineNum, col)
          }
          if (scanner.peek().isDefined && scanner.peek().get == '[') {
            scanner.consume()
            return Token(TokenKind.EPC_LOC_SCOPE_OPEN, "[[", lineNum, col)
          }
          return Token(TokenKind.LEFT_SQUARE_BRACKET, "[", lineNum, col)
        }
        if (c == ']') {
          scanner.consume()
          if (scanner.peek().isDefined && scanner.peek().get == ']') {
            scanner.consume()
            return Token(TokenKind.EPC_LOC_SCOPE_CLOSE, "]]", lineNum, col)
          }
          return Token(TokenKind.RIGHT_SQUARE_BRACKET, "]", lineNum, col)
        }
        if (c == '{') {
          scanner.consume()
          if (scanner.peek().isDefined && scanner.peek().get == '{') {
            scanner.consume()
            return Token(TokenKind.EPC_MULTISET_OPEN, "{{", lineNum, col)
          }
          return Token(TokenKind.LEFT_CURLY_BRACKET, "{", lineNum, col)
        }
        if (c == '}') {
          scanner.consume()
          if (scanner.peek().isDefined && scanner.peek().get == '}') {
            scanner.consume()
            return Token(TokenKind.EPC_MULTISET_CLOSE, "}}", lineNum, col)
          }
          return Token(TokenKind.RIGHT_CURLY_BRACKET, "}", lineNum, col)
        }
        if (c == '=') {
          scanner.consume()
          if (scanner.peek().isDefined && scanner.peek().get == '=') {
            scanner.consume()
            // Equal separator handling
            val sbValue = new StringBuilder("==")
            while (scanner.peek().isDefined && scanner.peek().get == '=') {
              sbValue += scanner.consume().get
            }
            if (sbValue.length >= 4) {
              return Token(TokenKind.EQUAL_SEPARATOR, sbValue.toString, lineNum, col)
            }
            if (sbValue.length == 2) {
              return Token(TokenKind.DEFINE, "==", lineNum, col)
            }
            // === is unknown
            return Token(TokenKind.UNKNOWN_OPERATOR, sbValue.toString, lineNum, col)
          }
          if (scanner.peek().isDefined && scanner.peek().get == '>') {
            scanner.consume()
            return Token(TokenKind.IMPLICATION, "=>", lineNum, col)
          }
          if (scanner.peek().isDefined && scanner.peek().get == '<') {
            scanner.consume()
            return Token(TokenKind.LE, "=<", lineNum, col)
          }
          return Token(TokenKind.EQ, "=", lineNum, col)
        }
        if (c == '#') {
          scanner.consume()
          return Token(TokenKind.NEQ, "#", lineNum, col)
        }
        if (c == '<') {
          scanner.consume()
          if (scanner.peek().isDefined && scanner.peek().get == '-') {
            scanner.consume()
            return Token(TokenKind.LEFT_ARROW, "<-", lineNum, col)
          }
          if (scanner.peek().isDefined && scanner.peek().get == '=') {
            scanner.consume()
            if (scanner.peek().isDefined && scanner.peek().get == '>') {
              scanner.consume()
              return Token(TokenKind.EQUIVALENCE, "<=>", lineNum, col)
            }
            return Token(TokenKind.LE, "<=", lineNum, col)
          }
          if (scanner.peek().isDefined && scanner.peek().get == '<') {
            scanner.consume()
            return Token(TokenKind.LEFT_ANGLE_BRACKET, "<<", lineNum, col)
          }
          if (scanner.peek().isDefined && scanner.peek().get == '>') {
            scanner.consume()
            return Token(TokenKind.DIAMOND, "<>", lineNum, col)
          }
          if (scanner.peek().isDefined && scanner.peek().get == '|') {
            scanner.consume()
            return Token(TokenKind.EPC_LOC_MEMORY_CODE_SEP, "<|", lineNum, col)
          }
          return Token(TokenKind.LT, "<", lineNum, col)
        }
        if (c == '>') {
          scanner.consume()
          if (scanner.peek().isDefined && scanner.peek().get == '>') {
            scanner.consume()
            return Token(TokenKind.RIGHT_ANGLE_BRACKET, ">>", lineNum, col)
          }
          if (scanner.peek().isDefined && scanner.peek().get == '=') {
            scanner.consume()
            return Token(TokenKind.GE, ">=", lineNum, col)
          }
          return Token(TokenKind.GT, ">", lineNum, col)
        }
        if (c == '|') {
          scanner.consume()
          if (scanner.peek().isDefined && scanner.peek().get == '-') {
            scanner.consume()
            if (scanner.peek().isDefined && scanner.peek().get == '>') {
              scanner.consume()
              return Token(TokenKind.MAPS_TO, "|->", lineNum, col)
            }
            return Token(TokenKind.UNKNOWN_OPERATOR, "|-", lineNum, col)
          }
          if (scanner.peek().isDefined && scanner.peek().get == '|') {
            scanner.consume()
            return Token(TokenKind.EPC_LOC_COMPOSITION, "||", lineNum, col)
          }
          return Token(TokenKind.EPC_THREAD_COMPOSITION, "|", lineNum, col)
        }
        if (c == '/') {
          scanner.consume()
          if (scanner.peek().isDefined && scanner.peek().get == '\\') {
            scanner.consume()
            return Token(TokenKind.AND, "/\\", lineNum, col)
          }
          if (scanner.peek().isDefined && scanner.peek().get == '=') {
            scanner.consume()
            return Token(TokenKind.NEQ, "/=", lineNum, col)
          }
          return Token(TokenKind.DIV, "/", lineNum, col)
        }
        // One-line comment \* or \/ or symbol starting with \ (\A, \E, \in, ...)
        if (c == '\\') {
          val sbName = new StringBuilder
          sbName += scanner.consume().get
          optionChar = scanner.peek()
          // One-line comment handling
          if (optionChar.isDefined && optionChar.get == '*') {
            // Throw away the following characters that come before the newline
            scanner.consume()
            optionChar = scanner.peek()
            while (optionChar.isDefined && optionChar.get != '\n' && optionChar.get != '\r') {
              scanner.consume()
              optionChar = scanner.peek()
            }
            return consume(keepBuffer = true)
          }
          // \/ handling
          if (optionChar.isDefined && optionChar.get == '/') {
            scanner.consume()
            return Token(TokenKind.OR, "\\/", lineNum, col)
          }
          // Symbol starting with \
          while (optionChar.isDefined && optionChar.get.isLetter) {
            sbName += scanner.consume().get
            optionChar = scanner.peek()
          }
          val name = sbName.toString
          return name match {
            case "\\land" => Token(TokenKind.AND, name, lineNum, col)
            case "\\lor" => Token(TokenKind.OR, name, lineNum, col)
            case "\\lnot" => Token(TokenKind.NOT, name, lineNum, col)
            case "\\neg" => Token(TokenKind.NOT, name, lineNum, col)
            case "\\equiv" => Token(TokenKind.EQUIVALENCE, name, lineNum, col)
            case "\\A" => Token(TokenKind.FORALL, name, lineNum, col)
            case "\\E" => Token(TokenKind.EXISTS, name, lineNum, col)
            case "\\in" => Token(TokenKind.SET_IN, name, lineNum, col)
            case "\\notin" => Token(TokenKind.NOTIN, name, lineNum, col)
            case "\\cup" => Token(TokenKind.UNION, name, lineNum, col)
            case "\\union" => Token(TokenKind.UNION, name, lineNum, col)
            case "\\cap" => Token(TokenKind.INTERSECT, name, lineNum, col)
            case "\\intersect" => Token(TokenKind.INTERSECT, name, lineNum, col)
            case "\\subset" => Token(TokenKind.SUBSET, name, lineNum, col)
            case "\\subseteq" => Token(TokenKind.SUBSETEQ, name, lineNum, col)
            case "\\" => Token(TokenKind.SET_DIFFERENCE, name, lineNum, col)
            case "\\X" => Token(TokenKind.TIMES, name, lineNum, col)
            case "\\times" => Token(TokenKind.TIMES, name, lineNum, col)
            case "\\o" => Token(TokenKind.CIRCLE, name, lineNum, col)
            case "\\circ" => Token(TokenKind.CIRCLE, name, lineNum, col)
            case "\\oplus" => Token(TokenKind.OPLUS, name, lineNum, col)
            case "\\ominus" => Token(TokenKind.OMINUS, name, lineNum, col)
            case "\\div" => Token(TokenKind.INT_DIV, name, lineNum, col)
            case "\\sqsubseteq" => Token(TokenKind.SQSUBSETEQ, name, lineNum, col)
            case default => Token(TokenKind.BACKSLASH_OPERATOR, name, lineNum, col)
          }
        }
        if (c == '@') {
          scanner.consume()
          if (scanner.peek().isDefined && scanner.peek().get == '@') {
            scanner.consume()
            return Token(TokenKind.AT_AT, "@@", lineNum, col)
          }
          return Token(TokenKind.AT, "@", lineNum, col)
        }
        // Identifier: [a-zA-Z_][a-zA-Z0-9_]*
        if (c.isLetter || c == '_') {
          val sbName = new StringBuilder
          sbName += scanner.consume().get
          optionChar = scanner.peek()
          while (optionChar.isDefined
            && (optionChar.get.isLetterOrDigit || optionChar.get == '_')
            && (sbName.toString != "WF" && sbName.toString != "SF"))
          {
            sbName += scanner.consume().get
            optionChar = scanner.peek()
          }
          val name = sbName.toString
          return name match {
            // Module-Level Constructs
            case "MODULE" => Token(TokenKind.MODULE, name, lineNum, col)
            case "EXTENDS" => Token(TokenKind.EXTENDS, name, lineNum, col)
            case "CONSTANT" => Token(TokenKind.CONSTANT, name, lineNum, col)
            case "CONSTANTS" => Token(TokenKind.CONSTANTS, name, lineNum, col)
            case "VARIABLE" => Token(TokenKind.VARIABLE, name, lineNum, col)
            case "VARIABLES" => Token(TokenKind.VARIABLES, name, lineNum, col)
            case "ASSUME" => Token(TokenKind.ASSUME, name, lineNum, col)
            case "INSTANCE" => Token(TokenKind.INSTANCE, name, lineNum, col)
            case "WITH" => Token(TokenKind.WITH, name, lineNum, col)
            case "THEOREM" => Token(TokenKind.THEOREM, name, lineNum, col)
            case "LOCAL" => Token(TokenKind.LOCAL, name, lineNum, col)
            // Logical operators
            case "TRUE" => Token(TokenKind.TRUE, name, lineNum, col)
            case "FALSE" => Token(TokenKind.FALSE, name, lineNum, col)
            case "BOOLEAN" => Token(TokenKind.BOOLEAN, name, lineNum, col)
            case "CHOOSE" => Token(TokenKind.CHOOSE, name, lineNum, col)
            // Set operators
            case "SUBSET" => Token(TokenKind.BIG_SUBSET, name, lineNum, col)
            case "UNION" => Token(TokenKind.BIG_UNION, name, lineNum, col)
            // Functions
            case "DOMAIN" => Token(TokenKind.DOMAIN, name, lineNum, col)
            case "EXCEPT" => Token(TokenKind.EXCEPT, name, lineNum, col)
            // Miscellaneous constructs
            case "IF" => Token(TokenKind.IF, name, lineNum, col)
            case "THEN" => Token(TokenKind.THEN, name, lineNum, col)
            case "ELSE" => Token(TokenKind.ELSE, name, lineNum, col)
            case "CASE" => Token(TokenKind.CASE, name, lineNum, col)
            case "OTHER" => Token(TokenKind.OTHER, name, lineNum, col)
            case "LET" => Token(TokenKind.LET, name, lineNum, col)
            case "IN" => Token(TokenKind.IN, name, lineNum, col)
            // Actions
            case "ENABLED" => Token(TokenKind.ENABLED, name, lineNum, col)
            case "UNCHANGED" => Token(TokenKind.UNCHANGED, name, lineNum, col)
            // Temporal operators
            case "WF" => Token(TokenKind.WF, name, lineNum, col)
            case "SF" => Token(TokenKind.SF, name, lineNum, col)
            // EPC operators
            case "var" => Token(TokenKind.EPC_VAR, name, lineNum, col)
            case "chan" => Token(TokenKind.EPC_CHAN, name, lineNum, col)
            case "suspchan" => Token(TokenKind.EPC_SUSPCHAN, name, lineNum, col)
            case "if" => Token(TokenKind.EPC_IF, name, lineNum, col)
            case "then" => Token(TokenKind.EPC_THEN, name, lineNum, col)
            case "else" => Token(TokenKind.EPC_ELSE, name, lineNum, col)
            case "fi" => Token(TokenKind.EPC_FI, name, lineNum, col)
            case "skip" => Token(TokenKind.EPC_SKIP, name, lineNum, col)
            case "def" => Token(TokenKind.EPC_DEF, name, lineNum, col)
            // case "Undefined" => Token(TokenKind.EPC_UNDEFINED, name)
            // case "Initialized" => Token(TokenKind.EPC_INITIALIZED, name)
            // Identifier
            case default => Token(TokenKind.IDENTIFIER, name, lineNum, col)
          }
        }
        // Number: [0-9]+
        if (c.isDigit) {
          val sbValue = new StringBuilder
          sbValue += scanner.consume().get
          optionChar = scanner.peek()
          while (optionChar.isDefined && optionChar.get.isDigit) {
            sbValue += scanner.consume().get
            optionChar = scanner.peek()
          }
          val value = sbValue.toString
          return Token(TokenKind.INTEGER, value, lineNum, col)
        }
        // String
        if (c == '"') {
          val sbValue = new StringBuilder
          scanner.consume().get
          optionChar = scanner.peek()
          while (optionChar.isDefined && optionChar.get != '"') {
            sbValue += scanner.consume().get
            optionChar = scanner.peek()
          }
          scanner.consume()
          val value = sbValue.toString
          return Token(TokenKind.STRING, value, lineNum, col)
        }
        Token(TokenKind.UNKNOWN_OPERATOR, c.toString, lineNum, col)
      } else {
        // End of file
        Token(TokenKind.EOF, null, lineNum, col + 1)
      }
    }


}
