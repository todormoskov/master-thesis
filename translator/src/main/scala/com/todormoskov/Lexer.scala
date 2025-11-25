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
    SQSUBSETEQ,
    COLON_LT,

    EPC_VAR,
    EPC_ASSIGN,
    EPC_LOC_SCOPE_OPEN,
    EPC_LOC_SCOPE_CLOSE,
    EPC_LOC_MEMORY_CODE_SEP,
    EPC_LOC_COMPOSITION,
    EPC_THREAD_COMPOSITION,
    EPC_MULTISET_OPEN,
    EPC_MULTISET_CLOSE,
    EPC_NEW,
    EPC_IF,
    EPC_THEN,
    EPC_ELSE,
    EPC_DEF,
    EPC_UNDEFINED,
    EPC_INITIALIZED

  case class Token(kind: TokenKind, value: String)

  final class Scanner(input: String):
    // The whole input as a single string
    private val chars = input
    private val len = chars.length
    private var idx = 0
    // A buffer of one character
    private var buffer: Option[Char] = None

    def eof: Boolean = idx >= len

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
          Some(c)
      } else None

  final class Tokenizer(val scanner: Scanner):
    // A buffer of tokens
    private val buffer: ListBuffer[Token] = ListBuffer.empty

    // State flags
    private var isNewLine: Boolean = true
    private var isLogicalLine: Boolean = false

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
      if (optionChar.isDefined) {
        var c = optionChar.get
        // Whitespace handling (including new lines)
        if (c.isWhitespace) {
          if (c == '\n' || c == '\r') {
            // Line feed and carriage return handling
            isNewLine = true
            scanner.consume()
            return Token(TokenKind.NEWLINE, null)
          }
          // Consume whitespace
          scanner.consume()
          // Look recursively for the next token
          return consume(keepBuffer = true)
        }
        // Operators and symbols
        if (c == '+') {
          scanner.consume()
          return Token(TokenKind.PLUS, "+")
        }
        if (c == '-') {
          scanner.consume()
          if (scanner.peek().isDefined && scanner.peek().get == '>') {
            scanner.consume()
            return Token(TokenKind.RIGHT_ARROW, "->")
          }
          // Minus separator handling
          val sbValue = new StringBuilder("-")
          while (scanner.peek().isDefined && scanner.peek().get == '-') {
            sbValue += scanner.consume().get
          }
          if (sbValue.length >= 4) {
            return Token(TokenKind.MINUS_SEPARATOR, sbValue.toString)
          }
          if (sbValue.length == 1) {
            return Token(TokenKind.MINUS, "-")
          }
          // The operators -- and --- are unknown
          return Token(TokenKind.UNKNOWN_OPERATOR, sbValue.toString)
        }
        if (c == '*') {
          scanner.consume()
          return Token(TokenKind.MUL, "*")
        }
        if (c == '^') {
          scanner.consume()
          return Token(TokenKind.EXP, "^")
        }
        if (c == '%') {
          scanner.consume()
          return Token(TokenKind.MOD, "%")
        }
        if (c == '.') {
          scanner.consume()
          if (scanner.peek().isDefined && scanner.peek().get == '.') {
            scanner.consume()
            return Token(TokenKind.DOT_DOT, "..")
          }
          return Token(TokenKind.DOT, ".")
        }
        if (c == ':') {
          scanner.consume()
          if (scanner.peek().isDefined && scanner.peek().get == '>') {
            scanner.consume()
            return Token(TokenKind.COLON_LT, ":>")
          }
          if (scanner.peek().isDefined && scanner.peek().get == '=') {
            scanner.consume()
            return Token(TokenKind.EPC_ASSIGN, ":=")
          }
          return Token(TokenKind.COLON, ":")
        }
        if (c == ',') {
          scanner.consume()
          return Token(TokenKind.COMMA, ",")
        }
        if (c == '!') {
          scanner.consume()
          return Token(TokenKind.BANG, "!")
        }
        if (c == '_') {
          scanner.consume()
          return Token(TokenKind.UNDERSCORE, "_")
        }
        if (c == '~') {
          scanner.consume()
          if (scanner.peek().isDefined && scanner.peek().get == '>') {
            scanner.consume()
            return Token(TokenKind.LEADS_TO, "~>")
          }
          return Token(TokenKind.NOT, "~")
        }
        if (c == '\'') {
          scanner.consume()
          return Token(TokenKind.PRIME, "'")
        }
        // '(' or '(*' ... '*)' comments
        if (c == '(') {
          scanner.consume()
          if (scanner.peek().isDefined && scanner.peek().get == '*') {
            scanner.consume()
            // Throw away all characters before the next '*)'
            val sbValue = new StringBuilder
            while (scanner.peek().isDefined && !sbValue.endsWith("*)")) {
              sbValue += scanner.consume().get
              optionChar = scanner.peek()
            }
            return consume(keepBuffer = true)
          }
          if (scanner.peek().isDefined && scanner.peek().get == '+') {
            scanner.consume()
            if (scanner.peek().isDefined && scanner.peek().get == ')') {
              scanner.consume()
              return Token(TokenKind.OPLUS, "(+)")
            }
            return Token(TokenKind.UNKNOWN_OPERATOR, "(+")
          }
          if (scanner.peek().isDefined && scanner.peek().get == '-') {
            scanner.consume()
            if (scanner.peek().isDefined && scanner.peek().get == ')') {
              scanner.consume()
              return Token(TokenKind.OMINUS, "(-)")
            }
            return Token(TokenKind.UNKNOWN_OPERATOR, "(-")
          }
          return Token(TokenKind.LEFT_ROUND_BRACKET, "(")
        }
        if (c == ')') {
          scanner.consume()
          return Token(TokenKind.RIGHT_ROUND_BRACKET, ")")
        }
        if (c == '[') {
          scanner.consume()
          if (scanner.peek().isDefined && scanner.peek().get == ']') {
            scanner.consume()
            return Token(TokenKind.SQUARE, "[]")
          }
          if (scanner.peek().isDefined && scanner.peek().get == '[') {
            scanner.consume()
            return Token(TokenKind.EPC_LOC_SCOPE_OPEN, "[[")
          }
          return Token(TokenKind.LEFT_SQUARE_BRACKET, "[")
        }
        if (c == ']') {
          scanner.consume()
          if (scanner.peek().isDefined && scanner.peek().get == ']') {
            scanner.consume()
            return Token(TokenKind.EPC_LOC_SCOPE_CLOSE, "]]")
          }
          return Token(TokenKind.RIGHT_SQUARE_BRACKET, "]")
        }
        if (c == '{') {
          scanner.consume()
          if (scanner.peek().isDefined && scanner.peek().get == '{') {
            scanner.consume()
            return Token(TokenKind.EPC_MULTISET_OPEN, "{{")
          }
          return Token(TokenKind.LEFT_CURLY_BRACKET, "{")
        }
        if (c == '}') {
          scanner.consume()
          if (scanner.peek().isDefined && scanner.peek().get == '}') {
            scanner.consume()
            return Token(TokenKind.EPC_MULTISET_CLOSE, "}}")
          }
          return Token(TokenKind.RIGHT_CURLY_BRACKET, "}")
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
              return Token(TokenKind.EQUAL_SEPARATOR, sbValue.toString)
            }
            if (sbValue.length == 2) {
              return Token(TokenKind.DEFINE, "==")
            }
            // === is unknown
            return Token(TokenKind.UNKNOWN_OPERATOR, sbValue.toString)
          }
          if (scanner.peek().isDefined && scanner.peek().get == '>') {
            scanner.consume()
            return Token(TokenKind.IMPLICATION, "=>")
          }
          if (scanner.peek().isDefined && scanner.peek().get == '<') {
            scanner.consume()
            return Token(TokenKind.LE, "=<")
          }
          return Token(TokenKind.EQ, "=")
        }
        if (c == '#') {
          scanner.consume()
          return Token(TokenKind.NEQ, "#")
        }
        if (c == '<') {
          scanner.consume()
          if (scanner.peek().isDefined && scanner.peek().get == '-') {
            scanner.consume()
            return Token(TokenKind.LEFT_ARROW, "<-")
          }
          if (scanner.peek().isDefined && scanner.peek().get == '=') {
            scanner.consume()
            if (scanner.peek().isDefined && scanner.peek().get == '>') {
              scanner.consume()
              return Token(TokenKind.EQUIVALENCE, "<=>")
            }
            return Token(TokenKind.LE, "<=")
          }
          if (scanner.peek().isDefined && scanner.peek().get == '<') {
            scanner.consume()
            return Token(TokenKind.LEFT_ANGLE_BRACKET, "<<")
          }
          if (scanner.peek().isDefined && scanner.peek().get == '>') {
            scanner.consume()
            return Token(TokenKind.DIAMOND, "<>")
          }
          if (scanner.peek().isDefined && scanner.peek().get == '|') {
            scanner.consume()
            return Token(TokenKind.EPC_LOC_MEMORY_CODE_SEP, "<|")
          }
          return Token(TokenKind.LT, "<")
        }
        if (c == '>') {
          scanner.consume()
          if (scanner.peek().isDefined && scanner.peek().get == '>') {
            scanner.consume()
            return Token(TokenKind.RIGHT_ANGLE_BRACKET, ">>")
          }
          if (scanner.peek().isDefined && scanner.peek().get == '=') {
            scanner.consume()
            return Token(TokenKind.GE, ">=")
          }
          return Token(TokenKind.GT, ">")
        }
        if (c == '|') {
          scanner.consume()
          if (scanner.peek().isDefined && scanner.peek().get == '-') {
            scanner.consume()
            if (scanner.peek().isDefined && scanner.peek().get == '>') {
              scanner.consume()
              return Token(TokenKind.MAPS_TO, "|->")
            }
            return Token(TokenKind.UNKNOWN_OPERATOR, "|-")
          }
          if (scanner.peek().isDefined && scanner.peek().get == '|') {
            scanner.consume()
            return Token(TokenKind.EPC_LOC_COMPOSITION, "||")
          }
          return Token(TokenKind.EPC_THREAD_COMPOSITION, "|")
        }
        if (c == '/') {
          scanner.consume()
          if (scanner.peek().isDefined && scanner.peek().get == '\\') {
            scanner.consume()
            return Token(TokenKind.AND, "/\\")
          }
          if (scanner.peek().isDefined && scanner.peek().get == '=') {
            scanner.consume()
            return Token(TokenKind.NEQ, "/=")
          }
          return Token(TokenKind.DIV, "/")
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
            return Token(TokenKind.OR, "\\/")
          }
          // Symbol starting with \
          while (optionChar.isDefined && optionChar.get.isLetter) {
            sbName += scanner.consume().get
            optionChar = scanner.peek()
          }
          val name = sbName.toString
          return name match {
            case "\\land" => Token(TokenKind.AND, name)
            case "\\lor" => Token(TokenKind.OR, name)
            case "\\lnot" => Token(TokenKind.NOT, name)
            case "\\neg" => Token(TokenKind.NOT, name)
            case "\\equiv" => Token(TokenKind.EQUIVALENCE, name)
            case "\\A" => Token(TokenKind.FORALL, name)
            case "\\E" => Token(TokenKind.EXISTS, name)
            case "\\in" => Token(TokenKind.SET_IN, name)
            case "\\notin" => Token(TokenKind.NOTIN, name)
            case "\\cup" => Token(TokenKind.UNION, name)
            case "\\union" => Token(TokenKind.UNION, name)
            case "\\cap" => Token(TokenKind.INTERSECT, name)
            case "\\intersect" => Token(TokenKind.INTERSECT, name)
            case "\\subset" => Token(TokenKind.SUBSET, name)
            case "\\" => Token(TokenKind.SET_DIFFERENCE, name)
            case "\\X" => Token(TokenKind.TIMES, name)
            case "\\times" => Token(TokenKind.TIMES, name)
            case "\\o" => Token(TokenKind.CIRCLE, name)
            case "\\circ" => Token(TokenKind.CIRCLE, name)
            case "\\oplus" => Token(TokenKind.OPLUS, name)
            case "\\ominus" => Token(TokenKind.OMINUS, name)
            case "\\sqsubseteq" => Token(TokenKind.SQSUBSETEQ, name)
            case default => Token(TokenKind.BACKSLASH_OPERATOR, name)
          }
        }
        if (c == '@') {
          scanner.consume()
          if (scanner.peek().isDefined && scanner.peek().get == '@') {
            scanner.consume()
            return Token(TokenKind.AT_AT, "@@")
          }
          return Token(TokenKind.AT, "@")
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
            case "MODULE" => Token(TokenKind.MODULE, name)
            case "EXTENDS" => Token(TokenKind.EXTENDS, name)
            case "CONSTANT" => Token(TokenKind.CONSTANT, name)
            case "CONSTANTS" => Token(TokenKind.CONSTANTS, name)
            case "VARIABLE" => Token(TokenKind.VARIABLE, name)
            case "VARIABLES" => Token(TokenKind.VARIABLES, name)
            case "ASSUME" => Token(TokenKind.ASSUME, name)
            case "INSTANCE" => Token(TokenKind.INSTANCE, name)
            case "WITH" => Token(TokenKind.WITH, name)
            case "THEOREM" => Token(TokenKind.THEOREM, name)
            case "LOCAL" => Token(TokenKind.LOCAL, name)
            // Logical operators
            case "TRUE" => Token(TokenKind.TRUE, name)
            case "FALSE" => Token(TokenKind.FALSE, name)
            case "BOOLEAN" => Token(TokenKind.BOOLEAN, name)
            case "CHOOSE" => Token(TokenKind.CHOOSE, name)
            // Set operators
            case "SUBSET" => Token(TokenKind.BIG_SUBSET, name)
            case "UNION" => Token(TokenKind.BIG_UNION, name)
            // Functions
            case "DOMAIN" => Token(TokenKind.DOMAIN, name)
            case "EXCEPT" => Token(TokenKind.EXCEPT, name)
            // Miscellaneous constructs
            case "IF" => Token(TokenKind.IF, name)
            case "THEN" => Token(TokenKind.THEN, name)
            case "ELSE" => Token(TokenKind.ELSE, name)
            case "CASE" => Token(TokenKind.CASE, name)
            case "OTHER" => Token(TokenKind.OTHER, name)
            case "LET" => Token(TokenKind.LET, name)
            case "IN" => Token(TokenKind.IN, name)
            // Actions
            case "ENABLED" => Token(TokenKind.ENABLED, name)
            case "UNCHANGED" => Token(TokenKind.UNCHANGED, name)
            // Temporal operators
            case "WF" => Token(TokenKind.WF, name)
            case "SF" => Token(TokenKind.SF, name)
            // EPC operators
            case "var" => Token(TokenKind.EPC_VAR, name)
            case "new" => Token(TokenKind.EPC_NEW, name)
            case "if" => Token(TokenKind.EPC_IF, name)
            case "then" => Token(TokenKind.EPC_THEN, name)
            case "else" => Token(TokenKind.EPC_ELSE, name)
            case "def" => Token(TokenKind.EPC_DEF, name)
            case "Undefined" => Token(TokenKind.EPC_UNDEFINED, name)
            case "Initialized" => Token(TokenKind.EPC_INITIALIZED, name)
            // Identifier
            case default => Token(TokenKind.IDENTIFIER, name)
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
          return Token(TokenKind.INTEGER, value)
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
          return Token(TokenKind.STRING, value)
        }
        Token(TokenKind.UNKNOWN_OPERATOR, c.toString)
      } else {
        // End of file
        Token(TokenKind.EOF, null)
      }
    }
}
