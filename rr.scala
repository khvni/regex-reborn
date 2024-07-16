// 4 different regex components to recognize: +, *, |, and char literals
abstract class RegexExpr

// We need to be able to represent when one expression follows another.
// ., a, b
case class Literal(c: Char) extends RegexExpr
// a|b
case class Or(expr1: RegexExpr, expr2: RegexExpr) extends RegexExpr
// ab -> Concat(a,b); abc -> Concat(a, Concat(b, c))
case class Concat(first: RegexExpr, second: RegexExpr) extends RegexExpr
// a* 
case class Repeat(expr: RegexExpr) extends RegexExpr
// a+
case class Plus(expr: RegexExpr) extends RegexExpr

// define a parser that will build a RegexExpr
// RHS says "find smth that matches \w or a period. 
// if found, turn it into a Literal"
object RegexParser extends RegexParsers {
    def charLit: Parser[RegexExpr] = ("""\w""".r | ".") ^^ {
        char => Literal(char.head)
    }
    // parentheses must be defined at lowest level of parser since they're the strongest binding.
    // allows anything to be put in parentheses
    def parenExpr: Parser[RegexExpr] = "(" ~> highExpr <~ ")"
    def lit: Parser[RegexExpr] = charLit | parenExpr

    // defining both * and +
    def repeat: Parser[RegexExpr] = lit <~ "*"
        ^^ { case 1 => Repeat(1)}
    def plus: Parser[RegexExpr] = lit <~ "+" 
        ^^ {case p => Plus(p)}
    def lowExpr: Parser[RegexExpr] = repeat | plus | lit

    // defining concatenation
    def concat: Parser[RegexExpr] = rep(lowExpr)
        ^^ { case list => listToConcat(list)}
    def midExpr: Parser[RegexExpr] = concat | lowExpr

    // define "or"
    def or: Parser[RegexExpr] = midExpr ~ "|" ~ midExpr 
        ^^ {case 1 ~ "|" ~ r => Or(1, r)}
    
    // highExpr - the weakest binding operator.
    // highExpr is an "or", else if no "or" it's a midExpr
    def highExpr: Parser[RegexExpr] = or | midExpr

    // helper functions
    def listToConcat(list: List[RegexExpr]): RegexExpr = list match {
        case head :: Nil => head
        case head :: rest => Concat(head, listToConcat(rest))
    }

    def apply(input: String): Option[RegexExpr] = {
        parseAll(highExpr, input) match {
            case Success(result, _) => Some(result)
            case failure : NoSuccess => None
        }
    }
}

