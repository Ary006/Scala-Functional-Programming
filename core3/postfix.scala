// Shunting Yard Algorithm
// by Edsger Dijkstra
// ========================

object C3a 
{

  // type of tokens
  type Toks = List[String]

  // the operations in the basic version of the algorithm
  val ops = List("+", "-", "*", "/")

  // the precedences of the operators
  val precs = Map("+" -> 1,
                  "-" -> 1,
                  "*" -> 2,
                  "/" -> 2)

  // helper function for splitting strings into tokens
  def split(s: String) : Toks = s.split(" ").toList

  // ADD YOUR CODE BELOW
  //======================


  // (1)
  def is_op(op: String) : Boolean = ops.contains(op)

  def prec(op1: String, op2: String) : Boolean = precs(op1) >= precs(op2)

  def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = toks match 
  {
    case Nil => out ++ st
    case head :: tail => head match {
      case "(" => syard(tail, head :: st, out)
      case ")" => 
        val (ops, rest) = st.span(_ != "(")
        syard(tail, rest.tail, out ++ ops)
      case op if is_op(op) =>
        val (higher, rest) = st.span(x => is_op(x) && prec(x, op))
        syard(tail, op :: rest, out ++ higher)
      case num => syard(tail, st, out :+ num)
    }
  }

  // test cases
  // syard(split("3 + 4 * ( 2 - 1 )"))  // 3 4 2 1 - * +
  // syard(split("10 + 12 * 33"))       // 10 12 33 * +
  // syard(split("( 5 + 7 ) * 2"))      // 5 7 + 2 *
  // syard(split("5 + 7 / 2"))          // 5 7 2 / +
  // syard(split("5 * 7 / 2"))          // 5 7 * 2 /
  // syard(split("9 + 24 / ( 7 - 3 )")) // 9 24 7 3 - / +
  // syard(split("3 + 4 + 5"))           // 3 4 + 5 +
  // syard(split("( ( 3 + 4 ) + 5 )"))    // 3 4 + 5 +
  // syard(split("( 3 + ( 4 + 5 ) )"))    // 3 4 5 + +
  // syard(split("( ( ( 3 ) ) + ( ( 4 + ( 5 ) ) ) )")) // 3 4 5 + +

  // (2)
  def compute(toks: Toks, st: List[Int] = Nil) : Int = toks match 
  {
    case Nil => st.head
    case head :: tail => head match {
      case "+" => compute(tail, (st(1) + st(0)) :: st.drop(2))
      case "-" => compute(tail, (st(1) - st(0)) :: st.drop(2))
      case "*" => compute(tail, (st(1) * st(0)) :: st.drop(2))
      case "/" => compute(tail, (st(1) / st(0)) :: st.drop(2))
      case num => compute(tail, num.toInt :: st)
    }
  }

  // test cases
  // compute(syard(split("3 + 4 * ( 2 - 1 )")))  // 7
  // compute(syard(split("10 + 12 * 33")))       // 406
  // compute(syard(split("( 5 + 7 ) * 2")))      // 24
  // compute(syard(split("5 + 7 / 2")))          // 8
  // compute(syard(split("5 * 7 / 2")))          // 17
  // compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15
  
}