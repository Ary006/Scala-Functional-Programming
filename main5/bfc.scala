// Main Part 5 about a "Compiler" for the Brainf*** language
//============================================================


object M5b 
{

  // !!! Copy any function you need from file bf.scala !!!
  //
  // If you need any auxiliary function, feel free to 
  // implement it, but do not make any changes to the
  // templates below.


  // DEBUGGING INFORMATION FOR COMPILERS!!!
  //
  // Compiler, even real ones, are fiendishly difficult to get
  // to produce correct code. One way to debug them is to run
  // example programs ``unoptimised''; and then optimised. Does
  // the optimised version still produce the same result?


  // for timing purposes
  def time_needed[T](n: Int, code: => T) = 
  {
    val start = System.nanoTime()
    for (i <- 0 until n) code
    val end = System.nanoTime()
    (end - start)/(n * 1.0e9)
  }


  type Mem = Map[Int, Int]

  import io.Source
  import scala.util._

  // ADD YOUR CODE BELOW
  //======================

  def load_bff(name: String): String = 
  {
    Try(Source.fromFile(name)("ISO-8859-1").mkString).getOrElse("")
  }

  def sread(mem: Mem, mp: Int): Int = mem.getOrElse(mp, 0)
  def write(mem: Mem, mp: Int, v: Int): Mem = mem + (mp -> v)

  // (6) 
  def jtable(pg: String) : Map[Int, Int] = 
  {
    def jumpRight(prog: String, pc: Int, level: Int): Int = 
    {
      if (pc >= prog.length) pc
      else prog(pc) match {
        case '[' => jumpRight(prog, pc + 1, level + 1)
        case ']' if level == 0 => pc + 1
        case ']' => jumpRight(prog, pc + 1, level - 1)
        case _ => jumpRight(prog, pc + 1, level)
      }
    }

    def jumpLeft(prog: String, pc: Int, level: Int): Int = 
    {
      if (pc < 0) pc
      else prog(pc) match {
        case ']' => jumpLeft(prog, pc - 1, level + 1)
        case '[' if level == 0 => pc + 1
        case '[' => jumpLeft(prog, pc - 1, level - 1)
        case _ => jumpLeft(prog, pc - 1, level)
      }
    }

    def buildTable(pos: Int, acc: Map[Int, Int]): Map[Int, Int] = 
    {
      if (pos >= pg.length) acc
      else pg(pos) match {
        case '[' =>
          val rightPos = jumpRight(pg, pos + 1, 0)
          buildTable(pos + 1, acc + (pos -> rightPos))
        case ']' =>
          val leftPos = jumpLeft(pg, pos - 1, 0)
          buildTable(pos + 1, acc + (pos -> leftPos))
        case _ => buildTable(pos + 1, acc)
      }
    }

    buildTable(0, Map())
  }

  // testcase
  //
  // jtable("""+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]""")
  // =>  Map(69 -> 61, 5 -> 20, 60 -> 70, 27 -> 44, 43 -> 28, 19 -> 6)


  def compute2(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = 
  {
    if (pc < 0 || pc >= pg.length) mem
    else pg(pc) match {
      case '>' => compute2(pg, tb, pc + 1, mp + 1, mem)
      case '<' => compute2(pg, tb, pc + 1, mp - 1, mem)
      case '+' => compute2(pg, tb, pc + 1, mp, write(mem, mp, sread(mem, mp) + 1))
      case '-' => compute2(pg, tb, pc + 1, mp, write(mem, mp, sread(mem, mp) - 1))
      case '0' => compute2(pg, tb, pc + 1, mp, write(mem, mp, 0))
      case '.' => {
        print(sread(mem, mp).toChar)
        compute2(pg, tb, pc + 1, mp, mem)
      }
      case '[' if sread(mem, mp) == 0 => compute2(pg, tb, tb(pc), mp, mem)
      case '[' => compute2(pg, tb, pc + 1, mp, mem)
      case ']' if sread(mem, mp) != 0 => compute2(pg, tb, tb(pc), mp, mem)
      case ']' => compute2(pg, tb, pc + 1, mp, mem)
      case _ => compute2(pg, tb, pc + 1, mp, mem)
    }
  }
  def run2(pg: String, m: Mem = Map()) : Mem = compute2(pg, jtable(pg), 0, 0, m)

  // testcases
  // time_needed(1, run2(load_bff("benchmark.bf")))
  // time_needed(1, run2(load_bff("sierpinski.bf")))



  // (7) 

  def optimise(s: String) : String = 
  {
    s.replaceAll("""[^<>+\-\.\[\]]""", "")
      .replaceAll("""\[-\]""", "0")
  }

  def compute3(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = compute2(pg, tb, pc, mp, mem)


  def run3(pg: String, m: Mem = Map()) : Mem = compute3(optimise(pg), jtable(optimise(pg)), 0, 0, m)



  // testcases
  //
  // optimise(load_bff("benchmark.bf"))          // should have inserted 0's
  // optimise(load_bff("mandelbrot.bf")).length  // => 11205
  // 
  // time_needed(1, run3(load_bff("benchmark.bf")))



  // (8)  
  def combine(s: String) : String = 
  {
    def processGroup(c: Char, count: Int): String = 
    {
      if (count == 0) ""
      else if (count > 26) {
        c.toString + 'Z' + processGroup(c, count - 26)
      } else {
        c.toString + (count + 64).toChar
      }
    }

    def helper(chars: List[Char], currentChar: Char, count: Int, acc: String): String = chars match 
    {
      case Nil => acc + (if (count > 0) processGroup(currentChar, count) else "")
      case h :: t if h == currentChar => helper(t, currentChar, count + 1, acc)
      case h :: t if h == '+' || h == '-' || h == '<' || h == '>' =>
        helper(t, h, 1, acc + (if (count > 0) processGroup(currentChar, count) else ""))
      case h :: t => 
        helper(t, currentChar, 0, acc + (if (count > 0) processGroup(currentChar, count) else "") + h)
    }

    s.toList match 
    {
      case Nil => ""
      case h :: t if h == '+' || h == '-' || h == '<' || h == '>' => helper(t, h, 1, "")
      case h :: t => helper(t, ' ', 0, h.toString)
    }
  }

  // testcase
  // combine(load_bff("benchmark.bf"))

  def compute4(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = compute2(pg, tb, pc, mp, mem)


  // should call first optimise and then combine on the input string
  //
  def run4(pg: String, m: Mem = Map()) : Mem = compute4(combine(optimise(pg)), jtable(combine(optimise(pg))), 0, 0, m)



  // testcases
  // combine(optimise(load_bff("benchmark.bf"))) // => """>A+B[<A+M>A-A]<A[[....."""

  // testcases (they should now run much faster)
  // time_needed(1, run4(load_bff("benchmark.bf")))
  // time_needed(1, run4(load_bff("sierpinski.bf"))) 
  // time_needed(1, run4(load_bff("mandelbrot.bf")))


}