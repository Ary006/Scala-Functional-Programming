// Main Part 2 about Evil Wordle
//===============================

object M2 
{ 

  import io.Source
  import scala.util._

  // ADD YOUR CODE BELOW
  //======================


  //(1)
  def get_wordle_list(url: String) : List[String] = 
  {
      Try {
        Source.fromURL(url).getLines().filter(_.length == 5).toList
      }.getOrElse(List.empty[String])
  }

  // val secrets = get_wordle_list("https://nms.kcl.ac.uk/christian.urban/wordle.txt")
  // secrets.length // => 12972
  // secrets.filter(_.length != 5) // => Nil

  //(2)
  def removeN[A](xs: List[A], elem: A, n: Int) : List[A] = 
  {
    if (n <= 0) xs
    else xs match 
    {
      case Nil => Nil
      case h :: t if h == elem => removeN(t, elem, n - 1)
      case h :: t => h :: removeN(t, elem, n)
    }
  }


  // removeN(List(1,2,3,2,1), 3, 1)  // => List(1, 2, 2, 1)
  // removeN(List(1,2,3,2,1), 2, 1)  // => List(1, 3, 2, 1)
  // removeN(List(1,2,3,2,1), 1, 1)  // => List(2, 3, 2, 1)
  // removeN(List(1,2,3,2,1), 0, 2)  // => List(1, 2, 3, 2, 1)

  //(3)
  abstract class Tip
  case object Absent extends Tip
  case object Present extends Tip
  case object Correct extends Tip


  def pool(secret: String, word: String) : List[Char] = 
  {
    def removeCorrectMatches(s: String, w: String): String = 
    {
      (s.toList zip w.toList).filter { case (s, w) => s != w }.map(_._1).mkString
    }
    
    // First remove all correct matches
    val remainingSecret = removeCorrectMatches(secret, word)
    val remainingWord = removeCorrectMatches(word, secret)
    
    // Convert to list of chars that are in secret but not matched correctly
    remainingSecret.toList
  }

  def aux(secret: List[Char], word: List[Char], pool: List[Char]) : List[Tip] = 
  {
    (secret, word) match {
      case (Nil, _) | (_, Nil) => Nil
      case (s :: st, w :: wt) =>
        if (s == w) {
          Correct :: aux(st, wt, pool)
        } else if (pool.contains(w)) {
          Present :: aux(st, wt, removeN(pool, w, 1))
        } else {
          Absent :: aux(st, wt, pool)
        }
    }
  }

  def score(secret: String, word: String) : List[Tip] = 
  {
    val poolChars = pool(secret, word)
    aux(secret.toList, word.toList, poolChars)
  }


  // score("chess", "caves") // => List(Correct, Absent, Absent, Present, Correct)
  // score("doses", "slide") // => List(Present, Absent, Absent, Present, Present)
  // score("chess", "swiss") // => List(Absent, Absent, Absent, Correct, Correct)
  // score("chess", "eexss") // => List(Present, Absent, Absent, Correct, Correct)

  //(4)
  def eval(t: Tip) : Int = t match 
  {
    case Correct => 10
    case Present => 1
    case Absent => 0
  }

  def iscore(secret: String, word: String) : Int = 
  {
    score(secret, word).map(eval).sum
  }

  //iscore("chess", "caves") // => 21
  //iscore("chess", "swiss") // => 20

  //(5)
  def lowest(secrets: List[String], word: String, current: Int, acc: List[String]) : List[String] = 
  {
    secrets match 
    {
      case Nil => acc
      case head :: tail =>
        val score = iscore(head, word)
        if (score < current) lowest(tail, word, score, List(head))
        else if (score == current) lowest(tail, word, current, head :: acc)
        else lowest(tail, word, current, acc)
    }
  }

  def evil(secrets: List[String], word: String) : List[String] = 
  {
    lowest(secrets, word, Int.MaxValue, Nil)
  }


  //evil(secrets, "stent").length
  //evil(secrets, "hexes").length
  //evil(secrets, "horse").length
  //evil(secrets, "hoise").length
  //evil(secrets, "house").length

  //(6)
  def frequencies(secrets: List[String]) : Map[Char, Double] = 
  {
    val allChars = secrets.flatMap(_.toLowerCase)
    val total = allChars.length.toDouble
    ('a' to 'z').map { c =>
      val count = allChars.count(_ == c)
      c -> (1.0 - (count / total))
    }.toMap
  }

  //(7)
  def rank(frqs: Map[Char, Double], s: String) : Double = 
  {
      s.toLowerCase.map(frqs.getOrElse(_, 0.0)).sum
  }

  def ranked_evil(secrets: List[String], word: String) : List[String] = 
  {
    val evilWords = evil(secrets, word)
    val freqMap = frequencies(secrets)
    val rankedWords = evilWords.map(w => (w, rank(freqMap, w)))
    val maxRank = rankedWords.map(_._2).max
    rankedWords.collect { case (w, r) if r >= maxRank => w }
  }

}