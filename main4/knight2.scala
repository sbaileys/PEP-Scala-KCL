// Core Part about finding a single tour for a board using the
// Warnsdorf Rule
//==============================================================

object CW9b {


// !!! Copy any function you need from file knight1.scala !!!
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

def is_legal(dim: Int, path: Path, x: Pos) : Boolean = 
!path.contains(x) && x._1 >= 0 && x._2 >= 0 && x._1 < dim && x._2 < dim

def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
val allMoves = List((x._1 + 1, x._2 + 2), (x._1 + 2, x._2 + 1), (x._1 + 2, x._2 - 1), (x._1 + 1, x._2 - 2), (x._1 - 1, x._2 - 2), (x._1 - 2, x._2 - 1), (x._1 - 2, x._2 + 1), (x._1 - 1, x._2 + 2))
allMoves.filter(x => is_legal(dim, path, x))
}

def count_tours(dim: Int, path: Path) : Int = {
  if(path.length == dim * dim) 1
  else {
    val allLegalMoves = legal_moves(dim, path, path.head)
    (for(i <- allLegalMoves) yield count_tours(dim, i::path)).sum
  }
}

def enum_tours(dim: Int, path: Path) : List[Path] = {
  if(path.length == dim * dim) List(path)
  else {
    val allLegalMoves = legal_moves(dim, path, path.head)
    (for(i <- allLegalMoves) yield enum_tours(dim, i::path)).flatten
  }
}

//(4) Implement a first-function that finds the first 
//    element, say x, in the list xs where f is not None. 
//    In that case Return f(x), otherwise None. If possible,
//    calculate f(x) only once.

def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = xs match {
    case Nil => None
    case x::xs if f(x).isDefined => f(x)
    case x::xs if !f(x).isDefined => first(xs, f)
}

// testcases
//
// def foo(x: (Int, Int)) = if (x._1 > 3) Some(List(x)) else None

// first(List((1, 0),(2, 0),(3, 0),(4, 0)), foo)   // Some(List((4,0)))
// first(List((1, 0),(2, 0),(3, 0)), foo)          // None


//(5) Implement a function that uses the first-function from (5) for
//    trying out onward moves, and searches recursively for a
//    knight tour on a dim * dim-board.

def first_tour(dim: Int, path: Path) : Option[Path] = {
    if (path.size == dim * dim) Some(path)
    else first(legal_moves(dim, path, path.head), x => first_tour(dim, x::path))
}


//(6) Complete the function that calculates a list of onward
//    moves like in (2) but orders them according to Warnsdorf’s 
//    rule. That means moves with the fewest legal onward moves 
//    should come first.


def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = ???


//(7) Complete the function that searches for a single *closed* 
//    tour using the ordered_moves function from (6). This
//    function will be tested on a 6 x 6 board. 


def first_closed_tour_heuristics(dim: Int, path: Path) : Option[Path] = ???


//(8) Same as (7) but searches for *non-closed* tours. This 
//    version of the function will be called with dimensions of 
//    up to 30 * 30.

def first_tour_heuristics(dim: Int, path: Path) : Option[Path] = ???



//Helper functions


// // for measuring time
// def time_needed[T](code: => T) : T = {
//   val start = System.nanoTime()
//   val result = code
//   val end = System.nanoTime()
//   println(f"Time needed: ${(end - start) / 1.0e9}%3.3f secs.")
//   result
// }

// // can be called for example with
// //     time_needed(count_tours(dim, List((0, 0))))
// // in order to print out the time that is needed for 
// // running count_tours


// // for printing a board
// def print_board(dim: Int, path: Path): Unit = {
//   println()
//   for (i <- 0 until dim) {
//     for (j <- 0 until dim) {
//       print(f"${path.reverse.indexOf((j, dim - i - 1))}%3.0f ")
//     }
//     println()
//   } 
// }

}
