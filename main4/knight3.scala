// Finding a single tour on a "mega" board
//=========================================

object CW9c {

import scala.annotation.tailrec
// !!! Copy any function you need from file knight1.scala !!!
// !!! or knight2.scala                                   !!! 
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.


type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

//(1) Complete the function that tests whether the position x
//    is inside the board and not yet element in the path.

def is_legal(dim: Int, path: Path, x: Pos) : Boolean = 
    !path.contains(x) && x._1 >= 0 && x._2 >= 0 && x._1 < dim && x._2 < dim

//(2) Complete the function that calculates for a position x
//    all legal onward moves that are not already in the path. 
//    The moves should be ordered in a "clockwise" manner.
 
def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
    val allMoves = List((x._1 + 1, x._2 + 2), (x._1 + 2, x._2 + 1), (x._1 + 2, x._2 - 1), (x._1 + 1, x._2 - 2), (x._1 - 1, x._2 - 2), (x._1 - 2, x._2 - 1), (x._1 - 2, x._2 + 1), (x._1 - 1, x._2 + 2))
    allMoves.filter(x => is_legal(dim, path, x))
}

 //comment
//(6) Complete the function that calculates a list of onward
//    moves like in (2) but orders them according to Warnsdorf’s 
//    rule. That means moves with the fewest legal onward moves 
//    should come first.
//

def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = 
    legal_moves(dim, path, x).sortBy(legal_moves(dim,path,_).length)

//(9) Implement a function that searches for a 
//    you have to be careful to write a tail-recursive version as this 
//    function will be called with dimensions of up to 70 * 70
//    and starting field (0, 0). It has to produce a solution within
//    30 seconds.

def tour_on_mega_board(dim: Int, path: Path) : Option[Path] =
    recursive_mega(dim, path, path::List())

@tailrec
def recursive_mega(dim: Int, path: Path, accumulator: List[Path]) : Option[Path] = accumulator match {
  case Nil => None
  case x::xs => if (x.length == dim * dim) Some(x) 
  else recursive_mega(dim, path, ordered_moves(dim, x, x.head).map(_::x))
}

}
