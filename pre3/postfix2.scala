import scala.annotation.tailrec
import scala.collection.immutable
import scala.math.pow
// Shunting Yard Algorithm 
// including Associativity for Operators 
// =====================================

object CW8b {


// type of tokens
type Toks = List[String]

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList

// left- and right-associativity
abstract class Assoc
case object LA extends Assoc
case object RA extends Assoc


// power is right-associative,
// everything else is left-associative
def assoc(s: String) : Assoc = s match {
  case "^" => RA
  case _ => LA
}


// the precedences of the operators
val precs = Map("+" -> 1,
  		"-" -> 1,
		"*" -> 2,
		"/" -> 2,
                "^" -> 4)

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/", "^")

// (3) Implement the extended version of the shunting yard algorithm.
// This version should properly account for the fact that the power 
// operation is right-associative. Apart from the extension to include
// the power operation, you can make the same assumptions as in 
// basic version.
def is_op(op: String) : Boolean = ops.contains(op)

def prec(op1: String, op2: String) : Boolean = precs(op2) > precs(op1)
def equal(op1: String, op2: String) : Boolean = precs(op2) == precs(op1)

def left(op: String) : Boolean = {
  assoc(op: String) == LA
}

@tailrec
def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = (toks, st) match {
  case(Nil, _) => out:::st
  case(x::xs, _) if (x forall Character.isDigit) => syard(xs, st, out:::List(x))
  case(x::xs, Nil) if is_op(x) => syard(xs, List(x):::st, out)
  case(x::xs, y::_) if is_op(x) && (!is_op(y) || (!(prec(x, y)) && !equal(x, y))) => syard(xs, List(x):::st, out)
  case(x::xs, y::ys) if is_op(x) && prec(x, y) => syard(toks, ys, out:::List(y))
  case(x::xs, y::ys) if is_op(x) && equal(x, y) && left(y) => syard(toks, ys, out:::List(y))
  case(x::xs, y::ys) if is_op(x) && equal(x, y) && !left(y) => syard(xs, List(x):::st, out)
  case("("::xs, _) => syard(xs, List("("):::st, out)
  case(")"::xs, "("::ys) => syard(xs, st.drop(1), out)
  case(")"::xs, _) => syard(toks, st.drop(1), out:::List(st.head))
}

// test cases
//syard(split("3 + 4 * 8 / ( 5 - 1 ) ^ 2 ^ 3"))  // 3 4 8 * 5 1 - 2 3 ^ ^ / +

// (4) Implement a compute function that produces an Int for an
// input list of tokens in postfix notation.

@tailrec
def compute(toks: Toks, st: List[Int] = Nil) : Int = toks match {
  case Nil => st.head
  case x::_ if(x forall Character.isDigit) => compute(toks.tail, st:::List(toks.head.toInt))
  case "+"::_ => compute(toks.tail, st.take(st.length-2):::List(st(st.length-2)+st.last))
  case "-"::_ => compute(toks.tail, st.take(st.length-2):::List(st(st.length-2)-st.last))
  case "*"::_ => compute(toks.tail, st.take(st.length-2):::List(st(st.length-2)*st.last))
  case "/"::_ => compute(toks.tail, st.take(st.length-2):::List(st(st.length-2)/st.last))
  case "^"::_ => compute(toks.tail, st.take(st.length-2):::List(pow(st(st.length-2),st.last).toInt))
}


// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))   // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15
// compute(syard(split("4 ^ 3 ^ 2")))      // 262144
// compute(syard(split("4 ^ ( 3 ^ 2 )")))  // 262144
// compute(syard(split("( 4 ^ 3 ) ^ 2")))  // 4096
// compute(syard(split("( 3 + 1 ) ^ 2 ^ 3")))   // 65536

}
