import scala.annotation.tailrec
import scala.collection.immutable
// Shunting Yard Algorithm
// by Edsger Dijkstra
// ========================

object CW8a {

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

// (1) Implement below the shunting yard algorithm. The most
// convenient way to this in Scala is to implement a recursive 
// function and to heavily use pattern matching. The function syard 
// takes some input tokens as first argument. The second and third 
// arguments represent the stack and the output of the shunting yard 
// algorithm.
//
// In the marking, you can assume the function is called only with 
// an empty stack and an empty output list. You can also assume the
// input os  only properly formatted (infix) arithmetic expressions
// (all parentheses will be well-nested, the input only contains 
// operators and numbers).

// You can implement any additional helper function you need. I found 
// it helpful to implement two auxiliary functions for the pattern matching:  
// 

def is_op(op: String) : Boolean = ops.contains(op)

def prec(op1: String, op2: String) : Boolean = precs(op2) >= precs(op1)

@tailrec
def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = (toks, st) match {
  case(Nil, _) => out:::st
  case(x::xs, _) if (x forall Character.isDigit) => syard(xs, st, out:::List(x))
  case(x::xs, Nil) if is_op(x) => syard(xs, List(x):::st, out)
  case(x::xs, y::_) if is_op(x) && ((!is_op(y)) || (!(prec(x, y)))) => syard(xs, List(x):::st, out)
  case(x::xs, y::ys) if is_op(x) && (prec(x, y)) => syard(toks, ys, out:::List(y))
  case("("::xs, _) => syard(xs, List("("):::st, out)
  case(")"::xs, "("::ys) => syard(xs, st.drop(1), out)
  case(")"::xs, _) => syard(toks, st.drop(1), out:::List(st.head))
}



// test cases
// syard(split("3 + 4 * ( 2 - 1 )"))  // 3 4 2 1 - * +
// syard(split("( 5 + 7 ) * 2"))      // 5 7 + 2 *
// syard(split("9 + 24 / ( 7 - 3 )")) // 9 24 7 3 - / +
// syard(split("( ( 3 + 4 ) + 5 )"))    // 3 4 + 5 +
// syard(split("( 3 + ( 4 + 5 ) )"))    // 3 4 5 + +
// syard(split("( ( ( 3 ) ) + ( ( 4 + ( 5 ) ) ) )")) // 3 4 5 + +
// syard(split("3 + 4 + 5"))           // 3 4 + 5 +
// syard(split("5 * 7 / 2"))          // 5 7 * 2 /
// syard(split("5 + 7 / 2"))          // 5 7 2 / +
// syard(split("10 + 12 * 33"))       // 10 12 33 * +
 
// (2) Implement a compute function that evaluates an input list
// in postfix notation. This function takes a list of tokens
// and a stack as argumenta. The function should produce the 
// result as an integer using the stack. You can assume 
// this function will be only called with proper postfix 
// expressions.    

@tailrec
def compute(toks: Toks, st: List[Int] = Nil) : Int = toks match {
  case Nil => st.head
  case x::_ if(x forall Character.isDigit) => compute(toks.tail, st:::List(toks.head.toInt))
  case "+"::_ => compute(toks.tail, st.take(st.length-2):::List(st(st.length-2)+st.last))
  case "-"::_ => compute(toks.tail, st.take(st.length-2):::List(st(st.length-2)-st.last))
  case "*"::_ => compute(toks.tail, st.take(st.length-2):::List(st(st.length-2)*st.last))
  case "/"::_ => compute(toks.tail, st.take(st.length-2):::List(st(st.length-2)/st.last))
}

// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))  // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15

}