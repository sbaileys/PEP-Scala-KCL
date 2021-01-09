// Core Part about a "Compiler" for the Brainf*** language
//======================================================


object CW10b {


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
def time_needed[T](n: Int, code: => T) = {
  val start = System.nanoTime()
  for (i <- 0 until n) code
  val end = System.nanoTime()
  (end - start)/(n * 1.0e9)
}


type Mem = Map[Int, Int]

import io.Source
import scala.util._
import scala.annotation.tailrec

def load_bff(name: String) : String = 
  Try(Source.fromFile(s"${name}").mkString).getOrElse("")


// (2) Complete the functions for safely reading  
// and writing brainf***++ memory. 

def sread(mem: Mem, mp: Int) : Int = 
    mem.get(mp).getOrElse(0)

def write(mem: Mem, mp: Int, v: Int) : Mem = 
    mem + (mp -> v)

@tailrec
def jumpRight(prog: String, pc: Int, level: Int) : Int = {
    if (pc < prog.length() && pc >= 0) prog(pc) match {
        case '[' => jumpRight(prog, pc+1, level+1)
        case ']' => if (level == 0) pc+1 else jumpRight(prog, pc+1, level-1)
        case _ => jumpRight(prog, pc+1, level)
    }
    else pc
}

@tailrec
def jumpLeft(prog: String, pc: Int, level: Int) : Int = {
    if (pc < prog.length() && pc >= 0) prog(pc) match {
        case '[' => if (level == 0) pc+1 else jumpLeft(prog, pc-1, level-1)
        case ']' => jumpLeft(prog, pc-1, level+1)
        case _ => jumpLeft(prog, pc-1, level)
    }
    else pc
}


// TASKS
//=======

// (5) Write a function jtable that precomputes the "jump
//     table" for a bf-program. This function takes a bf-program 
//     as an argument and Returns a Map[Int, Int]. The 
//     purpose of this map is to record the information about
//     pc positions where '[' or a ']' are stored. The information
//     is to which pc-position do we need to jump next?
// 
//     For example for the program
//    
//       "+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]"
//
//     we obtain the map
//
//       Map(69 -> 61, 5 -> 20, 60 -> 70, 27 -> 44, 43 -> 28, 19 -> 6)
//  
//     This states that for the '[' on position 5, we need to
//     jump to position 20, which is just after the corresponding ']'.
//     Similarly, for the ']' on position 19, we need to jump to
//     position 6, which is just after the '[' on position 5, and so
//     on. The idea is to not calculate this information each time
//     we hit a bracket, but just look up this information in the 
//     jtable. You can use the jumpLeft and jumpRight functions
//     from Part 1 for calculating the jtable.
//
//     Then adapt the compute and run functions from Part 1 
//     in order to take advantage of the information stored in the jtable. 
//     This means whenever jumpLeft and jumpRight was called previously,
//     you should immediately look up the jump address in the jtable.
 
def jtable(pg: String) : Map[Int, Int] = {
  val mapRight = (for (i <- 0 until pg.length if pg.charAt(i)== '[') yield (i, jumpRight(pg, i+1, 0))).toMap
  val mapLeft = (for (i <- 0 until pg.length if pg.charAt(i)== ']') yield (i, jumpLeft(pg, i-1, 0))).toMap
  mapRight++mapLeft
}

// testcase
//
// jtable("""+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]""")
// =>  Map(69 -> 61, 5 -> 20, 60 -> 70, 27 -> 44, 43 -> 28, 19 -> 6)

@tailrec
def compute2(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
  if(pc > pg.length - 1 || pc < 0) mem
  else pg(pc) match {
    case '>' => compute2(pg, tb, pc + 1, mp + 1, mem)
    case '<' => compute2(pg, tb, pc + 1, mp - 1, mem)
    case '+' => compute2(pg, tb, pc + 1, mp, write(mem, mp, sread(mem, mp) + 1))
    case '-' => compute2(pg, tb, pc + 1, mp, write(mem, mp, sread(mem, mp) - 1))
    case '.' => {
      print(sread(mem, mp).toChar)
      compute2(pg, tb, pc + 1, mp, mem)
    }
    case '[' => {
      if(sread(mem, mp) == 0) compute2(pg, tb, tb(pc), mp, mem)
      else compute2(pg, tb, pc + 1, mp, mem) 
    }
    case ']' => {
      if(sread(mem, mp) != 0) compute2(pg, tb, tb(pc), mp, mem)
      else compute2(pg, tb, pc + 1, mp, mem)
    }
    case '*' => compute2(pg, tb, pc + 1, mp, write(mem, mp, mem(mp) * sread(mem, mp - 1)))
    case '@' => compute2(pg, tb, pc + 1, mp, write(mem, sread(mem, mp), sread(mem, mp - 1)))
    case '#' => {
      print(sread(mem, mp).toInt)
      compute2(pg, tb, pc + 1, mp, mem)
    }
    case _ => compute2(pg, tb, pc + 1, mp, mem)
  }
}

def run2(pg: String, m: Mem = Map()) = 
  compute2(pg, jtable(pg), 0, 0, m)


// testcases
// time_needed(1, run2(load_bff("benchmark.bf")))
// time_needed(1, run2(load_bff("sierpinski.bf")))



// (6) Write a function optimise which deletes "dead code" (everything
// that is not a bf-command) and also replaces substrings of the form
// [-] by a new command 0. The idea is that the loop [-] just resets the
// memory at the current location to 0. In the compute3 and run3 functions
// below you implement this command by writing the number 0 to mem(mp), 
// that is write(mem, mp, 0). 
//
// The easiest way to modify a string in this way is to use the regular
// expression """[^<>+-.,\[\]]""", which recognises everything that is 
// not a bf-command and replace it by the empty string. Similarly the
// regular expression """\[-\]""" finds all occurrences of [-] and 
// by using the Scala method .replaceAll you can replace it with the 
// string "0" standing for the new bf-command.

def optimise(s: String) : String = 
  s.replaceAll("""[^<>+-.,\[\]]""", "").replaceAll("""\[-\]""", "0")

@tailrec
def compute3(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
  if(pc > pg.length - 1 || pc < 0) mem
  else pg(pc) match {
    case '>' => compute3(pg, tb, pc + 1, mp + 1, mem)
    case '<' => compute3(pg, tb, pc + 1, mp - 1, mem)
    case '+' => compute3(pg, tb, pc + 1, mp, write(mem, mp, sread(mem, mp) + 1))
    case '-' => compute3(pg, tb, pc + 1, mp, write(mem, mp, sread(mem, mp) - 1))
    case '.' => {
      print(sread(mem, mp).toChar)
      compute3(pg, tb, pc + 1, mp, mem)
    }
    case '[' => {
      if(sread(mem, mp) == 0) compute3(pg, tb, tb(pc), mp, mem)
      else compute3(pg, tb, pc + 1, mp, mem) 
    }
    case ']' => {
      if(sread(mem, mp) != 0) compute3(pg, tb, tb(pc), mp, mem)
      else compute3(pg, tb, pc + 1, mp, mem)
    }
    case '*' => compute3(pg, tb, pc + 1, mp, write(mem, mp, mem(mp) * sread(mem, mp - 1)))
    case '@' => compute3(pg, tb, pc + 1, mp, write(mem, sread(mem, mp), sread(mem, mp - 1)))
    case '#' => {
      print(sread(mem, mp).toInt)
      compute3(pg, tb, pc + 1, mp, mem)
    }
    case '0' => {
      write(mem, mp, 0)
      compute3(pg, tb, pc + 1, mp, mem)
    }
    case _ => compute3(pg, tb, pc + 1, mp, mem)
  }
}


def run3(pg: String, m: Mem = Map()) = 
  compute3(optimise(pg), jtable(pg), 0, 0, m)


// testcases
//
// optimise(load_bff("benchmark.bf"))          // should have inserted 0's
// optimise(load_bff("mandelbrot.bf")).length  // => 11205
// 
// time_needed(1, run3(load_bff("benchmark.bf")))



// (7)  Write a function combine which replaces sequences
// of repeated increment and decrement commands by appropriate
// two-character commands. For example for sequences of +
//
//              orig bf-cmds  | replacement
//            ------------------------------
//              +             | +A 
//              ++            | +B
//              +++           | +C
//                            |
//              ...           |
//                            | 
//              +++....+++    | +Z
//                (where length = 26)
//
//  Similar for the bf-command -, > and <. All other commands should
//  be unaffected by this change.
//
//  Adapt the compute4 and run4 functions such that they can deal
//  appropriately with such two-character commands.


def combine(s: String) : String = ???

// testcase
// combine(load_bff("benchmark.bf"))


def compute4(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = ???


// should call first optimise and then combine on the input string
//
def run4(pg: String, m: Mem = Map()) = 
  compute4(optimise(pg), jtable(pg), 0, 0, m)


// testcases
// combine(optimise(load_bff("benchmark.bf"))) // => """>A+B[<A+M>A-A]<A[[....."""

// testcases (they should now run much faster)
// time_needed(1, run4(load_bff("benchmark.bf")))
// time_needed(1, run4(load_bff("sierpinski.bf"))) 
// time_needed(1, run4(load_bff("mandelbrot.bf")))


}
