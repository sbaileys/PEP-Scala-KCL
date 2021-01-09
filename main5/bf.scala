import scala.annotation.tailrec
// Core Part about an Interpreter for 
// the Brainf***++ language
//==============================================


object CW10a {


// representation of Bf memory 

type Mem = Map[Int, Int]


// (1) Write a function that takes a file name as argument and
// and requests the corresponding file from disk. It Returns the
// content of the file as a String. If the file does not exists,
// the function should Return the empty string.

import io.Source
import scala.util._

def load_bff(name: String) : String = 
  Try(Source.fromFile(s"${name}").mkString).getOrElse("")


// (2) Complete the functions for safely reading  
// and writing brainf***++ memory. Safely read should
// Return the value stored in the Map for a given memory
// pointer, provided it exists; otherwise it Returns 0. The
// writing function generates a new Map with the
// same data, except at the given memory pointer the
// value v is stored.


def sread(mem: Mem, mp: Int) : Int = 
    mem.get(mp).getOrElse(0)

def write(mem: Mem, mp: Int, v: Int) : Mem = 
    mem + (mp -> v)



// (3) Implement the two jumping instructions in the 
// brainf***++ language. In jumpRight, given a program and 
// a program counter move the program counter to the right 
// until after the *matching* ]-command. Similarly, 
// jumpLeft implements the move to the left to just after
// the *matching* [-command.
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


// testcases
//jumpRight("""--[..+>--],>,++""", 3, 0)         // => 10
//jumpLeft("""--[..+>--],>,++""", 8, 0)          // => 3
//jumpRight("""--[..[+>]--],>,++""", 3, 0)       // => 12
//jumpRight("""--[..[[-]+>[.]]--],>,++""", 3, 0) // => 18
//jumpRight("""--[..[[-]+>[.]]--,>,++""", 3, 0)  // => 22 (outside)
//jumpLeft("""[******]***""", 7, 0)              // => -1 (outside)



// (4) Complete the compute function that interprets (runs) a brainf***++
// program: the arguments are a program (represented as a string), a program 
// counter, a memory counter and a brainf***++ memory. It Returns the
// memory at the stage when the execution of the brainf***++ program
// finishes. The interpretation finishes once the program counter
// pc is pointing to something outside the program string.
// If the pc points to a character inside the program, the pc, 
// memory pointer and memory need to be updated according to 
// rules of the brainf***++ language. Then, recursively, the compute 
// function continues with the command at the new program
// counter. 
//
// Implement the run function that calls compute with the program
// counter and memory counter set to 0.

@tailrec
def compute(prog: String, pc: Int, mp: Int, mem: Mem) : Mem = {
  if(pc > prog.length - 1 || pc < 0) mem
  else prog(pc) match {
    case '>' => compute(prog, pc + 1, mp + 1, mem)
    case '<' => compute(prog, pc + 1, mp - 1, mem)
    case '+' => compute(prog, pc + 1, mp, write(mem, mp, sread(mem, mp) + 1))
    case '-' => compute(prog, pc + 1, mp, write(mem, mp, sread(mem, mp) - 1))
    case '.' => {
      print(sread(mem, mp).toChar)
      compute(prog, pc + 1, mp, mem)
    }
    case '[' => {
      if(sread(mem, mp) == 0) compute(prog, jumpRight(prog, pc + 1, 0), mp, mem)
      else compute(prog, pc + 1, mp, mem) 
    }
    case ']' => {
      if(sread(mem, mp) != 0) compute(prog, jumpLeft(prog, pc - 1, 0), mp, mem)
      else compute(prog, pc + 1, mp, mem)
    }
    case '*' => compute(prog, pc + 1, mp, write(mem, mp, mem(mp) * sread(mem, mp - 1)))
    case '@' => compute(prog, pc + 1, mp, write(mem, sread(mem, mp), sread(mem, mp - 1)))
    case '#' => {
      print(sread(mem, mp).toInt)
      compute(prog, pc + 1, mp, mem)
    }
    case _ => compute(prog, pc + 1, mp, mem)
  }
}

def run(prog: String, m: Mem = Map()) = 
  compute(prog, 0, 0, m)


// some sample bf/bf++-programs collected from the Internet
//==========================================================


// some contrived (small) programs
//---------------------------------

// clears the 0-cell
//run("[-]", Map(0 -> 100))    // Map will be 0 -> 0

// moves content of the 0-cell to 1-cell
//run("[->+<]", Map(0 -> 10))  // Map will be 0 -> 0, 1 -> 10


// copies content of the 0-cell to 2-cell and 4-cell
//run("[>>+>>+<<<<-]", Map(0 -> 42))    // Map(0 -> 0, 2 -> 42, 4 -> 42)


// prints out numbers 0 to 9
//run("""+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]""")

// bf++ program calculating the cube-function, 10 * 10 * 10 = 1000
//run("""++++++++++#>+***#""")           // Map(0 -> 10, 1 -> 1000)


// bf++ program copies 3 from 0-cell to to cells 1, 4, 5, 6 and 7
// (note that because of how the program wprks cell 1 will contain 7) 
//run("""+++>+@+@+@+@+@""")   // Map(0 -> 3, 1 -> 7, 4 -> 3, 5 -> 3, 6 -> 3, 7 -> 3)



// some more "useful" programs
//-----------------------------

// hello world program 1
//run("""++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++
//       ..+++.>>.<-.<.+++.------.--------.>>+.>++.""")

// hello world program 2
//run("""++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>+
//       +.<<+++++++++++++++.>.+++.------.--------.>+.>.""")

// hello world program 3
//run("""+++++++++[>++++++++>+++++++++++>+++++<<<-]>.>++.+++++++..
//       +++.>-.------------.<++++++++.--------.+++.------.--------.>+.""")

 
// draws the Sierpinski triangle
//run(load_bff("sierpinski.bf"))


//fibonacci numbers below 100
//run("""+++++++++++
//      >+>>>>++++++++++++++++++++++++++++++++++++++++++++
//      >++++++++++++++++++++++++++++++++<<<<<<[>[>>>>>>+>
//      +<<<<<<<-]>>>>>>>[<<<<<<<+>>>>>>>-]<[>++++++++++[-
//      <-[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<[>>>+<<<
//      -]>>[-]]<<]>>>[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]
//      >[<<+>>[-]]<<<<<<<]>>>>>[+++++++++++++++++++++++++
//      +++++++++++++++++++++++.[-]]++++++++++<[->-<]>++++
//      ++++++++++++++++++++++++++++++++++++++++++++.[-]<<
//      <<<<<<<<<<[>>>+>+<<<<-]>>>>[<<<<+>>>>-]<-[>>.>.<<<
//      [-]]<<[>>+>+<<<-]>>>[<<<+>>>-]<<[<+>-]>[<+>-]<<<-]""")

//outputs the square numbers up to 10000
//run("""++++[>+++++<-]>[<+++++>-]+<+[>[>+>+<<-]++>>[<<+>>-]>>>[-]++>[-]+
//       >>>+[[-]++++++>>>]<<<[[<++++++++<++>>-]+<.<[>----<-]<]
//       <<[>>>>>[>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<]]<[>+<-]>]<<-]<<-]""")


// calculates 2 to the power of 6 
//(example from a C-to-BF compiler at https://github.com/elikaski/BF-it)
//run(""">>[-]>[-]++>[-]++++++><<<>>>>[-]+><>[-]<<[-]>[>+<<+>-]>[<+>-]
//       <><[-]>[-]<<<[>>+>+<<<-]>>>[<<<+>>>-][-]><<>>[-]>[-]<<<[>>[-]
//       <[>+>+<<-]>[<+>-]+>[[-]<-<->>]<<<-]>>[<<+>>-]<<[[-]>[-]<<[>+>
//       +<<-]>>[<<+>>-][-]>[-]<<<<<[>>>>+>+<<<<<-]>>>>>[<<<<<+>>>>>-]
//       <<>>[-]>[-]<<<[>>>+<<<-]>>>[<<[<+>>+<-]>[<+>-]>-]<<<>[-]<<[-]
//       >[>+<<+>-]>[<+>-]<><[-]>[-]<<<[>>+>+<<<-]>>>-[<<<+>>>-]<[-]>[-]
//       <<<[>>+>+<<<-]>>>[<<<+>>>-][-]><<>>[-]>[-]<<<[>>[-]<[>+>+<<-]>
//       [<+>-]+>[[-]<-<->>]<<<-]>>[<<+>>-]<<][-]>[-]<<[>+>+<<-]>>[<<+>
//       >-]<<<<<[-]>>>>[<<<<+>>>>-]<<<<><>[-]<<[-]>[>+<<+>-]>[<+>-]<>
//       <[-]>[-]>[-]<<<[>>+>+<<<-]>>>[<<<+>>>-]<<>>[-]>[-]>[-]>[-]>[-]>
//       [-]>[-]>[-]>[-]>[-]<<<<<<<<<<>>++++++++++<<[->+>-[>+>>]>[+[-<+
//       >]>+>>]<<<<<<]>>[-]>>>++++++++++<[->-[>+>>]>[+[-<+>]>+>>]<<<<<
//       ]>[-]>>[>++++++[-<++++++++>]<.<<+>+>[-]]<[<[->-<]++++++[->++++
//       ++++<]>.[-]]<<++++++[-<++++++++>]<.[-]<<[-<+>]<<><<<""")



// a Mandelbrot set generator in brainf*** written by Erik Bosman
// (http://esoteric.sange.fi/brainfuck/utils/mandelbrot/)
//
//run(load_bff("mandelbrot.bf"))


// a benchmark program (counts down from 'Z' to 'A')
//
//run(load_bff("benchmark.bf"))

// calculates the Collatz series for numbers from 1 to 30
//
//run(load_bff("collatz.bf"))

}
