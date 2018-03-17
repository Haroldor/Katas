import java.time.LocalDateTime

def binBase[B](z: B)(acc: (B, String) => B)(rest: Int): B = rest match {
  case 0 => z
  case x => acc(binBase(z)(acc)(Math.floor(rest/2).toInt), (rest % 2).toString)
}

//def bin: Int =>  String = binBase("")(_ + _)
//def binList: Int => List[String] = binBase(List.empty[String])(_ :+ _)
//(0 to 30).map(binList).map(_.mkString("")).mkString("\n")

def binaryGap(x: Int): Int = {
  case class Tally(maxCount: Int, isCounting: Boolean, currentCount: Int){
    def next(digit: String) = digit match {
      case "1" => Tally(Math.max(currentCount, maxCount), true, 0)
      case "0" => Tally(maxCount, isCounting, if (isCounting) currentCount + 1 else 0)
    }
  }
  binBase(Tally(0, false, 0))( _ next _ )(x).maxCount
}

val expecteds = List(0 -> 0, 1 -> 0, 4 -> 0, 5 -> 1, 9 -> 2, 17 -> 3, 34 -> 3)

println("Testing " + LocalDateTime.now())
expecteds.foreach {
  case (dec, exp) => if(!(binaryGap(dec) == exp)) {
    println(s"$dec, ${binaryGap(4)} was not equal to $exp, was ${binaryGap(dec)}")
  }
}

/* *** Comments ***

   This was fiddlier than it first appeared
   Breaking the problem into discrete parts was a good approach
    i.e. first making a binary string from a number then figuring out how to count the gap

   as was coming up with a rough draft of the procedure on paper first
   (I didn't flesh out the whole thing and it was the remaining part, the state transition for is counting which caused errors)

   This problem is very well modelled with a state machine.

   All the state that would be set in a mutable version of the algorithm has to be passed to the next frame as a parameters.

   At first, I calculated 'tail' and returned a tuple from iterate an assigned it to multiple values

   e.g.
       val (maxCount, isCounting, currentCount) = iterate(tail)

   but this approach made me uncomfortable as the meaning of the return values was implicit in the ordering and not 'named'

   - folding over Strings and char arrays was a little painful as I couldn't work out how to pattern match on a string's
    head :: tail and pattern matching on an array looks a bit yucky.
   so I ended up manually converting back and forth between String and array of char and extracting the head/tail long hand
   and then doing a separate match on the head

   In the end, I was able to remove two/three of the zero parameters and encode instead in the data passed onwards

   having a test bench at the bottom worked well, but if it was in the test folder, is might have been able to leverage
   scalatests' assertions

   Setting up the intellij project wasn't too bad, but there must be an archetype or sth, similar for an empty project
   with ScalaTest in

   Syntax practice:
      - matches/partial functions done without explicitly writing the parameter and the term 'match' only work with parameterless
      methods/functions

   Takeaway points:
      - For parsing a stream of input, have a case class which defines its transitions according to the next element in the stream
          to act as the accumulator. This case class must contain any state to pass between frames and is the end result
      - Doing preliminary work on paper, going through cases of the problem was effective
      - Breaking the problem up was effective
      - The worksheet seems to be a fairly good way of iterating

   To try:
     - try starting with a case class, possibly for a different problem - e.g. what's the longest repeating stretch
     - Same using mutable state
     - Try with Monoid
     - Same but with test bench encoded as unit tests
     - Finding a way to draw the algorithm and write what state is passed in each stage, possibly with reference to the
       elec eng modelling work
     - Try with a State Monad
     - Read the Red Book on Rand generator and think about modelling state, perhaps
     - learn nomenclature for def xyz: String => Int v.s. def xyz(a: String): Int

 */