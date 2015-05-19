import lars.core.Formulas.Atom
import lars.core.LStreams.{Evaluation, Timeline, LStream}

val m = Map(3 -> Set(Atom("a")))
val em = Map[Int,Set[Atom]]()
val vm = Evaluation(m)
val vem = Evaluation(em)

Timeline(1,4) <= Timeline(0,5)
vem <= vem
vm <= vm
//LStream((0,5),m) <= LStream((0,5),m)
val S14 = LStream((1,4),em)
val S05 = LStream((0,5),em)
S14 <= S05
//
LStream((1,4),m) <= LStream((0,5),m)
//
val S1 = LStream(Timeline(0, 5), Evaluation(m))
val S2 = LStream(Timeline(2, 4), m) //using implicit Evaluation
val S3 = LStream((0, 5), em) //using also implicit Timeline
S1 <= S1
S2 <= S1
S3 <= S1
