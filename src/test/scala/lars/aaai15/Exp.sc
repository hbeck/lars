import lars.core.Formulas.{Diamond, Window, Atom}
import lars.core.LStreams.{Timeline, Evaluation, LStream}
import lars.core.{WindowOperators, Structure}
import lars.core.windowfn.{TimeBasedWindowParameters, TimeBasedWindow}
val B = Set(Atom("plan","l1","b","m","8"),
            Atom("plan","l2","g","m","7"),
            Atom("plan","l3","h","m","3"),
            Atom("line","a1,l1"),
            Atom("line","a2","l2"),
            Atom("line","a3","l3"))
val T = Timeline(0,50)
val v = Evaluation(Map(36 -> Set(Atom("tram","a1","b")), 40 -> Set(Atom("tram","a3","h"))))
val D = LStream(T,v)
val SS = D ++ LStream(T,Evaluation(Map(43 -> Set(Atom("exp","a3","m")), 44 -> Set(Atom("exp","a1","m")))))
val M = Structure(SS.T,SS.v,B)
val fm = Window(TimeBasedWindow,WindowOperators.ch2,TimeBasedWindowParameters(0,5,1),Diamond(Atom("exp","a3","m")))
M/SS/42 ||- fm
