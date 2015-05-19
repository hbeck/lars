import lars.core.Formulas.Atom
import lars.core.LStreams.{Timeline, Evaluation, LStream}
import lars.core.windowfn.TimeBasedWindow

val T = Timeline(0,50)
val v = Evaluation(Map(36 -> Set(Atom("tram","a1","b")), 40 -> Set(Atom("tram","a3","h"))))
val D = LStream(T,v)

val S1 = TimeBasedWindow(D,42,(4,0,1))
val S2 = TimeBasedWindow(D,42,4,0,1)
val S3 = TimeBasedWindow(D,42,4)
def w = TimeBasedWindow
val S4 = w(D,42,4)
S1 == S2
S1 == S3
S1 == S4
val v1 = Evaluation(Map(40 -> Set(Atom("tram","a3","h"))))
S1 == LStream((38,42),v1)