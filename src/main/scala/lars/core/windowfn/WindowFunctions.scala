package lars.core.windowfn

import lars.core.LStreams.LStream

/**
 * Created by hb on 1/2/15.
 */

//abstract class WindowType
//abstract class TimeBasedWindowType extends WindowType

abstract class WindowParameters

abstract class WindowFunction[U <: WindowParameters]() extends ((LStream, Int, U) => LStream)


