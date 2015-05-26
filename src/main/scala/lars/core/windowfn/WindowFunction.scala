package lars.core.windowfn

import lars.core.semantics.streams.S

/**
 * Created by hb on 5/26/15.
 */
abstract class WindowFunction[U <: WindowParameters]() extends ((S, Int, U) => S)
