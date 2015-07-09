package lars.core.windowfn

import lars.core.semantics.streams.S

/**
 * Window function
 *
 * Created by hb on 7/6/15.
 */
abstract class WindowFunction[X <: WindowParameters]() extends ((S,Int,X) => S)
