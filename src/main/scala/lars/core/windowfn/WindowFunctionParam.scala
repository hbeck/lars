package lars.core.windowfn

import lars.core.semantics.streams.S

/**
 * Window function that deviates from the standard by having parameters as additional argument
 *
 * Created by hb on 7/6/15.
 */
abstract class WindowFunctionParam[X <: WindowParameters]() extends ((S,Int,X) => S)
