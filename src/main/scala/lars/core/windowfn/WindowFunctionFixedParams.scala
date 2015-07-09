package lars.core.windowfn

import lars.core.semantics.streams.S

/**
 * Window function with fixed parameters
 *
 * Created by hb on 5/26/15.
 */
abstract class WindowFunctionFixedParams extends ((S, Int) => S)