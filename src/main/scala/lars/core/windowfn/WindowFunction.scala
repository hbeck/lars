package lars.core.windowfn

import lars.core.semantics.streams.S

/**
 * Window function closer to general definition. Parameters fixed.
 *
 * Created by hb on 5/26/15.
 */
abstract class WindowFunction /* [X <: WindowParameters](x: X) */ extends ((S, Int) => S)