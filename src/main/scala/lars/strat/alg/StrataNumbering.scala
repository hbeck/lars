package lars.strat.alg

import lars.strat.DepGraph

/**
 * Created by hb on 7/16/15.
 */
abstract class StrataNumbering extends (PartitionedGraph => Map[DepGraph,Int])
