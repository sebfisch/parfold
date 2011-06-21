# Performance of different parallel fold functions in Haskell

Inspired by discussions on Haskell mailing lists:

  * [efficient parallel foldMap for lists/sequences][foldMap]
  * [Dead else branch does influence runtime?][uvector]

A version with unboxed vectors is based on [a similar
function][foldb_cap] by Johannes Waldmann that was first proposed by
Daniel Fischer in the second discussion above.

[foldMap]: https://groups.google.com/forum/#!topic/parallel-haskell/s015bmZy_Ws
[uvector]: http://www.haskell.org/pipermail/haskell-cafe/2011-June/093141.html
[foldb_cap]: http://www.imn.htwk-leipzig.de/~waldmann/ss11/skpp/code/kw24/mps-vector.hs
