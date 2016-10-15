package week3

/**
 * The iterator contract:
 * ▶ next can be called only if hasNext returns true
 * ▶ after hasNext returns false, it will always return false
 */
trait Iterator[T] {

  def hasNext: Boolean
  def next(): T

  def foldLeft[S](z: S)(f: (S, T) => S): S = {
    var result = z
    while (hasNext) {
      result = f(result, next())
    }
    result
  }
}

