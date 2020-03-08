val help: (Int, Int => Boolean, Int, Int) => List[Int] = {
  case (n, p, p1, p2) if n < p2 => List()
  case (n, p, p1, p2) if p(p2) => p2 :: help(n, p, p2, p1 + p2)
  case (n, p, p1, p2) => help(n, p, p2, p1 + p2)
}

val fib: (Int, Int => Boolean) => List[Int] = {
  (n, p) => help(n, p, 0, 1)
}
