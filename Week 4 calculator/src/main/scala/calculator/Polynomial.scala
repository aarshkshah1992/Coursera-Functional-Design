package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal {
      val ax = a()
      val bx = b()
      val cx = c()
      (bx * bx) - (4 * ax * cx)

    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val deltaroot = Math.sqrt(delta())
      val minusb = -(b())
      val twoa = 2 * a()
      Set((minusb + deltaroot) / twoa, (minusb - deltaroot) / twoa)
    }
  }
}
