package test

//Example data:
//class Cov(eff: Int, term: Int)
//
//val coverages = List(
//Cov(1, 20), Cov(21, 30), Cov(15, 25), Cov(28, 40), Cov(50, 60), Cov(61, 200)

object Hello extends App {
  case class Cov(eff: Int, term: Int) {
    def days: Int = term - eff + 1
    override def toString: String = s"Cov($eff, $term):$days"
  }
  def Coverage(ls: List[Cov]): Option[Cov] = {
    ls.foldLeft(List.empty[Cov]) { (rc, cov) =>
      rc.headOption match {
        case None => List(cov)
        case Some(h) => {
          if ((h.term + 1) < cov.eff) (cov +: rc) else {
            val combined: Cov = Cov(Math.min(h.eff, cov.eff), Math.max(h.term, cov.term))
            combined +: rc.tail
          }
        }
      }
    }.sortWith(_.days > _.days).headOption
  }
  val covs: List[Cov] = List(Cov(1, 20), Cov(21, 30), Cov(15, 25), Cov(28, 40), Cov(50, 60), Cov(61, 200))
  println(Coverage(covs))
  println(Coverage(List.empty[Cov]))
}


