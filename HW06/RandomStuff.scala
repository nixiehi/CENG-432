object RandomStuff extends RandomStuffTrait {
  def transform(list: List[Int], func: (Int) => Int): List[Int] = {
    if (list != null) {
      val result = for (i <- list) yield func(i)
      return result
    }
    else
      null
  }

  def allValid(list: List[Int], func: (Int) => Boolean): Boolean = {
    for (i <- list) {
      if (!func(i))
        return false
    }
    return true
  }

  val a = 1

  def executeWithRetry(times: Int, func: => Int): Option[Int] = {
    var temp = times + 1

    if (times < 0) {
      temp = 1
    }
    while (temp > 0) {
      try {
        val result = func
        return Some(result)
      }
      catch {
        case ex: Exception => temp = temp - 1
      }
    }

    return None
  }

}
trait RandomStuffTrait{
  def transform(list:List[Int], func: (Int) => Int):List[Int]
  def allValid(list:List[Int], func: (Int) => Boolean):Boolean
  def executeWithRetry(times:Int, func: => Int):Option[Int]
}
