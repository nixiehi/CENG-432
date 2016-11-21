class IyteMutableSet {
  private var bucket = new Array[LList](139)


  def add(item: Int): Unit = {
    var index: Int = hashCode(item)
    if (bucket(index) == null) {
      var elem = LList()
      elem.add(item)
      bucket(index) = elem
    }
    else {
      if(! bucket(index).contains(item))
        bucket(index).add(item)
    }
  }

  def contains(item:Int): Boolean ={
    var index: Int = hashCode(item)
    bucket(index).contains(item)
  }


  private def hashCode(item: Int): Int = {
    (item.hashCode() % 139)%47
  }

  override def toString: String = {
    var result: String = ""
    for (i <- 0 until bucket.length) {
      if (bucket(i) != null)
        result =result+bucket(i) + ","
    }
    if(result == null)
      return ""
    return result.dropRight(1)
  }

}
 class LList {
  class Node (var data:Int,var next:Node)

  private var head: Node = null
  private val last: Node=null
  private var prev: Node=null
  private var size: Int = 0

  def add(temp: Int) {
    head match {
      case null =>
        head = new Node(temp, last)
        prev = head
      case _ =>
        var newNode = new Node(temp, last)
        prev.next =newNode
        prev = newNode
    }
    size = size + 1;
  }
  def contains(item: Int): Boolean = goToNext(item, head)

  private def goToNext(item: Int, middle:Node): Boolean = {
    if (middle != last)
      middle.data match {
        case s if (s == item) => true
        case _ => goToNext(item, middle.next)
      }
    else
      false
  }

  override def toString: String = {
    var result:String=""
    while(head.next !=last){
      result=result+head.data+","
      head=head.next
    }
    result+prev.data
  }
  def length(): Int ={
    size
  }
}
object  LList{
  def apply(): LList = new LList()
}

object  IyteMutableSet{
  def apply(): IyteMutableSet = new IyteMutableSet()
}
