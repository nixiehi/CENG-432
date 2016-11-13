class IyteMutableList {
  private var head: Node = null
  private val last: Node = null
  private var prev: Node = null

  def add(x:Int): Unit = {
    if (head==null) {
      head = new Node(x, last)
      prev = head
    }
     else{
      var newNode = new Node(x, last)
        prev.next = newNode
        prev = newNode
    }
    }

  override def toString: String= {
    var result:String = ""
    while(head.next !=null){
      result=result+head.data+","
      head=head.next
    }
      result=result+prev.data
      result
  }

  class Node (var data:Int,var next:Node)
}

object  IyteMutableList{
  def apply(): IyteMutableList = new IyteMutableList()
}
