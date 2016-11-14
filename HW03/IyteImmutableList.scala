abstract class IyteImmutableList {
  def head:Int
  def tail:IyteImmutableList
  def add(x:Int):IyteImmutableList
}

class Node (override val head:Int,override val tail:IyteImmutableList) extends IyteImmutableList{
  override def add(x:Int): IyteImmutableList = {
    return new Node(x,this)
  }

  def next(t: IyteImmutableList): String = {
    if (t.tail == NonValue())
      t.head+""
    else
      t.head + "," +next(t.tail)
  }

  override def toString:String={
    next(this)
  }
}

 case class NonValue() extends IyteImmutableList{
  override def head: Int =throw new IllegalArgumentException("no  such an element")

  override def tail: IyteImmutableList = throw new IllegalArgumentException("no  such an element")

  override def add(x: Int): IyteImmutableList =new  Node(x,NonValue())
 }

object  IyteImmutableList{
  def apply(): IyteImmutableList = NonValue()
}
