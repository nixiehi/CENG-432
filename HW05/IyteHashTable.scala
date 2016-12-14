object IyteHashTable{
  def apply(): IyteHashTable = new IyteHashTable()
}
class IyteHashTable {
  var size: Int = 12
  var current_size=0

  private var bucket = new Array [Entry](size)

  def set(K: String, V: String): Unit = {
    val index=myHash(K) %size
    var elem=bucket(index)
    while (elem != null && elem.K !=K) {
      elem=elem.next
    }
   if(elem ==null){
         bucket(index)=  Entry(K, V,bucket(index))
         if( current_size>size*0.80)
           reHash()
   }
       else elem.V=V
    current_size = current_size + 1
    }

  private def reHash(): Unit ={
    size=size+size
    val old_bucket=bucket
    bucket= new Array [Entry](size)
    for (item<-old_bucket if item != null){
          var temp=item
      while(temp !=null){
        val next=temp.next
        val index=myHash(temp.K)%size
        temp.next=bucket(index)
        bucket(index)=temp
        temp=next

      }
  }
  }
  def get(key:String): String ={
    val index=myHash(key)%size
    var temp=bucket(index)
    if(temp==null)
      return null
    else{

      while(temp !=null){
        if (temp.K==key) return temp.V
        temp=temp.next
      }
      return null
    }
  }
  case class Entry(var K: String, var V: String,var next:Entry)

  private def myHash(K: String): Int ={
    var hash = 997

    for (ch <- K.toCharArray)
      hash = 31 * hash + ch

    hash = hash ^ (hash >> 20) ^ (hash >> 12)
    ( hash ^ (hash >> 7) ^ (hash >> 4))>>>1
  }




}
