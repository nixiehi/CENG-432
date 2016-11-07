object Simple {
def main(args:Array[String]){
var a=List(10,25,30)
a.foreach(s=> 
if(s%2==0)
 println(s*2)
else 
 println(s*3)
);
}
}
