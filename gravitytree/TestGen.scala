object TestGen{
 import scala.collection.mutable._
 
 def main(args: Array[String]) {
    var n = 100000
    var q = 3
    
    println(n)
    var line = new StringBuffer()
    for(i<-1 to n-1){
    	line.append(i)
	line.append(" ")
    }
    println(line)

    println(q)
    println("1 100000")
    println("1 99999")
    println("1 99998")
    /*
    for(i<- 1 to q){
        println("100000" + (n-i+1))
    }*/
 }
}