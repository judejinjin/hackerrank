import scala.collection.mutable._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

object Solution {
    val modulo:Long = (Math.pow(10, 9) + 7).toLong
        
    def gcd(a: BigInt,b: BigInt): BigInt = { 
        var aVar = a
        var bVar = b
        while( bVar!=0 ){
          aVar = aVar % bVar;
          if( aVar==0 ) return bVar;
          bVar = bVar % aVar;
       }
       return aVar;
    } 

    def findSmallestCommonDenominator(A :Array[BigInt], l:Int, r: Int): BigInt = {
        var scd:BigInt = A(l) 
        for(i<- l+1 to r){
            val g = gcd(scd, A(i))
            scd = scd  * A(i) /g
        }
        return scd
    }
    
    def runJob(lines: Array[String], start:Int, end:Int, A: Array[BigInt]) : List[Int] ={
       var buffer = ListBuffer[Int]()
//       var A = AasList.toArray

       //println(start + ":" + end)

       for(i <- 0 to lines.length-1){
            val query = lines(i).split(" ")
            if(query(0).equals("Q") && i >= start && i < end){
                val l = query(1).toInt
                val r = query(2).toInt
               
                val result  = findSmallestCommonDenominator(A, l, r)

                //println(start + ":" + end + ":Q" + l + "-"+ r + ":" + result)
                if(result > modulo)
                    buffer += (result%modulo).toInt
                else
                    buffer += result.toInt
            }else if(query(0).equals("U") && i < end){
                val idx = query(1).toInt
                val value = query(2).toInt
                
                A(idx) = A(idx) * value
                // println(start + ":" + end + ":U" + idx + "-" + value + ":" + A.toList)
            }
        } 
        return buffer.toList
    }
        
    def main(args: Array[String]) {
        /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution
*/
        var lines:List[String] = io.Source.stdin.getLines.toList
        val N = lines(0).split(" ")(0).toInt
        val tokens = lines(1).split(" ")
            
        var A = new Array[BigInt](N)
        
        for(i <- 1 to N){
            A(i-1) = tokens(i-1).toLong
        }
        //println(A.toList)
        val K = lines(2).split(" ")(0).toInt
        lines = lines.takeRight(lines.length-3)

	var results =  runJob(lines.toArray[String], 0, lines.length, A);
	for(i<-results){
            println(i)
	}
/*

        val futures = new ListBuffer[Future[List[Int]]]
        var counter = 0
        
        var threads = 1
        if(lines.length > 8)
            threads = 2
        var chunk:Int = (lines.length / threads).toInt
        while(counter < threads) {
            val start = counter * chunk 
            val end = if(counter+1==threads) lines.length else start + chunk
            
            val future :Future[List[Int]] = Future { runJob(lines.toArray[String], start, end, A) }      
            futures += future
            counter += 1 
        }
        val f = Future.sequence(futures.toList)
        Await.ready(f, Duration.Inf)
        var results = f.value.get
        //print(results)


        for(i <- results.get.flatten){
            
                println(i)
        }
*/

    }
}