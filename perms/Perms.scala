object Solution {
    import scala.collection.mutable._
    import scala.math._
    val modulo = (pow(10, 9) + 7).toInt
    
    def C(n:Int, k:Int, factorial:Long, resultsHM1:Array[Long], resultsHM2:Array[Long]):Long ={

	var results_i:Long = 0
	results_i = factorial
	resultsHM2(k) = results_i
	
	for(i <- k+1 to n){
	    results_i = (results_i + (i-1)*resultsHM1(i-1)) % modulo
	    resultsHM2(i) = results_i
	}

	return results_i
    }
        
    def main(args: Array[String]) {

        val lines = io.Source.stdin.getLines
        val N = lines.next.toInt
            
        val results = new ArrayBuffer[Long]()

	var factorial:BigInt = 1

	var resultsHM1 = new Array[Long](N+1)
    	var resultsHM2 = new Array[Long](N+1)	      

	for(i <- 2 to N){
	      resultsHM2(i) = i*(i-1)/2
	}

	results += resultsHM2(N)
	var temp = resultsHM1
	resultsHM1 = resultsHM2
	resultsHM2 = temp
	      
	for(i <- 2 to N-1){
              factorial = factorial * i
	      
              results += C(N, i, (factorial/2%modulo).toLong, resultsHM1, resultsHM2)

	      var temp = resultsHM1
	      resultsHM1 = resultsHM2
	      resultsHM2 = temp
        }    
            
        println(results.map(_ % modulo).mkString(" "))
    }
}