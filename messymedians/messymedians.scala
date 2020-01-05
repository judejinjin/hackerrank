object Solution {
    import scala.collection.mutable._
    import scala.collection.mutable.HashMap
    import java.io._
        
    def median(s: ArrayBuffer[Int]): Int =
    {
        
        if(s.length == 1) return s(0)

        if(s.length%2==0){
            if(s(s.length/2-1) < s(s.length/2))
                return s(s.length/2-1)
            else
                return s(s.length/2)
        }else{
            return s(s.length/2)
        }
    }

    def main(args: Array[String]) {

        val lines = scala.io.Source.stdin.getLines().toList.map(_.toInt).toArray[Int]
        val T = lines(0)
//        val bw = new BufferedWriter(new OutputStreamWriter(System.out));

        var states = HashMap[Int, Array[Int]]()

        var cache = new Array[Boolean](T)
        for(i<- 1 to T){
           val n = lines(i)
           if(n < 0)
                cache(i+n-1) = true
        }
        var lastState = ArrayBuffer[Int]()
        var i = 1
	var max = 0

	var results = new Array[Int](T)
	
        while(i <= T){
            
            val n = lines(i)
            
            if(n>0){

                    if(lastState.length > 0 && n >= max){
//		        println(i + ": append")
                        lastState += n
			max = n
                    }else if(lastState.length > 0 && n <= lastState(0)){
                        lastState.prepend(n)
                    }else if(lastState.length > 0){

                       var inserted = false

                       var i = lastState.length/2
		       var lastBegin = 0
		       var lastEnd = lastState.length-1

                       do{
	//		   println("insert " + n + ":" + i + ":" + lastBegin + ":" + lastEnd + ":" + lastState.length + ":" + lastState.toList)
                           if(lastState(i) >= n){
                               if(i-1>=0){
                                   if(lastState(i-1) <= n){
                                       inserted = true
                                       lastState.insert(i, n)
                                   }else{
				       lastEnd = i
                                       i = i /2
				       lastBegin = 0
                                   }
                               }else{
                                   inserted = true
                                   lastState.insert(i, n)
                               }
                           }else if(lastState(i)==n){
                               inserted = true
                               lastState.insert(i, n)
                           }else{
                               if(i+1 <= lastState.length-1){
                                   if(lastState(i+1) >= n){
                                       inserted = true
                                       lastState.insert(i+1, n)
                                   }else{
				       lastBegin = i
				       i = ( i + lastEnd) / 2
                                   }
                               }else{
                                   lastState += n
                                   inserted = true
                               }
                           }
                       }while(inserted == false)
                   }else{
                       lastState += n
		       max = n
                   }

                if(cache(i-1))
                      states += (i-1 -> lastState.toArray)

                //println("size:" + states.size)
                //println(median(lastState.toList))

                var out = median(lastState)
		results(i-1) = out
//		println(out)
//                bw.write(out, 0, out.length)

            }else{
                lastState = ArrayBuffer()
                lastState = lastState ++ states.get(i+n-1).get
                //println(median(lastState.toList))
               
                var out = median(lastState)
//		println(out)
//                bw.write(out, 0, out.length)
		results(i-1) = out

		max = lastState(lastState.length-1)
                    
                if(cache(i-1))
                      states += (i-1 -> lastState.toArray)

            }
            
            i += 1
        }

	for(j <- results){
		println(j)
	}

    }
}
