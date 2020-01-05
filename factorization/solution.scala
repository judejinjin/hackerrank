object Solution {
    import scala.collection.mutable._
    
    def find(numbers: Array[Int], N:Long, best:Array[Int], state:Array[Int], states:ArrayBuffer[Array[Int]], index:Int) {
       
        for(i<- index to numbers.length-1){
            if( (N/state(state.length-1)) % numbers(i) == 0 && (best.length == 1 || (best.length!= 1 && (state.length+1) <= best.length)) && ( states.length == 0 || (state.length+1) <=states(0).length )) {
                var newState = new ArrayBuffer[Int]
                
                newState ++= state
                newState += state(state.length-1) * numbers(i)
                //println(state.toList + ":" + newState.toList)
          
                if(newState(newState.length-1) < N){
                   
                    find(numbers, N, best, newState.toArray, states, index)
                }else{
                    //println("find:" + newState.toList)
		    if(states.length==0 || (state.length+1) < states(0).length){
                          states.prepend(newState.toArray)
		    }else if(states.length==0 || (state.length+1) == states(0).length){
                          states += newState.toArray
		    }
                }
                    
            }
        }
        
    }
    
    def findBest(numbers: Array[Int], N:Long, K:Long): Array[Int] ={
        
        val start = 1
        var best = Array[Int](-1)
        
        var sortedNumber = numbers.toList.sortWith(_>_).toArray[Int]
        //println(sortedNumber.toList)

        for(i<- 0 to sortedNumber.length-1){
            val state = new ArrayBuffer[Int]()
            var states = new ArrayBuffer[Array[Int]]()
          
            state += start
            if(N % sortedNumber(i)==0 && N != 1){
                state += sortedNumber(i)
                find(sortedNumber, N, best, state.toArray, states, i)
            }
            states = states.filter(x => (x(x.length-1)==N))
                
            if(states.length>0){
                println(numbers(i) + ": find " + states.length + " solution")
                for(state<- states){    
                    if(best.length == 1 && best(0) == -1){
                        best = state
                    }else{
                        if(state.length < best.length){
                            best = state
                        }else if(state.length == best.length){
                            var beat = 0
                            var stateTotal = 1
                            var bestTotal = 1
                            var equal = 1
                                
                            for(i<- 1 to state.length-1){
                                if(i < state.length-1 && (numbers.indexOf(state(i)/stateTotal) < numbers.indexOf(best(i)/bestTotal))){
                                    if(equal==1)
                                        beat = 1
                                    
                                }else if(numbers.indexOf(state(i)/stateTotal) != numbers.indexOf(best(i)/bestTotal)){
                                    equal = 0
                                }

                                stateTotal = state(i)
                                bestTotal = best(i)
                            }
                            if(beat==1)
                                best = state
                        }
                    }

                }
                //println("best:" + best.toList)
            }else{
                //println(numbers(i) + ": find no solution")
            }
        }
        return best.toArray
    }
    
    def main(args: Array[String]) {
        /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution
*/
        val lines = io.Source.stdin.getLines
        val params = lines.next.split(" ").toList.map(_.toLong)
        val numbers = lines.next.split(" ").toList.map(_.toInt).toArray[Int]
            
        println(findBest(numbers, params(0), params(1)).mkString(" "))
            
    }
}