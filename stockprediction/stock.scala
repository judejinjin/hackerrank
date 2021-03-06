object Solution {
    
    def findLongest(numbers: Array[Long], min:Long, margin:Long):Int={
        var longest = 0
        var counter = 0
        var i = 0
        do{
            var stopped = 0
            var stoppedIndex = 0
            if(numbers(i) >= min && numbers(i) <=(min+margin) && (numbers.length - 1- i)>=longest){
               
                counter = 0
                var j = i
                do{
                    if(numbers(j) >= min && numbers(j)<=(min+margin) && stopped == 0)
                        counter += 1
                    else{
                        stopped = 1
                        stoppedIndex = j
                    }
                    j += 1
                    if(stopped==1)
                        j = numbers.length
                }while(j <= numbers.length-1)
                if(counter > longest)
                    longest = counter
            }
            i+= 1
            if(stopped==1)
                i = stoppedIndex+1
        }while(i <= numbers.length-1)
        return longest
    }
    
    def main(args: Array[String]) {
        /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution
*/
        val lines = io.Source.stdin.getLines
        val N = lines.next.toInt
        val numbers = lines.next.split(" ").toList.map(_.toLong).toArray[Long]
        val M = lines.next.toInt
        for(i<- 1 to M){
            val params = lines.next.split(" ").toList.map(_.toInt)
            println(findLongest(numbers, numbers(params(0)), params(1)))
        }
    }
}