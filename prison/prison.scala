object Solution {
    import scala.collection.mutable._
       
    def findCircles(matrix: Array[HashSet[Int]], visited:HashSet[Int], circle:HashSet[Int], index:Int) {
        for(i <- 0 to index){
            //println("checking " + i)
            if(!visited.contains(i) && i != index && matrix(index).contains(i)){
                circle += i
                visited += i
                
                if(matrix(i).toList.filter(!visited.contains(_)).length > 0)
                    findCircles(matrix, visited, circle, i)
            }
        }
        for(i <- index to matrix.length-1){
            //println("checking " + i)
            if(!visited.contains(i) && i != index && matrix(i).contains(index)){
                circle += i
                visited += i
                if(matrix(i).toList.filter(!visited.contains(_)).length > 0)
                    findCircles(matrix, visited, circle, i)
            }
        }
    }    
 
    def main(args: Array[String]) {
        
        val lines = io.Source.stdin.getLines
        val N = lines.next.toInt
        val M = lines.next.toInt
        
        var matrix = new Array[HashSet[Int]](N)
        for(i<-1 to N){
            matrix(i-1) = new HashSet[Int]()
            matrix(i-1) += i
    
        }
        
        for(i<-1 to M){
            val pair = lines.next.split(" ").toList.map(_.toInt).sorted
            matrix(pair(0)-1) += pair(1)-1
            matrix(pair(1)-1) += pair(0)-1
        }
        
        for(i <- matrix){
            //println(i.toList)
        }
        
        val circles = new ArrayBuffer[HashSet[Int]]()    
        var visited = new HashSet[Int]()
        for(i <- 0 to N-1){
            if(!visited.contains(i)){
                visited += i
                var h = new HashSet[Int]()
                h += i
                //println("finding circle for " + i)
                findCircles(matrix, visited, h, i)
                circles += h
                //println(h.toList)
            }
        }
        var cost = 0
        for(i <- circles.toList){
            //println(i.toList)
            var k = Math.sqrt(i.size)
            if(k.toInt * k.toInt   < i.size)
               k = k.toInt + 1
            cost += k.toInt 
        }
        println(cost)
    }
}
