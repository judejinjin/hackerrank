object Solution {
    import scala.math._
    val ERROR: Double = pow(10, -10)
    var flag = false
    
    def findR(sides:Array[Int]) : Double = {

        var e : Double = 0
	var max = sides.max
        var minR:Double = sides.max / 2.0
        var maxR:Double = sides.sum / 4.0

	System.err.println("sum: " + sides.sum)
	System.err.println("min " + minR + " max " + maxR);
	
	var r:Double = maxR
        do{
            var sum : Double = 0
            for(i <- sides){
                sum += 2 * asin(i/(2.0*r))
            }
            e = abs(2*Pi - sum)

	    System.err.println("total:" + sum + " error:" + e + " R:" + r+ " maxR:" + maxR + " minR:" + minR)            
            if(sum>2*Pi){
    		minR = r
                r = r + (maxR-r) / 2
            }else{
		maxR = r
                r = r - (r-minR) / 2
		if(r < minR)
		   r = minR
            }

	    System.err.println("after update:"  + " R:" + r)
	    
        }while(e > ERROR && r-minR != 0 )

	if(e > ERROR){

	flag = true
	System.err.println("take 2")
	
	maxR = sides.sum-sides.max

	minR = sides.max / 2.0
	
	r = minR

	System.err.println("min " + minR + " max " + maxR);
        do{
            var sum : Double = 0
            for(i <- sides){
	        if(i == max)
		                sum += (2*Pi - 2 * asin(i/(2.0*r)))
	        else
		                sum += 2 * asin(i/(2.0*r))
            }
            e = abs(2*Pi - sum)

	    System.err.println("total:" + sum + " error:" + e + " R:" + r+ " maxR:" + maxR + " minR:" + minR)

            if(sum<2*Pi){
    		minR = r
                r = r + (maxR-r) / 2
            }else{
		maxR = r
                r = r - (r-minR) / 2
		if(r < minR)
		   r = minR
            }
	    
	    System.err.println("after update:"  + " R:" + r)
	    
        }while(e > ERROR)

	}
	System.err.println("returning" + r)	     
        return r
    }
    
    def main(args: Array[String]) {
        /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution
*/
        val lines = io.Source.stdin.getLines
        val N = lines.next.toInt
        val sides = lines.next.split(" ").map(_.toInt).toArray[Int]
            
        val R = findR(sides)
        
        val y:Double = sides(0)/2.0
        val x:Double = sqrt(R*R - y*y)
        
        //println("Radius:" + R + " x:" + x + " y:" + y)    
       
        val format = "%1.10f"
        println(format.format(0.0))
        println(format.format(0.0))
        println("")
        println(format.format(0.0))
        println(format.format(sides(0).toDouble))
        println("")
        
	var angleAccum:Double = asin(sides(0)/2.0/R)
	var max = sides.max

	for(i <- 1 to sides.length-2){
            var angle:Double = 2 * asin(sides(i)/2.0/R)
	    
	    if(java.lang.Double.isNaN(angle)){
	        System.err.println(i + " side : " + sides(i) + " -- :" + angleAccum + " R: " + R + " " + sides(i)/2.0/R + " " + asin(sides(i)/2.0/R))
	    }
	    if(flag && sides(i) == max){
	       angle = 2.0*Pi- 2.0 * asin(sides(i)/2.0/R)
	       System.err.println("max:" + angle/Pi * 180)
	    }

	    angleAccum += angle
            var cosSide:Double = cos(angleAccum)*R
            var sinSide:Double = sin(angleAccum)*R

	    System.err.println(angleAccum / Pi * 180.0)
            println(format.format(x - cosSide))
            println(format.format(y + sinSide))
            println("")
         }

	angleAccum += 2 * asin(sides(sides.length-1)/2.0/R) + asin(sides(0)/2.0/R)
	System.err.println("total:" + angleAccum)
    }
}