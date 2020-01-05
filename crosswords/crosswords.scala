object Solution {
    def fillH(g:Array[String], y:Int, x:Int, w:String): Array[String] = {
        var results = new Array[String](g.length)
        for(i<- 0 to g.length-1)
            results(i) = g(i)
            
        for(i <- 0 to w.length-1){
            val len = results(y).length;
         
            if( x + w.length < results(y).length){
                results(y) = results(y).substring(0, x) + w + results(y).substring(x+w.length, results(y).length);
            }else{
                results(y) = results(y).substring(0, x) + w;
            }
        }
        return results;    
    }
    
    def fillV(g:Array[String], y:Int, x:Int, w:String): Array[String] = {
        var results = new Array[String](g.length)
        for(i<- 0 to g.length-1)
            results(i) = g(i)
        
        for(i <- 0 to w.length-1){
            if(x + 1 < results(y+i).length){
                results(y+i) = results(y+i).substring(0, x) + w.substring(i,i+1) + results(y+i).substring(x+1, results(y+i).length)
            }else{
                results(y+i) = results(y+i).substring(0, x) + w.substring(i,i+1);
            }
        }
        
        return results;
    }
    
    def fitH(g: Array[String], y:Int, x:Int, w: String): Boolean = {
        var fit = true
//        println("fitH: " + y + "-" + x + ":" + w);            
        var counter = 0;
        if(w.length+x <= g(y).length){

            for(i <- x to g(y).length-1 ){
                if(counter < w.length && (g(y).charAt(i) == '-' || g(y).charAt(i) == w.charAt(counter)))
                    counter += 1;
                else{
                    if(counter < w.length)
                        fit = false;
                }
            }
        }
	if(counter < w.length)
		   fit = false;
        return fit;
    }
    
    def fitV(g:Array[String], y:Int, x:Int, w: String): Boolean = {
        var fit = true
  //      println("fitV: " + y + "-" + x + ":" + w);            
        var counter = 0;
        if(w.length+y <= g.length){

            for(i <- y to g.length-1 ){
                if(counter < w.length && (g(i).charAt(x) == '-' || g(i).charAt(x) == w.charAt(counter)))
                    counter += 1;
                else{
                    if(counter < w.length)
                        fit = false;
                }
            }
        }
	if(counter < w.length)
		   fit = false;
        return fit;
    }
    
    def fitOnGrids(grids: List[Array[String]], w: String): List[Array[String]] = {
        var results = List[Array[String]]()
        
        for(g <- grids){
            for(i <- 0 to g.length-1){
                for(j <- 0 to g(i).length-1){
		    if(g(i).length - j >= w.length){
                        if(fitH(g, i, j, w)){
                            results = results ++ List(fillH(g, i, j, w))
                        }  
		    }
                    if(g.length - i >= w.length){
                        if(fitV(g, i, j, w)){
                            results = results ++ List(fillV(g, i, j, w))
                        }
		    }
                }
            }
            
        }
        
        return results
    }
    
    def findFit(grid: Array[String], words: List[String]) : Array[String]={
        var grids = List[Array[String]](grid)
            
        for(i <- words){
            grids = fitOnGrids(grids, i)
/*
	    println(i + ":" + grids.length);
	    for(g <- grids){
	       for(row <- g){
	           println(row);
	       }
	       println(" ");
	       println(" ");

	    }
*/
        }
        
        for(g <- grids){
            var good = true
            for(row <- g){
                if(row.indexOf('-') >=0){
                    good = false
                }
            }
            if(good)
                return g
        }
//        println("no solution")
        return grid
    }
    
    def main(args: Array[String]) {
        /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution
*/
        val lines = io.Source.stdin.getLines.toList
        val grid = lines.take(10).toArray[String]
        val words = lines.takeRight(1)(0).split(";").toList
            
        /*for(i <- grid)
            println(i)
        println(words)
        */
        val solution = findFit(grid, words)
        for(row <- solution)
            println(row)
            
    
    }
}