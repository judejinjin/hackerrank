object Solution {

    
    def rotate(i: Int, j:Int, r:Int, matrix:Array[Array[Int]], M:Int, N: Int, x:Int):Int={
         var circum = 0
         //println(i+":"+j)
             var nth = 0
             if(i > M-(i+1)){
                 nth = M-(i+1)
             }else{
                 nth = i
             }
             if(j > N - (j+1)){
                 if(N-(j+1) < nth)
                     nth = N-(j+1)
             }else{
                 if(j < nth)
                     nth = j
             }
        
        var row = M-nth*2
        var col = N-nth*2
            
        circum = row*2 + (col-2)*2 
        
        var rth = r % circum
        
        var i_nth = i - nth
        var j_nth = j - nth
            
        //find rth after (i_nth, j_nth)
        /*
        var index = 0    
        if(i_nth == row-1){
            index = row + col -2 + ( col -1 - j_nth)
        }else if(j_nth==0 && i_nth != 0){
            index = j_nth + (row-i_nth) + (col-1)*2 + row - 2 
        }else{
            index = j_nth + i_nth
        }
        */
        var rth_i = 0
        var rth_j = 0
        var shift = 0
        if(i_nth == 0){
            shift += col - j_nth - 1 
            if(rth <= shift){
                rth_i = i_nth + nth
                rth_j = j_nth + rth + nth
                return matrix(rth_i)(rth_j)
            }else{
                if((rth - shift) < row){
                    rth_j = col + nth - 1
                    rth_i = rth - shift + nth
                    return matrix(rth_i)(rth_j)
                }else{
                    if((rth - shift) < (row + col-2)){
                        rth_i = row - 1 + nth
                        rth_j = col - (rth - shift - (row - 1)) + nth - 1
                        return matrix(rth_i)(rth_j)    
                    }else if((rth - shift) <= ( row + col + row - 3)){
                        rth_j = nth
                        rth_i = row - 1 - (rth - shift - row - col + 2) + nth
                        return matrix(rth_i)(rth_j)    
                    }else{
                        if( j_nth != 0){
                            rth_i = nth 
                            rth_j = rth - shift - (row + col + row - 3) + nth
                            return matrix(rth_i)(rth_j)  
                        }else{
                            rth_i = nth
                            rth_j = rth - shift - (row + row + col - 3) + nth
                            return matrix(rth_i)(rth_j)  
                        }
                    }
                }
            }
        }else if(i_nth == row - 1){
            shift += j_nth
            if(rth <= shift){
                rth_i = i_nth + nth
                rth_j = j_nth - rth + nth
                return matrix(rth_i)(rth_j)  
            }else{
                if((rth - shift) < row){
                    rth_j = nth
                    rth_i = i_nth - (rth - shift) + nth
                    return matrix(rth_i)(rth_j)
                }else{
                    if((rth - shift) < (row + col-2)){
                        rth_i = nth
                        rth_j = rth - shift - row + nth + 1
                        return matrix(rth_i)(rth_j)
                    }else if((rth-shift) <= (row + col + row - 3)){
                        rth_j = nth + col - 1
                        rth_i = rth - shift - row - col + 2 + nth
                        return matrix(rth_i)(rth_j)    
                    }else{
                        if(j_nth != col - 1){
                            rth_i = row - 1 + nth
                            rth_j = col-1-(rth - shift - (row + col+row - 3)) + nth
                            return matrix(rth_i)(rth_j)   
                        }else{
                            rth_i = row - 1 + nth
                            rth_j = rth - shift - (row + col+row - 3) + j_nth + nth
                            return matrix(rth_i)(rth_j)   
                        }
                    }
                }
            }  
        }else{
            if(j_nth == 0){
                shift += i_nth
                if(rth <= shift){
                    rth_i = i_nth - rth + nth
                    rth_j = nth
                    return matrix(rth_i)(rth_j)
                }else{
                    if((rth - shift) < col){
                        rth_j = rth - shift + nth
                        rth_i = nth
                        return matrix(rth_i)(rth_j)
                    }else{
                        if((rth - shift) < (row + col - 2)){
                            rth_j = col - 1 + nth
                            rth_i = rth - shift - col + nth + 1
                            return matrix(rth_i)(rth_j)    
                        }else if((rth - shift) <= (row+col+col-3)){
                            rth_i = row - 1 + nth
                            rth_j = col - 1 - (rth - shift - (row+col-2)) + nth
                            return matrix(rth_i)(rth_j)
                        }else{
                            if(i_nth != (row - 1)){
                                rth_i = row - 1 - (rth - shift - (row + col+col - 3))  + nth
                                rth_j = j_nth + nth
                                return matrix(rth_i)(rth_j)      
                            }else{
                                rth_i = row - 1 + nth
                                rth_j = rth - shift - (row + col+col - 3) + j_nth + nth
                                return matrix(rth_i)(rth_j)     
                            }
                        }
                    }
                }    
            }else{
                shift += row - i_nth - 1
                if(rth <= shift){
                    rth_i = i_nth +  rth + nth
                    rth_j = nth + col - 1
                    return matrix(rth_i)(rth_j)
                }else{
                    if((rth - shift) < col){
                        rth_j = col- (rth - shift) + nth - 1
                        rth_i = row - 1 + nth
                        return matrix(rth_i)(rth_j)
                    }else{
                        if((rth - shift) < (row + col - 2)){
                            rth_j = nth
                            rth_i = row-(rth - shift - col+1) + nth - 1
                            return matrix(rth_i)(rth_j)    
                        }else if((rth - shift) <=(row + col + col - 3)){
                            
                            rth_i = nth
                            rth_j = rth - shift - col - row + 2 + nth
                            return matrix(rth_i)(rth_j)      
                        }else{
                            if(i_nth != 0){
                                rth_i = rth - shift - col - row - col + 3 + nth
                                rth_j = j_nth + nth
                                return matrix(rth_i)(rth_j)    
                            }else{
                                rth_i = nth
                                rth_j = col - 2 + nth
                                return matrix(rth_i)(rth_j) 
                            }
                        }
                    }
                }    
            }
        }
        
            
        //return circum
    }
    
    def main(args: Array[String]) {
        /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution
*/
        val lines = io.Source.stdin.getLines
        val numbers = lines.next.split(" ").toList.map(_.toInt)
        val M = numbers(0)
        val N = numbers(1)
        val R = numbers(2)
        val matrix = new Array[Array[Int]](M)
        for(i<- 0 to M-1){
            matrix(i) = lines.next.split(" ").toList.map(_.toInt).toArray[Int]
        }
        var x = M/2
        if(M > N){
            x = N/2
        }
       
        for(i<- 0 to M-1){
            var row = new Array[Int](N)
            for(j <- 0 to N-1){
                row(j) = rotate(i, j, R, matrix, M, N, x)   
            }
            println(row.mkString(" "))
        }
    }
}