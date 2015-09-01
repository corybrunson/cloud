#' Distance between two points.
#' 
#' This function calculates the Euclidean distance between two points.
#' @param x The n coordinates of one point.
#' @param y The n coordinates of the other point.
#' @export

distance <-
    function(x, y, ...) {
        
        # Formula
        sqrt(sum((x - y) ^ 2))
    }
