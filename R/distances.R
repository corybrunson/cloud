#' Distances between pairs of points.
#' 
#' This function calculates the Euclidean distances between pairs of points.
#' @param x An m-by-n matrix of the n coordinates of m first points.
#' @param y An m-by-n matrix of the n coordinates of m second points.
#' @export

distances <-
    function(x, y, ...) {
        
        # Difference matrix
        sq_dists <- (x - y) ^ 2
        
        # Square roots of row sums
        sqrt(rowSums(sq_dists))
    }
