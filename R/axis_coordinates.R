#' Variables attached to an axis
#' 
#' The following functions inpute a point cloud (as a matrix of coordinates) and
#' an axis (as a direction vector), and return the coordinates of a desired
#' variable with respect to the cloud attached to the axis.
#' @param cloud An m-by-n matrix of coordinates for m points in n-dimensional
#'   space.
#' @param dir_vec An n-dimensional vector giving the direction of the axis
#'   through the cloud.
#' @param variable Character; which variable to calculate, from among
#'   \code{covariant}, \code{calibrated}, \code{standard}, and \code{axial}.
#'   Does not default.
#' @export

axis_coordinates <-
    function(cloud, dir_vec, variable) {
        
        # Match variable
        variable <- match.arg(variable, c("covariant", "calibrated",
                                          "standard", "axial"))
        
        # Barycenter
        bc <- barycenter(cloud)
        
        # Barycentered cloud
        centered_cloud <- cloud - rep(bc, each = nrow(cloud))
        
        # Inner product with direction vector
        covariant_var <- centered_cloud %*% as.vector(dir_vec)
        if (variable == "covariant") return(covariant_var)
        
        # Magnitude of dir_vec
        mag <- sqrt(sum(dir_vec * dir_vec))
        
        if (variable == "axial") {
            covariant_var / (mag ^ 2)
        } else {
            calibrated_var <- covariant_var / mag
            if (variable == "calibrated") {
                calibrated_var
            } else {
                calibrated_var / sd(calibrated_var)
            }
        }
    }

covariant_coordinates <-
    function(cloud, dir_vec) {
        axis_coordinates(cloud = cloud, dir_vec = dir_vec,
                         variable = "covariant")
    }

calibrated_coordinates <-
    function(cloud, dir_vec) {
        axis_coordinates(cloud = cloud, dir_vec = dir_vec,
                         variable = "calibrated")
    }

standard_coordinates <-
    function(cloud, dir_vec) {
        axis_coordinates(cloud = cloud, dir_vec = dir_vec,
                         variable = "standard")
    }

axial_coordinates <-
    function(cloud, dir_vec) {
        axis_coordinates(cloud = cloud, dir_vec = dir_vec,
                         variable = "axial")
    }
