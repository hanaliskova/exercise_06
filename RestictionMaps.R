#install.packages("gtools")
library(gtools)


DoubleDigestProblem <- function(fragment_A, fragment_B, fragment_AB){
   position_map_A <- c(0, cumsum(fragment_A))
   position_map_B <- c(0, cumsum(fragment_B))
   combined_position_AB <- unique(sort(c(position_map_A, position_map_B)))
   sort_diff_from_combined <- sort(diff(combined_position_AB))
   
   if (identical(sort_diff_from_combined, fragment_AB)){
     return(list(position_A = position_map_A[2:((length(position_map_A))-1)],
                 position_B = position_map_B[2:((length(position_map_B))-1)]))
   }
   else{
     return(NULL)
   }
}

DoubleDigestSolver <- function(fragment_A, fragment_B, fragment_AB){
  perms_A <- permutations(length(fragment_A), length(fragment_A), fragment_A)
  perms_B <-  permutations(length(fragment_B), length(fragment_B), fragment_B)

  solutions <- list()
  
  for (i in 1:nrow(perms_A)) {
    for (j in 1:nrow(perms_B)) {
      result <- DoubleDigestProblem(perms_A[i, ], perms_B[j, ], fragment_AB)
      if (!is.null(result)) {
        solutions <- append(solutions, list(result))
      }
    }
  }
  if (length(solutions) == 0) {
    return("No valid arrangement found.")
  } else {
    return(solutions)
  }
}



fragment_A <- c(2,3,5,10)
fragment_B <- c(3,7,10)
fragment_AB <- c(1,2,2,5,5,5)

DoubleDigestSolver(fragment_A, fragment_B, fragment_AB)
