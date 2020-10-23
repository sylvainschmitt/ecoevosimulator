#' @importFrom stats na.omit rnorm
NULL

#' Topography generator with the square-diamond alogirthm
#'
#' @param grid int. Matrix grid size.
#' @param rudgeness double. Rugedness parameter
#'
#' @return Environmental matrix of size grid x grid for the simulator
#' 
#' @export
#'
#' @examples
#' squareDiamondTopography(5,1)
#' 
squareDiamondTopography <- function(
  grid = 20,
  rudgeness = 1 # rudgeness
){
  # init
  size <- 2^ceiling(log2(grid-1))+1
  # size <- 2^n+1
  M <- matrix(NA, nrow = size, ncol = size)
  for(x in c(1,size))
    for(y in c(1,size))
      M[x,y] <- rudgeness*rnorm(1)
  hs <- size - 1
  # loop
  while(hs > 1){
    s <- hs
    hs <- hs/2
    for(x in seq(from = hs + 1, to = size - 1, by = s))
      for(y in seq(from = hs + 1, to = size - 1, by = s))
        M  <- squarediamond(M, x, y, hs, rudgeness) 
  }
  return(M[1:grid, 1:grid])
}

#' Square-Diamond algorithm step
#'
#' @param M matrix. Environmental matrix
#' @param x int. Row
#' @param y int. Col
#' @param hs int. half-step
#' @param h double. Rudgeness
#'
#' @return Environmental matrix of size grid x grid
#' 
#' @export
squarediamond <- function(M, x, y, hs, h = 1){
  M  <- square(M, x, y, hs, h) 
  M <- diamond(M, x - hs, y, hs, h)
  M <- diamond(M, x + hs, y, hs, h)
  M <- diamond(M, x, y - hs, hs)
  M <- diamond(M, x, y + hs, hs, h)
  return(M)
}

#' Square step from the Square-Diamond algorithm
#'
#' @param M matrix. Environmental matrix
#' @param x int. Row
#' @param y int. Col
#' @param hs int. half-step
#' @param h double. Rudgeness
#'
#' @return Environmental matrix of size grid x grid
#' 
#' @export
square <- function(M, x, y, hs, h = 1){
  # a   b
  #   x
  # c   d
  a <- sampleM(M, x-hs, y-hs)
  b <- sampleM(M, x-hs, y+hs)
  c <- sampleM(M, x+hs, y-hs)
  d <- sampleM(M, x+hs, y+hs)
  M[x, y] <- (a+b+c+d)/4 + rnorm(1)*h
  return(M)
}

#' Diamond step from the Square-Diamond algorithm
#'
#' @param M matrix. Environmental matrix
#' @param x int. Row
#' @param y int. Col
#' @param hs int. half-step
#' @param h double. Rudgeness
#'
#' @return Environmental matrix of size grid x grid
#' 
#' @export
diamond <- function(M, x, y, hs, h = 1){
  #   a 
  # d x b
  #   c
  a <- sampleM(M, x - hs, y)
  b <- sampleM(M, x, y + hs)
  c <- sampleM(M, x + hs, y)
  d <- sampleM(M, x, y - hs)
  v <- na.omit(c(a,b,c,d))
  M[x, y] <- sum(v)/length(v) + rnorm(1)*h
  return(M)
}

#' Values sampling for the Square-Diamond algorithm
#' 
#' Special sampler of matrix returning NA if coordinates don't exists in the matrix
#'
#' @param M matrix. Environmental matrix
#' @param x int. Row
#' @param y int. Col
#'
#' @return Environmental matrix of size grid x grid
#' 
#' @export
sampleM <- function(M, x, y){
  if(x > 0 & x < nrow(M) + 1 & y > 0 & y < ncol(M) + 1)
    return(M[x,y])
  else
    return(NA)
}
