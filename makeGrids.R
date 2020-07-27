# References --------------------------------------------------------------
# http://3dmdesign.com/development/hexmap-coordinates-the-easy-way
# http://keekerdc.com/2011/03/hexagon-grids-coordinate-systems-and-distance-calculations/
# http://answers.unity3d.com/questions/1128820/how-to-generate-list-of-hexagonal-coordinates.html
# https://tbswithunity3d.wordpress.com/2012/02/21/hexagonal-grid-generating-the-grid/
# http://catlikecoding.com/unity/tutorials/hex-map-1/
# http://statmath.wu.ac.at/research/friday/resources_WS0708_SS08/igraph.pdf
# http://www.redblobgames.com/grids/hexagons/

# Distances ---------------------------------------------------------------
rm(list=ls())

euc.dist <- function(x1, x2) {
  sqrt(sum((x1 - x2) ^ 2))
}

# euc.dist(grid[["(0,0)"]], grid[["(0,1)"]])

# Distance is equal to the greatest of the absolute values of: 
#   *the difference along the x-axis
#   *the difference along the y-axis
#   *or the difference of these two differences.
# see: http://3dmdesign.com/development/hexmap-coordinates-the-easy-way

hex.dist <- function(v1, v2) {
  deltaX <- v2[1] - v1[1]
  deltaY <- v2[2] - v1[2]
  deltaD <- deltaY - deltaX
  deltas <- abs(c(deltaX, deltaY, deltaD))
  D <- max(deltas)
  return(D)
}

point <- c(1,1)
neighbours <- list()
neighbours[[1]] <- c(0,0)
neighbours[[2]] <- c(1,0)
neighbours[[3]] <- c(2,1)
neighbours[[4]] <- c(2,2)
neighbours[[5]] <- c(1,2)
neighbours[[6]] <- c(0,1)

for(k in 1:length(neighbours)) {
  cat(neighbours[[k]]); cat(": ");
  cat(hex.dist(point, neighbours[[k]]))
  cat("\n")
}

# TRANSFORMATION!!!!! Hexagonal To Cartesian !!!!!
hex_to_geo <- function(r, vHex) {
  # vHex <- c(2,2)
  # r <- 1
  vGeo <- numeric(2)
  vGeo[2] <- 2 * r * vHex[1] * sin(pi/3)
  vGeo[1] <- 2 * r * vHex[1] * cos(pi/3) + (2 * r) * vHex[1]
  return(vGeo)
}

# hex_to_geo(int xh, int yh, double r, double *xg, double *yg)
# {
#   *yg = (2.0 * r) * xh * (sqrt(3)/2.0);
#   *xg = (2.0 * r) * xh * (1.0/2.0) + (2.0 * r) * yh;
# }


# REFERENCES:
if(FALSE) {
  #DISTANCE:
  # https://groups.google.com/forum/#!topic/rec.games.design/-5n_Km1SqWc
  # m...@nowhere.edu skribis:
  #   > I've created a hex grid bitmap, and have assigned each hex a
  # > coordinate exactly as portrayed on this page,
  # > http://www.gamedev.net/reference/articles/article1800.asp and am now
  # > trying to nail down the algorithm to determine the distance between
  # > any two hexes when the coordinates are known.
  # I worked this out a few decades ago, and I believe my answer was even
  # published in some gaming magazine - one of SPI's, as I recall.
  # 
  # To make it easier, I had non-staggering coördinates, so that with
  # coördinates of [x,y], all hexes with the same value of "y" were in the
  # same horizontal row, and all with the same value of "x" were in the
  # same diagonal row.
  # 
  # [0,0]     [0,1]     [0,2]     [0,3]     [0,4]     [0,5]
  #       [1,1]     [1,2}     [1,3]     [1,4]     [1,5]     [1,6]
  # [2,1]     [2,2]     [2,3]     [2,4]     [2,5]     [2,6]
  #       [3,2]     [3,3]     [3,4]     [3,5]     [3,6]     [3,7]
  # [4,2]     [4,3]     [4,4]     [4,5]     [4,6]     [4,7]
  #       [5,3]     [5,4]     [5,5]     [5,6]     [5,7]     [5,6]
  # 
  # Now to calculate the hex-distance between two hexes, calculate the
  # difference between their x-coördinates, the difference between their y-
  #   coördinates, and the difference between these two differences; take
  # the absolute value of each of these three numbers, and the distance is
  # the largest of these three values.
  # 
  # So -- for the distrance from [5,5] to [3,6],  x = -2,  y = 1, and the
  # difference between the two deltas is 3.  The greatest of 2, 1, and 3
  # is three, and Bob's your uncle.
  # 
  # --
  # Steve
  # 299,792,358 m/s -- it's not just a good idea; it's the LAW!

  
  # COORDINATE CONVERSION:
  # http://www3.cs.stonybrook.edu/~skiena/392/lectures/week12/
  # To convert between triangular/hexagonal coordinates and geometrical coordinates, we assume that the origin of both systems is the center of a disk at $(0,0)$.
  # 
  # The hexagonal coordinate (xh,yh) refers to the center of the disk on the horizontal row xh and diagonal column yh. The geometric coordinate of such a point is a function of the radius of the disk $r$, half that of the diameter $d$ described in the previous section:
  #   
  #   
  #   hex_to_geo(int xh, int yh, double r, double *xg, double *yg)
  # {
  #   *yg = (2.0 * r) * xh * (sqrt(3)/2.0);
  #   *xg = (2.0 * r) * xh * (1.0/2.0) + (2.0 * r) * yh;
  #   }
  # 
  # geo_to_hex(double xg, double yg, double r, double *xh, double *yh)
  # {
  #   *xh = (2.0/sqrt(3)) * yg / (2.0 * r);
  #   *yh = (xg - (2.0 * r) * (*xh) * (1.0/2.0) ) / (2.0 * r);
  # }
  
  
  # http://stackoverflow.com/questions/7705228/hexagonal-grids-how-do-you-find-which-hexagon-a-point-is-in
  
  }
  

# Set Stuff ---------------------------------------------------------------


rm(list=ls())

grid <- list()

# make ixj cartesian grid
# NxM
N <- 5
M <- 5

for(i in 1:N) {
  for(j in 1:M) {
    pointName <- paste("(", paste(i-1,j-1,sep = ","), ")",sep = "")
    grid[[pointName]] <- c(i-1,j-1) 
  }
}

scaleXY <- c(1,1)
# add scaling on both x and y axes. simply multiply every element
for(k in 1:length(grid)) {
  grid[[k]] <- scaleXY * grid[[k]]
}

# make an hexagonal grid

# Method 1 ----------------------------------------------------------------


is.odd <- function(x) {
  if(!is.integer(x)) {
    stop("Input must be integer!")
  }
  x %% 2 != 0
}

is.even <- function(x) {
  if(!is.integer(x)) {
  stop("Input must be integer!")
  }
  x %% 2 == 0
}

# displace odd rows with odd Y axis value up and right
hexGrid <- grid
hexGridPos <- grid 
deltaOdd <- c(1,1) - c(cos(pi/3), sin(pi/3)) # dx,dy displacements
deltaOdd <- c(1,-1) * deltaOdd # move down vertically (y) and right horizontally (x)

deltaEven <- c(0,-1) * deltaOdd # move down vertically (y) and right horizontally (x)

# for(k in 1:6) {
#   if(is.odd(as.integer(hexGrid[[k]][2]))) {
#     hexGrid[[k]] <- hexGrid[[k]] + delta
#   }
# }

for(k in 1:length(hexGrid)) {
  if(hexGridPos[[k]][2] == 0) {
    next
  }
  if(is.odd(as.integer(hexGridPos[[k]][2]))) {
    hexGrid[[k]] <- hexGrid[[k]] + deltaOdd
  }
  if(is.even(as.integer(hexGridPos[[k]][2]))) {
    hexGrid[[k]] <- hexGrid[[k]] + deltaEven
  }
}


# for(k in 1:length(hexGrid)) {
#   if(is.odd(as.integer(hexGrid[[k]][2]))) {
#     hexGrid[[k]] <- hexGrid[[k]] + delta
#   }
# }

euc.dist <- function(x1, x2) { sqrt(sum((x1 - x2) ^ 2)) }


for(k in 1:length(hexGrid)) {
  if( euc.dist(hexGrid[["(3,3)"]], hexGrid[[k]]) ==  1) {
    #cat(hexGrid[[k]]); cat("\n");
    cat(grid[[k]]); cat("\n");
  }
}

euc.dist <- function(x1, x2) { sqrt(sum((x1 - x2) ^ 2)) }
# neighbours of (0,0)
euc.dist(hexGrid[["(0,0)"]], hexGrid[["(0,1)"]]) # OK
euc.dist(hexGrid[["(0,0)"]], hexGrid[["(1,0)"]]) # OK


# neighbours of (1,1)
euc.dist(hexGrid[["(1,1)"]], hexGrid[["(1,0)"]]) == 1# OK
euc.dist(hexGrid[["(1,1)"]], hexGrid[["(2,1)"]])
euc.dist(hexGrid[["(1,1)"]], hexGrid[["(1,2)"]])
euc.dist(hexGrid[["(1,1)"]], hexGrid[["(0,2)"]])

# Method 2 ----------------------------------------------------------------

deltaTheta <- pi /3  # k <- 2 * pi / 6 # for N-multidimensional and other grids (eg, octo, deca - gonal) extensions
Nsteps <- 6
steps <- 0:(Nsteps-1)
theta <- steps * deltaTheta # six thetas generated
r <- 1 # define radius
startPoint <- c(0,0)

neighbours <- list()
for(k in 1:Nsteps) {
  neighbours[[k]] <- c(startPoint[1] + r*cos(theta[k]), startPoint[2] + r*sin(theta[k]))
}

euc.dist <- function(x1, x2) { sqrt(sum((x1 - x2) ^ 2)) }
for(k in 1:length(neighbours)) {
  cat( euc.dist(startPoint, neighbours[[k]]) )
}

# makeNeighbours <- function(r, theta) {}

startPoint2 <- c(0.5,0.866)
neighbours2 <- list()
for(k in 1:Nsteps) {
   neighbours2[[k]] <- c(startPoint2[1] + r*cos(theta[k]), startPoint2[2] + r*sin(theta[k]))
}
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
for(k in 1:length(neighbours2)) {
     cat( euc.dist(startPoint2, neighbours2[[k]]) )
}


startPoint3 <- c(1,1.73)
neighbours3 <- list()
for(k in 1:Nsteps) {
  neighbours3[[k]] <- c(startPoint3[1] + r*cos(theta[k]), startPoint3[2] + r*sin(theta[k]))
}
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
for(k in 1:length(neighbours2)) {
  cat( euc.dist(startPoint3, neighbours3[[k]]) )
}

# make a graph, using igraph




# # distances:
# # this
# euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
# euc.dist(grid[["0,0"]], grid[["2,2"]])
# euc.dist(grid[["1,1"]], grid[["2,2"]])
# euc.dist(grid[["0,0"]], grid[["1,0"]])
# 
# # or this
# dist(x = rbind(grid[["0,0"]], grid[["1,0"]]), 
#      method = "euclidian")
# dist(x = rbind(grid[["0,0"]],grid[["2,2"]]), 
#      method = "euclidian")



# IDEAS:
# 1)
# -> start by picking an initial point, eg, (0,0)
# -> generate neighbours by shifting in 6 directions by an angle theta (pi/3). can start horizontally
# or sloped. c(startPoint[1] + r*cos(theta[k]), startPoint[2] + r*sin(theta[k]))
# -> best is to use a list of list, eg, a list of points [(0,0), (0,1), (2,2), etc...]
# for point (0,0) , for instance, it will be assigned a list of neighbours. one can filter the 
# relevat ones (in the 1st quadrant, for instance <-> positive cooordinates)
# pick the neighbours one by one, assign the to the main list, and find their neighbours too.
# repeat the process.
# -> use out of bound filtering for stopping condition. have a limit for up and right number
# of objects in the main list. grid 3x4. 
#
#     * * * *       
#     * * * *
#     * * * *
# something like this:
#     * * * *       
#       * * * *
#     * * * *
# -> assign names to the lists
# for instance, starting point is (0,0), the neighbor to the right (1, 0)
# or simply their coordinates rounded to 2 decimals
# -> for simplicity, just keep on generating non repeated points within an arbitrary radius
# (while within radius, keep on...). when finished, filter the wanted ones. :)

# 2)
# -> given, for instance, the (0,0) point, find create a line os point at the right, spaced equally (say 1.)
# then, find the coordinates of the point on the row above (sines, cosines, etc). it will be at distance 1 from 
# the source point. save the x and y. points on that row will have the same y, but shift to the left by 1, say.
# so on and so forth...

###########################
### OTHER RELATED STUFF ###
###########################

# hexbin library ----------------------------------------------------------
# http://stackoverflow.com/questions/24839423/create-hexagonal-cells-grid-using-lat-lon-coordinates

rm(list=ls())

library(hexbin)
X<-seq(-10,20,by=0.1) # create coordinates vectors X and Y
Y<-seq(35,65,by=0.1)

coords <- hcell2xy( hexbin(x=X,y=Y))
head(coords)

euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

distances <- numeric(0)
for(k in 1:(length(coords$x)-1)) {
  distances[k] <- euc.dist(c(coords$x[k],coords$y[k]), c(coords$x[k+1],coords$y[k+1]))
}

# hexbin2 -----------------------------------------------------------------

rm(list=ls())

library(hexbin)
x <- seq(-10,20,by=0.1) # create coordinates vectors X and Y
y <- seq(35,65,by=0.1)

coords <- hcell2xy( hexbin(x = x, y = y))
head(coords)

euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

distances <- numeric(0)
for(k in 1:(length(coords$x)-1)) {
  distances[k] <- euc.dist(c(coords$x[k],coords$y[k]), c(coords$x[k+1],coords$y[k+1]))
}

# hexbin3 -----------------------------------------------------------------

rm(list=ls())

library(hexbin)
x <- seq(0, 10, by = 0.1) # create coordinates vectors X and Y
y <- seq(0, 10, by = 0.1)

coords <- hcell2xy( hexbin(x = x, y = y))
head(coords)

euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

distances <- numeric(0)
for(k in 1:(length(coords$x)-1)) {
  distances[k] <- euc.dist(c(coords$x[k],coords$y[k]), c(coords$x[k+1],coords$y[k+1]))
}

unique(round(x = distances, digits = 2))

library(igraph)
bsk.network<-graph.data.frame(coords, directed=F)

V(bsk.network) #prints the list of vertices (people)
E(bsk.network) #prints the list of edges (relationships)
degree(bsk.network) #print the number of edges per vertex (relationships per people)

plot(bsk.network)

# Ideas -------------------------------------------------------------------
# IDEA:
# initialize a cartesian or hexagonal grid with a large number of points
# e.g., a buffer, and the exclude the non required. just for fun. not really needed :P