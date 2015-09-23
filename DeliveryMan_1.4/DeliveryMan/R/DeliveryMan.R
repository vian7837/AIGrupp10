dumbDM=function(roads,car,packages){
  car$nextMove=sample(c(2,4,6,8),1)
  return (car)
}
basicDM=function(roads,car,packages) {
  nextMove=0
  toGo=0
  offset=0
  if (car$load==0) {
    toGo=which(packages[,5]==0)[1]
  } else {
    toGo=car$load  
    offset=2
  }
  if (car$x<packages[toGo,1+offset]) {nextMove=6}
  else if (car$x>packages[toGo,1+offset]) {nextMove=4}
  else if (car$y<packages[toGo,2+offset]) {nextMove=8}
  else if (car$y>packages[toGo,2+offset]) {nextMove=2}
  else {nextMove=5}
  car$nextMove=nextMove
  car$mem=list()
  return (car)
}
manualDM=function(roads,car,packages) {
  print(roads$hroads)
  print(roads$vroads)
  if (car$load>0) {
    #print(paste("Current load:",car$load))
    #print(paste("Destination: X",packages[car$load,3],"Y",packages[car$load,4]))
    
  } 
  nextPackage(car$x,car$y,packages)
  car$nextMove=readline("Enter next move. Valid moves are 2,4,6,8,0 (directions as on keypad) or q for quit.")
  if (car$nextMove=="q") {stop("Game terminated on user request.")}
  return (car)
}

gCalc = function(roads, parent,neighborX, neighborY) {
  parentX <- as.integer(parent[1])
  parentY <- as.integer(parent[2])
  neighborX <- as.integer(neighborX)
  neighborY <- as.integer(neighborY)
  if (parent[1] == neighborX) {
    #vertical move
    return (roads$vroads[min(parentY,neighborY), parentX] + parent$gCost)
  }
  else {
    #horizontal move
    return (roads$hroads[parentY,min(neighborX,parentX)] + parent$gCost)
  }
}

hCalc = function(neighborX, neighborY, xDest, yDest) {
  neighborX <- as.integer(neighborX)
  neighborY <- as.integer(neighborY)
  xDest <- as.integer(xDest)
  yDest <- as.integer(yDest)
  
  xCost <- abs(neighborX - xDest)
  yCost <- abs(as.integer(neighborY) - yDest)
  manhattanCost <- (xCost + yCost)
  return (manhattanCost)
}

fCalc = function(roads,parent,neighborX,neighborY,xDest,yDest) {
  fTotal <- (gCalc(roads,parent,as.integer(neighborX),as.integer(neighborY))+hCalc(as.integer(neighborX),as.integer(neighborY),as.integer(xDest),as.integer(yDest)))
  return (fTotal) 
}

isValid = function(x, y) {
  if (x < 1 || x > 10 || y > 10 || y < 1) {
    return (FALSE) 
  }
  else {
    return(TRUE)
    }
}

getNeighbors = function(x,y) {
  x <- as.integer(x)
  y <- as.integer(y)
  validNeighborList = list()
  left = list(list(x-1,y))
  right = list(list(x+1,y))
  down = list(list(x,y-1))
  up = list(list(x,y+1))
  
  if (isValid(left[[1]][1], left[[1]][2])) {
    validNeighborList <- append(validNeighborList,left)
  }
  if (isValid(right[[1]][1], right[[1]][2])) {
    validNeighborList <- append(validNeighborList,right)
  }
  if (isValid(down[[1]][1], down[[1]][2])) {
    validNeighborList <- append(validNeighborList,down)
  }
  if (isValid(up[[1]][1], up[[1]][2])) {
    validNeighborList <- append(validNeighborList,up)
  }
  return (validNeighborList)
}


nextPackage=function(x,y,packages) {
   hValue = Inf
   noOfPackages <- nrow(packages)
   bestNode <- list()
    for (i in 1:noOfPackages){
    if  (hValue > (abs(x - packages[i,1]) + abs(y-packages[i,2])) && (packages[i,5] != 2)){
      bestNode <- list(packages[i,1], packages[i,2])
      hValue = abs(x - packages[i,1]) + abs(y-packages[i,2])
    }
    }
  return(bestNode)
}

translator <- function(car,destx,desty) {
  destx <- as.integer(destx)
  desty <- as.integer(desty)
  if (car$x == destx && ((car$y - 1) == desty)) {
    return (2)
  }
  else if (car$x == destx && car$y == (desty - 1)) {
    return (8)
  }
  else if (car$x == (destx - 1) && car$y == desty) {
    return (6)
  }
  else if ((car$x - 1) == destx && car$y == desty) {
    return (4)
  }
  else (return (5))
}

existsInList <- function(element, list){
  value <- FALSE
  lengthOfList <- length(list)
  if (lengthOfList == 0) {
    return (FALSE)
  }
  for (i in 1:lengthOfList){
    if ((as.integer(element[1]) == as.integer(list[[i]][1])) && (as.integer(element[2]) == as.integer(list[[i]][2]))){
      value <- TRUE
    }
  }
  return (value)
}

superFGCalc <- function(parent, roads, list, destination) {
  lengthOfList <- length(list)
  for (i in 1:lengthOfList) {
    list[[i]]$fCost <- fCalc(roads, parent, list[[i]][1], list[[i]][2], destination[1], destination[2])
    list[[i]]$gCost <- gCalc(roads, parent, list[[i]][1], list[[i]][2])
  }
  return(list)
}

getLowestFNode <- function(nodeList, destination) {
  lengthOfList <- length(nodeList)
  bestNode <- list()
  indexForBestNode <- integer
  fTemp <- Inf
  for (i in 1:lengthOfList) {
    if (nodeList[[i]]$fCost < fTemp) {
      bestNode <- nodeList[[i]]
      fTemp <- nodeList[[i]]$fCost
      indexForBestNode <- i
    }
  }
  return (list(bestNode, indexForBestNode))
}

aStar <- function(car,roads, destination) {
  startElement <- list(car$x,car$y)
  startElement$fCost <- hCalc(car$x, car$y,destination[[1]], destination[[2]])
  startElement$gCost <- 0
  startElement$hCost <- hCalc(car$x, car$y,destination[[1]], destination[[2]])
  startElement$parent <- list(car$x,car$y)
  openList <- list()
  openList <- append(openList, list(startElement))
  closedList <- list()
  goalReached <- FALSE
  while(!goalReached) {
    currentSquareAndIndex <- getLowestFNode(openList, destination)
    currentSquare <- currentSquareAndIndex[1]
    currentSquareIndex <- as.integer(currentSquareAndIndex[2])
    currentSquare <- currentSquare[[1]]
    openList[[currentSquareIndex]] <- NULL
    if(currentSquare$hCost == 0) { #i.e. the goal node is reached
      goalReached <- TRUE
    }
    closedList <- append(closedList,list(currentSquare))
    neighborList <- getNeighbors(currentSquare[1], currentSquare[2])
    neighborList <- superFGCalc(currentSquare, roads, neighborList, destination)
    for (i in 1:length(neighborList)) {
      if (existsInList(neighborList[[i]], closedList)){
        next()
      }
      if(!existsInList(neighborList[[i]], openList)) {
        neighborList[[i]]$parent <- currentSquare
        neighborList[[i]]$hCost <- hCalc(neighborList[[i]][1],neighborList[[i]][2], destination[1], destination[2])
        openList <- append(openList, list(neighborList[[i]]))
        next()
      }
      if(existsInList(neighborList[[i]], openList)) {
        
        # If it is on the open list already,check to see if this path to that square is better, 
        # using G cost as the measure. A lower G cost means that this is a better path. 
        # If so, change the parent of the square to the current square, and recalculate 
        # the G and F scores of the square. If you are keeping your open list sorted by F score, 
        # you may need to resort the list to account for the change.
        
        newGCost <- gCalc(roads,currentSquare,neighborList[[i]][1],neighborList[[i]][2]) #make sure that gCalc now calculates result by way of parent
        newFCost <- newGCost + hCalc(neighborList[[i]][1], neighborList[[i]][2], destination[1], destination[2])
        oldGCost <- neighborList[[i]]$gCost
        if(newGCost < oldGCost) {
          neighborList[[i]]$parent <- currentSquare
          neighborList[[i]]$fCost <- newFCost
          neighborList[[i]]$gCost <- newGCost 
        }
        next()
      }
    }  
  }
  return (closedList[[length(closedList)]])
}

list.depth <- function(this, thisdepth = 0) {
  # code from http://stackoverflow.com/a/13433689/1270695
  if(!is.list(this)) {
    return(thisdepth)
  } else {
    return(max(unlist(lapply(this, list.depth, thisdepth = thisdepth+1))))    
  }
}

backwardsIterator <- function(iList, iterator) {
  oldList <- iList
  newList <- list()
  iterator <- iterator - 1
  for (i in 1:iterator) {
    newList <- append(newList, list(oldList[1], oldList[2]))
    oldList <- oldList$parent
  }
  return (newList)
}

masterMindDM=function(roads,car,packages) {
  
  if (length(nextPackage(car$x, car$y,packages)) > 0) {
    if (car$load == 0) {
      destination <- nextPackage(car$x,car$y,packages)
      if (length(destination) != 0) {
        moveList <- aStar(car, roads, destination)
      }
    }
    else if (!(car$load == 0)) {
      destination <- list(packages[car$load, 3], packages[car$load,4])
      moveList <- aStar(car, roads, destination)
    }
    newMoveList <- backwardsIterator(moveList, list.depth(moveList))
    #print(paste(newMoveList))
    if (length(newMoveList) > 2) {
      moveDestination <- list(newMoveList[[length(newMoveList) - 3]][1], newMoveList[[length(newMoveList) - 2]][1])
    }
    else if (length(newMoveList) <= 2) {
      moveDestination <- list(newMoveList[[length(newMoveList) - 1]][1], newMoveList[[length(newMoveList)]][1])
    }
    nextStep <- translator(car, moveDestination[[1]], moveDestination[[2]])
    car$nextMove <- nextStep
  }
  
  return (car)
}
#' Run Delivery Man
#' 
#' Runs the delivery man game. In this game, deliveries are randomly placed on a city grid. You
#' must pick up and deliver the deliveries as fast as possible under changing traffic conditions.
#' Your score is the time it takes for you to complete this task. To play manually pass manualDM
#' as the carReady function and enter the number pad direction numbers to make moves.
#' @param carReady Your function that takes three arguments: (1) a list of two matrices giving the 
#' traffice conditions. The first matrix is named 'hroads' and gives a matrix of traffice conditions
#' on the horizontal roads. The second matrix is named 'vroads' and gives a matrix of traffic 
#' conditional on the vertical roads. (2) a list providing information about your car. This
#' list includes the x and y coordinates of the car with names 'x' and 'y', the package the car 
#' is carrying, with name 'load' (this is 0 if no package is being carried), a list called
#' 'mem' that you can use to store information you want to remember from turn to turn, and
#' a field called nextMove where you will write what you want the car to do. Moves are 
#' specified as on the number-pad (2 down, 4 left, 6 right, 8 up, 5 stay still). (3) A
#' matrix containing information about the packages. This contains five columns and a row for each
#' package. The first two columns give x and y coordinates about where the package should be picked
#' up from. The next two columns give x and y coordinates about where the package should be 
#' delivered to. The final column specifies the package status (0 is not picked up, 1 is picked up but not delivered, 2 is delivered).
#' Your function should return the car object with the nextMove specified.
#' @param dim The dimension of the board. You will be scored on a board of dimension 10.
#' @param turns The number of turns the game should go for if deliveries are not made. Ignore this 
#' except for noting that the default is 2000 so if you have not made deliveries after 2000 turns
#' you fail.
#' @param pause The pause period between moves. Ignore this.
#' @param del The number of deliveries. You will be scored on a board with 5 deliveries.
#' @return A string describing the outcome of the game.
#' @export
runDeliveryMan <- function (carReady=manualDM,dim=10,turns=2000,pause=0.1,del=5) {
  roads=makeRoadMatrices(dim)
  car=list(x=1,y=1,wait=0,load=0,nextMove=NA,mem=list())
  packages=matrix(sample(1:dim,replace=T,5*del),ncol=5)
  packages[,5]=rep(0,del)
  for (i in 1:turns) {
    makeDotGrid(dim,i) 
    roads=updateRoads(roads$hroads,roads$vroads)
    plotRoads(roads$hroads,roads$vroads) 
    points(car$x,car$y,pch=16,col="blue",cex=3)  
    plotPackages(packages)
    if (car$wait==0) {
      if (car$load==0) {
        on=packageOn(car$x,car$y,packages)
        if (on!=0) {
          packages[on,5]=1
          car$load=on
        }
      } else if (packages[car$load,3]==car$x && packages[car$load,4]==car$y) {
        packages[car$load,5]=2
        car$load=0
        if (sum(packages[,5])==2*nrow(packages)) {
          print (paste("Congratulations! You suceeded in",i,"turns!"))
          return (i)
        }
      }      
      car=carReady(roads,car,packages)
      car=processNextMove(car,roads,dim)
    } else {
      car$wait=car$wait-1
    }
    Sys.sleep(pause)
  }
  print (paste("You failed to complete the task. Try again."))
  return (NA)
}
packageOn<-function(x,y,packages){
  notpickedup=which(packages[,5]==0)
  onX=which(packages[,1]==x)
  onY=which(packages[,2]==y)
  available=intersect(notpickedup,intersect(onX,onY))
  if (length(available)!=0) {
    return (available[1])
  } 
  return (0)
}
processNextMove<-function(car,roads,dim) {
  nextMove=car$nextMove
  if (nextMove==8) {
    if (car$y!=dim) {
      car$wait=roads$vroads[car$y,car$x]
      car$y=car$y+1
    } else {
      warning(paste("Cannot move up from y-position",car$y))
    }
  } else if (nextMove==2) {
    if (car$y!=1) {
      car$y=car$y-1
      car$wait=roads$vroads[car$y,car$x]
    } else {
      warning(paste("Cannot move down from y-position",car$y))
    }
  }  else if (nextMove==4) {
    if (car$x!=1) {
      car$x=car$x-1
      car$wait=roads$hroads[car$y,car$x]
    } else {
      warning(paste("Cannot move left from x-position",car$x))
    }
  }  else if (nextMove==6) {
    if (car$x!=dim) {
      car$wait=roads$hroads[car$y,car$x]
      car$x=car$x+1
    } else {
      warning(paste("Cannot move right from x-position",car$x))
    }
  } else if (nextMove!=5) {
    warning("Invalid move. No move made. Use 5 for deliberate no move.")    
  }
  car$nextMove=NA
  return (car)
} 

plotPackages=function(packages) {
  notpickedup=which(packages[,5]==0) 
  notdelivered=which(packages[,5]!=2)
  points(packages[notpickedup,1],packages[notpickedup,2],col="green",pch=18,cex=3)
  points(packages[notdelivered,3],packages[notdelivered,4],col="red",pch=18,cex=3)
}

makeRoadGrid<-function() {
  
  out=matrix(rep("S",51*51),ncol=51)
  out[26,]=rep("H",51)
  out[,26]=rep("H",51)
}

makeRoadGrid<-function() {
  out=matrix(rep("S",51*51),ncol=51)
  out[26,]=rep("H",51)
  out[,26]=rep("H",51)
}
#' @export
makeDotGrid<-function(n,i) {
  plot(rep(seq(1,n),each=n),rep(seq(1,n),n),xlab="X",ylab="Y",main=paste("Delivery Man. Turn ", i,".",sep=""))
}

#' @export
makeRoadMatrices<-function(n){
  hroads=matrix(rep(1,n*(n-1)),nrow=n)
  vroads=matrix(rep(1,(n-1)*n),nrow=n-1)
  list(hroads=hroads,vroads=vroads)
}

#' @export
plotRoads<- function (hroads,vroads) {
  for (row in 1:nrow(hroads)) {
    for (col in 1:ncol(hroads)) {
      lines(c(col,col+1),c(row,row),col=hroads[row,col])
    }
  }
  for (row in 1:nrow(vroads)) {
    for (col in 1:ncol(vroads)) {
      lines(c(col,col),c(row,row+1),col=vroads[row,col])
    }
  }
}

#' @export
updateRoads<-function(hroads,vroads) {
  r1=runif(length(hroads))
  r2=runif(length(hroads))
  for (i in 1:length(hroads)) {
    h=hroads[i]
    if (h==1) {
      if (r1[i]<.05) {
        hroads[i]=2
      }
    }
    else {
      if (r1[i]<.05) {
        hroads[i]=h-1
      } else if (r1[i]<.1) {
        hroads[i]=h+1
      }
    }
    v=vroads[i]
    if (v==1) {
      if (r2[i]<.05) {
        vroads[i]=2
      }
    }
    else {
      if (r2[i]<.05) {
        vroads[i]=v-1
      } else if (r2[i]<.1) {
        vroads[i]=v+1
      }
    }    
  }
  list (hroads=hroads,vroads=vroads)
}


