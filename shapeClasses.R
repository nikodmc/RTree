#Niko Drake-McLaughlin

#This code creates all of the shape classes, except for crime specific shapes. 
#The only associated function is findBoundingBox.


###########
#S4 Classes
###########

setClass("Shape")

#non tilted rectangles
#the corners matrix gives the lower left hand corner in the first row and the upper
#right hand corner in the second row. This is the same format all bounding boxes
#are given in.
setClass("Rectangle", slots = c(id = "character", cornersXY = "matrix"), 
         contains = "Shape")

#contains numeric vector of length two with x and y coordinate of center and the
#radius
setClass("Circle", slots = c(id = "character", centerXY = "numeric", 
                             radius = "numeric"), 
         contains = "Shape")

#contains a numeric matrix where each row is the coordinate of a corner
setClass("Polygon", slots = c(id = "character", cornersXY = "matrix"), 
         contains = "Shape")

#contains numeric vector with x and y coordinates
setClass("Point", slots = c(id = "character", pointXY = "numeric"), 
         contains = "Shape")

#the shape union class contains a list of shapes
setClass("Shape Union", slots = c(id = "character", listOfShapes = "list"),
         contains = "Shape")

#A line segment defined by two endpoints. The endPoint matrix is a 2x2 matrix,
#where each row gives the xy coordinates of one of the points. The points can be
#input in any order.
setClass("Line Segment", slots = c(id = "character", endPoints = "matrix"),
         contains = "Shape")

#######################
#Bounding Box Functions
#######################
#All bounding box functions output a matrix that gives the lower left hand corner 
#of the bounding box in the first row and the upper right hand corner in the 
#second row. 
setGeneric("findBoundingBox", function(x,...) {
  standardGeneric("findBoundingBox")
})

setMethod("findBoundingBox", signature(x="Shape"), function(x) {
  return(findBoundingBox(x))
})

setMethod("findBoundingBox", signature(x="Rectangle"), function(x) {
  return(x@cornersXY)
})


setMethod("findBoundingBox", signature(x="Circle"), function(x) {
  return(matrix(c(x@centerXY[1] - x@radius, x@centerXY[2] - x@radius,
                  x@centerXY[1] + x@radius, x@centerXY[2] + x@radius), 
                ncol = 2, byrow = TRUE))
})

setMethod("findBoundingBox", signature(x="Polygon"), function(x) {
  return(matrix(c(min(x@cornersXY[,1]), min(x@cornersXY[,2]), 
                  max(x@cornersXY[,1]), max(x@cornersXY[,2])), 
                ncol = 2, byrow = TRUE))
})

setMethod("findBoundingBox", signature(x="Point"), function(x) {
  return(rbind(x@pointXY,x@pointXY))
})
#Thought about doing it this way, but decided not to
# setMethod("findBoundingBox", signature(x="nullShape"), function(x) {
#   return(NULL)
# })
setMethod("findBoundingBox", signature(x="NULL"), function(x) {
  return(NULL)
})
findBoundingBox(NULL)

setMethod("findBoundingBox", signature = "Shape Union", function(x) {
  boundingBoxList = lapply(x@listOfShapes, findBoundingBox)
  listLength = length(x@listOfShapes)
  
  #there should be at least 1 thing in the list of shapes (should write code to 
  #check for nothing too probably)
  boundingBoxes = boundingBoxList[[1]]
  listIndex = 2
  while(listIndex <= listLength) {
    boundingBoxes = rbind(boundingBoxes, boundingBoxList[[listIndex]])
    listIndex = listIndex + 1
  }
  
  return(matrix(c(min(boundingBoxes[,1]), min(boundingBoxes[,2]), 
                  max(boundingBoxes[,1]), max(boundingBoxes[,2])), 
                ncol = 2, byrow = TRUE))
  
})

#Line endpoints should ideally be given into the matrix with the first row the lower
#left point and the second row the upper right point, but in case they aren't, find
#the minimums and maximums for the endPoint matrix.
setMethod("findBoundingBox", signature = "Line Segment", function(x) {
  return(matrix(c(min(x@endPoints[,1]), min(x@endPoints[,2]), 
                  max(x@endPoints[,1]), max(x@endPoints[,2])), 
                ncol = 2, byrow = TRUE))
})
