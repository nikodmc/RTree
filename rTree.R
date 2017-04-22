#Niko Drake-McLaughlin

#Below is the code for the R Tree class and all of the R Tree functions, excluding
#some crime data and crime tree specific functions.
#There are probably too many comments, but they were meant to help me write the
#code and remember what it does, and I ran out of time to edit them.

#Two functions use splitting heuristic: chooseBestBoxOption, and 
#dataLeftDataRightOption
#There are two functions that determine the heuristic and that are fed into the
#functions mentioned above: boundingArea and boundingPerimeter
#Using the plotBoundingBox function as a visual check, boundingPerimeter looks like
#the better function, and that is the one that is used.


setClass("R Tree", 
         slots = c(boundingBox = "matrix", 
                   data1 = "ANY", data2 = "ANY", data3 = "ANY",
                   leftChild = "ANY", rightChild = "ANY")
)

createRTree = function() {
  return(new("R Tree", leftChild = NULL, rightChild = NULL, 
             boundingBox = matrix(c(0)),
             data1 = NULL, data2 = NULL, data3 = NULL))
}


insert.RTree = function(root, shape, decisionFunction = boundingPerimeter) {
  #check if there are children, if not insert
  #shouldn't need to check for rightChild, if there is a left child there
  #should be a right child
  
  #I could change this to have it travel down existing children first, but this way 
  #works too
  if(is.null(root@leftChild)) {
    if(is.null(root@data1)) {
      root@data1 = shape
      root@boundingBox = resizeBoundingBox(root)
      return(root)
    }
    else if(is.null(root@data2)) {
      root@data2 = shape
      root@boundingBox = resizeBoundingBox(root)
      return(root)
    }
    else if(is.null(root@data3)) {
      root@data3 = shape
      root@boundingBox = resizeBoundingBox(root)
      return(root)
    }
    else {
      #When the data is full and there are no children, then it must be decided
      #how to split up the data into new children's bounding boxes. Gonna need
      #some functions for this.
      #dataToLeftDataToRight
      #dataLeft, dataRight
      #create leftChild and rightChild
      #use insert function with appropriate data from list to insert data into 
      #them
      #set data in this node to NULL
      #browser()
      dataLeftdataRight = dataToLeftDataToRightOption(root@data1, root@data2, 
                                                      root@data3, shape,
                                                      decisionFunction)
      root@leftChild = createRTree()
      root@leftChild = insert.RTree(root@leftChild, dataLeftdataRight$left[[1]])
      root@leftChild = insert.RTree(root@leftChild, dataLeftdataRight$left[[2]])
      
      root@rightChild = createRTree()
      root@rightChild = insert.RTree(root@rightChild, dataLeftdataRight$right[[1]])
      root@rightChild = insert.RTree(root@rightChild, dataLeftdataRight$right[[2]])
      #data has been passed to children, so set to NULL in root
      root@data1 = NULL
      root@data2 = NULL
      root@data3 = NULL
      root@boundingBox = resizeBoundingBox(root)
      return(root)
    }
  }
  #If there are children, then skip the first chunk of code and decide which child 
  #to go to. Might want to put this bit first actually.
  else {
    #browser()
    childTreeToAddTo = chooseBestBoxOption(root@leftChild, root@rightChild, shape, 
                                           decisionFunction)
    if(childTreeToAddTo == "left") {
      root@leftChild = insert.RTree(root@leftChild, shape)
    }
    else{
      root@rightChild = insert.RTree(root@rightChild, shape)
    }
    root@boundingBox = resizeBoundingBox(root)
    return(root)
  }
  
}

#check data or check children and create appropriate bounding box for R Tree node

resizeBoundingBox = function(root) {
  #shouldn't need to check for rightChild, if there is a left child there
  #should always be a right child
  if(is.null(root@leftChild)) {
    box1 = findBoundingBox(root@data1)
    box2 = findBoundingBox(root@data2)
    box3 = findBoundingBox(root@data3)
    
    if(is.null(box1) & is.null(box2) & is.null(box3)) {
      return(matrix(c(0)))
    }
    
    if(!is.null(box1)) {
      dataBoxes = box1
    }
    
    if(!is.null(box2)){
      dataBoxes = rbind(box1, box2)
    }
    if(!is.null(box3)){
      dataBoxes = rbind(dataBoxes, box3)
    }
    
    return(rootBoundingBox(dataBoxes))
  }
  else {
    childBoxes = rbind(root@leftChild@boundingBox, root@rightChild@boundingBox)
    return(rootBoundingBox(childBoxes))
  }
  
}

#Takes a nx2 matrix where each row is the xy-coordinates of a point, and converts
#it to a 2x2 matrix signifying the bounding box of all of those points. Each point
#could be the corner of a polygon, and it would return the appropriate bounding box
rootBoundingBox = function(x) {
  return(matrix(c(min(x[,1]), min(x[,2]), max(x[,1]), max(x[,2])), 
                ncol = 2, byrow = TRUE))
}



#based on shape and associated bounding boxes, choose to go either to 
#left or right child
#pass in the function you want to decide where to go, either boundingArea or
#boundingPerimeter
chooseBestBoxOption = function(leftChild, rightChild, shape, decisionFunction) {
  #browser()
  #smallest new area as criteria for which to choose?
  leftBox = leftChild@boundingBox
  rightBox = rightChild@boundingBox
  #try adding the point to the left box first
  leftBoxPlus = rootBoundingBox(rbind(leftBox, findBoundingBox(shape)))
  addLeftCombined = decisionFunction(leftBoxPlus) + decisionFunction(rightBox)
  #try adding the point to the right box first
  rightBoxPlus = rootBoundingBox(rbind(rightBox, findBoundingBox(shape)))
  addRightCombined = decisionFunction(rightBoxPlus) + decisionFunction(leftBox)
  
  if (addLeftCombined <= addRightCombined) {
    return("left")
  }
  else {
    return("right")
  }
}

#boundingBox(shape) returns either NULL or a 2x2 matrix, with the first
#row the (x,y) coordinates of the lower left corner, and the second row the 
#(x,y) coordinates of the upper right corner

#returns the area of a bounding box given in the form of a 2x2 matrix, with the first
#row the (x,y) coordinates of the lower left corner, and the second row the 
#(x,y) coordinates of the upper right corner
boundingArea = function(x) {
  return( (x[2,1] - x[1,1]) *  (x[2,2] - x[1,2]))
}

#returns perimeter of a boundingbox
#multiplying by 2 should be unnecessary for all functionality here, but it doesn't
#hurt either
boundingPerimeter = function(x) {
  return( 2*((x[2,1] - x[1,1]) + (x[2,2] - x[1,2])))
}


#When a new node must be created, this function decides which data should go left
#and which data should go right based on the decision function passed in.
dataToLeftDataToRightOption = function(data1, data2, data3, shape, decisionFunction) {
  #make bounding boxes with every combination of the four shapes (dataX are shapes)
  #data1 and data2 vs data3 and shape
  #data1 and data3 vs data2 and shape
  #data1 and shape vs data2 and data3
  
  #use decision function for each of the two new bounding boxes for these 3
  #combinations. For the combination with the smallest total area return 
  #list(left = list(data1, data3), right = list(data2, shape))
  #
  #3 options
  #Option 1
  #browser()
  split1L = rootBoundingBox(rbind(findBoundingBox(data1), findBoundingBox(data2)))
  split1R = rootBoundingBox(rbind(findBoundingBox(data3), findBoundingBox(shape)))
  combined1 = decisionFunction(split1L) + decisionFunction(split1R)
  #Option 2
  split2L = rootBoundingBox(rbind(findBoundingBox(data1), findBoundingBox(data3)))
  split2R = rootBoundingBox(rbind(findBoundingBox(data2), findBoundingBox(shape)))
  combined2 = decisionFunction(split2L) + decisionFunction(split2R)
  #Option 3
  split3L = rootBoundingBox(rbind(findBoundingBox(data1), findBoundingBox(shape)))
  split3R = rootBoundingBox(rbind(findBoundingBox(data2), findBoundingBox(data3)))
  combined3 = decisionFunction(split3L) + decisionFunction(split3R)
  
  if(combined1 <= combined2 & combined1 <= combined3) {
    return(list(left = list(data1, data2), right = list(data3, shape)))
  }
  else if(combined2 <= combined1 & combined2 <= combined3) {
    return(list(left = list(data1, data3), right = list(data2, shape)))
  }
  else {
    return(list(left = list(data1, shape), right = list(data2, data3)))
  }
  
}
#Search through entire tree looking for data with correct ID, then remove it.
#Tricky bit comes when that is the last piece of data in that leaf. Then need to 
#remove entire node and remerge the parent node
#must return an entire tree
#will delete all shapes with given ID (could be entered multiple times, I guess)
deleteByID = function(root, ID) {
  #If there are children, then there are not shapes, so just go to both children
  if(!is.null(root@leftChild)) {
    #browser()
    #recursively call delete on each child
    root@leftChild = deleteByID(root@leftChild, ID)
    root@rightChild = deleteByID(root@rightChild, ID)
    
    #if one of the children comes back NULL, then all of the data from that node
    #was deleted. Root should become one of its othe children, if any survived
    #left child is null, root becomes right child. If right child is NULL that 
    #is fine
    if(is.null(root@leftChild)) {
      return(root@rightChild)
    }
    #right child is null, root becomes left child
    else if(is.null(root@rightChild)) {
      return(root@leftChild)
    }
    
    #if neither child node is returned NULL, then resize bounding box and 
    #return root
    root@boundingBox = resizeBoundingBox(root)
    return(root)
  }
  #If there are no children, then we are in a leaf node with data
  #check if shape is in one of the data slots and remove it
  #if the removed shape was in data1 or data2, shift any remaining shapes from a 
  #later data spot up
  if(!is.null(root@data3)) {
    if(root@data3@id == ID) {
      root@data3 = NULL
    }
  }
  
  if(!is.null(root@data2)) {
    if(root@data2@id == ID) {
      #I think this works to shift the data up to slots 1 and 2 and avoids 
      #another if statement
      #If data3 is NULL, that's ok, it just sets data2 to NULL, which is what we
      #want. Otherwise it shifts the shape up the data slots.
      root@data2 = root@data3
      root@data3 = NULL
    }
    
  }
  
  if(!is.null(root@data1)) {
    if(root@data1@id == ID) {
      root@data1 = root@data2
      root@data2 = root@data3
      root@data3 = NULL
    }
  }
  #If there is still data in this node
  if(sum(c(is.null(root@data1), is.null(root@data2), is.null(root@data3))) < 3) {
    root@boundingBox = resizeBoundingBox(root)
    return(root)
  }
  #if there is now no data in this node, it gets complicated
  #this node must be deleted and the parent node must become it's other child node
  else {
    return(NULL)
  }
}


#level should always be set to 1 initially, maybe I should make a wrapper for this
printRTree = function(x, level = 1) {
  if(is.null(x)){
    cat("\nNull")
    return(0)
  }
  
  cat("\nR Tree Level: ", level)
  cat("\nBounding box of node: \n")
  print(x@boundingBox)
  #  if(!is.null(x@data1)) {
  cat("\ndata1: ")
  print(x@data1)
  cat("\ndata2: ")
  print(x@data2)
  cat("\ndata3: ")
  print(x@data3)
  #  }
  #  else {
  cat("\nLeft Child of level", level)
  printRTree(x@leftChild, level + 1)
  cat("\nRight Child of level", level)
  printRTree(x@rightChild, level + 1)
  #  }
  
}

#Graphical representation of the boxes
#would be nice to add:
#a legend for which colors correspond to which levels
#some sort of representation for the bounding boxes of the shapes themselves,
#not just the nodes.
plotBoundingBoxes = function(root, level = 1) {
  #If we are in a NULL node, get out.
  if(is.null(root)) {
    return(NULL)
  }
  #If we are in a newly created node with nothing in it, get out. I don't think
  #this should be possible
  # if(root@boundingBox == matrix(c(0))) {
  #   return(NULL)
  # }
  #Plot the overall bounding box of everything for the first node
  if (level == 1) {
    plot(root@boundingBox)
  }
  
  #create a matrix with all four corners of the bounding box
  allFourCorners = rbind(root@boundingBox[1,], #lower left corner
                         c(root@boundingBox[1,1], root@boundingBox[2,2]), #upper left corner
                         root@boundingBox[2,], #upper right corner
                         c(root@boundingBox[2,1], root@boundingBox[1,2])
  )
  #create another matrix with all four corners in a slightly different order
  allFourCornersShifted = rbind(allFourCorners[2:4,], allFourCorners[1,])
  #draw line segments between all four corners
  segments(allFourCorners[,1], allFourCorners[,2], 
           allFourCornersShifted[,1], allFourCornersShifted[,2], col = level)
  
  if(!is.null(root@leftChild)) {
    plotBoundingBoxes(root@leftChild, level + 1)
    plotBoundingBoxes(root@rightChild, level + 1)
    
  }
  
}

overlapCheck = function(mat1, mat2) {
  #If given null data or wrong size matrices, get out, signaling no overlap
  if(is.null(mat1) | is.null(mat2) | 
     any(dim(mat1) != c(2,2)) | any(dim(mat2) != c(2,2))) {
    return(FALSE)
  }
  
  leftNew = mat1[1,1]
  lowerNew = mat1[1,2]
  rightNew = mat1[2,1]
  upperNew = mat1[2,2]
  
  leftOld = mat2[1,1] 
  lowerOld = mat2[1,2] 
  rightOld = mat2[2,1] 
  upperOld = mat2[2,2] 
  
  #I am dividing this into three cases based on where the upper side of new is in
  #relation to old.
  #case 1
  #When the top of new is less than the bottom of old, there can be no overlap
  if(upperNew < lowerOld) {
    return(FALSE)
  }
  #case 2
  #when the top of new is between the bottom of old and the top of old, then 
  #we must check where the left and right corners are
  if(upperNew >= lowerOld & upperNew <= upperOld) {
    #if the left side of new is to the right of old, there can be no overlap
    if(leftNew > rightOld) {
      return(FALSE)
    }
    #if the left side of new is between the left and right side of old, there must
    #be an overlap in this case
    if(leftNew <= rightOld & leftNew >= leftOld) {
      return(TRUE)
    }
    #when the left side of new is to the left of the left side of old, then the right
    #side must be tested. If the right side of new is greater than the  left 
    #side of old, there is an overlap.
    #I don't actually need to include "leftNew <= leftOld" below, because that is
    #the only remaining option (I think). But for my own sanity and clarity, I'm 
    #leaving it in.
    if(leftNew <= leftOld & rightNew >= leftOld) {
      return(TRUE)
    }
    else {
      #The only remaining possibility for this case is that the right side of new 
      #is to the left of the left side of old, which means there cannot be an overlap
      return(FALSE)
    }
  }
  
  #case 3
  #when the top of new is above the top of old, then lower and left and right sides
  #must be tested
  if(upperNew > upperOld) {
    #when the bottom of new is above the top of old, there cannot be overlap in 
    #this case
    if(lowerNew > upperOld) {
      return(FALSE)
    }
   
    #if the bottom of new is less than the top of old, and the left side of new
    #is less than the right side of old and the right side of new is greater
    #than the left side of old, then there is overlap. 
    if(lowerNew <= upperOld & leftNew <= rightOld & rightNew >= leftOld) {
      return(TRUE)
    }
    #I think this covers all possibilities of overlap for this case
    return(FALSE)
    
  }
  
}

#When given an R Tree and queryBox, a 2x2 matrix representing a bounding box, this 
#function returns a list of all shapes that overlap with that bounding box
returnAllOverlappingShapes = function(root, queryBox) {
  #if root is null, return an empty list
  if(is.null(root)) {
    return(list())
  }
  #if root has children, test if there is bounding box overlap with the children
  #pass the function on to children with overlap. Both children could overlap.
  if(!is.null(root@leftChild)) {
    overlapListL = list()
    overlapListR = list()
    
    #if the overlap check function returns TRUE for left childs bounding box and
    #the query box, call function on that child
    if(overlapCheck(queryBox, root@leftChild@boundingBox)) {
      overlapListL = returnAllOverlappingShapes(root@leftChild, queryBox)
    }
    #same for right child
    if(overlapCheck(queryBox, root@rightChild@boundingBox)) {
      overlapListR = returnAllOverlappingShapes(root@rightChild, queryBox)
    }
    #return the concatenation of these two lists
    return(append(overlapListL, overlapListR))
    
  }
  #If we are in a leaf node, check each shape to see if it overlaps with the
  #query box. If it overlaps, return it in a list
  overlappingShapeList = list()
  
  #I think there has to be data in data1 at this point as we are in a leaf node.
  #But even if there isn't data (i.e. it is NULL), as is possible with data2 and
  #data3, the findBoundingBox function should then return NULL, and then
  #overlapCheck should return FALSE, so we should be safe without explicitly
  #checking
  if(overlapCheck(queryBox, findBoundingBox(root@data1))) {
    overlappingShapeList = list(root@data1)
  }
  #Do the same thing for data2 
  if(overlapCheck(queryBox, findBoundingBox(root@data2))) {
    overlappingShapeList = append(overlappingShapeList, root@data2)
  }
  #And same for data3
  if(overlapCheck(queryBox, findBoundingBox(root@data3))) {
    overlappingShapeList = append(overlappingShapeList, root@data3)
  }
  #return a list with all the shapes that overlap with the query box
  return(overlappingShapeList)
}





