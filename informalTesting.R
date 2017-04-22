#Informal function testing
#Niko Drake-McLaughlin

#It was not possible to formally test all functionality, partially because I did
#not have time to create equality functions for the different classes. I did however 
#informally test all of the funtions, which is what the code below does.

#At the bottom, the insert, delete and return all overlapping shapes functions are
#informally tested.


library(testthat)
source("shapeClasses.R")
source("rTree.R")


#####################
#Make shapes to test
####################
#Rectangles
###################
rec = new("Rectangle", id = "rec", cornersXY = matrix(c(2,3,4,5), ncol = 2))
#findBoundingBox(rec)
rec2 = new("Rectangle", id = "rec2", cornersXY = matrix(c(12,13,14.5,50), ncol = 2))
#findBoundingBox(rec2)
rec3 = new("Rectangle", id = "rec3", cornersXY = matrix(c(99,100,98,101), ncol = 2))
#findBoundingBox(rec3)
####################
#Circles
###################
circ = new("Circle", id = "circ", centerXY = c(2,3), radius = 1)
#circ
##findBoundingBox(circ)
circ2 = new("Circle", id = "circ2", centerXY = c(21,-31), radius = 10)
#circ2
##findBoundingBox(circ2)
circ3 = new("Circle", id = "circ3", centerXY = c(2,2), radius = 5)
circ4 = new("Circle", id = "circ4", centerXY = c(25,45), radius = 11)
####################
#Polygons
###################
poly = new("Polygon", id = "poly", cornersXY = matrix(c(1,2,3,4,6,4.1212,3,2,1,-7), 
                                         ncol = 2, byrow = TRUE))
#findBoundingBox(poly)

poly2 = new("Polygon", id = "poly2", 
            cornersXY = matrix(c(55,56,57,42,59,43,61,57,58,65), 
                               ncol = 2, byrow = TRUE))
#findBoundingBox(poly2)

poly3 = new("Polygon", id = "poly3", cornersXY = matrix(c(-14, -21, -19, -11, -3,-3), 
                                                      ncol = 2, byrow = TRUE))
#findBoundingBox(poly3)
####################
#Points
###################
koiPond = new("Point", id = "koiPond", pointXY = c(5,4))
#koiPond
#findBoundingBox(koiPond)

pontiff = new("Point", id = "pontiff", pointXY = c(59,14))
#pontiff
#findBoundingBox(pontiff)

pointer = new("Point", id = "pointer", pointXY = c(5,21))

pizzaPizza = new("Point", id = "pizzaPizza", pointXY = c(41, -21))

leaningTower = new("Point", id = "leaningTower", pointXY = c(49,39))

point1 = new("Point", id = "point1", pointXY = c(10,20))
point2 = new("Point", id = "point2", pointXY = c(12,25))
point3 = new("Point", id = "point3", pointXY = c(11,27))
point4 = new("Point", id = "point4", pointXY = c(13,23))

point70 = new("Point", id = "point70", pointXY = c(70,95))
point71 = new("Point", id = "point71", pointXY = c(71,56))
point72 = new("Point", id = "point72", pointXY = c(72,35))
point73 = new("Point", id = "point73", pointXY = c(73,19))
point74 = new("Point", id = "point74", pointXY = c(74,11))
point75 = new("Point", id = "point75", pointXY = c(75,41))

point60 = new("Point", id = "point60", pointXY = c(60,80))
point61 = new("Point", id = "point61", pointXY = c(61,0))
point62 = new("Point", id = "point62", pointXY = c(62,-39))
point63 = new("Point", id = "point63", pointXY = c(65,-19))

point80 = new("Point", id = "point80", pointXY = c(80,60))
point81 = new("Point", id = "point81", pointXY = c(81,55))
point82 = new("Point", id = "point82", pointXY = c(82,50))
point83 = new("Point", id = "point83", pointXY = c(83,51))
point84 = new("Point", id = "point84", pointXY = c(84,69))
point85 = new("Point", id = "point85", pointXY = c(85,72))

####################
#Unions of Shapes
###################
bunchOfShapes = new("Shape Union", id = "bunch1", 
                    listOfShapes = list(koiPond, rec, circ, poly))
#findBoundingBox(bunchOfShapes)

bunchOfShapes2 = new("Shape Union", id = "bunch2", 
                    listOfShapes = list(rec3, poly2))
#findBoundingBox(bunchOfShapes2)

bunchOfShapes3 = new("Shape Union", id = "bunchOfShapes3", 
                     listOfShapes = list(point1, point2, point3, point4))
#findBoundingBox(bunchOfShapes2)


####################
#Line Segments
###################
line1 = new("Line Segment", id = "line1", endPoints = matrix(c(4,21,10,33), ncol = 2))
#line1
#findBoundingBox(line1)

line2 = new("Line Segment", id = "line2", 
            endPoints = matrix(c(-7,-1,-10,-33), ncol = 2, byrow = TRUE))
#line2
#findBoundingBox(line2)

line3 = new("Line Segment", id = "line2", 
            endPoints = matrix(c(55,41,33,28), ncol = 2, byrow = TRUE))
#findBoundingBox(line3)




######################
#Test functions
#####################
#rootBoundingBox
#####################
xMat
xMat = matrix(c(1,2,3,4,5,6,7,8,5,5,4,4,-9,-8,6,1), ncol = 2)
max(xMat[,1])
rootBoundingBox(xMat)
expect_equal(rootBoundingBox(xMat), matrix(c(1,8,-9,6), ncol = 2))

#####################
#boundingArea
#####################
boundingArea()

xMat2 = matrix(c(1,8,-9,6), ncol = 2)
area = (8 - 1)*(6 - -9)
boundingArea(xMat2)

expect_equal(boundingArea(xMat2), 105)


#############################
#resizeBoundingBox
#####################
rec = new("Rectangle", cornersXY = matrix(c(2,3,4,5), ncol = 2))
findBoundingBox(rec)
rec2 = new("Rectangle", cornersXY = matrix(c(12,13,14.5,50), ncol = 2))
findBoundingBox(rec2)

circ = new("Circle", centerXY = c(2,3), radius = 1)
circ
findBoundingBox(circ)
circ2 = new("Circle", centerXY = c(21,-31), radius = 10)
circ2
findBoundingBox(circ2)

poly = new("Polygon", cornersXY = matrix(c(1,2,3,4,6,4.1212,3,2,1,-7), 
                                         ncol = 2, byrow = TRUE))
poly
findBoundingBox(poly)
rTreePractice1 = new("R Tree", leftChild = NULL, rightChild = NULL, 
                     boundingBox = matrix(c(0)),
                     data1 = rec, data2 = circ, data3 = poly)

resizeBoundingBox(rTreePractice1)

rTreePractice2 = new("R Tree", leftChild = NULL, rightChild = NULL, 
                     boundingBox = matrix(c(0)),
                     data1 = rec, data2 = circ, data3 = NULL)
resizeBoundingBox(rTreePractice2)

rTreePractice3 = new("R Tree", leftChild = NULL, rightChild = NULL, 
                     boundingBox = matrix(c(0)),
                     data1 = rec, data2 = NULL, data3 = NULL)
resizeBoundingBox(rTreePractice3)

rTreePractice4L = new("R Tree", leftChild = NULL, rightChild = NULL, 
                     boundingBox = matrix(c(0)),
                     data1 = rec, data2 = circ, data3 = poly)

rTreePractice4L@boundingBox = resizeBoundingBox(rTreePractice4L)
rTreePractice4L

rTreePractice4R = new("R Tree", leftChild = NULL, rightChild = NULL, 
                     boundingBox = matrix(c(0)),
                     data1 = rec2, data2 = circ2, data3 = NULL)
rTreePractice4R@boundingBox = resizeBoundingBox(rTreePractice4R)
rTreePractice4R

rTreePractice4Root = new("R Tree", leftChild = rTreePractice4L, 
                         rightChild = rTreePractice4R, 
                         boundingBox = matrix(c(0)),
                         data1 = NULL, data2 = NULL, data3 = NULL)
rTreePractice4Root@boundingBox = resizeBoundingBox(rTreePractice4Root)
rTreePractice4Root@boundingBox

rTreePractice5 = createRTree()
rTreePractice5
resizeBoundingBox(rTreePractice5)



#########################
#Choose best box
########################
#This appears to work, but I think I should choose a different heuristic for which
#box a new shape should go into.
#New metric should minimize the expansion of the sum of x and y values. Shouldn't
#be that hard.
pontiff = new("Point", pointXY = c(59,14))
pontiff
findBoundingBox(pontiff)

chooseBestBox(rTreePractice4Root@leftChild, rTreePractice4Root@rightChild, pontiff)
rTreePractice4Root


##############
#dataToLeftDataToRight
#####################
#appears to work

dataToLeftDataToRight(rec, circ, rec2, circ2)
dataToLeftDataToRight(rec, rec2, circ, circ2)
dataToLeftDataToRight(rec2, rec, circ2, circ)

circ3 = new("Circle", centerXY = c(2,2), radius = 5)
dataToLeftDataToRight(rec2, rec, circ3, circ)


#####################
#Insert, printRTrees, plotBoundingBoxes, deleteByID
#####################

rTree1 = createRTree()
rTree1 = insert.RTree(rTree1, circ)
rTree1 = insert.RTree(rTree1, circ2)
rTree1 = insert.RTree(rTree1, circ3)
rTree1 = insert.RTree(rTree1, rec2)
print(rTree1)
printRTree(rTree1)
plotBoundingBoxes(rTree1)

rTree1 = deleteByID(rTree1, ID = "circ3")
rTree1 = deleteByID(rTree1, ID = "circ")

printRTree(rTree1)

print(matrix(c(2,1,1,2), ncol = 2))
plot(matrix(c(2,1,1,2), ncol = 2))

root = createRTree()
sum(c(is.null(root@data1), is.null(root@data2), is.null(root@data3)))


rTree2 = createRTree()
rTree2 = insert.RTree(rTree2, circ)
rTree2 = insert.RTree(rTree2, circ2)
rTree2 = insert.RTree(rTree2, circ3)
rTree2 = insert.RTree(rTree2, rec2)
rTree2 = insert.RTree(rTree2, rec)
rTree2 = insert.RTree(rTree2, pontiff)
rTree2 = insert.RTree(rTree2, koiPond)
rTree2 = insert.RTree(rTree2, poly)
#print(rTree1)
printRTree(rTree2)
#Confusing to test, but looks like it works!
plotBoundingBoxes(rTree2)


rTree3 = createRTree()
rTree3 = insert.RTree(rTree3, pontiff)
rTree3 = insert.RTree(rTree3, poly2)
rTree3 = insert.RTree(rTree3, rec3)
rTree3 = insert.RTree(rTree3, point1)
rTree3 = insert.RTree(rTree3, line1)
rTree3 = insert.RTree(rTree3, bunchOfShapes3)
rTree3 = insert.RTree(rTree3, rec2)
rTree3 = insert.RTree(rTree3, leaningTower)
rTree3 = insert.RTree(rTree3, point3)
rTree3 = insert.RTree(rTree3, poly3)
rTree3 = insert.RTree(rTree3, rec)
rTree3 = insert.RTree(rTree3, poly)
rTree3 = insert.RTree(rTree3, point2)
rTree3 = insert.RTree(rTree3, line2)
rTree3 = insert.RTree(rTree3, point4)
rTree3 = insert.RTree(rTree3, circ4)
rTree3 = insert.RTree(rTree3, line3)
rTree3 = insert.RTree(rTree3, circ2)
rTree3 = insert.RTree(rTree3, circ3)
rTree3 = insert.RTree(rTree3, koiPond)
rTree3 = insert.RTree(rTree3, circ)
rTree3 = insert.RTree(rTree3, pizzaPizza)
rTree3 = insert.RTree(rTree3, pointer)

plotBoundingBoxes(rTree3)
#uhh, it looks like it is working!

##########
#insert more points
##########

rTree3 = insert.RTree(rTree3, point80)
rTree3 = insert.RTree(rTree3, point81)
rTree3 = insert.RTree(rTree3, point82)
rTree3 = insert.RTree(rTree3, point83)
rTree3 = insert.RTree(rTree3, point84)
rTree3 = insert.RTree(rTree3, point85)

plotBoundingBoxes(rTree3)



rTree3 = insert.RTree(rTree3, point70)
rTree3 = insert.RTree(rTree3, point71)
rTree3 = insert.RTree(rTree3, point72)
rTree3 = insert.RTree(rTree3, point73)
rTree3 = insert.RTree(rTree3, point74)
rTree3 = insert.RTree(rTree3, point75)

rTree3 = insert.RTree(rTree3, point60)
rTree3 = insert.RTree(rTree3, point61)
rTree3 = insert.RTree(rTree3, point62)
rTree3 = insert.RTree(rTree3, point63)

plotBoundingBoxes(rTree3)

###########
#the big delete
#could be trouble
############
rTree3 = deleteByID(rTree3, ID = "point60")
rTree3 = deleteByID(rTree3, ID = "point61")
rTree3 = deleteByID(rTree3, ID = "point62")
rTree3 = deleteByID(rTree3, ID = "point63")

plotBoundingBoxes(rTree3)

rTree3 = deleteByID(rTree3, ID = "point70")
rTree3 = deleteByID(rTree3, ID = "point71")
rTree3 = deleteByID(rTree3, ID = "point72")
rTree3 = deleteByID(rTree3, ID = "point73")
rTree3 = deleteByID(rTree3, ID = "point74")
rTree3 = deleteByID(rTree3, ID = "point75")

plotBoundingBoxes(rTree3)

rTree3 = deleteByID(rTree3, ID = "point80")
rTree3 = deleteByID(rTree3, ID = "point81")
rTree3 = deleteByID(rTree3, ID = "point82")
rTree3 = deleteByID(rTree3, ID = "point83")
rTree3 = deleteByID(rTree3, ID = "point84")
rTree3 = deleteByID(rTree3, ID = "point85")

plotBoundingBoxes(rTree3)

rTree3 = deleteByID(rTree3, ID = "koiPond")
rTree3 = deleteByID(rTree3, ID = "pontiff")
rTree3 = deleteByID(rTree3, ID = "circ")
rTree3 = deleteByID(rTree3, ID = "pizzaPizza")
rTree3 = deleteByID(rTree3, ID = "pointer")

plotBoundingBoxes(rTree3)
                    
rTree3 = deleteByID(rTree3, ID = "pontiff")
rTree3 = deleteByID(rTree3, ID = "poly2")
rTree3 = deleteByID(rTree3, ID = "rec3")
rTree3 = deleteByID(rTree3, ID = "point1")
rTree3 = deleteByID(rTree3, ID = "line1")
rTree3 = deleteByID(rTree3, ID = "bunchOfShapes3")
rTree3 = deleteByID(rTree3, ID = "rec2")
rTree3 = deleteByID(rTree3, ID = "leaningTower")

plotBoundingBoxes(rTree3)
                    
rTree3 = deleteByID(rTree3, ID = "point3")
rTree3 = deleteByID(rTree3, ID = "poly3")
rTree3 = deleteByID(rTree3, ID = "rec")
rTree3 = deleteByID(rTree3, ID = "poly")
rTree3 = deleteByID(rTree3, ID = "point2")
rTree3 = deleteByID(rTree3, ID = "line2")

plotBoundingBoxes(rTree3)
                    
rTree3 = deleteByID(rTree3, ID = "point4")
rTree3 = deleteByID(rTree3, ID = "circ4")
plotBoundingBoxes(rTree3)
printRTree(rTree3)

rTree3 = deleteByID(rTree3, ID = "line3")
plotBoundingBoxes(rTree3)


rTree3 = deleteByID(rTree3, ID = "circ2")
plotBoundingBoxes(rTree3)
printRTree(rTree3)
#what is still in the tree?
#circ3
#It works!

rTree3 = deleteByID(rTree3, ID = "circ3")
plotBoundingBoxes(rTree3)
printRTree(rTree3)
rTree3
##
#Delete works!
##

##############################################
#100 random points

xcoord = runif(100, min = -100, max = 100)
ycoord = runif(100, min = -100, max = 100)

rTree4 = createRTree()

for (i in 1:100) {
  tempID = paste("point", i, sep = "")
  pointForInsertion = new("Point", id = tempID, pointXY = c(xcoord[i], ycoord[i]))
  rTree4 = insert.RTree(rTree4, pointForInsertion)
}

plotBoundingBoxes(rTree4)

###
#try returnAllOverlappingShapes with the 100 random points
matrix(c(50,-100,100,0), ncol = 2, byrow = TRUE)
testLowerRight = returnAllOverlappingShapes(rTree4, 
                                  matrix(c(50,-100,100,0), ncol = 2, byrow = TRUE))

testLowerRight

testAll = returnAllOverlappingShapes(rTree4,
                                     matrix(c(-100,-100,100,100), 
                                            ncol = 2, byrow = TRUE))

length(testAll)

testNone = returnAllOverlappingShapes(rTree4,
                                     matrix(c(0, 0, 0, 0), 
                                            ncol = 2, byrow = TRUE))
length(testNone)

#Pressing my luck
#1000 random points
xcoord = runif(1000, min = -1000, max = 1000)
ycoord = runif(1000, min = -1000, max = 1000)

rTree5 = createRTree()

for (i in 1:1000) {
  tempID = paste("point", i, sep = "")
  pointForInsertion = new("Point", id = tempID, pointXY = c(xcoord[i], ycoord[i]))
  rTree5 = insert.RTree(rTree5, pointForInsertion)
}

plotBoundingBoxes(rTree5)
#awesssooommmee

rTree6 = createRTree()
rTree6 = insert.RTree(rTree6, point60)
rTree6 = insert.RTree(rTree6, point61)
rTree6 = insert.RTree(rTree6, point62)
rTree6 = insert.RTree(rTree6, point63)
rTree6 = insert.RTree(rTree6, point70)
rTree6 = insert.RTree(rTree6, point71)
rTree6 = insert.RTree(rTree6, point72)
rTree6 = insert.RTree(rTree6, point73)
rTree6 = insert.RTree(rTree6, point74)
rTree6 = insert.RTree(rTree6, point75)
rTree6 = insert.RTree(rTree6, point80)
rTree6 = insert.RTree(rTree6, point81)
rTree6 = insert.RTree(rTree6, point82)
rTree6 = insert.RTree(rTree6, point83)
rTree6 = insert.RTree(rTree6, point84)
rTree6 = insert.RTree(rTree6, point85)
rTree6 = insert.RTree(rTree6, koiPond)
rTree6 = insert.RTree(rTree6, pontiff)
rTree6 = insert.RTree(rTree6, circ)
rTree6 = insert.RTree(rTree6, pizzaPizza)
rTree6 = insert.RTree(rTree6, pointer)
rTree6 = insert.RTree(rTree6, pontiff)
rTree6 = insert.RTree(rTree6, poly2)
rTree6 = insert.RTree(rTree6, rec3)
rTree6 = insert.RTree(rTree6, point1)
rTree6 = insert.RTree(rTree6, line1)
rTree6 = insert.RTree(rTree6, bunchOfShapes3)
rTree6 = insert.RTree(rTree6, rec2)
rTree6 = insert.RTree(rTree6, leaningTower)
rTree6 = insert.RTree(rTree6, point3)
rTree6 = insert.RTree(rTree6, poly3)
rTree6 = insert.RTree(rTree6, rec)
rTree6 = insert.RTree(rTree6, poly)
rTree6 = insert.RTree(rTree6, point2)
rTree6 = insert.RTree(rTree6, line2)
rTree6 = insert.RTree(rTree6, point4)
rTree6 = insert.RTree(rTree6, circ4)
rTree6 = insert.RTree(rTree6, line3)
rTree6 = insert.RTree(rTree6, circ2)

plotBoundingBoxes(rTree6)

points60to90 = returnAllOverlappingShapes(rTree6, matrix(c(60,-45, 90, 100), 
                                                         ncol = 2, byrow = TRUE))
points60to90

points60only = returnAllOverlappingShapes(rTree6, matrix(c(60,80, 60, 80), 
                                                         ncol = 2, byrow = TRUE))
points60only
#appears to work

