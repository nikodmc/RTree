#!/usr/bin/env Rscript

#Niko Drake-McLaughlin

#This script lets the user run search for crimes from the command line, using the
#R tree and functions set up in crimeData.R. 
#Unfortunately it has to build the R tree each time. How do I avoid this?
#The user specifies the section and the bounding box, and a list of crimes is
#printed out. To avoid printing out 1909 crime shapes, choose the bounding box
#carefully. Probably would be better to print to a .txt or something, but no more
#time for that. I tested it on the Shadyside bounding box, which worked well.

#building the Rtree
suppressMessages(library(methods))
suppressMessages(library("argparse"))
source("crimeData.R")

parser = ArgumentParser()
parser$add_argument("-s", "--section", type = "integer",
                    help="assaults are section 2702, robberies are section 3701")
parser$add_argument("-l", "--left", type = "double", 
                    help="left side of the query box")
parser$add_argument("-b", "--bottom", type = "double", 
                    help="bottom side of the query box")
parser$add_argument("-r", "--right", type = "double", 
                    help="right side of the query box")
parser$add_argument("-t", "--top", type = "double", 
                    help="top side of the query box")

args <- parser$parse_args()

queryBox1 = matrix(c(args$left, args$bottom, args$right, args$top), 
                  ncol = 2, byrow = TRUE)

#returnAllOverlappingShapes(root, queryBox)
listOfCrimes = returnAllOverlappingShapes(crimeRTree, queryBox1)
#This list has every crime type. The code below prints only the specified crime type.
listLength = length(listOfCrimes)
typeSpecificCrimes = list()

for (i in 1:listLength) {
  if(listOfCrimes[[i]]@section == args$section) {
    typeSpecificCrimes = append(typeSpecificCrimes, listOfCrimes[[i]])
  }
  
}

print(typeSpecificCrimes)
