#Loading the scripts that handles the 3D-rotation, transposition and plotting functions

source("3Dstrings/2011-02-21_plotting_functions.R")

#Downloading the structure of one loop of DNA in .pdf format. (If this fails because of firewalls or such, just download the file directly)
structure<-loadPDFfile("3Dstrings/pdb425d.ent")

maxComplexity<-10

plot.default(x=NULL,y=NULL,xlim=c(-100,200),ylim=c(-200,100),frame.plot=FALSE,axes=FALSE,xlab=" ",ylab=" ")

#far-background strands are made of code from the beginning of the ANRIL gene
source("3Dstrings/chr9p21.R")

#first strand, far background
from<-c(316.2, 16.4, 407.25)
to<-c(96.2, 5.4, 202.25)
rotationPoint<-c(-3.8, -53.6, 207.25)
outPoint<-drawDnaString(from,to,rotationPoint,maxComplexity=maxComplexity,test=F,structure=structure,sequence=chr9p21)

#second strand, far background
from<-c(299.2, 6.3, 398.4)
to<-c(119.2, -13.7, 228.4)
rotationPoint<-c(-20.8, -93.7, 198.4)
outPoint<-drawDnaString(from,to,rotationPoint,maxComplexity=maxComplexity,test=F,structure=structure,sequence=chr9p21)

#attractive strand
from=c(120,20,200)
to=c(-7,5,-10)
rotationPoint<-c(0, 50, -100)
seq1<-"GGGGGGGGGGGGGCTTGCTTCTTCTGAATTTCTTAAAGAACGTTCTGAAAATTCTACTCATGAATCTATTTCTCCCCCCCCCCCCCCCACCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC"
outPoint<-drawDnaString(from,to,rotationPoint,maxComplexity=maxComplexity,test=F,structure=structure,sequence=seq1)

#non-attractive strand
from=c(140, 0, 230)
to=c(-19, 8, -50)
rotationPoint<-c(0, 50, -100)
seq2<-"GGGGGGGGGGGGGCTTGCTTCTTCTGAATTTCTTAAAGAACGTTCTGAAAATTCTACTCATGAATCTATTTCTCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC"
outPoint<-drawDnaString(from,to,rotationPoint,maxComplexity=maxComplexity,test=F,structure=structure,sequence=seq2)

#open strand to the left
structureA<-structure[structure[,"chainID"]%in%"A",]
rotationPoint<-c(0, 50, -100)
from<-c(-8,	18, -5.5)
to<-c(11, 48, -103)
drawDnaString(from,to,rotationPoint=rotationPoint,maxComplexity=maxComplexity,test=F,structure=structureA,startAtom="15",endAtom="241",sequence="GCCGGTACCGGT")

#open strand to the right
structureB<-structure[structure[,"chainID"]%in%"B",]
rotationPoint<-c(50, 50, -100)
from<-c(-14, 17, -3.5)
to<-c(0, 20, -53)
drawDnaString(from,to,rotationPoint=rotationPoint,maxComplexity=maxComplexity,test=F,structure=structureA,startAtom="15",endAtom="241",sequence="CGGCCATGGCCA")
