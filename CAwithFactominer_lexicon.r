library(FactoMineR)

pdf("Results.pdf")

#read data
files <- list.files(pattern = '*.txt')
files
import.list <-lapply(files, read.table, sep="\t", header=TRUE, quote = "", stringsAsFactors = FALSE)
#import.list
data <- Reduce(function(x, y) merge(x, y, all=T, by= "Word"), import.list, accumulate=F)

nr_texts <- length(files)
nr_texts_plus_one <- length(files)+1


data_rownames <- data.frame(data[,-1], row.names=data[,1])
matrix <-data.matrix(data_rownames, rownames.force = NA)

#TODO change this with smoothing algorithm 1/n

matrix[is.na(matrix)] <- 0
data[is.na(data)] <- 0

data[1:3,1:3]

pattern_ids<-data[,1:1]
rownames(data) <- pattern_ids
data[1:3,1:3]
matrix[1:3,1:3]




#nomralize matrix
matrix <- sweep(matrix, 2, colSums(matrix), FUN="/")
matrix <- scale(matrix, center=FALSE, scale=colSums(matrix))
#matrix[1:3,1:3]

matrix <- t(matrix)
#matrix[1:3,1:3]






########################################################################################################
#ANALYSIS
########################################################################################################

#Correspondence analys
cares <-CA(matrix)



plot(cares,invisible=c("col"), title="just characters")


plot(cares,selectCol="contrib 10", unselect=1, title="CA Factor Map - Contrib 10", autoLab="yes", col.row="black", col.col="grey70")


plot(cares,selectCol="contrib 50", unselect=1, title="CA Factor Map - Contrib 50", autoLab="yes", col.row="black", col.col="grey70")


dev.off()
