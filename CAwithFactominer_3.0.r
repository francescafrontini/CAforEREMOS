library(FactoMineR)

pdf("Results.pdf")

########################################################################################################
#USAGE
########################################################################################################

#PUT THIS IN THE FOLDER NAMED "MODELS"
#IT WILL WORK ON ALL FILES IN THAT FOLDER WITH EXTENTION .txt
#OUTPUTS
#GRAPHS, and two FILES


MC = 200

#PASS AS PARAMETER THE MAXIMUM SIZE OF CONTRIBUTIONS THAT YOU WANT TO SEE PRINTED
#THIS SCIRIPT WILL PRODUCE A PLOT FOR WITH THE FIRST 10, 50, 100, <N> MOST CONTRIBUTIVE PATTERNS, 
#WHERE <N> IS THE value of max_contrib


########################################################################################################
#DATA PREPARATION
########################################################################################################


#read data
files <- list.files(pattern = '*.txt')
files
import.list <-lapply(files, read.table, sep="", header=TRUE)
#import.list
data <- Reduce(function(x, y) merge(x, y, all=T, by= "Pattern_ID"), import.list, accumulate=F)

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

#get most frequent patterns for each character

most_freqs <- c()

for (r in colnames(matrix)[1:nr_texts]){
	max_id <- which(matrix[,r]==max(matrix[,r]))
	max_value <- matrix[max_id,r,drop=FALSE]
	most_freqs <- c(most_freqs,rownames(max_value))
}



#load the list of pattern names by id 
pattern_names <- read.csv("../Model.txt", sep="\t" , header=FALSE, stringsAsFactors=FALSE, strip.white=TRUE, quote = "")
colnames(pattern_names) <-c("Pattern_Name", "Pattern_Number")
pattern_names <- transform(pattern_names, Pattern_Number = sprintf('Pattern_%d', Pattern_Number))
rownames(pattern_names) = pattern_names[,2]





#nomralize matrix
matrix <- sweep(matrix, 2, colSums(matrix), FUN="/")
matrix <- scale(matrix, center=FALSE, scale=colSums(matrix))
#matrix[1:3,1:3]

matrix <- t(matrix)
#matrix[1:3,1:3]


########################################################################################################
#FUNCTIONS
########################################################################################################

get_freqs <- function(key){
	freqs <-data[data$Pattern_ID==key,]
	freqs <- freqs[2:nr_texts_plus_one]
	return(paste(freqs,collapse="\t"))
}




########################################################################################################
#ANALYSIS
########################################################################################################

#Correspondence analys
cares <-CA(matrix)


#contribution according to Factominer (taken from the code)... not quite the same though!!!!!
#contrib_per_variance <- cares$col$contrib[,1,drop=FALSE]*cares$eig[1,1]+cares$col$contrib[,2,drop=FALSE]*cares$eig[2,1]

#significant_contrib <- contrib_per_variance[order(-contrib_per_variance[,1]), , drop=FALSE]

#significant_contrib <- contrib_per_variance[ order(-contrib_per_variance[,1]), ]



#contribution with means of two axes
means <- rowMeans(cares$col$contrib[,1:2])

means[means > 1.0]

significant_contrib <- sort(means, decreasing = TRUE)


significant10_contrib_names <- names(significant_contrib[1:10])
significant50_contrib_names <- names(significant_contrib[1:50])
significant100_contrib_names <- names(significant_contrib[1:100])
significant_MC_contrib_names <- names(significant_contrib[1:MC])

#find most frequent patterns for each character

cat("MOST FREQUENT PATTERN FOR EACH CHARACTER", "\n",  file = "analysis_new.out", append=FALSE)
for (k in most_freqs){
	full_patt <- pattern_names[k,1:1]
	cat(k,"\t",full_patt,"\t",significant_contrib[k],"\t","NONE","\t",get_freqs(k),"\n",  file = "analysis_new.out", append=TRUE)
}

#max(data)





plot(cares,invisible=c("col"), title="just characters")


plot(cares,selectCol=significant10_contrib_names, unselect=1, title="CA Factor Map - Contrib 10", autoLab="yes", col.row="black", col.col="grey70")

plot(cares,selectCol=significant50_contrib_names, unselect=1, title="CA Factor Map - Contrib 50", autoLab="yes", col.row="black", col.col="grey70")

plot(cares,selectCol=significant100_contrib_names, unselect=1, title="CA Factor Map - Contrib 100", autoLab="yes", col.row="black", col.col="grey70")

title_MC = cbind("CA Factor Map - Contrib", MC)

plot(cares,selectCol=significant_MC_contrib_names, unselect=1, title=title_MC, autoLab="yes", col.row="black", col.col="grey70")

plot(cares,selectCol=most_freqs, title="Most Freqs Absolute", autoLab="yes", col.row="black", col.col="grey70")



#vectors with the names of the N most significant patterns by contribution
significant10_contrib_names <- names(significant_contrib[0:10])

cat("FIRST TEN FOR CONTRIBUTION ABSOLUTE", "\n",  file = "analysis_new.out", append=TRUE)


for (k in significant10_contrib_names) {
	full_patt <- pattern_names[k,1:1]
	cat(k,"\t",full_patt,"\t",significant_contrib[k],"\t","NONE","\t",get_freqs(k),"\n",  file = "analysis_new.out", append=TRUE)

}


significant_contrib_names <-names(significant_contrib)

significant50_contrib_names <- names(significant_contrib[0:50])

significant100_contrib_names <- names(significant_contrib[0:MC])


significant_50_fullpatterns <-pattern_names[match(significant50_contrib_names, pattern_names$Pattern_Number), ]
significant_100_fullpatterns <-pattern_names[match(significant100_contrib_names, pattern_names$Pattern_Number), ]
significant_MC_fullpatterns <-pattern_names[match(significant_MC_contrib_names, pattern_names$Pattern_Number), ]

coordinates <-cares$col$coord[,1:2]

significant50_coords <-coordinates[rownames(coordinates) %in% significant50_contrib_names,] 

characters_coords <- cares$row$coord[,1:2]


names <-colnames(data)[2:nr_texts_plus_one]
cat("CA Rank","PATTERNID","PATTERN","CONTRIBUTION", names, names, "NearestTo" ,sep="\t","\n", file = "analysis_contrib.csv", append=FALSE)

i <- 1 

for (pattern in significant_MC_contrib_names) {
	full_patt <- pattern_names[pattern,1:1]
	cat(i,"\t",pattern,"\t",full_patt,"\t",significant_contrib[pattern],"\t",get_freqs(pattern),"\t",  file = "analysis_contrib.csv", append=TRUE)
	higher = 100
	nearest_to = ""
	for (character in rownames(characters_coords)) {
		distance <-(dist(rbind(characters_coords[character,],coordinates[pattern,])))
		cat(distance, "\t",  file = "analysis_contrib.csv", append=TRUE)
		if (distance < higher) {
			nearest_to = character
			higher = distance
		}
		else if (distance <= higher){
		nearest_to = nearest_to + character
		}
		
	}
	cat(nearest_to, "\t",  file = "analysis_contrib.csv", append=TRUE)	
	cat("\n",  file = "analysis_contrib.csv", append=TRUE)
	i <- i+1
}






#read names of patterns


dev.off()


