library(FactoMineR)
library(MASS)

 pdf("Results.pdf")

########################################################################################################
#USAGE
########################################################################################################

#PUT THIS IN THE FOLDER NAMED "MODELS"
#IT WILL WORK ON ALL FILES IN THAT FOLDER WITH EXTENTION .txt
#OUTPUTS
#GRAPHS, and two FILES


#SET MANUAL THRESHOLD FOR SIGNIFICANT PATTERNS
MC = 400

#PASS AS PARAMETER THE MAXIMUM SIZE OF CONTRIBUTIONS THAT YOU WANT TO SEE PRINTED
#THIS SCIRIPT WILL PRODUCE A PLOT FOR WITH THE FIRST 10, 50, 100, <N> MOST CONTRIBUTIVE PATTERNS, 
#WHERE <N> IS THE value of max_contrib

#IF CULLING = 1, THEN ONLY THE PATTERNS THAT ARE PRESENT IN ALL TEXTS ARE USED
CULLING = 0

#SHOULD WE CARRY OUT DYNAMIC VIZUALISATION ?
dynVizBool = 0


########################################################################################################
#DATA PREPARATION
########################################################################################################


#read data
files <- list.files(pattern = '*.txt')
files
import.list <-lapply(files, read.table, sep="", header=TRUE)
#import.list
data <- Reduce(function(x, y) merge(x, y, all=T, by= "Pattern_ID"), import.list, accumulate=F)

data2 <- data[,-1]
rownames(data2) <- data[,1]

nr_texts <- length(files)
nr_texts_plus_one <- length(files)+1


data_rownames <- data.frame(data[,-1], row.names=data[,1])
matrix <-data.matrix(data_rownames, rownames.force = NA)

#CULLING
#REMOVES ALL LINES WITH NA
#THIS REPLACES SMOOTHING AND KEEPS ONLY PATTERNS THAT ARE PRESENT IN ALL 

if (CULLING==1) {
   matrix <- matrix[complete.cases(matrix),]
   data <- data[complete.cases(data),]
} else {
	matrix[is.na(matrix)] <- 0
	data[is.na(data)] <- 0
}




pattern_ids<-data[,1:1]
rownames(data) <- pattern_ids


write.matrix(data, file = "data.csv", sep = "\t")


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


original_matrix <- matrix

#nomralize matrix
matrix <- sweep(matrix, 2, colSums(matrix), FUN="/")
matrix <- scale(matrix, center=FALSE, scale=colSums(matrix))



write.table(matrix, file = "data_normalised.csv", row.names=T, col.names=NA, sep = "\t")


normalized_matrix <- matrix

matrix <- t(matrix)

#original_matrix["Pattern_1002", "FredericDard_BeruBeru_patterns_mapped"]
#normalized_matrix["Pattern_1002", "FredericDard_BeruBeru_patterns_mapped"]


########################################################################################################
#FUNCTIONS
########################################################################################################



get_freqs <- function(key){
	freqs <-data[data$Pattern_ID==key,]
	freqs <- freqs[2:nr_texts_plus_one]
	return(paste(freqs,collapse="\t"))
}

get_absolute_count <- function(pattern,text_list){
	count = c()
	for (test in text_list){
		count <- c(count,original_matrix[pattern,test])
		}
	return(paste(count,collapse="\t"))

}


get_relative_count <- function(pattern,text_list){
	count = c()
	for (test in text_list){
		count <-c(count,normalized_matrix[pattern,test])
		}
	return(paste(count,collapse="\t"))

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



#calculate mean contribution for dimensions 1 and 2 for each pattern
contrib_dim12 <- rowMeans(cares$col$contrib[,1:2])

#print patterns with contribution over 1
contrib_dim12[contrib_dim12 > 1.0]

#calculate average contribution for all patterns used for CA
average_contrib <- mean(contrib_dim12)
average_contrib

#keep only patterns with contribution above average
contrib_dim12_aa <- contrib_dim12[contrib_dim12 > average_contrib]


#sort by contribution
significant_contrib_AA <- sort(contrib_dim12_aa, decreasing = TRUE)
significant_contrib <- sort(contrib_dim12, decreasing = TRUE)



significant10_contrib_names <- names(significant_contrib[1:10])

significant_MC_contrib_names <- names(significant_contrib[1:MC])


significant_AA_contrib_names <- names(significant_contrib_AA)




plot(cares,invisible=c("col"), title="CA Factor Map - just characters", autoLab="yes", col.row="black")


plot(cares,selectCol=significant10_contrib_names, unselect=1, title="CA Factor Map - Contrib 10", autoLab="yes", col.row="black", col.col="grey70")


title_MC = cbind("CA Factor Map - Contrib", MC)

plot(cares,selectCol=significant_MC_contrib_names, unselect=1, title=title_MC, autoLab="yes", col.row="black", col.col="grey70")


plot(cares,selectCol=significant_AA_contrib_names, unselect=1, title="CA Factor Map - Contrib Above average", autoLab="yes", col.row="black", col.col="grey70")

plot(cares,selectCol=most_freqs, title="Most Freqs Absolute", autoLab="yes", col.row="black", col.col="grey70")



#vectors with the names of the N most significant patterns by contribution
significant10_contrib_names <- names(significant_contrib[0:10])


significant_contrib_names <-names(significant_contrib)
significantMC_contrib_names <- names(significant_contrib[0:MC])



coordinates <-cares$col$coord[,1:2]
characters_coords <- cares$row$coord[,1:2]


names <-colnames(data)[2:nr_texts_plus_one]



#print results for all patterns above average

cat("CA Rank","PATTERNID","PATTERN","CONTRIBUTION", names, names, names, "NearestTo", "MostFreqIn" ,sep="\t","\n", file = "analysis_AA.csv", append=FALSE)

i <- 1 

for (pattern in significant_AA_contrib_names) {
	get_absolute_count(pattern,names)
	full_patt <- pattern_names[pattern,1:1]
	cat(i,"\t",pattern,"\t",full_patt,"\t",significant_contrib[pattern],"\t",get_absolute_count(pattern,names),"\t",get_relative_count(pattern,names),"\t",  file = "analysis_AA.csv", append=TRUE)
	higher = 100
	nearest_to = ""
	for (character in rownames(characters_coords)) {
		distance <-(dist(rbind(characters_coords[character,],coordinates[pattern,])))
		cat(distance, "\t",  file = "analysis_AA.csv", append=TRUE)
		if (distance < higher) {
			nearest_to = character
			higher = distance
		}
		else if (distance <= higher){
		nearest_to = nearest_to + character
		}
		
	}
	
	cat(nearest_to, "\t",  file = "analysis_AA.csv", append=TRUE)	

	most_freq_in = ""
	count=0
	for (text in names){
		freq = normalized_matrix[pattern,text]
		if (freq > count) {
			freq
			count = freq
			most_freq_in = text
		}
		
	}
	cat(most_freq_in, "\t",  file = "analysis_AA.csv", append=TRUE)	
	cat("\n",  file = "analysis_AA.csv", append=TRUE)
	i <- i+1
}



#print results for all patterns above MC

cat("CA Rank","PATTERNID","PATTERN","CONTRIBUTION", names, names, names, "NearestTo", "MostFreqIn" ,sep="\t","\n", file = "analysis_MC.csv", append=FALSE)

i <- 1 

for (pattern in significantMC_contrib_names) {
	get_absolute_count(pattern,names)
	full_patt <- pattern_names[pattern,1:1]
	cat(i,"\t",pattern,"\t",full_patt,"\t",significant_contrib[pattern],"\t",get_absolute_count(pattern,names),"\t",get_relative_count(pattern,names),"\t",  file = "analysis_MC.csv", append=TRUE)
	higher = 100
	nearest_to = ""
	for (character in rownames(characters_coords)) {
		distance <-(dist(rbind(characters_coords[character,],coordinates[pattern,])))
		cat(distance, "\t",  file = "analysis_MC.csv", append=TRUE)
		if (distance < higher) {
			nearest_to = character
			higher = distance
		}
		else if (distance <= higher){
		nearest_to = nearest_to + character
		}
		
	}
	
	cat(nearest_to, "\t",  file = "analysis_MC.csv", append=TRUE)	

	most_freq_in = ""
	count=0
	for (text in names){
		freq = normalized_matrix[pattern,text]
		if (freq > count) {
			freq
			count = freq
			most_freq_in = text
		}
		
	}
	cat(most_freq_in, "\t",  file = "analysis_MC.csv", append=TRUE)	
	cat("\n",  file = "analysis_MC.csv", append=TRUE)
	i <- i+1
}





#read names of patterns


dev.off()


# dynamic vizualisation by explor (plot and some summary stats should show up in your browser ...)
if(dynVizBool == 1)
{
	library(explor)
	explor(cares)
}
