#############################################################

# quiz1.R
#
# Author: Manas Trivedi, Roll no. 181CO231
#
# Time taken to write the code: 10-12 hours
# (spread over 3 days, 90% for debugging)
#
# Time taken for executing the code:
# - Solution 1:		6.361 secs
# - Solution 2: 	4.329735 secs
# - Solution 3: 	4.662559 secs
# - Innovation 1: 	4.687641 secs
# - Innovation 2: 	18.4528 secs
#
#############################################################
#
#
# PLEASE NOTE: The description of this code file is present
# in BDA_Quiz1_181CO231.pdf (attached in the submission)
#
#
#############################################################

# ---------------------Solution 1----------------------------

rm(list=ls())
library("sets")

startTime <- Sys.time()

Quiz1File <- "C:/Users/Manas/Learn/BDA/Quiz1/Quiz1.csv"
Quiz1Int <- read.csv(Quiz1File)
Quiz1Int <- unique(Quiz1Int)
print(nrow(Quiz1Int))
s <- set()
rowsForReview <- data.frame (
  STITCH = c(),
  Compound = c()
)

for (row in 1:nrow(Quiz1Int)) {
  if (set_contains_element(s, Quiz1Int[row, "Compound"])) {
    rowsForReview <- rbind(rowsForReview, Quiz1Int[row,])
  }
  else {
    s <- set_union(s, set(Quiz1Int[row, "Compound"]))
  }
}
print(head(rowsForReview))
print(unique(rowsForReview$Compound))

for(row in 1:nrow(Quiz1Int)) {
  if (Quiz1Int[row, "Compound"] == "") {
    rowsForReview <- rbind(Quiz1Int[row,], rowsForReview)
    break
  }
}

timeTakenForSoln1 <- Sys.time() - startTime
print(timeTakenForSoln1)

# ---------------------Solution 2----------------------------

rm(list=ls())

startTime <- Sys.time()

Quiz1File <- "C:/Users/Manas/Learn/BDA/Quiz1/Quiz1.csv"
Quiz1Int <- read.csv(Quiz1File)
Quiz1Int <- unique(Quiz1Int)
print(nrow(Quiz1Int))

rowsForReview <- Quiz1Int[duplicated(Quiz1Int$Compound),]

print(head(rowsForReview))
print(unique(rowsForReview$Compound))

for(row in 1:nrow(Quiz1Int)) {
  if (Quiz1Int[row, "Compound"] == "") {
    rowsForReview <- rbind(Quiz1Int[row,], rowsForReview)
    break
  }
}

timeTakenForSoln2 <- Sys.time() - startTime
print(timeTakenForSoln2)

# ---------------------Solution 3----------------------------

rm(list=ls())

startTime <- Sys.time()

Quiz1File <- "C:/Users/Manas/Learn/BDA/Quiz1/Quiz1.csv"
Quiz1Int <- read.csv(Quiz1File)
Quiz1Int <- unique(Quiz1Int)

rowsForReview <- Quiz1Int[!nzchar(Quiz1Int$Compound) | is.na(Quiz1Int$Compound),]

timeTakenForSoln3 <- Sys.time() - startTime
print(timeTakenForSoln3)

# ---------------------Innovation 1----------------------------

rm(list=ls())

startTime <- Sys.time()

Quiz1File <- "C:/Users/Manas/Learn/BDA/Quiz1/Quiz1.csv"
Quiz1Int <- read.csv(Quiz1File)
Quiz1Int <- unique(Quiz1Int)

rowsForReview <- Quiz1Int[!nzchar(Quiz1Int$Compound) | is.na(Quiz1Int$Compound),]

nrowVector <- c(nrow(Quiz1Int) - nrow(rowsForReview), nrow(rowsForReview))

label1 <- paste("STITCH-Compound Matches (", nrow(Quiz1Int) - nrow(rowsForReview), ")")
label2 <- paste("Unmatched STITCH (", nrow(rowsForReview), ")")
labelVector <- c(label1, label2)

jpeg(file="C:/Users/Manas/Learn/BDA/Quiz1/barplot.jpeg")
barplot(nrowVector, names.arg = labelVector, main = "STITCH-Compound Matches and Unmatched STITCH")
dev.off()

jpeg(file="C:/Users/Manas/Learn/BDA/Quiz1/pie.jpeg")
pie(nrowVector, labels = labelVector, main = "STITCH-Compound Matches and Unmatched STITCH")
dev.off()

timeTakenForInno1 <- Sys.time() - startTime
print(timeTakenForInno1)

# ---------------------Innovation 2----------------------------

rm(list=ls())

startTime <- Sys.time()

DrugSideEffect1File <- "C:/Users/Manas/Learn/BDA/Quiz1/ChSe-Decagon_monopharmacy.csv"
DrugSideEffect1Int <- read.csv(DrugSideEffect1File)
DrugSideEffect1Int <- unique(DrugSideEffect1Int)

DrugSideEffect2File <- "C:/Users/Manas/Learn/BDA/Quiz1/ChChSe-Decagon_polypharmacy.csv"
DrugSideEffect2Int <- read.csv(DrugSideEffect2File)
DrugSideEffect2Int <- unique(DrugSideEffect2Int)

drugsWithNoDrugDrugInteraction <- DrugSideEffect1Int[!((DrugSideEffect1Int$X..STITCH %in% DrugSideEffect2Int$X..STITCH.1) | (DrugSideEffect1Int$X..STITCH %in% DrugSideEffect2Int$STITCH.2)),]

dim(drugsWithNoDrugDrugInteraction)

timeTakenForInno2 <- Sys.time() - startTime
print(timeTakenForInno2)
