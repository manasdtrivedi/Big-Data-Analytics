#############################################################
#
# tutorial.R
#
# Author: Manas Trivedi, Roll no. 181CO231
#
# Time taken to design the EDA method + write the code: 10-12 hours
# (spread over 6 days)
#
# Time taken for executing the code: 9.398002 seconds
#
#
#############################################################
#
#
# PLEASE NOTE: The description of this code file is present
# in BDA_Tutorial_181CO231.pdf (attached in the submission)
#
#
#############################################################

rm(list=ls())

startTime <- Sys.time()

data <- readr::read_csv("C:/Users/Manas/Learn/BDA/Tutorial/lish_moa_annotated.csv.zip")

print(nrow(data)) # 27796
print(ncol(data)) # 1486

# Description of columns:
# sig_id            : Experiment ID
# drug_id           : Drug ID
# training          : True if row is from train set, False for test set
# cp_type           : trt_cp if drug is real, ctl_vehicle if drug is vehicle
# cp_time           : Sample time (24/48/72 hrs)
# cp_dose           : Dosage of drug (D1/D2)
# g-*               : Normalized gene expression level of a particular gene
# c-*               : Normalized cell viability of a particular cell line
# <remaining cols>  : Mechanisms of Action, 1 if MoA is associated with experiment, 0 otherwise

# Finding if each sig_id is unique
print(length(unique(data$sig_id)))

# Finding number of genes
print(sum(grepl("^g-", colnames(data)))) # 772, thus genes from index 7 to 778

# Finding number of cell lines
print(sum(grepl("^c-", colnames(data)))) # 100, thus cell lines from index 779 to 878

# Remaining columns are for MoAs, from index 879 to 1486

# -------------------------- Visualizing frequency of MoAs -----------------------------------------

moaFreqVector <- colSums(data[879:1486], na.rm = TRUE)

jpeg(file="C:/Users/Manas/Learn/BDA/Tutorial/top25MoAs.jpeg")
par(mar=c(20, 4, 4, 2))
barplot(sort(moaFreqVector, decreasing = TRUE)[1:25], main = "Top 25 MoAs", ylab = "Frequency", las = 2)
dev.off()

jpeg(file="C:/Users/Manas/Learn/BDA/Tutorial/bottom25MoAs.jpeg")
par(mar=c(20, 4, 4, 2))
barplot(sort(moaFreqVector, decreasing = FALSE)[1:25], main = "Bottom 25 MoAs", ylab = "Frequency", las = 2)
dev.off()

length(moaFreqVector[which(moaFreqVector == 0)])

jpeg(file="C:/Users/Manas/Learn/BDA/Tutorial/bottom25NonZeroMoAs.jpeg")
par(mar=c(20, 4, 4, 2))
barplot(sort(moaFreqVector[moaFreqVector != 0], decreasing = FALSE)[1:25], main = "Bottom 25 Non-Zero MoAs", ylab = "Frequency", las = 2)
dev.off()

# Around a 3.5% of the samples (832 rows in the dataset) correspond to nuclear factor kappa B (NF-??B) inhibitors, 
# followed by a 3% of entries corresponding to proteasome inhibitors. 
# These are two common targets of many anti-cancer compounds, 
# so it's not strange to find in the dataset many entries with these MoAs.

# ------------------ Visualizing distribution of cp_type ---------------------------

cpTypeVector <- data[["cp_type"]]

# jpeg(file="C:/Users/Manas/Learn/BDA/Tutorial/cpTypeDistribution.jpeg")
# barplot(c(length(cpTypeVector[grepl('trt_cp', cpTypeVector)]), length(cpTypeVector[grepl('ctl_vehicle', cpTypeVector)])), names.arg = c("trt_cp", "ctl_vehicle"), main = "cp_type Distribution")
# dev.off()

jpeg(file="C:/Users/Manas/Learn/BDA/Tutorial/cpTypeDistributionPie.jpeg")
pie(c(length(cpTypeVector[grepl('trt_cp', cpTypeVector)]), length(cpTypeVector[grepl('ctl_vehicle', cpTypeVector)])), labels = c("trt_cp", "ctl_vehicle"), main = "cp_type Distribution")
dev.off()

# ------------------ Visualizing distribution of cp_time ---------------------------

cpTimeVector <- data[["cp_time"]]

jpeg(file="C:/Users/Manas/Learn/BDA/Tutorial/cpTimeDistributionPie.jpeg")
pie(c(length(cpTimeVector[grepl('24', cpTimeVector)]), length(cpTimeVector[grepl('48', cpTimeVector)]), length(cpTimeVector[grepl('72', cpTimeVector)])), labels = c("24 hrs", "48 hrs", "72 hrs"), main = "cp_time Distribution")
dev.off()

jpeg(file="C:/Users/Manas/Learn/BDA/Tutorial/cpTimeDistributionBar.jpeg")
barplot(c(length(cpTimeVector[grepl('24', cpTimeVector)]), length(cpTimeVector[grepl('48', cpTimeVector)]), length(cpTimeVector[grepl('72', cpTimeVector)])), names.arg = c("24 hrs", "48 hrs", "72 hrs"), main = "cp_time Distribution", ylab = "Frequency")
dev.off()

# ------------------ Visualizing distribution of cp_dose ---------------------------

cpDoseVector <- data[["cp_dose"]]

jpeg(file="C:/Users/Manas/Learn/BDA/Tutorial/cpDoseDistributionPie.jpeg")
pie(c(length(cpDoseVector[grepl('D1', cpDoseVector)]), length(cpDoseVector[grepl('D2', cpDoseVector)])), labels = c("D1", "D2"), main = "cp_dose Distribution")
dev.off()

# ------------------ Visualizing variances of genes ---------------------------

library("matrixStats")

geneVarianceVector <- colVars(as.matrix(data[7:778]), na.rm = TRUE)
names(geneVarianceVector) <- colnames(data[7:778])

jpeg(file="C:/Users/Manas/Learn/BDA/Tutorial/top25GeneVariances.jpeg")
par(mar=c(20, 4, 4, 2))
barplot(sort(geneVarianceVector, decreasing = TRUE)[1:25], main = "Top 25 Gene Variances", ylab = "Variance", las = 2)
dev.off()

jpeg(file="C:/Users/Manas/Learn/BDA/Tutorial/bottom25GeneVariances.jpeg")
par(mar=c(20, 4, 4, 2))
barplot(sort(geneVarianceVector, decreasing = FALSE)[1:25], main = "Bottom 25 Gene Variances", ylab = "Variance", las = 2)
dev.off()

# ------------------ Visualizing distribution of values for top 3 genes ---------------------------

jpeg(file="C:/Users/Manas/Learn/BDA/Tutorial/valueDistributionOfg-COL1A1_1277.jpeg")
plot(density(data[["g-COL1A1_1277"]]), main = "Value Distribution of g-COL1A1_1277")
dev.off()

jpeg(file="C:/Users/Manas/Learn/BDA/Tutorial/valueDistributionOfg-CCNA2_890.jpeg")
plot(density(data[["g-CCNA2_890"]]), main = "Value Distribution of g-CCNA2_890")
dev.off()

jpeg(file="C:/Users/Manas/Learn/BDA/Tutorial/valueDistributionOfg-BUB1B_701.jpeg")
plot(density(data[["g-BUB1B_701"]]), main = "Value Distribution of g-BUB1B_701")
dev.off()

# ------------------ Visualizing distribution of values for bottom 3 genes ---------------------------

jpeg(file="C:/Users/Manas/Learn/BDA/Tutorial/valueDistributionOfg-NOS3_4846.jpeg")
plot(density(data[["g-NOS3_4846"]]), main = "Value Distribution of g-NOS3_4846")
dev.off()

jpeg(file="C:/Users/Manas/Learn/BDA/Tutorial/valueDistributionOfg-RPS6_6194.jpeg")
plot(density(data[["g-RPS6_6194"]]), main = "Value Distribution of g-RPS6_6194")
dev.off()

jpeg(file="C:/Users/Manas/Learn/BDA/Tutorial/valueDistributionOfg-E2F2_1870.jpeg")
plot(density(data[["g-E2F2_1870"]]), main = "Value Distribution of g-E2F2_1870")
dev.off()

# ------------------ Visualizing variances of cell viability ---------------------------

cellVarianceVector <- colVars(as.matrix(data[779:878]), na.rm = TRUE)
names(cellVarianceVector) <- colnames(data[779:878])

jpeg(file="C:/Users/Manas/Learn/BDA/Tutorial/top25CellVariances.jpeg")
par(mar=c(20, 4, 4, 2))
barplot(sort(cellVarianceVector, decreasing = TRUE)[1:25], main = "Top 25 Cell Variances", ylab = "Variance", las = 2)
dev.off()

jpeg(file="C:/Users/Manas/Learn/BDA/Tutorial/bottom25CellVariances.jpeg")
par(mar=c(20, 4, 4, 2))
barplot(sort(cellVarianceVector, decreasing = FALSE)[1:25], main = "Bottom 25 Cell Variances", ylab = "Variance", las = 2)
dev.off()

# ------------------ Visualizing distribution of values for bottom 3 cells ---------------------------

jpeg(file="C:/Users/Manas/Learn/BDA/Tutorial/valueDistributionOfc-74-OV7_OVARY.jpeg")
plot(density(data[["c-74-OV7_OVARY"]]), main = "Value Distribution of c-74-OV7_OVARY")
dev.off()

jpeg(file="C:/Users/Manas/Learn/BDA/Tutorial/valueDistributionOfc-58-SW1353_BONE.jpeg")
plot(density(data[["c-58-SW1353_BONE"]]), main = "Value Distribution of c-58-SW1353_BONE")
dev.off()

jpeg(file="C:/Users/Manas/Learn/BDA/Tutorial/valueDistributionOfc-37-SJSA1_BONE.jpeg")
plot(density(data[["c-37-SJSA1_BONE"]]), main = "Value Distribution of c-37-SJSA1_BONE")
dev.off()


# ------------------ Finding most common MoAs of drugs effective against OV7 cancer ---------------------------

exptsWithReducedOV7 <- data[data$`c-74-OV7_OVARY` <= -2, ]
moaFreqVector <- colSums(exptsWithReducedOV7[879:1486], na.rm = TRUE)

jpeg(file="C:/Users/Manas/Learn/BDA/Tutorial/top5MoAsForReducingOV7Cancer.jpeg")
par(mar=c(20, 4, 4, 2))
barplot(sort(moaFreqVector, decreasing = TRUE)[1:5], main = "Top 5 MoAs For Reducing OV7 Cancer", ylab = "Frequency", las = 2)
dev.off()

# ------------------ Visualizing distribution of values for top 3 cells ---------------------------

jpeg(file="C:/Users/Manas/Learn/BDA/Tutorial/valueDistributionOfc-63-SNU886_LIVER.jpeg")
plot(density(data[["c-63-SNU886_LIVER"]]), main = "Value Distribution of c-63-SNU886_LIVER")
dev.off()

jpeg(file="C:/Users/Manas/Learn/BDA/Tutorial/valueDistributionOfc-38-A2058_SKIN.jpeg")
plot(density(data[["c-38-A2058_SKIN"]]), main = "Value Distribution of c-38-A2058_SKIN")
dev.off()

jpeg(file="C:/Users/Manas/Learn/BDA/Tutorial/valueDistributionOfc-18-YD10B_UPPER_AERODIGESTIVE_TRACT.jpeg")
plot(density(data[["c-18-YD10B_UPPER_AERODIGESTIVE_TRACT"]]), main = "Value Distribution of c-18-YD10B_UPPER_AERODIGESTIVE_TRACT")
dev.off()

# ------------------ Finding most common MoAs of drugs effective against SNU886 cancer ---------------------------

exptsWithReducedSNU886 <- data[data$`c-63-SNU886_LIVER` <= -2, ]
moaFreqVector <- colSums(exptsWithReducedSNU886[879:1486], na.rm = TRUE)

jpeg(file="C:/Users/Manas/Learn/BDA/Tutorial/top5MoAsForReducingSNU886Cancer.jpeg")
par(mar=c(20, 4, 4, 2))
barplot(sort(moaFreqVector, decreasing = TRUE)[1:5], main = "Top 5 MoAs For Reducing SNU886 Cancer", ylab = "Frequency", las = 2)
dev.off()

# ------------------ Finding most common categories of MoAs ---------------------------

moaDataFrame <- data[879:1486]
colnames(moaDataFrame) <- sub("^.*_", "", colnames(moaDataFrame))
moaCategoryFreqVector <- colSums(moaDataFrame, na.rm = TRUE)
moaCategoryFreqVector <- tapply(moaCategoryFreqVector, names(moaCategoryFreqVector), sum)

jpeg(file="C:/Users/Manas/Learn/BDA/Tutorial/top5MoACategories.jpeg")
par(mar=c(20, 4, 4, 2))
barplot(sort(moaCategoryFreqVector, decreasing = TRUE)[1:5], main = "Top 5 MoA Categories", ylab = "Frequency", las = 2)
dev.off()

executionTime <- Sys.time() - startTime
print(executionTime)
