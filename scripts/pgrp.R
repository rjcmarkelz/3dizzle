library(YplantQMC)
# get genotype specific leaf file
# make genotype specific plants 

setwd("/Users/Cody_2/git.repos/3D_reconstruction/data/Yplant")

?constructplant
R500_CR_HN <- constructplant("R500_CR_HN.p", "br_sh_hn_leaves.l")
str(R500_CR_HN)
plot(R500_CR_HN)

longleaf

R500_CR_HN$leafdata$area <-  1
R500_CR_HN$leafdata$area <- 20
plot(R500_CR_HN)

R500_CR_HN <- readp(pfile = "R500_CR_HN.p")
R500_CR_HN

#same basic shape for all treatments for now
R500_CR_LN <- R500_CR_HN
R500_UN_HN <- R500_CR_HN
R500_UN_LN <- R500_CR_HN

# field data


# phenotype data 
setwd("/Users/Cody_2/git.repos/brassica_meta_analysis/Cleaned_data")
?read.table
pheno <- read.table("all_traits.csv", sep = ",", na.strings = "NA")
head(pheno)
hist(pheno$relative_gr_rate_CR)
hist(pheno$relative_gr_rate)
hist(pheno$LeafLnUN)
hist(pheno$LeafLnCR)
hist(pheno$relative_gr_rate - pheno$relative_gr_rate_CR)
pheno$comp <- (pheno$relative_gr_rate - pheno$relative_gr_rate_CR)/(pheno$relative_gr_rate)
hist(pheno$comp)
head(pheno)
dim(pheno)
colnames(pheno) <- pheno[1,]
colnames(pheno)
pheno[,c(1,136)]
name <- as.character(pheno[1,1])
str(name)
colnames(pheno)

write.table(test, "test_plant.p", sep = "\t", row.names = FALSE)