library(YplantQMC)
library(reshape2)
library(ggplot2)
# get genotype specific leaf file
# make genotype specific plants 

setwd("/Users/Cody_2/git.repos/3D_reconstruction/data/Yplant")

# ?constructplant
# R500_CR_HN <- constructplant("R500_CR_HN.p", "br_sh_hn_leaves.l")
# str(R500_CR_HN)
# plot(R500_CR_HN)

# longleaf

# R500_CR_HN$leafdata$area <-  1
# R500_CR_HN$leafdata$area <- 20
# plot(R500_CR_HN)

R500_CR_HN <- readp(pfile = "R500_CR_HN.p")


#elongate internodes
R500_CR_HN[,7] <- c(0, 0, 0, 10, 20, 25, 30, 30, 30, 30, 15, 10, 5)

#same basic shape for all treatments for now
R500_CR_LN <- R500_CR_HN
R500_UN_HN <- R500_CR_HN
R500_UN_LN <- R500_CR_HN

# field data
x <- 175
R500_UN_HN[,20] <- c(0, 0, 0, (x*.15), (x*.15), (x*.5), (x*.75), (x*.75), x, (x*.75), (x*.45), (x*.25), (x*.15))
R500_UN_HN

x <- 160
R500_UN_LN[,20] <- c(0, 0, 0, (x*.15), (x*.15), (x*.5), (x*.75), (x*.75), x, (x*.75), (x*.45), (x*.25), (x*.15))
R500_UN_LN

x <- 90
R500_CR_LN[,20] <- c(0, 0, 0, (x*.15), (x*.15), (x*.5), (x*.75), (x*.75), x, (x*.75), (x*.45), (x*.25), (x*.15))
R500_CR_LN

x <- 115
R500_CR_HN[,20] <- c(0, 0, 0, (x*.15), (x*.15), (x*.5), (x*.75), (x*.75), x, (x*.75), (x*.45), (x*.25), (x*.15))
R500_CR_HN

setwd("/Users/Cody_2/git.repos/3D_reconstruction/data/Yplant")
write.table(R500_CR_HN, "R500_CR_HN.p", sep = "\t", row.names = FALSE)
R500_CR_HN_plant <- constructplant("R500_CR_HN.p", "br_sh_hn_leaves.l")
plot(R500_CR_HN_plant)

write.table(R500_CR_LN, "R500_CR_LN.p", sep = "\t", row.names = FALSE)
R500_CR_LN_plant <- constructplant("R500_CR_LN.p", "br_sh_hn_leaves.l")
plot(R500_CR_LN_plant)

write.table(R500_UN_HN, "R500_UN_HN.p", sep = "\t", row.names = FALSE)
R500_UN_HN_plant <- constructplant("R500_UN_HN.p", "br_sh_hn_leaves.l")
plot(R500_UN_HN_plant)

write.table(R500_UN_LN, "R500_UN_LN.p", sep = "\t", row.names = FALSE)
R500_UN_LN_plant <- constructplant("R500_UN_LN.p", "br_sh_hn_leaves.l")
plot(R500_UN_LN_plant)

R500_stand_CR_HN <- makeStand(list(R500_CR_HN_plant,R500_CR_HN_plant,R500_CR_HN_plant,R500_CR_HN_plant,R500_CR_HN_plant),
                       xyz=data.frame(x=c(-50,50,0,-50,50),
                                      y=c(-50,50,0,50,-50),
                                      z=c(0,0,0,0,0)))

plot(R500_stand_CR_HN)

R500_stand_CR_LN <- makeStand(list(R500_CR_LN_plant,R500_CR_LN_plant,R500_CR_LN_plant,R500_CR_LN_plant,R500_CR_LN_plant),
                       xyz=data.frame(x=c(-50,50,0,-50,50),
                                      y=c(-50,50,0,50,-50),
                                      z=c(0,0,0,0,0)))


R500_stand_UN_LN <- makeStand(list(R500_UN_LN_plant,R500_UN_LN_plant,R500_UN_LN_plant,R500_UN_LN_plant,R500_UN_LN_plant),
                       xyz=data.frame(x=c(-200,200,0,-200,200),
                                      y=c(-200,200,0,200,-200),
                                      z=c(0,0,0,0,0)))

R500_stand_UN_HN <- makeStand(list(R500_UN_HN_plant,R500_UN_HN_plant,R500_UN_HN_plant,R500_UN_HN_plant,R500_UN_HN_plant),
                       xyz=data.frame(x=c(-200,200,0,-200,200),
                                      y=c(-200,200,0,200,-200),
                                      z=c(0,0,0,0,0)))


plot(R500_stand_UN_LN)

R500_stand_CR_HN_sim <- runYplant(R500_stand_CR_HN, fbeam=1, altitude=90, azimuth=0, reflec=0.15, transmit=0.1, intern = FALSE, rewriteplantfile = TRUE)
R500_stand_CR_LN_sim <- runYplant(R500_stand_CR_LN, fbeam=1, altitude=90, azimuth=0, reflec=0.15, transmit=0.1, intern = FALSE, rewriteplantfile = TRUE)
R500_stand_UN_HN_sim <- runYplant(R500_stand_UN_HN, fbeam=1, altitude=90, azimuth=0, reflec=0.15, transmit=0.1, intern = FALSE, rewriteplantfile = TRUE)
R500_stand_UN_LN_sim <- runYplant(R500_stand_UN_LN, fbeam=1, altitude=90, azimuth=0, reflec=0.15, transmit=0.1, intern = FALSE, rewriteplantfile = TRUE)


head(R500_stand_CR_HN_sim) 
head(R500_stand_CR_LN_sim)

R500_stand_CR_HN_sim
R500_stand_CR_HN_sim3 <- subset(R500_stand_CR_HN_sim, plantnr == 3)
R500_stand_CR_HN_sim3$leaf <- 1:10

R500_stand_CR_LN_sim
R500_stand_CR_LN_sim3 <- subset(R500_stand_CR_LN_sim, plantnr == 3)
R500_stand_CR_LN_sim3$leaf <- 1:10

R500_stand_UN_LN_sim
R500_stand_UN_LN_sim3 <- subset(R500_stand_UN_LN_sim, plantnr == 3)
R500_stand_UN_LN_sim3$leaf <- 1:10

R500_stand_UN_HN_sim
R500_stand_UN_HN_sim3 <- subset(R500_stand_UN_HN_sim, plantnr == 3)
R500_stand_UN_HN_sim3$leaf <- 1:10

R500merged <- as.data.frame(cbind(R500_stand_UN_HN_sim3[,3], R500_stand_CR_HN_sim3[,3], 
                         R500_stand_UN_LN_sim3[,3], R500_stand_CR_LN_sim3[,3]))
names(R500merged) <- c("UN_HN", "CR_HN", "UN_LN", "CR_LN")
R500merged$leaf <- 1:10


R500merged <- melt(R500merged, id.vars = "leaf")
R500merged

R500PAR <- ggplot(R500merged) + 
  geom_line(aes(x = leaf, y = value, color = variable), size = 3) +
  scale_colour_manual(values=c("black", "red", "blue", "green"),
    name ="PAR", labels=c("UN_HN", "CR_HN", "UN_LN", "CR_LN")) +
  scale_x_discrete("leaf") +  xlab("Leaf Number") + ylab("PAR umol photons s-1") +
  theme(axis.title.x = element_text(face="bold", size=20),
           axis.text.x  = element_text(size=16),
           axis.title.y = element_text(face="bold", size=20),
           axis.text.y  = element_text(size=16))
R500PAR








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

# relative growth potential from field data
# competition models for each
# database
# metabolic model?
# 






write.table(test, "test_plant.p", sep = "\t", row.names = FALSE)