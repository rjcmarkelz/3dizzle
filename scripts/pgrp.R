library(YplantQMC)
library(reshape2)
library(ggplot2)
# get genotype specific leaf file
# make genotype specific plants 

setwd("/Users/Cody_2/git.repos/3D_reconstruction/data/Yplant")

# ?constructplant
R93_CR_HN <- constructplant("R500_CR_HN.p", "br_sh_hn_leaves.l")
# str(R500_CR_HN)
plot(R93_CR_HN)

test <- runYplant(R93_CR_HN)
test

# longleaf

# R500_CR_HN$leafdata$area <-  1
# R500_CR_HN$leafdata$area <- 20
# plot(R500_CR_HN)

R500_CR_HN <- readp(pfile = "R500_CR_HN.p")
plot(R500_CR_HN)

#elongate internodes
R500_CR_HN[,7] <- c(0, 0, 0, 10, 20, 25, 30, 30, 30, 30, 15, 10, 5)

# same basic shape for all treatments for now
# very substaintially based on treatment, but need to constrain
R500_CR_LN <- R500_CR_HN
R500_UN_HN <- R500_CR_HN
R500_UN_LN <- R500_CR_HN
plot(R500_CR_LN)


IMB211_CR_HN <- R500_CR_HN
IMB211_CR_LN <- R500_CR_HN
IMB211_UN_HN <- R500_CR_HN
IMB211_UN_LN <- R500_CR_HN

# field data
# use field data as parameters for leaf length
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
plot(R500_stand_UN_HN)
# make large stand with all plants
R500_stand_total <- makeStand(list(R500_UN_HN_plant,R500_UN_HN_plant,R500_UN_HN_plant,R500_UN_HN_plant,R500_UN_HN_plant,
                                   R500_UN_LN_plant,R500_UN_LN_plant,R500_UN_LN_plant,R500_UN_LN_plant,R500_UN_LN_plant,
                                   R500_CR_HN_plant,R500_CR_HN_plant,R500_CR_HN_plant,R500_CR_HN_plant,R500_CR_HN_plant
                                   #R500_CR_LN_plant,R500_CR_LN_plant,R500_CR_LN_plant,R500_CR_LN_plant,R500_CR_LN_plant),
                                   ),
                       xyz=data.frame(x=c(-200,200,0,-200,200, 600,600,800,1000,1000, -50,50,0,-50,50),
                                      y=c(-200,200,0,200,-200, 200,-200,0,200,-200,   -650,-550,-600,-550,-650),
                                      z=c(0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0)))
plot(R500_stand_total)

#please pardon the terrible formatting
R500_stand_total <- makeStand(list(R500_UN_HN_plant,
                                   R500_UN_LN_plant,
                                   R500_CR_HN_plant,R500_CR_HN_plant,R500_CR_HN_plant,R500_CR_HN_plant,R500_CR_HN_plant,
                                   R500_CR_LN_plant,R500_CR_LN_plant,R500_CR_LN_plant,R500_CR_LN_plant,R500_CR_LN_plant),
                       xyz=data.frame(x=c(0,350, -50,50,0,-50,50,          300,400,350,300,400),
                                      y=c(0,0,   -400,-300,-350,-300,-400, -300,-300,-350,-400,-400),
                                      z=c(0,0, 0,0,0,0,0, 0,0,0,0,0)))
plot(R500_stand_total)


R500_stand_total <- makeStand(list(R500_UN_HN_plant,
                                   R500_UN_LN_plant,
                                   R500_CR_HN_plant,R500_CR_HN_plant,R500_CR_HN_plant,R500_CR_HN_plant,R500_CR_HN_plant,
                                   R500_CR_LN_plant,R500_CR_LN_plant,R500_CR_LN_plant,R500_CR_LN_plant,R500_CR_LN_plant),
                       xyz=data.frame(x=c(0,350, -50,50,0,-50,50,          300,400,350,300,400),
                                      y=c(0,0,   -400,-300,-350,-300,-400, -300,-300,-350,-400,-400),
                                      z=c(0,0, 0,0,0,0,0, 0,0,0,0,0)))
plot(R500_stand_total)
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
  scale_x_discrete("leaf") +  xlab("Leaf Number") + ylab("PAR (umol m-2 s-1)") +
  theme(axis.title.x = element_text(face="bold", size=20),
           axis.text.x  = element_text(size=16),
           axis.title.y = element_text(face="bold", size=20),
           axis.text.y  = element_text(size=16))
R500PAR


R500_LA_merged <- as.data.frame(cbind(R500_stand_UN_HN_sim3[,10], R500_stand_CR_HN_sim3[,10], 
                         R500_stand_UN_LN_sim3[,10], R500_stand_CR_LN_sim3[,10]))
names(R500_LA_merged) <- c("UN_HN", "CR_HN", "UN_LN", "CR_LN")
R500_LA_merged$leaf <- 1:10


R500_LA_merged <- melt(R500_LA_merged, id.vars = "leaf")
R500_LA_merged[3] <- R500_LA_merged[3]/1e+06
R500_LA_merged[3]


R500_LA <- ggplot(R500_LA_merged) + 
  geom_line(aes(x = leaf, y = value, color = variable), size = 3) +
  scale_colour_manual(values=c("black", "red", "blue", "green"),
    name ="PAR", labels=c("UN_HN", "CR_HN", "UN_LN", "CR_LN")) +
  scale_x_discrete("leaf") +  xlab("Leaf Number") + ylab("Sunlit Leaf Area (m^2)") +
  scale_y_continuous(limits=c(0, 0.0085)) + 
  theme(axis.title.x = element_text(face="bold", size=20),
           axis.text.x  = element_text(size=16),
           axis.title.y = element_text(face="bold", size=20),
           axis.text.y  = element_text(size=16))
R500_LA



#same basic shape for all treatments for now
R93_CR_HN <- R500_CR_HN
#shorten internodes
R93_CR_HN[,7] <- c(0, 0, 0, 5, 12, 12, 20, 20, 20, 20, 10, 5, 3)
R93_CR_LN <- R93_CR_HN
R93_UN_HN <- R93_CR_HN
R93_UN_LN <- R93_CR_HN

# field data
x <- 120
R93_UN_HN[,20] <- c(0, 0, 0, (x*.15), (x*.15), (x*.5), (x*.75), (x*.75), x, (x*.75), (x*.45), (x*.25), (x*.15))
R93_UN_HN

x <- 90
R93_CR_HN[,20] <- c(0, 0, 0, (x*.15), (x*.15), (x*.5), (x*.75), (x*.75), x, (x*.75), (x*.45), (x*.25), (x*.15))
R93_CR_HN

x <- 75
R93_UN_LN[,20] <- c(0, 0, 0, (x*.15), (x*.15), (x*.5), (x*.75), (x*.75), x, (x*.75), (x*.45), (x*.25), (x*.15))
R93_UN_LN

x <- 60
R93_CR_LN[,20] <- c(0, 0, 0, (x*.15), (x*.15), (x*.5), (x*.75), (x*.75), x, (x*.75), (x*.45), (x*.25), (x*.15))
R93_CR_LN

setwd("/Users/Cody_2/git.repos/3D_reconstruction/data/Yplant")
write.table(R93_CR_HN, "R93_CR_HN.p", sep = "\t", row.names = FALSE)
R93_CR_HN_plant <- constructplant("R93_CR_HN.p", "br_sh_hn_leaves.l")
plot(R93_CR_HN_plant)

write.table(R93_CR_LN, "R93_CR_LN.p", sep = "\t", row.names = FALSE)
R93_CR_LN_plant <- constructplant("R93_CR_LN.p", "br_sh_hn_leaves.l")
plot(R93_CR_LN_plant)

write.table(R93_UN_HN, "R93_UN_HN.p", sep = "\t", row.names = FALSE)
R93_UN_HN_plant <- constructplant("R93_UN_HN.p", "br_sh_hn_leaves.l")
plot(R93_UN_HN_plant)

write.table(R93_UN_LN, "R93_UN_LN.p", sep = "\t", row.names = FALSE)
R93_UN_LN_plant <- constructplant("R93_UN_LN.p", "br_sh_hn_leaves.l")
plot(R93_UN_LN_plant)

R93_stand_CR_HN <- makeStand(list(R93_CR_HN_plant,R93_CR_HN_plant,R93_CR_HN_plant,R93_CR_HN_plant,R93_CR_HN_plant),
                       xyz=data.frame(x=c(-50,50,0,-50,50),
                                      y=c(-50,50,0,50,-50),
                                      z=c(0,0,0,0,0)))

plot(R93_stand_CR_HN)

R93_stand_CR_LN <- makeStand(list(R93_CR_LN_plant,R93_CR_LN_plant,R93_CR_LN_plant,R93_CR_LN_plant,R93_CR_LN_plant),
                       xyz=data.frame(x=c(-50,50,0,-50,50),
                                      y=c(-50,50,0,50,-50),
                                      z=c(0,0,0,0,0)))


R93_stand_UN_LN <- makeStand(list(R93_UN_LN_plant,R93_UN_LN_plant,R93_UN_LN_plant,R93_UN_LN_plant,R93_UN_LN_plant),
                       xyz=data.frame(x=c(-200,200,0,-200,200),
                                      y=c(-200,200,0,200,-200),
                                      z=c(0,0,0,0,0)))

R93_stand_UN_HN <- makeStand(list(R93_UN_HN_plant,R93_UN_HN_plant,R93_UN_HN_plant,R93_UN_HN_plant,R93_UN_HN_plant),
                       xyz=data.frame(x=c(-200,200,0,-200,200),
                                      y=c(-200,200,0,200,-200),
                                      z=c(0,0,0,0,0)))



plot(R93_stand_UN_LN)

R93_stand_CR_HN_sim <- runYplant(R93_stand_CR_HN, fbeam=1, altitude=90, azimuth=0, reflec=0.15, transmit=0.1, intern = FALSE, rewriteplantfile = TRUE)
R93_stand_CR_LN_sim <- runYplant(R93_stand_CR_LN, fbeam=1, altitude=90, azimuth=0, reflec=0.15, transmit=0.1, intern = FALSE, rewriteplantfile = TRUE)
R93_stand_UN_HN_sim <- runYplant(R93_stand_UN_HN, fbeam=1, altitude=90, azimuth=0, reflec=0.15, transmit=0.1, intern = FALSE, rewriteplantfile = TRUE)
R93_stand_UN_LN_sim <- runYplant(R93_stand_UN_LN, fbeam=1, altitude=90, azimuth=0, reflec=0.15, transmit=0.1, intern = FALSE, rewriteplantfile = TRUE)

R93_stand_CR_HN_sim
R93_stand_CR_HN_sim3 <- subset(R93_stand_CR_HN_sim, plantnr = 3)
R93_stand_CR_HN_sim3$leaf <- 1:10

R93_stand_CR_LN_sim
R93_stand_CR_LN_sim3 <- subset(R93_stand_CR_LN_sim, plantnr == 3)
R93_stand_CR_LN_sim3$leaf <- 1:10

R93_stand_UN_LN_sim
R93_stand_UN_LN_sim3 <- subset(R93_stand_UN_LN_sim, plantnr == 3)
R93_stand_UN_LN_sim3$leaf <- 1:10

R93_stand_UN_HN_sim
R93_stand_UN_HN_sim3 <- subset(R93_stand_UN_HN_sim, plantnr == 3)
R93_stand_UN_HN_sim3$leaf <- 1:10

R93merged <- as.data.frame(cbind(R93_stand_UN_HN_sim3[,3], R93_stand_CR_HN_sim3[,3], 
                         R93_stand_UN_LN_sim3[,3], R93_stand_CR_LN_sim3[,3]))
names(R93merged) <- c("UN_HN", "CR_HN", "UN_LN", "CR_LN")
R93merged$leaf <- 1:10


R93merged <- melt(R93merged, id.vars = "leaf")
R93merged

R93PAR <- ggplot(R93merged) + 
  geom_line(aes(x = leaf, y = value, color = variable), size = 3) +
  scale_colour_manual(values=c("black", "red", "blue", "green"),
    name ="PAR", labels=c("UN_HN", "CR_HN", "UN_LN", "CR_LN")) +
  scale_x_discrete("leaf") +  xlab("Leaf Number") + ylab("PAR (umol m-2 s-1)") +
  theme(axis.title.x = element_text(face="bold", size=20),
           axis.text.x  = element_text(size=16),
           axis.title.y = element_text(face="bold", size=20),
           axis.text.y  = element_text(size=16))
R93PAR
R500PAR

plot(R93_UN_HN_plant)
plot(R93_stand_CR_HN)


R93_LA_merged <- as.data.frame(cbind(R93_stand_UN_HN_sim3[,10], R93_stand_CR_HN_sim3[,10], 
                         R93_stand_UN_LN_sim3[,10], R93_stand_CR_LN_sim3[,10]))
names(R93_LA_merged) <- c("UN_HN", "CR_HN", "UN_LN", "CR_LN")
R93_LA_merged$leaf <- 1:10


R93_LA_merged <- melt(R93_LA_merged, id.vars = "leaf")
R93_LA_merged[3] <- R93_LA_merged[3]/1e+06
R93_LA_merged[3]


R93_LA <- ggplot(R93_LA_merged) + 
  geom_line(aes(x = leaf, y = value, color = variable), size = 3) +
  scale_colour_manual(values=c("black", "red", "blue", "green"),
    name ="Treatment", labels=c("UN_HN", "CR_HN", "UN_LN", "CR_LN")) +
  scale_x_discrete("leaf") +  xlab("Leaf Number") + ylab("Sunlit Leaf Area (m^2)") +
  scale_y_continuous(limits=c(0, 0.0085)) +
  theme(axis.title.x = element_text(face="bold", size=20),
           axis.text.x  = element_text(size=16),
           axis.title.y = element_text(face="bold", size=20),
           axis.text.y  = element_text(size=16))
R93_LA


geno_comparison <- makeStand(list(R93_UN_HN_plant, R500_UN_HN_plant),
                       xyz=data.frame(x=c(-150,0),
                                      y=c(-150,0),
                                      z=c(0,0)))
plot(geno_comparison)


plot(R93_stand_CR_HN)

R93_comparison <- makeStand(list(R93_UN_HN_plant, R93_CR_HN_plant, R93_CR_LN_plant, R93_UN_LN_plant),
                       xyz=data.frame(x=c(-120, 120, 120, -120),
                                      y=c(120, 120, -120, -120),
                                      z=c(0, 0, 0, 0)))
plot(R93_comparison)

R500_comparison <- makeStand(list(R500_UN_HN_plant, R500_CR_HN_plant, R500_CR_LN_plant, R500_UN_LN_plant),
                       xyz=data.frame(x=c(-120, 120, 120, -120),
                                      y=c(120, 120, -120, -120),
                                      z=c(0, 0, 0, 0)))
plot(R500_comparison)



?scale_y_continuous

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


1000 mm / m
sqrt(2000)
1000*1000
2000/1e+06
1000000/1000


write.table(test, "test_plant.p", sep = "\t", row.names = FALSE)

##########
# GxE Stand
##########
R500_stand_total <- makeStand(list(R500_UN_HN_plant,
                                   R500_UN_LN_plant,
                                   R500_CR_HN_plant,R500_CR_HN_plant,R500_CR_HN_plant,R500_CR_HN_plant,R500_CR_HN_plant,
                                   R500_CR_LN_plant,R500_CR_LN_plant,R500_CR_LN_plant,R500_CR_LN_plant,R500_CR_LN_plant),
                       xyz=data.frame(x=c(0,350, -50,50,0,-50,50,          300,400,350,300,400),
                                      y=c(0,0,   -400,-300,-350,-300,-400, -300,-300,-350,-400,-400),
                                      z=c(0,0, 0,0,0,0,0, 0,0,0,0,0)))
plot(R500_stand_total)



# final stand figure for research proposal
# change the angle of the screen capture to make plants appear more 3D
R93_stand_total <- makeStand(list(R93_UN_HN_plant,
                                   R93_UN_LN_plant,
                                   R93_CR_HN_plant,R93_CR_HN_plant,R93_CR_HN_plant,R93_CR_HN_plant,R93_CR_HN_plant,
                                   R93_CR_LN_plant,R93_CR_LN_plant,R93_CR_LN_plant,R93_CR_LN_plant,R93_CR_LN_plant),
                       xyz=data.frame(x=c(0,350, -50,50,0,-50,50,          300,400,350,300,400),
                                      y=c(0,0,   -400,-300,-350,-300,-400, -300,-300,-350,-400,-400),
                                      z=c(0,0, 0,0,0,0,0, 0,0,0,0,0)))
plot(R93_stand_total)



R93_stand_total_Y <- runYplant(R93_stand_total, fbeam=1, altitude=90, azimuth=0, reflec=0.15, transmit=0.1, intern = FALSE, rewriteplantfile = TRUE)
R93_stand_total_Y

#subset only leaves we are intersted in
plantid <- c(1,2,5,10)
R93_stand_total_Y3 <- R93_stand_total_Y[R93_stand_total_Y$plantnr %in% plantid,]

R93_stand_total_Y3
R93_stand_total_Y3$leaf <- 1:10
R93_stand_total_Y3
str(R93_stand_total_Y3)
dim(R93_stand_total_Y3)
R93_stand_total_Y3$leaf <- as.factor(R93_stand_total_Y3$leaf)
R93_stand_total_Y3$plantnr <- as.factor(R93_stand_total_Y3$plantnr)

R93_melt <- R93_stand_total_Y3[10:12]
str(R93_melt)

?melt

R93_melt <- melt(R93_melt, id.vars = c("leaf", "plantnr"))
R93_melt[3] <- R93_melt[3]/1e+06
R93_melt$value <- R93_melt$value/1e+06

R93_melt$plantnr

R93_LA_plot <- ggplot(R93_melt, aes(x = leaf, y = value, group = plantnr, color = plantnr)) + 
  geom_line(size = 3) +
  scale_colour_manual(values = cols, name ="Treatment", labels=c("UN_High", "UN_Low", "CR_High", "CR_Low")) +
  ylab(expression(bold(paste(Modeled~Direct~Sunlit~Leaf~Area,~(m^2))))) +
  xlab("Leaf Number in Canopy") +
  theme_bw() +
  theme(axis.title.x = element_text(face="bold", size=26),
        axis.text.x  = element_text(face="bold", size=22),
        axis.title.y = element_text(face="bold", size=26),
        axis.text.y  = element_text(face="bold", size=22),
        legend.title = element_blank(),
        legend.position = c(0.15, 0.9), 
        legend.text = element_text(size=20, face="bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
R93_LA_plot

setwd("~/git.repos/brassica_field_2014_gh/output/")
ggsave("modeled_sunlit_leaf_area.pdf", width = 8, height = 8)
# see also Brassica_2014_second_timepoint.R
library(cowplot)
?plot_grid
figure_X <- plot_grid(R93_LA_plot, fruits_2_plot, ncol = 1)
figure_X
ggsave("joint_model_pod_figure.pdf", figure_X, height = 20, width = 15)




gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

n = 4
cols = gg_color_hue(4)
cols
dev.new(width=15, height=15)
plot(1:n, pch=16, cex=10, col=cols)
