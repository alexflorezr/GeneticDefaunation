rm(list=ls())
#### Graph for fossil record in the Database for supplementary materials
#### Read the database
data_dir <- "/Users/afr/Desktop/PhD/Thesis/Raw_data/Clean_database/"
setwd(data_dir)
DATABASE <- read.delim("DATABASE_16-06-16.txt", header=T, stringsAsFactors=F)
alces <- which(DATABASE$Species == c("Alces_alces"))
equus <- which(DATABASE$Species == c("Equus_caballus"))
capra <- which(DATABASE$Species == c("Capra_pyrenaica"))
canis <- which(DATABASE$Species == c("Canis_lupus"))
DATABASE <- DATABASE[-c(alces,equus,capra, canis),]
### Loop through the database to find the only-fossil records
Only_fossil <- DATABASE[nchar(DATABASE$Sequence) == 0, ]
table(Only_fossil$Species)

Only_fossil$Dating_Method[which(Only_fossil$Dating_Method == "[AMS] C14")] <- "C14_AMS"
Only_fossil$Dating_Method[which(Only_fossil$Dating_Method == "AMS C14")] <- "C14_AMS"
Only_fossil$Dating_Method[which(Only_fossil$Dating_Method == "AMS_C14")] <- "C14_AMS"
Only_fossil$Dating_Method[which(Only_fossil$Dating_Method == "C14-Indirect")] <- "C14_indirect"
Only_fossil$Dating_Method[which(Only_fossil$Dating_Method == "indirect")] <- "C14_indirect"
Only_fossil$Dating_Method[which(Only_fossil$Dating_Method == "Modern")] <- "Not_a_fossil"
Only_fossil$Dating_Method[which(Only_fossil$Dating_Method == "C.a.")] <- "C14"
Only_fossil$Dating_Method[which(Only_fossil$Dating_Method == "OSL")] <- "C14_indirect"
### Remove indirect dating method
inderect <- c(which(Only_fossil$Dating_Method == "ESR"), which(Only_fossil$Dating_Method == "TL"), which(Only_fossil$Dating_Method == "OSL"))
Only_fossil <- Only_fossil[-inderect,]
######
Only_fossil <- read.delim(file = "Only_fossil.txt", sep = "\t", stringsAsFactors = F)
plot(as.numeric(Only_fossil$C14_Age), rep(0, length(Only_fossil$C14_Age)), ylim=c(-5000, 5000), xlim = c(0,60000),
     frame=F, xlab=NA, ylab=NA, pch=21, bg="#000000", axes = F, xaxs="i", yaxs="i")
for(sigma in seq_along(Only_fossil$Sigma)){
        x0 <- Only_fossil$C14_Age[sigma]
        x1 <- x0
        sigma <- as.numeric(Only_fossil$Sigma[sigma])
        y0 <- 0-sigma
        y1 <- 0+sigma
        segments(x0,y0,x1,y1, col="#BEBEBE")
}
axis(1, at=(seq(0, 60000, 10000)), labels =seq(0, 60000, 10000))
axis(2, at=(seq(-5000, 5000, 1000)), labels =seq(-5000, 5000, 1000))
segments(x0 = 0, y0 = 500, x1 = 22000,y1 = 500, lty = 2, lwd=2)
segments(x0 = 0, y0 = -500, x1 = 22000,y1 = -500, lty = 2, lwd=2)
segments(x0 = 22000, y0 = 1000, x1 = 50000,y1 = 1000, lty = 2, lwd=2)
segments(x0 = 22000, y0 = -1000, x1 = 50000,y1 = -1000, lty = 2, lwd=2)




Only_fossil_c14_error <- cbind(Only_fossil$C14_Age, Only_fossil$Sigma)
Only_fossil_c14_error[which(Only_fossil_c14_error[,2] < 1)]
Only_fossil_c14_error <- Only_fossil_c14_error[-which(Only_fossil_c14_error[,2] < 1),]
Only_fossil_c14_error <- Only_fossil_c14_error[order(Only_fossil_c14_error[,1]),]
Only_fossil_c14_error <- cbind(seq(1,dim(Only_fossil_c14_error)[1],  by=1), Only_fossil_c14_error)
plot(Only_fossil_c14_error[,2], Only_fossil_c14_error[,1], xlab=NA, ylab=NA, frame=F, pch=21, bg="#8B7355", col="#8B7355")
for(sigma in seq_along(Only_fossil_c14_error[,3])){
draw.circle(as.numeric(Only_fossil_c14_error[sigma,2]), as.numeric(Only_fossil_c14_error[sigma,1]), radius = as.numeric(Only_fossil_c14_error[sigma,3])) 
}
Only_fossil_c14_error[which(as.numeric(Only_fossil_c14_error[,3]) > 5000),]

