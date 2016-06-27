#### Graph for fossil record in the Database for supplementary materials
#### Read the database
data_dir <- "~/Desktop"
setwd(data_dir)
DATABASE <- read.delim("DATABASE_19-05-16.txt", header=T, stringsAsFactors=F)
alces <- which(DATABASE$Species == c("Alces_alces"))
equus <- which(DATABASE$Species == c("Equus_caballus"))
capra <- which(DATABASE$Species == c("Capra_pyrenaica"))
canis <- which(DATABASE$Species == c("Canis_lupus"))
DATABASE <- DATABASE[-c(alces,equus,capra, canis),]
### Loop through the database to find the only-fossil records
Only_fossil <- DATABASE[nchar(DATABASE$Sequence) == 0, ]
table(Only_fossil$Dating_Method)


Only_fossil_c14_error <- cbind(Only_fossil$C14_Age, Only_fossil$Sigma)
Only_fossil_c14_error <- cbind(seq(1,dim(Only_fossil_c14_error)[1], 1), Only_fossil_c14_error)
Only_fossil_c14_error[which(Only_fossil_c14_error[,3] < 1),3]
Only_fossil_c14_error <- Only_fossil_c14_error[-which(Only_fossil_c14_error[,3] < 1),]
plot(Only_fossil_c14_error[,2], Only_fossil_c14_error[,1], xlab=NA, ylab=NA, frame=F, pch=21, bg="#8B7355", col="#8B7355")
for(sigma in seq_along(Only_fossil_c14_error[,3])){
draw.circle(as.numeric(Only_fossil_c14_error[sigma,2]), as.numeric(Only_fossil_c14_error[sigma,1]), radius = as.numeric(Only_fossil_c14_error[sigma,3])) 
}
Only_fossil_c14_error[which(as.numeric(Only_fossil_c14_error[,3]) > 5000),]

