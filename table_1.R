Database <- read.delim(file.choose(), header = T, sep="\t", stringsAsFactors = F)
Species <- unique(Database$Species)

sp <- Database[Database$Species == "Balaena_mysticetus",]
Sp_temp <- paste(strsplit(unique(sp$Species), split="_")[[1]][1], strsplit(unique(sp$Species), split="_")[[1]][2], sep=" ")
no_rcdated <- which(nchar(sp$Lab_Code) < 1)
if(no_rcdated > 0){
        print(paste("check", no_rcdated))
}
points_sp <- cbind(sp$Longitude, sp$Latitude)
if (sum(is.na(points_sp)) > 0){
        print(paste("check", which(is.na(points_sp))))
}
total_rcdated <- sum(nchar(sp$Lab_Code) > 1)


no_data <- which(!nchar(sp$Lab_Code) > 0)

points_sp <- cbind(sp$Longitude, sp$Latitude)


seqs <- sum(nchar(sp$Sequence) > 0)
library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-180, 180), ylim = c(0,90), asp=1)
points_sp <- cbind(sp$Longitude, sp$Latitude)
points(sp$Longitude, sp$Latitude, pch=16, col="green")
dim(points_sp)
which(is.na(points_sp))
unique(ca$Dating_Method)
which(is.na(sp$Lab_Code))
no_data <- which(!nchar(sp$Lab_Code) > 0)
sp[which(sp$C14_Age == 44200),]
points(sp$Longitude, sp$Latitude, pch=16, col="green")
