### clean the DATABASE
Full_DB <- DATABASE
Full_DB$Longitude <- as.numeric(Full_DB$Longitude)
Full_DB$Latitude <- as.numeric(Full_DB$Latitude)
full_vector <- as.vector(NULL)
for (i in seq_along(Full_DB$Latitude)){
        if (is.na(Full_DB$Longitude[i]) || is.na(Full_DB$Latitude[i])){
                full_vector <- c(full_vector,i)
        }
}
# remove the records without longitude and or latitude
Full_DB_LL <- Full_DB[-full_vector,]
# remove the records older than 50000 years
Full_DB_LL <- Full_DB_LL[-which(Full_DB_LL$Median_Age > 50000),]
# add two empty colums to assign color and type of point in the map
For_map <- matrix(NA,nrow=length(Full_DB_LL$Latitude), ncol=2)
colnames(For_map) <- c("Map_color", "Map_type")
Full_DB_map <- cbind(Full_DB_LL, For_map)



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
