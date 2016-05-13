### A single map for species including fossil record and DNA sequences sampling
### Upload the database
data_dir <- "~/Desktop/PhD/Thesis/Raw_data/Clean_database"
setwd(data_dir)
DATABASE <- read.delim("DATABASE.txt", header=T, stringsAsFactors=F)

#####################
#### histograms #####
#####################
par(mfrow=c(7,3), mar=c(2,2,2,2))
plot.new()
species <- unique(DATABASE$Species)
for (sp in seq_along(species)){
        DATABASE_species <- DATABASE[DATABASE$Species == species[sp],]
        temp_hist <- hist(DATABASE_species$Median_Age[DATABASE_species$Median_Age > 1000], ylab="", xlab="", col="#810f7c",cex=0.75, cex.axis=1, cex.lab=1, labels=T, xaxs="i")
}

hist(DATABASE$Median_Age)
###########################
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




##################################
### maps: all species together ###
##################################

##################################
### SUPER IMPORTANT IMPORTANT  ###
##################################
### USE FROM HERE ####


library(rworldmap) 
points_colors <- c("#8B4513","#8B5A00","#D2691E","#EE7600","#FFA500","#CDCD00","#9ACD32","#43CD80","#008B45", "#006400")
points_time  <- seq(50000, 5000, by=-5000)
points_table <- cbind(points_colors, points_time)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-180, 180), ylim = c(0,90), asp=1)
map_colums <- c(which(colnames(Full_DB_map) == "Map_color"), which(colnames(Full_DB_map) == "Map_type"))
for (k in seq_along(Full_DB_map[,1])){
        for (j in seq_along(points_table[,2])){
                if (Full_DB_map$Median_Age[k] <= as.numeric(points_table[j,2]) && Full_DB_map$Median_Age[k] >= as.numeric(points_table[j,2])-5000){
                        if (nchar(Full_DB_map$Sequence[k]) > 2){
                                Full_DB_map[k,map_colums]  <- c(points_table[j,1], 24)
                        }
                        if (nchar(Full_DB_map$Sequence[k]) < 2){
                                Full_DB_map[k,map_colums]  <- c(points_table[j,1], 21)
                        }
                }
        }
}
points(as.numeric(Full_DB_map$Longitude), as.numeric(Full_DB_map$Latitude), col=Full_DB_map$Map_color, cex=1, pch=as.numeric(Full_DB_map$Map_type), bg=paste(Full_DB_map$Map_color, 90, sep=""))
#### TO HERE AND CHANGE THE COLORS TO HOLOCENE VS PLEISTOCENE #####


points(as.numeric(Full_DB_map$Longitude), as.numeric(Full_DB_map$Latitude))
species_vector <- vector()
bin <-5000
points_colors <- c("#8B5A00","#8B4513","#D2691E","#EE7600","#FFA500","#CDCD00","#9ACD32","#43CD80","#008B45", "#006400")
species_vector<- c("bs", "ca","mp","om")
species_name <- c("Bison","Woolly rhinoceros","Mammoth","Musk ox")
}
par(mfrow=c(3,5), mar=c(2,2,1,1))
for (i in seq_along(species_vector)){
        temp_file <- read.delim(paste(species_vector[i], "_db_seq_clean.txt", sep=""), header=T, stringsAsFactors=F)
        main <- species_vector[i]
        temp_hist <- hist(temp_file$median_OxCal[temp_file$median_OxCal > 1000], breaks=seq(0,55000,bin), main=main, ylab="", xlab="", ylim=c(0,65), col="#810f7c",cex=0.75, cex.axis=1, cex.lab=1, labels=T, plot=F, xaxs="i")
        temp_hist$counts[temp_hist$counts== 0] <- ""
        hist(temp_file$median_OxCal[temp_file$median_OxCal > 1000], breaks=seq(0,55000,bin), main=NULL, ylab="", xlab="", ylim=c(0,65), col=points_colors ,cex=0.75, cex.axis=1, cex.lab=1, labels=temp_hist$counts, yaxt="n", pos=-0.1, border="white") 
        text(x=30000, y=60, species_name[i], cex=1.3)
}
#####################
#### Maps ###########
#####################
library(rworldmap)
points_colors <- c("#8B4513","#8B5A00","#D2691E","#EE7600","#FFA500","#CDCD00","#9ACD32","#43CD80","#008B45", "#006400")
points_time  <- seq(50000, 5000, by=-5000)
points_table <- cbind(points_colors, points_time)
for (i in seq_along(species_vector)){
        temp_points <- read.delim(paste(species_vector[i], "_db_seq_clean.txt", sep=""), header=T, stringsAsFactors=F)
        newmap <- getMap(resolution = "low")
        plot(newmap, xlim = c(-180, 180), ylim = c(0,90), asp=1, main="Mammuthus primigenius")
        for (i in seq_along(temp_points$median_OxCal)){
                if(is.na(temp_points$Sequence[i])){
                        temp_points$Map_type[i] <- 21
                        temp_points$Map_bg[i] <- temp_points$Map_color[i]
                }
                else{
                        temp_points$Map_type[i] <- 24
                        temp_points$Map_bg[i] <- "black"
                }
                for (j in seq_along(points_table[,2])){
                        if (temp_points$median_OxCal[i] <= as.numeric(points_table[j,2]) && temp_points$median_OxCal[i] >= as.numeric(points_table[j,2])-4999){
                                temp_points$Map_color[i] <- points_table[j,1]
                        }
                }
        }
}

points(temp_points$Longitude, temp_points$Latitude, col=temp_points$Map_bg, cex=1.2, pch=temp_points$Map_type, bg=paste(temp_points$Map_color, 95, sep=""))

