rm(list=ls())
### maps included in the supplemetary materials

### Required libraries
library(maps)
library(mapproj)
library(plotrix)
library(stringr)

### Upload the database
data_dir <- "~/Desktop/PhD/Thesis/Raw_data/Clean_database"
setwd(data_dir)
DATABASE <- read.delim("DATABASE_19-05-16.txt", header=T, stringsAsFactors=F)
alces <- which(DATABASE$Species == c("Alces_alces"))
equus <- which(DATABASE$Species == c("Equus_caballus"))
capra <- which(DATABASE$Species == c("Capra_pyrenaica"))
DATABASE <- DATABASE[-c(alces,equus,capra),]

### Extenal variables
col_pleis <- "#5C5C5C"
col_holoc <- "#B5B5B5"
col_DNA <- "#FF7F24"
col_no_DNA <- "#8EE5EE"
pleis_holoc_boundary <- 11700

### A single map for all species including fossil record and DNA sequences sampling
single_map <- as.data.frame(matrix(numeric(), nrow = dim(DATABASE)[1], ncol = 5))
colnames(single_map) <- c("Species", "Longitude", "Latitude", "Time", "Type")
for (record in seq_along(DATABASE[,1])){
        single_map[record, c(1,2,3)] <- c(DATABASE$Species[record], DATABASE$Longitude[record], DATABASE$Latitude[record])
        if (DATABASE$Mean_Age[record] >= 11700){
                single_map[record,4] <- "Pleistocene"
        }else{
                single_map[record,4] <- "Holocene"
        }
        if (nchar(DATABASE$Sequence[record]) > 1){
                single_map[record,5] <- "DNA"
        }else{
                single_map[record,5] <- "Fossil"
        }
}

### For plotting the sampling map for all the species together
map("world",proj="azequidist", resolution = 0.2, ylim = c(33,90), lwd=1.5, mar=c(0,2,2,2), interior = F) 
points(mapproject(as.numeric(single_map$Longitude), as.numeric(single_map$Latitude)), 
       bg=ifelse(single_map$Time == "Pleistocene",col_pleis, col_holoc), 
       cex=1.4, pch=ifelse(single_map$Type == "DNA",24, 21), 
       col=ifelse(single_map$Type == "DNA",col_DNA, col_no_DNA))
draw.circle(0,0,1.17, lwd=2)
mtext("18 species of Holarctic mammals", side=3, line=2)
mtext(paste("n = ", sum(!is.na(single_map$Longitude))), line=1)
legend( -1.32,-0.55, pch=c(24, 21), c("DNA", "Not DNA"), bty = "n", col =c(col_DNA, col_no_DNA), pt.cex = 2, pt.lwd = 3, title="By type", title.adj = c(0,0), y.intersp = 0.6, x.intersp = 0.5)
legend( -1.32,-0.82, pch=15, c("Pleistocene", "Holocene"), bty = "n", col=c(col_pleis, col_holoc), pt.cex = 3, pt.lwd = 3, title = "By time", title.adj = c(0,0), y.intersp = 0.6, x.intersp = 0.5)

### Function to create the maps for single species
sp_maps_defaunation <- function(map_data){
        species <- str_replace(unique(map_data$Species),"_", " ")
        map("world",proj="azequidist", resolution = 0.2, ylim = c(33,90), lwd=1.5, mar=c(0,2,2,2), interior = F) 
        points(mapproject(as.numeric(map_data$Longitude), as.numeric(map_data$Latitude)), 
                bg=ifelse(map_data$Time == "Pleistocene",col_pleis, col_holoc), 
                cex=1.4, pch=ifelse(map_data$Type == "DNA",24, 21),
                col=ifelse(map_data$Type == "DNA",col_DNA, col_no_DNA))
        draw.circle(0,0,1.17, lwd=2)
        mtext(species, side=3, line=2)
        mtext(paste("n = ", sum(!is.na(map_data$Longitude))), line=1)
        legend( -1.32,-0.55, pch=c(24, 21), c("DNA", "Not DNA"), bty = "n", col =c(col_DNA, col_no_DNA), pt.cex = 2, pt.lwd = 3, title="By type", title.adj = c(0,0), y.intersp = 0.6, x.intersp = 0.5)
        legend( -1.32,-0.82, pch=15, c("Pleistocene", "Holocene"), bty = "n", col=c(col_pleis, col_holoc), pt.cex = 3, pt.lwd = 3, title = "By time", title.adj = c(0,0), y.intersp = 0.6, x.intersp = 0.5)

}

### Loop to run the sp_maps_defaunation function over the database
for(sp in unique(single_map$Species)){
        temp_map <- single_map[which(single_map$Species == sp),]
        sp_maps_defaunation(temp_map)
}