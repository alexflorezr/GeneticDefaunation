rm(list=ls())
### Table.1 supplementary materials

### Required libraries
library(stringr)

### Upload the database
data_dir <- "~/Desktop/PhD/Thesis/Raw_data/Clean_database"
setwd(data_dir)
DATABASE <- read.delim("DATABASE_19-05-16.txt", header=T, stringsAsFactors=F)
alces <- which(DATABASE$Species == c("Alces_alces"))
equus <- which(DATABASE$Species == c("Equus_caballus"))
capra <- which(DATABASE$Species == c("Capra_pyrenaica"))
canis <- which(DATABASE$Species == c("Canis_lupus"))
DATABASE <- DATABASE[-c(alces,equus,capra, canis),]


### Extenal variables
pleis_holoc_boundary <- 11700

# remove the records older than 50000 years
DATABASE <- DATABASE[-which(DATABASE$Mean_Age >= 50000),]





Species <- unique(DATABASE$Species)
Table.1 <- as.data.frame(matrix(nrow=length(Species), ncol=8))
colnames(Table.1) <- c("Species", "Total samples", "Radiocarbon-dated fossils", 
                       "DNA Late Pleistocene", "DNA Holocene", "Locus", "Alignment length", 
                       "Extiction status")
for(sp in seq_along(Species)){
        temp_db <- DATABASE[DATABASE$Species == Species[sp],]
        Table.1$Species[sp] <- species <- str_replace(unique(temp_db$Species),"_", " ")
        # how many are radiocarbon dated
        Table.1$`Radiocarbon-dated fossils`[sp] <- temp_lab_code <- sum(nchar(temp_db$Lab_Code) > 1)
        temp_NO_lab_code <- sum(nchar(temp_db$Lab_Code) < 1)
        temp_NA_lab_code <- sum(is.na(temp_db$Lab_Code))
        # how many are aDNA
        Table.1$`DNA Late Pleistocene`[sp] <- temp_DNA_pleis <- sum(nchar(temp_db$Sequence) > 1 & temp_db$Mean_Age > pleis_holoc_boundary)
        Table.1$`DNA Holocene`[sp] <- temp_DNA_holoc <- sum(nchar(temp_db$Sequence) > 1 & temp_db$Mean_Age <= pleis_holoc_boundary)
        # what locus was use
        Table.1$Locus[sp] <- temp_locus <- unique(temp_db$Loci)[1]
        temp_length <- paste(as.vector(range(temp_db$Length, na.rm = T)), sep = "-")
        Table.1$`Alignment length`[sp] <- paste(temp_length[1], temp_length[2], sep = "-")
}




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
