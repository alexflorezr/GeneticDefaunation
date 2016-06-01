rm(list=ls())
### create a clean database

### Required libraries
library(plotrix)
library(stringr)

### Upload the database
data_dir <- "~/Desktop/"
setwd(data_dir)
DATABASE <- read.delim("DATABASE_19-05-16.txt", header=T, stringsAsFactors=F)
### remove the species that were not used
alces <- which(DATABASE$Species == c("Alces_alces"))
equus <- which(DATABASE$Species == c("Equus_caballus"))
capra <- which(DATABASE$Species == c("Capra_pyrenaica"))
canis <- which(DATABASE$Species == c("Canis_lupus"))
DATABASE <- DATABASE[-c(alces,equus,capra, canis),]
DATABASE <- DATABASE[-which(DATABASE$Mean_Age >= 50000),]

### Conservative (strict) database cleaning
### dirty data for fossils
DATABASE_fos <- DATABASE[DATABASE$Mean_Age > 50,]
Lat_fos_NA  <- which(is.na(DATABASE_fos$Latitude))
Lat_fos_empty <- which(nchar(DATABASE_fos$Latitude) < 1)
Labcode_fos_NA  <- which(is.na(DATABASE_fos$Lab_Code))
Labcode_fos_empty <- which(nchar(DATABASE_fos$Lab_Code) < 1)
rows_2_remove_fos <- unique(c(Lat_fos_NA, Lat_fos_empty, Labcode_fos_NA, Labcode_fos_empty))
DATABASE_fos_dirty <- DATABASE_fos[rows_2_remove_fos,]
write.table(DATABASE_fos_dirty, file = "DATABASE_fos_dirty.txt", sep = "\t")
table(DATABASE_fos_dirty$Species)
DATABASE_fos_clean <- DATABASE_fos[-rows_2_remove_fos,]
write.table(DATABASE_fos_clean, file = "DATABASE_fos_clean.txt", sep = "\t")
table(DATABASE_fos_clean$Species)

### dirty data for modern data
DATABASE_mod <- DATABASE[DATABASE$Mean_Age <= 50,]
Lat_mod_NA  <- which(is.na(DATABASE_mod$Latitude))
Lat_mod_empty <- which(nchar(DATABASE_mod$Latitude) < 1)
rows_2_remove_mod <- unique(c(Lat_mod_NA, Lat_mod_empty))
DATABASE_mod_dirty <- DATABASE_mod[rows_2_remove_mod,]
write.table(DATABASE_mod_dirty, file = "DATABASE_mod_dirty.txt", sep = "\t")
table(DATABASE_mod_dirty$Species)
DATABASE_mod_clean <- DATABASE_mod[-rows_2_remove_mod,]
write.table(DATABASE_mod_clean, file = "DATABASE_mod_clean.txt", sep = "\t")
table(DATABASE_mod_clean$Species)


### find the records with no geo data
        ### samples with NA 
        lat_long_NA <- DATABASE[which(is.na(DATABASE$Latitude)),]
        table(lat_long_NA$Species)

        ### samples with empty data ""
        lat_long_empty <- DATABASE[which(nchar(DATABASE$Latitude) < 1),]
        table(lat_long_empty$Species)
        which(nchar(lat_long_empty$Sequence) < 1)
        which(nchar(lat_long_empty$Mean_Age) > 1)

### find the fossils with no radiocarbon data
        ### remove samples with mean age 0
        lab_Code_zero_mean_age <- DATABASE[which(DATABASE$Mean_Age > 50),]
        ### samples with NA
        lab_Code_NA <- lab_Code_zero_mean_age[which(is.na(lab_Code_zero_mean_age$Lab_Code)),]
        table(lab_Code_NA$Species)
        ### samples with empty data ""
        lab_Code_empty <- lab_Code_zero_mean_age[which(nchar(lab_Code_zero_mean_age$Lab_Code) < 1),]
        table(lab_Code_empty$Species)
        nchar(lab_Code_empty$Sequence) > 1
        ### loop to find repeated records
        mirar <- DATABASE[NULL,]
        for (empty in seq_along(lab_Code_empty$Sequence)){
                to_match <- c(lab_Code_empty$Species[empty], lab_Code_empty$C14_Age[empty])
                possible <- which(DATABASE$Species == to_match[1] & DATABASE$C14_Age == to_match[2] & nchar(DATABASE$Lab_Code) >1)
                temp_rbind <- rbind(lab_Code_empty[empty,], DATABASE[possible,])
                mirar <- rbind(mirar, temp_rbind)
        }
##### test per species ###
for(sp in unique(DATABASE$Species)){
        temp_sp <- DATABASE[DATABASE$Species == sp,]
        temp_lat_NA <- which(is.na(temp_sp$Latitude))
        temp_lat_empty <- which(nchar(temp_sp$Latitude) < 1)
        temp_labcode_NA <- which(is.na(temp_sp$Lab_Code)  & temp_sp$Mean_Age > 50)
        temp_labcode_empty <- which(nchar(temp_sp$Lab_Code) < 1 & temp_sp$Mean_Age > 50)
        if(length(temp_lat_NA) > 1){
                print(temp_sp$Accession_GB[temp_lat_NA])
        }
        if(length(temp_lat_empty) > 1){
                print(temp_sp$Accession_GB[temp_lat_empty])
        }
        if(length(temp_labcode_NA) > 1){
                print(temp_sp$Accession_GB[temp_labcode_NA])
        }
        if(length(temp_labcode_empty) > 1){
                print(temp_sp$Accession_GB[temp_labcode_empty])
        }
}












# create a clean database
# remove no lat-long
# for each column check the type of data (class) and add or remove wrong values