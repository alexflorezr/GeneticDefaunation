# create a clean database
# remove no lat-long
# for each column check the type of data (class) and add or remove wrong values
rm(list=ls())
### Upload the raw database
data_dir <- "~/Desktop/PhD/Thesis/Raw_data/Clean_database"
setwd(data_dir)
DATABASE_raw <- read.delim("DATABASE_16-06-16.txt", header=T, stringsAsFactors=F)
alces <- which(DATABASE_raw$Species == c("Alces_alces"))
equus <- which(DATABASE_raw$Species == c("Equus_caballus"))
capra <- which(DATABASE_raw$Species == c("Capra_pyrenaica"))
canis <- which(DATABASE_raw$Species == c("Canis_lupus"))
DATABASE_all <- DATABASE_raw[-c(alces,equus,capra, canis),]
# remove the records older than 50000 years
DATABASE_all <- DATABASE_all[-which(DATABASE_all$Mean_Age >= 50000),]
DATABASE <- DATABASE_all[DATABASE_all$Species == "Ursus_arctos",]
### Individual variables for the DATABASE
        ### Species
                unique(DATABASE$Species)
                ### the gray whale has to be included in the DATABASE
        ### Latitude
                ### How many records with NA
                table(is.na(DATABASE$Latitude))
                ### How many records with NA
                table(DATABASE$Species[which(is.na(DATABASE$Latitude))])
                ### How many have "" as value
                length(which(DATABASE$Latitude == ""))
                DATABASE$Latitude[which(DATABASE$Latitude == "")] <- NA
        ### Longitude
                ### How many records with NA
                table(is.na(DATABASE$Longitude))
                ### How many records with NA
                table(DATABASE$Species[which(is.na(DATABASE$Longitude))])
                ### How many have "" as value
                sum(DATABASE$Longitude == "")
        ### Compare latitude and longitude
                ### is missing data in Latitude and Longitude the same
                sum(which(is.na(DATABASE$Latitude)) != which(is.na(DATABASE$Latitude)))
                ### are this samples missing locality information?
                table(DATABASE$Locality[which(is.na(DATABASE$Latitude))])
                table(DATABASE$Country[which(is.na(DATABASE$Latitude))])
                ### No, they have localities, but not easily to assign
                ### are all of them sequences?
                table(nchar(DATABASE$Sequence[which(is.na(DATABASE$Latitude))]) > 1)
                ### how many sequences with NAs are modern samples
                DATABASE$C14_Age[which(is.na(DATABASE$Latitude))]
                DATABASE$Lab_Code[which(is.na(DATABASE$Latitude))]
                DATABASE$Dating_Method[which(is.na(DATABASE$Latitude))]
                ### The majority are sequences, but 35 are not
                ### which ones are not sequences?
                table(DATABASE$Species[which(nchar(DATABASE$Sequence[which(is.na(DATABASE$Latitude))]) < 1)])
                ### all of them are Balaena mysticetus
        ### LabCodes
                ### What type of labcodes are included in the DATABASE?
                ### Labcodes == NA (198 with NAs and 3 are "")
                table(DATABASE$Species[which(is.na(DATABASE$Lab_Code))])
                length(which(is.na(DATABASE$Lab_Code)))
                length(which(DATABASE$Lab_Code == ""))
                table(nchar(DATABASE$Sequence[which(is.na(DATABASE$Lab_Code))]) > 1)
                table(DATABASE$Dating_Method[which(is.na(DATABASE$Lab_Code[]))])
                ### all of them are sequences
                table(DATABASE$Species[which(DATABASE$Lab_Code == "")])
                table(nchar(DATABASE$Species[which(DATABASE$Lab_Code == "")]) > 1)
        ### Dating method
                ### Fixed values for the Dating method (only: C14_AMS, C14, C14_indirect, ESR, Not_a_fossil )
                table(DATABASE$Dating_Method)
                DATABASE$Dating_Method[which(DATABASE$Dating_Method == "[AMS] C14")] <- "C14_AMS"
                DATABASE$Dating_Method[which(DATABASE$Dating_Method == "AMS C14")] <- "C14_AMS"
                DATABASE$Dating_Method[which(DATABASE$Dating_Method == "AMS_C14")] <- "C14_AMS"
                DATABASE$Dating_Method[which(DATABASE$Dating_Method == "C14-Indirect")] <- "C14_indirect"
                DATABASE$Dating_Method[which(DATABASE$Dating_Method == "indirect")] <- "C14_indirect"
                DATABASE$Dating_Method[which(DATABASE$Dating_Method == "Modern")] <- "Not_a_fossil"
                DATABASE$Dating_Method[which(DATABASE$Dating_Method == "C14" & as.numeric(DATABASE$C14_Age) == 0),] <- "Not_a_fossil"
                DATABASE$Dating_Method[which(DATABASE$Dating_Method == "Mod_sample" & as.numeric(DATABASE$C14_Age) == 0)] <- "Not_a_fossil"
                DATABASE$Dating_Method[which(DATABASE$Dating_Method == "historical" & as.numeric(DATABASE$C14_Age) == 0)] <- "Not_a_fossil"
                DATABASE$Dating_Method[which(DATABASE$Dating_Method == "C.a.")] <- "C14"
                DATABASE$Dating_Method[which(DATABASE$Dating_Method == "OSL")] <- "C14_indirect"
        ### Remove indirect dating method
                inderect <- c(which(DATABASE$Dating_Method == "ESR"), which(DATABASE$Dating_Method == "TL"), which(DATABASE$Dating_Method == "OSL"))
                DATABASE <- DATABASE[-inderect,]
        ### This part only applies for sequences
                ### all of the sequences have an accesion number
                DATABASE$Accession_GB[which(nchar(DATABASE$Sequence) > 1)]
                nchar(DATABASE$Sequence[which(nchar(DATABASE$Sequence) > 1)])
        ### Loci
                table(DATABASE$Loci[which(nchar(DATABASE$Sequence) > 1)])
                DATABASE$Loci[ncahrDATABASE$Loci == "['CR']"] <- "Control_region"
                #DATABASE$Loci[nchar(DATABASE$Loci) >1] <- "Control_region"
                DATABASE$Loci[DATABASE$Loci == "['cyt-b']"] <- "Cytochrome_b"
                ### Export the species database
                setwd("~/Desktop/PhD/Thesis/Raw_data/Clean_database/DB_per_species")
                write.table(DATABASE, file = "DB_ua_clean.txt", sep = "\t", row.names = F)
