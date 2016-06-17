rm(list=ls())
### Table.1 supplementary materials

### Required libraries
library(stringr)

### Upload the database
data_dir <- "~/Desktop/PhD/Thesis/Raw_data/Clean_database"
setwd(data_dir)
DATABASE <- read.delim("DATABASE_16-06-16.txt", header=T, stringsAsFactors=F)
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
        # total samples per species
        Table.1$`Total samples`[sp] <- dim(temp_db)[1]
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
write.table(Table.1, file="Table_1.txt", sep="\t", row.names = F)

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
                ### is missing data in Latitude and Longitude the same
                sum(which(is.na(DATABASE$Latitude)) != which(is.na(DATABASE$Latitude)))
                ### are this samples missing locality information?
                table(DATABASE$Locality[which(is.na(DATABASE$Latitude))])
                table(DATABASE$Country[which(is.na(DATABASE$Latitude))])
                        ### No, they have localities, but not easily to assign
                ### are all of them sequences?
                table(nchar(DATABASE$Sequence[which(is.na(DATABASE$Latitude))]) > 1)
                        ### The majority are sequences, but 35 are not
                ### which ones are not sequences?
                table(DATABASE$Species[which(nchar(DATABASE$Sequence[which(is.na(DATABASE$Latitude))]) < 1)])
                ### all of them are Balaena mysticetus
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
                
                
                DATABASE[which(DATABASE$Dating_Method == "TL"), c(12,13,1,4,15,16)]
                table(DATABASE$Lab_Code[which(DATABASE$Dating_Method == "")])
                DATABASE[which(DATABASE$Dating_Method == "Statigraphical"),]
                