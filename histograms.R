rm(list=ls())
setwd("/Users/afr/Desktop/Ch1_Figures/Figure_one")
Full_DB <- read.delim("DATABASE.txt", header=T, stringsAsFactors=F)
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
DATABASE <- Full_DB_LL[-which(Full_DB_LL$Median_Age >= 50000),]
############################################################################################################################
############################################################################################################################
require(ape)
require(pegas)
require(adegenet)
require(strataG)
require(sidier)
############################################################################################################################
############################################################################################################################
setwd("~/Desktop/MSc_Ditte/")
Sp_fast_slow <- read.delim("Velocity_4_species", stringsAsFactors=F, header=T)
#Sp_fast_slow <- Sp_fast_slow[3,]
#str(Sp_fast_slow)
#Sampling_aDNA <- function(Sp_fast_slow, database, ind_color){
Sp_bef_aft <- as.data.frame(matrix(nrow=dim(Sp_fast_slow)[1], ncol=7))
colnames(Sp_bef_aft) <- c("Species", "Bef", "Aft","Nuc_div_Total", "Phist", "Fst", "Nei_DA")
Event_name <- "Fast and slow events"
# plot a histogram for the fossil record and the sequences
hist_rec_seq <- TRUE
# add events to the histogram
plot_events <- TRUE
# add the values of the SS to the histogram
hist_add_SS <- TRUE
# plot all the histograms in a single plot, like a grid
hist_2gether <- FALSE
# do a subsamling of the sequences when the sampling is different before and after
subsampling <- FALSE
# plot the subsampling
plot_sub <- FALSE
# Estimate the population parameters
pop_stats <- TRUE
# Estimate the parameters using only the adjacent sequences
adjacent_nuc <- TRUE
# plot a map of the sequences 
plot_map <- TRUE
#this directory is for the alignments
setwd("~/Desktop/Ch1_Figures/Figure_one/Same_length_seqs/")
###########################################################################################
### Plotting all histograms together #######################################################################################################
if (hist_2gether == TRUE){
        nfrow <- as.vector(n2mfrow(length(Sp_fast_slow[,1])))
        par(mfrow=nfrow , mar=c(2,3,3,1), yaxs="i",xaxs="i",lwd=0.5)
}else{
        par(mar=c(4,3,3,1), yaxs="i",xaxs="i",lwd=0.5) 
}
### ENDS HERE #########################################################################################################################
################################################################################################################################
buffer <- 7000
Nuc_bef_aft_raw <- as.data.frame(matrix(nrow=length(Sp_fast_slow$Species)*2, ncol= 5))
colnames(Nuc_bef_aft_raw) <- c("Species", "Nuc_bef", "Nuc_aft", "Nuc_diff", "Event_type")
for (sp in seq_along(Sp_fast_slow$Species)){
        ### STARTS HERE ##############################  
        if(is.element(Sp_fast_slow$Species[sp], unique(DATABASE$Species))){
                ## STARTS: histogram for the fossil record and sequences ##
                temp_sp_rec_db <- Full_DB[which(Full_DB$Species == Sp_fast_slow$Species[sp] & Full_DB$Mean_Age <= 50000),]
                temp_hist_rec <- hist(temp_sp_rec_db$Mean_Age, breaks=seq(0, 50000, 2000), plot=F)
                temp_hist_rec$counts[which(temp_hist_rec$counts == 0)] <- NA
                temp_max <- max(na.omit(temp_hist_rec$counts))
                temp_genus <- strsplit(Sp_fast_slow$Species[sp], split="_")[[1]][1]
                temp_specific <- strsplit(Sp_fast_slow$Species[sp], split="_")[[1]][2]
                main_name <- paste(temp_genus, temp_specific, sep=" ")
                sp_short_name <- paste(strsplit(temp_genus, split="")[[1]][1], strsplit(temp_specific, split="")[[1]][1], sep="")
                temp_sp_seq_db <- temp_sp_rec_db[nchar(temp_sp_rec_db$Sequence) > 1,]
                temp_hist_seq <- hist(temp_sp_seq_db$Mean_Age,breaks=seq(0, 71000, 2000), plot=F )
                temp_hist_seq$counts[which(temp_hist_seq$counts == 0)] <- NA
                sp_color <- "#00BFFF"
                #Events <- as.numeric(Sp_fast_slow[sp,c(2,3)])
                Events <- c(Sp_fast_slow$Fast[sp],Sp_fast_slow$Slow[sp])
                temp_sp <-  paste(strsplit(temp_genus, split="")[[1]][1], strsplit(temp_specific, split="")[[1]][1], sep="")
                ### STARTS HERE #########################################################################################################################
                ### Plotting histograms #######################################################################################################
                if(hist_rec_seq == TRUE){
                        hist(temp_sp_rec_db$Mean_Age, breaks=seq(0, 50000, 2000), main=NULL, xaxt="n", yaxt="n", ylab=NULL, xlab=NULL, col="#00BFFF50", border=NA)
                        axis(side=1)
                        axis(side=2)
                        mtext(side=3, main_name, line=1)
                        hist(temp_sp_seq_db$Mean_Age,breaks=seq(0, 50000, 2000), add=T, col=paste(sp_color, 90, sep=""), border=NA)
                        text(x=temp_hist_seq$mids, y=0.5, labels=as.character(temp_hist_seq$counts), cex=0.7, adj=c(0,0))
                        #labels=as.character(temp_hist_rec$counts)
                        #if (ind_color == T) {sp_color <= paste(Sp_fast_slow$Color_color[sp], 90, sep="")}
                        if (plot_events == TRUE){
                                x_fast <- c(Sp_fast_slow$Fast_end[sp],Sp_fast_slow$Fast_end[sp], Sp_fast_slow$Fast_start[sp],Sp_fast_slow$Fast_start[sp])
                                x_fast_buffer <- x_fast+c(-buffer, -buffer, buffer, buffer)
                                y_fast <- c(0,temp_max, temp_max, 0 )
                                x_slow <- c(Sp_fast_slow$Slow_end[sp],Sp_fast_slow$Slow_end[sp], Sp_fast_slow$Slow_start[sp],Sp_fast_slow$Slow_start[sp])
                                x_slow_buffer <- x_slow+c(-buffer, -buffer, buffer, buffer)
                                y_slow <- c(0,temp_max, temp_max, 0 )
                                polygon(x=x_fast_buffer, y = y_fast, col="#FF7F0099", border="#FF7F0099")
                                polygon(x=x_fast, y = y_fast, col="#FF7F00", border="#FF7F00")
                                polygon(x=x_slow_buffer, y = y_slow, col= "#EEC59199", border= "#EEC59199")
                                polygon(x=x_slow, y = y_slow, col= "#EEC591", border= "#EEC591")
                        }
                }
                ### ENDS HERE #########################################################################################################################
                ###############