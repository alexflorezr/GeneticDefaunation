### histograms included in the supplemetary materials
rm(list=ls())

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
col_pleis <- "#5C5C5C"
col_holoc <- "#B5B5B5"
col_DNA <- "#FF7F24"
col_no_DNA <- "#8EE5EE"
hist_break <- c(seq(0, 10000, 2000), 11700, 14000, seq(16000, 50000, 2000))
hist_col <- ifelse(hist_break < 11700, col_holoc, col_pleis)
pleis_holoc_boundary <- 11700


# remove the records older than 50000 years
DATABASE <- DATABASE[-which(DATABASE$Mean_Age >= 50000),]
temp_map <- DATABASE[which(DATABASE$Species == "Castor_fiber"),]
plot.new()
### Function to create the histograms for each species
sp_hist_defaunation <- function(hist_data){
        temp_seq <- hist_data[nchar(hist_data$Sequence) > 1,]
        temp_no_seq <- hist_data[nchar(hist_data$Sequence) < 1,]
        if(nrow(temp_no_seq) < 1){
                no_plot_no_seq <- hist(0,  plot=F)
                no_plot_no_seq$counts <- 0
        }else{
                no_plot_no_seq <- hist(temp_no_seq$Mean_Age, breaks=hist_break,   plot=F) 
        }
        no_plot_seq <- hist(temp_seq$Mean_Age, breaks=hist_break,  plot=F)
        all_sorted <- sort(c(no_plot_no_seq$counts, no_plot_seq$counts), decreasing = T)
        at_hist <- seq(0,30,length.out = length(hist_break))[seq(1,26,5)]
        lab_hist <- hist_break[seq(1,26,5)]
        par(lwd=2)
        if(all_sorted[2] < all_sorted[1]/1.5){
                new_limit <-  all_sorted[1]-(all_sorted[1]-all_sorted[2]-all_sorted[2]/4)
                if(max(all_sorted) == max(no_plot_seq$counts)){
                        no_plot_seq_gap <- no_plot_seq
                        no_plot_seq_gap$counts[no_plot_seq$counts > all_sorted[2]] <- new_limit
                        no_plot_no_seq_gap <- no_plot_no_seq
                 }
                if(max(all_sorted) == max(no_plot_no_seq$counts)){
                        no_plot_seq_gap <- no_plot_seq
                        no_plot_no_seq_gap <- no_plot_no_seq
                        no_plot_no_seq_gap$counts[no_plot_no_seq$counts > all_sorted[2]] <- new_limit
                }
                barplot(no_plot_no_seq_gap$counts,ylab="", xlab="", col=hist_col,
                        border=ifelse(no_plot_no_seq$counts == 0, NA,col_no_DNA), axes=FALSE, ylim=c(0,new_limit+new_limit/10))
                barplot(no_plot_seq_gap$counts,
                        ylab="", xlab="", col=hist_col, 
                        border=ifelse(no_plot_seq_gap$counts == 0, NA,col_DNA), axes=FALSE, add=T)
                axis(2,at=c(0,all_sorted[2],new_limit),
                        labels=c(0,all_sorted[2],all_sorted[1]), las=2)
                axis.break(2,all_sorted[2]+1)
                legend(at_hist[5]+1.5, new_limit+new_limit/10, pch=22, c("DNA", "Not DNA"), bty = "n", col =c(col_DNA, col_no_DNA), pt.cex = 2, pt.lwd = 3, title="By type", title.adj = c(0,0), y.intersp = 0.6, x.intersp = 0.5)
                legend(at_hist[5]+1.5, new_limit-(new_limit/10)*0.5,pch=15, c("Pleistocene", "Holocene"), bty = "n", col=c(col_pleis, col_holoc), pt.cex = 3, pt.lwd = 3, title = "By time", title.adj = c(0,0), y.intersp = 0.6, x.intersp = 0.5)
        }else{
                barplot(no_plot_no_seq$counts,
                        ylab="", xlab="", col=hist_col, 
                        border=ifelse(no_plot_no_seq$counts == 0, NA,col_no_DNA), ylim=c(0,max(all_sorted)+max(all_sorted)/10))
                barplot(no_plot_seq$counts,
                        ylab="", xlab="", col=hist_col,
                        border=ifelse(no_plot_seq$counts == 0, NA,col_DNA), add=T)
                abline(v=11700)
                legend(at_hist[5]+1.5, max(all_sorted)+(max(all_sorted)/10), pch=22, legend=c("DNA", "Not DNA"), bty = "n", col =c(col_DNA, col_no_DNA), pt.cex = 2, pt.lwd = 3, title="By type", title.adj = c(0,0), y.intersp = 0.6, x.intersp = 0.5)
                legend(at_hist[5]+1.5, max(all_sorted)-(max(all_sorted)/10)*0.5,pch=15,legend = c("Pleistocene", "Holocene"), bty = "n", col=c(col_pleis, col_holoc), pt.cex = 3, pt.lwd = 3, title = "By time", title.adj = c(0,0), y.intersp = 0.6, x.intersp = 0.5)

        }
        axis(1,at=seq(0,30,length.out = length(hist_break)), labels=NA)
        axis(1,at=at_hist, labels=lab_hist/1000)
        species <- str_replace(unique(hist_data$Species),"_", " ")
        mtext(species, side = 3, line=1, cex=2)
}

### Loop to run the sp_maps_defaunation function over the database
table_sp_hist <-as.data.frame( matrix(nrow=18, ncol=3))
for(sp in unique(DATABASE$Species)){
        temp_map <- DATABASE[which(DATABASE$Species == sp),]
        #hist_data <-temp_map <- DATABASE[which(DATABASE$Species == sp),]
        #temp_seq <- hist_data[nchar(hist_data$Sequence) > 1,]
        #temp_no_seq <- hist_data[nchar(hist_data$Sequence) < 1,]
        #no_plot_no_seq <- hist(temp_no_seq$Mean_Age, breaks=hist_break,  plot=F)
        #no_plot_seq <- hist(temp_seq$Mean_Age, breaks=hist_break,  plot=F)
        #table_sp_hist[sp,] <- c(unique(hist_data$Species), max(no_plot_no_seq$counts),max(no_plot_seq$counts))
        #temp_map <- DATABASE[which(DATABASE$Species == "Balaena_mysticetus"),]
        #"Balaena_mysticetus"
        sp_hist_defaunation(temp_map)
}

hist_data <- DATABASE[which(DATABASE$Species == "Crocuta_crocuta"),]


#### Add the species name for each one and loop through the database 

breaks=seq(0, 51000, 2000),
hist_break <- c(seq(0, 10000, 2000), 11700, 14000, seq(16000, 50000, 2000))
hist_col <- ifelse(hist_break < 11700, col_holoc, col_pleis)

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