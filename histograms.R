rm(list=ls())
### histograms included in the supplemetary materials

### Required libraries
library(plotrix)
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
col_pleis <- "#5C5C5C"
col_holoc <- "#B5B5B5"
col_DNA <- "#FF7F24"
col_no_DNA <- "#8EE5EE"
hist_break <- c(seq(0, 10000, 2000), 11700, 14000, seq(16000, 50000, 2000))
hist_col <- ifelse(hist_break < 11700, col_holoc, col_pleis)
pleis_holoc_boundary <- 11700

# remove the records older than 50000 years
DATABASE <- DATABASE[-which(DATABASE$Mean_Age >= 50000),]

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
for(sp in unique(DATABASE$Species)){
        temp_map <- DATABASE[which(DATABASE$Species == sp),]
        sp_hist_defaunation(temp_map)
}
