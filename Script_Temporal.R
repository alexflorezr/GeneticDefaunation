table(nchar(DATABASE[which(is.na(DATABASE$Rank)),31]) > 1)
sum(DATABASE$Rank == "")
(table(DATABASE$Rank)*100)/10754


