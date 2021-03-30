##
## create all batch.pfx files
##

batchNames <- list.files("C:/Users/skati/Desktop/Floodplain_Shade_inputDocs")[-1]
for(i in 2:length(batchNames)){
writeLines(batchNames[i], 
           paste0("C:/Users/skati/Desktop/Floodplain_Shade_inputDocs/", batchNames[i], "/batch.pfx"))
}

##
## create hsbatch file
##

batchlist <- c("batch directory list", batchNames)
batchlist <- mapply(function(l) paste0(l, "\n"), 
                    batchlist) 
batchlist <- do.call(paste0, as.list(batchlist))
writeLines(batchlist, "C:/Users/skati/Desktop/Floodplain_Shade_inputDocs/floodplain_shade.hsbatch")



