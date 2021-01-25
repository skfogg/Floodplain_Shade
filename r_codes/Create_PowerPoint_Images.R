library(officer)

filepng <- "C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\pretty_plots\\soil_1.0m_heatmap\\heatmap_sunny_soil_1.0m_"

for (i in 1:24){
assign(paste0("image.file", i),
       paste0(filepng, i,".png"))
}

doc <- read_pptx()
for(i in 1:24){
  doc <- add_slide(doc)
  doc <- ph_with(x = doc, 
                 external_img(get(paste0("image.file", i)), 
                              width = 10, 
                              height = 7.5),
                 location = ph_location_fullsize())
}
print(doc, target = "C:\\Users\\Katie Fogg\\Desktop\\HGSwork\\ph_with_img.pptx")

