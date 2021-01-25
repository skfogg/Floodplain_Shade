
monthnames_abrv <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

daysinmonth <- c(31, 28, 31, 
                 30, 31, 30,
                 31, 31, 30, 
                 31, 30, 31)

dayofyr <- paste(monthnames_abrv[1], seq(1:daysinmonth[1]))
for(i in 2:12){
 dayofyr <- c(dayofyr, paste(monthnames_abrv[i], seq(1:daysinmonth[i])))
}






