files <- list.files('data/Nigeria_Climate')


simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


t1 <- lapply(files,function(x){
  m1 <- apply(read.csv(paste('data/Nigeria_Climate/',x, sep = ''))[,2:5], 2, mean)
  
  return(
    list(
    name = simpleCap(gsub('-',' ',gsub("^-|,-nigeria.csv","",x))),
    average_min = m1[1],
    average_mean = m1[2],
    average_max = m1[3],
    average_precipitation = m1[4])
  )
})


df1 <- (as.data.frame(do.call(rbind, t1))) %>% mutate_all(unlist)


write.csv(df1,"data/Nigeria_Climate_Aggregate.csv", row.names = F)
