change.labels.order <-
function(path) {
  all.names <- read.csv(path)
  
  pots <- nrow(all.names)
  
  if((pots/14)%%1==0){pots.per.block <- 14}else{
        if((pots/10)%%1==0){pots.per.block <- 10}else{message("Names do not correspond either to blocks of 14 (monosp communities), neither blocks of 10 (mixed communities)")}
  }
  
  blocks <- pots/pots.per.block
  
  
  if(pots.per.block==14){
        block.order <- c(1, NA, 8, NA, 2, 3, 9, 10, 4, 5, 11, 12, 6, 7, 13, 14)
  }else{block.order <- c(1, NA, 6, NA, 2, 3, 7, 8, 4, 5, 9, 10)}
  
  ordered.pots <- unlist(lapply(seq(0, pots.per.block*(blocks-1), by=pots.per.block), "+", block.order))
  
  names <- all.names[ordered.pots,, drop=F]
  
  names$names <- as.character(names$names)
  names[is.na(names$names), "names"] <- "mossless"
  print(names)
  
  write.table(names, "tif.mossless/names.csv", row.names=F, quote=F)
}
