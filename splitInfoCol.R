# A data.table implementation of VCF data processing: split the INFO column into its parts and reformat to just the data.
# Extremely fast!

library(data.table)

splitInfoCol <- function(x){
  dt <- x
  dt <- dt[,c("POS", "INFO")]
  dt <- dt[,c("BRF", "FR", "HP", "HapScore", "MGOF", "MMLQ", "MQ", "NF", "NR", "PP", "QD", "SC", "SbPval", "Source", "TC", "TCF", "TCR", "TR", "WE", "WS") := tstrsplit(INFO, ";", fixed=T)]
  for(i in names(dt[,-c(1)])){
    dt[[i]] <- sub('.*=', '', dt[[i]])
  }
  return(dt)
}

# use:

# vcf <- fread(...)
# vcf <- splitInfoCol(vcf)
