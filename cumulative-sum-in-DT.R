# cumulative sum number of variants (or, a moving sum in a given window size)
# claire malley march 31, 2017
library(data.table)
library(plyr)
library(zoo)
library(ggplot2)
library(ggthemes)

# 1. get number of variants in each data.table
x <- il4r.preQC[,.N] #24961
x2 <- il4r[,.N]#17428

# 2. new column where "ID" just indicates there is a variant present. 
il4r.preQC[,ID := seq(1, 24961, 1)]
il4r[,ID := seq(1, 17428, 1)]

# 3. new data.table that is one row per position for the entire length of the window considered in il4r/il4r.pre.
seqDT.il4r <- as.data.table(seq(26516211, 28185184))

# 4. merge the data.table containing all positions and the data.table containing only positions with variants. this creates NAs in the latter.
il4r.preQC.merge <- merge(il4r.preQC, seqDT.il4r, by.x="POS", by.y="V1", all.y=T)

# 5. this creates all '1's where there are variants.
il4r.preQC.merge[, ID:= ifelse(ID > 1, 1, ID)]

# 6. replace NAs with zeroes.
il4r.preQC.merge[, ID:= ifelse(is.na(ID), 0, 1)]

# 7. I only need these two columns...
il4r.preQC.merge <- il4r.preQC.merge[,c("POS", "ID")]

# 8. use rollsumr() from the zoo package to calculate the cumulative ("moving") sum of the ID column. in this case I have a big window size of 10K because it is very noisy when it is smaller.
Cum.Sum.10000.preQC <- transform(il4r.preQC.merge, roll = ave(ID, FUN = function(x) rollsumr(x, 10000, fill = NA)))

ggplot(Cum.Sum.10000) + geom_line(aes(x=POS, y=roll)) +theme_bw() + labs(y="Number of variants in window", x="Postion, bp", title="Cumulative sum of number of variants in a 10000-bp sliding window, IL4R region")
