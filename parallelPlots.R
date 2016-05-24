args<-commandArgs(TRUE)
# takes:
# an input csv file, with ";" separator, where the first column is named "ID", and the last is "type"
# the number of column to be viewed as input variables
# the number of column to be viewed as output variables
# the column number to be used as filter, then the extremes (low and up) to filter the data
# a second column number to be used as filter, then the extremes (low and up) to filter the data
# a 0/1 flag to filter data upon the "type" column
# width, height and name of the ouput png file
# the column upon wich grouping the lines in colors
# how many intervals (then colors) to use
# the working directory
## args format  (input_file.csv, n_col_input, ncol_output, col_filter1, low1, up1, col_filter2, low2, up2, only_DoE_0_1, width_px, height_px, output_file.png, group_col, group_intervals, working_dir)

setwd(args[length(args)]) #set wdir
args <- args[-length(args)] #rm wdir entry in args

require(MASS)
require(dplyr)

onlyDoe <- args[10] %>% as.numeric() # 1 if onlyDoe
lfilter <- args[4] %>% as.numeric()  # 0 if the dataset has not to be filtered
lfilter2 <- args[7] %>% as.numeric()

# just header in dd
dd <-args[1] %>% read.csv(sep=";") %>% as.tbl() %>% names()
# if there's no filter, use "ID" column
if (lfilter == 0){
  col <- "ID"
  low <-  args[1] %>% read.csv(sep=";") %>% as.tbl() %>% select_("ID") %>% min
  up <-  args[1] %>% read.csv(sep=";") %>% as.tbl() %>% select_("ID") %>% max
} else{
  col <- dd[lfilter]
  low <- args[5] %>% as.numeric
  up <- args[6] %>% as.numeric
  if(lfilter2 != 0){
    col2 <- dd[lfilter2]
    low2 <- args[8] %>% as.numeric
    up2 <- args[9] %>% as.numeric
  }
}

if (onlyDoe){
  ds <- args[1] %>% read.csv(sep=";") %>% as.tbl() %>% filter(type=="exact") %>%
    select_("-type")
} else{
  ds <- args[1] %>% read.csv(sep=";") %>% as.tbl() %>% select_("-type")
}

# absolute values in the dataset: this is to fix the scale beside of filtering, required in the project
absmax <- apply(ds, 2, max)
absmin <- apply(ds, 2, min)

# first filter is without check, since we use "ID" as default
ds <- ds[(ds[, col] <= up) & (ds[, col] >= low), ]
# second filter with check
if(lfilter2 != 0)
  ds <- ds[(ds[, col2] <= up2) & (ds[, col2] >= low2), ]
###

ds <- rbind(ds, absmin)
ds <- rbind(ds, absmax)
ds <- ds %>% as.tbl()

# useful to group with colors
intervals <- as.numeric(args[length(args)])
args <- args[-length(args)] #rm group_intervals entry in args

gcol <- as.numeric(args[length(args)])

mmin <- args[1] %>% read.csv(sep = ";") %>% as.tbl() %>% select_(gcol) %>% min
mmax <- args[1] %>% read.csv(sep = ";") %>% as.tbl() %>% select_(gcol) %>% max
delta <- (mmax - mmin) / intervals
offst <- mmin %/% delta

png(args[13], width = as.numeric(args[11]), height= as.numeric(args[12]))
    
  for(i in 1:nrow(ds)) intervals[i] <- ds[i, gcol] %/% delta - offst + 1  
  ds <- cbind(ds, as.numeric(intervals))
  names(ds)[length(ds)] <- "interval"

  clrs <- colors()[ds$interval * 5]
  
  ds <- ds %>% select_("-ID")
  ds <- ds %>% select_("-interval")
  parcoord(ds, col = clrs, var.label=TRUE)
  
dev.off()
