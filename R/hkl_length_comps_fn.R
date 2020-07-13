
hkl_length_comps_fn <- function(catch, name, comp.dir){
years = unique(catch$YEAR)
data  = catch[catch$COMNAME == name, ]
data  = data[!is.na(data$LENGTH), ]

min_len = ifelse(floor(min(data$LENGTH)) > 10, floor(min(data$LENGTH)) , 10)
max_len = floor(max(data$LENGTH))
bin_size = ifelse(max_len - min_len > 60, 4, 2)
len_bins = seq(min_len , max_len - 2*bin_size, bin_size)

len <- c(len_bins, Inf)
data$lbin <- len[findInterval(data$LENGTH, len, all.inside=T)]

results = NULL
for(y in 1:length(years))
{
    # Identify relevant rows
    Which = which(data[,'YEAR'] == years[y] )
    # Skip this year unless there are rows
    if(length(Which)>0)
    {
        Row = c('Year' = years[y])
        Tot = length(data[Which,"LENGTH"])
        # Loop across length bins
        for(l in 1:length(len_bins)){
        	# Subset to relevant rows
        	Which2 = Which[which(data[Which,'lbin'] == len_bins[l])]
        	if(length(Which2) == 0) Row = c(Row, 0)
        	if(length(Which2) >= 1) Row = c(Row, round(100*(length(data[Which2,"LENGTH"]) / Tot),2) )
        }
        # Add to results matrix
        results = rbind(results, Row)
    }    
}    
colnames(results) = c("Year", len_bins)


x <- as.numeric(as.character(results[,"Year"])) 
dat <- results[,-1]
numLens <- ncol(dat)
y <- as.numeric(colnames(dat))
z <- c(unlist(dat),max(dat)) # Changed from max to min to better visualize subset data

title = paste("NWFSC Hook & Line", name)
pngfun(wd = paste0(comp.dir, "/plots"), file = paste0("HKL_", name, '_lengths.png'), h = 7, w = 12)
symbols(c(rep(x,length(y)),0),c(rep(y,each=length(x)),0),circles=z,inches=0.15, xlim = c(min(years), max(years)), ylab = "Length (cm)", 
	ylim = c(0, max(len_bins) + bin_size), xlab = "Year", main = title)
dev.off()
    
}