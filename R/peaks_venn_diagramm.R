## Find peaks that are unique for colonies
# Find all peaks in a subset of one colony and write to vector. Repeat for all colonies. Create data.frame that shows overall presence or absence of a peak in the respective colony. Find unique peaks for each colony. -> Venn diagramm

library(nVennR)

load("RData/final/scent_nmds-pupsonly2017_allcolonies.RData")

## convert scent data.frame to logicals. Peaks == 1, No Peaks == 0. Prepending a + will type cast to integer.
scent.log <- + sapply(scent, as.logical)
scent.log <- as.data.frame(scent.log)
rownames(scent.log) <- rownames(scent)

## find all peaks in a subset of one colony
# index with meta data scent_factors
# as function:
colPeaks <- function(colTag){ # as.character(colTag)
  with(scent_factors, 
       # write output as a vector, apply function to array (list, data.frame...)
       # subset {scent.log} to only include individuals from "SSB" Colony
       as.vector(apply(scent.log[which(colony == as.character(colTag)),], 
                       # apply on each column
                       2, 
                       # apply function with parameter, prepending a
                       # + will type cast to integer (T=1, F=0)
                       function(x) +any(x!= 0)
       )))
}

# create character.vector with colony level names
colony_names <- levels(as.factor(scent_factors$colony))

# create data.frame with unique peaks for each colony using colPeaks function and apply on each element in 
# colony_names vector
colony.peaks <- as.data.frame(sapply(colony_names, colPeaks))

# define rownames:-> peak names
colony.peaks <- cbind(Peaks = colnames(scent.log), colony.peaks)


## Plot Venn Diagramm

# Subset colonies for Venn Diagram according to the example in 'nVennR' Vignette
FWB <- subset(colony.peaks, FWB == 1)$Peaks
johnson <- subset(colony.peaks, johnson == 1)$Peaks
landing_beach = subset(colony.peaks, landing_beach == 1)$Peaks
main_bay <- subset(colony.peaks, main_bay == 1)$Peaks
natural_arch <- subset(colony.peaks, natural_arch == 1)$Peaks
SSB <- subset(colony.peaks, SSB == 1)$Peaks

# create Venn Diagram and output as .svg file (vector graphic)
myVenn <- plotVenn(list(FWB = FWB, 
                        Johnson = johnson,
                        Landing = landing_beach,
                        MainBay = main_bay,
                        NaturalArch = natural_arch,
                        SSB = SSB
), # close list
nCycles = 9999,
outFile='~/iter1.svg'
) #close plotVenn

# rerun the nVennObj 'myVenn' to increase computation speed and accuracy of the diagramm output
myVenn <- plotVenn(nVennObj = myVenn,
                   outFile = '~/iter2.svg')

myVenn <- plotVenn(nVennObj = myVenn,
                   outFile = '~/iter3.svg')

myVenn <- plotVenn(nVennObj = myVenn,
                   outFile = '~/Venn_allcolonies.svg')
```