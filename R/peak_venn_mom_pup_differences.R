## Find peaks that are unique for colonies
# Find all peaks in a subset of one colony and write to vector. Repeat for all colonies. Create data.frame that shows overall presence or absence of a peak in the respective colony. Find unique peaks for each colony. -> Venn diagramm

library(nVennR)

load("RData/final/scent_nmds-mompup2017_ssbfwb.RData")

## convert scent data.frame to logicals. Peaks == 1, No Peaks == 0. Prepending a + will type cast to integer.
scent.log <- + sapply(scent, as.logical)
scent.log <- as.data.frame(scent.log)
rownames(scent.log) <- rownames(scent)

## find all peaks in a subset of one colony
# index with meta data scent_factors
# as function:
colPeaks <- function(colTag){ # as.character(colTag)
  with(scent_nmds, 
       # write output as a vector, apply function to array (list, data.frame...)
       # subset {scent.log} to only include individuals from "SSB" Colony
       as.vector(apply(scent.log[which(BeachAge == as.character(colTag)),], 
                       # apply on each column
                       2, 
                       # apply function with parameter, prepending a
                       # + will type cast to integer (T=1, F=0)
                       function(x) +any(x!= 0)
       )))
}

# create character.vector with colony level names -> transformed to not colony level names but names
# with information about maturity and colony status!
venn_factor_names <- levels(as.factor(scent_nmds$BeachAge))

# create data.frame with unique peaks for each colony using colPeaks function and apply on each element in 
# venn_factor_names vector
venn_factor.peaks <- as.data.frame(sapply(venn_factor_names, colPeaks))

# define rownames:-> peak names
venn_factor.peaks <- cbind(Peaks = colnames(scent.log), venn_factor.peaks)


## Plot Venn Diagramm

# Subset colonies for Venn Diagram according to the example in 'nVennR' Vignette
FWB1 <- subset(venn_factor.peaks, FWB_1 == 1)$Peaks
FWB2 <- subset(venn_factor.peaks, FWB_2 == 1)$Peaks
SSB1 = subset(venn_factor.peaks, SSB_1 == 1)$Peaks
SSB2 <- subset(venn_factor.peaks, SSB_1 == 1)$Peaks


# create Venn Diagram and output as .svg file (vector graphic)
myVenn <- plotVenn(list(FWB1 = FWB1, 
                        FWB2 = FWB2,
                        SSB1 = SSB1, 
                        SSB2 = SSB2
                    
), # close list
nCycles = 9999,
outFile='RData/final/iter1_mp_ssb_fwb.svg'
) #close plotVenn

# rerun the nVennObj 'myVenn' to increase computation speed and accuracy of the diagramm output
myVenn <- plotVenn(nVennObj = myVenn,
                   outFile = 'RData/final/iter2_mp_ssb_fwb.svg')

myVenn <- plotVenn(nVennObj = myVenn,
                   outFile = 'RData/final/iter3_mp_ssb_fwb.svg')

myVenn <- plotVenn(nVennObj = myVenn,
                   labelRegions = F,
                   borderWidth = 2,
                   setColors = c("#D55E00", 
                                 "#E69F00", 
                                 "#0072B2",
                                 "#56B4E9"),
                   outFile = 'RData/final/Venn_mp_ssb_fwb.svg')
```