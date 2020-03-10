library(vegan)
library(ggplot2)

# form geographical distance matrix (in meter)
# rows and cols ordered after levels(as.factor(scent_factors$colony))
geogr_dist_beaches <- matrix(c(0,	1661,	225,	430,	2002,	354,
                               1661,	0,	1634,	1231,	3593,	1636,
                               225,	1634,	0,	454,	1968,	129,
                               430,	1231,	454,	0,	2410,	517,
                               2002,	3593,	1968,	2410,	0,	1957,
                               354,	1636,	129,	517,	1957,	0),
                             nrow = 6, ncol = 6)


# load in data to be transformed to be comparable in a Mantel test with geographical distances
load("RData/objects/pup_colonies_nmds_scaling.RData")

# transfer BeachAge Column from scent_nmds to meta data.frame scent_factors
scent_factors <- cbind(scent_factors, 
                       BeachAge = scent_nmds$BeachAge)
# create index column for meta data frame
scent_factors <- cbind(scent_factors,
                       SampleIndex = 1:length(rownames(scent_factors)))

# iterate/repeat mantel test 
# create data.frame to track mantel results over repeated tests
mantel_btrap_cont <- data.frame(R2 = double(), p = double())

for (iter in 1:999) {
  # sample one individual for each colony and assign index of individual to new vector
  # that scent_Index_mantel_permute can then be used easily to reference indeces in all scent dfs
  scent_Index_mantel_permute <- NULL
  for (i in 1:length(levels(as.factor(scent_factors$colony)))) {
    scent_Index_mantel_permute[i] <- sample(scent_factors$SampleIndex[scent_factors$colony == levels(as.factor(scent_factors$colony))[i]], 
                                            1, 
                                            replace = F)
  } #close i
  
  # create scent profile dissimilarity matrix for 6 differing individuals using Bray-Curtis 
  scent.mantel <- as.matrix(vegdist(scent[scent_Index_mantel_permute,], method = "bray"))
  
  # perform mantel test: scent dissimilarity by metric distance 
  mantel_result <- mantel(scent.mantel, geogr_dist_beaches, 
                          method = "pearson",
                          permutations = 999)
  # store results for rbind in new vector
  mantel_iter_result <- cbind(mantel_result$statistic, mantel_result$signif)
  mantel_btrap_cont <- rbind(mantel_btrap_cont,
                             mantel_iter_result)
} #close iter
colnames(mantel_btrap_cont) <- c("R2", "p-Value")

mantel_btrap_plot <- ggplot(data = mantel_btrap_cont, aes(x = `p-Value`, y = R2)) +
  geom_point(size = 1) + 
  geom_vline(xintercept = 0.05, color = "red", linetype = 3) +
  scale_x_continuous("p-Value",
                     breaks = c(0.05, 0.2, 0.35, 0.5, 0.65, 0.8, 0.95)) +
  ylab("R2") + 
  theme_classic() +
  theme(panel.background = element_rect(colour = "black", size = 1.25, fill = NA),
        axis.text = element_text(colour = "black"),
        legend.position = "none")
mantel_btrap_plot

ggsave('mantel_btrap_plot.svg', 
       plot = mantel_btrap_plot,
       path="RData/figures/")

  


## MANTEL TEST BY COLONY LEVEL SCENT PROFILES BASED ON SUMMING INDIVIDUAL PEAKS TO SIMULATE A COLONY PROFILE
## create colony scent profiles for six breeding beaches of Antarctic fur seals

# load in aligned peak data and metadata of pup colonies
load("RData/objects/pup_colonies_nmds_scaling.RData")

# How to do it, while storing everything in an r list?
fwb_profile <- (apply(scent[which(scent_factors$colony == "FWB"),], 2, function(x) sum(x))) #[x>0]
johnson_profile <- (apply(scent[which(scent_factors$colony == "johnson"),], 2, function(x) sum(x))) #[x>0]
landing_profile <- (apply(scent[which(scent_factors$colony == "landing_beach"),], 2, function(x) sum(x))) #[x>0]
mainbay_profile <- (apply(scent[which(scent_factors$colony == "main_bay"),], 2, function(x) sum(x))) #[x>0]
natarch_profile <- (apply(scent[which(scent_factors$colony == "natural_arch"),], 2, function(x) sum(x))) #[x>0]
ssb_profile <- (apply(scent[which(scent_factors$colony == "SSB"),], 2, function(x) sum(x))) #[x>0]

colony_profiles <- as.data.frame(rbind(FWB = fwb_profile,
                                       Johnson = johnson_profile,
                                       LandingBeach = landing_profile,
                                       MainBay = mainbay_profile,
                                       NatArch = natarch_profile,
                                       SSB = ssb_profile))
colony_profiles_dist <- as.matrix(vegan::vegdist(colony_profiles, method = "bray"))

# form geographical distance matrix (in meter)
# rows and cols ordered after levels(as.factor(scent_factors$colony))
geogr_dist_beaches <- matrix(c(0,	1661,	225,	430,	2002,	354,
                               1661,	0,	1634,	1231,	3593,	1636,
                               225,	1634,	0,	454,	1968,	129,
                               430,	1231,	454,	0,	2410,	517,
                               2002,	3593,	1968,	2410,	0,	1957,
                               354,	1636,	129,	517,	1957,	0),
                             nrow = 6, ncol = 6)

# Manteltest
mantel(colony_profiles_dist, geogr_dist_beaches, 
       method = "pearson",
       permutations = 999)
# RESULT:
# Call:
#   mantel(xdis = colony_profiles_dist, ydis = geogr_dist_beaches,      method = "pearson", permutations = 999) 
# 
# Mantel statistic r: -0.1771 
# Significance: 0.67917 
# 
# Upper quantiles of permutations (null model):
#   90%   95% 97.5%   99% 
#   0.479 0.531 0.548 0.561 
# Permutation: free
# Number of permutations: 719
#---------------------------


## MANTEL TEST BY COLONY LEVEL SCENT PROFILES BASED ON MEAN INDIVIDUAL PEAKS TO SIMULATE A COLONY PROFILE
## create colony scent profiles for six breeding beaches of Antarctic fur seals

# load in aligned peak data and metadata of pup colonies
load("RData/final/scent_nmds-pupsonly2017_allcolonies.RData")

# How to do it, while storing everything in an r list?
fwb_profile <- (apply(scent[which(scent_factors$colony == "FWB"),], 2, function(x) mean(x))) #[x>0]
johnson_profile <- (apply(scent[which(scent_factors$colony == "johnson"),], 2, function(x) mean(x))) #[x>0]
landing_profile <- (apply(scent[which(scent_factors$colony == "landing_beach"),], 2, function(x) mean(x))) #[x>0]
mainbay_profile <- (apply(scent[which(scent_factors$colony == "main_bay"),], 2, function(x) mean(x))) #[x>0]
natarch_profile <- (apply(scent[which(scent_factors$colony == "natural_arch"),], 2, function(x) mean(x))) #[x>0]
ssb_profile <- (apply(scent[which(scent_factors$colony == "SSB"),], 2, function(x) mean(x))) #[x>0]

colony_profiles <- as.data.frame(rbind(FWB = fwb_profile,
                                       Johnson = johnson_profile,
                                       LandingBeach = landing_profile,
                                       MainBay = mainbay_profile,
                                       NatArch = natarch_profile,
                                       SSB = ssb_profile))
colony_profiles_dist <- as.matrix(vegan::vegdist(colony_profiles, method = "bray"))

# form geographical distance matrix (in meter)
# rows and cols ordered after levels(as.factor(scent_factors$colony))
geogr_dist_beaches <- matrix(c(0,	1661,	225,	430,	2002,	354,
                               1661,	0,	1634,	1231,	3593,	1636,
                               225,	1634,	0,	454,	1968,	129,
                               430,	1231,	454,	0,	2410,	517,
                               2002,	3593,	1968,	2410,	0,	1957,
                               354,	1636,	129,	517,	1957,	0),
                             nrow = 6, ncol = 6)

# Manteltest
mantel(colony_profiles_dist, geogr_dist_beaches, 
       method = "pearson",
       permutations = 999)

# Call:
#   mantel(xdis = colony_profiles_dist, ydis = geogr_dist_beaches,      method = "pearson", permutations = 999) 
# 
# Mantel statistic r: -0.2559 
# Significance: 0.66528 
# 
# Upper quantiles of permutations (null model):
#   90%   95% 97.5%   99% 
#   0.411 0.772 0.799 0.844 
# Permutation: free
# Number of permutations: 719

