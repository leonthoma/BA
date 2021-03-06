# Load & check data ----
setwd("~/Dokumente/Uni/Umwi/B.Sc. Thesis/IDENT/Analysis")
ident_trees_ssm <- read.csv("ident_trees_SSM_18.csv", na.strings = "") # set empyt cells to ""
ident_trees_fr <- read.csv("ident_trees_FR_18_final.csv")
ident_damage_ssm <- read.csv("leaf_damage_SSM_18.csv")
ident_damage_fr <- read.csv("leaf_damage_FR_18.csv", na.strings = "")
ident_insects_ssm <- read.csv("insect_id_SSM_18.csv")
ident_insects_fr <- read.csv("insect_id_FR_18_final.csv")
infos <- read.csv("Ident Zusammenfassung-2_updated_8.7.2018.csv") # FR's Blocks and SR Levels
options(max.print = 3000)

# ident_trees ----

# SSM

# Delete unused columns
  ident_trees_ssm <- ident_trees_ssm[,-which(colnames(ident_trees_ssm) == "Comment")]
  ident_trees_ssm <- ident_trees_ssm[,-which(colnames(ident_trees_ssm) == "Weather")]
  ident_trees_ssm <- ident_trees_ssm[,-which(colnames(ident_trees_ssm) == "Times.sampled")]
  ident_trees_ssm <- ident_trees_ssm[,-which(colnames(ident_trees_ssm) == "Leaves.out..old.")]
  ident_trees_ssm <- ident_trees_ssm[,-which(colnames(ident_trees_ssm) == "Leaves.out..new.")]
  ident_trees_ssm <- ident_trees_ssm[,-which(colnames(ident_trees_ssm) == "Date")]
  ident_trees_ssm <- ident_trees_ssm[,-which(colnames(ident_trees_ssm) == "Time")]
  
# Create column with individual tree numbers B.P.T (Block.Plot.TreeID)
  BPT_col_trees <- as.factor(paste(paste(ident_trees_ssm$Block, ident_trees_ssm$Plot, sep = "."), ident_trees_ssm$Tree.ID, sep = "."))
  ident_trees_ssm$BPT <- BPT_col_trees # add BPT as a new column, has factor class (maybe troublesome)

# Fix missing values
  # B6 P3 T24 and B6 P18 T66 have missing Sampled.percentage values -> set to mean of species
  ident_trees_ssm$Sampled.percentage[which(ident_trees_ssm$BPT == "6.3.24")] <- round(mean(ident_trees_ssm$Sampled.percentage[which(ident_trees_ssm$Species == "BEPA")], na.rm = TRUE)) 
  ident_trees_ssm$Sampled.percentage[which(ident_trees_ssm$BPT == "6.18.66")] <- round(mean(ident_trees_ssm$Sampled.percentage[which(ident_trees_ssm$Species ==  "ACSA")], na.rm = TRUE))
  # B5 P14 T 63 was dead and no extra tree was sampled, set all values to 0
  ident_trees_ssm$Sampled.percentage[which(ident_trees_ssm$BPT == "5.14.63")] <-  0
  ident_trees_ssm$Insects.sampled[which(ident_trees_ssm$BPT == "5.14.63")] <-  0

# Set all sampled percentage values less than 1 to whole numbers
  ident_trees_ssm$Sampled.percentage[which(ident_trees_ssm$Sampled.percentage < 1)] <- 1
  
# Account for sampled percentage per tree for the insect abundance  
  ident_trees_ssm$Insects.sampled <- ident_trees_ssm$Insects.sampled/(ident_trees_ssm$Sampled.percentage/100) 
    
# Check columns
  names(ident_trees_ssm)
  str(ident_trees_ssm)  
  
# Overall looks good; check levels
  levels(ident_trees_ssm$Species) # ok
  levels(ident_trees_ssm$Location) # ok
  # All levels are ok
  
# Check complete without errors

# FR

# Delete unused columns
  ident_trees_fr <- ident_trees_fr[,-which(colnames(ident_trees_fr) == "Comment")]
  ident_trees_fr <- ident_trees_fr[,-which(colnames(ident_trees_fr) == "Weather")]
  ident_trees_fr <- ident_trees_fr[,-which(colnames(ident_trees_fr) == "Date")]
  ident_trees_fr <- ident_trees_fr[,-which(colnames(ident_trees_fr) == "Time")]
  ident_trees_fr <- ident_trees_fr[,-which(colnames(ident_trees_fr) == "Escaped.insect")]
  ident_trees_fr <- ident_trees_fr[,-which(colnames(ident_trees_fr) == "Leaves.out..Y.N.B.")]
  ident_trees_fr <- ident_trees_fr[,-which(colnames(ident_trees_fr) == "Times.sampled")]
  ident_trees_fr <- ident_trees_fr[,-which(colnames(ident_trees_fr) == "Sampling.Height.Bottom")]
  ident_trees_fr <- ident_trees_fr[,-which(colnames(ident_trees_fr) == "Sampling.Height.Top")]
  
# Fill Block and SR-Level columns
  ident_trees_fr$Block <- infos$Feld[ident_trees_fr$Plot] # Get Block values
  ident_trees_fr$SR.Level <- infos$Artenzahl[ident_trees_fr$Plot] # Get SR-Level values

# Create column with individual tree numbers B.P.T (Block.Plot.TreeID)
  BPT_col_trees_fr <- as.factor(paste(paste(ident_trees_fr$Block, ident_trees_fr$Plot, sep = "."), ident_trees_fr$Tree.ID, sep = "."))
  ident_trees_fr$BPT <- BPT_col_trees_fr # add BPT as a new column, has factor class (maybe troublesome)
  
# Fill Consecutive Plot column
  ident_trees_fr$Consecutive.Plot <- rep(c(1:length(unique(ident_trees_fr$Plot))), each = 6 ) # Fill Consecutive Plot column
  
# Fill Location column
  ident_trees_fr$Location <- as.factor("FR") # Fill Location column
  
# Fill Species.type column  
  ident_trees_fr$Species.type <- "Eurp" # set all to Eurp
  ident_trees_fr$Species.type[which(ident_trees_fr$Species %in% c("ACSA", "BEPA", "LALA", "PIST", "PIGL", "QURU"))] <- "NrAm" # Set the non Eurp to NrAm
  ident_trees_fr$Species.type <- as.factor(ident_trees_fr$Species.type)
 
# Create Insects.sampled column
  
# Since the number of sampled Insects per tree was not noted while sampling insect abundance in FR, but rather counted during the identification process,
# the "Frequency" column from the ident_insects_fr file will be used as "Insects.sampled" in the ident_trees_fr df

  # Load ident_insects_fr
  insect_dummy <- read.csv("insect_id_FR_18_final.csv", colClasses = c(NA, NA, NA, rep("NULL", 8), NA,rep("NULL", 7))) # only load the Block, Plot, Tree.ID and Frequency columns
  # Aggregate to Tree level and sum the insects per tree
  insect_dummy$Block <- infos$Feld[insect_dummy$Plot] # Fill Block column
  BPT_dummy <- as.factor(paste(paste(insect_dummy$Block, insect_dummy$Plot, sep = "."), insect_dummy$Tree.ID, sep = "."))
  insect_dummy$BPT <- BPT_dummy # add BPT as a new column
  insect_dummy_agg <- aggregate(insect_dummy$Frequency, by = list(insect_dummy$BPT, insect_dummy$Plot), FUN = sum) # use Plot column to "sort" BPT
  insect_dummy_agg$x <- insect_dummy_agg$x/(ident_trees_fr$Sampled.percentage[which(ident_trees_fr$No.Sample == "FALSCH")]/100) # Account for sampled percentage per tree for the insect abundance (only for positive samples)
  ident_trees_fr$Insects.sampled <- 0 # Create column and set all to zero
  ident_trees_fr$Insects.sampled[which(ident_trees_fr$No.Sample == "FALSCH")] <- insect_dummy_agg$x # Fill Insects.sampled column with aggregated Frequency
  ident_trees_fr <- ident_trees_fr[,-which(colnames(ident_trees_fr) == "No.Sample")] # Delete No.Sample column
  
# Check columns 
  names(ident_trees_fr)
  str(ident_trees_fr)  
  
# Overall looks good; check levels
  levels(ident_trees_fr$Species) # ok
  levels(ident_trees_fr$Location) # ok
  # All levels are ok
  
# Check complete without errors

# Merge ident_trees_ssm and ident_trees _fr
  ident_trees <- merge(ident_trees_ssm, ident_trees_fr, all = T)

# Delete intermediates
  rm(insect_dummy)
  rm(insect_dummy_agg)
  rm(BPT_col_trees)
  rm(BPT_col_trees_fr)
  rm(BPT_dummy)
  
# ident_damage ----

# SSM 

# Delete unused columns
  ident_damage_ssm <- ident_damage_ssm[,-which(colnames(ident_damage_ssm) == "Comment")]
  ident_damage_ssm <- ident_damage_ssm[,-which(colnames(ident_damage_ssm) == "Date")]
  ident_damage_ssm <- ident_damage_ssm[,-which(colnames(ident_damage_ssm) == "Sampled.at.")]
  ident_damage_ssm <- ident_damage_ssm[,-which(colnames(ident_damage_ssm) == "Upper.sampling.height")]
  ident_damage_ssm <- ident_damage_ssm[,-which(colnames(ident_damage_ssm) == "Lower.sampling.height")]
  
# Create column with individual tree numbers B.P.T (Block.Plot.TreeID)
  BPT_col_damage <- as.factor(paste(paste(ident_damage_ssm$Block, ident_damage_ssm$Plot, sep = "."), ident_damage_ssm$Tree.ID, sep = "."))
  ident_damage_ssm$BPT <- BPT_col_damage # add BPT as a new column has factor class (maybe troublesome)  

# Remove B5P26 to have equal Consecutive.Plot columns
  ident_damage_ssm <- ident_damage_ssm[-which(ident_damage_ssm$Block == 5 & ident_damage_ssm$Plot == 26),]

# Set Sampled.T.F column to logical
  ident_damage_ssm$Sampled.T.F <- as.logical(ident_damage_ssm$Sampled.T.F)
  
# Set all the cells with Sampling = T to 0
  na.zeros <- function(x) {
    x[is.na(x)] <- 0
      x
  } 
  # Set all Sampled = T NA's to 0
  ident_damage_ssm[which(ident_damage_ssm$Sampled.T.F == T), c("Chewing", "Mining", "Skeletonizing", "Rolling", "Gall", "Unknown")] <- na.zeros(subset(x = ident_damage_ssm, subset = Sampled.T.F == T, select = c(Chewing, Mining, Skeletonizing, Rolling, Gall, Unknown)))
  
# Create Total damage per leaf col
  ident_damage_ssm$Total.damage.per.leaf <- rowSums(ident_damage_ssm[,c("Chewing", "Mining", "Skeletonizing", "Rolling", "Gall", "Unknown")]) # calculate total damage per leaf

# Delete non sampled rows and then Sampled.T.F col
  ident_damage_ssm <- ident_damage_ssm[which(ident_damage_ssm$Sampled.T.F == T),] # Df now contains only Sampled = T values
  ident_damage_ssm <- ident_damage_ssm[,-which(colnames(ident_damage_ssm) == "Sampled.T.F")] # Col not needed anymore; delete for merging
  
# Check columns
  names(ident_damage_ssm)
  str(ident_damage_ssm)
  
# Overall looks good; check levels
  levels(ident_damage_ssm$Species) # ok
  levels(ident_damage_ssm$Location) # ok
  levels(ident_damage_ssm$Species.type) # ok
  
# Check complete without errors

# FR
  
# Delete unused columns
  ident_damage_fr <- ident_damage_fr[,-which(colnames(ident_damage_fr) == "Dead.needles.leaves")]
  ident_damage_fr <- ident_damage_fr[,-which(colnames(ident_damage_fr) == "Fungal")]
  ident_damage_fr <- ident_damage_fr[,-which(colnames(ident_damage_fr) == "Number.of.Herbivores.on.branch")]
  ident_damage_fr <- ident_damage_fr[,-which(colnames(ident_damage_fr) == "Comment")]
  ident_damage_fr <- ident_damage_fr[,-which(colnames(ident_damage_fr) == "Sampled.at.")]
  ident_damage_fr <- ident_damage_fr[,-which(colnames(ident_damage_fr) == "Date")]

# Fill Block and SR-Level columns
  ident_damage_fr$Block <- infos$Feld[ident_damage_fr$Plot] # Get Block values
  ident_damage_fr$SR.Level <- infos$Artenzahl[ident_damage_fr$Plot] # Get SR-Level values
  
# Create column with individual tree numbers B.P.T (Block.Plot.TreeID)
  BPT_col_damage_fr <- as.factor(paste(paste(ident_damage_fr$Block, ident_damage_fr$Plot, sep = "."), ident_damage_fr$Tree.ID, sep = "."))
  ident_damage_fr$BPT <- BPT_col_damage_fr # add BPT as a new column, has factor class (maybe troublesome)
  
# Fill Consecutive Plot column
  ident_damage_fr$Consecutive.Plot <- rep(c(1:length(unique(ident_damage_fr$Plot))), each = 120) # Fill Consecutive Plot column
  
# Fill Location column
  ident_damage_fr$Location <- as.factor("FR") # Fill Location column
  
# Delete dead, infected and not sampled leaves
  na.rows <- apply(ident_damage_fr[, c("Chewing", "Mining", "Skeletonizing", "Gall")], 1, function(x){
    all(is.na(x))
    })
  ident_damage_fr <- ident_damage_fr[-which(na.rows), ] # Df now contains only Sampled = T values

# Set all Na's to 0
  ident_damage_fr[, "Chewing"] <- ifelse(is.na(ident_damage_fr[, "Chewing"]), 0, ident_damage_fr[, "Chewing"])
  ident_damage_fr[, "Mining"] <- ifelse(is.na(ident_damage_fr[, "Mining"]), 0, ident_damage_fr[, "Mining"])
  ident_damage_fr[, "Skeletonizing"] <- ifelse(is.na(ident_damage_fr[, "Skeletonizing"]), 0,   ident_damage_fr[, "Skeletonizing"])
  ident_damage_fr[, "Gall"] <- ifelse(is.na(ident_damage_fr[, "Gall"]), 0,   ident_damage_fr[, "Gall"])
  
# Create Total damage per leaf col
  ident_damage_fr$Total.damage.per.leaf <- rowSums(ident_damage_fr[,c("Chewing", "Mining", "Skeletonizing", "Gall")]) # calculate total damage per leaf

# Fill Species type col
  ident_damage_fr$Species.type <- "Eurp" # set all to Eurp
  ident_damage_fr$Species.type[which(ident_damage_fr$Species %in% c("ACSA", "BEPA", "LALA", "PIST", "PIGL", "QURU"))] <- "NrAm" # Set the non Eurp to NrAm
  ident_damage_fr$Species.type <- as.factor(ident_damage_fr$Species.type)

# Check columns
  names(ident_damage_fr)
  str(ident_damage_fr)
  
# Overall looks good; check levels
  levels(ident_damage_fr$Species) # ok
  levels(ident_damage_fr$Sampled.at.) # ok
  levels(ident_damage_fr$Location) # ok
  levels(ident_damage_fr$Species.type) # ok
  
# Merge ident_damage_ssm and ident_damage_fr
  ident_damage <- merge(ident_damage_ssm, ident_damage_fr, all = T)
    
# Delete intermediates
  rm(BPT_col_damage)
  rm(BPT_col_damage_fr)
  rm(na.rows)
  rm(na.zeros)
  
# ident_insects ----

# SSM
  
# Delete unused columns
  ident_insects_ssm <- ident_insects_ssm[,-which(colnames(ident_insects_ssm) == "Comment")]
  
# Create column with individual tree numbers B.P.T (Block.Plot.TreeID)
  BPT_col_insects <- as.factor(paste(paste(ident_insects_ssm$Block, ident_insects_ssm$Plot, sep = "."), ident_insects_ssm$Tree.ID, sep = "."))
  ident_insects_ssm$BPT <- BPT_col_insects # add BPT as a new column, has factor class (maybe troublesome)

# Create column with Functional groups
  levels(ident_insects_ssm$Order) # Check which Orders are present
  ident_insects_ssm$Functional.Group <- "Uncategorized"
  # Define functional groups
  ident_insects_ssm$Functional.Group[c(which(ident_insects_ssm$Order == "Collembola"), which(ident_insects_ssm$Suborder == "Astigmata"))] <- "Detritivore"
  ident_insects_ssm$Functional.Group[c(which(ident_insects_ssm$Suborder == "Auchenorrhyncha"), which(ident_insects_ssm$Suborder == "Sternorrhyncha"), which(ident_insects_ssm$Family == "Lygaeidae"), which(ident_insects_ssm$Family == "Pentatomidae"), which(ident_insects_ssm$Family == "Curculionidae"), which(ident_insects_ssm$Order == "Lepidoptera"), which(ident_insects_ssm$Order == "Thysanoptera"), which(ident_insects_ssm$Order == "Myriapoda"))] <- "Herbivore"
  ident_insects_ssm$Functional.Group[c(which(ident_insects_ssm$Order == "Araneae"), which(ident_insects_ssm$Suborder == "Mesostigmata"), which(ident_insects_ssm$Family == "Nabidae"), which(ident_insects_ssm$Family == "Coccinellidae"), which(ident_insects_ssm$Family == "Carabidae"), which(ident_insects_ssm$Order == "Opiliones"), which(ident_insects_ssm$Order == "Neuroptera"), which(ident_insects_ssm$Family == "Staphylinidae"))] <- "Predator"
  ident_insects_ssm$Functional.Group[c(which(ident_insects_ssm$Order == "Psocoptera"), which(ident_insects_ssm$Family == "Latridiidae"))] <- "Funghivore"
  ident_insects_ssm$Functional.Group[c(which(ident_insects_ssm$Order == "Hymenoptera"), which(ident_insects_ssm$Order == "Diptera"))] <- "Heterogeneous"
  
# FR
  
# Delete unused columns
  ident_insects_fr <- ident_insects_fr[,-which(colnames(ident_insects_fr) == "Description")]
  ident_insects_fr <- ident_insects_fr[,-which(colnames(ident_insects_fr) == "Image")]
  ident_insects_fr <- ident_insects_fr[,-which(colnames(ident_insects_fr) == "Box.No.")]
  ident_insects_fr <- ident_insects_fr[,-which(colnames(ident_insects_fr) == "S.No.")]

# Fill Block and SR-Level columns
  ident_insects_fr$Block <- infos$Feld[ident_insects_fr$Plot] # Get Block values
  ident_insects_fr$SR.Level <- infos$Artenzahl[ident_insects_fr$Plot] # Get SR-Level values
  
# Create column with individual tree numbers B.P.T (Block.Plot.TreeID)
  BPT_col_insects_fr <- as.factor(paste(paste(ident_insects_fr$Block, ident_insects_fr$Plot, sep = "."), ident_insects_fr$Tree.ID, sep = "."))
  ident_insects_fr$BPT <- BPT_col_insects_fr # add BPT as a new column, has factor class (maybe troublesome)
  
# Fill Consecutive Plot column
  plot_count <- as.data.frame(table(ident_insects_fr$Plot)) # intermediate for frequency of each plot
  ident_insects_fr$Consecutive.Plot <- rep(c(1:length(unique(ident_insects_fr$Plot))), times = c(plot_count$Freq)) # Fill Consecutive Plot column
  
# Fill Location column
  ident_insects_fr$Location <- as.factor("FR") # Fill Location column
    
# Format df to have one row for every insect
  library(splitstackshape)
  ident_insects_fr <- expandRows(dataset = ident_insects_fr, count = "Frequency") # Duplicated rows based on "Frequency"
  
# Fill Species type col
  ident_insects_fr$Species.type <- "Eurp" # set all to Eurp
  ident_insects_fr$Species.type[which(ident_insects_fr$Species %in% c("ACSA", "BEPA", "LALA", "PIST", "PIGL", "QURU"))] <- "NrAm" # Set the non Eurp to NrAm
  ident_insects_fr$Species.type <- as.factor(ident_insects_fr$Species.type)

# Create column with Functional groups
  levels(ident_insects_fr$Order) # Check which Orders are present
  ident_insects_fr$Functional.Group <- "Uncategorized"  
  
# Define functional groups
  ident_insects_fr$Functional.Group[c(which(ident_insects_fr$Order == "Collembola"), which(ident_insects_fr$Order == "Isopoda"))] <- "Detritivore"
  ident_insects_fr$Functional.Group[c(which(ident_insects_fr$Suborder == "Auchennorhyncha"), which(ident_insects_fr$Suborder == "Sternorrhyncha"), which(ident_insects_fr$Family == "Apionidae"), which(ident_insects_fr$Family == "Chrysomelidae"), which(ident_insects_fr$Family == "Curculionidae"), which(ident_insects_fr$Order == "Blattodea"), which(ident_insects_fr$Order == "Pulmonata"), which(ident_insects_fr$Order == "Dermaptera"), which(ident_insects_fr$Order == "Thysanoptera"), which(ident_insects_fr$Order == "Lepidoptera"), which(ident_insects_fr$Family == "Coreidae"))] <- "Herbivore"
  ident_insects_fr$Functional.Group[c(which(ident_insects_fr$Order == "Araneae"), which(ident_insects_fr$Family == "Coccinellidae"), which(ident_insects_fr$Family == "Staphylinidae"), which(ident_insects_fr$Order == "Ixodida"), which(ident_insects_fr$Order == "Neuroptera"), which(ident_insects_fr$Family == "Nabidae"), which(ident_insects_fr$Family == "Miridae"), which(ident_insects_fr$Family == "Reduviidae"))] <- "Predator"
  ident_insects_fr$Functional.Group[c(which(ident_insects_fr$Family == "Endomychidae"), which(ident_insects_fr$Family == "Latridiidae"), which(ident_insects_fr$Family == "Phalacridae"), which(ident_insects_fr$Order == "Psocoptera"))] <- "Funghivore"
  ident_insects_fr$Functional.Group[c(which(ident_insects_fr$Order == "Hymenoptera"), which(ident_insects_fr$Order == "Diptera"))] <- "Heterogeneous"
  # Thysanura are not a real taxonomic group so leave in uncategorized
  
# Merge ident_insects_ssm and ident_insects_fr
  ident_insects <- merge(ident_insects_ssm, ident_insects_fr, all = T)
  ident_insects <- ident_insects[,-which(colnames(ident_insects) == "Life.Stage")] # Was only needed for assigning groups
  
# Delete intermediates
  rm(infos)
  rm(plot_count)
  rm(BPT_col_insects)
  rm(BPT_col_insects_fr)
  
  rm(ident_trees_ssm)
  rm(ident_trees_fr)
  rm(ident_damage_ssm)
  rm(ident_damage_fr)
  rm(ident_insects_ssm)
  rm(ident_insects_fr)