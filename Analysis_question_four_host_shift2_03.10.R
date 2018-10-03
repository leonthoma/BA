# Analysis Question four
setwd("~/Dokumente/Uni/Umwi/B.Sc. Thesis/IDENT/Analysis")
source(file = "data_formatting.R", local = T)
library(ggplot2)

# # Abbreviations:----
# Eurp: european sp.
# NrAm: northamerican sp.
# FR: Freiburg site
# SSM: Sault. Ste. Marie site

## CHANGE LOG
# 30.09: corrected "ratio" for app. comp
#        problem with  bug_count_NrAm_FR due to non exsitant entry for PIGL !!! -> SOLVED
# 01.10: changed host_shift df to absolute p. abundance
#        added percentage per location to host_shift df

# Polydrusus abundance ----

# NrAm in SSM

  # Create a df with NrAm trees from SSM
  # Only logical reason to keep non p trees is to plot a ratio of no. of p per tot. no of insects per plot
  NrAm_SSM_trees <- subset(ident_trees, subset = Location == "SSM" & Species.type == "NrAm", select = c("Consecutive.Plot", "BPT", "Species", "Insects.sampled"))

  # Create a df with focus species
  NrAm_SSM_focus <- subset(ident_insects, subset = Location == "SSM" & Species.type == "NrAm" & Genus != "", select = c("Consecutive.Plot", "BPT", "Species")) # Select all that have a genus entry -> equals select all phyllobius and polydrusus
  NrAm_SSM_focus$P.sampled <- 1 # Add the count for the focus species
  
  # Merge the dfs
  NrAm_SSM_comb <- merge(NrAm_SSM_trees, NrAm_SSM_focus, all = T)
  NrAm_SSM_comb <- aggregate(Insects.sampled ~ Consecutive.Plot + Species + P.sampled, data = NrAm_SSM_comb, FUN = mean)
  NrAm_SSM_comb$P.sampled <- NrAm_SSM_comb$P.sampled/NrAm_SSM_comb$Insects.sampled # Compute the ratio between the total insect abundance an the P. abundance
  NrAm_SSM_comb$Category <- 1 # Add category column
  NrAm_SSM_comb <- NrAm_SSM_comb[,-1] # delete Consecutive.Plot column

# NrAm in FR

  # Create a df with NrAm trees from FR
  # Only logical reason to keep non p trees is to plot a ratio of no. of p per tot. no of insects per plot
  NrAm_FR_trees <- subset(ident_trees, subset = Location == "FR" & Species.type == "NrAm", select = c("Plot", "BPT", "Species", "Insects.sampled"))
  
  # Create a df with focus species
  NrAm_FR_focus <- subset(ident_insects, subset = Location == "FR" & Species.type == "NrAm" & Genus != "", select = c("Plot", "BPT", "Species")) # Select all that have a genus entry -> equals select all phyllobius and polydrusus
  NrAm_FR_focus$P.sampled <- 1 # Add the count for the focus species
  
  # Merge the dfs
  NrAm_FR_comb <- merge(NrAm_FR_trees, NrAm_FR_focus, all =T)
  NrAm_FR_comb <- aggregate(Insects.sampled ~ Plot + Species + P.sampled, data = NrAm_FR_comb, FUN = mean)
  NrAm_FR_comb$P.sampled <- NrAm_FR_comb$P.sampled/NrAm_FR_comb$Insects.sampled # Compute the ratio between the total insect abundance an the P. abundance
  NrAm_FR_comb$Category <- 2 # Add category column
  NrAm_FR_comb <- NrAm_FR_comb[,-1] # delete Plot column
  
# Eurp in FR

  # Create a df with Eurp trees from FR
  # Only logical reason to keep non p trees is to plot a ratio of no. of p per tot. no of insects per plot
  Eurp_FR_trees <- subset(ident_trees, subset = Location == "FR" & Species.type == "Eurp", select = c("Plot", "BPT", "Species", "Insects.sampled"))
  
  # Create a df with focus species
  Eurp_FR_focus <- subset(ident_insects, subset = Location == "FR" & Species.type == "Eurp" & Genus != "", select = c("Plot", "BPT", "Species")) # Select all that have a genus entry -> equals select all phyllobius and polydrusus
  Eurp_FR_focus$P.sampled <- 1 # Add the count for the focus species
  
  # Merge the dfs
  Eurp_FR_comb <- merge(Eurp_FR_trees, Eurp_FR_focus, all =T)
  Eurp_FR_comb <- aggregate(Insects.sampled ~ Plot + Species + P.sampled, data = Eurp_FR_comb, FUN = mean)
  Eurp_FR_comb$P.sampled <- Eurp_FR_comb$P.sampled/Eurp_FR_comb$Insects.sampled # Compute the ratio between the total insect abundance an the P. abundance
  Eurp_FR_comb$Category <- 3 # Add category column
  Eurp_FR_comb <- Eurp_FR_comb[,-1] # delete Plot column
  
# Linear models ----
  
  # Df with all three categories
  abun_all <- merge(merge(NrAm_FR_comb, Eurp_FR_comb, all = T), NrAm_SSM_comb, all = T)
  abun_all$Tree.genus <- 0 # create Tree.genus col
  # STILL DO RATIO OR JUST PLOT THE ABUNDANCE OF WEEVILS PER PLOT (P.Sampled)
  
  # Set tree genera
  abun_sp_gen <- data.frame("Tree.sp" = sort(unique(abun_all$Species)),"Tree.genus" = c("Acer", "Betula", "Larix", "Picea", "Pinus", "Quercus")) # df with tree species and genus col
  for(i in 1:12) {  
    abun_all$Tree.genus[which(abun_all$Species == as.character(abun_sp_gen$Tree.sp[i]))] <- as.character(abun_sp_gen$Tree.genus[i])
  } # Set congeners to respecitve genus
  abun_all$Category <- as.factor(abun_all$Category)
  abun_all$Tree.genus <- as.factor(abun_all$Tree.genus)
  
  # Mean and SD
  genera <- c("Acer", "Betula", "Larix", "Picea", "Pinus", "Quercus") # create vector containing the genera names
  categories <- c(1,2,3) # create vector containing category levels 
  
  abun_all$P.sampled_SD <- 0 # Add SD col to abun_all
  abun_all$P.sampled_Mean <- 0 # Add Mean col to abun_all
  abun_all$P.sampled_SE <- 0 # Add SE col to abun_all
  for(p in 1:3) { # for every category...
    for(i in 1:6) { #...do...
      abun_all$P.sampled_SD[which(abun_all$Tree.genus == as.character(genera[i]) & abun_all$Category == categories[p])] <- sd(abun_all$P.sampled[which(abun_all$Tree.genus == as.character(genera[i]) & abun_all$Category == categories[p])], na.rm = T) # calculate SD values and write them into the df
      abun_all$P.sampled_Mean[which(abun_all$Tree.genus == as.character(genera[i]) & abun_all$Category == categories[p])] <- mean(abun_all$P.sampled[which(abun_all$Tree.genus == as.character(genera[i]) & abun_all$Category == categories[p])]) # calulate mean values and write them into the df
      abun_all$P.sampled_SE[which(abun_all$Tree.genus == as.character(genera[i]) & abun_all$Category == categories[p])] <- abun_all$P.sampled_SD[which(abun_all$Tree.genus == as.character(genera[i]) & abun_all$Category == categories[p])]/sqrt(length(which(abun_all$Tree.genus == as.character(genera[i]) & abun_all$Category == categories[p])))# calulate se values and write them into the df
    }
  }
  
  
  # Linear model
  # Check predictor interaction
  interaction.plot(x.factor = abun_all$Category, trace.factor = abun_all$Tree.genus, response = abun_all$P.sampled, col = c(1:6))
  # Plot shows that interactions occur -> see anova result for the significance of the Genus:Category interaction
  abun_test <- lm(formula = P.sampled ~ Tree.genus * Category, data = abun_all) 
  anova(abun_test)
  
  # With independent predictors
  
  abun_fit <- lm(formula = P.sampled ~ Tree.genus + Category, data = abun_all)
  summary(abun_fit)
  anova(abun_fit)
  par(mfrow = c(2,2))
  plot(abun_fit)
  
  # With dependent predictors
  
  abun_fit2 <- lm(formula = P.sampled ~ Tree.genus * Category, data = abun_all)
  summary(abun_fit2)
  anova(abun_fit2)
  par(mfrow = c(2,2))
  plot(abun_fit2)
  
# Plots ----
  
  ggplot(data = abun_all, aes(x = as.factor(Tree.genus), y = P.sampled , fill = Category)) +
    geom_bar(stat = "identity", position = position_dodge(1)) +
    geom_errorbar(aes(ymin = P.sampled - P.sampled_Mean, ymax = P.sampled + P.sampled_Mean), width = 0.1, position = position_dodge(1)) +
    scale_fill_discrete(name = "Category",
                        breaks = c(1, 2, 3),
                        labels = c("NrAm in SSM", "NrAm in FR", "Eurp in FR")) +
    xlab("Genus") + ylab("No. of Focus sp. per total insect abundance")
  
# Apparent competition ----
  
# NrAm in FR
  
  # Eurp + NrAm trees
  # Load info df to select plots where both species types are present in the same plot
  info <- read.csv("Ident Zusammenfassung-2_updated_8.7.2018.csv", colClasses = c("NULL", NA, rep("NULL",13), NA, NA, rep("NULL",9)))
  both_plots <- info$Plot[which(info$EU == 1 & info$USA == 1)] # create vector with plot nos. NOTE: HAS FIVE PLOTS THAT WEREN'T SAMPLED (335 338 340 343 377)
  
  # Create a df with Eurp + NrAm trees from FR
  NrAm_FR_app_both_all <- subset(ident_trees, subset = Location == "FR", select = c("Plot", "BPT", "Species", "Insects.sampled")) # Get all FR trees
  NrAm_FR_app_both <- NrAm_FR_app_both_all[which(NrAm_FR_app_both_all$Plot %in% both_plots),] # only plots with both sp present
  
  # Create a df with focus species
  NrAm_FR_app_both_focus_all <- subset(ident_insects, subset = Location == "FR" & Genus != "", select = c("Plot", "BPT", "Species")) # Select all in FR that have a genus entry -> equals select all phyllobius and polydrusus
  NrAm_FR_app_both_focus_all$P.sampled <- 1 # Add the count for the focus species
  NrAm_FR_app_both_focus <- NrAm_FR_app_both_focus_all[which(NrAm_FR_app_both_focus_all$Plot %in% both_plots),] # only plots with both sp present
  
  # Merge the dfs
  NrAm_FR_app_both_comb <- merge(NrAm_FR_app_both, NrAm_FR_app_both_focus, all =T)
  NrAm_FR_app_both_comb <- aggregate(Insects.sampled ~ Plot + Species + P.sampled, data = NrAm_FR_app_both_comb, FUN = mean)
  NrAm_FR_app_both_comb$P.sampled <- NrAm_FR_app_both_comb$P.sampled/NrAm_FR_app_both_comb$Insects.sampled # Compute the ratio between the total insect abundance an the P. abundance
  NrAm_FR_app_both_comb$Category <- as.factor(1) # Add category column
  
  # NrAm only
  # Select plots where only NrAm species are present in a plot
  single_plots <- info$Plot[which(info$EU == 0 & info$USA == 1)] # create vector with plot nos. NOTE: HAS ONE PLOT THAT WASN'T SAMPLED (275)
  # Since plot col was deleted NrAm_FR_comb df is created again
  NrAm_FR_comb2 <- merge(NrAm_FR_trees, NrAm_FR_focus, all =T)
  NrAm_FR_comb2 <- aggregate(Insects.sampled ~ Plot + Species + P.sampled, data = NrAm_FR_comb2, FUN = mean)
  NrAm_FR_comb2$P.sampled <- NrAm_FR_comb2$P.sampled/NrAm_FR_comb2$Insects.sampled # Compute the ratio between the total insect abundance an the P. abundance
  NrAm_FR_app_one_comb <- NrAm_FR_comb2[which(NrAm_FR_comb2$Plot %in% single_plots),] # only plots with NrAm sp present
  NrAm_FR_app_one_comb$Category <- as.factor(2) # change category 
  
# Linear models
  
  # Df with all three categories
  NrAm_app <- merge(NrAm_FR_app_both_comb, NrAm_FR_app_one_comb, all = T)
  NrAm_app$Tree.genus <- 0 # create Tree.genus col
  
  # Set tree genera
  #abun_sp_gen <- data.frame("Tree.sp" = sort(unique(abun_all$Species)),"Tree.genus" = c("Acer", "Betula", "Larix", "Picea", "Pinus", "Quercus")) # df with tree species and genus col
  for(i in 1:12) {  
    NrAm_app$Tree.genus[which(NrAm_app$Species == as.character(abun_sp_gen$Tree.sp[i]))] <- as.character(abun_sp_gen$Tree.genus[i])
  } # Set congeners to respecitve genus
  
  NrAm_app$Tree.genus <- as.factor(NrAm_app$Tree.genus)
  
# Linear model
  NrAm_app_fit <- lm(formula = P.sampled ~ Tree.genus * Category, data = NrAm_app)
  summary(NrAm_app_fit)
  anova(NrAm_app_fit)
  par(mfrow = c(2,2))
  plot(NrAm_app_fit)  
  
# Plot
  
  ggplot(data = NrAm_app, aes(x = as.factor(Tree.genus), y = P.sampled , fill = Category)) +
    geom_bar(stat = "identity", position = position_dodge(1)) +
    #geom_errorbar(aes(x = Genus, ymin =  Mean - SD, ymax = Mean + SD), width = 0.1, stat = "identity", position = position_dodge(1)) +
    scale_fill_discrete(name = "Category",
                        breaks = c(1, 2),
                        labels = c("NrAm + Eurp plots", "NrAm only plots")) +
    xlab("Genus") + ylab("No. of Focus sp. per total insect abundance")
  

  
# Eurp in FR
  
  # Eurp + NrAm trees
 
  
  # Create a df with Eurp + NrAm trees from FR
  Eurp_FR_app_both_all <- subset(ident_trees, subset = Location == "FR", select = c("Plot", "BPT", "Species", "Insects.sampled")) # Get all FR trees
  # Use both_plots to select plots where both species types are present in the same plot
  Eurp_FR_app_both <- Eurp_FR_app_both_all[which(Eurp_FR_app_both_all$Plot %in% both_plots),] # only plots with both sp present
  
  # Create a df with focus species
  Eurp_FR_app_both_focus_all <- subset(ident_insects, subset = Location == "FR" & Genus != "", select = c("Plot", "BPT", "Species")) # Select all in FR that have a genus entry -> equals select all phyllobius and polydrusus
  Eurp_FR_app_both_focus_all$P.sampled <- 1 # Add the count for the focus species
  Eurp_FR_app_both_focus <- Eurp_FR_app_both_focus_all[which(Eurp_FR_app_both_focus_all$Plot %in% both_plots),] # only plots with both sp present
  
  # Merge the dfs
  Eurp_FR_app_both_comb <- merge(Eurp_FR_app_both, Eurp_FR_app_both_focus, all =T)
  Eurp_FR_app_both_comb <- aggregate(Insects.sampled ~ Plot + Species + P.sampled, data = Eurp_FR_app_both_comb, FUN = mean)
  Eurp_FR_app_both_comb$P.sampled <- Eurp_FR_app_both_comb$P.sampled/Eurp_FR_app_both_comb$Insects.sampled # Compute the ratio between the total insect abundance an the P. abundance
  Eurp_FR_app_both_comb$P.sampled[is.na(Eurp_FR_app_both_comb$P.sampled)] <- 0 # Set NAs to zero
  Eurp_FR_app_both_comb$Category <- as.factor(1) # Add category column
  
  # Eurp only
  #  Use Info df to select plots where only Eurp species are present in a plot
  single_plots_1 <- info$Plot[which(info$EU == 1 & info$USA == 0)] # create vector with plot nos. NOTE: HAS SEVERAL PLOTS THAT WEREN'T SAMPLED
  # Since plot col was deleted NrAm_FR_comb df is created again
  Eurp_FR_comb2 <- merge(Eurp_FR_trees, Eurp_FR_focus, all =T)
  Eurp_FR_comb2 <- aggregate(Insects.sampled ~ Plot + Species + P.sampled, data = Eurp_FR_comb2, FUN = mean)
  Eurp_FR_comb2$P.sampled <- Eurp_FR_comb2$P.sampled/Eurp_FR_comb2$Insects.sampled # Compute the ratio between the total insect abundance an the P. abundance
  Eurp_FR_comb2$Category <- 3 # Add category column
  Eurp_FR_app_one_comb <- Eurp_FR_comb2[which(Eurp_FR_comb2$Plot %in% single_plots_1),] # only plots with NrAm sp present
  Eurp_FR_app_one_comb$Category <- as.factor(2) # change category 
  
  # Linear models
  
  # Df with all three categories
  Eurp_app <- merge(Eurp_FR_app_both_comb, Eurp_FR_app_one_comb, all = T)
  Eurp_app$Tree.genus <- 0 # create Tree.genus col
  
  # Set tree genera
  abun_sp_gen <- data.frame("Tree.sp" = sort(unique(abun_all$Species)),"Tree.genus" = c("Acer", "Betula", "Larix", "Picea", "Pinus", "Quercus")) # df with tree species and genus col
  for(i in 1:12) {  
    Eurp_app$Tree.genus[which(Eurp_app$Species == as.character(abun_sp_gen$Tree.sp[i]))] <- as.character(abun_sp_gen$Tree.genus[i])
  } # Set congeners to respecitve genus
  
# Linear model
  Eurp_app_fit <- lm(formula = P.sampled ~ Category * Tree.genus, data = Eurp_app)
  summary(Eurp_app_fit)
  anova(Eurp_app_fit)
  par(mfrow = c(2,2))
  plot(Eurp_app_fit)  
  
# Plot  

  ggplot(data = Eurp_app , aes(x = as.factor(Tree.genus), y = P.sampled , fill = Category)) +
    geom_bar(stat = "identity", position = position_dodge(1)) +
    #geom_errorbar(aes(x = Genus, ymin =  Mean - SD, ymax = Mean + SD), width = 0.1, stat = "identity", position = position_dodge(1)) +
    scale_fill_discrete(name = "Category",
                        breaks = c(1, 2),
                        labels = c("NrAm + Eurp plots", "Eurp only plots")) +
    xlab("Genus") + ylab("No. of Focus sp. per total insect abundance")

# Host shift ----
  
# Create df for chi squared test
  host_shift <- data.frame("Category" = c("NrAm in SSM", "NrAm in FR", "Eurp in FR", "Percentage NrAm in SSM", "Percentage NrAm in FR", "Percentage Eurp in FR"), "Acer" = 1, "Betula" = 1, "Larix" = 1, "Picea" = 1, "Pinus" = 1, "Quercus" = 1)
  
  # Fill columns
  sp <- sort(unique(NrAm_SSM_trees$Species))
  
  # NrAm in SSM
  bug_count_NrAm_SSM <- vector(mode = "list", 6)
  names(bug_count_NrAm_SSM) <- sp
  for(i in 1:6) {
    bug_count_NrAm_SSM[[i]] <- sum(subset(NrAm_SSM_focus, subset = Species == as.character(sp[i]), select = P.sampled))
  } # get the p.count count for every tree species 
  
  # NrAm in FR
  bug_count_NrAm_FR <- vector(mode = "list", 6)
  names(bug_count_NrAm_FR) <- sp
  bug_count_NrAm_FR[["PIGL"]] <- 0 # No weevils found on PIGL
  for(i in 1:3) {
    bug_count_NrAm_FR[[i]] <- sum(subset(NrAm_FR_focus, subset = Species == as.character(sp[i]), select = P.sampled))
  } # get the p.count count for every tree species; errors on PIGL -> can't sum w/o any value !
  bug_count_NrAm_FR[["PIST"]] <- sum(subset(NrAm_FR_focus, subset = Species == "PIST", select = P.sampled))
  bug_count_NrAm_FR[["QURU"]] <- sum(subset(NrAm_FR_focus, subset = Species == "QURU", select = P.sampled))
  
  # Eurp in FR
  bug_count_Eurp_FR <- vector(mode = "list", 6)
  names(bug_count_Eurp_FR) <- sort(unique(Eurp_FR_trees$Species))
  sp2 <- sort(unique(Eurp_FR_trees$Species))
  for(i in 1:6) {
    bug_count_Eurp_FR[[i]] <- sum(subset(Eurp_FR_focus, subset = Species == as.character(sp2[i]), select = P.sampled))
  } # get the p.count count for every tree species 
  
  
  host_shift[1,c(2:7)] <- c(bug_count_NrAm_SSM[[1]], bug_count_NrAm_SSM[[2]], bug_count_NrAm_SSM[[3]], bug_count_NrAm_SSM[[4]], bug_count_NrAm_SSM[[5]], bug_count_NrAm_SSM[[6]])
  host_shift[2,c(2:7)] <- c(bug_count_NrAm_FR[[1]], bug_count_NrAm_FR[[2]], bug_count_NrAm_FR[[3]], bug_count_NrAm_FR[[4]], bug_count_NrAm_FR[[5]], bug_count_NrAm_FR[[6]])
  host_shift[3,c(2:7)] <- c(bug_count_Eurp_FR[[1]], bug_count_Eurp_FR[[2]], bug_count_Eurp_FR[[3]], bug_count_Eurp_FR[[4]], bug_count_Eurp_FR[[5]], bug_count_Eurp_FR[[6]])

  # Percentage per Location
  
  # NrAm in SSM
  p_NrAm_SSM <- vector(mode="list", 6)
  names(p_NrAm_SSM) <- sp
  for (i in 1:6) {
    p_NrAm_SSM[[i]] <- sum(subset(NrAm_SSM_focus, subset = Species == as.character(sp[i]), select = P.sampled))/(sum(subset(NrAm_SSM_comb, subset = Species == as.character(sp[i]), select = Insects.sampled)) - length(NrAm_SSM_focus$P.sampled[which(NrAm_SSM_focus$Species == as.character(sp[i]))]))
  }
  
  # NrAm in FR
  p_NrAm_FR <- vector(mode="list", 6)
  names(p_NrAm_FR) <- sp
  p_NrAm_FR[["PIGL"]] <- 0
  for (i in 1:3) {
    p_NrAm_FR[[i]] <- sum(subset(NrAm_FR_focus, subset = Species == as.character(sp[i]), select = P.sampled))/(sum(subset(NrAm_FR_comb, subset = Species == as.character(sp[i]), select = Insects.sampled)) - length(NrAm_FR_focus$P.sampled[which(NrAm_FR_focus$Species == as.character(sp[i]))]))
  }
  p_NrAm_FR[["PIST"]] <- sum(subset(NrAm_FR_focus, subset = Species == "PIST", select = P.sampled))/(sum(subset(NrAm_FR_comb, subset = Species == "PIST", select = Insects.sampled)) - length(NrAm_FR_focus$P.sampled[which(NrAm_FR_focus$Species == "PIST")]))
  p_NrAm_FR[["QURU"]] <- sum(subset(NrAm_FR_focus, subset = Species == "QURU", select = P.sampled))/(sum(subset(NrAm_FR_comb, subset = Species == "QURU", select = Insects.sampled)) - length(NrAm_FR_focus$P.sampled[which(NrAm_FR_focus$Species == "QURU")]))
  
  # Eurp in FR
  p_Eurp_FR <- vector(mode="list", 6)
  names(p_Eurp_FR) <- sp2
  for (i in 1:6) {
    p_Eurp_FR[[i]] <- sum(subset(Eurp_FR_focus, subset = Species == as.character(sp2[i]), select = P.sampled))/(sum(subset(Eurp_FR_comb, subset = Species == as.character(sp2[i]), select = Insects.sampled)) - length(Eurp_FR_focus$P.sampled[which(Eurp_FR_focus$Species == as.character(sp2[i]))]))
  }
  
  host_shift[4,c(2:7)] <- c(p_NrAm_SSM[[1]], p_NrAm_SSM[[2]], p_NrAm_SSM[[3]], p_NrAm_SSM[[4]], p_NrAm_SSM[[5]], p_NrAm_SSM[[6]])
  host_shift[5,c(2:7)] <- c(p_NrAm_FR[[1]], p_NrAm_FR[[2]], p_NrAm_FR[[3]], p_NrAm_FR[[4]], p_NrAm_FR[[5]], p_NrAm_FR[[6]])
  host_shift[6,c(2:7)] <- c(p_Eurp_FR[[1]], p_Eurp_FR[[2]], p_Eurp_FR[[3]], p_Eurp_FR[[4]], p_Eurp_FR[[5]], p_Eurp_FR[[6]])
  
# Chi squared test
  
  chisq.test(host_shift[c(2:4),c(2:7)]) # How should this be interpreted ??!
  chisq.test(host_shift[c(2:4),c(2:7)])$expected # Test not valid due to expected values < 5
  # betula und quercus wichtige hosts in fr; rest wichtiger in ssm
  # Round to integer ??
  fisher.test(host_shift[c(2:4),c(2:7)])
  