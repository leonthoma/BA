# Analysis question three
setwd("~/Dokumente/Uni/Umwi/B.Sc. Thesis/IDENT/Analysis")
source(file = "data_formatting.R", local = T)
library(ggplot2)
library(gridExtra)
library(ggpubr)

# # Abbreviations:----
# Eurp: european sp.
# NrAm: northamerican sp.
# FR: Freiburg site
# SSM: Sault. Ste. Marie site

# UPDATE LOG :
# ?? KICKED OUT SR 1
# 12.09 fixed subsetting to include SR 1 plots
# 14.09 log+1 transformation for abun and dmg
#       added associational effect analysis
#       realized that there can't be SR 1 plots on plots that have NrAm AND Eurp sp. !!! -> Changed Cat 3 subsetting back to original
#       added shared legend for plots
# 25.09 error in abun lm due to singularities 


# Insect abundance ----

# NrAm in SSM
  
  # Create df with all NrAM trees in SSM
  NrAm_ssm_trees <- subset(ident_trees, subset = Location == "SSM" & Species.type == "NrAm", select = c(BPT, Consecutive.Plot, Species, Insects.sampled, SR.Level)) # All trees including predators
  
  # Excluding predators
  # Include predators ?! # add _all to above df 
  #NrAm_ssm_preds <- unique(subset(ident_insects, subset = Functional.Group == "Predator" & Location == "SSM" & Species.type == "NrAm", select = c(BPT))) # BPT of trees with predators
  #NrAm_ssm_trees <- NrAm_ssm_trees_all[-which(NrAm_ssm_trees_all$BPT %in% NrAm_ssm_preds$BPT),] # only trees that had no predators
  
  # Df with aggregated abundance containing all six tree species
  NrAm_ssm_agg_abun_data <- aggregate(Insects.sampled ~ Consecutive.Plot + SR.Level + Species, data = NrAm_ssm_trees, FUN = mean) 

  # Delete all SR = 4 plots because they were not sampled in FR
  NrAm_ssm_agg_abun_data <- NrAm_ssm_agg_abun_data[-which(NrAm_ssm_agg_abun_data$SR.Level == 4),]
  NrAm_ssm_agg_abun_data$Category <- 1 # Add category column
  names(NrAm_ssm_agg_abun_data) <- c("Plot", "SR.Level", "Species", "Insects.sampled", "Category")

# NrAm in FR
  
  # Create df with all NrAM trees in FR
  NrAm_fr_trees_all <- subset(ident_trees, subset = Location == "FR", select = c(BPT, Plot, Species, Insects.sampled, SR.Level)) # All trees including predators
  
  # Select plots where only NrAm species are present in a plot
  info <- read.csv("Ident Zusammenfassung-2_updated_8.7.2018.csv", colClasses = c("NULL", NA, rep("NULL",13), NA, NA, rep("NULL",9)))
  single_plots <- info$Plot[which(info$EU == 0 & info$USA == 1)]
  NrAm_fr_trees <- NrAm_fr_trees_all[which(NrAm_fr_trees_all$Plot %in% single_plots),]
  
  # Excluding predators
  # Include predators ? # add _all to above df
  #NrAm_fr_preds <- unique(subset(ident_insects, subset = Functional.Group == "Predator" & Location == "FR" & Species.type == "NrAm", select = c(BPT))) # BPT of trees with predators
  #NrAm_fr_trees <- NrAm_fr_trees_all[-which(NrAm_fr_trees_all$BPT %in% NrAm_fr_preds$BPT),] # only trees that had no predators
  
  # Df with aggregated abundance containing all six tree species 
  NrAm_fr_agg_abun_data <- aggregate(Insects.sampled ~ Plot + SR.Level + Species, data = NrAm_fr_trees, FUN = mean) 
  NrAm_fr_agg_abun_data$Category <- 2 # Add category column
  
# NrAm + Eurp in FR  
  
  # Create df with all NrAM and Eurp trees in FR
  comb_fr_trees_all <- subset(ident_trees, subset = Location == "FR" & Species.type == c("NrAm", "Eurp"), select = c(BPT, Plot, Species, Insects.sampled, SR.Level)) # All trees including predators
  
  # Use info df to select plots where both species types are present in the same plot
  both_plots <- info$Plot[which(info$EU == 1 & info$USA == 1)] # create vector with plot nos.
  comb_fr_trees <- comb_fr_trees_all[which(comb_fr_trees_all$Plot %in% both_plots), ] 
  
  # Excluding predators
  # Include predators ?! # add _all to above df
  #comb_fr_preds <- unique(subset(ident_insects, subset = Functional.Group == "Predator" & Location == "FR", select = c(BPT))) # BPT of trees with predators
  #comb_fr_trees <- comb_fr_trees_all[-which(comb_fr_trees_all$BPT %in% comb_fr_preds$BPT),] # only trees that had no predators
  
  # Df with aggregated abundance containing all 12 tree species
  comb_fr_agg_abun_data <- aggregate(Insects.sampled ~ Plot + SR.Level + Species, data = comb_fr_trees, FUN = mean) 
  comb_fr_agg_abun_data$Category <- 3 # Add category column
  
# Leaf damage ----
  
# NrAm in SSM  
  
  # Create df with all NrAm trees in SSM
  NrAm_ssm_damage <- subset(ident_damage, subset = Location == "SSM" & Species.type == "NrAm", select = c(Consecutive.Plot, Species, SR.Level, BPT, Total.damage.per.leaf))
  
  # Use NrAm_ssm_preds to exclude predators
  # Include predators ?! # add _all to above df
  #NrAm_ssm_damage <- NrAm_ssm_damage_all[-which(NrAm_ssm_damage_all$BPT %in% NrAm_ssm_preds$BPT),] # only trees that had no predators
  
  # Aggregate the Total damage per Tree
  NrAm_ssm_agg_damage <- aggregate(x = list(Total.damage.per.tree = NrAm_ssm_damage$Total.damage.per.leaf), by = subset(NrAm_ssm_damage, select = c(BPT, Consecutive.Plot, Species, SR.Level)), FUN = mean)
  
  # Df with mean damage per plot containing all six tree species
  NrAm_ssm_agg_dam_data <- aggregate(Total.damage.per.tree ~ Consecutive.Plot + SR.Level + Species, data = NrAm_ssm_agg_damage, FUN = mean)
  
  # Delete all SR = 4 plots because they were not sampled in FR
  NrAm_ssm_agg_dam_data <- NrAm_ssm_agg_dam_data[-which(NrAm_ssm_agg_dam_data$SR.Level == 4),]
  NrAm_ssm_agg_dam_data$Category <- 1 # Add category column
  names(NrAm_ssm_agg_dam_data) <- c("Plot", "SR.Level", "Species", "Mean.damage.per.plot", "Category")
  
# NrAm in FR
  
  # Create df with all NrAM trees in FR
  NrAm_fr_damage_all <- subset(ident_damage, subset = Location == "FR", select = c(BPT, Plot, Species, Total.damage.per.leaf, SR.Level)) # All damage including predators
  
  # Use single_plots to select plots where only NrAm species are present in a plot
  NrAm_fr_damage <- NrAm_fr_damage_all[which(NrAm_fr_damage_all$Plot %in% single_plots),]
  
  # Use NrAm_fr_preds to exclude predators
  # Include predators ?  # add _all to above df
  #NrAm_fr_damage <- NrAm_fr_damage_all[-which(NrAm_fr_damage_all$BPT %in% NrAm_fr_preds$BPT),] # only damage that had no predators
  
  # Aggregate the Total damage per Tree
  NrAm_fr_agg_damage <- aggregate(x = list(Total.damage.per.tree = NrAm_fr_damage$Total.damage.per.leaf), by = subset(NrAm_fr_damage, select = c(BPT, Plot, Species, SR.Level)), FUN = mean)
  
  # Df with mean damage per plot containing all six tree species
  NrAm_fr_agg_dam_data <- aggregate(Total.damage.per.tree ~ Plot + SR.Level + Species, data = NrAm_fr_agg_damage, FUN = mean)
  NrAm_fr_agg_dam_data$Category <- 2 # Add category column
  names(NrAm_fr_agg_dam_data) <- c("Plot", "SR.Level", "Species", "Mean.damage.per.plot", "Category")
    
# NrAm + Eurp in FR

  # Create df with all comb trees in FR
  comb_fr_damage_all <- subset(ident_damage, subset = Location == "FR", select = c(BPT, Plot, Species, Total.damage.per.leaf, SR.Level)) # All damage including predators
  
  # Use both_plots to select plots where both species types are present in the same plot
  both_plots <- info$Plot[which(info$EU == 1 & info$USA == 1)] # create vector with plot nos.
  comb_fr_damage <- comb_fr_damage_all[which(comb_fr_damage_all$Plot %in% both_plots), ]
  
  # Use comb_fr_preds to exclude predators
  # Include predators ? # add _all to above df
  #comb_fr_damage <- comb_fr_damage_all[-which(comb_fr_damage_all$BPT %in% comb_fr_preds$BPT),] # only damage that had no predators
  
  # Aggregate the Total damage per Tree
  comb_fr_agg_damage <- aggregate(x = list(Total.damage.per.tree = comb_fr_damage$Total.damage.per.leaf), by = subset(comb_fr_damage, select = c(BPT, Plot, Species, SR.Level)), FUN = mean)
  
  # Df with mean damage per plot containing all six tree species
  comb_fr_agg_dam_data <- aggregate(Total.damage.per.tree ~ Plot + SR.Level + Species, data = comb_fr_agg_damage, FUN = mean)
  comb_fr_agg_dam_data$Category <- 3 # Add category column
  names(comb_fr_agg_dam_data) <- c("Plot", "SR.Level", "Species", "Mean.damage.per.plot", "Category")

# No of functional groups ----
  
# NrAm in SSM
  
  # Create df with all NrAm trees in SSM excluding predators
  NrAm_ssm_fg <- subset(ident_insects, subset = Location == "SSM" & Species.type == "NrAm", select = c(BPT, Consecutive.Plot, Species, Functional.Group, SR.Level))
 
  #Exclude predators ? if yes add _all to above df
  #NrAm_SSM_fg <- NrAm_SSM_fg_all[-which(NrAm_SSM_fg_all$BPT %in% ssm_preds$BPT),] # only trees that had no predators
  
  NrAm_ssm_agg_fg_data <- aggregate(Functional.Group ~ Consecutive.Plot + SR.Level + Species, data = NrAm_ssm_fg, FUN = function(x) {
    length(unique(x))
  }) 
  
  # Delete all SR = 4 plots because they were not sampled in FR
  NrAm_ssm_agg_fg_data <- NrAm_ssm_agg_fg_data[-which(NrAm_ssm_agg_fg_data$SR.Level == 4),]
  NrAm_ssm_agg_fg_data$SR.Level <- as.factor(NrAm_ssm_agg_fg_data$SR.Level)
  NrAm_ssm_agg_fg_data$Category <- 1
  names(NrAm_ssm_agg_fg_data) <- c("Plot", "SR.Level", "Species", "Functional.Group", "Category")

# NrAM in FR  
  
  # Create df with all NrAm trees in SSM excluding predators
  NrAm_fr_fg_all <- subset(ident_insects, subset = Location == "FR", select = c(BPT, Plot, Species, Functional.Group, SR.Level))
  
  # Use single_plots to select plots where only NrAm species are present in a plot
  NrAm_fr_fg <- NrAm_fr_fg_all[which(NrAm_fr_damage_all$Plot %in% single_plots),]
  
  #Exclude predators ? if yes add _all to above df
  #NrAm_SSM_fg <- NrAm_SSM_fg_all[-which(NrAm_SSM_fg_all$BPT %in% ssm_preds$BPT),] # only trees that had no predators
  
  NrAm_fr_agg_fg_data <- aggregate(Functional.Group ~ Plot + SR.Level + Species, data = NrAm_fr_fg, FUN = function(x) {
    length(unique(x))
  }) 
  NrAm_fr_agg_fg_data$SR.Level <- as.factor(NrAm_fr_agg_fg_data$SR.Level)
  NrAm_fr_agg_fg_data$Category <- 2
  
# NrAm + Eurp in FR    
  
  # Create df with all comb trees in FR excluding predators
  comb_fr_fg_all <- subset(ident_insects, subset = Location == "FR", select = c(BPT, Plot, Species, Functional.Group, SR.Level))
  
  # Use both_plots to select plots where both species types are present in the same plot, but keep the SR = 1 plots !!
  both_plots <- info$Plot[which(info$EU == 1 & info$USA == 1)] # create vector with plot nos.
  comb_fr_fg <- comb_fr_fg_all[which(comb_fr_fg_all$Plot %in% both_plots), ]
  
  #Exclude predators ? if yes add _all to above df
  #NrAm_SSM_fg <- NrAm_SSM_fg_all[-which(NrAm_SSM_fg_all$BPT %in% ssm_preds$BPT),] # only trees that had no predators
  
  comb_fr_agg_fg_data <- aggregate(Functional.Group ~ Plot + SR.Level + Species, data = comb_fr_fg, FUN = function(x) {
    length(unique(x))
  }) 
  comb_fr_agg_fg_data$SR.Level <- as.factor(comb_fr_agg_fg_data$SR.Level)
  comb_fr_agg_fg_data$Category <- 3
     
# Linear models ----
  
# Insect abundance
  
  # Df with all three categories
  abun_all <- merge(merge(NrAm_ssm_agg_abun_data, NrAm_fr_agg_abun_data, all = T), comb_fr_agg_abun_data, all = T)
  abun_all$Category <- as.factor(abun_all$Category)
  abun_all$SR.Level <- as.factor(abun_all$SR.Level)
  
  
  # Linear model
  
  # Check predictor interactions
  interaction.plot(x.factor = abun_all$Category, trace.factor = abun_all$SR.Level, response = abun_all$Insects.sampled)
  # Interactions occur -> check anova to see if the effect SR.Level:Category is significant
  abun_test <- lm(log(Insects.sampled + 1) ~ SR.Level * Category, abun_all)
  anova(abun_test) 
  
  lm_list_abun <- vector(mode = "list", 6) # create "list" for fitted models
  NrAm_sp <- unique(NrAm_fr_trees$Species) # vector with NrAm tree spe names
  names(lm_list_abun) <- NrAm_sp # set species names for result list
  for(i in 1:6) {
    lm_list_abun[[i]] <- lm(formula = log(Insects.sampled + 1) ~ SR.Level * Category, data = abun_all[which(abun_all$Species == as.character(NrAm_sp[i])),])
  } # Compute models for each tree species and write results in glm_list
  
  # Summaries # ERROR: SINGULARITY ??
  # ACSA
  summary(lm_list_abun$ACSA) # SR 1 + SR6:cat3 significant effect 
  # F value is not significant ! -> INVALID REGRESSION 
  anova(lm_list_abun$ACSA)
  
  # BEPA
  summary(lm_list_abun$BEPA) # SR1 has a significant effect
  # F value is not significant ! -> INVALID REGRESSION
  anova(lm_list_abun$BEPA)
  
  # LALA
  summary(lm_list_abun$LALA) # SR1 & SR6 have a significant effect
  # F value is not significant ! -> INVALID REGRESSION
  anova(lm_list_abun$LALA)
  
  # PIGL
  summary(lm_list_abun$PIGL) # SR 1 & cat3 have significant effects
  # F value is not significant ! -> INVALID REGRESSION
  anova(lm_list_abun$PIGL)
  
  # PIST
  summary(lm_list_abun$PIST) # Sr 1 has a significant effect
  # F values is not significant ! -> INVALID REGRESSION
  anova(lm_list_abun$PIST)
  
  # QURU
  summary(lm_list_abun$QURU) # SR1 has a significant effect
  # F values is not significant ! -> INVALID REGRESSION
  anova(lm_list_abun$QURU)
  
# Leaf damage 
  
  # Df with all three categories
  dam_all <- merge(merge(NrAm_ssm_agg_dam_data, NrAm_fr_agg_dam_data, all = T), comb_fr_agg_dam_data, all = T)
  dam_all$Category <- as.factor(dam_all$Category)
  dam_all$SR.Level <- as.factor(dam_all$SR.Level)
  
  # Check predictor interactions
  interaction.plot(x.factor = dam_all$Category, trace.factor = dam_all$SR.Level, response = dam_all$Mean.damage.per.plot)
  # Interactions occur -> check anova to see if the effect SR.Level:Category is significant
  dam_test <- lm(log(Mean.damage.per.plot + 1) ~ SR.Level * Category, dam_all)
  anova(dam_test) 
  
  # Linear model
  
  # With independent predictors
  
  # Linear model
  lm_list_dam <- vector(mode = "list", 6) # create "list" for fitted models
  NrAm_sp <- unique(NrAm_fr_trees$Species) # vector with NrAm tree spe names
  names(lm_list_dam) <- NrAm_sp # set species names for result list
  for(i in 1:6) {
    lm_list_dam[[i]] <- lm(formula = log(Mean.damage.per.plot + 1) ~ SR.Level * Category, data = dam_all[which(dam_all$Species == as.character(NrAm_sp[i])),])
  }
  
  # Summaries
  # ACSA
  summary(lm_list_dam$ACSA) # SR 1 & SR2:cat2 have a significant effect
  # F value is significant ! -> VALID REGRESSION
  anova(lm_list_dam$ACSA)
  
  # BEPA
  summary(lm_list_dam$BEPA) # SR 1 & SR6:cat2 & SR6:cat3 have a significant effect
  # F values is significant ! -> VALID REGRESSION
  anova(lm_list_dam$BEPA)
  
  # LALA
  summary(lm_list_dam$LALA) # No significant effect
  # F values is not significant ! -> INVALID REGRESSION
  anova(lm_list_dam$LALA)
  
  # PIGL
  summary(lm_list_dam$PIGL) # No significant effect
  # F values is not significant ! -> INVALID REGRESSION
  anova(lm_list_dam$PIGL)
  
  # PIST
  summary(lm_list_dam$PIST) # No significant effect
  # F values is not significant ! -> INVALID REGRESSION
  anova(lm_list_dam$PIST)
  
  # QURU
  summary(lm_list_dam$QURU) # SR 1 has a significant effects
  # F values is not significant ! -> INVALID REGRESSION
  anova(lm_list_dam$QURU)
  
# No of functional groups  
  
  # Df with all three categories
  fg_all <- merge(merge(NrAm_ssm_agg_fg_data, NrAm_fr_agg_fg_data, all = T), comb_fr_agg_fg_data, all = T)
  fg_all$Category <- as.factor(fg_all$Category)
  fg_all$SR.Level <- as.factor(fg_all$SR.Level)
  
  # Check predictor interactions
  interaction.plot(x.factor = fg_all$Category, trace.factor = fg_all$SR.Level, response = fg_all$Functional.Group)
  # Interactions occur -> check anova to see if the effect SR.Level:Category is significant
  fg_test <- lm(Functional.Group ~ SR.Level * Category, fg_all)
  anova(fg_test) 
  
  # Linear model
  lm_list_fg <- vector(mode = "list", 6) # create "list" for fitted models
  NrAm_sp <- unique(NrAm_fr_trees$Species) # vector with NrAm tree spe names
  names(lm_list_fg) <- NrAm_sp # set species names for result list
  # Log transformation ?! -> creates negative values !
  for(i in 1:6) {
    lm_list_fg[[i]] <- lm(formula = Functional.Group ~ SR.Level * Category, data = fg_all[which(fg_all$Species == as.character(NrAm_sp[i])),])
  }
  
  # Summaries
  # ACSA
  summary(lm_list_fg$ACSA) # Only SR 1 has a significant effect
  # F values is not significant ! -> INVALID REGRESSION
  anova(lm_list_fg$ACSA)
  
  # BEPA
  summary(lm_list_fg$BEPA) # Only SR 1 has a significant effect
  # F values is not significant ! -> INVALID REGRESSION
  anova(lm_list_fg$BEPA)
  
  # LALA
  summary(lm_list_fg$LALA) # SR 1 and SR2:CAT3 have a significant effect
  # F values is significant ! -> VALID REGRESSION
  anova(lm_list_fg$LALA)
  
  # PIGL
  summary(lm_list_fg$PIGL) # SR 1, SR 2 and Sr 6 have a significant effect
  # F values is significant ! -> VALID REGRESSION
  anova(lm_list_fg$PIGL)
  
  # PIST
  summary(lm_list_fg$PIST) # SR 1 has a significant effect
  # F values is significant ! -> VALID REGRESSION
  anova(lm_list_fg$PIST)
  
  # QURU
  summary(lm_list_fg$QURU) # SR 1 has a significant effect
  # F values is significant ! -> VALID REGRESSION
  anova(lm_list_fg$QURU)
  
# Plots ----
  
# Insect abundance
  sp <- unique(NrAm_ssm_agg_abun_data$Species) # vector with tree species
  abun_plots <- list() # create a list for all the plots
  for(i in 1:6) {
    abun_plots[[i]] <- ggplot(data = subset(abun_all, subset = Species == as.character(sp[i]), select = c("SR.Level", "Insects.sampled", "Category")), aes(x = as.factor(SR.Level), y = log(Insects.sampled + 1), fill = Category)) +
      geom_bar(stat = "identity", position = "dodge") +
      #geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.1, stat = "identity", position = position_dodge(1)) +
      ggtitle(as.character(sp[i])) +
      scale_fill_discrete(name = "Category",
                          breaks = c(1, 2, 3),
                          labels = c("NrAm in SSM", "NrAm only plots in FR", "NrAm + Eurp plots in FR")) +
      xlab("SR-Level") + ylab("Insect abundance")
  } # generate the plots for every NrAm species
  
  # WOULD BE NICER IF EACH SR-LEVEL GROUP WOULD BE A LITTLE BIT FURTHER APART
  ggarrange(abun_plots[[1]],abun_plots[[2]],abun_plots[[3]],abun_plots[[4]],abun_plots[[5]],abun_plots[[6]], ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom") # plot of all species together
  
# Leaf damage
    
  sp <- unique(NrAm_ssm_agg_dam_data$Species) # vector with tree species
  dam_plots <- list() # create a list for all the plots
  for(i in 1:6) {
    dam_plots[[i]] <- ggplot(data = subset(dam_all, subset = Species == as.character(sp[i]), select = c("SR.Level", "Mean.damage.per.plot", "Category")), aes(x = as.factor(SR.Level), y = log(Mean.damage.per.plot + 1), fill = Category)) +
      geom_bar(stat = "identity", position = "dodge") +
      #geom_errorbar(aes(ymin =  - SD, ymax = Mean + SD), width = 0.1, stat = "identity", position = position_dodge(1)) +
      ggtitle(as.character(sp[i])) +
      scale_fill_discrete(name = "Category",
                          breaks = c(1, 2, 3),
                          labels = c("NrAm in SSM", "NrAm only plots in FR", "NrAm + Eurp plots in FR")) +
      xlab("SR-Level") + ylab("Mean.damage.per.plot")
  } # generate the plots for every NrAm species
  
  # WOULD BE NICER IF EACH SR-LEVEL GROUP WOULD BE A LITTLE BIT FURTHER APART
  ggarrange(dam_plots[[1]],dam_plots[[2]],dam_plots[[3]],dam_plots[[4]],dam_plots[[5]],dam_plots[[6]], ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom") # plot of all species together
  
# No of functional groups
  sp <- unique(NrAm_ssm_agg_abun_data$Species) # vector with tree species
  fg_plots <- list() # create a list for all the plots
  for(i in 1:6) {
    fg_plots[[i]] <- ggplot(data = subset(fg_all, subset = Species == as.character(sp[i]), select = c("SR.Level", "Functional.Group", "Category")), aes(x = as.factor(SR.Level), y = Functional.Group, fill = Category)) +
      geom_bar(stat = "identity", position = position_dodge(1)) +
      #geom_errorbar(aes(ymin =  - SD, ymax = Mean + SD), width = 0.1, stat = "identity", position = position_dodge(1)) +
      ggtitle(as.character(sp[i])) +
      scale_fill_discrete(name = "Category",
                          breaks = c(1, 2, 3),
                          labels = c("NrAm in SSM", "NrAm only plots in FR", "NrAm + Eurp plots in FR")) +
      xlab("SR-Level") + ylab("No of functional groups")
  } # generate the plots for every NrAm species
  
  # WOULD BE NICER IF EACH SR-LEVEL GROUP WOULD BE A LITTLE BIT FURTHER APART
  ggarrange(fg_plots[[1]],fg_plots[[2]],fg_plots[[3]],fg_plots[[4]],fg_plots[[5]],fg_plots[[6]], ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom") # plot of all species together
  
# Associational effects ----
  sp <- unique(NrAm_ssm_agg_abun_data$Species)
  
  # SSM NrAM tree damage
  NrAm_ssm_dmg_per_sp <- data.frame("Species" = sp, "Mean Damage" = c(1:6)) # df with mean damage per tree species
  for(i in 1:6) {
    NrAm_ssm_dmg_per_sp$Mean.Damage[i] <- mean(NrAm_ssm_agg_dam_data$Mean.damage.per.plot[which(NrAm_ssm_agg_dam_data$Species == as.character(sp[i]))])
  } # calcualate sum and write it into df
  
  # FR NrAM tree damage
  NrAm_fr_dmg_per_sp <- data.frame("Species" = sp, "Mean Damage" = c(1:6)) # df with mean damage per tree species
  for(i in 1:6) {
    NrAm_fr_dmg_per_sp$Mean.Damage[i] <- mean(NrAm_fr_agg_dam_data$Mean.damage.per.plot[which(NrAm_fr_agg_dam_data$Species == as.character(sp[i]))])
  } # calcualate sum and write it into df
  
  # FR NrAM + Eurp tree damage
  
  both_fr_dmg_per_sp <- data.frame("Species" = sp, "Mean Damage" = c(1:6)) # df with mean damage per tree species
  for(i in 1:6) {
    both_fr_dmg_per_sp$Mean.Damage[i] <- mean(comb_fr_agg_dam_data$Mean.damage.per.plot[which(comb_fr_agg_dam_data$Species == as.character(sp[i]))])
  } # calcualate sum and write it into df
  
  As_eff <- data.frame("Species" = sp, "NrAm/SSM" = NrAm_ssm_dmg_per_sp$Mean.Damage, "NrAm/FR" = NrAm_fr_dmg_per_sp$Mean.Damage, "Both/FR" = both_fr_dmg_per_sp$Mean.Damage)
  # Just compare the mean damage or lm neccessary ?
   
    # # Herbivore/Predator ratio ----
  #   
  # # NrAm in SSM
  #   
  #   # Herbivores
  #   NrAm_ssm_herb <- unique(subset(ident_insects, subset = Functional.Group == "Herbivore" & Location == "SSM" & Species.type == "NrAm", select = c(BPT))) # BPT of trees with predators
  #   NrAm_ssm_herb_count <- NrAm_ssm_trees_all[match(NrAm_ssm_herb$BPT, NrAm_ssm_trees$BPT), c("Species", "Insects.sampled")]  # only trees that had herbivores
  #   NrAm_ssm_herb_per_sp <- data.frame("Species" = sp, "Total Herbivores" = c(1:6)) # df with Sum of Herbivores per tree species
  #   # sp <- unique(agg_abun_data$Species) # vector with tree species
  #   for(i in 1:6) {
  #     NrAm_ssm_herb_per_sp$Total.Herbivores[i] <- sum(NrAm_ssm_herb_count$Insects.sampled[which(NrAm_ssm_herb_count$Species == as.character(sp[i]))])
  #   } # calcualate sum and write it into df
  #   
  #   # Predators
  #   NrAm_ssm_pred <- unique(subset(ident_insects, subset = Functional.Group == "Predator" & Location == "SSM" & Species.type == "NrAm", select = c(BPT))) # BPT of trees with predators
  #   NrAm_ssm_pred_count <- NrAm_ssm_trees_all[match(NrAm_ssm_pred$BPT, NrAm_ssm_trees_all$BPT),c("Species", "Insects.sampled")] # only trees that had Predators
#   NrAm_ssm_pred_per_sp <- data.frame("Species" = sp, "Total Predators" = c(1:6)) # df with Sum of predators per tree species
#   # sp <- unique(agg_abun_data$Species) # vector with tree species
#   for(i in 1:6) {
#     NrAm_ssm_pred_per_sp$Total.Predators[i] <- sum(NrAm_ssm_pred_count$Insects.sampled[which(NrAm_ssm_pred_count$Species == as.character(sp[i]))])
#   } # calcualate sum and write it into df
#   
#   # Ratio df
#   NrAm_ssm_herb_pred_ratio <- data.frame("Species" = sp, "Total Herbivores" = NrAm_ssm_herb_per_sp$Total.Herbivores, "Total Predators" = NrAm_ssm_pred_per_sp$Total.Predators, "Ratio" = NrAm_ssm_herb_per_sp$Total.Herbivores/NrAm_ssm_pred_per_sp$Total.Predators)
#   # Ratio of Herbivores per Predator
#   
# # NrAm in FR
#   
#   # Herbivores
#   NrAm_fr_herb <- unique(subset(ident_insects, subset = Functional.Group == "Herbivore" & Location == "FR", select = c(BPT, Plot))) # BPT of trees with predators
#   # Use single_plots to select plots where only NrAm species are present in a plot
#   NrAm_fr_herb <- NrAm_fr_herb[which(NrAm_fr_herb$Plot %in% single_plots),]
#   NrAm_fr_herb_count <- NrAm_fr_trees_all[match(NrAm_fr_herb$BPT, NrAm_fr_trees_all$BPT), c("Species", "Insects.sampled")]  # only trees that had herbivores
#   NrAm_fr_herb_per_sp <- data.frame("Species" = sp, "Total Herbivores" = c(1:6)) # df with Sum of Herbivores per tree species
#   # sp <- unique(agg_abun_data$Species) # vector with tree species
#   for(i in 1:6) {
#     NrAm_fr_herb_per_sp$Total.Herbivores[i] <- sum(NrAm_fr_herb_count$Insects.sampled[which(NrAm_fr_herb_count$Species == as.character(sp[i]))])
#   } # calculate sum and write it into df
#   
#   # Predators
#   NrAm_fr_pred <- unique(subset(ident_insects, subset = Functional.Group == "Predator" & Location == "FR" , select = c(BPT, Plot))) # BPT of trees with predators
#   # Use single_plots to select plots where only NrAm species are present in a plot
#   NrAm_fr_pred <- NrAm_fr_pred[which(NrAm_fr_pred$Plot %in% single_plots),]
#   NrAm_fr_pred_count <- NrAm_fr_trees_all[match(NrAm_fr_pred$BPT, NrAm_fr_trees_all$BPT),c("Species", "Insects.sampled")] # only trees that had Predators
#   NrAm_fr_pred_per_sp <- data.frame("Species" = sp, "Total Predators" = c(1:6)) # df with Sum of predators per tree species
#   # sp <- unique(agg_abun_data$Species) # vector with tree species
#   for(i in 1:6) {
#     NrAm_fr_pred_per_sp$Total.Predators[i] <- sum(NrAm_fr_pred_count$Insects.sampled[which(NrAm_fr_pred_count$Species == as.character(sp[i]))])
#   } # calculate sum and write it into df
#   
#   # Ratio df
#   NrAm_fr_herb_pred_ratio <- data.frame("Species" = sp, "Total Herbivores" = NrAm_fr_herb_per_sp$Total.Herbivores, "Total Predators" = NrAm_fr_pred_per_sp$Total.Predators, "Ratio" = NrAm_fr_herb_per_sp$Total.Herbivores/NrAm_fr_pred_per_sp$Total.Predators)
#   # Ratio of Herbivores per Predator
#   
#   