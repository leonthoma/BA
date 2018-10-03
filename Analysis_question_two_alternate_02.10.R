# Analysis Question two
setwd("~/Dokumente/Uni/Umwi/B.Sc. Thesis/IDENT/Analysis")
source(file = "data_formatting.R", local = T)
library(ggplot2)
library(ggpubr)

# # Abbreviations:----
# Eurp: european sp.
# NrAm: northamerican sp.
# FR: Freiburg site
# SSM: Sault. Ste. Marie site

# UPDATE LOG :
# 14.09 changed abun and dmg model to log+1 transformation
#       added shared legends for plots (not needed actually)
# 28.09 added regression line to plots
#       code for different colour of regression line (sig./not sig.) added (line 238)
# 02.10 deleted pred/host ratio

# Abundances per SR.Level ----

  # Create df with all NrAM trees in SSM excluding predators
  ssm_trees_all <- subset(ident_trees, subset = Location == "SSM" & Species.type == "NrAm", select = c(BPT, Consecutive.Plot, Species, Insects.sampled, SR.Level)) # All trees including predators
  ssm_preds <- unique(subset(ident_insects, subset = Functional.Group == "Predator" & Location == "SSM" & Species.type == "NrAm", select = c(BPT))) # BPT of trees with predators
  ssm_trees <- ssm_trees_all[-which(ssm_trees_all$BPT %in% ssm_preds$BPT),] # only trees that had no predators

  # Df with aggregated abundance containing all six tree species
  agg_abun_data <- aggregate(Insects.sampled ~ Consecutive.Plot + SR.Level + Species, data = ssm_trees, FUN = mean) 
  agg_abun_data$SR.Level <- as.factor(agg_abun_data$SR.Level)

# Leaf damage per SR.Level ----

  # Create df with all NrAm trees in SSM excluding predators
  ssm_damage_all <- subset(ident_damage, subset = Location == "SSM" & Species.type == "NrAm", select = c(Consecutive.Plot, Species, SR.Level, BPT, Total.damage.per.leaf))
  ssm_preds <- unique(subset(ident_insects, subset = Functional.Group == "Predator" & Location == "SSM" & Species.type == "NrAm", select = c(BPT))) # BPT of trees with predators
  ssm_damage <- ssm_damage_all[-which(ssm_damage_all$BPT %in% ssm_preds$BPT),] # only trees that had no predators
  
  # Aggregate the Total damage per Tree
  agg_dam <- aggregate(x = list(Total.damage.per.tree = ssm_damage$Total.damage.per.leaf), by = subset(ssm_damage, select = c(BPT, Consecutive.Plot, Species, SR.Level)), FUN = mean)

  # Df with Mean damage per plot containing all six tree species
  agg_dam_data <- aggregate(Total.damage.per.tree ~ Consecutive.Plot + SR.Level + Species, data = agg_dam, FUN = mean)
  names(agg_dam_data) <- c("Consecutive.Plot", "SR.Level", "Species", "Mean.damage.per.plot")
  agg_dam_data$SR.Level <- as.factor(agg_dam_data$SR.Level)

  # No. of functional groups per SR.Level ----
  
  # Create df with all NrAm trees in SSM excluding predators
  ssm_fg_all <- subset(ident_insects, subset = Location == "SSM" & Species.type == "NrAm", select = c(BPT, Consecutive.Plot, Species, Functional.Group, SR.Level))
  ssm_fg <- ssm_fg_all[-which(ssm_fg_all$BPT %in% ssm_preds$BPT),] # only trees that had no predators
  
  agg_fg_data <- aggregate(Functional.Group ~ Consecutive.Plot + Species + SR.Level, data = ssm_fg, FUN = function(x) {
    length(unique(x))
  }) 
  agg_fg_data$SR.Level <- as.factor(agg_fg_data$SR.Level)
  
  
# Linear models ----
  
# Abundance
  
  # Create list with results of fitted models
  lm_list_abun <- vector(mode = "list", 6) # create "list" for fitted models
  sp <- unique(agg_abun_data$Species) # vector with tree species
  names(lm_list_abun) <- sp # set species names for result list
  for(i in 1:6) {
    lm_list_abun[[i]] <- lm(formula = log(Insects.sampled + 1) ~ SR.Level, data = agg_abun_data[which(agg_abun_data$Species == as.character(sp[i])),])
  } # Compute models for each tree species and write results in glm_list
  
  # Summaries
  # ACSA
  summary(lm_list_abun$ACSA) # Only SR 1 has a significant effect
  # F value is not significant ! -> INVALID REGRESSION
  anova(lm_list_abun$ACSA)
  
  # BEPA
  summary(lm_list_abun$BEPA) # SR 1 & 3 have a significant effect
  # F value is not significant ! -> INVALID REGRESSION
  anova(lm_list_abun$BEPA)
  
  # LALA
  summary(lm_list_abun$LALA) # Only SR 1 & 6 have a significant effect
  # F value is significant ! -> VALID REGRESSION
  anova(lm_list_abun$LALA)
  
  # PIGL
  summary(lm_list_abun$PIGL) # Only SR 1 has a significant effect
  # F value is not significant ! -> INVALID REGRESSION
  anova(lm_list_abun$PIGL)
  
  # PIST
  summary(lm_list_abun$PIST) # Only SR 1 has a significant effect
  # F value is not significant ! -> INVALID REGRESSION
  anova(lm_list_abun$PIST)
  
  # QURU
  summary(lm_list_abun$QURU) # Only SR 1 has a significant effect
  # F value is significant ! -> VALID REGRESSION
  anova(lm_list_abun$QURU)
  
  # Abundance lm plots
  par(mfrow = c(2,2))
  plot(lm_list_abun[["ACSA"]]) # 1st variance not increasing towards right end; 2nd pretty good; 3rd I CAN'T INTERPRET THIS; 4th no critical values
  plot(lm_list_abun[["BEPA"]]) # 1st slight decrease in variance at rigth end but otherwise ok; 2nd ok; 3rd I CAN'T INTERPRET THIS; 4th no critical values
  plot(lm_list_abun[["LALA"]]) # 1st increase in variance at middle problematic but otherwise ok; 2nd slight deviance at the end; 3rd I CAN'T INTERPRET THIS; 4th no critical values
  plot(lm_list_abun[["PIGL"]]) # 1st variance not increasing towards right end; 2nd slight deviance at beginning and end; 3rd I CAN'T INTERPRET THIS; 4th no critical values
  plot(lm_list_abun[["PIST"]]) # 1st variance not increasing towards right end; 2nd slight deviance at beginning and end; 3rd I CAN'T INTERPRET THIS; 4th no critical values
  plot(lm_list_abun[["QURU"]]) # 1st slight decrease in variance at rigth end but otherwise ok; 2nd slight deviance at beginning and end; 3rd I CAN'T INTERPRET THIS; 4th no critical values
  
# Leaf damage
  
  # Create list with results of fitted models
  lm_list_dam <- vector(mode = "list", 6) # create "list" for fitted models
  sp <- unique(agg_abun_data$Species) # vector with tree species
  names(lm_list_dam) <- sp # set species names for result list
  for(i in 1:6) {
    lm_list_dam[[i]] <- lm(formula = log(Mean.damage.per.plot + 1) ~ SR.Level, data = agg_dam_data[which(agg_dam_data$Species == as.character(sp[i])),])
  }
  
  # Summaries
  # ACSA
  summary(lm_list_dam$ACSA) # Only SR 1 has a significant effect
  # F value is not significant ! -> INVALID REGRESSION
  anova(lm_list_dam$ACSA)
  
  # BEPA
  summary(lm_list_dam$BEPA) # SR 1 & 6 have a significant effect
  # F value is significant ! -> VALID REGRESSION
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
  summary(lm_list_dam$QURU) # Only SR 1 has a significant effect
  # F values is not significant ! -> INVALID REGRESSION
  anova(lm_list_dam$QURU)
  
  # Leaf damage lm plots
  plot(lm_list_dam[["ACSA"]]) # 1st slight decrease in variance at rigth end but otherwise ok; 2nd slight deviance at beginning and end; 3rd I CAN'T INTERPRET THIS; 4th no critical values
  plot(lm_list_dam[["BEPA"]]) # 1st increase in variance at middle problematic but otherwise ok; 2nd slight deviance at beginning and end; 3rd I CAN'T INTERPRET THIS; 4th no critical values
  plot(lm_list_dam[["LALA"]]) # 1st ok; 2nd slight deviance at beginning and big deviance at end; 3rd I CAN'T INTERPRET THIS; 4th 197 is a critical value
  plot(lm_list_dam[["PIGL"]]) # 1st ok; 2nd slight deviance at beginning and big deviance at end; 3rd I CAN'T INTERPRET THIS; 4th 262 is a critical value
  plot(lm_list_dam[["PIST"]]) # 1st increase in variance at middle problematic but otherwise ok; 2nd big deviance at end; 3rd I CAN'T INTERPRET THIS; 4th no critical values
  plot(lm_list_dam[["QURU"]]) # 1st variance at left side too big; 2nd slight deviance at beginning and end; 3rd I CAN'T INTERPRET THIS; 4th 402 is a critical value
  
# No. of functional groups
  
  # Create list with results of fitted models
  lm_list_fg <- vector(mode = "list", 6) # create "list" for fitted models
  sp <- unique(agg_abun_data$Species) # vector with tree species
  names(lm_list_fg) <- sp # set species names for result list
  for(i in 1:6) {
    lm_list_fg[[i]] <- lm(formula = Functional.Group ~ SR.Level, data = agg_fg_data[which(agg_fg_data$Species == as.character(sp[i])),])
  } # Compute models for each tree species and write results in glm_list
  
  # Summaries
  # ACSA
  summary(lm_list_fg$ACSA) # Only SR 1 has a significant effect
  # F values is not significant ! -> INVALID REGRESSION
  anova(lm_list_fg$ACSA)
  
  # BEPA
  summary(lm_list_fg$BEPA) # SR 1 has a significant effect
  # F values is insignificant ! -> INVALID REGRESSION
  anova(lm_list_fg$BEPA)
  
  # LALA
  summary(lm_list_fg$LALA) # Only SR 1 has a significant effect
  # F values is not significant ! -> INVALID REGRESSION
  anova(lm_list_fg$LALA)
  
  # PIGL
  summary(lm_list_fg$PIGL) # Only SR 1 has a significant effect
  # F values is not significant ! -> INVALID REGRESSION
  anova(lm_list_fg$PIGL)
  
  # PIST
  summary(lm_list_fg$PIST)  # All SR Levels have a significant effect
  # F values is significant ! -> VALID REGRESSION
  anova(lm_list_fg$PIST)
  
  # QURU
  summary(lm_list_fg$QURU) # Only SR 1 has a significant effect
  # F values is not significant ! -> INVALID REGRESSION
  anova(lm_list_fg$QURU)
  
  # No of functional groups lm plots
  par(mfrow = c(2,2))
  plot(lm_list_fg[["ACSA"]]) # 1st variance not increasing towards right end; 2nd pretty good; 3rd I CAN'T INTERPRET THIS; 4th no critical values
  plot(lm_list_fg[["BEPA"]]) # 1st slight decrease in variance at rigth end but otherwise ok; 2nd ok; 3rd I CAN'T INTERPRET THIS; 4th no critical values
  plot(lm_list_fg[["LALA"]]) # 1st increase in variance at middle problematic but otherwise ok; 2nd slight deviance at the end; 3rd I CAN'T INTERPRET THIS; 4th no critical values
  plot(lm_list_fg[["PIGL"]]) # 1st variance not increasing towards right end; 2nd slight deviance at beginning and end; 3rd I CAN'T INTERPRET THIS; 4th no critical values
  plot(lm_list_fg[["PIST"]]) # 1st variance not increasing towards right end; 2nd slight deviance at beginning and end; 3rd I CAN'T INTERPRET THIS; 4th no critical values
  plot(lm_list_fg[["QURU"]]) # 1st slight decrease in variance at rigth end but otherwise ok; 2nd slight deviance at beginning and end; 3rd I CAN'T INTERPRET THIS; 4th no critical values
  
  
  
# Plots ----
  
# Insect abundance
 
  sp <- unique(agg_abun_data$Species) # vector with tree species
  abun_plots <- list() # create a list for all the plots
  for(i in 1:6) {
    abun_plots[[i]] <- ggplot(data = subset(agg_abun_data, subset = Species == as.character(sp[i]), select = c("SR.Level", "Insects.sampled")), aes(x = as.factor(SR.Level), y = log(Insects.sampled + 1))) +
      geom_boxplot()+
      geom_smooth(method = "lm", se = FALSE, color = "black", aes(group = 1), size = .5) +
      ggtitle(as.character(sp[i])) +
      xlab("SR-Level") + ylab("Mean insect abundance (log + 1)")
  } # generate the plots for every NrAm species
  
  ggarrange(abun_plots[[1]],abun_plots[[2]],abun_plots[[3]],abun_plots[[4]],abun_plots[[5]],abun_plots[[6]], ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom") # plot of all species together
  
# Leaf damage
  
  sp <- unique(agg_abun_data$Species) # vector with tree species
  dam_plots <- list() # create a list for all the plots
  for(i in 1:6) {
    dam_plots[[i]] <- ggplot(data = subset(agg_dam_data, subset = Species == as.character(sp[i]), select = c("SR.Level", "Mean.damage.per.plot")), aes(x = as.factor(SR.Level), y = log(Mean.damage.per.plot + 1))) +
      geom_boxplot()+
      geom_smooth(method = "lm", se = FALSE, color = "black", aes(group = 1), size = .5) +
      ggtitle(as.character(sp[i])) +
      xlab("SR-Level") + ylab("Mean leaf damage (log+1)")
  } # generate the plots for every NrAm species
  
  # Make the regression line for significant sp red
  dam_plots[[2]] <- ggplot(data = subset(agg_dam_data, subset = Species == as.character(sp[2]), select = c("SR.Level", "Mean.damage.per.plot")), aes(x = as.factor(SR.Level), y = log(Mean.damage.per.plot + 1))) +
    geom_boxplot()+
    geom_smooth(method = "lm", se = FALSE, color = "red", aes(group = 1), size = .5) +
    ggtitle(as.character(sp[2])) +
    xlab("SR-Level") + ylab("Mean leaf damage (log+1)")
   
  
  ggarrange(dam_plots[[1]],dam_plots[[2]],dam_plots[[3]],dam_plots[[4]],dam_plots[[5]],dam_plots[[6]], ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom") # plot of all species together

# No of functional groups  
  
  sp <- unique(agg_abun_data$Species) # vector with tree species
  fg_plots <- list() # create a list for all the plots
  for(i in 1:6) {
    fg_plots[[i]] <- ggplot(data = subset(agg_fg_data, subset = Species == as.character(sp[i]), select = c("SR.Level", "Functional.Group")), aes(x = as.factor(SR.Level), y = Functional.Group)) +
      geom_boxplot()+
      ggtitle(as.character(sp[i])) +
      xlab("SR-Level") + ylab(" No. of functional group") + scale_y_log10()
  } # generate the plots for every NrAm species
  
  ggarrange(fg_plots[[1]],fg_plots[[2]],fg_plots[[3]],fg_plots[[4]],fg_plots[[5]],fg_plots[[6]], ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom") # plot of all species together

# Apparent competition ----
  # Compare SR1 and SR2
  
# Insect abundance
  
  # Create df with subsetted data
  abun_app <- subset(agg_abun_data, subset = SR.Level == c(1,2), select = c(Consecutive.Plot, Species, Insects.sampled, SR.Level))
  
  # Linear model 
  lm_list_abun_app <- vector(mode = "list", 6) # create "list" for fitted models
  sp <- unique(agg_abun_data$Species) # vector with tree species
  names(lm_list_abun_app) <- sp # set species names for result list
  for(i in 1:6) {
    lm_list_abun_app[[i]] <- lm(formula = log(Insects.sampled + 1) ~ SR.Level, data = abun_app[which(abun_app$Species == as.character(sp[i])),])
  }

  # Summaries
  # ACSA
  summary(lm_list_abun_app$ACSA) # Only SR 1 has a significant effect
  # F values is not significant ! -> INVALID REGRESSION
  anova(lm_list_abun_app$ACSA)
  
  # BEPA
  summary(lm_list_abun_app$BEPA) # Only SR 1 has a significant effect
  # F values is not significant ! -> INVALID REGRESSION
  anova(lm_list_abun_app$BEPA)
  
  # LALA
  summary(lm_list_abun_app$LALA) # SR1 has a significant effect
  # F values is not significant ! -> INVALID REGRESSION
  anova(lm_list_abun_app$LALA)
  
  # PIGL
  summary(lm_list_abun_app$PIGL) # SR1 has a significant effect
  # F values is not significant ! -> INVALID REGRESSION
  anova(lm_list_abun_app$PIGL)
  
  # PIST
  summary(lm_list_abun_app$PIST) # SR1 has a significant effect
  # F values is not significant ! -> INVALID REGRESSION
  anova(lm_list_abun_app$PIST)
  
  # QURU
  summary(lm_list_abun_app$QURU) # Only SR 1 has a significant effect
  # F values is not significant ! -> INVALID REGRESSION
  anova(lm_list_abun_app$QURU)
  
  # Leaf damage lm plots
  plot(lm_list_abun_app[["ACSA"]]) # 1st slight decrease in variance at rigth end but otherwise ok; 2nd slight deviance at beginning and end; 3rd I CAN'T INTERPRET THIS; 4th no critical values
  plot(lm_list_abun_app[["BEPA"]]) # 1st increase in variance at middle problematic but otherwise ok; 2nd slight deviance at beginning and end; 3rd I CAN'T INTERPRET THIS; 4th no critical values
  plot(lm_list_abun_app[["LALA"]]) # 1st ok; 2nd slight deviance at beginning and big deviance at end; 3rd I CAN'T INTERPRET THIS; 4th 197 is a critical value
  plot(lm_list_abun_app[["PIGL"]]) # 1st ok; 2nd slight deviance at beginning and big deviance at end; 3rd I CAN'T INTERPRET THIS; 4th 262 is a critical value
  plot(lm_list_abun_app[["PIST"]]) # 1st increase in variance at middle problematic but otherwise ok; 2nd big deviance at end; 3rd I CAN'T INTERPRET THIS; 4th no critical values
  plot(lm_list_abun_app[["QURU"]]) # 1st variance at left side too big; 2nd slight deviance at beginning and end; 3rd I CAN'T INTERPRET THIS; 4th 402 is a critical value
  
  # Plot
  
  sp <- unique(abun_app$Species) # vector with tree species
  abun_app_plots <- list() # create a list for all the plots
  for(i in 1:6) {
    abun_app_plots[[i]] <- ggplot(data = subset(abun_app, subset = Species == as.character(sp[i]), select = c("SR.Level", "Insects.sampled")), aes(x = as.factor(SR.Level), y = Insects.sampled)) +
      geom_boxplot()+
      ggtitle(as.character(sp[i])) +
      xlab("SR-Level") + ylab("log insect abundance")
  } # generate the plots for every NrAm species
  
  ggarrange(abun_app_plots[[1]], abun_app_plots[[2]], abun_app_plots[[3]], abun_app_plots[[4]], abun_app_plots[[5]], abun_app_plots[[6]], ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom") # plot of all species together

# Leaf damage
  
  # Create df with subsetted data
  dam_app <- subset(agg_dam_data, subset = SR.Level == c(1,2), select = c(Consecutive.Plot, Species, Mean.damage.per.plot, SR.Level))
  
  # Linear model 
  lm_list_dam_app <- vector(mode = "list", 6) # create "list" for fitted models
  sp <- unique(agg_dam_data$Species) # vector with tree species
  names(lm_list_dam_app) <- sp # set species names for result list
  # Log transformation ?! -> creates negative values !
  dam_app_log <- dam_app[which(dam_app$Mean.damage.per.plot > 0),] # exclude zeros to use log()
  for(i in 1:6) {
    lm_list_dam_app[[i]] <- lm(formula = log(Mean.damage.per.plot + 1) ~ SR.Level, data = dam_app[which(dam_app$Species == as.character(sp[i])),])
  }
  
  # Summaries
  # ACSA
  summary(lm_list_dam_app$ACSA) # Only SR 1 has a significant effect
  # F values is not significant ! -> INVALID REGRESSION
  anova(lm_list_dam_app$ACSA)
  
  # BEPA
  summary(lm_list_dam_app$BEPA) # Only SR 1 has a significant effect
  # F values is not significant ! -> INVALID REGRESSION
  anova(lm_list_dam_app$BEPA)
  
  # LALA
  summary(lm_list_dam_app$LALA) # No significant effect
  # F values is not significant ! -> INVALID REGRESSION
  anova(lm_list_dam_app$LALA)
  
  # PIGL
  summary(lm_list_dam_app$PIGL) # SR 1 and SR2 have significant effects
  # F values is significant ! -> VALID REGRESSION
  anova(lm_list_dam_app$PIGL)
  
  # PIST
  summary(lm_list_dam_app$PIST) # Only SR1 has a significant effect
  # F values is not significant ! -> INVALID REGRESSION
  anova(lm_list_dam_app$PIST)
  
  # QURU
  summary(lm_list_dam_app$QURU) # Only SR 1 has a significant effect
  # F values is not significant ! -> INVALID REGRESSION
  anova(lm_list_dam_app$QURU)
  
  # Leaf damage lm plots
  plot(lm_list_dam_app[["ACSA"]]) # 1st slight decrease in variance at rigth end but otherwise ok; 2nd slight deviance at beginning and end; 3rd I CAN'T INTERPRET THIS; 4th no critical values
  plot(lm_list_dam_app[["BEPA"]]) # 1st increase in variance at middle problematic but otherwise ok; 2nd slight deviance at beginning and end; 3rd I CAN'T INTERPRET THIS; 4th no critical values
  plot(lm_list_dam_app[["LALA"]]) # 1st ok; 2nd slight deviance at beginning and big deviance at end; 3rd I CAN'T INTERPRET THIS; 4th 197 is a critical value
  plot(lm_list_dam_app[["PIGL"]]) # 1st ok; 2nd slight deviance at beginning and big deviance at end; 3rd I CAN'T INTERPRET THIS; 4th 262 is a critical value
  plot(lm_list_dam_app[["PIST"]]) # 1st increase in variance at middle problematic but otherwise ok; 2nd big deviance at end; 3rd I CAN'T INTERPRET THIS; 4th no critical values
  plot(lm_list_dam_app[["QURU"]]) # 1st variance at left side too big; 2nd slight deviance at beginning and end; 3rd I CAN'T INTERPRET THIS; 4th 402 is a critical value
  
  # Plot
  
  sp <- unique(dam_app$Species) # vector with tree species
  dam_app_plots <- list() # create a list for all the plots
  for(i in 1:6) {
    dam_app_plots[[i]] <- ggplot(data = subset(dam_app, subset = Species == as.character(sp[i]), select = c("SR.Level", "Mean.damage.per.plot")), aes(x = as.factor(SR.Level), y = Mean.damage.per.plot)) +
      geom_boxplot()+
      ggtitle(as.character(sp[i])) +
      xlab("SR-Level") + ylab("Leaf damage") + scale_y_log10()
  } # generate the plots for every NrAm species
  
  ggarrange(dam_app_plots[[1]],dam_app_plots[[2]],dam_app_plots[[3]],dam_app_plots[[4]],dam_app_plots[[5]],dam_app_plots[[6]], ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom") # plot of all species together
  