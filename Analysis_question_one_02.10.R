# Analysis question one
setwd("~/Dokumente/Uni/Umwi/B.Sc. Thesis/IDENT/Analysis")
source(file = "data_formatting.R", local = T)
library(ggplot2)

# # Abbreviations:----
# Eurp: european sp.
# NrAm: northamerican sp.
# FR: Freiburg site
# SSM: Sault. Ste. Marie site

# UPDATE LOG:
# 14.09 : abun and dmg model with log+1 transformation
# 30.09 : included Cat3 again

# Insect abundance ----
  
  # NrAm in SSM 
  NrAm_SSM_trees <- subset(ident_trees, subset = Location == "SSM" & Species.type == "NrAm" & SR.Level == 1, select = c(BPT, Consecutive.Plot, Species, Insects.sampled)) # Dataframe with all SR = 1 plots of NrAm sp. in SSM
  abun_NrAm_SSM <- aggregate(Insects.sampled ~ Consecutive.Plot + Species, data = NrAm_SSM_trees, FUN = mean, na.rm = T) # Aggregated dataframe
  abun_NrAm_SSM$Category <- 1 # Add category column
  names(abun_NrAm_SSM) <- c("Plot", "Species", "Insects.sampled", "Category")

  # NrAm in FR
  NrAm_FR_trees <- subset(ident_trees, subset = Location == "FR" & Species.type == "NrAm" & SR.Level == 1, select = c(BPT, Plot, Species, Insects.sampled)) # Dataframe with all SR = 1 plots of NrAm sp. in FR
  abun_NrAm_FR <- aggregate(Insects.sampled ~ Plot + Species, data = NrAm_FR_trees, FUN = mean, na.rm = T) # Aggregated dataframe
  abun_NrAm_FR$Category <- 2 # Add category column
  
  # Eurp in FR
  Eurp_FR_trees <- subset(ident_trees, subset = Location == "FR" & Species.type == "Eurp" & SR.Level == 1, select = c(BPT, Plot, Species, Insects.sampled)) # Dataframe with all SR = 1 plots of Eurp sp. in FR
  abun_Eurp_FR <- aggregate(Insects.sampled ~ Plot + Species, data = Eurp_FR_trees, FUN = mean, na.rm = T) # Aggregated dataframe
  abun_Eurp_FR$Category <- 3 # Add category column
  
  # Number of functional groups ----
  
  # NrAm in SSM
  NrAm_SSM_insects <- subset(ident_insects, subset = Location == "SSM" & Species.type == "NrAm" & SR.Level == 1, select = c(BPT, Consecutive.Plot, Species, Functional.Group)) # Dataframe with all SR = 1 plots of NrAm sp in SSM # NEEDS FUNCTIONAL.GROUP COL WHICH IS NOT FINISHED !!
  fg_NrAm_SSM <- aggregate(Functional.Group ~ Consecutive.Plot + Species, data = NrAm_SSM_insects, FUN = function(x) {
    length(unique(x))
  }) 
  fg_NrAm_SSM$Category <- 1 # Add category column
  names(fg_NrAm_SSM) <- c("Plot", "Species", "Functional.Group", "Category")
  
  # NrAm in FR
  NrAm_FR_insects <- subset(ident_insects, subset = Location == "FR" & Species.type == "NrAm" & SR.Level == 1, select = c(BPT, Plot, Species, Order, Suborder, Family, Genus, Functional.Group))
  fg_NrAm_FR <- aggregate(Functional.Group ~ Plot + Species, data = NrAm_FR_insects, FUN = function(x) {
    length(unique(x))
  })
  fg_NrAm_FR$Category <- 2 # Add category column
  
  # Eurp in FR
  Eurp_FR_insects <- subset(ident_insects, subset = Location == "FR" & Species.type == "Eurp" & SR.Level == 1, select = c(BPT, Plot, Species, Order, Suborder, Family, Genus, Functional.Group))
  fg_Eurp_FR <- aggregate(Functional.Group ~ Plot + Species, data = Eurp_FR_insects, FUN = function(x) {
    length(unique(x))
  })
  fg_Eurp_FR$Category <- 3 # Add category column
  
# Leaf damage ----
  
  # NrAm in SSM
  NrAm_SSM_dmg <- subset(ident_damage, subset = Location == "SSM" & Species.type == "NrAm" & SR.Level == 1, select = c(BPT, Consecutive.Plot, Species, Total.damage.per.leaf))
  
  # Aggregate the Total damage per Tree
  NrAm_SSM_dmg_agg <- aggregate(x = list(Total.damage.per.tree = NrAm_SSM_dmg$Total.damage.per.leaf), by = subset(NrAm_SSM_dmg, select = c(BPT, Consecutive.Plot, Species)), FUN = mean)
  
  # Aggregate to mean damage per plot
  tot_dmg_NrAm_SSM <- aggregate(Total.damage.per.tree ~ Consecutive.Plot + Species, data = NrAm_SSM_dmg_agg, FUN = mean) # aggregate damage to plot level
  tot_dmg_NrAm_SSM$Category <- 1 # Add category column
  names(tot_dmg_NrAm_SSM) <- c("Plot", "Species", "Mean.damage.per.plot", "Category")
  
  # NrAm in FR
  NrAm_FR_dmg <- subset(ident_damage, subset = Location == "FR" & Species.type == "NrAm" & SR.Level == 1, select = c(BPT, Plot, Species, Total.damage.per.leaf))
  
  # Aggregate to total damage per tree
  NrAm_FR_dmg_agg <- aggregate(x = list(Total.damage.per.tree = NrAm_FR_dmg$Total.damage.per.leaf), by = subset(NrAm_FR_dmg, select = c(BPT, Plot, Species)), FUN = mean)
  
  # Aggregate to mean damage per plot
  tot_dmg_NrAm_FR <- aggregate(Total.damage.per.tree ~ Plot + Species, data = NrAm_FR_dmg_agg, FUN = mean) # aggregate damage to plot level
  tot_dmg_NrAm_FR$Category <- 2 # Add category column
  names(tot_dmg_NrAm_FR) <- c("Plot", "Species", "Mean.damage.per.plot", "Category")
  
  # Eurp in FR
  Eurp_FR_dmg <- subset(ident_damage, subset = Location == "FR" & Species.type == "Eurp" & SR.Level == 1, select = c(BPT, Plot, Species, Total.damage.per.leaf))
  
  # Aggregate to total damage per tree
  Eurp_FR_dmg_agg <- aggregate(x = list(Total.damage.per.tree = Eurp_FR_dmg$Total.damage.per.leaf), by = subset(Eurp_FR_dmg, select = c(BPT, Plot, Species)), FUN = mean)
  
  # Aggregate to mean damage per plot
  tot_dmg_Eurp_FR <- aggregate(Total.damage.per.tree ~ Plot + Species, data = Eurp_FR_dmg_agg, FUN = mean) # aggregate damage to plot level
  tot_dmg_Eurp_FR$Category <- 3 # Add category column
  names(tot_dmg_Eurp_FR) <- c("Plot", "Species", "Mean.damage.per.plot", "Category")
  
# Linear models ----
  
# Insect abundance
  
  # Df with all three categories
  abun_all <- merge(merge(abun_Eurp_FR, abun_NrAm_FR, all = T), abun_NrAm_SSM, all = T)
  
  # Set tree genera
  abun_sp_gen <- data.frame("Tree.sp" = sort(unique(abun_all$Species)),"Genus" = c("Acer", "Betula", "Larix", "Picea", "Pinus", "Quercus")) # df with tree species and genus col
  for(i in 1:12) {  
    abun_all$Genus[which(abun_all$Species == as.character(abun_sp_gen$Tree.sp[i]))] <- as.character(abun_sp_gen$Genus[i])
  } # Set congeners to respecitve genus
  abun_all$Category <- as.factor(abun_all$Category)
  abun_all$Genus <- as.factor(abun_all$Genus)
  
  # Check predictor interaction
  interaction.plot(x.factor = abun_all$Category, trace.factor = abun_all$Genus, response = log(abun_all$Insects.sampled + 1), col = c(1:6))
  # Plot shows that interactions occur -> see anova result for the significance of the Genus:Category interaction
  abun_test <- lm(formula = log(Insects.sampled + 1) ~ Genus * Category, data = abun_all) 
  anova(abun_test)
  
  # Linear model
  
  # With independent predictors
  abun_fit <- lm(formula = log(Insects.sampled + 1) ~ Genus * Category, data = abun_all)
  summary(abun_fit)
  # Larix, Picea and Pinus have a significant effect on the insect abundance; also the thrid category is significantly influencing the insect abundance
  # F test shows that the regression is significant
  anova(abun_fit) # Genus and Category are significant
  par(mfrow = c(2,2))
  plot(abun_fit)
  # 1st plot looks good; 2nd just a little deviance from 1:1 line; 3rd I CAN'T INTERPRET THIS !; 4th shows that the 64th value might be critical
  
  
  # # With dependent predictors
  # abun_fit2 <- lm(formula = Insects.sampled ~ Genus * Category, data = abun_all) 
  # summary(abun_fit2)
  # anova(abun_fit2)
  # par(mfrow = c(2,2))
  # plot(abun_fit2)
  
  
# Number of functional groups
  
  # Df with all three categories
  fg_all <- merge(merge(fg_Eurp_FR, fg_NrAm_FR, all = T), fg_NrAm_SSM, all = T)
  
  # Set tree genera
  fg_sp_gen <- data.frame("Tree.sp" = sort(unique(fg_all$Species)),"Genus" = c("Acer", "Betula", "Larix", "Picea", "Pinus", "Quercus")) # df with tree species and genus col
  for(i in 1:12) {  
    fg_all$Genus[which(fg_all$Species == as.character(fg_sp_gen$Tree.sp[i]))] <- as.character(fg_sp_gen$Genus[i])
  } # Set congeners to respecitve genus
  fg_all$Category <- as.factor(fg_all$Category)
  fg_all$Genus <- as.factor(fg_all$Genus)
  
  # Check predictor interaction
  interaction.plot(x.factor = fg_all$Category, trace.factor = fg_all$Genus, response = fg_all$Functional.Group, col = c(1:6))
  # Plot shows that interactions occur -> see anova result for the significance of the Genus:Category interaction
  fg_test <- lm(formula = Functional.Group ~ Genus * Category, data = fg_all) 
  anova(fg_test)
  
  
  # Linear model
  # With independent predictors
  fg_fit <- lm(formula = Functional.Group ~ Genus * Category, data = fg_all)
  summary(fg_fit)
  # Acer, Picea and Pinus have a significant effect on the number of functional groups; 3rd category has a significant effect
  # F test shows that the regression is significant
  anova(fg_fit)
  par(mfrow = c(2,2))
  plot(fg_fit)
  
  
  # # With dependent predictors
  # fg_fit2 <- lm(formula = Functional.Group ~ Genus * Category, data = fg_all) 
  # summary(fg_fit2)
  # anova(fg_fit2)
  # par(mfrow = c(2,2))
  # plot(fg_fit2)
  
# Leaf damage 
  
  # Df with all three categories
  dmg_all <- merge(merge(tot_dmg_Eurp_FR, tot_dmg_NrAm_FR, all = T), tot_dmg_NrAm_SSM, all = T)
  
  # Set tree genera
  dmg_sp_gen <- data.frame("Tree.sp" = sort(unique(dmg_all$Species)),"Genus" = c("Acer", "Betula", "Larix", "Picea", "Pinus", "Quercus")) # df with tree species and genus col
  for(i in 1:12) {  
    dmg_all$Genus[which(dmg_all$Species == as.character(dmg_sp_gen$Tree.sp[i]))] <- as.character(dmg_sp_gen$Genus[i])
  } # Set congeners to respecitve genus
  dmg_all$Category <- as.factor(dmg_all$Category)
  dmg_all$Genus <- as.factor(dmg_all$Genus)
  
  # Check predictor interaction
  interaction.plot(x.factor = dmg_all$Category, trace.factor = dmg_all$Genus, response = dmg_all$Mean.damage.per.plot, col = c(1:6))
  # Plot shows that interactions occur -> see anova result for the significance of the Genus:Category interaction
  dmg_test <- lm(formula = Mean.damage.per.plot ~ Genus * Category, data = dmg_all) 
  anova(dmg_test)
  # The interaction is smaller 0.05 (3.034e-05) and thus significant ! -> treat predictors as dependent
  
  # Linear model
  # # With independent predictors
  # dmg_fit <- lm(formula = Total.damage.per.leaf ~ Genus + Category, data = dmg_all)
  # summary(dmg_fit)
  # anova(dmg_fit)
  # par(mfrow = c(2,2))
  # plot(dmg_fit)
  
  # With dependent predictors
  dmg_fit <- lm(formula = log(Mean.damage.per.plot + 1) ~ Genus * Category, data = dmg_all) 
  summary(dmg_fit)
  # For category 1 all tree genera have significant effects, in Cat 3 Acer,Larix, Picea, Pinus and Quercus have significant effects
  # F valiue shows that the regression is significant
  anova(dmg_fit) # Genus, Category and Interaction are significant
  # Only Sum sq of the interaction to be compared
  par(mfrow = c(2,2))
  plot(dmg_fit)
  # 1st plot should have more variance towards reight end; 2nd pretty high deviance from 1:1 line at the beginning and end; 3rd I CAN'T INTERPRET THIS !; 4th no critical values apparent
  
  
  
# Plots ----

# Add SD and Mean for every Genus
  genera <- c("Acer", "Betula", "Larix", "Picea", "Pinus", "Quercus") # create vector containing the genera names
  categories <- c(1,2,3) # create vector containing category levels 
  
  # Insect abundance
  abun_all$lSD <- 0 # Add SD col to abun_all
  abun_all$lMean <- 0 # Add Mean col to abun_all
  for(p in 1:3) { # for every category...
    for(i in 1:6) { #...do...
      abun_all$lSD[which(abun_all$Genus == as.character(genera[i]) & abun_all$Category == categories[p])] <- sd(log(abun_all$Insects.sampled[which(abun_all$Genus == as.character(genera[i]) & abun_all$Category == categories[p])]+1), na.rm = T) # calculate SD values and write them into the df
      abun_all$lMean[which(abun_all$Genus == as.character(genera[i]) & abun_all$Category == categories[p])] <- mean(log(abun_all$Insects.sampled[which(abun_all$Genus == as.character(genera[i]) & abun_all$Category == categories[p])]+1)) # calulate mean and write them into the df
    }
  }
  
  # No. of functional groups
  fg_all$SD <- 0 # Add SD col to fg_all
  fg_allMean <- 0 # Add Mean col to fg_all
  for(p in 1:3) { # for every category...
    for(i in 1:6) { #...do...
      fg_all$lSD[which(fg_all$Genus == as.character(genera[i]) & fg_all$Category == categories[p])] <- sd(fg_all$Functional.Group[which(fg_all$Genus == as.character(genera[i]) & fg_all$Category == categories[p])], na.rm = T) # calculate SD values and write them into the df
      fg_all$lMean[which(fg_all$Genus == as.character(genera[i]) & fg_all$Category == categories[p])] <- mean(unique(fg_all$Functional.Group[which(fg_all$Genus == as.character(genera[i]) & fg_all$Category == categories[p])])) # calulate mean and write them into the df
      }
  } 
  
  # Leaf damage
  dmg_all$SD <- 0 # Add SD col to dmg_all
  dmg_all$Mean <- 0 # Add Mean col to dmg_all
  for(p in 1:3) { # for every category...
    for(i in 1:6) { #...do...
      dmg_all$SD[which(dmg_all$Genus == as.character(genera[i]) & dmg_all$Category == categories[p])] <- sd(dmg_all$Total.damage.per.leaf[which(dmg_all$Genus == as.character(genera[i]) & dmg_all$Category == categories[p])], na.rm = T) # calculate SD values and write them into the df
      dmg_all$Mean[which(dmg_all$Genus == as.character(genera[i]) & dmg_all$Category == categories[p])] <- mean(dmg_all$Total.damage.per.leaf[which(dmg_all$Genus == as.character(genera[i]) & dmg_all$Category == categories[p])]) # calulate mean and write them into the df
    }
  }

# Plotting  
  
  # X achse rücktransformieren auf tatsächliche abundanz
  # in geom_bar help nachschauen; reihenfolge von transformieren und mittelwert berechnen is problem
  # alternativ log(insect.sampled) in abun_all
  # SE nicht SD benutzen
  # Insect abundance  
  ggplot(data = abun_all, aes(x = as.factor(Genus), y = log(Insects.sampled + 1), fill = as.factor(Category))) +
    geom_col(stat = "count", position = "dodge") +
    geom_errorbar(aes(ymin =  lMean - lSD, ymax = lMean + lSD), stat = "identity", position = "dodge") +
    scale_fill_discrete(name = "Category",
                        breaks = c(1, 2, 3),
                        labels = c("NrAm in SSM", "NrAm in FR", "Eurp in FR")) +
    xlab("Genus") + ylab("Mean insect abundance (log+1)")
  
  # No. of functional groups
  ggplot(data = fg_all, aes(x = as.factor(Genus), y = Functional.Group, fill = as.factor(Category))) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes( ymin =  Mean - SD, ymax = Mean + SD), stat = "identity", position = position_dodge(1)) +
    scale_fill_discrete(name = "Category",
                        breaks = c(1, 2, 3),
                        labels = c("NrAm in SSM", "NrAm in FR", "Eurp in FR")) +
    xlab("Genus") + ylab("No. of functional groups")
  
  
  # Leaf damage
  ggplot(data = dmg_all, aes(x = Genus, y = log(Mean.damage.per.plot + 1), fill = Category)) +
    geom_bar(stat = "identity", position = "dodge") +
    #geom_errorbar(aes(x = Genus, ymin =  Mean - SD, ymax = Mean + SD), width = 0.1, stat = "identity", position = position_dodge(1)) +
    scale_fill_discrete(name = "Category",
                        breaks = c(1, 2, 3),
                        labels = c("NrAm in SSM", "NrAm in FR", "Eurp in FR")) +
    xlab("Genus") + ylab("Mean leaf damage (log+1)")
  