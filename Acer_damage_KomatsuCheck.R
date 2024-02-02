# library(rcompanion)
library(car)
# library(lattice)
# library(effects)
library(MASS)
library(lme4)
library(emmeans)
library(RVAideMemoire) #backtransform emmeans
library(glmmTMB) #mixed models for zero-inflated count data (bug data)
library(ggpubr)
library(RColorBrewer)
library(gridExtra)
library(tidyverse)

setwd("~/Dropbox (University of Michigan)/2021_SummerInterns/EB") #elizabeth
setwd("C:/Users/hrusk/Dropbox (Smithsonian)/2021_SummerInterns/EB") #amy laptop
setwd("C:\\Users\\kjkomatsu\\OneDrive - UNCG\\manuscripts\\last author\\blake_urban tree damage") # kim


theme_set(theme_bw())
theme_update(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
             axis.line = element_line(colour = "black"), text = element_text(size = 24),
             axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)), 
             axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))


#barGraphStats(data=, variable="", byFactorNames=c(""))
barGraphStats <- function(data, variable, byFactorNames) {
  count <- length(byFactorNames)
  N <- aggregate(data[[variable]], data[byFactorNames], FUN=length)
  names(N)[1:count] <- byFactorNames
  names(N) <- sub("^x$", "N", names(N))
  mean <- aggregate(data[[variable]], data[byFactorNames], FUN=mean)
  names(mean)[1:count] <- byFactorNames
  names(mean) <- sub("^x$", "mean", names(mean))
  sd <- aggregate(data[[variable]], data[byFactorNames], FUN=sd)
  names(sd)[1:count] <- byFactorNames
  names(sd) <- sub("^x$", "sd", names(sd))
  preSummaryStats <- merge(N, mean, by=byFactorNames)
  finalSummaryStats <- merge(preSummaryStats, sd, by=byFactorNames)
  finalSummaryStats$se <- finalSummaryStats$sd / sqrt(finalSummaryStats$N)
  return(finalSummaryStats)
}

options(contrasts=c('contr.sum','contr.poly'))

#### Organize Data ####

damage <- read.csv("A_rubrum_damage_04Aug2021.csv") %>% 
  mutate(Income=as.character(replace_na(Income, "None"))) %>% # income level to character, set forest income to "None" (not applicable for the site)
  filter(!is.na(Percent_damage)) %>% 
  #create zero indicator for percent damage data
  mutate(Percent_damage_zeroindicator=ifelse(Percent_damage==0, 0, 1)) %>% 
  #create total herbivore and predator columns
  mutate(herbivore = Grasshoppers_crickets_katydids + Caterpillar_like + Hoppers + Aphids + Thrips + Herb_beetles + Whiteflies_mealybugs_scale.insects + Mirid + Larve_pinchers, 
         predator = Pred_beetles + Assassins + Spiders + Wasps) 


#### Testing for normality ####
plot(damage$Percent_damage)
hist(damage$Percent_damage)

shapiro.test(damage$Percent_damage)
ggqqplot(damage$Percent_damage)

#log transform
damage$Percent_damage_log <- log(damage$Percent_damage +1)
shapiro.test(damage$Percent_damage_log)
ggqqplot(damage$Percent_damage_log)
plot(damage$Percent_damage_log)
hist(damage$Percent_damage_log)

#sin transform
damage$Percent_damage_sin <- asinh(damage$Percent_damage)
shapiro.test(damage$Percent_damage_sin)
ggqqplot(damage$Percent_damage_sin)
plot(damage$Percent_damage_sin)
hist(damage$Percent_damage_sin)



#### Effect of land use on leaf damage (natural, suburban, urban) ####

## Stats ##

# Part 1: probability of zero damage
summary(zero_model <- glmer(Percent_damage_zeroindicator ~ Area_type + (1|Tree_num),
                            data = damage,
                            family = binomial))
Anova(zero_model, type='III') # Area_type   227.234  2  < 2.2e-16 ***

# Part 2: distribution of the continuous, non-zero data
summary(cont_model <- lmer(log(Percent_damage) ~ Area_type + (1|Tree_num),
                            data = subset(damage, Percent_damage_zeroindicator != 0)))
Anova(cont_model) #Area_type 8.0695  2    0.01769 *
back.emmeans(emmeans(cont_model, ~Area_type), transform='log')


## Figure ##
EBpalette1 <- c("#228833", "#725DEF", "#EE6677", "#702082")
EBpalettewoforest <- c("#DC267F", "#785EF0")

# ggplot(data = barGraphStats(data = damage, variable = "Percent_damage", byFactorNames = c("Area_type")), aes(x=Area_type, y=mean, fill=Area_type)) +
#   geom_bar(stat='identity', position=position_dodge(), width=0.8) +
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position = position_dodge(0.9)) +
#   annotate("text", x = 1, y = 2.7 , label = "a", size = 8)+
#   annotate("text", x = 2, y = 2.7 , label = "a", size = 8)+
#   annotate("text", x = 3, y = 1.75, label = "b", size = 8) +
#   scale_fill_manual(values = EBpalette1) + theme(legend.position = "none") + xlab("Area Type") + ylab("Percent Damage") + 
#   theme(legend.position = "none")

ggplot(data=damage, aes(x=Area_type, y=Percent_damage, fill=Area_type, color=Area_type)) + 
  scale_fill_manual(values = EBpalette1) + 
  scale_color_manual(values = EBpalette1) + 
  xlab("Land Use Type") + ylab("Leaf Damage (%)") +
  theme(legend.position='none') + 
  geom_violin(aes(fill=Area_type, color=Area_type,
                  fill=after_scale(colorspace::lighten(fill, .3))),
              size=1, bw=.3) +
  geom_boxplot(width=0.1, size=1, outlier.size=3, fill='white') +
  scale_x_discrete(limits=c('Urban', 'Suburban ', 'Forest')) +
  annotate("text", x = 3, y = -2, label = "c", size = 8) + #probability zero damage
  annotate("text", x = 2, y = -2, label = "b", size = 8) +
  annotate("text", x = 1, y = -2, label = "a", size = 8) +
  annotate("text", x = 3, y = 67, label = "B", size = 8) + #average of non-zero data
  annotate("text", x = 2, y = 67, label = "A", size = 8) +
  annotate("text", x = 1, y = 55, label = "A", size = 8)
#export at 600x600



#### Effect of land use and income on leaf damage (suburban, urban) ####

## Stats ##

# Part 1: probability of zero damage
summary(zero_model <- glmer(Percent_damage_zeroindicator ~ Area_type*Income + (1|Tree_num),
                            data = subset(damage, Area_type!='Forest'),
                            family = binomial))
Anova(zero_model, type='III') 
# Area_type        104.9298  1  < 2.2e-16 ***
# Income            13.0624  2   0.001457 ** 
# Area_type:Income  10.6615  2   0.004840 **
emmeans(zero_model, ~Area_type*Income)

# Part 2: distribution of the continuous, non-zero data
summary(cont_model <- lmer(log(Percent_damage) ~ Area_type*Income + (1|Tree_num),
                           data = subset(damage, Percent_damage_zeroindicator != 0 & Area_type!='Forest')))
Anova(cont_model)
# Area_type         0.3581  1     0.5496    
# Income           18.5898  2  9.189e-05 ***
# Area_type:Income 37.4114  2  7.520e-09 ***
back.emmeans(emmeans(cont_model, ~Area_type*Income), transform='log')


## Figure ##
damage$Income <- factor(damage$Income, levels=c("Low", "Medium", "High", "None"))
EBpalette <- c("#FFCB05" ,"#00B2A9", "#00274C", "black")

# ggplot(data = barGraphStats(data = subset(damage, !is.na(Percent_damage) & Area_type!='Forest'), variable = "Percent_damage", byFactorNames = c("Area_type", "Income")), aes(x= Area_type, y=mean, fill=Income)) +
#   geom_bar(stat='identity', position=position_dodge()) +
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position = position_dodge(0.9)) +
#   annotate("text", x = .7, y = 3.2, label = "a", size = 8)+
#   annotate("text", x = 1, y = 1.8, label = "b", size = 8)+
#   annotate("text", x = 1.3, y = 4, label = "a", size = 8) +
#   annotate("text", x = 1.7, y = 3.7, label = "a", size = 8) +
#   annotate("text", x = 2, y = 1.2, label = "b", size = 8) +
#   annotate("text", x = 2.3, y = 1.5, label = "b", size = 8) +
#   scale_fill_manual(values = EBpalette, breaks=c('Low', 'Medium', 'High')) + 
#   xlab("Area Type") + ylab("Percent Damage")

ggplot(data=subset(damage, Area_type!='Forest'), aes(x=Area_type, y=Percent_damage, fill=Area_type, color=Income)) + 
  scale_fill_manual(values = EBpalette) + 
  scale_color_manual(values = EBpalette) + 
  xlab("Land Use Type") + ylab("Leaf Damage (%)") +
  geom_violin(aes(fill=Income, color=Income,
                  fill=after_scale(colorspace::lighten(fill, .3))),
              size=1, bw=.3) +
  scale_x_discrete(limits=c('Urban', 'Suburban ')) +
  geom_boxplot(width=0.1, size=1, outlier.size=3, fill='white', position=position_dodge(0.9)) +
  annotate("text", x = 1.7, y = -2, label = "c", size = 8) + #probability zero damage
  annotate("text", x = 2.0, y = -2, label = "bc", size = 8) +
  annotate("text", x = 2.3, y = -2, label = "c", size = 8) +
  annotate("text", x = 0.7, y = -2, label = "a", size = 8) +
  annotate("text", x = 1.0, y = -2, label = "b", size = 8) +
  annotate("text", x = 1.3, y = -2, label = "b", size = 8) +
  annotate("text", x = 1.7, y = 50, label = "A", size = 8) + #average of non-zero data
  annotate("text", x = 2.0, y = 41, label = "A", size = 8) +
  annotate("text", x = 2.3, y = 67, label = "B", size = 8) +
  annotate("text", x = 0.7, y = 55, label = "B", size = 8) +
  annotate("text", x = 1.0, y = 19, label = "A", size = 8) +
  annotate("text", x = 1.3, y = 36, label = "A", size = 8)
#export at 900x600




#### Effect of land use on herbivore and predator abundance (suburban, urban) ####

## Herbivore Stats ##
summary(herbivoreModel <- glmmTMB(herbivore ~ Area_type*Income + (1|Tree_num), 
                        ziformula = ~ 1,  # Specify the zero-inflation formula
                        data = subset(damage, Area_type!='Forest'), 
                        family = nbinom2))
Anova(herbivoreModel, type='III')
# Area_type        269.490  1  < 2.2e-16 ***
# Income           679.353  2  < 2.2e-16 ***
# Area_type:Income 148.500  2  < 2.2e-16 ***
emmeans(herbivoreModel, ~Area_type*Income)

## Herbivore Figure ##

a <- ggplot(data=subset(damage, Area_type!='Forest'), aes(x=Area_type, y=herbivore, fill=Income)) + 
  scale_fill_manual(values = EBpalette) + 
  xlab("Land Use Type") + ylab("Herbivore Abundance") +
  geom_boxplot(width=0.8, size=1, outlier.size=3, position=position_dodge(0.9), color='honeydew4') +
  theme(legend.position='none') +
  scale_x_discrete(limits=c('Urban', 'Suburban ')) +
  annotate("text", x = 1.7, y = 6, label = "a", size = 8) +
  annotate("text", x = 2, y = 13, label = "c", size = 8) +
  annotate("text", x = 2.3, y = 26, label = "c", size = 8) +
  annotate("text", x = 0.7, y = 6, label = "a", size = 8) +
  annotate("text", x = 1, y = 8, label = "b", size = 8) +
  annotate("text", x = 1.3, y = 11, label = "b", size = 8)
  


## Predator Stats ##
summary(predatorModel <- glmmTMB(predator ~ Area_type*Income + (1|Tree_num), 
                                ziformula = ~ 1,  # Specify the zero-inflation formula
                                data = subset(damage, Area_type!='Forest'), 
                                family = nbinom2))
Anova(predatorModel, type='III')
# Area_type        339.412  1  < 2.2e-16 ***
# Income           315.881  2  < 2.2e-16 ***
# Area_type:Income 124.686  2  < 2.2e-16 ***
emmeans(predatorModel, ~Area_type*Income)

## Predator Figure ##

b <- ggplot(data=subset(damage, Area_type!='Forest'), aes(x=Area_type, y=predator, fill=Income)) + 
  scale_fill_manual(values = EBpalette) + 
  xlab("Land Use Type") + ylab("Predator Abundance") +
  geom_boxplot(width=0.8, size=1, outlier.size=3, position=position_dodge(0.9), color='honeydew4') +
  theme(legend.position=c(0.25,0.85)) +
  scale_x_discrete(limits=c('Urban', 'Suburban ')) +
  annotate("text", x = 1.7, y = 9, label = "b", size = 8) +
  annotate("text", x = 2, y = 9, label = "b", size = 8) +
  annotate("text", x = 2.3, y = 17, label = "c", size = 8) +
  annotate("text", x = 0.7, y = 4, label = "a", size = 8) +
  annotate("text", x = 1, y = 6, label = "b", size = 8) +
  annotate("text", x = 1.3, y = 7, label = "b", size = 8)

grid.arrange(a, b, ncol=2)
#export at 1200x600



#### Exploring correlations between damage and bug data ####
ggplot(data=subset(damage, Area_type!='Forest'), aes(x=herbivore, y=Percent_damage, color=Income, shape=Area_type)) +
  geom_point() + geom_smooth(method='lm', se=F)

ggplot(data=subset(damage, Area_type!='Forest'), aes(x=herbivore, y=predator, color=Income, shape=Area_type)) +
  geom_point() + geom_smooth(method='lm', se=F)