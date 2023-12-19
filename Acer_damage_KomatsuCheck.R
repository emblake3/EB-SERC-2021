# library(rcompanion)
library(car)
# library(lattice)
# library(effects)
library(MASS)
library(lme4)
library(emmeans)
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

#Part 1: probability of zero damage
summary(zero_model <- glmer(Percent_damage_zeroindicator ~ Area_type + (1|Tree_num),
                            data = damage,
                            family = binomial))
Anova(zero_model) # Area_type 227.23  2  < 2.2e-16 ***
emmeans(zero_model, ~Area_type)

summary(cont_model <- lmer(log(Percent_damage) ~ Area_type + (1|Tree_num),
                            data = subset(damage, Percent_damage_zeroindicator != 0)))
Anova(cont_model) #Area_type 8.0695  2    0.01769 *
emmeans(cont_model, ~Area_type)


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

ggplot(data=damage, aes(x=Area_type, y=Percent_damage, fill=Area_type)) + 
  geom_violin(size = 1.2, bw = .1) +
  geom_boxplot(fill='white', width=0.05, outlier.size=3) +
  scale_fill_manual(values = EBpalette1) + 
  xlab("Land Use Type") + ylab("Leaf Damage (%)") +
  theme(legend.position='none') 

ggplot(data=damage, aes(x=Area_type, y=Percent_damage, fill=Area_type, color=Area_type)) + 
  scale_fill_manual(values = EBpalette1) + 
  scale_color_manual(values = EBpalette1) + 
  xlab("Land Use Type") + ylab("Leaf Damage (%)") +
  theme(legend.position='none') + 
  geom_violin(aes(fill=Area_type, color=Area_type,
                  fill=after_scale(colorspace::lighten(fill, .3))),
              size=1, bw=.3) +
  geom_boxplot(width=0.1, size=1, outlier.size=3, fill='white')
#export at 600x600

#### Effect of land use and income on leaf damage (suburban, urban) ####

## Stats ##


## Figure ##
damage$Income <- factor(damage$Income, levels=c("Low", "Medium", "High", "None"))
EBpalette <- c("#FFCB05" ,"#00B2A9", "#00274C", "black")

ggplot(data = barGraphStats(data = subset(damage, !is.na(Percent_damage) & Area_type!='Forest'), variable = "Percent_damage", byFactorNames = c("Area_type", "Income")), aes(x= Area_type, y=mean, fill=Income)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position = position_dodge(0.9)) +
  annotate("text", x = .7, y = 3.2, label = "a", size = 8)+
  annotate("text", x = 1, y = 1.8, label = "b", size = 8)+
  annotate("text", x = 1.3, y = 4, label = "a", size = 8) +
  annotate("text", x = 1.7, y = 3.7, label = "a", size = 8) +
  annotate("text", x = 2, y = 1.2, label = "b", size = 8) +
  annotate("text", x = 2.3, y = 1.5, label = "b", size = 8) +
  scale_fill_manual(values = EBpalette, breaks=c('Low', 'Medium', 'High')) + 
  xlab("Area Type") + ylab("Percent Damage")


#### Effect of land use on herbivore and predator abundance (suburban, urban) ####

## Stats ##
landUseModel <- glmmTMB(Percent_damage ~ Area_type + (1|Tree_num), 
                        ziformula = ~ 1,  # Specify the zero-inflation formula
                        data = damage, 
                        family = nbinom2)

## Figures ##
# #herbivores without income
# ggplot(data = barGraphStats(data = subset(damage, Area_type!="Forest"), variable = "herbivore", byFactorNames = c("Area_type")), aes(x=Area_type, y=mean, fill=Area_type)) +
#   geom_bar(stat='identity', position=position_dodge()) +
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position = position_dodge(0.9)) +
#   scale_fill_manual(values = EBpalettewoforest) + xlab("Area Type") + ylab("Herbivore Abundance") + theme(legend.position = "none") +
#   annotate("text", x = 1, y = 7.3, label = "*", size = 8) + 
#   theme(legend.position = "none")

#herbivores with income
ggplot(data = barGraphStats(data = subset(damage, Area_type!= "Forest"), variable = "herbivore", byFactorNames = c("Area_type","Income")), aes(x=Area_type, y=mean, fill=Income)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position = position_dodge(0.9)) +
  annotate("text", x = .7, y = 2.5, label = "a", size = 8)+
  annotate("text", x = 1, y = 8.25, label = "b", size = 8)+
  annotate("text", x = 1.3, y = 12.5, label = "c", size = 8) +
  annotate("text", x = 1.7, y = 2.75, label = "a", size = 8) +
  annotate("text", x = 2, y = 4, label = "d", size = 8) +
  annotate("text", x = 2.3, y = 4.5, label = "d", size = 8) +
  scale_fill_manual(values = EBpalette, breaks=c('Low', 'Medium', 'High')) + 
  xlab("Area Type") + ylab("Herbivore Abundance") 


# #predators without income
# ggplot(data = barGraphStats(data = subset(damage, Area_type!="Forest"), variable = "predator", byFactorNames = c("Area_type")), aes(x=Area_type, y=mean, fill=Area_type)) +
#   geom_bar(stat='identity', position=position_dodge()) +
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position = position_dodge(0.9)) +
#   scale_fill_manual(values = EBpalettewoforest) + xlab("Area Type") + ylab("Predator Abundance") + theme(legend.position = "none") +
#   annotate("text", x = 1, y = 5.3, label = "*", size = 8) + 
#   theme(legend.position = "none")

#predators with income
ggplot(data = barGraphStats(data = subset(damage, Area_type!= "Forest"), variable = "predator", byFactorNames = c("Area_type","Income")), aes(x=Area_type, y=mean, fill=Income)) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.2, position = position_dodge(0.9)) +
  annotate("text", x = .7, y = 4.5, label = "a", size = 8)+
  annotate("text", x = 1, y = 3.74, label = "b", size = 8)+
  annotate("text", x = 1.3, y = 9.75, label = "c", size = 8) +
  annotate("text", x = 1.7, y = 1.75, label = "d", size = 8) +
  annotate("text", x = 2, y = 3, label = "b", size = 8) +
  annotate("text", x = 2.3, y = 3.5, label = "b", size = 8) +
  scale_fill_manual(values = EBpalette, breaks=c('Low', 'Medium', 'High')) + 
  xlab("Area Type") + ylab("Predator Abundance")



#### Relationship between herbivore abundance and leaf damage (suburban, urban) ####


## Stats ##


## Figure ##




## Percent damage vs. Area and Income Level ##




#####mod1 <- lme(Percent_damage ~ Area_type, data = damage2, random = 1|Area_type/Tree_num)


#mod0 <- lm(Percent_damage ~ Area_type, data = damage2)

#mod1 <- lme(Percent_damage ~Area_type, random = ~1|Area_type/Tree_num, data = damage2)

mod2 <- lme(Percent_damage ~ Area_type, random = ~1|Tree_num, data = damage2)

mod3 <- lme(Percent_damage ~ Area_type*Income, random = ~1|Tree_num, data = damage3)

anova(mod2) ##Fstat and p-value for mod2##
anova(mod3) ##Fstat and p-value for mod3##

#posthoc test for pairwise comparison of means##
emmeans(mod2, list(pairwise ~ Area_type), adjust = "tukey")
emmeans(mod3, list(pairwise ~ Area_type*Income), adjust = "tukey")


```
```{r}
#Anovas for Bugs by Area 

herbanova <- aov(herbivore ~ Area_type, data = damage2)
summary(herbanova)

predanova <- aov(predator ~ Area_type, data = damage2)
summary(predanova)

anova3 <- aov(abundance ~ Area_type * insectgroup, data = damage5)

anova3tukey <- TukeyHSD(anova3)
anova3tukey

herbtukey <- TukeyHSD(herbanova)
herbtukey
```


Model effect of area on herbivores and predators
```{r}
library(car)

#Use generalized linear models with poisson distribution because it is count data ##

#Area 
herbivore_area <- glm(herbivore ~ Area_type, family = poisson, data = damage2)
Anova(herbivore_area)

predator_area <- glm(predator ~ Area_type, family = poisson, data = damage2)
Anova(predator_area)

#Income
herbivore_income <- glm(herbivore ~ Income, family = poisson, data = damage4)
Anova(herbivore_income)

predator_income <- glm(predator ~ Income, family = poisson, data = damage4)
Anova(predator_income)


predator_areain <- glm(predator ~ Area_type * Income, family = poisson, data = subset(damage2, Area_type!= "Forest"))
Anova(predator_areain)

herbivore_areain <- glm(herbivore ~ Area_type * Income, family = poisson, data = subset(damage2, Area_type!= "Forest"))
Anova(herbivore_areain)

emmeans(herbivore_areain, list(pairwise ~ Area_type*Income), adjust = "tukey")

emmeans(predator_areain, list(pairwise ~ Area_type*Income), adjust = "tukey")


```


```{r}
##Effects of Herbivores on damage##

herbivore_glm <- lm(Percent_damage ~ herbivore*Area_type, data = damage2)
summary(herbivore_lm)

predictedherbivore <- predict(herbivore_lm, interval = "confidence", level = 0.95)

EBpalette2 <- c("#DC267F", "#785EF0", "#702082")
EBpalette3 <- c("#117733", "#882255")

##Scatterplot of HERB by Area without forest##
g69 <- ggplot(damage3, aes(x = herbivore, y = Percent_damage, color = Area_type)) + 
  geom_point(shape = 1) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE) +
  xlab("Number of Herbivores") + ylab("Percent Damage") + scale_color_manual(values = EBpalette2) +  theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 24),axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))

g69
```


```{r}
##Scatterplot of PRED by Area##
g420 <- ggplot(damage3, aes(x = predator, y = Percent_damage, color = Area_type)) + 
  geom_point(shape = 1) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE) +
  xlab("Number of Predator") + ylab("Percent Damage") + scale_color_manual(values = EBpalette2) +  theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 24),axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))
```


```{r}
##Scatterplot of PRED by Herb##
g333 <- ggplot(damage3, aes(x = herbivore, y = predator, color = Area_type)) + 
  geom_point(shape = 1) +
  scale_colour_hue(l=50) +
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE) +
  xlab("Number of Herbivore") + ylab("Number of Predator") + scale_color_manual(values = EBpalette2) +  theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size = 24),axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)), axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))

```


```

