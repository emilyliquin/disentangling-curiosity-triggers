library(tidyverse)
library(car)
library(lme4)
library(jtools)


####### load STUDY 3 KID DATA #####

Datas2kids <- read_csv("../../model_results/StudySI2_kids.csv")


##### remove excluded participants

# same curioisty rating on all trials
to_exclude <- Datas2kids %>% group_by(oid) %>%
  summarize(same_rating = length(unique(Curiosity)) == 1) %>%
  filter(same_rating == TRUE)

Datas2kids <- Datas2kids %>% filter(!(oid %in% to_exclude$oid))


# surprise = unsigned reward prediction error (absolute value)
Datas2kids$RPE_MAP_abs <- abs(Datas2kids$RPE_MAP)

# on first trial, RPE_MAP_abs manually set to 0.5 
Datas2kids$RPE_MAP_abs <- ifelse(is.nan(Datas2kids$RPE_MAP_abs), 0.5, Datas2kids$RPE_MAP_abs)


##### other data cleaning 
Datas2kids <- Datas2kids %>% 
  mutate(oid = as.factor(oid),
         feedback = as.factor(feedback),
         subj_choice = as.factor(subj_choice))


####### load STUDY 3 ADULT DATA #####

Datas2adults <- read_csv("../../model_results/StudySI2_adults.csv")

##### remove excluded participants

# incorrect answers to attn checks
Datas2adults$Include <- ifelse(Datas2adults$Attn1 == "A squirrel" & Datas2adults$Attn2 == "I am a slightly curious person", 1, 0)
Datas2adults <- subset(Datas2adults, Datas2adults$Include == 1)

# same curioisty rating on all trials
to_exclude <- Datas2adults %>% group_by(oid) %>%
  summarize(same_rating = length(unique(Curiosity)) == 1) %>%
  filter(same_rating == TRUE)

Datas2adults <- Datas2adults %>% filter(!(oid %in% to_exclude$oid))


# surprise = unsigned reward prediction error (absolute value)
Datas2adults$RPE_MAP_abs <- abs(Datas2adults$RPE_MAP)

# on first trial, RPE_MAP_abs manually set to 0.5 
Datas2adults$RPE_MAP_abs <- ifelse(is.nan(Datas2adults$RPE_MAP_abs), 0.5, Datas2adults$RPE_MAP_abs)


##### other data cleaning 
Datas2adults <- Datas2adults %>% 
  mutate(oid = as.factor(oid),
         feedback = as.factor(feedback),
         subj_choice = as.factor(subj_choice))


########## Merge kid and adults data #######


Datas2kids$AgeGroup <- "Children"
Datas2adults$AgeGroup <- "Adults"
Datas2adults$AgeYr <- NA
Datas2adults$AgeMonth <- NA

Kcols <- colnames(Datas2kids)
Acols <- colnames(Datas2adults)

intercols <- intersect(Kcols, Acols)

# make sure IDs are distinct
Datas2adults$oid <- as.factor(as.numeric(as.character(Datas2adults$oid)) + 1000)

Datas2 <- rbind(Datas2kids[,intercols], Datas2adults[,intercols])


# how many participants
table(Datas2kids$AgeYr)/20
length(unique(Datas2kids$oid))

length(unique(Datas2adults$oid))


######### Main analyses: multiple regression #########


##### kids vs. adults
m1 <- lmer(scale(Curiosity) ~ (scale(RPE_MAP_abs) + scale(Entropy_theta) + 
                                 scale(Entropy_z) + scale(EIG_theta))*AgeGroup + 
             (scale(RPE_MAP_abs) + scale(Entropy_theta) + 
                scale(Entropy_z) + scale(EIG_theta)||oid) ,
           data = Datas2, control = lmerControl(optimizer = "bobyqa"))
summary(m1)

round(Confint(m1), 2)
drop1(m1, test = "Chisq")


#### follow-up - within kids 

m1a <- lmer(scale(Curiosity) ~ (scale(RPE_MAP_abs) + scale(Entropy_theta) + 
                                  scale(Entropy_z) + scale(EIG_theta)) + 
              (scale(RPE_MAP_abs) + scale(Entropy_theta) + 
                 scale(Entropy_z) + scale(EIG_theta)||oid) ,
            data = Datas2kids, control = lmerControl(optimizer = "bobyqa"))
summary(m1a)

round(Confint(m1a), 2)
drop1(m1a, test = "Chisq")


#### follow-up - within adults
m1b <- lmer(scale(Curiosity) ~ (scale(RPE_MAP_abs) + scale(Entropy_theta) + 
                                  scale(Entropy_z) + scale(EIG_theta)) + 
              (scale(RPE_MAP_abs) + scale(Entropy_theta) + 
                 scale(Entropy_z) + scale(EIG_theta)||oid) ,
            data = Datas2adults, control = lmerControl(optimizer = "bobyqa"))
summary(m1b)

round(Confint(m1b), 2)
drop1(m1b, test = "Chisq")


p2 <- plot_summs(m1a, m1b, 
                 model.names = c("Children", "Adults"), 
                 legend.title = "Age Group", coefs = c("Surprise" = "scale(RPE_MAP_abs)",
                                                       "Global Uncertainty" = "scale(Entropy_theta)",
                                                       "Global\nLearning Potential" = "scale(EIG_theta)",
                                                       "Local\nLearning Potential" = "scale(Entropy_z)"
                 ),
                 colors = c("#e5d200", "#999999"))+ 
  theme_classic(base_size = 12) + ylab("Model-Estimated Trigger") + 
  xlab("Standardized Coefficient") +
  theme(legend.position = "top")
p2

setwd("/Users/emilyliquin/Documents/00_Work/Research/03_ESC_RL/ESC_RL/Figures")
ggsave("SupplementFig4.pdf", p2, height = 3, width = 4, units = "in")




##### learning #####


Data_by_part <- Datas2 %>% group_by(oid, AgeGroup) %>%
  summarize(esthigh = EstHigh[1],
            estlow = EstLow[1])

Estimates <- Data_by_part %>% pivot_longer(c(esthigh, estlow),
                                    values_to = "Estimate",
                                    names_to = "Machine")

Estimates$Machine <- as.factor(Estimates$Machine)


Estimates %>% group_by(AgeGroup, Machine) %>%
  summarize(est = mean(Estimate),
            sd = sd(Estimate))


# just children
m2a <- lmer(Estimate ~ Machine + (1|oid), data = subset(Estimates, Estimates$AgeGroup == "Children"))
summary(m2a)
drop1(m2a, test = "Chisq")

# just adults
m2b <- lmer(Estimate ~ Machine + (1|oid), data = subset(Estimates, Estimates$AgeGroup == "Adults"))
summary(m2b)
drop1(m2b, test = "Chisq")

### interaction with age group
m2 <- lmer(Estimate ~ Machine*AgeGroup + (1|oid), data = Estimates)
summary(m2)
drop1(m2, test = "Chisq")


