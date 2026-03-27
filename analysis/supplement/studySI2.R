library(tidyverse)
library(car)
library(lme4)
library(jtools)
library(partR2)
library(patchwork)

source("../utils.R")

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

##### standardize everything #####
Datas2 <- Datas2 %>%
  mutate(
    Curiosity_z = scale(Curiosity),
    RPE_MAP_abs_z = scale(RPE_MAP_abs),
    Entropy_theta_z = scale(Entropy_theta),
    Entropy_z_z = scale(Entropy_z),
    EIG_theta_z = scale(EIG_theta)
  )

Datas2kids <- Datas2kids %>%
  mutate(
    Curiosity_z = scale(Curiosity),
    RPE_MAP_abs_z = scale(RPE_MAP_abs),
    Entropy_theta_z = scale(Entropy_theta),
    Entropy_z_z = scale(Entropy_z),
    EIG_theta_z = scale(EIG_theta)
  )

Datas2adults <- Datas2adults %>%
  mutate(
    Curiosity_z = scale(Curiosity),
    RPE_MAP_abs_z = scale(RPE_MAP_abs),
    Entropy_theta_z = scale(Entropy_theta),
    Entropy_z_z = scale(Entropy_z),
    EIG_theta_z = scale(EIG_theta)
  )

######### Main analyses: multiple regression #########


##### kids vs. adults
m1 <- lmer(Curiosity_z ~ (RPE_MAP_abs_z + Entropy_theta_z + 
                                 Entropy_z_z + EIG_theta_z)*AgeGroup + 
             (RPE_MAP_abs_z + Entropy_theta_z + 
                Entropy_z_z + EIG_theta_z||oid) ,
           data = Datas2, control = lmerControl(optimizer = "bobyqa"))
summary(m1)

round(Confint(m1), 2)
drop1(m1, test = "Chisq")


#### follow-up - within kids 

m1a <- lmer(Curiosity_z ~ (RPE_MAP_abs_z + Entropy_theta_z + 
                                  Entropy_z_z + EIG_theta_z) + 
              (RPE_MAP_abs_z + Entropy_theta_z + 
                 Entropy_z_z + EIG_theta_z||oid) ,
            data = Datas2kids, control = lmerControl(optimizer = "bobyqa"))
summary(m1a)

round(Confint(m1a), 3)
drop1(m1a, test = "Chisq")

m1a.2 <- lmer(Curiosity_z ~ (RPE_MAP_abs_z + Entropy_theta_z + 
                                    Entropy_z_z + EIG_theta_z) + 
                (1|oid) ,
              data = Datas2kids, control = lmerControl(optimizer = "bobyqa"))

m1a_partr2 <- partR2(m1a.2,
                     partvars = c("RPE_MAP_abs_z",
                                  "Entropy_theta_z",
                                  "Entropy_z_z",
                                  "EIG_theta_z"),
                     data = Datas2kids, 
                     max_level = 2)
m1a_partr2


#### follow-up - within adults
m1b <- lmer(Curiosity_z ~ (RPE_MAP_abs_z + Entropy_theta_z + 
                                  Entropy_z_z + EIG_theta_z) + 
              (RPE_MAP_abs_z + Entropy_theta_z + 
                 Entropy_z_z + EIG_theta_z||oid) ,
            data = Datas2adults, control = lmerControl(optimizer = "bobyqa"))
summary(m1b)

round(Confint(m1b), 2)
drop1(m1b, test = "Chisq")


m1b.2 <- lmer(Curiosity_z ~ (RPE_MAP_abs_z + Entropy_theta_z + 
                                    Entropy_z_z + EIG_theta_z) + 
                (1|oid) ,
              data = Datas2adults, control = lmerControl(optimizer = "bobyqa"))

m1b_partr2 <- partR2(m1b.2,
                     partvars = c("RPE_MAP_abs_z",
                                  "Entropy_theta_z",
                                  "Entropy_z_z",
                                  "EIG_theta_z"),
                     data = Datas2adults, 
                     max_level = 2)
m1b_partr2


regcoefs <- plot_summs(m1b, m1a,
                 model.names = c("Adults", "Children"), 
                 legend.title = "Age Group", coefs = c("Local\nLearning Potential" = "Entropy_z_z",
                                                       "Global\nLearning Potential" = "EIG_theta_z",
                                                       "Global Uncertainty" = "Entropy_theta_z",
                                                       "Surprise" = "RPE_MAP_abs_z"),
                 colors = c( "#999999", "#e5d200"),
                 point.size = 4)+ 
  theme_classic(base_size = 8) + ylab("Predictor") + 
  xlab(expression("Standardized coefficient ("~ beta~")")) + 
  theme(legend.position = "top")
regcoefs

kids_partr2 <- m1a_partr2$R2
adults_partr2 <- m1b_partr2$R2

p1 <- plot_partr2(kids_partr2) + ggtitle("Children")
p2 <- plot_partr2(adults_partr2) + ggtitle("Adults")

combined_fig_s2 <- regcoefs / (p2 | p1 ) + plot_annotation(tag_levels = 'a')& 
  theme(plot.tag = element_text(face = "bold", size = 10))
combined_fig_s2

ggsave("../../figures/StudySI2_Results.pdf", combined_fig_s2, height = 5, width = 6.5, units = "in")




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
Confint(m2)

