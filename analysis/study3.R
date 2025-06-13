library(tidyverse)
library(car)
library(lme4)

####### load STUDY 3 KID DATA #####

Data3kids <- read_csv("../data/study3_kids.csv")

##### remove excluded participants

# incorrect answers to attn checks
Data3kids <- Data3kids %>% filter(Include == 1)

# same curioisty rating on all trials
to_exclude <- Data3kids %>% group_by(oid) %>%
  summarize(same_rating = length(unique(Curiosity)) == 1) %>%
  filter(same_rating == TRUE)

Data3kids <- Data3kids %>% filter(!(oid %in% to_exclude$oid))


##### add model estimates
model_s3kids <- read.csv("../model_results/CMC2BW_Kid-Final_Clean_Output.csv")


colnames(model_s3kids)

selected <- c("oid", "trial_num", "Mean_theta",
              "RPE_MAP", "Entropy_z", "Entropy_theta", "EIG_theta")

Data3kids <- merge(Data3kids, model_s3kids[selected], by = c("oid", "trial_num"))


# surprise = unsigned reward prediction error (absolute value)
Data3kids$RPE_MAP_abs <- abs(Data3kids$RPE_MAP)

# on first trial, RPE_MAP_abs manually set to 0.5 
Data3kids$RPE_MAP_abs <- ifelse(is.nan(Data3kids$RPE_MAP_abs), 0.5, Data3kids$RPE_MAP_abs)


##### other data cleaning 
Data3kids <- Data3kids %>% 
  mutate(oid = as.factor(oid),
         feedback = as.factor(feedback),
         subj_choice = as.factor(subj_choice))

####### load STUDY 3 ADULT DATA #####

Data3adults <- read_csv("../data/study3_adults.csv")

##### remove excluded participants

# incorrect answers to attn checks
Data3adults <- Data3adults %>% filter(Include == 1)

# same curioisty rating on all trials
to_exclude <- Data3adults %>% group_by(oid) %>%
  summarize(same_rating = length(unique(Curiosity)) == 1) %>%
  filter(same_rating == TRUE)

Data3adults <- Data3adults %>% filter(!(oid %in% to_exclude$oid))


##### add model estimates
model_s3adults <- read.csv("../model_results/CMC2BW_Adult-Final_Clean_Output.csv")


colnames(model_s3adults)

selected <- c("oid", "trial_num", "Mean_theta",
              "RPE_MAP", "Entropy_z", "Entropy_theta", "EIG_theta")

Data3adults <- merge(Data3adults, model_s3adults[selected], by = c("oid", "trial_num"))


# surprise = unsigned reward prediction error (absolute value)
Data3adults$RPE_MAP_abs <- abs(Data3adults$RPE_MAP)

# on first trial, RPE_MAP_abs manually set to 0.5 
Data3adults$RPE_MAP_abs <- ifelse(is.nan(Data3adults$RPE_MAP_abs), 0.5, Data3adults$RPE_MAP_abs)


##### other data cleaning 
Data3adults <- Data3adults %>% 
  mutate(oid = as.factor(oid),
         feedback = as.factor(feedback),
         subj_choice = as.factor(subj_choice))


########## Merge kid and adults data #######


Data3kids$AgeGroup <- "Children"
Data3adults$AgeGroup <- "Adults"
Data3adults$AgeYr <- "Adult"
Data3adults$AgeMonth <- NA

Kcols <- colnames(Data3kids)
Acols <- colnames(Data3adults)

intercols <- intersect(Kcols, Acols)

# make sure IDs are distinct
Data3adults$oid <- as.factor(as.numeric(as.character(Data3adults$oid)) + 1000)

Data3 <- rbind(Data3kids[,intercols], Data3adults[,intercols])


# how many participants
table(Data3kids$AgeYr)/16
length(unique(Data3kids$oid))

length(unique(Data3adults$oid))



######### Main analyses: multiple regression #########


##### kids vs. adults
m1 <- lmer(scale(Curiosity) ~ (scale(RPE_MAP_abs) + scale(Entropy_theta) + 
                                 scale(Entropy_z) + scale(EIG_theta))*AgeGroup + 
             (scale(RPE_MAP_abs) + scale(Entropy_theta) + 
                scale(Entropy_z) + scale(EIG_theta)||oid) ,
           data = Data3, control = lmerControl(optimizer = "bobyqa"))
summary(m1)

round(Confint(m1), 2)
drop1(m1, test = "Chisq")


#### follow-up - within kids 

m1a <- lmer(scale(Curiosity) ~ (scale(RPE_MAP_abs) + scale(Entropy_theta) + 
                                 scale(Entropy_z) + scale(EIG_theta)) + 
             (scale(RPE_MAP_abs) + scale(Entropy_theta) + 
                scale(Entropy_z) + scale(EIG_theta)||oid) ,
           data = Data3kids, control = lmerControl(optimizer = "bobyqa"))
summary(m1a)

round(Confint(m1a), 2)
drop1(m1a, test = "Chisq")


#### follow-up - within adults
m1b <- lmer(scale(Curiosity) ~ (scale(RPE_MAP_abs) + scale(Entropy_theta) + 
                                  scale(Entropy_z) + scale(EIG_theta)) + 
              (scale(RPE_MAP_abs) + scale(Entropy_theta) + 
                 scale(Entropy_z) + scale(EIG_theta)||oid) ,
            data = Data3adults, control = lmerControl(optimizer = "bobyqa"))
summary(m1b)

round(Confint(m1b), 2)
drop1(m1b, test = "Chisq")



##### within kids

# pre-scale everything because it gets weird with emmeans
Data3kids <- Data3kids %>% mutate(Curiosity_s = as.vector(scale(Curiosity)),
                                  RPE_MAP_abs_s = as.vector(scale(RPE_MAP_abs)),
                                  Entropy_theta_s = as.vector(scale(Entropy_theta)),
                                  Entropy_z_s = as.vector(scale(Entropy_z)),
                                  EIG_theta_s = as.vector(scale(EIG_theta)))

m2 <- lmer(Curiosity_s ~ (RPE_MAP_abs_s + Entropy_theta_s + 
                            Entropy_z_s + EIG_theta_s)*AgeMonth + 
             (RPE_MAP_abs_s + Entropy_theta_s + 
                Entropy_z_s + EIG_theta_s||oid) ,
           data = Data3kids, control = lmerControl(optimizer = "bobyqa"))
summary(m2)

round(Confint(m2), 2)
drop1(m2, test = "Chisq")


## predicted effect at different ages
library(emmeans)
emtrends(m2, ~ AgeMonth, var = "Entropy_z_s",
         at = list(AgeMonth = c(60, 108)))


##### SI: alternative explanations ######

######## 1. Validate measure of curiosity ####


Data3$ed_curiosity_diff <- Data3$echidna_curiosity - Data3$dog_curiosity
Data3$animal_choice <- as.factor(Data3$animal_choice)

Data3kids$ed_curiosity_diff <- Data3kids$echidna_curiosity - Data3kids$dog_curiosity
Data3kids$animal_choice <- as.factor(Data3kids$animal_choice)

df_full <- Data3 %>% group_by(oid, AgeGroup) %>%
  summarize(ed_curiosity_diff = ed_curiosity_diff[1],
            animal_choice = animal_choice[1])

df_k <- Data3kids %>% group_by(oid, AgeMonth) %>%
  summarize(ed_curiosity_diff = ed_curiosity_diff[1],
            animal_choice = animal_choice[1])

#within kids
m3a <- glm(animal_choice ~ ed_curiosity_diff, data = df_full %>% filter(AgeGroup == "Children"), family = "binomial")
summary(m3a)
exp(Confint(m3a))
drop1(m3a, test = "Chisq")

# within adults
m3b <- glm(animal_choice ~ ed_curiosity_diff, data = df_full %>% filter(AgeGroup == "Adults"), family = "binomial")
summary(m3b)
exp(Confint(m3b))
drop1(m3b, test = "Chisq")


#interaction with age group
m3 <- glm(animal_choice ~ ed_curiosity_diff*AgeGroup, data = df_full, family = "binomial")
summary(m3)
drop1(m3, test = "Chisq")



#interaction with age (years)
m4 <- glm(animal_choice ~ ed_curiosity_diff*AgeMonth,
          data = df_k, family = "binomial")
summary(m4)

drop1(m4, test = "Chisq")



######## 1. Control for differences in learning ####

# rescale guess to proportion of 10 berries
Data3$Theta_guess <- Data3$Theta_guess/10 

Data3$AbsGuessError <- abs(Data3$Theta_guess - Data3$Mean_theta)
Data3$GuessError <- Data3$Theta_guess - Data3$Mean_theta

# get by-participant average absolute guess error (learning noise) and guess error (learning error)
means <- Data3 %>% group_by(oid, AgeGroup) %>%
  summarize(Av_abs_guess_error = mean(AbsGuessError, na.rm = TRUE),
            Av_guess_error = mean(GuessError, na.rm = TRUE))
means
Data3 <- merge(Data3, means)


#### learning error: 
t.test(means$Av_guess_error)
# on average, people make guesses lower than 
# the true value (aren't taking into account the intervention enough, 
# thus overestimating the number of yellow berries)


# difference between ages -- NO DIFFERENCE
t.test(Av_guess_error ~ AgeGroup, data = means)

#### learning noise: 
t.test(means$Av_abs_guess_error)
# on average, people are off from the true value


# difference between ages -- SIG DIFFERENCE
t.test(Av_abs_guess_error ~ AgeGroup, data = means, var.equal = TRUE)


##### control for learning noise in analysis of triggers


m <- lmer(scale(Curiosity) ~ 
            (scale(RPE_MAP_abs) + 
               scale(Entropy_theta) + 
               scale(Entropy_z) + 
               scale(EIG_theta))*AgeGroup + 
            (scale(RPE_MAP_abs) + 
               scale(Entropy_theta) + 
               scale(Entropy_z) + 
               scale(EIG_theta))*Av_abs_guess_error + 
            (scale(RPE_MAP_abs) + 
               scale(Entropy_theta) + 
               scale(Entropy_z) + 
               scale(EIG_theta)||oid), data = Data3,
          control = lmerControl(optimizer= "bobyqa"))
summary(m)
drop1(m, test = "Chisq")

sjPlot::plot_model(m, terms = c("RPE_MAP_abs", "AgeGroup", "Av_abs_guess_error"), type = "pred")
sjPlot::plot_model(m, terms = c("Entropy_theta", "AgeGroup", "Av_abs_guess_error"), type = "pred")
sjPlot::plot_model(m, terms = c("Entropy_z", "AgeGroup", "Av_abs_guess_error"), type = "pred")
sjPlot::plot_model(m, terms = c("EIG_theta", "AgeGroup", "Av_abs_guess_error"), type = "pred")


sjPlot::plot_model(m, terms = c("RPE_MAP_abs", "Av_abs_guess_error"), type = "pred")
sjPlot::plot_model(m, terms = c("Entropy_theta", "Av_abs_guess_error"), type = "pred")
sjPlot::plot_model(m, terms = c("Entropy_z", "Av_abs_guess_error"), type = "pred")
sjPlot::plot_model(m, terms = c("EIG_theta", "Av_abs_guess_error"), type = "pred")


sjPlot::plot_model(m, terms = c("Entropy_z", "AgeGroup", "Av_abs_guess_error"), 
                   type = "pred") + 
  theme_classic(base_size = 10) + 
  scale_color_manual(values = c("#e5d200", "#999999")) +
  scale_fill_manual(values = c("#e5d200", "#999999")) 



