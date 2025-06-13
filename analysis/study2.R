library(tidyverse)
library(car)
library(lme4)

###### LOAD STUDY 2A DATA #####

Data2a <- read_csv("../data/study2a.csv")

##### remove excluded participants

# incorrect answers to attn checks
Data2a <- Data2a %>% filter(Include == 1)

# same curioisty rating on all trials
to_exclude <- Data2a %>% group_by(oid) %>%
  summarize(same_rating = length(unique(curiosity)) == 1) %>%
  filter(same_rating == TRUE)

Data2a <- Data2a %>% filter(!(oid %in% to_exclude$oid))


##### add model estimates
Sequences <- read.csv("../model_results/dorfman_exp1.csv")

# renumber subjects from 1 to n
Sequences$subject <- as.factor(as.numeric(as.factor(Sequences$subject)))

# match col names from data
Sequences$agent_condition <- ifelse(Sequences$condition == 1, "bandit",
                                    ifelse(Sequences$condition == 2, "tycoon", "sheriff"))
Sequences$yoked_participant <- Sequences$subject


selected <- c("chosen_mine_prob", "latent_agent", 
              "RPE_MAP", "Entropy_z", "Entropy_theta", "EIG_theta",
              "agent_condition", "yoked_participant", "trial_num")

Data2a <- merge(Data2a, Sequences[selected], by = c("agent_condition", "yoked_participant", "trial_num"))


# surprise = unsigned reward prediction error (absolute value)
Data2a$RPE_MAP_abs <- abs(Data2a$RPE_MAP)

# on first trial, RPE_MAP_abs manually set to 0.5 
Data2a$RPE_MAP_abs <- ifelse(is.nan(Data2a$RPE_MAP_abs), 0.5, Data2a$RPE_MAP_abs)


##### other data cleaning 

str(Data2a)

Data2a <- Data2a %>% 
  mutate(agent_condition = as.factor(agent_condition),
         yoked_participant = as.factor(yoked_participant),
         oid = as.factor(oid),
         rating_condition = as.factor(rating_condition),
         feedback = as.factor(feedback),
         subj_choice = as.factor(subj_choice),
         agent_condition = as.factor(agent_condition))


##### get participant-level information (demographics and condition assignments)
demographics_s2a <- Data2a %>% group_by(oid) %>%
  summarize(age = age[1],
            gender = gender[1],
            condition = agent_condition[1])

summary(demographics_s2a$age)
table(demographics_s2a$gender)
summary(demographics_s2a$condition)

###### LOAD STUDY 2b DATA #####

Data2b <- read_csv("../data/study2b.csv")

##### remove excluded participants

# incorrect answers to attn checks
Data2b <- Data2b %>% filter(Include == 1)

# same curioisty rating on all trials
to_exclude <- Data2b %>% group_by(oid) %>%
  summarize(same_rating = length(unique(globallearn)) == 1) %>%
  filter(same_rating == TRUE)

Data2b <- Data2b %>% filter(!(oid %in% to_exclude$oid))


##### add model estimates
Sequences <- read.csv("../model_results/dorfman_exp1.csv")

# renumber subjects from 1 to n
Sequences$subject <- as.factor(as.numeric(as.factor(Sequences$subject)))

# match col names from data
Sequences$agent_condition <- ifelse(Sequences$condition == 1, "bandit",
                                    ifelse(Sequences$condition == 2, "tycoon", "sheriff"))
Sequences$yoked_participant <- Sequences$subject


selected <- c("chosen_mine_prob", "latent_agent", 
              "RPE_MAP", "Entropy_z", "Entropy_theta", "EIG_theta",
              "agent_condition", "yoked_participant", "trial_num")

Data2b <- merge(Data2b, Sequences[selected], by = c("agent_condition", "yoked_participant", "trial_num"))


# surprise = unsigned reward prediction error (absolute value)
Data2b$RPE_MAP_abs <- abs(Data2b$RPE_MAP)

# on first trial, RPE_MAP_abs manually set to 0.5 
Data2b$RPE_MAP_abs <- ifelse(is.nan(Data2b$RPE_MAP_abs), 0.5, Data2b$RPE_MAP_abs)


##### other data cleaning 

str(Data2b)

Data2b <- Data2b %>% 
  mutate(agent_condition = as.factor(agent_condition),
         yoked_participant = as.factor(yoked_participant),
         oid = as.factor(oid),
         rating_condition = as.factor(rating_condition),
         feedback = as.factor(feedback),
         subj_choice = as.factor(subj_choice),
         agent_condition = as.factor(agent_condition))


##### get participant-level information (demographics and condition assignments)
demographics_s2b <- Data2b %>% group_by(oid) %>%
  summarize(age = age[1],
            gender = gender[1],
            condition = agent_condition[1])

summary(demographics_s2b$age)
table(demographics_s2b$gender)
summary(demographics_s2b$condition)


######### Main analyses: multiple regression #########

##### study 2a ######

m1 <- lmer(scale(curiosity) ~ (scale(RPE_MAP_abs) + scale(Entropy_theta) + 
                                 scale(Entropy_z) + scale(EIG_theta))*rating_condition + 
             (scale(RPE_MAP_abs) + scale(Entropy_theta) + 
                scale(Entropy_z) + scale(EIG_theta)|oid) ,
           data = Data2a, control = lmerControl(optimizer = "bobyqa"))
summary(m1)

round(Confint(m1), 2)
drop1(m1, test = "Chisq")


# follow up - just in LOCAL learning goal condition
m1a <- lmer(scale(curiosity) ~ (scale(RPE_MAP_abs) + scale(Entropy_theta) + 
                                 scale(Entropy_z) + scale(EIG_theta)) + 
             (scale(RPE_MAP_abs) + scale(Entropy_theta) + 
                scale(Entropy_z) + scale(EIG_theta)|oid) ,
           data = Data2a %>% filter(rating_condition == "Curiosity_Outcome"), 
           control = lmerControl(optimizer = "bobyqa"))
summary(m1a)
round(Confint(m1a), 2)
drop1(m1a, test = "Chisq")

# follow up - just in GLOBAL learning goal condition
m1b <- lmer(scale(curiosity) ~ (scale(RPE_MAP_abs) + scale(Entropy_theta) + 
                                  scale(Entropy_z) + scale(EIG_theta)) + 
              (scale(RPE_MAP_abs) + scale(Entropy_theta) + 
                 scale(Entropy_z) + scale(EIG_theta)|oid) ,
            data = Data2a %>% filter(rating_condition == "Curiosity_Mine"), 
            control = lmerControl(optimizer = "bobyqa"))
summary(m1b)
round(Confint(m1b), 2)
drop1(m1b, test = "Chisq")

# global - just Entropy_z
m1c <- lmer(scale(curiosity) ~ (scale(Entropy_z)) + 
              (scale(Entropy_z)|oid) ,
            data = Data2a %>% filter(rating_condition == "Curiosity_Mine"), 
            control = lmerControl(optimizer = "bobyqa"))
summary(m1c)
round(Confint(m1c), 2)
drop1(m1c, test = "Chisq")


######### study 2b #######


# combine data with Curiosity_Outcome condition of Study 2a
Data2a_local <- Data2a %>% filter(rating_condition == "Curiosity_Outcome")


### make sure oids are distinct
Data2a_local$oid <- as.factor(as.numeric(as.character(Data2a_local$oid)) + 1000)
sum(Data2b$oid %in% Data2a_local$oid)
sum(Data2a_local$oid %in% Data2b$oid)

### change curiosity and globallearn to rating
colnames(Data2a_local)[which(colnames(Data2a_local) == "curiosity")] <- "rating"
colnames(Data2b)[which(colnames(Data2b) == "globallearn")] <- "rating"


### merge dataframes to full data
all.equal(colnames(Data2a_local), colnames(Data2b))

DataFull <- rbind(Data2a_local, Data2b)

DataFull$rating_condition <- as.factor(DataFull$rating_condition)



##### multiple regression

m2 <- lmer(scale(rating) ~ (scale(RPE_MAP_abs) + scale(Entropy_theta) + 
                                 scale(Entropy_z) + scale(EIG_theta))*rating_condition + 
             (scale(RPE_MAP_abs) + scale(Entropy_theta) + 
                scale(Entropy_z) + scale(EIG_theta)|oid),
           data = DataFull, control = lmerControl(optimizer = "bobyqa"))
summary(m2)

round(car::Confint(m2), 2)
drop1(m2, test = "Chisq")


# follow up - just rating global learning potential
m2a <- lmer(scale(rating) ~ (scale(RPE_MAP_abs) + scale(Entropy_theta) + 
                                  scale(Entropy_z) + scale(EIG_theta)) + 
              (scale(RPE_MAP_abs) + scale(Entropy_theta) + 
                 scale(Entropy_z) + scale(EIG_theta)|oid) ,
            data = Data2b, 
            control = lmerControl(optimizer = "bobyqa"))
summary(m2a)
round(Confint(m2a), 2)
drop1(m2a, test = "Chisq")


