library(tidyverse)
library(car)
library(lme4)
library(jtools)

###### LOAD STUDY 1A DATA #####

Data1a <- read_csv("../data/study1a.csv")

##### remove excluded participants

# incorrect answers to attn checks
Data1a <- Data1a %>% filter(Include == 1)

# same curioisty rating on all trials
to_exclude <- Data1a %>% group_by(oid) %>%
  summarize(same_rating = length(unique(Curiosity)) == 1) %>%
  filter(same_rating == TRUE)

Data1a <- Data1a %>% filter(!(oid %in% to_exclude$oid))


##### add model estimates
Sequences <- read.csv("../model_results/dorfman_exp1.csv")

# renumber subjects from 1 to n
Sequences$subject <- as.factor(as.numeric(as.factor(Sequences$subject)))

# match col names from data
Sequences$Condition <- Sequences$condition
Sequences$D.Participant <- Sequences$subject
Sequences$Trial <- Sequences$trial_num

colnames(Sequences)

selected <- c("feedback", "subj_choice", "chosen_mine_prob", "latent_agent", 
              "RPE_MAP", "Entropy_z", "Entropy_theta", "EIG_theta",
              "Condition", "D.Participant", "Trial")

Data1a <- merge(Data1a, Sequences[selected], by = c("Condition", "D.Participant", "Trial"))


# surprise = unsigned reward prediction error (absolute value)
Data1a$RPE_MAP_abs <- abs(Data1a$RPE_MAP)

# on first trial, RPE_MAP_abs manually set to 0.5 
Data1a$RPE_MAP_abs <- ifelse(is.nan(Data1a$RPE_MAP_abs), 0.5, Data1a$RPE_MAP_abs)


##### other data cleaning 

# make more interpretable condition variable
Data1a$agent_condition <- ifelse(Data1a$Condition == 1, "bandit",
                                ifelse(Data1a$Condition == 2, "tycoon", 
                                       "sheriff"))

str(Data1a)

Data1a <- Data1a %>% 
  mutate(Condition = as.factor(Condition),
         D.Participant = as.factor(D.Participant),
         oid = as.factor(oid),
         feedback = as.factor(feedback),
         subj_choice = as.factor(subj_choice),
         agent_condition = as.factor(agent_condition))


##### get participant-level information (demographics and condition assignments)
demographics_s1a <- Data1a %>% group_by(oid) %>%
  summarize(age = age[1],
            gender = gender[1],
            condition = Condition[1])

summary(demographics_s1a$age)
table(demographics_s1a$gender)
summary(demographics_s1a$condition)



####### load STUDY 1B DATA #####

##### with model estimates
Data1b <- read.csv("../model_results/study1b.csv")

##### remove excluded participants

# incorrect answers to attn checks
Data1b <- Data1b %>% filter(Include == 1)

# same curioisty rating on all trials
to_exclude <- Data1b %>% group_by(oid) %>%
  summarize(same_rating = length(unique(curiosity)) == 1) %>%
  filter(same_rating == TRUE)

Data1b <- Data1b %>% filter(!(oid %in% to_exclude$oid))


# surprise = unsigned reward prediction error (absolute value)
Data1b$RPE_MAP_abs <- abs(Data1b$RPE_MAP)

# on first trial, RPE_MAP_abs manually set to 0.5 
Data1b$RPE_MAP_abs <- ifelse(is.nan(Data1b$RPE_MAP_abs), 0.5, Data1b$RPE_MAP_abs)


##### other data cleaning 

# make more interpretable condition variable
Data1b$agent_condition <- ifelse(Data1b$condition == 1, "bandit",
                                ifelse(Data1b$condition == 2, "tycoon", 
                                       "sheriff"))

str(Data1b)

Data1b <- Data1b %>% 
  mutate(oid = as.factor(oid),
         condition = as.factor(condition),
         feedback = as.factor(feedback),
         subj_choice = as.factor(subj_choice),
         agent_condition = as.factor(agent_condition))


##### get participant-level information (demographics and condition assignments)
demographics_s1b <- Data1b %>% group_by(oid) %>%
  summarize(age = age[1],
            gender = gender[1],
            condition = agent_condition[1])

summary(demographics_s1b$age)
table(demographics_s1b$gender)
summary(demographics_s1b$condition)



######### Main analyses: multiple regression #########

#how correlated are global learning potential and global uncertainty?
cor.test(Data1a$Entropy_theta, Data1a$EIG_theta)



m1 <- lmer(scale(Curiosity) ~ (scale(RPE_MAP_abs) + scale(Entropy_theta) + 
                                 scale(Entropy_z) + scale(EIG_theta)) + 
             (scale(RPE_MAP_abs) + scale(Entropy_theta) + 
                scale(Entropy_z) + scale(EIG_theta)|oid) ,
           data = Data1a, control = lmerControl(optimizer = "bobyqa"))
summary(m1)

round(Confint(m1), 2)
drop1(m1, test = "Chisq")
vif(m1)


######### study 1b #######

m2 <- lmer(scale(curiosity) ~ (scale(RPE_MAP_abs) + scale(Entropy_theta) + 
                                 scale(Entropy_z) + scale(EIG_theta)) + 
             (scale(RPE_MAP_abs) + scale(Entropy_theta) + 
                scale(Entropy_z) + scale(EIG_theta)|oid),
           data = Data1b, control = lmerControl(optimizer = "bobyqa"))
summary(m2)

round(car::Confint(m2), 2)
drop1(m2, test = "Chisq")
vif(m2)


##### make Fig. 3 part 1
p_panel1 <- plot_summs(m1, m2, 
                       model.names = c("1a (Yoked Choice)", "1b (Free Choice)"), 
                       legend.title = "Study", coefs = c("Surprise" = "scale(RPE_MAP_abs)",
                                                         "Global Uncertainty" = "scale(Entropy_theta)",
                                                         "Global\nLearning Potential" = "scale(EIG_theta)",
                                                         "Local\nLearning Potential" = "scale(Entropy_z)"),
                       colors = c("#49b7fc", "#ff7b00"),
                       point.size = 4)+ 
  theme_classic(base_size = 8) + 
  theme(legend.position = "right") + 
  ylab("") + 
  xlab("") + 
  coord_cartesian(xlim = c(-0.6, 0.9)) + 
  ggtitle("Study 1")
p_panel1



##### SI: individual regressions ######


##### for Study 1a: individual regressions

m1a <- lmer(scale(Curiosity) ~ scale(RPE_MAP_abs) + 
             (scale(RPE_MAP_abs)|oid) ,
           data = Data1a, control = lmerControl(optimizer = "bobyqa"))
round(Confint(m1a), 2)
drop1(m1a, test = "Chisq")

m1b <- lmer(scale(Curiosity) ~ scale(Entropy_theta) + 
             (scale(Entropy_theta)|oid) ,
           data = Data1a, control = lmerControl(optimizer = "bobyqa"))
round(car::Confint(m1b), 2)
drop1(m1b, test = "Chisq")

m1c <- lmer(scale(Curiosity) ~ scale(Entropy_z) + 
             (scale(Entropy_z)|oid) ,
           data = Data1a, control = lmerControl(optimizer = "bobyqa"))
round(car::Confint(m1c), 2)
drop1(m1c, test = "Chisq")

m1d <- lmer(scale(Curiosity) ~ scale(EIG_theta) + 
             (scale(EIG_theta)|oid) ,
           data = Data1a, control = lmerControl(optimizer = "bobyqa"))
round(car::Confint(m1d), 2)
drop1(m1d, test = "Chisq")


##### for Study 1b: individual regressions

m2a <- lmer(scale(curiosity) ~ scale(RPE_MAP_abs) + 
             (scale(RPE_MAP_abs)|oid) ,
           data = Data1b, control = lmerControl(optimizer = "bobyqa"))
round(car::Confint(m2a), 2)
drop1(m2a, test = "Chisq")

m2b <- lmer(scale(curiosity) ~ scale(Entropy_theta) + 
             (scale(Entropy_theta)|oid) ,
           data = Data1b, control = lmerControl(optimizer = "bobyqa"))
round(car::Confint(m2b), 2)
drop1(m2b, test = "Chisq")

m2c <- lmer(scale(curiosity) ~ scale(Entropy_z) + 
             (scale(Entropy_z)|oid) ,
           data = Data1b, control = lmerControl(optimizer = "bobyqa"))
round(car::Confint(m2c), 2)
drop1(m2c, test = "Chisq")

m2d <- lmer(scale(curiosity) ~ scale(EIG_theta) + 
             (scale(EIG_theta)|oid) ,
           data = Data1b, control = lmerControl(optimizer = "bobyqa"))
round(car::Confint(m2d), 2)
drop1(m2d, test = "Chisq")
