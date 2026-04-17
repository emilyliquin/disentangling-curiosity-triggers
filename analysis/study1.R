library(tidyverse)
library(car)
library(lme4)
library(jtools)
library(simr)
library(partR2)
library(MuMIn)

# load utilities (for plotting)
source("utils.R")

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

# scale everything
Data1a <- Data1a %>%
  mutate(
    Curiosity_z = scale(Curiosity),
    RPE_MAP_abs_z = scale(RPE_MAP_abs),
    Entropy_theta_z = scale(Entropy_theta),
    Entropy_z_z = scale(Entropy_z),
    EIG_theta_z = scale(EIG_theta),
  )

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


##### main regression model #####

m1 <- lmer(
  Curiosity_z ~ 
    RPE_MAP_abs_z +
    Entropy_theta_z +
    Entropy_z_z + 
    EIG_theta_z + 
    (RPE_MAP_abs_z +
       Entropy_theta_z +
       Entropy_z_z + 
       EIG_theta_z | oid),
  data = Data1a,
  control = lmerControl(optimizer = "bobyqa")
)
summary(m1)
round(Confint(m1), 2)
drop1(m1, test = "Chisq")
vif(m1)


#### partial r2 - relative contributions ####
# only possible without random slopes, so we need to refit the model
m1a <- lmer(
  Curiosity_z ~ 
    RPE_MAP_abs_z +
    Entropy_theta_z +
    Entropy_z_z + 
    EIG_theta_z + 
    (1 | oid),
  data = Data1a,
  control = lmerControl(optimizer = "bobyqa")
)

m1a_partr2 <- partR2(m1a,
       partvars = c("RPE_MAP_abs_z",
                    "Entropy_theta_z",
                    "Entropy_z_z",
                    "EIG_theta_z"),
       data = Data1a, 
       max_level = 2)

m1a_partr2



######### study 1b #######

# scale everything
Data1b <- Data1b %>%
  mutate(
    Curiosity_z = scale(curiosity),
    RPE_MAP_abs_z = scale(RPE_MAP_abs),
    Entropy_theta_z = scale(Entropy_theta),
    Entropy_z_z = scale(Entropy_z),
    EIG_theta_z = scale(EIG_theta),
  )

##### main regression model #####

m2 <- lmer(
  Curiosity_z ~ 
    RPE_MAP_abs_z +
    Entropy_theta_z +
    Entropy_z_z + 
    EIG_theta_z + 
    (RPE_MAP_abs_z +
       Entropy_theta_z +
       Entropy_z_z + 
       EIG_theta_z | oid),
  data = Data1b,
  control = lmerControl(optimizer = "bobyqa")
)
summary(m2)
round(Confint(m2), 2)
drop1(m2, test = "Chisq")
vif(m2)



#### partial r2 - relative contributions ####
# only works without random slopes
m2a <- lmer(
  Curiosity_z ~ 
    RPE_MAP_abs_z +
    Entropy_theta_z +
    Entropy_z_z + 
    EIG_theta_z + 
    (1 | oid),
  data = Data1b,
  control = lmerControl(optimizer = "bobyqa")
)
summary(m2a)


m2a_partr2 <- partR2(m2a,
                     partvars = c("RPE_MAP_abs_z",
                                  "Entropy_theta_z",
                                  "Entropy_z_z",
                                  "EIG_theta_z"),
                     data = Data1b, 
                    max_level = 2)
m2a_partr2

##### make Figure for Study 1 ######
p_panel1 <- plot_summs(m1, m2, 
                       model.names = c("1a (yoked choice)", "1b (free choice)"), 
                       legend.title = "Study", coefs = c("Local LP" = "Entropy_z_z",
                                                         "Global LP" = "EIG_theta_z",
                                                         "Global\nuncertainty" = "Entropy_theta_z",
                                                         "Surprise" = "RPE_MAP_abs_z"
                                                         ),
                       colors = c("#49b7fc", "#ff7b00"),
                       point.size = 4)+ 
  theme_classic(base_size = 7) + 
  theme(legend.position = c(0.84,0.18),
        plot.title = element_text(size = rel(1)),
        plot.tag = element_text(face = "bold")) + 
  ylab("Predictor") + 
  xlab(expression("Standardized coefficient ("~ beta~")")) + 
  coord_cartesian(xlim = c(-0.6, 0.9)) + 
  ggtitle("Study 1") + 
  labs(tag = "a")
p_panel1


### make plots for part R2 ###

s1a_partr2 <- m1a_partr2$R2
s1b_partr2 <- m2a_partr2$R2


p1 <- plot_partr2(s1a_partr2) + ggtitle("Study 1a (yoked choice)") + 
  labs(tag = "b") + theme(plot.tag = element_text(face = "bold"))


p2 <- plot_partr2(s1b_partr2) + ggtitle("Study 1b (free choice)") + 
  labs(tag = "c") + theme(plot.tag = element_text(face = "bold"))




##### methods: sensitivity analysis for power #####

mod_sim <- lmer(Curiosity_z ~ RPE_MAP_abs_z + Entropy_theta_z + 
                                 Entropy_z_z + EIG_theta_z + 
             (RPE_MAP_abs_z + Entropy_theta_z + 
                Entropy_z_z + EIG_theta_z|oid) ,
           data = Data1a, 
           control = lmerControl(optimizer = "bobyqa"))
summary(mod_sim)


fixef(mod_sim)["RPE_MAP_abs_z"] <- 0.05


# note - this takes a long time to run! (~2 hours)
sim_power <- powerSim(mod_sim,
                      fcompare(.~.-RPE_MAP_abs_z),
                      seed=1234, #set for replication
                      nsim=1000, #set low for time or high for real
                      alpha=.05) #alpha
sim_power

# Power for model comparison, (95% confidence interval):
#   64.20% (61.14, 67.18)
# 
# Test: Likelihood ratio
# Comparison to . ~ . - scale(RPE_MAP_abs) + [re]
# 
# Based on 1000 simulations, (0 warnings, 0 errors)
# alpha = 0.05, nrow = 8300
# 
# Time elapsed: 1 h 46 m 12 s

# code adapted from: https://thechangelab.stanford.edu/tutorials/power-analysis/post-hoc-power-sensitivity-analysis-using-the-sesoi/
detectable_effect <- 0.05 #setting the effect size to test
Current_Power <- 0.64 #current power

#repeat until the power reaches 80%
# note - this takes a long time to run! (~2 hours)
while (Current_Power < 0.8){
  #update the effect size
  detectable_effect <- detectable_effect + 0.01 #increase by 0.01
  fixef(mod_sim)["RPE_MAP_abs_z"] <- detectable_effect
  
  #Power for the effect of interest
  SimPower_Fixed <- powerSim(mod_sim,
                             fcompare(.~.-RPE_MAP_abs_z),
                             seed=5234, #set for replication
                             nsim=1000, #set low for time or high for real
                             alpha=.05,
                             progress = TRUE) 
  
  #examine post-hoc power
  Current_Power <- summary(SimPower_Fixed)$mean #save the power estimate
  cat(detectable_effect,"with power",Current_Power,"\n")
}
# 0.06 with power 0.807

##### SI: individual regressions ######


##### for Study 1a: individual regressions

m1a <- lmer(Curiosity_z ~ RPE_MAP_abs_z + 
             (RPE_MAP_abs_z|oid) ,
           data = Data1a, control = lmerControl(optimizer = "bobyqa"))
round(Confint(m1a), 2)
drop1(m1a, test = "Chisq")
r.squaredGLMM(m1a)

m1b <- lmer(Curiosity_z ~ Entropy_theta_z + 
             (Entropy_theta_z|oid) ,
           data = Data1a, control = lmerControl(optimizer = "bobyqa"))
round(Confint(m1b), 2)
drop1(m1b, test = "Chisq")
r.squaredGLMM(m1b)

m1c <- lmer(Curiosity_z ~ Entropy_z_z + 
             (Entropy_z_z|oid) ,
           data = Data1a, control = lmerControl(optimizer = "bobyqa"))
round(Confint(m1c), 2)
drop1(m1c, test = "Chisq")
r.squaredGLMM(m1c)

m1d <- lmer(Curiosity_z ~ EIG_theta_z + 
             (EIG_theta_z|oid) ,
           data = Data1a, control = lmerControl(optimizer = "bobyqa"))
round(Confint(m1d), 2)
drop1(m1d, test = "Chisq")
r.squaredGLMM(m1d)


##### for Study 1b: individual regressions

m2a <- lmer(Curiosity_z ~ RPE_MAP_abs_z + 
             (RPE_MAP_abs_z|oid) ,
           data = Data1b, control = lmerControl(optimizer = "bobyqa"))
round(Confint(m2a), 2)
drop1(m2a, test = "Chisq")
r.squaredGLMM(m2a)


m2b <- lmer(Curiosity_z ~ Entropy_theta_z + 
             (Entropy_theta_z|oid) ,
           data = Data1b, control = lmerControl(optimizer = "bobyqa"))
round(Confint(m2b), 2)
drop1(m2b, test = "Chisq")
r.squaredGLMM(m2b)

m2c <- lmer(Curiosity_z ~ Entropy_z_z + 
             (Entropy_z_z|oid) ,
           data = Data1b, control = lmerControl(optimizer = "bobyqa"))
round(Confint(m2c), 2)
drop1(m2c, test = "Chisq")
r.squaredGLMM(m2c)

m2d <- lmer(Curiosity_z ~ EIG_theta_z + 
             (EIG_theta_z|oid) ,
           data = Data1b, control = lmerControl(optimizer = "bobyqa"))
round(Confint(m2d), 2)
drop1(m2d, test = "Chisq")
r.squaredGLMM(m2d)
