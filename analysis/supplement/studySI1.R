library(tidyverse)
library(car)
library(lme4)
library(patchwork)
library(rstatix)
library(mlogit)
library(Formula)
library(gridExtra)
library(jtools)
library(emmeans)
library(partR2)
library(MuMIn)

source("../utils.R")


####### load STUDY S2 DATA- part 1, fixed choice #####

Data1a <- read_csv("../../model_results/StudySI1_fixedchoice.csv")

##### remove excluded participants

# incorrect answers to attn checks
Data1a <- Data1a %>% filter(Include == 1)

# same curioisty rating on all trials
to_exclude <- Data1a %>% group_by(oid) %>%
  summarize(same_rating = length(unique(curiosity)) == 1) %>%
  filter(same_rating == TRUE)

Data1a <- Data1a %>% filter(!(oid %in% to_exclude$oid))


# surprise = unsigned reward prediction error (absolute value)
Data1a$RPE_MAP_abs <- abs(Data1a$RPE_MAP)

# on first trial, RPE_MAP_abs manually set to 0.5 
Data1a$RPE_MAP_abs <- ifelse(is.nan(Data1a$RPE_MAP_abs), 0.5, Data1a$RPE_MAP_abs)


##### other data cleaning 
Data1a <- Data1a %>% 
  mutate(oid = as.factor(oid),
         feedback = as.factor(feedback),
         subj_choice = as.factor(subj_choice))


####### load STUDY S2 DATA- part 2, free choice #####

Data1b <- read_csv("../../model_results/StudySI1_freechoice.csv")

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
Data1b <- Data1b %>% 
  mutate(oid = as.factor(oid),
         feedback = as.factor(feedback),
         subj_choice = as.factor(subj_choice))


########## Merge both datasets #######

s1acols <- colnames(Data1a)
s1bcols <- colnames(Data1b)

intercols <- intersect(s1acols, s1bcols)

# make sure IDs are distinct
intersect(Data1b$oid, Data1a$oid)

DataS1 <- bind_rows(Data1a[,intercols], Data1b[,intercols])


# make condition-aligned versions of each instrumental trigger
DataS1$VOI_theta <- ifelse(DataS1$task_condition == "Choice",
                           DataS1$VOI_theta_choice,
                           DataS1$VOI_theta_prediction)

DataS1$VOI_z <- ifelse(DataS1$task_condition == "Choice",
                       DataS1$VOI_z_choice,
                       DataS1$VOI_z_prediction)

DataS1$Heuristic <- ifelse(DataS1$task_condition == "Choice",
                           DataS1$Difference_competitor,
                           DataS1$Difference_50)


##### standardize everything #####
DataS1 <- DataS1 %>%
  mutate(
    Curiosity_z = scale(curiosity),
    RPE_MAP_abs_z = scale(RPE_MAP_abs),
    Entropy_theta_z = scale(Entropy_theta),
    Entropy_z_z = scale(Entropy_z),
    EIG_theta_z = scale(EIG_theta),
    VOI_theta_choice_z = scale(VOI_theta_choice),
    VOI_theta_prediction_z = scale(VOI_theta_prediction),
    VOI_theta_z = scale(VOI_theta),
    VOI_z_choice_z = scale(VOI_z_choice),
    VOI_z_prediction_z = scale(VOI_z_prediction),
    VOI_z_z = scale(VOI_z),
    Difference_50_z = scale(Difference_50),
    Difference_competitor_z = scale(Difference_competitor),
    Heuristic_z = scale(Heuristic)
  )

########### Analyses ###############

####### 1. Regression predicting curiosity, with mine (20/65/80), task condition, 
# and their interaction as categorical predictors. Prediction: significant interaction.

DataS1$chosen_mine_prob <- as.factor(DataS1$chosen_mine_prob)
DataS1$task_condition <- as.factor(DataS1$task_condition)
DataS1$oid <- as.factor(DataS1$oid)

mod1 <- lmer(Curiosity_z ~ task_condition*chosen_mine_prob + (chosen_mine_prob|oid), data = DataS1)
summary(mod1)
drop1(mod1, test = "Chisq")

emmeans(mod1, pairwise ~ chosen_mine_prob, pbkrtest.limit = 9150)




###### 4. Regression predicting curiosity, is there a condition-specific effect?

DataS1$task_condition <- relevel(DataS1$task_condition, ref = "Choice")

# heuristic value
mod_heur <- lmer(Curiosity_z ~ (Difference_50_z + Difference_competitor_z)*
                   task_condition + 
                   (Difference_50_z + Difference_competitor_z|oid), 
                 data = DataS1, 
                 control = lmerControl(optimizer = "bobyqa"))
(sum_heur <- summary(mod_heur))
Confint(mod_heur)

drop1(mod_heur, test = "Chisq")


#### VOI
mod_V1 <- lme4::lmer(Curiosity_z ~ (VOI_z_prediction_z + VOI_z_choice_z)*task_condition + 
                       (VOI_z_prediction_z + VOI_z_choice_z|oid), 
                     data = DataS1)
(sum_V1 <- summary(mod_V1))
Confint(mod_V1)

drop1(mod_V1, test = "Chisq")


#### VPI
mod_V2 <- lme4::lmer(Curiosity_z ~ (VOI_theta_prediction_z + VOI_theta_choice_z)*task_condition + 
                       (VOI_theta_prediction_z + VOI_theta_choice_z|oid), 
                     data = DataS1,
                     control = lmerControl(optimizer = "bobyqa"))
(sum_V2 <- summary(mod_V2))
Confint(mod_V2)

drop1(mod_V2, test = "Chisq")


##### Does instrumental value predict curiosity when just looking at condition-aligned versions? #####

m3 <- lme4::lmer(Curiosity_z ~ (Heuristic_z) + 
                   (Heuristic_z|oid),
                 data = DataS1, control = lmerControl(optimizer = "bobyqa"))
summary(m3)
drop1(m3, test = "Chisq")
Confint(m3)
r.squaredGLMM(m3)

m3 <- lme4::lmer(Curiosity_z ~ (VOI_z_z) + 
                   (VOI_z_z|oid),
                 data = DataS1, control = lmerControl(optimizer = "bobyqa"))
summary(m3)
drop1(m3, test = "Chisq")
Confint(m3)
r.squaredGLMM(m3)


m3 <- lme4::lmer(Curiosity_z ~ (VOI_theta_z) + 
                   (VOI_theta_z|oid),
                 data = DataS1, control = lmerControl(optimizer = "bobyqa"))
summary(m3)
drop1(m3, test = "Chisq")
Confint(m3)
r.squaredGLMM(m3)


#### base model - replicate other studies

m3 <- lme4::lmer(Curiosity_z ~ (RPE_MAP_abs_z + Entropy_theta_z + 
                                       Entropy_z_z + EIG_theta_z) + 
                   (RPE_MAP_abs_z + Entropy_theta_z + 
                      Entropy_z_z + EIG_theta_z|oid),
                 data = DataS1, control = lmerControl(optimizer = "bobyqa"))
summary(m3)
drop1(m3, test = "Chisq")
Confint(m3)

# part r2
m3a <- lme4::lmer(Curiosity_z ~ (RPE_MAP_abs_z + Entropy_theta_z + 
                                    Entropy_z_z + EIG_theta_z) + 
                (1|oid) ,
              data = DataS1, 
              control = lmerControl(optimizer = "bobyqa"))

m3a_partr2 <- partR2(m3a,
                     partvars = c("RPE_MAP_abs_z",
                                  "Entropy_theta_z",
                                  "Entropy_z_z",
                                  "EIG_theta_z"),
                     data = DataS1, 
                     max_level = 2)
m3a_partr2


### add each instrumental feature

m3a <- lme4::lmer(Curiosity_z ~ (RPE_MAP_abs_z + Entropy_theta_z + 
                                        Entropy_z_z + EIG_theta_z + 
                                        Heuristic_z) + 
                    (RPE_MAP_abs_z + Entropy_theta_z + 
                       Entropy_z_z + EIG_theta_z + Heuristic_z|oid),
                  data = DataS1, control = lmerControl(optimizer = "bobyqa"))
summary(m3a)
Confint(m3a)
drop1(m3a, test = "Chisq")


m3b <- lme4::lmer(Curiosity_z ~ (RPE_MAP_abs_z + Entropy_theta_z + 
                                        Entropy_z_z + EIG_theta_z + 
                                        VOI_z_z) + 
                    (RPE_MAP_abs_z + Entropy_theta_z + 
                       Entropy_z_z + EIG_theta_z + VOI_z_z|oid),
                  data = DataS1, control = lmerControl(optimizer = "bobyqa"))
summary(m3b)
Confint(m3b)
drop1(m3b, test = "Chisq")

# get part r2
m3b.2 <- lme4::lmer(Curiosity_z ~ (RPE_MAP_abs_z + Entropy_theta_z + 
                                        Entropy_z_z + EIG_theta_z + 
                                        VOI_z_z) + 
                    (1|oid),
                  data = DataS1, control = lmerControl(optimizer = "bobyqa"))
m3b.2_partr2 <- partR2(m3b.2,
                     partvars = c("RPE_MAP_abs_z",
                                  "Entropy_theta_z",
                                  "Entropy_z_z",
                                  "EIG_theta_z",
                                  "VOI_z_z"),
                     data = DataS1, 
                     max_level = 2)
m3b.2_partr2


m3c <- lme4::lmer(Curiosity_z ~ (RPE_MAP_abs_z + Entropy_theta_z + 
                                        Entropy_z_z + EIG_theta_z + 
                                        VOI_theta_z) + 
                    (RPE_MAP_abs_z + Entropy_theta_z + 
                       Entropy_z_z + EIG_theta_z + VOI_theta_z|oid),
                  data = DataS1, control = lmerControl(optimizer = "bobyqa"))
summary(m3c)
Confint(m3c)
drop1(m3c, test = "Chisq")


psupp1 <- plot_summs(m3, coefs = c("Local\nlearning potential" = "Entropy_z_z",
                     "Global\nlearning potential" = "EIG_theta_z",
                     "Global uncertainty" = "Entropy_theta_z", 
                     "Surprise" = "RPE_MAP_abs_z"),
                             colors = c("#0015ff"),
                     point.size = 4) + 
  theme_classic(base_size = 8) + 
  ylab("Predictor") + 
  xlab(expression("Standardized coefficient ("~ beta~")")) + 
  theme(legend.position = "right")
psupp1

psupp2 <- plot_summs(m3b, m3c, m3a, coefs = c("Local\nlearning potential" = "Entropy_z_z",
                                              "Global\nlearning potential" = "EIG_theta_z",
                                              "Global uncertainty" = "Entropy_theta_z", 
                                              "Surprise" = "RPE_MAP_abs_z",
                                                     "VOI" = "VOI_z_z",
                                                     "VPI" = "VOI_theta_z",
                                                     "HV" = "Heuristic_z"),
                            model.names = c("Model 1\n(VOI added)",
                                            "Model 2\n(VPI added)",
                                            "Model 3\n(HV added)"),
                     point.size = 4) + 
  theme_classic(base_size = 8) + 
  ylab("Predictor") + 
  xlab(expression("Standardized coefficient ("~ beta~")")) +
  theme(legend.position = "right")
psupp2

partr2_data <- m3a_partr2$R2

p1 <- plot_partr2(partr2_data) 
p1

h_patch <- (free(psupp1) | p1) / psupp2 + plot_annotation(tag_levels = 'a')& 
  theme(plot.tag = element_text(face = "bold", size = 10))
h_patch

ggsave("../../figures/StudySI1_Results.pdf", h_patch, width = 6.5, height = 5, units = "in")



#################################### PART 3: Does instrumental value predict choices? #################################### 


## we only want to look at free choice trials
DataObserveFree <- subset(Data1b, Data1b$forcedfree == "free")


####### 1. For each participant, we will calculate the proportion of trials 
# on which they chose each mine (20/65/80). We will conduct three Mann-Whitney 
# U tests, one for each mine, testing for a difference as a function of task 
# condition.


by_part <- DataObserveFree %>% group_by(oid, task_condition) %>%
  summarise(prop80 = sum(chosen_mine_prob == 0.80)/18, #number of free choice trials is 18
            prop65 = sum(chosen_mine_prob == 0.65)/18,
            prop20 = sum(chosen_mine_prob == 0.20)/18)

by_part






wilcox.test(prop80~task_condition, data = by_part) #prediction: difference in means (choice - pred) is greater than zero
wilcox_effsize(data.frame(by_part), prop80~task_condition)

wilcox.test(prop65~task_condition, data = by_part) #no prediction
wilcox_effsize(data.frame(by_part), prop65~task_condition)

wilcox.test(prop20~task_condition, data = by_part) #prediction: difference in means (choice - pred) is less than zero
wilcox_effsize(data.frame(by_part), prop20~task_condition)



### make fig. s3
by_part$task_condition <- factor(by_part$task_condition, levels = c("Choice", "Prediction"))
by_part$Condition <- by_part$task_condition

colnames(by_part)[3:5] <- c("80%", "65%", "20%")
plotdf2 <- by_part %>% pivot_longer(`80%`:`20%`, 
                                    names_to = "mine",
                                    values_to = "prop")

anno <- data.frame(x1 = c(1, 1, 1, 2), x2 = c(2, 3, 2, 3), 
                   y1 = c(0.46, 0.49, 0.38, 0.41), y2 = c(0.47, 0.50, 0.39, 0.42), 
                   xstar = c(1.5, 2, 1.5, 2.5), ystar = c(0.485, 0.515, 0.405, 0.435),
                   lab = c("***", "***", "*", "ns"),
                   Condition = c("Choice", "Choice", "Prediction", "Prediction"))

plotdf2$Condition <- relevel(plotdf2$Condition, ref = "Prediction")
anno$Condition <- factor(anno$Condition, levels = c("Prediction", "Choice"))

choices.S3 <- ggplot(plotdf2, aes(x = mine, y = prop, color = Condition)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(0.5)) + 
  ylab("Proportion of Choices") + xlab("Mine") + theme_bw(base_size = 10) + 
  facet_wrap(~Condition) + scale_color_brewer(palette = "Set2") + 
  geom_text(data = anno, aes(x = xstar,  y = ystar, label = lab),
            color = "black", size = 3) +
  geom_segment(data = anno, aes(x = x1, xend = x1, 
                                y = y1, yend = y2),
               colour = "grey30") +
  geom_segment(data = anno, aes(x = x2, xend = x2, 
                                y = y1, yend = y2),
               colour = "grey30") +
  geom_segment(data = anno, aes(x = x1, xend = x2, 
                                y = y2, yend = y2),
               colour = "grey30") + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 10)) + 
  ggtitle("Mine Choices")
choices.S3





plotdf <- DataS1
plotdf$Condition <- plotdf$task_condition

plotdf$chosen_mine_prob <- fct_recode(plotdf$chosen_mine_prob, 
                                      "20%" = "0.2",
                                      "65%" = "0.65",
                                      "80%" = "0.8")

anno2 <- data.frame(x1 = c(1, 1, 1, 2), x2 = c(2, 3, 2, 3), 
                    y1 = c(58, 61, 62, 63.5), y2 = c(58.5, 61.5, 62.5, 64), 
                    xstar = c(1.5, 2, 1.5, 2.5), ystar = c(60, 63, 64, 65.5),
                    lab = c("**", "ns", "***", "ns"),
                    Condition = c("Choice", "Choice", "Prediction", "Prediction"))

plotdf$Condition <- relevel(plotdf$Condition, ref = "Prediction")
anno2$Condition <- factor(anno2$Condition, levels = c("Prediction", "Choice"))


curioisty.S3 <- ggplot(plotdf, aes(x = chosen_mine_prob, y = curiosity, color = Condition)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(0.5)) + 
  ylab("Curiosity") + xlab("Chosen Mine") + theme_bw(base_size = 10) + 
  facet_wrap(~Condition) + scale_color_brewer(palette = "Set2") + 
  geom_text(data = anno2, aes(x = xstar,  y = ystar, label = lab),
            color = "black", size = 3) +
  geom_segment(data = anno2, aes(x = x1, xend = x1, 
                                 y = y1, yend = y2),
               colour = "grey30") +
  geom_segment(data = anno2, aes(x = x2, xend = x2, 
                                 y = y1, yend = y2),
               colour = "grey30") +
  geom_segment(data = anno2, aes(x = x1, xend = x2, 
                                 y = y2, yend = y2),
               colour = "grey30") + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 10)) + 
  ggtitle("Curiosity") + coord_cartesian(ylim = c(45, 70))
curioisty.S3




mat <- rbind(c(1, 2))

fullplot <- grid.arrange(curioisty.S3,choices.S3, layout_matrix = mat)
fullplot



###### 3. Multinomial logistic regression 


##### get choice predictors from model

colnames(DataObserveFree)

DataObserveFree <- DataObserveFree %>% mutate(
  VOI_theta_choice.20 = case_when(mine_prob_win_left == 0.20 ~ VOI_theta_choice_1,
                                  mine_prob_win_center == 0.20 ~ VOI_theta_choice_2,
                                  mine_prob_win_right == 0.20 ~ VOI_theta_choice_3),
  VOI_theta_choice.65 = case_when(mine_prob_win_left == 0.65 ~ VOI_theta_choice_1,
                                  mine_prob_win_center == 0.65 ~ VOI_theta_choice_2,
                                  mine_prob_win_right == 0.65 ~ VOI_theta_choice_3),
  VOI_theta_choice.80 = case_when(mine_prob_win_left == 0.80 ~ VOI_theta_choice_1,
                                  mine_prob_win_center == 0.80 ~ VOI_theta_choice_2,
                                  mine_prob_win_right == 0.80 ~ VOI_theta_choice_3),
  VOI_theta_prediction.20 = case_when(mine_prob_win_left == 0.20 ~ VOI_theta_prediction_1,
                                  mine_prob_win_center == 0.20 ~ VOI_theta_prediction_2,
                                  mine_prob_win_right == 0.20 ~ VOI_theta_prediction_3),
  VOI_theta_prediction.65 = case_when(mine_prob_win_left == 0.65 ~ VOI_theta_prediction_1,
                                  mine_prob_win_center == 0.65 ~ VOI_theta_prediction_2,
                                  mine_prob_win_right == 0.65 ~ VOI_theta_prediction_3),
  VOI_theta_prediction.80 = case_when(mine_prob_win_left == 0.80 ~ VOI_theta_prediction_1,
                                  mine_prob_win_center == 0.80 ~ VOI_theta_prediction_2,
                                  mine_prob_win_right == 0.80 ~ VOI_theta_prediction_3),
  Difference_50.20 = case_when(mine_prob_win_left == 0.20 ~ Difference_50_1,
                                      mine_prob_win_center == 0.20 ~ Difference_50_2,
                                      mine_prob_win_right == 0.20 ~ Difference_50_3),
  Difference_50.65 = case_when(mine_prob_win_left == 0.65 ~ Difference_50_1,
                                      mine_prob_win_center == 0.65 ~ Difference_50_2,
                                      mine_prob_win_right == 0.65 ~ Difference_50_3),
  Difference_50.80 = case_when(mine_prob_win_left == 0.80 ~ Difference_50_1,
                                      mine_prob_win_center == 0.80 ~ Difference_50_2,
                                      mine_prob_win_right == 0.80 ~ Difference_50_3),
  Difference_competitor.20 = case_when(mine_prob_win_left == 0.20 ~ Difference_competitor_1,
                               mine_prob_win_center == 0.20 ~ Difference_competitor_2,
                               mine_prob_win_right == 0.20 ~ Difference_competitor_3),
  Difference_competitor.65 = case_when(mine_prob_win_left == 0.65 ~ Difference_competitor_1,
                               mine_prob_win_center == 0.65 ~ Difference_competitor_2,
                               mine_prob_win_right == 0.65 ~ Difference_competitor_3),
  Difference_competitor.80 = case_when(mine_prob_win_left == 0.80 ~ Difference_competitor_1,
                               mine_prob_win_center == 0.80 ~ Difference_competitor_2,
                               mine_prob_win_right == 0.80 ~ Difference_competitor_3),
)





colnames(DataObserveFree)
# get rid of curiosity features just to avoid confusion
DataObserveFree <- DataObserveFree %>% 
  select(-c(RPE_MAP, Entropy_z, Entropy_theta,
            EIG_theta, VOI_z_choice, VOI_theta_choice, VOI_z_prediction,
            VOI_theta_prediction, Difference_50, Difference_competitor, 
            RPE_MAP_abs))



# attribute variables
cols <- c("VOI_theta_choice.20", "VOI_theta_choice.65", "VOI_theta_choice.80",
          "VOI_theta_prediction.20", "VOI_theta_prediction.65", "VOI_theta_prediction.80",
          "Difference_50.20", "Difference_50.65", "Difference_50.80",
          "Difference_competitor.20", "Difference_competitor.65", "Difference_competitor.80")

DataObserveFree$chosen_mine_prob <- as.factor(ifelse(DataObserveFree$chosen_mine_prob == "0.2", "20",
                                                     ifelse(DataObserveFree$chosen_mine_prob == "0.65", "65",
                                                            ifelse(DataObserveFree$chosen_mine_prob == "0.8", "80", NA))))



# reshape to long choice data
df_choice <- dfidx(as.data.frame(DataObserveFree), shape = "wide", varying = which(colnames(DataObserveFree) %in% cols), 
                   choice = "chosen_mine_prob",
                   idnames = c("chid", "alt"))

df_choice$task_condition <- factor(df_choice$task_condition, levels = c("Prediction", "Choice"))

#fit model - heuristic features
m_heur <- mlogit(Formula(chosen_mine_prob ~ 0 + Difference_50 + Difference_competitor + 
                           (Difference_50 + Difference_competitor):task_condition), df_choice)
(s_heur <- summary(m_heur))

round(exp(Confint(m_heur)),3)


#fit model - optimal features
m_val <- mlogit(Formula(chosen_mine_prob ~ 0 + VOI_theta_prediction + VOI_theta_choice + 
                          (VOI_theta_prediction + VOI_theta_choice):task_condition), df_choice)
(s_val <- summary(m_val))
exp(Confint(m_val))

