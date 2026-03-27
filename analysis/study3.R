library(tidyverse)
library(car)
library(lme4)
library(jtools)
library(ggeffects)
library(ggbeeswarm)
library(emmeans)
library(sjPlot)
library(rstatix)
library(partR2)
library(patchwork)
library(MuMIn)

source("utils.R")

####### load STUDY 3 KID DATA #####

Data3kids <- read_csv("../model_results/study3_kids.csv")

##### remove excluded participants

# incorrect answers to attn checks
Data3kids <- Data3kids %>% filter(Include == 1)

# same curioisty rating on all trials
to_exclude <- Data3kids %>% group_by(oid) %>%
  summarize(same_rating = length(unique(Curiosity)) == 1) %>%
  filter(same_rating == TRUE)

Data3kids <- Data3kids %>% filter(!(oid %in% to_exclude$oid))


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

Data3adults <- read_csv("../model_results/study3_adults.csv")

##### remove excluded participants

# incorrect answers to attn checks
Data3adults <- Data3adults %>% filter(Include == 1)

# same curioisty rating on all trials
to_exclude <- Data3adults %>% group_by(oid) %>%
  summarize(same_rating = length(unique(Curiosity)) == 1) %>%
  filter(same_rating == TRUE)

Data3adults <- Data3adults %>% filter(!(oid %in% to_exclude$oid))

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


#### standardize variables ####

Data3 <- Data3 %>%
  mutate(
    Curiosity_z = scale(Curiosity),
    RPE_MAP_abs_z = scale(RPE_MAP_abs),
    Entropy_theta_z = scale(Entropy_theta),
    Entropy_z_z = scale(Entropy_z),
    EIG_theta_z = scale(EIG_theta),
  )
Data3kids <- Data3kids %>%
  mutate(
    Curiosity_z = scale(Curiosity),
    RPE_MAP_abs_z = scale(RPE_MAP_abs),
    Entropy_theta_z = scale(Entropy_theta),
    Entropy_z_z = scale(Entropy_z),
    EIG_theta_z = scale(EIG_theta),
  )
Data3adults <- Data3adults %>%
  mutate(
    Curiosity_z = scale(Curiosity),
    RPE_MAP_abs_z = scale(RPE_MAP_abs),
    Entropy_theta_z = scale(Entropy_theta),
    Entropy_z_z = scale(Entropy_z),
    EIG_theta_z = scale(EIG_theta),
  )

######### Main analyses: multiple regression #########


##### kids vs. adults
m1 <- lmer(
  Curiosity_z ~ 
    (RPE_MAP_abs_z +
       Entropy_theta_z +
       Entropy_z_z + 
       EIG_theta_z)*AgeGroup + 
    (RPE_MAP_abs_z +
       Entropy_theta_z +
       Entropy_z_z + 
       EIG_theta_z || oid),
           data = Data3, control = lmerControl(optimizer = "bobyqa"))
summary(m1)

round(Confint(m1), 2)
drop1(m1, test = "Chisq")
vif(m1)


#### follow-up - within kids 

m1a <- lmer(
  Curiosity_z ~ 
    RPE_MAP_abs_z +
       Entropy_theta_z +
       Entropy_z_z + 
       EIG_theta_z + 
    (RPE_MAP_abs_z +
       Entropy_theta_z +
       Entropy_z_z + 
       EIG_theta_z || oid),
  data = Data3kids, control = lmerControl(optimizer = "bobyqa"))
summary(m1a)

round(Confint(m1a), 2)
drop1(m1a, test = "Chisq")
vif(m1a)

m1a.2 <- lmer(
  Curiosity_z ~ 
    RPE_MAP_abs_z +
       Entropy_theta_z +
       Entropy_z_z + 
       EIG_theta_z + 
    (1 | oid),
  data = Data3kids, control = lmerControl(optimizer = "bobyqa"))

m1a_partr2 <- partR2(m1a.2,
                     partvars = c("RPE_MAP_abs_z",
                                  "Entropy_theta_z",
                                  "Entropy_z_z",
                                  "EIG_theta_z"),
                     data = Data3kids, 
                     max_level = 2)
m1a_partr2




#### follow-up - within adults
m1b <- lmer(
  Curiosity_z ~ 
    RPE_MAP_abs_z +
       Entropy_theta_z +
       Entropy_z_z + 
       EIG_theta_z + 
    (RPE_MAP_abs_z +
       Entropy_theta_z +
       Entropy_z_z + 
       EIG_theta_z || oid),
  data = Data3adults, control = lmerControl(optimizer = "bobyqa"))
summary(m1b)

round(Confint(m1b), 2)
drop1(m1b, test = "Chisq")
vif(m1b)


m1b.2 <- lmer(
  Curiosity_z ~ 
    RPE_MAP_abs_z +
       Entropy_theta_z +
       Entropy_z_z + 
       EIG_theta_z + 
    (1 | oid),
  data = Data3adults, control = lmerControl(optimizer = "bobyqa"))

m1b_partr2 <- partR2(m1b.2,
                     partvars = c("RPE_MAP_abs_z",
                                  "Entropy_theta_z",
                                  "Entropy_z_z",
                                  "EIG_theta_z"),
                     data = Data3adults, 
                     max_level = 2)
m1b_partr2



##### within kids

m2 <- lmer(
  Curiosity_z ~ 
    (RPE_MAP_abs_z +
       Entropy_theta_z +
       Entropy_z_z + 
       EIG_theta_z)*AgeMonth + 
    (RPE_MAP_abs_z +
       Entropy_theta_z +
       Entropy_z_z + 
       EIG_theta_z || oid),
  data = Data3kids, control = lmerControl(optimizer = "bobyqa"))
summary(m2)

round(Confint(m2), 3)
drop1(m2, test = "Chisq")



# generate figure, study 3
p_panel4 <- plot_summs(m1b, m1a, model.names = c("Adults", "Children"), legend.title = "Age Group", 
                       coefs = c("Local\nlearning potential" = "Entropy_z_z",
                                 "Global\nlearning potential" = "EIG_theta_z",
                                 "Global uncertainty" = "Entropy_theta_z",
                                 "Surprise" = "RPE_MAP_abs_z"),
                       colors = c("#999999", "#e5d200"),
                       point.size = 4) + 
  theme_classic(base_size = 8) + 
  theme(legend.position = "right") + 
  ylab("Predictor") + 
  xlab(expression("Standardized coefficient ("~ beta~")")) + 
  coord_cartesian(xlim = c(-0.6, 0.9))
p_panel4


kids_partr2 <- m1a_partr2$R2
adults_partr2 <- m1b_partr2$R2

p1 <- plot_partr2(kids_partr2) + ggtitle("Children")
p2 <- plot_partr2(adults_partr2) + ggtitle("Adults")




combined_fig_s2 <- p_panel4 / (p2 | p1 ) + plot_annotation(tag_levels = 'a')& 
  theme(plot.tag = element_text(face = "bold", size = 10))
combined_fig_s2

ggsave("../figures/Study3_Results.pdf", combined_fig_s2, width = 6.5, height = 5, units = "in")




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



#interaction with age (months within kids)
m4 <- glm(animal_choice ~ ed_curiosity_diff*AgeMonth,
          data = df_k, family = "binomial")
summary(m4)
drop1(m4, test = "Chisq")

# fig. S1
preds <- ggpredict(m3, terms = c("ed_curiosity_diff", "AgeGroup"))
df_full$animal_choice_num <- ifelse(df_full$animal_choice == "echidna", 1, 0)

fig2 <- ggplot() + 
  geom_line(data = preds, mapping = aes(x = x, y = predicted, color = group)) +
  geom_ribbon(data = preds, mapping = aes(x = x, ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2) +
  geom_jitter(data = df_full, mapping = aes(x = ed_curiosity_diff, 
                                            y = animal_choice_num, 
                                            color = AgeGroup),
              height = 0.05, width = 0.2, alpha = 0.5) +
  theme_classic(base_size = 10) + 
  xlab("Curiosity Difference Between Q1 and Q2") +
  ylab("P(Choosing To Learn about Q1)") +
  scale_color_manual(values = c("#e5d200", "#999999")) + 
  scale_fill_manual(values = c("#e5d200", "#999999"))

fig2

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
(t_result <- t.test(means$Av_guess_error))
means %>% ungroup() %>% cohens_d(Av_guess_error ~ 1, mu = 0)
# on average, people make guesses lower than 
# the true value (aren't taking into account the intervention enough, 
# thus overestimating the number of yellow berries)


# difference between ages -- NO DIFFERENCE
t_result2 <- t.test(Av_guess_error ~ AgeGroup, data = means)
means %>% ungroup() %>% cohens_d(Av_guess_error ~ AgeGroup)


#### learning noise: 
t.test(means$Av_abs_guess_error)
# on average, people are off from the true value
means %>% ungroup() %>% cohens_d(Av_abs_guess_error ~ 1, mu = 0)


# difference between ages -- SIG DIFFERENCE
t.test(Av_abs_guess_error ~ AgeGroup, data = means, var.equal = TRUE)
means %>% ungroup() %>% cohens_d(Av_abs_guess_error ~ AgeGroup)

#### fig S2 (top)
means$AgeGroup <- factor(means$AgeGroup, levels = c("Children", "Adults"))
p1 <- ggplot(means, aes(x = AgeGroup, y = Av_guess_error, color = AgeGroup)) + 
  geom_hline(yintercept = 0) + 
  stat_summary(fun.data = "mean_cl_boot")+
  geom_quasirandom(alpha = 0.3) + 
  theme_classic(base_size = 10)+ 
  scale_color_manual(values = c("#e5d200", "#999999")) +
  xlab("Age Group") + ylab("Learning Error") + theme(legend.position = "none")
p1  

p2 <- ggplot(means, aes(x = AgeGroup, y = Av_abs_guess_error, color = AgeGroup)) + 
  geom_hline(yintercept = 0) + 
  stat_summary(fun.data = "mean_cl_boot")+
  geom_quasirandom(alpha = 0.3) + 
  theme_classic(base_size = 10) +
  scale_color_manual(values = c("#e5d200", "#999999")) +
  xlab("Age Group") + ylab("Learning Noise") + theme(legend.position = "none")
p2 


#### footnote: deviation from naive estimate
Data3 <- Data3 %>% group_by(oid) %>%
  mutate(Naive_theta = cumsum(as.numeric(as.character(feedback)))/(trial_num))
Data3$Naive_error <- Data3$Theta_guess - Data3$Naive_theta

means2 <- Data3 %>% group_by(oid, AgeGroup) %>%
  summarize(Av_naive_error = mean(Naive_error, na.rm = TRUE))
means2

t.test(means2$Av_naive_error)
# on average, people make guesses higher than just empirical probability (take intervention into account)
means2 %>% ungroup() %>% cohens_d(Av_naive_error ~ 1, mu = 0)


# difference between ages -- NO DIFFERENCE
t.test(Av_naive_error ~ AgeGroup, data = means2, var.equal = TRUE)
means2 %>% ungroup() %>% cohens_d(Av_naive_error ~ AgeGroup)


##### footnote: does model estimate predict guesses?
 
mguess <- lmer(Theta_guess ~ Mean_theta*AgeGroup + (Mean_theta||oid), data = Data3)
summary(mguess)
drop1(mguess, test = "Chisq")
Confint(mguess)


mguessa <- lmer(Theta_guess ~ Mean_theta + (Mean_theta||oid), data = Data3kids)
summary(mguessa)
drop1(mguessa, test = "Chisq")
Confint(mguessa)

mguessb <- lmer(Theta_guess ~ Mean_theta + (Mean_theta||oid), data = Data3adults)
summary(mguessb)
drop1(mguessb, test = "Chisq")
Confint(mguessb)


##### control for learning noise in analysis of triggers

m <- lmer(Curiosity_z ~ 
            (RPE_MAP_abs_z +
               Entropy_theta_z +
               Entropy_z_z + 
               EIG_theta_z)*AgeGroup + 
            (RPE_MAP_abs_z +
               Entropy_theta_z +
               Entropy_z_z + 
               EIG_theta_z)*Av_abs_guess_error + 
            (RPE_MAP_abs_z +
               Entropy_theta_z +
               Entropy_z_z + 
               EIG_theta_z||oid), data = Data3,
          control = lmerControl(optimizer= "bobyqa"))
summary(m)
Confint(m)
drop1(m, test = "Chisq")

#### figure S2, bottom
plot_model(m, terms = c("Entropy_z_z", "AgeGroup", "Av_abs_guess_error"), 
                   type = "pred") + 
  theme_classic(base_size = 10) + 
  scale_color_manual(values = c("#e5d200", "#999999")) +
  scale_fill_manual(values = c("#e5d200", "#999999")) 


######### SI ##########


##### simple regressions ####


# surprise
regs1_adults <- lmer(Curiosity_z ~ RPE_MAP_abs_z + 
                      (RPE_MAP_abs_z||oid) ,
                    data = Data3adults, 
                    control = lmerControl(optimizer = "bobyqa"))
round(Confint(regs1_adults), 2)
drop1(regs1_adults, test = "Chisq")
r.squaredGLMM(regs1_adults)

regs1_kids <- lmer(Curiosity_z ~ RPE_MAP_abs_z + 
                       (RPE_MAP_abs_z||oid) ,
                     data = Data3kids, 
                     control = lmerControl(optimizer = "bobyqa"))
round(Confint(regs1_kids), 2)
drop1(regs1_kids, test = "Chisq")
r.squaredGLMM(regs1_kids)

regs1a <- lmer(Curiosity_z ~ RPE_MAP_abs_z*AgeGroup + 
                (RPE_MAP_abs_z||oid) ,
              data = Data3, control = lmerControl(optimizer = "bobyqa"))
drop1(regs1a, test = "Chisq")

regs1b <- lmer(Curiosity_z ~ RPE_MAP_abs_z*AgeMonth + 
                (RPE_MAP_abs_z||oid) ,
              data = Data3kids, control = lmerControl(optimizer = "bobyqa"))
drop1(regs1b, test = "Chisq")


# global uncertainty
regs2_adults <- lmer(Curiosity_z ~ Entropy_theta_z + 
                       (Entropy_theta_z||oid) ,
                     data = Data3adults, 
                     control = lmerControl(optimizer = "bobyqa"))
summary(regs2_adults)
round(Confint(regs2_adults), 2)
drop1(regs2_adults, test = "Chisq")
r.squaredGLMM(regs2_adults)

regs2_kids <- lmer(Curiosity_z ~ Entropy_theta_z + 
                     (Entropy_theta_z||oid) ,
                   data = Data3kids, 
                   control = lmerControl(optimizer = "bobyqa"))
round(Confint(regs2_kids), 2)
drop1(regs2_kids, test = "Chisq")
r.squaredGLMM(regs2_kids)

regs2a <- lmer(Curiosity_z ~ Entropy_theta_z*AgeGroup + 
                 (Entropy_theta_z||oid) ,
               data = Data3, control = lmerControl(optimizer = "bobyqa"))
drop1(regs2a, test = "Chisq")

regs2b <- lmer(Curiosity_z ~ Entropy_theta_z*AgeMonth + 
                 (Entropy_theta_z||oid) ,
               data = Data3kids, control = lmerControl(optimizer = "bobyqa"))
drop1(regs2b, test = "Chisq")



# local learning potential
regs3_adults <- lmer(Curiosity_z ~ Entropy_z_z + 
                       (Entropy_z_z||oid) ,
                     data = Data3adults, 
                     control = lmerControl(optimizer = "bobyqa"))
round(Confint(regs3_adults), 2)
drop1(regs3_adults, test = "Chisq")
r.squaredGLMM(regs3_adults)

regs3_kids <- lmer(Curiosity_z ~ Entropy_z_z + 
                     (Entropy_z_z||oid) ,
                   data = Data3kids, 
                   control = lmerControl(optimizer = "bobyqa"))
round(Confint(regs3_kids), 2)
drop1(regs3_kids, test = "Chisq")
r.squaredGLMM(regs3_kids)


regs3a <- lmer(Curiosity_z ~ Entropy_z_z*AgeGroup + 
                 (Entropy_z_z||oid) ,
               data = Data3, control = lmerControl(optimizer = "bobyqa"))
drop1(regs3a, test = "Chisq")

regs3b <- lmer(Curiosity_z ~ Entropy_z_z*AgeMonth + 
                 (Entropy_z_z||oid) ,
               data = Data3kids, control = lmerControl(optimizer = "bobyqa"))
drop1(regs3b, test = "Chisq")

# global learning potential
regs4_adults <- lmer(Curiosity_z ~ EIG_theta_z + 
                       (EIG_theta_z||oid) ,
                     data = Data3adults, 
                     control = lmerControl(optimizer = "bobyqa"))
round(Confint(regs4_adults), 2)
drop1(regs4_adults, test = "Chisq")
r.squaredGLMM(regs4_adults)

regs4_kids <- lmer(Curiosity_z ~ EIG_theta_z + 
                     (EIG_theta_z||oid) ,
                   data = Data3kids, 
                   control = lmerControl(optimizer = "bobyqa"))
round(Confint(regs4_kids), 2)
drop1(regs4_kids, test = "Chisq")
r.squaredGLMM(regs4_kids)

regs4a <- lmer(Curiosity_z ~ EIG_theta_z*AgeGroup + 
                 (EIG_theta_z||oid) ,
               data = Data3, control = lmerControl(optimizer = "bobyqa"))
drop1(regs4a, test = "Chisq")

regs4b <- lmer(Curiosity_z ~ EIG_theta_z*AgeMonth + 
                 (EIG_theta_z||oid) ,
               data = Data3kids, control = lmerControl(optimizer = "bobyqa"))
drop1(regs4b, test = "Chisq")



##### ordinal regression version #####

library(ordinal)

Data3$Curiosity_Ord <- as.ordered(Data3$Curiosity)
Data3kids$Curiosity_Ord <- as.ordered(Data3kids$Curiosity)
Data3adults$Curiosity_Ord <- as.ordered(Data3adults$Curiosity)

full_mod <- clmm(Curiosity_Ord ~ (RPE_MAP_abs_z + Entropy_theta_z + 
                   Entropy_z_z + EIG_theta_z)*AgeGroup + 
                   (1|oid), data=Data3)
summary(full_mod)
round(exp(Confint(full_mod)), 2)
drop1(full_mod, test = "Chisq")



kid_mod <- clmm(Curiosity_Ord ~ RPE_MAP_abs_z + Entropy_theta_z + 
               Entropy_z_z + EIG_theta_z + (1|oid), data=Data3kids)
summary(kid_mod)
round(exp(Confint(kid_mod)), 2)
drop1(kid_mod, test = "Chisq")


adult_mod <- clmm(Curiosity_Ord ~ RPE_MAP_abs_z + Entropy_theta_z + 
                  Entropy_z_z + EIG_theta_z + (1|oid), data=Data3adults)
summary(adult_mod)
round(exp(Confint(adult_mod)), 2)
drop1(adult_mod, test = "Chisq")

plot_summs(adult_mod, kid_mod)

p_supp_ordinal <- plot_summs(adult_mod, kid_mod, model.names = c("Adults", "Children"), legend.title = "Age Group", 
                       coefs = c("Local\nlearning potential" = "Entropy_z_z",
                                 "Global\nlearning potential" = "EIG_theta_z",
                                 "Global uncertainty" = "Entropy_theta_z",
                                 "Surprise" = "RPE_MAP_abs_z"),
                       colors = c("#999999", "#e5d200"),
                       point.size = 4) + 
  theme_classic(base_size = 10) + 
  theme(legend.position = "right") + 
  ylab("Predictor") + 
  xlab(expression("Regression coefficient (log odds)"))
p_supp_ordinal

