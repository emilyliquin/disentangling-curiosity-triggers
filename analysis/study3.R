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

####### load STUDY 3 ADULT DATA #####

Data3adults <- read_csv("../model_results/study3_adults.csv")


##### check for systematic variation in exclusions for not understanding rating scale ####

Kids_ByID <- Data3kids %>% 
  group_by(oid, AgeYr, AgeMonth, MaxMinInclude) %>%
  summarize()


Adults_ByID <- Data3adults %>% 
  group_by(oid, MaxMinInclude) %>%
  summarize()

# make sure ID numbers are distinct
Adults_ByID$oid <- as.factor(as.numeric(as.character(Adults_ByID$oid)) + 1000)
Kids_ByID$oid <- as.factor(Kids_ByID$oid)

Adults_ByID$AgeYr <- NA
Adults_ByID$AgeMonth <- NA
Adults_ByID$AgeGroup <- "Adults"
Kids_ByID$AgeGroup <- "Children"

alldat_ByID <- rbind(Adults_ByID, Kids_ByID)

alldat_ByID$MaxMinExclude <- ifelse(alldat_ByID$MaxMinInclude == 1, 0, 1)
Kids_ByID$MaxMinExclude <- ifelse(Kids_ByID$MaxMinInclude == 1, 0, 1)

mexcl_a <- glm(MaxMinExclude ~ AgeGroup, data = alldat_ByID, family = "binomial")
summary(mexcl_a)
round(exp(Confint(mexcl_a)), 2)
drop1(mexcl_a, test = "Chisq")

mexcl_b <- glm(MaxMinExclude ~ scale(AgeMonth), data = Kids_ByID, family = "binomial")
summary(mexcl_b)
round(exp(Confint(mexcl_b)), 2)
drop1(mexcl_b, test = "Chisq")

table(Kids_ByID$AgeYr, Kids_ByID$MaxMinExclude)
18/(18+36)
6/(6+32)

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
                       coefs = c("Local LP" = "Entropy_z_z",
                                 "Global LP" = "EIG_theta_z",
                                 "Global\nuncertainty" = "Entropy_theta_z",
                                 "Surprise" = "RPE_MAP_abs_z"),
                       colors = c("#999999", "#e5d200"),
                       point.size = 4) + 
  theme_classic(base_size = 7) + 
  theme(legend.position = "right") + 
  ylab("Predictor") + 
  xlab(expression("Standardized coefficient ("~ beta~")")) + 
  coord_cartesian(xlim = c(-0.6, 0.9))
p_panel4


kids_partr2 <- m1a_partr2$R2
adults_partr2 <- m1b_partr2$R2

p1 <- plot_partr2_s3(kids_partr2) + ggtitle("Children")
p2 <- plot_partr2_s3(adults_partr2) + ggtitle("Adults")


layout <- "
AA
AA
BC
"

combined_fig_s2 <- p_panel4 + p2 + p1 + 
  plot_layout(design = layout) + 
  plot_annotation(tag_levels = 'a')& 
  theme(plot.tag = element_text(face = "bold", size = 10))
combined_fig_s2

ggsave("../figures/Study3_Results.pdf", combined_fig_s2, width = 11, height = 11, units = "cm")




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
  theme_classic(base_size = 7) + 
  xlab("Curiosity Difference Between Q1 and Q2") +
  ylab("P(Choosing To Learn about Q1)") +
  scale_color_manual(values = c("#e5d200", "#999999")) + 
  scale_fill_manual(values = c("#e5d200", "#999999"))

fig2

ggsave("../figures/Study3_SI_FigS1.pdf", fig2, width = 11, height = 9, units = "cm")

##### simulate adding measurement noise to adults ####

#### get true effects for interactions
m1a <- lmer(
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
summary(m1a)
true_interaction_effects <- fixef(m1a)

#### get true effects for validity
m3a <- glm(animal_choice ~ ed_curiosity_diff, data = df_full %>% filter(AgeGroup == "Children"), family = "binomial")
summary(m3a)
true_kid_validity <- exp(coef(m3a))[2]


m3b <- glm(animal_choice ~ ed_curiosity_diff, data = df_full %>% filter(AgeGroup == "Adults"), family = "binomial")
summary(m3b)
true_adult_validity <- exp(coef(m3b))[2]


###### simulate adding noise
random_amounts <- seq(0.1, 0.8, by = 0.1)
nsim <- 5000

simulation_output <- data.frame(sim_rpe_int = numeric(),
                                sim_ent.theta_int = numeric(),
                                sim_ent.z_int = numeric(),
                                sim_eig.theta_int = numeric(),
                                noise.level = numeric(),
                                noisy.validity = numeric())
set.seed(13423)
for(p_error in random_amounts){
  
  # place to store results
  int_RPE <- c()
  int_Ent_theta <- c()
  int_Ent_z <- c()
  int_EIG_theta <- c()
  validity_ORs <- c()
  
  for(i in 1:nsim){
    print(paste0("sim ", i, " of 5000 at error level ", p_error))
    # copy adult dataframe
    Data_adults_sim <- Data3adults
    # select which curiosity measurements to randomly change, proportion determined by p_error
    n_change <- rep(TRUE, round(p_error*length(Data_adults_sim$Curiosity)))
    n_nochange <- rep(FALSE, length(Data_adults_sim$Curiosity) - length(n_change))
    change <- sample(c(n_change, n_nochange))
    
    # change those to a random rating 1:4
    Data_adults_sim$Curiosity[change] <- sample(1:4, sum(change), replace = TRUE)
    
    # combine original and noisy data
    Data_adults_sim$DataType <- "Simulated"
    Data3adults$DataType <- "Real"
    Data_adults_sim$oid <- as.factor(as.numeric(as.character(Data_adults_sim$oid)) + 2000) # unique IDs
    Data_combine <- rbind(Data_adults_sim, Data3adults)
    
    # scale w/ new curiosity ratings
    Data_combine$Curiosity_z <- as.vector(scale(Data_combine$Curiosity))
    
    # get interaction terms
    m1b <- lmer(
      Curiosity_z ~ 
        (scale(RPE_MAP_abs) +
           scale(Entropy_theta) +
           scale(Entropy_z) + 
           scale(EIG_theta))*DataType + 
        (scale(RPE_MAP_abs) +
           scale(Entropy_theta) +
           scale(Entropy_z) + 
           scale(EIG_theta) || oid),
      data = Data_combine, control = lmerControl(optimizer = "bobyqa"))
    summary(m1b)
    fixef_output <- fixef(m1b)
    
    
    int_RPE <- c(int_RPE, fixef_output[7])
    int_Ent_theta <- c(int_Ent_theta, fixef_output[8])
    int_Ent_z <- c(int_Ent_z, fixef_output[9])
    int_EIG_theta <- c(int_EIG_theta, fixef_output[10])

    ### get predictive validity 
    # copy validity data
    Data_adults_sim_w <- Data3adults %>% group_by(oid, AgeGroup) %>%
      summarize(echidna_curiosity = echidna_curiosity[1],
                dog_curiosity = dog_curiosity[1],
                animal_choice = animal_choice[1],
                .groups = "drop")
    
    # randomly permute p_error ratings
    all_validity_ratings <- c(Data_adults_sim_w$echidna_curiosity, Data_adults_sim_w$dog_curiosity)

    # select which curiosity measurements to randomly change, proportion determined by p_error
    n_change <- rep(TRUE, round(p_error*length(all_validity_ratings)))
    n_nochange <- rep(FALSE, length(all_validity_ratings) - length(n_change))
    change <- sample(c(n_change, n_nochange))
    
    all_validity_ratings[change] <- sample(1:4, sum(change), replace = TRUE)
    Data_adults_sim_w$echidna_curiosity <- all_validity_ratings[1:(length(all_validity_ratings)/2)]
    Data_adults_sim_w$dog_curiosity <- all_validity_ratings[(length(all_validity_ratings)/2 + 1):length(all_validity_ratings)]
    
    # clean diff scores and choices
    Data_adults_sim_w$ed_curiosity_diff <- Data_adults_sim_w$echidna_curiosity - Data_adults_sim_w$dog_curiosity
    Data_adults_sim_w$animal_choice <- as.factor(Data_adults_sim_w$animal_choice)
    
    # fit model
    m4 <- glm(animal_choice ~ ed_curiosity_diff,
              data = Data_adults_sim_w, family = "binomial")
    summary(m4)
    validity_ORs <- c(validity_ORs, exp(coef(m4))[2])
  }
  
  this_out <- data.frame(sim_rpe_int = int_RPE,
                         sim_ent.theta_int = int_Ent_theta,
                         sim_ent.z_int = int_Ent_z,
                         sim_eig.theta_int = int_EIG_theta,
                         noiselevel = rep(p_error, nsim),
                         noisyvalidity = validity_ORs)
  
  simulation_output <- rbind(simulation_output, this_out)
}

# add true interactions to simulation output
simulation_output$true_rpe_int <- true_interaction_effects[7]
simulation_output$true_ent.theta_int <- true_interaction_effects[8]
simulation_output$true_ent.z_int <- true_interaction_effects[9]
simulation_output$true_eig.theta_int <- true_interaction_effects[10]


# reshape
simulation_output_l <- simulation_output %>%
  pivot_longer(c(sim_rpe_int:sim_eig.theta_int),
               names_sep = "_",
               names_to = c("type", "term", NA),
               values_to = "int")
simulation_output_l$true_int <- case_when(simulation_output_l$term == "rpe" ~ simulation_output_l$true_rpe_int,
                                          simulation_output_l$term == "ent.theta" ~ simulation_output_l$true_ent.theta_int,
                                          simulation_output_l$term == "ent.z" ~ simulation_output_l$true_ent.z_int,
                                          simulation_output_l$term == "eig.theta" ~ simulation_output_l$true_eig.theta_int)


library(geomtextpath)

simulation_output$true_kid_validity <- true_kid_validity

new_labels <- c("eig.theta" = "Global LP", 
                "ent.theta" = "Global uncertainty", 
                "ent.z" = "Local LP",
                "rpe" = "Surprise")

simulation_output_l$term <- factor(simulation_output_l$term,
                                   levels = c("ent.z", "eig.theta",
                                              "ent.theta", "rpe"))
psim_1 <- ggplot(simulation_output_l, aes(x = noiselevel, y = int, group = noiselevel)) + 
  stat_summary(fun.data = median_hilow, fun.args = list(conf.int = 0.95)) +
  geom_hline(aes(yintercept = true_int, color = "true interaction,\nchildren vs. adults", 
                 linetype = "true interaction,\nchildren vs. adults")) +
  theme_classic(base_size = 7) + 
  facet_grid(~term, labeller = as_labeller(new_labels)) +
  scale_color_manual(name = " ", values = c("true interaction,\nchildren vs. adults" = "red")) + 
  scale_linetype_manual(name = " ", values = c("true interaction,\nchildren vs. adults" = 2)) + 
  ylab("Interaction term") + 
  xlab("Noise level")+
  theme(legend.position = "bottom")


psim_2 <- ggplot(simulation_output, aes(x = noiselevel, y = noisyvalidity, group = noiselevel)) + 
  stat_summary(fun.data = median_hilow, fun.args = list(conf.int = 0.95)) +
  geom_hline(aes(yintercept = true_kid_validity, color = "true odds ratio,\nchildren", 
                 linetype = "true odds ratio,\nchildren")) +
  theme_classic(base_size = 7) +
  scale_color_manual(name = " ", values = c("true odds ratio,\nchildren" = "blue")) + 
  scale_linetype_manual(name = " ", values = c("true odds ratio,\nchildren" = 2))+ 
  ylab("Predictive validity (odds ratio)") + 
  xlab("Noise level") +
  theme(legend.position = "bottom")

figs2_new <- psim_1 + psim_2+
  plot_layout(widths = c(2, 1))

ggsave("../figures/Study3_SI_FigS2.pdf", figs2_new, width = 18, height = 10, units = "cm")


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
(t_result2 <- t.test(Av_guess_error ~ AgeGroup, data = means, var.equal = TRUE))
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
  theme_classic(base_size = 7)+ 
  scale_color_manual(values = c("#e5d200", "#999999")) +
  xlab("Age Group") + ylab("Learning Error") + theme(legend.position = "none")
p1  

p2 <- ggplot(means, aes(x = AgeGroup, y = Av_abs_guess_error, color = AgeGroup)) + 
  geom_hline(yintercept = 0) + 
  stat_summary(fun.data = "mean_cl_boot")+
  geom_quasirandom(alpha = 0.3) + 
  theme_classic(base_size = 7) +
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

Data3$AgeGroup <- factor(Data3$AgeGroup, levels = c("Children", "Adults"))
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

facet_names <- c(
  'Av_abs_guess_error = 0.14'="Learning Noise = 0.14",
  'Av_abs_guess_error = 0.22'="Learning Noise = 0.22",
  'Av_abs_guess_error = 0.31'="Learning Noise = 0.31"
)

bottomplot <- plot_model(m, terms = c("Entropy_z_z", "AgeGroup", "Av_abs_guess_error"), 
                   type = "pred") + 
  theme_classic(base_size = 7) + 
  scale_color_manual(values = c("#e5d200", "#999999")) +
  scale_fill_manual(values = c("#e5d200", "#999999")) + 
  xlab("Local Learning Potential") + 
  ylab ("Curiosity (Predicted)") + ggtitle("") +
  facet_grid(.~ facet, labeller = labeller(facet = facet_names)) + 
  labs(color = "Age Group") 
bottomplot

s2 <- (p1+p2)/bottomplot + plot_layout(guides = "collect")
s2

ggsave("../figures/Study3_SI_FigS3.pdf", s2, width = 18, height = 13, units = "cm")


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

Data3$AgeGroup <- factor(Data3$AgeGroup, levels = c("Adults", "Children"))

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
  theme_classic(base_size = 7) + 
  theme(legend.position = "right") + 
  ylab("Predictor") + 
  xlab(expression("Regression coefficient (log odds)"))
p_supp_ordinal

ggsave("../figures/Study3_SI_FigS4.pdf", p_supp_ordinal, width = 11, height = 8, units = "cm")

