This repository contains data, modeling, analysis scripts, and methods for "A computational approach to disentangling the triggers of curiosity in children and adults" (Liquin et al., in prep).

`data`: participant data
`model`: Bayesian model, which estimates several possible triggers of curiosity
`model_results`: Results from applying Bayesian model to human data (mine choices/outcomes)
`analysis`: Analysis scripts to produce figures and results
`methods`: Code/Materials for all experiments

Data & modeling details:

`Study 1a`: Participants' choices were yoked to participants' in Dorfman et al., 2019, "Causal inference about good and bad outcomes." Model is run on dataset `dorfman_exp1`, which is drawn from https://osf.io/3htpj/.
`Study 1b`: Participants made their own mine choices. Model is run on dataset `study1b`.
`Study 2a and Study 2b`: Participants' choices were yoked to participants' in Dorfman et al., 2019, "Causal inference about good and bad outcomes." Model is run on dataset `dorfman_exp1`, which is drawn from https://osf.io/3htpj/.
`Study 3`: Participants saw one of 11 fixed sequences of outcomes (single option - no choices). Model is run on datasets `study3_kids` and `study3_adults`.
`Study S1` (reported in SI): Participants saw either randomly determined choices, or made their own choices. Model is run on datasets `studySI1_freechoice` and `studySI1_fixedchoice`. For `studySI1_freechoice` only, we also model certain quantities as predictors of CHOICE, not just curiosity. This involves a slightly modified model, as participants rate curiosity AFTER observing an outcome, but they choose BEFORE observing an outcome.
`Study S2` (reported in SI): Participants made thier own choices. Model is run on datasets `studySI2_kids` and `studySI2_adults`.
