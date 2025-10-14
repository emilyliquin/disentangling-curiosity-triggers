# Overview

This repository contains data, modeling, analysis scripts, and methods for "A computational approach to disentangling the triggers of curiosity in children and adults" (Liquin et al., under review).

`data`: participant data

`model`: Bayesian model, which estimates several possible triggers of curiosity

`model_results`: Results from applying Bayesian model to human data (mine choices/outcomes)

`analysis`: Analysis scripts to produce figures and results

`methods`: Code/Materials for all experiments (see READMEs in individual experiment folders for more information)

Data & modeling details:

`Study 1a`: Participants' choices were yoked to participants' in Dorfman et al., 2019, "Causal inference about good and bad outcomes." Model is run on dataset `dorfman_exp1`, which is drawn from https://osf.io/3htpj/.

`Study 1b`: Participants made their own mine choices. Model is run on dataset `study1b`.

`Study 2a and Study 2b`: Participants' choices were yoked to participants' in Dorfman et al., 2019, "Causal inference about good and bad outcomes." Model is run on dataset `dorfman_exp1`, which is drawn from https://osf.io/3htpj/.

`Study 3`: Participants saw one of 11 fixed sequences of outcomes (single option - no choices). Model is run on datasets `study3_kids` and `study3_adults`.

`Study S1` (reported in SI): Participants saw either randomly determined choices, or made their own choices. Model is run on datasets `studySI1_freechoice` and `studySI1_fixedchoice`. For `studySI1_freechoice` only, we also model certain quantities as predictors of CHOICE, not just curiosity. This involves a slightly modified model, as participants rate curiosity AFTER observing an outcome, but they choose BEFORE observing an outcome.

`Study S2` (reported in SI): Participants made thier own choices. Model is run on datasets `studySI2_kids` and `studySI2_adults`.

# System Requirements

## Hardware requirements

Requires only a standard computer.

## Software requirements

### OS Requirements

This code has been tested on macOS Ventura (13.5) and Sequoia (15.7.1)

### Julia

Modeling code requires Julia (tested with 1.11.1 and 1.12.0)

For dependencies and version numbers, see `model/Manifest.toml`.

### R

Analysis code requires R (tested with 4.3.2)

Dependencies:

```
lme4
emmeans
car
dplyr
ggplot2
tidyr
jtools
ggeffects
sjPlot
ggbeeswarm
patchwork
gridExtra
rstatix
mlogit
Formula
```

# Installation Guide

For modeling results:

- [install julia](https://github.com/JuliaLang/juliaup) if you don't have it already. Installation should take only a few minutes.
- run `julia --project=. -e 'using Pkg; Pkg.resolve(); Pkg.instantiate()'` to install dependencies. This can take a few minutes to run.

For analysis code:

- [install R]() if you don't have it already. Installation should take only a few minutes
- Run the following code in R to install dependencies:

```
install.packages("remotes")
library(remotes)
install_version("lme4", version = "1.1-36")
install_version("emmeans", version = "1.8.9")
install_version("car", version = "3.1-2")
install_version("tidyverse", version = "2.0.0")
install_version("jtools", version = "2.2.2")
install_version("ggeffects", version = "1.6.0")
install_version("sjPlot", version = "2.8.16")
install_version("ggbeeswarm", version = "0.7.2")
install_version("patchwork", version = "1.2.0")
install_version("gridExtra", version = "2.3")
install_version("rstatix", version = "0.7.2")
install_version("mlogit", version = "1.1-1")
install_version("Formula", version = "1.2-5")
```

# Model

_Note: the main model runs in under a minute, so we do not provide a demo. See the instructions below to run the full model._

Key files:

- `main.jl` generates the result csvs.
- `model.jl` implements the basic Bayesian belief-updating model.
- `test_model.jl` tests the belief-updating (mostly the likelihood function).
- `features.jl` implements the features.

# Instructions for Use

Run `model/main.jl` to run the computational model on all data from these studies. Set your working directory to the model directory, then run main.jl:

```
cd model
julia --project=. main.jl
```

This will be printed in the terminal, indicating input and output files:

```
Reading file dorfman_exp1.csv
  using subject*condition as oid
  wrote ../model_results/dorfman_exp1.csv
Reading file study1b.csv
  wrote ../model_results/study1b.csv
Reading file study3_kids.csv
  setting ε=0.5
  wrote ../model_results/study3_kids.csv
Reading file study3_adults.csv
  setting ε=0.5
  wrote ../model_results/study3_adults.csv
Reading file StudySI1_fixedchoice.csv
  wrote ../model_results/StudySI1_fixedchoice.csv
Reading file StudySI1_freechoice.csv
  wrote ../model_results/StudySI1_freechoice.csv
Reading file StudySI2_kids.csv
  setting ε=1/3
  wrote ../model_results/StudySI2_kids.csv
Reading file StudySI2_adults.csv
  setting ε=1/3
  wrote ../model_results/StudySI2_adults.csv
```

This should take no more than a minute or two to run. If modeling results were reproduced correctly, all files in model_results will be overwritten with no changes to their contents.

To reproduce analyses:

- run `analysis/study1.R` to reproduce all analyses from Study 1.
- run `analysis/study2.R` to reproduce all analyses from Study 2.
- run `analysis/study3.R` to reproduce all analyses from Study 3.
- run `analysis/supplement/studySI1.R` to reproduce all analyses from Supplementary Study S1.
- run `analysis/supplement/studySI2.R` to reproduce all analyses from Supplementary Study S2.
