Experiment code for Study S1 (fixed choice version), "A computational approach to disentangling the triggers of curiosity in children and adults" (Liquin et al., submitted). Code was adapted from https://osf.io/3htpj/.

To preview the study locally, set up a local HTTP server in this directory. For example, with Python 3:

```
python -m http.server 8000
```

Proceed to http://localhost:8000/?cond=1.

Set the "cond" variable in the URL to view different conditions:
_Incentivized Task_: Prediction (conds 1, 3, 5), Choice (conds 2, 4, 6).
_Intervening Agent_: Copper Tycoon (conds 1, 4), Copper Bandit (conds 2, 5), Sheriff (conds 3, 6).
