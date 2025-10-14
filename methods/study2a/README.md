Experiment code for Study 2a, "A computational approach to disentangling the triggers of curiosity in children and adults" (Liquin et al., submitted). Code was adapted from https://osf.io/3htpj/.

To preview the study locally, set up a local HTTP server in this directory. For example, with Python 3:

```
python -m http.server 8000
```

Proceed to http://localhost:8000/?cond=1.

Set the "cond" variable in the URL to view different conditions:
_Target of curiosity_: Local (conds 1, 2, 3), Global (conds 4, 5, 6).
_Intervening Agent_: Bandit (conds 1, 4), sheriff (conds 2, 5), tycoon (conds 3, 6).
