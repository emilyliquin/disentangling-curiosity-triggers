Code for generating the curiosity-predicting features.

To reproduce the results:

- [install julia](https://github.com/JuliaLang/juliaup) if you don't have it already. The results were generated with julia 1.11.1, but it should work with newer versions.
- run `julia --project=. -e 'using Pkg; Pkg.resolve(); Pkg.instantiate()'` to install dependencies.
- run `julia main.jl` to generate the features.

Key files:

- `main.jl` generates the result csvs.
- `model.jl` implements the basic Bayesian belief-updating model.
- `test_model.jl` tests the belief-updating (mostly the likelihood function).
- `features.jl` implements the features.
