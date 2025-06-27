"Code for computing with probability density functions with domain [0, 1]"

using Distributions
using Statistics
using StatsBase
using Optim
using QuadGK: quadgk
using Cuba: cuhre

# not really a density, but here it is
function expectation(f::Function, d::Bernoulli)
    d.p ≈ 0. && return f(false)
    d.p ≈ 1. && return f(true)
    f(true) * d.p + f(false) * (1 - d.p)
end

struct Density{F<:Function}
    f::F
end
(d::Density)(x) = d.f(x)


"Mode of distribution with pdf f"
function StatsBase.mode(f::Density)
    # Check for the uniform case, where there isn't a well defined MAP
    if f(0.0) == f(0.1) == f(0.5) == f(1.0)
        return NaN
    end
    optimize(x->-f(x), 0, 1).minimizer
end

"Expectation of a distribution with pdf f
    E[f] = ∫ f(x) x dx
"
function expectation(f::Density)
    quadgk(0, 1) do x
        x * f(x)
    end |> first
end

"Expectation of a transformed variable g(x) where x has pdf f
    E[f] = ∫ f(x) g(x) dx
"
function expectation(g::Function, f::Density)
    quadgk(0, 1) do x
        g(x) * f(x)
    end |> first
end

"Variance of f
    Var[f] = ∫ f(x) (x - E[f])² dx
"
function variance(f::Density)
    μ = expectation(f)
    expectation(f) do x
        (x - μ) ^ 2
    end
end


"Makes a normalized version of the unnormalized pdf f"
function normalize(f::Density)
    normalizer = quadgk(0, 1) do x
        f(x)
    end |> first
    Density(x -> f(x) / normalizer)
end

"Surprisal of an observation x given a pdf f, -log(f(x))"
function surprisal(f::Density, x)
    -log(f(x))
end

"Differential entropy of a distribution with pdf f
    H(f) = E[I(x)]

    This is the most commonly used generalization of entropy to continuous
    distributions, but it may not be the best one. See:
    https://en.wikipedia.org/wiki/Limiting_density_of_discrete_points
"
function StatsBase.entropy(f::Density)
    expectation(f) do θ
        surprisal(f, θ)
    end
end

struct NaD end
StatsBase.entropy(::NaD) = NaN

"Integrates multivariate function f over the n-dimensional unit cube."
function integrate(f::Density, n; atol=1e-4)
    cuhre((x, y) -> y[1] = f(x), n, atol=atol).integral[1]
end

"Expectation of a multi-variate function given pdfs fs"
function multi_expectation(g::Function, fs::Vector{<:Density})
    integrate(length(fs)) do xs
        gx = g(xs)
        gx ≈ 0. && return 0.
        px = prod(f(x) for (f, x) in zip(fs, xs))
        px * gx
    end
end

