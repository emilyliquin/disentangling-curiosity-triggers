"""
z: intervention (0/1)
r: reward/gold (0/1)
ε: probability of an intervention
θ: probability that the mine produces gold (AKA mine value)
θ_z: probability that the mine produces gold if there is an intervention
f: probability density function for θ
"""

using Distributions
using QuadGK
using Cuba
using Optim
using LinearAlgebra: LowerTriangular

FAST_MODE = false
SKIP_ARGMAX = false
TEST_INTERVENTION = false

include("pdfs.jl")

# ---------- Basic generative model ---------- #

#=
z ~ Bernoulli(ε)
if z = 1 
    r ~ Bernolli(θ)
else
    r ~ Bernoulli(θ_z)
end
=#

"Prior probability of intervention
    P(z | ε)
"
prob_z(z, ε) = pdf(Bernoulli(ε), z)


"Probability of reward given intervention and mine value
    p(r | z, θ, θ_z, ε)
"
function prob_r(r, z, θ, θ_z, ε)
    pdf(Bernoulli(z == 1 ? θ_z : θ), r)
end

prob_r(r, z, θ, ::Missing, ε) = prob_r(r, z, θ, 0.5, ε)


"Probability of reward and intervention given intervention probability and mine value
    p(r, z | θ, θ_z, ε)
"
function prob_rz(r, z, θ, θ_z, ε)
    prob_r(r, z, θ, θ_z, ε) * prob_z(z, ε)
end


"Probability of intervention conditioned on reward and mine value
    P(z | ε, θ, θ_z, r) ∝ p(z | ε) p(r | z, θ, θ_z, ε)
"
function prob_z(z, ε, θ, θ_z, r)
    unnormalized = map(0:1) do z
        prob_z(z, ε) * prob_r(r, z, θ, θ_z, ε)
    end
    unnormalized[z+1] / sum(unnormalized)
end

"Probability of intervention condioned on reward and a distribution over mine value
    ∫ f(θ) P(z | ε, r, θ, θ_z) dθ
"
function prob_z(z, ε, f::Function, θ_z, r)
    pz = quadgk(0, 1) do θ
        f(θ) * prob_z(z, ε, θ, θ_z, r)
    end |> first
end


# ---------- Likelihood ---------- #

struct MineData
    rs::Vector{Int}
    zs::Vector{Union{Missing,Int}}  # can be unobserved!
end

MineData() = MineData([], [])

function observe!(md::MineData, r, z)
    push!(md.rs, r)
    push!(md.zs, z)
end

function observe_z!(md::MineData, z)
    @assert ismissing(md.zs[end])
    md.zs[end] = z
end


#=
Now comes the full likelihood, the probability of the observed sequence of rewards.
This requires marginalizing over the sequence of interventions:
    p(r⃗ |θ, ε) = ∑_z⃗ ∏_i p(r_i, z_i, θ, ε)

The naive implementation is easy. Each trial can have an intervention or not, so
there are 2^N possible sequences. For each one, we loop through the sequence
of rewards and compute the joint probability of the reward and the (hypothetical) 
intervention. We multiply the trial probabilities and then add the probabilities
for different intervention sequences.

This is easy... but it has exponential run time and only works for
sequences with length less than about 20.
=#

"Probability of observed rewards and interventions given mine value and intervention probability.
    p(data |θ, ε)
"
function naive_likelihood(md::MineData, θ, θ_z, ε)
    mapreduce(+, combinations(0:1, length(md.rs))) do zs
        legal = all(eachindex(md.zs)) do i
            ismissing(md.zs[i]) || md.zs[i] == zs[i]
        end
        legal || return 0.  # i told you it was naive...
        mapreduce(*, md.rs, zs) do r, z  # total probability of each reward
            prob_rz(r, z, θ, θ_z, ε)
        end
    end
end

"All length N sequences of combinations of elements in xs"
combinations(xs, N) = Iterators.product(fill(xs,N)...)


#=
Fortunately, we can avoid this exponential blow up by noticing that the order
of past experiences doesn't matter. There are only four possible cases (reward
and intervention) you can observe, so we can summarize a sequence by the
number of times each occurs. This means, we can compute the probability of
each case once and then marginalize over the number of times each case could
have occurred given the number of times the reward was observed. We can 
treat the cases where reward is observed vs not observed separately because
the z are conditionally independent given r, θ, θz, and, ε.
=#

mutable struct EventCounts
    n1_::Int      # number of trials where r=1, z=?  # ? -> no observation
    n0_::Int      # number of trials where r=0, z=?
    n00::Int      # number of trials where r=0, z=0
    n01::Int      # number of trials where r=0, z=1
    n10::Int      # number of trials where r=1, z=0
    n11::Int      # number of trials where r=1, z=1
end
EventCounts() = EventCounts(0,0,0,0,0,0)
function observe!(ec::EventCounts, r, z)
    case = Symbol(string("n", r, ismissing(z) ? "_" : z))
    setfield!(ec, case, getfield(ec, case) + 1)
end
function EventCounts(md::MineData)
    ec = EventCounts()
    for (r, z) in zip(md.rs, md.zs)
        observe!(ec, r, z)
    end
    ec
end

"Probability of a sequence of rewards given mine value and intervention probability.
    p(r⃗ | ⃗z, θ, θ_z, ε)
"
function likelihood(ec::EventCounts, θ, θ_z, ε)
    # p01 is the probability of r=0 and z=1
    p00 = prob_rz(0, 0, θ, θ_z, ε)
    p01 = prob_rz(0, 1, θ, θ_z, ε)
    p10 = prob_rz(1, 0, θ, θ_z, ε)
    p11 = prob_rz(1, 1, θ, θ_z, ε)

    # probability of all cases where reward was not received: p(n0) 
    # we integrate out the number of times the agent intervened when we didn't
    # observe either way (z1).
    pn0 = mapreduce(+, 0:ec.n0_) do z1  # number of times z=1 *when it was unobserved*
        z0 = ec.n0_ - z1  # number of times z = 0 when unobserved
        n00 = ec.n00 + z0
        n01 = ec.n01 + z1
        pz1 = p00 ^ n00 * p01 ^ n01  # p(n0 | z1, θ, θ_z, ε)
        # the binomial coefficient accounts for all the different sequences
        # that result in the same counts
        binomial(ec.n0_, z1) * pz1
    end
    # same thing for cases where reward was received: p(n1)
    pn1 = mapreduce(+, 0:ec.n1_) do z1
        z0 = ec.n1_ - z1
        n10 = ec.n10 + z0
        n11 = ec.n11 + z1
        pz1 = p10 ^ n10 * p11 ^ n11
        binomial(ec.n1_, z1) * pz1
    end
    pn0 * pn1
end


likelihood(md::MineData, θ, θ_z, ε) = likelihood(EventCounts(md), θ, θ_z, ε)

# integrate over ε or θ_z if unknown
# we assume flat priors in both case

function likelihood(ec::EventCounts, θ, ::Missing, ε)
    quadgk(0, 1) do θ_z
        likelihood(ec, θ, θ_z, ε)
    end |> first
end

function likelihood(ec::EventCounts, θ, θ_z, ::Missing)
    quadgk(0, 1) do ε
        likelihood(ec, θ, θ_z, ε)
    end |> first
end

# ---------- Posterior ---------- #

"Probability of mine value given observed rewards
    p(θ | rs, zs, ε, θ_z)
"
function make_posterior(md::MineData, ε, θ_z)
    # we assume a uniform prior, so it's just the normalized likelihood)
    ec = EventCounts(md)
    normalize() do θ
         likelihood(ec, θ, θ_z, ε)
    end
end

# "Posterior knowing whether there was intervention on the last trial.
#     p(θ | rs, zs, ε, θ_z, z_N)
# "
# function make_posterior(md::MineData, ε, θ_z, z_N)
#     @assert ismissing(md.zs[end])
#     md.zs[end], tmp = z_N, md.zs[end]
#     ec = EventCounts(md)
#     md.zs[end] = tmp
#     normalize() do θ
#         likelihood(ec, θ, θ_z, ε)
#     end
# end

posterior_predictive(f::Function, ε, θ_z) = expectation(f) * (1-ε) + θ_z * ε
posterior_predictive(md::MineData, ε, θ_z) = posterior_predictive(make_posterior(md, ε, θ_z), ε, θ_z)

# ---------- Expected learning features ---------- #

"Expected change in some function of the posteriors after observing the intervention"
function expected_learning_z(g, mds, choice, ε, θ_z)
    ismissing(mds[choice].zs[end]) || return NaN  # already observed!

    posteriors::Vector{Function} = make_posterior.(mds, ε, θ_z)
    val_before = g(posteriors)

    # expectation over the z value that you will observe
    p_z1 = prob_z(1, ε, posteriors[choice], θ_z, mds[choice].rs[end])
    expected_val_after = mapreduce(+, 0:1, [1-p_z1, p_z1]) do z, p_z
        p_z ≈ 0. && return 0.  # can't normalize because likelihood is 0
        # update posterior
        new_md = deepcopy(mds[choice])
        new_md.zs[end] = z
        posteriors[choice] = make_posterior(new_md, ε, θ_z)

        p_z * g(posteriors)
    end
    expected_val_after - val_before
end

"Expected change in some function of the posteriors after observing a reward"
function expected_learning_r(g, mds, choice, ε, θ_z)
    posteriors::Vector{Function} = make_posterior.(mds, ε, θ_z)
    val_before = g(posteriors)

    # expectation over the r value that you will observe
    p_r = posterior_predictive(posteriors[choice], ε, θ_z)
    expected_val_after = mapreduce(+, 0:1, [1-p_r, p_r]) do r, p_r
        p_r ≈ 0. && return 0.
        # update posterior
        new_md = deepcopy(mds[choice])
        observe!(new_md, r, missing)  # missing => z is unobserved
        posteriors[choice] = make_posterior(new_md, ε, θ_z)

        p_r * g(posteriors)
    end
    expected_val_after - val_before
end

"Expected information gain about θ from learning the last z"
function eig_θz(mds, choice, ε, θ_z)
    expected_learning_z(mds, choice, ε, θ_z) do posteriors
        -entropy(posteriors[choice])
    end
end

"Expected information gain about θ from observing a reward from a given mine"
function eig_θr(mds, choice, ε, θ_z)
    expected_learning_r(mds, choice, ε, θ_z) do posteriors
        -entropy(posteriors[choice])
    end
end

# ---------- Task-based features ---------- #

struct DeltaDistribution <: Function
    θ::Float64
end
expectation(d::DeltaDistribution) = d.θ

function test_predictive(f, ε, θ_z)
    TEST_INTERVENTION ? posterior_predictive(f, ε, θ_z) : expectation(f)
end

"Expected value for the choice task: maximum value."
function choice_objective(posteriors, ε, θ_z)
    map(posteriors) do f
        test_predictive(f, ε, θ_z)
    end |> maximum
end

"Expected value for the prediction task: average prediction accuracy."
function prediction_objective(posteriors, ε, θ_z)
    map(posteriors) do f
        p = test_predictive(f, ε, θ_z)
        max(p, 1 - p)
    end |> mean
end

"Expecteed improvement in task performance from learning the last z."
function voi_z(objective, mds, choice, ε, θ_z)
    expected_learning_z(mds, choice, ε, θ_z) do posteriors
        objective(posteriors, ε, θ_z)
    end
end

"Expected improvement in task performance from observing a reward."
function voi_r(objective, mds, choice, ε, θ_z)
    expected_learning_r(mds, choice, ε, θ_z) do posteriors
        objective(posteriors, ε, θ_z)
    end
end

"Expecteed improvement in task performance from perfectly learning θ of the chosen mine."
function voi_θ(objective, mds, choice, ε, θ_z)
    # NOTE: This implementation is much slower than the choice-specific version
    posteriors::Vector{Function} = make_posterior.(mds, ε, θ_z)
    current = objective(posteriors, ε, θ_z)
    expected_future = expectation(posteriors[choice]) do θ
        posteriors[choice] = DeltaDistribution(θ)
        objective(posteriors, ε, θ_z)
    end
    expected_future - current
end

"Expected improvement in task performance from perfectly learning θ of every mine"
function vpi(objective, mds, ε, θ_z)
    FAST_MODE && return NaN
    posteriors = make_posterior.(mds, ε, θ_z)
    current = objective(posteriors, ε, θ_z)
    future = multi_expectation(posteriors) do x
        objective(DeltaDistribution.(x), ε, θ_z)
    end
    future - current
end

"Distribution of the best mine, integrating over p(θ) for each mine."
function argmax_distribution(fs; method=cuhre, atol=1e-4)
    (FAST_MODE || SKIP_ARGMAX) && return NaD()
    try
        n = length(fs)
        res = method(n, n-1, atol=atol) do x, y
            y .= 0
            i = argmax(x)
            i == n && return  # determined by other two b/c sum-to-one constraint
            px = prod(f(xi) for (f, xi) in zip(fs, x))
            y[i] = px
        end
        p = res.integral
        if sum(p) > 1
            # @warn "Error in numerical integration" p sum(p) expectation.(fs)
            # sum(p) > 1.01 && error("Large error in numerical integration.")
            p /= sum(p)
        end
        Categorical([p; 1 - sum(p)])
    catch err
        isa(err, InterruptException) && rethrow(err)
        @error "Error in argmax_distribution" err #p sum(p) expectation.(fs)
        NaD()
    end
end

"Expected information gain about argmax(θs) from learning the last z"
function eig_argmax_z(h0, mds, choice, ε, θ_z)
    (FAST_MODE || SKIP_ARGMAX) && return NaN
    h1 = expected_learning_z(mds, choice, ε, θ_z) do posteriors
        entropy(argmax_distribution(posteriors))
    end
    h0 - h1
end

"Expected information gain about argmax(θs) from learning the last r"
function eig_argmax_r(h0, mds, choice, ε, θ_z)
    (FAST_MODE || SKIP_ARGMAX) && return NaN
    h1 = expected_learning_r(mds, choice, ε, θ_z) do posteriors
        entropy(argmax_distribution(posteriors))
    end
    h0 - h1
end

# ---------- Putting it all together ---------- #

function features(mds, choice, ε, θ_z)
    mdc = mds[choice]
    current_reward = mdc.rs[end]

    posteriors = make_posterior.(mds, ε, θ_z)

    # "prior" means before observing the current reward
    chosen_prior = make_posterior(MineData(mdc.rs[1:end-1], mdc.zs[1:end-1]), ε, θ_z)
    chosen_posterior = posteriors[choice]

    θ̂_chosen = mode(chosen_prior)
    θ̄_chosen = expectation(chosen_prior)
    predictive = Bernoulli(posterior_predictive(chosen_prior, ε, θ_z))
    pz = prob_z(1, ε, chosen_posterior, θ_z, current_reward)

    entropy_argmax = entropy(argmax_distribution(posteriors))
    pmeans = expectation.(posteriors)
    competitor = maximum(pmeans[i] for i in eachindex(pmeans) if i ≠ choice)

    (
        N_gold = sum(mdc.rs),
        N_rock = sum(mdc.rs .== 0),

        MAP_theta = θ̂_chosen,
        Mean_theta = θ̄_chosen,
        # Mean_theta_unchosen = θ̄_chosen_unchosen,
        Var_theta = variance(chosen_prior),

        RPE_MAP = current_reward - θ̂_chosen,
        RPE_mean = current_reward - θ̄_chosen,
        RPE_full = current_reward - mean(predictive),
        
        Surprise = -log(pdf(predictive, current_reward)),
        Confidence = -entropy(predictive),
        
        Prob_z = pz,
        Entropy_z = entropy(Bernoulli(pz)),  # also EIG_z
        Entropy_theta = entropy(chosen_posterior),
        Entropy_theta_last = entropy(chosen_prior),
        EIG_theta = eig_θz(mds, choice, ε, θ_z),

        Entropy_argmax = entropy_argmax,
        EIG_argmax = eig_argmax_z(entropy_argmax, mds, choice, ε, θ_z),

        VOI_z_choice = voi_z(choice_objective, mds, choice, ε, θ_z),
        VOI_theta_choice = voi_θ(choice_objective, mds, choice, ε, θ_z),
        VPI_choice = vpi(choice_objective, mds, ε, θ_z),
        
        VOI_z_prediction = voi_z(prediction_objective, mds, choice, ε, θ_z),
        VOI_theta_prediction = voi_θ(prediction_objective, mds, choice, ε, θ_z),
        VPI_prediction = vpi(prediction_objective, mds, ε, θ_z),

        Difference_50 = abs(pmeans[choice] - 0.5),
        Difference_competitor = abs(pmeans[choice] - competitor)
    )
end
