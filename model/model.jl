"""
z: intervention (0/1)
r: reward/gold (0/1)
ε: probability of an intervention
θ: probability that the mine produces gold (AKA mine value)
θ_z: probability that the mine produces gold if there is an intervention
f: probability density function for θ
"""

using Distributions
using Accessors

include("densities.jl")
include("keyedarray.jl")

# ---------- Basic generative model ---------- #

#=
z ~ Bernoulli(ε)
if z
    r ~ Bernolli(θ_z)
else
    r ~ Bernoulli(θ)
end
=#


"Prior probability of intervention
    P(z | ε)
"
prob_z(z; ε) = pdf(Bernoulli(ε), z)


"Probability of reward given intervention and mine value
    p(r | z, θ, θ_z)
"
function prob_r(r; z, θ, θ_z)
    p = Bool(z) ? θ_z : θ
    pdf(Bernoulli(p), r)
end


"Probability of reward and intervention given intervention probability and mine value
    p(r, z | θ, θ_z, ε)
"
function prob_rz(r, z; θ, θ_z, ε)
    prob_r(r; z, θ, θ_z) * prob_z(z; ε)
end


# ---------- Likelihood ---------- #

EventCounts = keyed_array_type(Int, 
    (false, true),  # reward: absent or present
    (false, true, missing);  # latent: absent, present, unobserved
    mutable=false)

"Probability of a sequence of rewards given mine value and intervention probability.
    p(r⃗ | ⃗z, θ, θ_z, ε)
"
function likelihood(ec::EventCounts, θ, θ_z, ε)
    # base probabilities
    p00 = prob_rz(0, 0; θ, θ_z, ε) # (1-θ) (1-ε)
    p01 = prob_rz(0, 1; θ, θ_z, ε) # (1-θ_z) ε
    p10 = prob_rz(1, 0; θ, θ_z, ε) # θ (1-ε)
    p11 = prob_rz(1, 1; θ, θ_z, ε) # θ_z ε

    # observed-vs-missing counts
    n00_obs = ec[false, false]
    n01_obs = ec[false, true]
    n0_miss = ec[false, missing]

    n10_obs = ec[ true, false]
    n11_obs = ec[ true, true]
    n1_miss = ec[ true, missing]

    pn0 = p00^n00_obs * p01^n01_obs * (p00 + p01)^n0_miss
    pn1 = p10^n10_obs * p11^n11_obs * (p10 + p11)^n1_miss
    return pn0 * pn1
end

# ---------- Posterior ---------- #

"Probability of mine value given observed rewards
    p(θ | rs, zs, ε, θ_z)
"
function make_posterior(ec::EventCounts, ε, θ_z)
    # we assume a uniform prior, so it's just the normalized likelihood)
    like = Density() do θ
        likelihood(ec, θ, θ_z, ε)
    end
    normalize(like)
end

struct BeliefState{N}
    ε::Float64
    θ_z::Float64
    event_counts::NTuple{N, EventCounts}
    posterior::NTuple{N, Density}
    current_obs::Union{Nothing, Tuple{Int, Bool}}  # mine, reward (for which we could observe latent)
end

function BeliefState{N}(ε, θ_z, event_counts=ntuple(i->zeros(EventCounts), N)) where N
    posterior = make_posterior.(event_counts, ε, θ_z)
    BeliefState(ε, θ_z, event_counts, posterior, nothing)
end

function observe_reward(b::BeliefState, mine::Int, r::Bool)
    @reset b.event_counts[mine][r, missing] += 1
    @reset b.posterior[mine] = make_posterior(b.event_counts[mine], b.ε, b.θ_z)
    @reset b.current_obs = (mine, r)
    b
end

function observe_latent(b::BeliefState, z::Bool)
    @assert !isnothing(b.current_obs)
    mine, reward = b.current_obs
    @reset b.event_counts[mine][reward, missing] -= 1
    @reset b.event_counts[mine][reward, z] += 1
    @reset b.posterior[mine] = make_posterior(b.event_counts[mine], b.ε, b.θ_z)
    @reset b.current_obs = nothing
    b
end

observe_latent(b::BeliefState, z::Missing) = @set b.current_obs = nothing

posterior(b::BeliefState, mine::Int) = b.posterior[mine]

"Probability of reward for a given mine under the current belief.
    ∫ f(θ) P(r | ε, θ, θ_z) dθ
"
function predictive_r(b::BeliefState, mine::Int)
    expectation(b.posterior[mine]) do θ
        expectation(Bernoulli(b.ε)) do z
            prob_r(true; z, θ, b.θ_z)
        end
    end |> Bernoulli
end

"Probability of intervention given the current belief and an observed reward.
    ∫ f(θ) P(z | ε, r, θ, θ_z) dθ
"
function predictive_z(b::BeliefState)
    mine, reward = b.current_obs
    (;ε, θ_z) = b
    expectation(b.posterior[mine]) do θ
        pz1 = prob_rz(reward, 1; θ, θ_z, ε)
        pz0 = prob_rz(reward, 0; θ, θ_z, ε)
        pz1 / (pz1 + pz0)
    end |> Bernoulli
end
