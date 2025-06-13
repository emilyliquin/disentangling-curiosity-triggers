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

const FAST_MODE = false
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

"Probability of a sequence of rewards given mine value and intervention probability.
    p(r⃗ |θ, ε)
"
function naive_likelihood(rs, θ, θ_z, ε)
    mapreduce(+, combinations(0:1, length(rs))) do zs
        mapreduce(*, rs, zs) do r, z  # total probability of each reward
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

#Possible values of (r, z)
@isdefined(CASES) || const CASES = [
    (0, 0),
    (0, 1),
    (1, 0),
    (1, 1)
]

"Probability of a sequence of rewards given mine value and intervention probability.
    p(r⃗ |θ, θ_z, ε)
"
function likelihood(rs, θ, θ_z, ε)
    case_probs = map(CASES) do (r, z)
        prob_rz(r, z, θ, θ_z, ε)
    end

    case_n = zeros(4)
    n1_ = sum(rs)  # number of times r is 1
    n0_ = length(rs) - n1_  # number of times r is 0

    p0 = mapreduce(+, 0:n0_) do n01
        case_n[1] = n0_ - n01
        case_n[2] = n01
        prz = case_probs[1] ^ case_n[1] * case_probs[2] ^ case_n[2]
        # the binomial coefficient accounts for all the different sequences
        # that result in the same counts
        binomial(n0_, n01) * prz
    end
    p1 = mapreduce(+, 0:n1_) do n11
        case_n[3] = n1_ - n11
        case_n[4] = n11
        prz = case_probs[3] ^ case_n[3] * case_probs[4] ^ case_n[4]
        binomial(n1_, n11) * prz
    end
    p0 * p1
end

#= 
Finally, we need a version of the likelihood that conditions on the
presence of intervention on the last trial. This is easy in the naive
implementation: we simply ignore any sequence that violates the
observed z value.
=#


"Likelihood knowing whether there was intervention on the last trial.
    p(r⃗ |θ, θ_z, ε, z_N)
"
function naive_likelihood(rs, θ, θ_z, ε, z_N)
    mapreduce(+, combinations(0:1, length(rs))) do zs
        zs[end] != z_N && return 0.  # violates observation
        mapreduce(*, rs, zs) do r, z
            prob_rz(r, z, θ, θ_z, ε)
        end
    end
end

#=
The clever version is a bit tricky. We have to limit the considered
case counts to be only those consistent with both the revealed
reward AND the observed intervention. To do so, we remove one "free"
z value, making us loop over a smaller number of possible interventions
and then add back in the case that we know for sure happened on the
inside of the loop.
=#

"Likelihood knowing whether there was intervention on the last trial.
    p(r⃗ |θ, θ_z, ε, z_N)
"
function likelihood(rs, θ, θ_z, ε, z)
    case_probs = map(CASES) do (r, z)
        prob_rz(r, z, θ, θ_z, ε)
    end
    r_N = rs[end]
    
    last_case = findfirst(CASES) do case
        case == (rs[end], z)
    end  

    # leave out the last trial when considering possible counts
    n1_ = sum(rs[1:end-1])
    n0_ = length(rs) - 1 - n1_
    case_n = zeros(4)

    p0 = mapreduce(+, 0:n0_) do n01
        case_n[1] = n0_ - n01
        case_n[2] = n01
        case_n[last_case] += 1  # add the last trial here
        prz = case_probs[1] ^ case_n[1] * case_probs[2] ^ case_n[2]
        binomial(n0_, n01) * prz
    end
    p1 = mapreduce(+, 0:n1_) do n11
        case_n[3] = n1_ - n11
        case_n[4] = n11
        case_n[last_case] += 1  # add the last trial here
        prz = case_probs[3] ^ case_n[3] * case_probs[4] ^ case_n[4]
        binomial(n1_, n11) * prz
    end
    p0 * p1
end

# ---------- Posterior ---------- #

"Makes a normalized version of the unnormalize pdf f"
function normalize(f)
    normalizer = quadgk(0, 1) do x
        f(x)
    end |> first
    x -> f(x) / normalizer
end

"Probability of mine value given observed rewards
    p(θ | rs, θ_z, ε)
"
function make_posterior(rs, ε, θ_z)
    # we assume a uniform prior, so it's just the normalized likelihood
    normalize() do θ
         likelihood(rs, θ, θ_z, ε)
    end
end

"Posterior knowing whether there was intervention on the last trial.
    p(θ | rs, ε, z_N)
"
function make_posterior(rs, ε, θ_z, z)
    normalize() do θ
        likelihood(rs, θ, θ_z, ε, z)
    end
end

function make_posteriors(history, ε, θ_z)::Vector{Function}
    map(history) do rs
        make_posterior(rs, ε, θ_z)
    end
end

# ---------- Mine Features ---------- #

"Expected information gain about θ learning the last z"
function eig_θz(rs, ε, θ_z)
    # entropy without knowing z
    f0 = make_posterior(rs, ε, θ_z)
    h0 = entropy(f0)

    # expectation over the z value that you will observe
    p_z1 = prob_z(1, ε, f0, θ_z, rs[end])
    mapreduce(+, 0:1, [1-p_z1, p_z1]) do z, p_z
        p_z ≈ 0. && return 0.  # can't normalize because likelihood is 0
        h1 = entropy(make_posterior(rs, ε, θ_z, z))
        p_z * (h0 - h1)  # information gain is reduction in entropy
    end
end

"Mode of distribution with pdf f"
function Distributions.mode(f)
    # Check for the uniform case, where there isn't a well defined MAP
    if f(0.1) == f(0.2) == f(0.3)
        return NaN
    end
    optimize(x->-f(x), 0, 1).minimizer
end

"Expectation of a distribution with pdf f
    E[f] = ∫ f(x) x dx
"
function expectation(f::Function)
    quadgk(0, 1) do x
        x * f(x)
    end |> first
end

"Expectation of a transformed variable g(x) where x has pdf f
    E[f] = ∫ f(x) g(x) dx
"
function expectation(g::Function, f::Function)
    quadgk(0, 1) do x
        g(x) * f(x)
    end |> first
end

"Variance of f
    Var[f] = ∫ f(x) (x - E[f])² dx
"
function variance(f::Function)
    μ = expectation(f)
    expectation(f) do x
        (x - μ) ^ 2
    end
end

"Probability of reward given posterior over mine value
    p(r_{t+1} | rs, ε) = ∫ f(θ) p(r | θ, θ_z, ε) dθ
                       = ∫ f(θ) [θ (1-ε) + θ_z ε dθ
                       = [ ∫ f(θ) θ dθ ] (1-ε) + θ_z ε
                       = E[f] (1-ε) + θ_z ε
"
posterior_predictive(f::Function, ε, θ_z) = expectation(f) * (1-ε) + θ_z * ε
posterior_predictive(rs::Vector{Int}, ε, θ_z) = posterior_predictive(make_posterior(rs, ε, θ_z), ε, θ_z)

"Surprisal AKA information content of an observation given a pdf"
function surprisal(f, x)
    -log(f(x))
end

"Entropy of a distribution with pdf f
    H(f) = E[I(x)]"
function Distributions.entropy(f::Function)
    expectation(f, θ->surprisal(f, θ))
end

# ---------- Best mine features ---------- #

struct NaD end
Distributions.entropy(::NaD) = NaN

"Distribution of the best mine, integrating over p(θ) for each mine."
function argmax_distribution(fs; method=cuhre, atol=1e-4)
    FAST_MODE && return NaD()
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
        @error "Error in argmax_distribution" err p sum(p) expectation.(fs)
        NaD()
    end
end

"Expectation of some function g of the posterior distributions after observing the intervention"
function expected_learning_z(g, history, choice, ε, θ_z)
    posteriors = make_posteriors(history, ε, θ_z)

    # expectation over the z value that you will observe
    p_z1 = prob_z(1, ε, posteriors[choice], θ_z, history[choice][end])
    mapreduce(+, 0:1, [1-p_z1, p_z1]) do z, p_z
        p_z ≈ 0. && return 0.  # can't normalize because likelihood is 0
        posteriors[choice] = make_posterior(history[choice], ε, θ_z, z)
        p_z * g(posteriors)
    end
end

"Expected information gain about argmax(θs) from learning the last z"
function eig_argmax_z(h0, history, choice, ε, θ_z)
    FAST_MODE && return NaN
    h1 = expected_learning_z(history, choice, ε, θ_z) do posteriors
        entropy(argmax_distribution(posteriors))
    end
    h0 - h1
end

# ---------- Task-based features ---------- #

struct DeltaDistribution <: Function
    θ::Float64
end
expectation(d::DeltaDistribution) = d.θ

"Expected value for the choice task: maximum value."
function choice_objective(posteriors, ε, θ_z)
    map(posteriors) do f
        posterior_predictive(f, ε, θ_z)
    end |> maximum
end

"Expected value for the prediction task: average prediction accuracy."
function prediction_objective(posteriors, ε, θ_z)
    map(posteriors) do f
        p = posterior_predictive(f, ε, θ_z)
        # TODO: is this correct? It seems like uncertainty over θ doesn't matter...
        max(p, 1 - p)
    end |> mean
end

"Expecteed improvement in task performance from learning the last z."
function voi_z(objective, history, choice, ε, θ_z)
    FAST_MODE && return NaN
    current = objective(make_posteriors(history, ε, θ_z), ε, θ_z)
    future = expected_learning_z(history, choice, ε, θ_z) do posteriors
        objective(posteriors, ε, θ_z)
    end
    future - current
end

"Expecteed improvement in task performance from perfectly learning θ of the chosen mine."
function voi_θ(objective, history, choice, ε, θ_z)
    # NOTE: This implementation is much slower than the choice-specific version
    posteriors = make_posteriors(history, ε, θ_z)
    current = objective(posteriors, ε, θ_z)
    future = expectation(posteriors[choice]) do θ
        posteriors[choice] = DeltaDistribution(θ)
        objective(posteriors, ε, θ_z)
    end
    future - current
end

"Integrates multivariate function f over the n-dimensional unit cube."
function integrate(f, n; atol=1e-4)
    cuhre((x, y) -> y[1] = f(x), n, atol=atol).integral[1]
end

"Expectation of a multi-variate function given pdfs fs"
function multi_expectation(g::Function, fs::Vector{T}) where T <: Function
    integrate(length(fs)) do xs
        gx = g(xs)
        gx ≈ 0. && return 0.
        px = prod(f(x) for (f, x) in zip(fs, xs))
        px * gx
    end
end

"Expected improvement in task performance from perfectly learning θ of every mine"
function vpi(objective, history, ε, θ_z)
    FAST_MODE && return NaN
    posteriors = make_posteriors(history, ε, θ_z)
    current = objective(posteriors, ε, θ_z)
    future = multi_expectation(posteriors) do x
        objective(DeltaDistribution.(x), ε, θ_z)
    end
    future - current
end


# function vpi(f1, f2)
#     future_max = hcubature([0.,0], [1.,1], abstol=1e-5) do (θ1, θ2)
#         p = f1(θ1) * f2(θ2)
#         p * max(θ1, θ2)
#     end |> first
#     current_max = max(expectation(f1), expectation(f2))
#     future_max - current_max
# end


# ---------- Putting it all together ---------- #

function features(history, choice, ε, θ_z)
    chosen_rewards = history[choice]
    current_reward = chosen_rewards[end]

    posteriors = make_posteriors(history, ε, θ_z)

    # "prior" means before observing the current reward
    chosen_prior = make_posterior(chosen_rewards[1:end-1], ε, θ_z)
    chosen_posterior = posteriors[choice]

    θ̂_chosen = mode(chosen_prior)
    θ̄_chosen = expectation(chosen_prior)
    predictive = Bernoulli(posterior_predictive(chosen_prior, ε, θ_z))
    pz = prob_z(1, ε, chosen_posterior, θ_z, current_reward)

    entropy_argmax = entropy(argmax_distribution(posteriors))

    (
        N_gold = sum(chosen_rewards),
        N_rock = sum(chosen_rewards .== 0),

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
        EIG_theta = eig_θz(chosen_rewards, ε, θ_z),

        # Entropy_argmax = entropy_argmax,
        # EIG_argmax = eig_argmax_z(entropy_argmax, history, choice, ε, θ_z),

        VOI_z_choice = voi_z(choice_objective, history, choice, ε, θ_z),
        VOI_theta_choice = voi_θ(choice_objective, history, choice, ε, θ_z),
        VPI_choice = vpi(choice_objective, history, ε, θ_z),
        
        VOI_z_prediction = voi_z(prediction_objective, history, choice, ε, θ_z),
        VOI_theta_prediction = voi_θ(prediction_objective, history, choice, ε, θ_z),
        VPI_prediction = vpi(prediction_objective, history, ε, θ_z),
    )
end