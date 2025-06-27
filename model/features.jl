
"Features that capture the unexpectedness of the just-seen reward."
function surprise_features(b::BeliefState, choice::Int, reward::Bool)
    @assert isnothing(b.current_obs)  # this is the belief before seeing the reward
    prior = b.posterior[choice]
    r_pred = predictive_r(b, choice)

    (;
        Mean_theta = expectation(prior),  # redundant; for compatiblity w/ analysis code
        RPE_MAP = reward - mode(prior),
        RPE_mean = reward - expectation(prior),
        RPE_full = reward - mean(r_pred),
        Surprise = -log(pdf(r_pred, reward)),
    )

end

"Features that capture the uncertainty and leaning potential in the reward-updated belief."
function learning_features(b::BeliefState)
    choice, reward = b.current_obs  # the just observed reward

    (;
        Entropy_z = entropy(predictive_z(b)),  # also EIG_z
        Entropy_theta = entropy(b.posterior[choice]),
        EIG_theta = eig_θz(b),
    )
end

"Features that depend on a goal (prediction or choice)."
function incentive_features(b::BeliefState)
    choice, _ = b.current_obs

    pmeans = expectation.(b.posterior)
    competitor = maximum(pmeans[i] for i in eachindex(pmeans) if i ≠ choice)

    (;
        VOI_z_choice = voi_z(choice_objective, b),
        VOI_z_prediction = voi_z(prediction_objective, b),
        VOI_theta_choice = voi_θ(choice_objective, b, choice),
        VOI_theta_prediction = voi_θ(prediction_objective, b, choice),

        Difference_50 = abs(pmeans[choice] - 0.5),
        Difference_competitor = abs(pmeans[choice] - competitor)
    )
end

"Features that might influence action selection."
function action_selection_features(b::BeliefState)
    @assert isnothing(b.current_obs)  # this is the belief before seeing the reward

    pmeans = expectation.(b.posterior)

    nested_feats = map(eachindex(b.posterior)) do choice
        (
            VOI_theta_choice = voi_θ(choice_objective, b, choice),
            VOI_theta_prediction = voi_θ(prediction_objective, b, choice),
            VOI_r_choice = voi_r(choice_objective, b, choice),
            VOI_r_prediction = voi_r(prediction_objective, b, choice),
            Entropy_r = entropy(predictive_r(b, choice)),
            EIG_theta_r = eig_θr(b, choice),
            Mean_theta = pmeans[choice],
            SD_theta = .√ variance(b.posterior[choice]),
            Difference_50 = abs(pmeans[choice] - 0.5),
            Difference_competitor = let
                competitor = maximum(pmeans[i] for i in eachindex(pmeans) if i ≠ choice)
                abs(pmeans[choice] - competitor)
            end,
        )
    end
    fnames = fieldnames(eltype(nested_feats))

    unnest = (
        Symbol(string(fn, "_", i)) => nested_feats[i][fn]
        for fn in fnames
        for i in eachindex(nested_feats)
    )
    (; unnest...)
end

# ---------- Expected learning features ---------- #

"Expected change in some function of the posteriors after observing the intervention"
function expected_learning_z(g, b::BeliefState)
    @assert !isnothing(b.current_obs)
    mine, reward = b.current_obs

    # expectation over the z value that you will observe
    z_pred = predictive_z(b)
    expected_val_after = expectation(z_pred) do z
        new_b = observe_latent(b, z)
        g(new_b)
    end
    expected_val_after - g(b)
end

"Expected change in some function of the posteriors after observing a reward"
function expected_learning_r(g, b::BeliefState, mine::Int)
    @assert isnothing(b.current_obs)

    # expectation over the r value that you will observe
    r_pred = predictive_r(b, mine)
    expected_val_after = expectation(r_pred) do r
        new_b = observe_reward(b, mine, r)
        g(new_b)
    end
    expected_val_after - g(b)
end

"Expected information gain about θ from learning the last z"
function eig_θz(b::BeliefState)
    @assert !isnothing(b.current_obs)
    mine, _ = b.current_obs
    expected_learning_z(b) do b
        -entropy(b.posterior[mine])
    end
end

"Expected information gain about θ from observing a reward from a given mine"
function eig_θr(b::BeliefState, mine::Int)
    expected_learning_r(b, mine) do b
        -entropy(b.posterior[mine])
    end
end

# ---------- Task-based features ---------- #

"Expected value for the choice task: maximum value."
function choice_objective(b::BeliefState)
    maximum(b.posterior) do f
        expectation(f)  # not posterior_predictive bc no interventions in test trials
    end
end

"Expected value for the prediction task: average prediction accuracy."
function prediction_objective(b::BeliefState)
    mean(b.posterior) do f
        p = expectation(f)
        max(p, 1 - p)
    end
end

"Expecteed improvement in task performance from learning the last z."
function voi_z(objective, b::BeliefState)
    expected_learning_z(b) do b
        objective(b)
    end
end

"Expected improvement in task performance from observing a reward."
function voi_r(objective, b::BeliefState, mine::Int)
    expected_learning_r(b, mine) do b
        objective(b)
    end
end

struct Dirac <: Function
    v::Float64
end
(d::Dirac)(x) = x == d.v ? Inf : 0.
expectation(d::Density{Dirac}) = d.f.v


"Expecteed improvement in task performance from perfectly learning θ of the chosen mine."
function voi_θ(objective, b::BeliefState, mine::Int)
    # NOTE: This implementation is much slower than the choice-specific version
    current = objective(b)
    expected_future = expectation(b.posterior[mine]) do θ
        @reset b.posterior[mine] = Density(Dirac(θ))
        objective(b)
    end
    expected_future - current
end
