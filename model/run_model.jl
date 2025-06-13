using Distributed
using CSV
using ProgressMeter

# %% --------

@everywhere begin
    include("model.jl")
    include("choice_model.jl")
    using DataFrames
    using CSV
    using SplitApplyCombine
    FAST_MODE = false

    function parse_state_actions(data; after_observe, ε=0.3)
        # ε = only(unique(data.latent_prob))
        agent = split(only(unique(data.agent_condition)), " ")[end]
        θ_z = Dict("bandit" => 0, "tycoon" => 1, "sheriff" => 0.5)[agent]
        @assert length(unique(data.oid)) == 1

        mds = [MineData() for i in 1:3]
        map(eachrow(data)) do row
            choice, reward = row.subj_choice, row.feedback
            latent = Bool(row.reveal) ? row.latent_agent : missing # missing means unobserved

            alt_mds = deepcopy(mds)
            if after_observe
                observe!(alt_mds[choice], reward, missing)  # but before seeing latent
            end
            state = (alt_mds, ε, θ_z)

            observe!(mds[choice], reward, latent)
            (state, choice)
        end
    end

    function curiosity_features(data::AbstractDataFrame)
        state_actions = parse_state_actions(data; after_observe=true)
        map(state_actions) do ((mds, ε, θ_z), choice)
            features(mds, choice, ε, θ_z)
        end |> DataFrame
    end

    function action_features(data::AbstractDataFrame)
        state_actions = parse_state_actions(data; after_observe=false)
        map(state_actions) do (state, action)

            mds, ε, θ_z = state
            posteriors = make_posterior.(mds, ε, θ_z)
            μ = expectation.(posteriors)
            σ = .√ variance.(posteriors)
            entropy_argmax = entropy(argmax_distribution(posteriors))
            φ = map(eachindex(mds)) do choice
                (
                    VOI_theta_choice = voi_θ(choice_objective, mds, choice, ε, θ_z),
                    VOI_theta_prediction = voi_θ(prediction_objective, mds, choice, ε, θ_z),
                    VOI_r_choice = voi_r(choice_objective, mds, choice, ε, θ_z),
                    VOI_r_prediction = voi_r(prediction_objective, mds, choice, ε, θ_z),
                    Entropy_r = entropy(Bernoulli(posterior_predictive(posteriors[choice], ε, θ_z))),
                    EIG_theta_r = eig_θr(mds, choice, ε, θ_z),
                    Mean_theta = μ[choice],
                    SD_theta = σ[choice],
                    Difference_50 = abs(μ[choice] - 0.5),
                    Difference_competitor = let
                        competitor = maximum(μ[i] for i in eachindex(μ) if i ≠ choice)
                        abs(μ[choice] - competitor)
                    end,
                    Entropy_argmax = entropy_argmax,
                    EIG_argmax = eig_argmax_r(entropy_argmax, mds, choice, ε, θ_z)
                )
            end |> invert
            
            x = OrderedDict(
                Symbol(string(fn, "_", i)) => v
                for fn in fieldnames(typeof(φ))
                for (i, v) in enumerate(getfield(φ, fn))
            )
            x[:Entropy_argmax] = entropy_argmax
            x
        end |> DataFrame
    end
end
 
function parallel_features(compute_features, data::DataFrame)
    split_data = collect(groupby(data, :oid))
    preds = @showprogress pmap(compute_features, split_data)
    reduce(vcat, preds)
end

# %% ==================== Simulate dorfman with varying latent ====================

@everywhere FAST_MODE = true

data = DataFrame!(CSV.File("../Data/Sequences/Other/dorfman_exp1.csv"))
data[!, :oid] = repeat(1:fld(size(data, 1), 50), inner=50)
data[!, :reveal] .= 0 
data.agent_condition = map(data.condition) do c
    ["bandit", "tycoon", "sheriff"][c]
end

split_data = collect(groupby(data, :oid));
εs = .1:.2:.9

results = @showprogress pmap(Iterators.product(split_data, εs)) do (dd, ε)
    state_actions = parse_state_actions(dd; after_observe=true, ε)
    df = map(state_actions) do ((mds, ε, θ_z), choice)
        features(mds, choice, ε, θ_z)
    end |> DataFrame
    df = hcat(dd, df)
    df.latent_prob .= ε
    df
end;

df = reduce(vcat, results);
df |> CSV.write("../Results/dorfman_vary_latent_prob.csv")

# %% ==================== Other stuff ====================

hcat(data, cpreds) |> CSV.write("../Results/IncentivesMany/CuriosityFeatures.csv")  #output file
apreds = parallel_features(action_features, data);
hcat(data, apreds) |> CSV.write("../Results/IncentivesMany/ActionFeatures.csv")  #output file


# %% --------
data = DataFrame!(CSV.File("../Data/IncentivesMany/IncentivesMany_Full-DataObserve.csv"))
data.subj_choice .+= 1
mkpath("../Results/IncentivesMany/")
cpreds = parallel_features(curiosity_features, data);
hcat(data, cpreds) |> CSV.write("../Results/IncentivesMany/CuriosityFeatures.csv")  #output file
apreds = parallel_features(action_features, data);
hcat(data, apreds) |> CSV.write("../Results/IncentivesMany/ActionFeatures.csv")  #output file

# %% --------
version = "Incentives_FixedChoice"
data = DataFrame!(CSV.File("../Data/$version/Incentives_FixedChoice-DataObserve.csv"))
data.subj_choice .+= 1
mkpath("../Results/$version/")
cpreds = parallel_features(curiosity_features, data);
hcat(data, cpreds) |> CSV.write("../Results/$version/CuriosityFeatures.csv")
apreds = parallel_features(action_features, data);
hcat(data, apreds) |> CSV.write("../Results/$version/ActionFeatures.csv")

# %% --------

# data = DataFrame!(CSV.File("../Data/IncentivesMany/IncentivesMany_Full-DataTest.csv"))



# %% --------
function fit_choice_model(data)
    states, _, actions = invert(mapreduce(parse_state_actions, vcat, collect(groupby(data, :oid))))
    choice_features = @showprogress pmap(states; batch_size=50) do s
        features(ChoiceModel, s)
    end

    L = Likelihood{ChoiceModel}(choice_features, actions; featurized=true);
    model, nll = find_mle(L)
    fn = fieldnames(typeof(model))
    DataFrame([fn .=> getfield.([model], fn); :nll => nll])
end

choice_fits = combine(fit_choice_model, groupby(data, :task_condition))

# %% --------

let
    data = DataFrame!(CSV.File("../Data/CMC_AdultData_Clean.csv"));  #change to file name
    rename!(data, :agent_cond => :agent_condition)
    data[!, :reveal] .= 0
    data[!, :latent_prob] .= 0.33333;  #latent probability
    preds = parallel_curiosity_features(data)
    hcat(data, preds) |> CSV.write("../Results/CMC_AdultData_Output.csv")  #output file
end
let
    data = DataFrame!(CSV.File("../Data/CMC_KidData_Clean.csv"));  #change to file name
    rename!(data, :agent_cond => :agent_condition)
    data[!, :reveal] .= 0
    data[!, :latent_prob] .= 0.33333;  #latent probability
    preds = parallel_curiosity_features(data)
    hcat(data, preds) |> CSV.write("../Results/CMC_KidData_Output.csv")  #output file
end

# %% --------

# NOTE: to run "../Data/CMC_AdultData_Clean.csv" checkout commit 800ff

# data = DataFrame!(CSV.File("../Data/CMC_AdultData_Clean.csv"));  #change to file name
# data[!, :participant] = data.oid;  #unique integer id (called participant, but this changes oid to participant)
# data[!, :latent_prob] .= 0.33333;  #latent probability
# preds = get_predictions(data)
# hcat(data, preds) |> CSV.write("../Results/CMC_AdultData_Output.csv")  #output file

# data = DataFrame!(CSV.File("../Data/KidDataTest.csv"));  #change to file name
# data[!, :participant] = data.oid;  #unique integer id (called participant, but this changes oid to participant)
# data[!, :latent_prob] .= 0.33333;  #latent probability
# preds = get_predictions(data)
# hcat(data, preds) |> CSV.write("../Results/CMC_KidData_Output.csv")  #output file

# # data = DataFrame!(CSV.File("../Data/ESC_RL_Choice-Sequences.csv"));  #change to file name
# # data[!, :participant] = data.oid;  #unique integer id (called participant, but this changes oid to participant)
# # data[!, :latent_prob] .= 0.3;  #latent probability
# # preds = get_predictions(data)
# # hcat(data, preds) |> CSV.write("../Results/choice_test_replicate.csv")  #output file



# # data = DataFrame!(CSV.File("../Data/Sequences/Other/dorfman_exp1.csv"))
# # data[!, :participant] = repeat(1:fld(size(data, 1), 50), inner=50)
# # preds = get_predictions(data)
# # hcat(data, preds) |> CSV.write("../Results/dorfman_exp1.csv")


