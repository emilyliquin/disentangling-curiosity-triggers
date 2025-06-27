using Distributed
using CSV, DataFrames
using StatsBase
using DataStructures: OrderedDict

# %% --------
include("model.jl")
include("features.jl")
mkpath("../model_results")

function prepare_data(df::SubDataFrame)
    # Handle agent_condition case like at line 104
    θ_z = if hasproperty(df, :condition)
        icond::Int = only(unique(df.condition))
        [0., 1., 0.5][icond]
    else
        scond::String = split(only(unique(df.agent_condition)), " ")[end]
        Dict("bandit" => 0., "tycoon" => 1., "sheriff" => 0.5)[scond]
    end
    
    ε = hasproperty(df, :latent_prob) ? only(unique(df.latent_prob)) : 0.3
    
    trials = map(eachrow(df)) do row
        (;
            choice = row.subj_choice + 1,  # convert to one-indexing
            reward_obs = Bool(row.feedback),
            latent_obs = get(row, :reveal, 0) == 1 ? Bool(row.latent_agent) : missing,
        )
    end
    
    (;θ_z, ε, trials)
end

function compute_features(subject_data::NamedTuple; include_incentive=true, include_action_selection=false)
    (;θ_z, ε, trials) = subject_data
    
    b_prior = BeliefState{3}(ε, θ_z)
    map(trials) do (;choice, reward_obs, latent_obs)
        b_post = observe_reward(b_prior, choice, reward_obs)  # but BEFORE observing latent
        
        feats = (;
            surprise_features(b_prior, choice, reward_obs)...,
            learning_features(b_post)...,
            (include_incentive ? incentive_features(b_post) : ())...,
            (include_action_selection ? action_selection_features(b_prior) : ())...,
        )
        
        # todays posterior is tomorrow's prior
        b_prior = observe_latent(b_post, latent_obs)  # also marks trial as done (b.curent_obs = nothing)
        
        feats
    end |> DataFrame
end

function compute_features(data::DataFrame; kwargs...)
    grouped = groupby(data, :oid)
    mapreduce(vcat, grouped) do subj_data
        prepped = prepare_data(subj_data)
        compute_features(prepped; kwargs...)
    end
end

# %% --------


function process_file(fn::String; overwrite=true)
    out_fn = "../model_results/" * fn
    if !overwrite && isfile(out_fn)
        println("Skipping $out_fn because it already exists")
        return
    end
    println("Reading file $fn")
    df = CSV.read("../data/" * fn, DataFrame)

    if fn == "dorfman_exp1.csv"
        println("  using subject*condition as oid")
        df.oid = df.subject .* df.condition
    elseif startswith(fn, "study3_",)
        println("  setting ε=0.5")
        df.latent_prob .= 0.5
    elseif startswith(fn, "StudySI2")
        println("  setting ε=1/3")
        df.latent_prob .= 1/3
    end
    include_action_selection = fn == "StudySI1_freechoice.csv"

    features = compute_features(df; include_action_selection)
    CSV.write(out_fn, hcat(df, features))
    println("  wrote $out_fn")
end

input_files = [
    "dorfman_exp1.csv",
    "study1b.csv",
    "study3_kids.csv",
    "study3_adults.csv",
    "StudySI1_fixedchoice.csv",
    "StudySI1_freechoice.csv",
    "StudySI2_kids.csv",
    "StudySI2_adults.csv",
]

foreach(process_file, input_files)

