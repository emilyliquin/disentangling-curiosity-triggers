@everywhere begin
    using SplitApplyCombine
    using Serialization
    using DataFrames
    using CSV
    using ProgressMeter
    using DataStructures
    include("model.jl")
    include("choice_model.jl")

    function simulate(m::ChoiceModel; θs, θ_z,  N_trial=20, ε=0.3, p_reveal=.2, fixed=())
        mds = [MineData() for i in eachindex(θs)]
        map(1:N_trial) do i        
            state = (deepcopy(mds), ε, θ_z)
            φ = features(typeof(m), state)
            if i <= length(fixed)        
                choice, reward = fixed[i]
                latent = missing
            else
                choice_probs = likelihood_φ(m, φ)
                choice = sample(Weights(choice_probs))
                z = rand(Bernoulli(ε))
                reward = Int(rand(Bernoulli(z ? θ_z : θs[choice])))
                reveal = rand(Bernoulli(p_reveal))
                latent = Bool(reveal) ? z : missing  # missing means unobserved
            end
            observe!(mds[choice], reward, latent)

            feats = OrderedDict(
                Symbol(string(fn, "_", i)) => v
                for fn in fieldnames(typeof(φ))
                for (i, v) in enumerate(getfield(φ, fn))
            )
            (
                trial_num = i,
                subj_choice = choice,
                feedback = reward,
                feats...
            )
        end
    end
end

function grid(;kws...)
    map(Iterators.product(values(kws)...)) do x
        (; zip(keys(kws), x)...)
    end[:]
end

# %% --------
G = grid(
    beta = [10, 20, 50],
    thetas = [
        [.20, .65, .80],
        # [.20, .35, .80],
        # [.45, .55, .70],
        # [.30, .50, .70],
        [.25, .50, .75],
    ],
    agent_condition = [:bandit, :tycoon, :sheriff],
    task_condition = [:Prediction, :Choice],
    first_fixed = [true],
    subj = 1:1000
)

results = @showprogress pmap(G) do g
    θ_z = Dict(:bandit => 0, :tycoon => 1, :sheriff => 0.5)[g.agent_condition]
    fixed = g.first_fixed ? ((1,0), (1,0), (2, 0), (2, 1), (3, 1), (3, 1)) : ()
    β = (g.task_condition .== [:Choice, :Prediction]) .* g.beta
    sim = simulate(ChoiceModel(β..., 0, 0); θs=g.thetas, θ_z, fixed, N_trial=30)
    thetas = join(round.(Int, g.thetas .* 100), "_")
    subj_uid = string(rand(1:100000000); base=62)
    map(sim) do s
        (;subj_uid, g..., thetas, s...)
    end
end;

df = DataFrame(reduce(vcat, results))
df |> CSV.write("../Results/choice_prediction_sim_forced9.csv")
