@everywhere begin
    using SplitApplyCombine
    using Serialization
    using DataFrames
    using CSV
    using ProgressMeter
    using DataStructures
    include("model.jl")

    function simulate(;θs, θ_z,  N_trial=50, ε=0.3, p_reveal=.2, fixed=())
        mds = [MineData() for i in eachindex(θs)]
        map(1:N_trial) do i        
            state = (deepcopy(mds), ε, θ_z)
            if i <= length(fixed)        
                choice, reward = fixed[i]
                latent = missing
            else
                choice = sample(eachindex(θs))
                z = rand(Bernoulli(ε))
                reward = Int(rand(Bernoulli(z ? θ_z : θs[choice])))
                reveal = rand(Bernoulli(p_reveal))
                latent = Bool(reveal) ? z : missing  # missing means unobserved
            end
            
            observe!(mds[choice], reward, missing)  # compute features before seeing latent
            feats = features(mds, choice, ε, θ_z)
            mds[choice].zs[end] = latent

            (
                trial_num = i,
                subj_choice = choice,
                feedback = reward,
                latent,
                feats...,
            )
        end
    end
end
@everywhere SKIP_ARGMAX = true
function grid(;kws...)
    map(Iterators.product(values(kws)...)) do x
        (; zip(keys(kws), x)...)
    end[:]
end

G = grid(
    thetas = [
        [.5, .5],
        [.5, .8],
        [.2, .8],
        [.8, .8],
    ],
    agent_condition = [:bandit, :tycoon, :sheriff],
    first_fixed = [false],
    subj = 1:100
)
#=
g = first(G)
@time res = simulate(; θs=g.thetas, θ_z=0.5, N_trial=5)
invert(res).Mean_theta
=#

results = @showprogress pmap(G) do g
    θ_z = Dict(:bandit => 0, :tycoon => 1, :sheriff => 0.5)[g.agent_condition]
    fixed = g.first_fixed ? ((1,0), (1,0), (2, 0), (2, 1), (3, 1), (3, 1)) : ()
    sim = simulate(; θs=g.thetas, θ_z, fixed, N_trial=50)
    thetas = join(round.(Int, g.thetas .* 100), "_")
    subj_uid = string(rand(1:100000000); base=62)
    map(sim) do s
        (;subj_uid, g..., thetas, s...)
    end
end;

# %% --------
df = DataFrame(reduce(vcat, results))
select!(df, Not(:first_fixed))
for col in eachcol(df)
    if col[1] isa Float64
        @. col = round(col, sigdigits=5)
    end
end
df |> CSV.write("../Results/two_mine_incentives_sim2.csv")
run(`du -h "../Results/two_mine_incentives_sim2.csv"`)



# %% --------
Base.isnan(::String) = false
Base.isnan(::Symbol) = false
map(collect(pairs(eachcol(df)))) do (name, col)
    name => mean(isnan.(col))
end
