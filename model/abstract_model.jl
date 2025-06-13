using StatsBase
using DataStructures
using Optim
using Sobol
using Memoize

Space = OrderedDict{Symbol,Any}

abstract type AbstractModel end

features(M::Type{AbstractModel}, state) = error("Not implemented")

likelihood_φ(m::AbstractModel, φ) = error("Not implemented")

parameter_bounds(m::AbstractModel)::Space = error("Not implemented")

# ---------- Likelihood ---------- #

function likelihood(m::M, state) where M <: AbstractModel
    φ = features(M, state)
    likelihood_φ(m, φ)
end

function act(m::M, state) where M <: AbstractModel
    sample(Weights(likelihood(m, state)))
end

struct Likelihood{M<:AbstractModel}
    φs::Vector{NamedTuple}
    actions::Vector{Int}

    Likelihood{M}(states, actions; featurized=false) where M <: AbstractModel = begin
        φs = featurized ? states : map(states) do s
            features(M, s)
        end
        new(φs, actions)
    end
end

function logp(k::Likelihood{M}, m::M) where M <: AbstractModel
    mapreduce(+, k.φs, k.actions) do φ, a
        log(likelihood_φ(m, φ)[a])
    end
end

function all_likelihoods(k::Likelihood{M}, m::M) where M <: AbstractModel
    mapreduce(hcat, k.φs, k.actions) do φ, a
        likelihood_φ(m, φ)
    end
end

# ---------- Fitting ---------- #

function continuous_bounds(space::Space)
    lo, hi = Float64[], Float64[]
    for spec in values(space)
        if spec isa Tuple
            push!(lo, spec[1]); push!(hi, spec[2])
        end
    end
    lo, hi
end

function combinations(space::Space)
    specs = filter(collect(values(space))) do spec
        spec isa Vector
    end
    Iterators.product(specs...)
end


function create_model(::Type{M}, x::Vector{T}, z) where M where T
    xs = Iterators.Stateful(x)
    zs = Iterators.Stateful(z)
    space = bounds(M)
    args = map(fieldnames(M)) do fn
        spec = space[fn]
        if spec isa Tuple
            first(xs)
        elseif spec isa Vector
            first(zs)
        else
            T(spec)
        end
    end
    M(args...)
end

# function rescale(x, lo, hi)

# end

# continuous_bounds(bounds(MGM))


@memoize function get_sobol(lower, upper, n)
    seq = SobolSeq(lower, upper)
    skip(seq, n)
    x0s = [Sobol.next!(seq) for i in 1:n]
end

function bfgs_random_restarts(loss, lower, upper, n_restart; max_err=n_restart/2)
    algorithms = [
        Fminbox(LBFGS()),
        Fminbox(LBFGS(linesearch=Optim.LineSearches.BackTracking())),
    ] |> Iterators.cycle |> Iterators.Stateful
    algo = first(algorithms)
    n_err = 0

    opts = map(get_sobol(lower, upper, n_restart)) do x0
        try
            optimize(loss, lower, upper, x0, algo, autodiff=:forward)
        catch err
            err isa InterruptException && rethrow(err)
            # @warn "First BFGS attempt failed" err linesearch=typeof(algo.method.linesearch!).name
            # try the other line search method
            algo = first(algorithms)  # this cycles
            try
                optimize(loss, lower, upper, x0, algo, autodiff=:forward)
            catch err
                err isa InterruptException && rethrow(err)
                @warn "Second BFGS attempt failed" err linesearch=typeof(algo.method.linesearch!).name
                n_err += 1
                if n_err >= max_err
                    @error "Too many optimization errors"
                    rethrow(err)
                end
                return missing
            end
        end
    end |> skipmissing |> collect
    isempty(opts) ? missing : partialsort(opts, 1; by=o->o.minimum)
end

function find_mle(k::Likelihood{M}; method=:bfgs, n_restart=20) where M <: AbstractModel
    space = bounds(M)
    lower, upper = continuous_bounds(space)
    space_size = upper .- lower
    @assert all(space_size .> 0)


    n_call = 0
    function make_loss(z)
        x -> begin
            n_call += 1
            model = create_model(M, x, z)
            # L1 = sum(abs.(x) ./ space_size)
            -logp(k, model) #+ 10 * L1
        end
    end

    results, elapsed = @timed map(combinations(space)) do z
        loss = make_loss(z)

        opt = begin
            if method == :samin
                x0 = lower .+ rand(length(lower)) .* space_size
                optimize(loss, lower, upper, x0, SAMIN(verbosity=0), Optim.Options(iterations=10^6))
            elseif method == :bfgs
                bfgs_random_restarts(loss, lower, upper, n_restart)
            end
        end
        ismissing(opt) && return missing
        model = create_model(M, opt.minimizer, z)
        model, -logp(k, model)
    end |> skipmissing |> collect 
    if isempty(results)
        @error("Could not fit $M to $(trials[1].wid)")
        error("Fitting error")
    end
    # @info "Fitting complete" n_call elapsed M
    models, losses = invert(results)
    i = argmin(losses)
    models[i], -logp(k, models[i])
end


abstract type SoftmaxModel <: AbstractModel end

function softmax(x)
    ex = exp.(x .- maximum(x))
    ex ./ sum(ex)
end

preferences(m::SoftmaxModel, φ) = error("Not implemented")
likelihood_φ(m::SoftmaxModel, φ) = softmax(preferences(m, φ))