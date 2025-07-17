using Test
using Distributions
include("model.jl")
include("test_utils.jl")

function sample_mine_data(N; θ, θ_z, ε, p_reveal=0.5)
    map(1:N) do i
        z = rand(Bernoulli(ε))
        r = rand(Bernoulli(z ? θ_z : θ))
        (;
            r_obs = r, 
            z_obs = rand(Bernoulli(p_reveal)) ? z : missing
        )
    end
end

function make_belief(data, prm)
    b = BeliefState{1}(prm.ε, prm.θ_z)
    for (r, z) in data
        b = observe_reward(b, 1, r)
        b = observe_latent(b, z)
    end
    b
end

combinations(xs, N) = Iterators.product(fill(xs,N)...)
    
function naive_likelihood(data, θ, θ_z, ε)
    mapreduce(+, combinations([false, true], length(data))) do zs
        legal = all(eachindex(data)) do i
            ismissing(data[i].z_obs) || data[i].z_obs == zs[i]
        end
        legal || return 0.  # i told you it was naive...
        mapreduce(*, eachindex(data)) do i
            prob_rz(data[i].r_obs, zs[i]; θ, θ_z, ε)
        end
    end
end

function prob_rz(r, ::Missing; θ, θ_z, ε)
    prob_rz(r, 0; θ, θ_z, ε) + prob_rz(r, 1; θ, θ_z, ε)
end

function naive_likelihood_two(data, θ, θ_z, ε)
    mapreduce(*, eachindex(data)) do i
        prob_rz(data[i].r_obs, data[i].z_obs; θ, θ_z, ε)
    end
end

@testset failfast=true "Clever likelihood matches naive likelihood" begin

    params = create_test_objects((;θ=(0.,1), θ_z=(0.,1), ε=(0.,1), p_reveal=(0.,1)); n_rand=2)
    
    for i in 1:length(params)
        @testset let prm = params[i]
            data = sample_mine_data(10; prm...)
            b = make_belief(data, prm)
            for θ in 0:.01:1
                @test naive_likelihood(data, θ, prm.θ_z, prm.ε) ≈ likelihood(b.event_counts[1], θ, prm.θ_z, prm.ε)
                @test naive_likelihood_two(data, θ, prm.θ_z, prm.ε) ≈ likelihood(b.event_counts[1], θ, prm.θ_z, prm.ε)
            end
        end
    end
end

function fit_beta(rs; prior=(1,1))
    α, β = prior
    α += sum(rs)
    β += length(rs) - sum(rs)
    Beta(α, β)
end

@testset failfast=true "posterior is Beta when ε = 0" begin

    params = create_test_objects((;θ=(0.,1), θ_z=(0.,1), ε=(0.,)))
    
    for i in 1:length(params)
        @testset let prm = params[i]
            data = sample_mine_data(30; prm...)
            b = make_belief(data, prm)
            f = b.posterior[1]
            beta = fit_beta(getfield.(data, :r_obs))
            for θ in 0:.01:1
                @test f(θ) ≈ pdf(beta, θ)
            end
        end
    end
end

@testset "posterior mode is mean reward when p_reveal=1." begin

    params = create_test_objects((;θ=(0.,1), θ_z=(0.,1), ε=(0.,0.7)))
    
    for i in 1:length(params)
        @testset let prm = params[i]
            data = sample_mine_data(50; prm..., p_reveal=1.)
            b = make_belief(data, prm)
            f = b.posterior[1]
            emp_mean = mean(d.r_obs for d in data if !d.z_obs)
            @test mode(f) ≈ emp_mean atol=1e-7
        end
    end
end
