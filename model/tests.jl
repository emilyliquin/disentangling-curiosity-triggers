using Test
include("model.jl")

@testset "Clever likelihood matches naive likelihood" begin
    for i in 1:100
        θ = rand()
        θ_z = rand([0,0.343,1])
        ε = rand()
        rs = rand(0:1, rand(1:10))
        z = rand(0:1)
        @test naive_likelihood(rs, θ, θ_z, ε) ≈ likelihood(rs, θ, θ_z, ε)
        @test naive_likelihood(rs, θ, θ_z, ε, z) ≈ likelihood(rs, θ, θ_z, ε, z)
    end
end

function fit_beta(rs; prior=(1,1))
    α, β = prior
    α += sum(rs)
    β += length(rs) - sum(rs)
    Beta(α, β)
end

@testset "Features match a Beta distribution when ε ≈ 0" begin
    for i in 1:100
        θ = rand()
        ε = 1e-10
        θ_z = 0.
        rs = rand(0:1, 30)
        posterior = make_posterior(rs, ε, θ_z)
        beta = fit_beta(rs)
        @test mode(beta) ≈ mode(posterior) atol = 1e-4
        @test entropy(beta) ≈ entropy(posterior)
        @test mean(beta) ≈ expectation(posterior)
        @test var(beta) ≈ variance(posterior)

    end
end
