using Test
using Distributions
include("model.jl")
include("features.jl")
include("test_utils.jl")

function make_belief(ε, θ_z)
    event_counts = ntuple(i -> EventCounts(rand(0:10, 2, 3)), 3)
    BeliefState{3}(ε, θ_z, event_counts)
end

@testset "Surprising rewards have higher EIG theta" begin
    for i in 1:1000
        b = make_belief(.3, .5)
        b1 = observe_reward(b, 1, true)
        b0 = observe_reward(b, 1, false)
        s1 = surprise_features(b, 1, true).Surprise
        s0 = surprise_features(b, 1, false).Surprise
        
        if abs(s1 - s0) > .01  # can get flips for small differences
            @test (s1 > s0) == (eig_θz(b1) > eig_θz(b0))
        end
    end
end

@testset failfast=true "incentive features are non-negative" begin
    for i in 1:1000
        b = observe_reward(make_belief(.3, .5), 1, rand(Bool))

        feats = incentive_features(b)
        @testset for fn in fieldnames(typeof(feats))
            @test feats[fn] >= -1e-10
        end
    end
end