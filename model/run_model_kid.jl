using Distributed
@everywhere include("kid_model.jl")
using SplitApplyCombine
using CSV
using ProgressMeter
@everywhere using DataFrames

FAST_MODE = true

@everywhere function get_predictions_one(data)
    history = [Int[], Int[], Int[]]
    ε = only(unique(data.latent_prob))
    cond = only(unique(data.condition))
    θ_z = [0., 1., 0.5][only(unique(data.condition))] ###### CHECK THIS FOR KID STUDIES!!!!!!
    participant = only(unique(data.participant))
    println("Processing data for participant $participant. Theta is $θ_z for condition $cond.")

    map(eachrow(data)) do row
        choice, reward = row.subj_choice, row.feedback
        push!(history[choice], reward)
        features(history, choice, ε, θ_z)
    end |> DataFrame
end

function get_predictions(data)
    split_data = collect(groupby(data, :participant))
    preds = @showprogress map(get_predictions_one, split_data)
    reduce(vcat, preds)
end


data = CSV.read("../Data/Sims_JobTalk.csv", DataFrame);  #change to file name
data[!, :latent_prob] .= 0.3;  #latent probability
preds = get_predictions(data)
hcat(data, preds) |> CSV.write("../Results/Sims_JobTalk_Output.csv")  #output file


# data = CSV.read("../Data/CMC/CMC2BW_Kid-Final_Clean.csv", DataFrame);  #change to file name
# data[!, :participant] = data.oid;  #unique integer id (called participant, but this changes oid to participant)
# data[!, :latent_prob] .= 0.5;  #latent probability
# preds = get_predictions(data)
# hcat(data, preds) |> CSV.write("../Results/CMC2BW_Kid-Final_Clean_Output.csv")  #output file

# data2 = CSV.read("../Data/CMC/CMC2BW_Adult-Final_Clean.csv", DataFrame);  #change to file name
# data2[!, :participant] = data2.oid;  #unique integer id (called participant, but this changes oid to participant)
# data2[!, :latent_prob] .= 0.5;  #latent probability
# preds = get_predictions(data2)
# hcat(data2, preds) |> CSV.write("../Results/CMC2BW_Adult-Final_Clean_Output.csv")  #output file


# data = CSV.read("../Data/ESC_RL-ManipulateRatings-Pilot1.csv", DataFrame);  #change to file name
# data[!, :latent_prob] .= 0.3;  #latent probability
# preds = get_predictions(data)
# hcat(data, preds) |> CSV.write("../Results/ESC_RL-ManipulateRatings-Pilot1-Output.csv")  #output file

# data = CSV.read("../Data/CMC/CMC2BW_KidPilot2_Clean.csv", DataFrame);  #change to file name
# data[!, :participant] = data.oid;  #unique integer id (called participant, but this changes oid to participant)
# data[!, :latent_prob] .= 0.5;  #latent probability
# preds = get_predictions(data)
# hcat(data, preds) |> CSV.write("../Results/CMC2BW_KidPilot2_Output.csv")  #output file


# data = CSV.read("../Data/CMC/CMC2BW_AdultPilot3_Clean.csv");  #change to file name
# data[!, :participant] = data.oid;  #unique integer id (called participant, but this changes oid to participant)
# data[!, :latent_prob] .= 0.5;  #latent probability
# preds = get_predictions(data)
# hcat(data, preds) |> CSV.write("../Results/CMC2BW_AdultPilot3_Output.csv")  #output file


# data = CSV.read("../Data/CMC/CMC2BW_KidPilot_Clean.csv");  #change to file name
# data[!, :participant] = data.oid;  #unique integer id (called participant, but this changes oid to participant)
# data[!, :latent_prob] .= 1/3;  #latent probability
# preds = get_predictions(data)
# hcat(data, preds) |> CSV.write("../Results/CMC2BW_KidPilot_Output.csv")  #output file


# data = CSV.read("../Data/CMC/CMC2BW_AdultPilot2_Clean.csv");  #change to file name
# data[!, :participant] = data.oid;  #unique integer id (called participant, but this changes oid to participant)
# data[!, :latent_prob] .= 1/3;  #latent probability
# preds = get_predictions(data)
# hcat(data, preds) |> CSV.write("../Results/CMC2BW_AdultPilot2_Output.csv")  #output file


# data = CSV.read("../Data/Sims_sys20trial.csv");  #change to file name
# preds = get_predictions(data)
# hcat(data, preds) |> CSV.write("../Results/Sims_sys20trial_output.csv")  #output file


# data = CSV.read("../Data/Sims_ep0.2.csv");  #change to file name
# preds = get_predictions(data)
# hcat(data, preds) |> CSV.write("../Results/Sims_ep0.2_output.csv")  #output file

# data = CSV.read("../Data/Sims_ep0.4.csv");  #change to file name
# preds = get_predictions(data)
# hcat(data, preds) |> CSV.write("../Results/Sims_ep0.4_output.csv")  #output file


# data = CSV.read("../Data/Sims_15_trials_2.csv");  #change to file name
# preds = get_predictions(data)
# hcat(data, preds) |> CSV.write("../Results/Sims_15_trials_output_2.csv")  #output file

# data = CSV.read("../Data/Sims_15_trials_3.csv");  #change to file name
# preds = get_predictions(data)
# hcat(data, preds) |> CSV.write("../Results/Sims_15_trials_output_3.csv")  #output file


# data = CSV.read("../Data/CMC_AdultData_Clean.csv");  #change to file name
# data[!, :participant] = data.oid;  #unique integer id (called participant, but this changes oid to participant)
# data[!, :latent_prob] .= 1/3;  #latent probability
# preds = get_predictions(data)
# hcat(data, preds) |> CSV.write("../Results/CMC_AdultData_Output.csv")  #output file

# data = CSV.read("../Data/CMC_KidData_Clean.csv");  #change to file name
# data[!, :participant] = data.oid;  #unique integer id (called participant, but this changes oid to participant)
# data[!, :latent_prob] .= 1/3;  #latent probability
# preds = get_predictions(data)
# hcat(data, preds) |> CSV.write("../Results/CMC_KidData_Output_New.csv")  #output file

# # data = CSV.read("../Data/ESC_RL_Choice-Sequences.csv");  #change to file name
# # data[!, :participant] = data.oid;  #unique integer id (called participant, but this changes oid to participant)
# # data[!, :latent_prob] .= 0.3;  #latent probability
# # preds = get_predictions(data)
# # hcat(data, preds) |> CSV.write("../Results/choice_test_replicate.csv")  #output file



# # data = CSV.read("../Data/Sequences/Other/dorfman_exp1.csv")
# # data[!, :participant] = repeat(1:fld(size(data, 1), 50), inner=50)
# # preds = get_predictions(data)
# # hcat(data, preds) |> CSV.write("../Results/dorfman_exp1.csv")


