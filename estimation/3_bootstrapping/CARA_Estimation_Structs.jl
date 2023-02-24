mutable struct Bidder
    bidder_seq_id::Int64
    bidder_id::String
    b_data::Array{Float64}
    score::Float64
    X::Array{Float64}
    fringe_ind::Int64
end

mutable struct MinimalAuction
    contract_no::Int64
    project_type_id::Int64
    num_bidders::Int64
    T::Int64
    bidders::Array{Bidder}
    optb_denom::Array{Float64}
    optb_num_coeff::Array{Float64}
    optb_num_main::Array{Float64}
    sum_qesq_sigsq::Float64
    sum_qe_co::Float64
    sum_qe_qa_sigsq::Float64
    q_a::Array{Float64}
    q_a_model::Array{Float64}
    q_o::Array{Float64}
    c::Array{Float64}
    sigma_sq::Array{Float64}
    H_coef1::Float64
    H_coef2::Float64
    H_exponent::Float64
    office_score::Float64
    ewo::Float64
    item_ids::Array{Int64}
    item_clusters::Array{Int64}
end

mutable struct Item_Instance
    contract_no::Int64 ## maps auctions the item is included in to the item's position
    seq_item_no::Int64
    auction::MinimalAuction
    num_obs::Int64
end

mutable struct Bidder_Instance
    contract_no::Int64 ## maps auctions the item is included in to the item's position
    bidder_seq_id::Int64
    auction::MinimalAuction
    num_obs::Int64
end

mutable struct Bidder_Instance_w_Skew
    contract_no::Int64 ## maps auctions the item is included in to the item's position
    bidder_seq_id::Int64
    auction::MinimalAuction
    num_obs::Int64
    skew_item_ids::Array{Float64}
    num_skew_obs::Int64
end

function constructMinimalAuction(auction_data::DataFrames.DataFrame, bidder_data::DataFrames.DataFrame, bidder_type_data::DataFrames.DataFrame, dollar_scale::Int64)

    contract_no = auction_data[!,:contract_no][1]
    project_type_id = auction_data[!,:project_type_id][1]

    q_a_model = convert(Array{Float64},auction_data[!, :q_at_model])
    q_e = convert(Array{Float64},auction_data[!,:q_ot])
    q_a = convert(Array{Float64},auction_data[!,:q_at])
    c_o = convert(Array{Float64},auction_data[!,:office_unit_price]/dollar_scale)
    sigma_t = convert(Array{Float64},auction_data[!, :sigma_t])

    item_ids = convert(Array{Float64},bidder_data[!,:item_id_sequential])
    item_clusters = convert(Array{Float64},bidder_data[!,:skew_cluster])

    ewo = convert(Array{Float64},auction_data[!,:extra_work_payment])[1]
    ewo = ewo/dollar_scale

    T = length(q_e)
    sigma_sq = sigma_t .* sigma_t

    sum_qesq_sigsq = sum((q_e[t]^2) / sigma_sq[t] for t in 1:T)
    sum_qe_co = sum((q_e[t]*c_o[t]) for t in 1:T)
    sum_qe_qa_sigsq = sum((q_e[t] * q_a_model[t]) / sigma_sq[t] for t in 1:T)

    denom = [c_o[t] - (q_e[t]/(sigma_sq[t]*sum_qesq_sigsq))*sum_qe_co for t in 1:T]
    num_coeff = [q_e[t]/(sigma_sq[t]*sum_qesq_sigsq) for t in 1:T]
    num_main = [(q_a_model[t]/sigma_sq[t]) - (num_coeff[t]*sum_qe_qa_sigsq) for t in 1:T]

    num_bidders = ncol(bidder_data) - 2 ## Accounting for new 'generic item ids'
    num_bidder_feat_cols = ncol(bidder_type_data)
    
    bidders = [ Bidder( 
                (i-2), 
                string(names(bidder_data)[i]),
                convert(Array{Float64},bidder_data[:,i]/dollar_scale),
                (((bidder_data[:,i])' * q_e) /dollar_scale),
                (permutedims(Vector(bidder_type_data[(3-2),4:num_bidder_feat_cols]))[1,:]), ## need to do df row to array conversion this way
                bidder_type_data[(i-2),3]
             ) for i = 3:(num_bidders+2)]

    bidders = sort!(bidders, by = x -> x.score)
    for i in 1:length(bidders)
        bidders[i].bidder_seq_id = i
    end

    ## compute coefficients for hazard rate approximation
    phi_approx_a = 1.702
    
    bidder_auction_mean = convert(Array{Float64},auction_data[!,:bidder_auction_mean])[1]
    bidder_auction_sigma_k = convert(Array{Float64},auction_data[!,:bidder_auction_sigma_k])[1]
    
    office_score = (c_o' * q_e)/dollar_scale
    
    H_coef1 = phi_approx_a / (bidder_auction_sigma_k * office_score)
    H_coef2 = exp( (phi_approx_a * bidder_auction_mean) / bidder_auction_sigma_k ) * (office_score)^(-phi_approx_a/bidder_auction_sigma_k)
    H_exponent = (-phi_approx_a)/bidder_auction_sigma_k
    
    return MinimalAuction(
                        contract_no, 
                        project_type_id, 
                        num_bidders, 
                        T, 
                        bidders,
                        denom, 
                        num_coeff, 
                        num_main, 
                        sum_qesq_sigsq, 
                        sum_qe_co, 
                        sum_qe_qa_sigsq, 
                        q_a, 
                        q_a_model, 
                        q_e, 
                        c_o, 
                        sigma_sq, 
                        H_coef1, 
                        H_coef2, 
                        H_exponent,
                        office_score, 
                        ewo, 
                        item_ids,
                        item_clusters)

end