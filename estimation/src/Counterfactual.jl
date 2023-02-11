module Counterfactual

using Memoize
using FiniteDiff
using OrdinaryDiffEq
using SCIP
using BlackBoxOptim
using Dates
using CSV
using DataFrames
using Distributions
using QuadGK
using Plots
using ForwardDiff
using NLsolve
using StatsBase
using JLD2
using Suppressor

# Optimizer imports
using JuMP
using Gurobi
using Ipopt
import Optim
import MathOptInterface

# The constant term used to determine the coarseness of the quadrature.
const EVALMAX = 1_000_000
const GUROBI = parse(Bool, get(ENV, "JLGUROBI", "false"))
const ALPHA_EPSILON = 1e-4 # ± for alpha range

const GRB_ENV = Ref{Gurobi.Env}()
function __init__()
    if GUROBI
        @info "Using Gurobi"
        GRB_ENV[] = Gurobi.Env()
    else
        @warn "Not using Gurobi"
    end
    return
end

# Generate auction types
include("auction.jl")

"""
    Risk aversion types

Abstract types to determine what the risk aversion is for the bidders.
"""
abstract type RiskAversion end
struct HomogeneousRisk <: RiskAversion
    gamma::Float64
end
struct HeterogeneousRisk <: RiskAversion
    coef::Float64
    fe::Float64
end

"""
    Auction structure

Structs to define whether to estimate a lump sum, scaling, or mixed counterfactual.
"""
abstract type AuctionType end
struct Lump <: AuctionType end
struct Scaling <: AuctionType end
struct Mixed <: AuctionType
    lambda::Float64
end

# Default constructor
Mixed() = new(0.5) # Default to 0.5 if not given

"""
    Riskiness

Structs to modify the imprecision of estimates.
"""
abstract type RiskType end
struct SymmetricRisk <: RiskType end
struct RiskScaling <: RiskType 
    delta::Float64
end
NoRisk() = RiskScaling(0.0)
StandardRisk() = RiskScaling(1.0)

"""
    Moral hazard

Whether to allow for item fudging, and if so, the unit cap and item number cap.
"""
abstract type HazardType end
struct NoHazard <: HazardType end
mutable struct MoralHazard <: HazardType
    cap::Float64
    n_items::Int
    y_bar::Vector{Float64}
end

MoralHazard(cap::Float64, n_items::Int) = MoralHazard(cap, n_items, zeros(1))

"""
    Extra work orders

Whether there is an additional term in the ODE.
"""
struct ExtraWorkOrder
    amount::Float64
end
NoEWO() = ExtraWorkOrder(0.0)

"""
    Solvers

Whether to use the exact solver or the JuMP one.
"""
abstract type Solver end
struct ExactSolver <: Solver end
struct NumericSolver <: Solver end

"""
    Estimate types

Whether to use the engineer estimates (q_o) or the model predicted ones (q_a_model).

Note that using `Perfect <: Foresight` overrides this setting and forces the use of q_a exact.
"""
abstract type QuantityType end
struct ActualPrediction <: QuantityType end
struct EngineerPrediction <: QuantityType end
struct ModelPrediction <: QuantityType end

"""
    Entry costs

How much it costs a bidder to enter and how many entrants there are.
"""
struct EntryParams
    amount::Float64
    n_entrants::Int
end

EntryParams(n::Int) = EntryParams(0.0, n)

"""
    Minimum bid setup

Defines the minimum bid of an auction.
"""
struct BidLimit
    fraction::Float64
end

StandardBid() = BidLimit(0.0)

"""
    Monopolist conditions

How high a multiple the monopolist can take.
`multiple` is multiplied by the office score
`c'q_o`.
"""
abstract type MonopolyConditions end
struct FixedAmount <: MonopolyConditions
    amount::Float64
end
struct ScaledMonopoly <: MonopolyConditions
    multiple::Float64
end
ScaledMonopoly() = ScaledMonopoly(1.25)

"""
    Config

Counterfactual estimation structs.
"""
struct Config{
    R<:RiskAversion,
    A<:AuctionType,
    Rt<:RiskType,
    H<:HazardType,
    E<:ExtraWorkOrder,
    S<:Solver,
    Q<:QuantityType,
    En<:EntryParams,
    B<:BidLimit,
    M<:MonopolyConditions
}
    risk_aversion::R
    auctype::A
    risk_adjust::Rt
    hazard::H
    ewo::E
    solver::S
    quantity::Q
    entry::En
    bid_limit::B
    tag::String
    monopoly::M
end

function Base.String(config::Config)
    return config.tag
end

function Config(
    auction::Auction;
    risk_aversion::Union{RiskAversion,Type{<:RiskAversion}}=
    HeterogeneousRisk(auction.alpha_coef, auction.auction_fe),
    auctype::AuctionType=Scaling(),
    risk_adjust::RiskType=StandardRisk(),
    hazard::HazardType=NoHazard(),
    ewo::ExtraWorkOrder=NoEWO(),
    solver::Solver=ExactSolver(),
    quantity::QuantityType=ModelPrediction(),
    entry::EntryParams=EntryParams(auction.num_bidders),
    bid_limit::BidLimit=StandardBid(),
    tag::String="",
    monopoly::MonopolyConditions=ScaledMonopoly()
)
    if hazard isa MoralHazard
        y_bar = calc_y_bar(
            auction,
            estimated(auction, quantity);
            cap=hazard.cap,
            n_items=hazard.n_items
        )

        hazard = MoralHazard(
            hazard.cap,
            hazard.n_items,
            y_bar
        )
    end

    if risk_aversion isa Type{HeterogeneousRisk}
        risk_aversion = HeterogeneousRisk(auction.alpha_coef, auction.auction_fe)
    end

    if !(hazard isa NoHazard)
        solver = NumericSolver()
    end

    return Config(
        risk_aversion,
        auctype,
        risk_adjust,
        hazard,
        ewo,
        solver,
        quantity,
        entry,
        bid_limit,
        tag,
        monopoly
    )
end

# Change only the entrants of a configuration.
function entrants(config::Config, n::Int, cost=missing)
    # Make new entrant struct
    cost = ismissing(cost) ? config.entry.amount : cost
    new_entry = EntryParams(cost, n)

    # Make new configuration
    new_config = Config(
        config.risk_aversion,
        config.auctype,
        config.risk_adjust,
        config.hazard,
        config.ewo,
        config.solver,
        config.quantity,
        new_entry,
        config.bid_limit,
        config.tag,# * "-n$n-cost$cost",
        config.monopoly
    )

    return new_config
end

# Change lambda & k of a configuration.
function reparameterize(config::Config, K, λ, m::MonopolyConditions; n=config.entry.n_entrants)
    # Make new entrant struct
    new_entry = EntryParams(K, n)

    # New lambda
    new_ewo = ExtraWorkOrder(λ)

    # Make new configuration
    new_config = Config(
        config.risk_aversion,
        config.auctype,
        config.risk_adjust,
        config.hazard,
        new_ewo,
        config.solver,
        config.quantity,
        new_entry,
        config.bid_limit,
        config.tag,
        m,
    )

    return new_config
end

function make_mixed(config::Config)
    # Make new entrant struct
    new_auc = Mixed(0.0)

    # Make new configuration
    new_config = Config(
        config.risk_aversion,
        new_auc,
        config.risk_adjust,
        config.hazard,
        config.ewo,
        config.solver,
        config.quantity,
        config.entry,
        config.bid_limit,
        config.tag,
        congig.monopoly,
    )

    return new_config
end

function make_lump(config::Config)
    # Make new configuration
    new_config = Config(
        config.risk_aversion,
        Lump(),
        config.risk_adjust,
        config.hazard,
        config.ewo,
        config.solver,
        config.quantity,
        config.entry,
        config.bid_limit,
        config.tag,
        config.monopoly,
    )

    return new_config
end

# File inclusion
include("bids.jl")
include("analytic.jl")
include("convenience.jl")
include("io.jl")
include("ce.jl")
include("cost-bounds.jl")

# Exports
export Config, Auction, load_auctions, calc_boundary, equilibrium, entrants,
    HomogeneousRisk, HeterogeneousRisk, # Gamma types
    Lump, Scaling, Mixed, # Auction structure
    NoRisk, RiskScaling, SymmetricRisk, # Scaling for item uncertainty
    NoHazard, MoralHazard, # Moral hazard types
    ExtraWorkOrder, # EWO info
    ExactSolver, NumericSolver, # Solver types
    ActualPrediction, EngineerPrediction, ModelPrediction, # q type to use
    EntryParams, # Information on entry
    BidLimit, StandardBid, # Whether there's a minimum bid
    ObservedAlpha, RandomAlpha, SpecificAlpha, static_utility,
    UniformAlpha,
    MonopolyConditions, ScaledMonopoly, FixedAmount,
    monopoly_rent, get_alphas, entry_utility,
    office_score,
    reparameterize # For changing the values inside a config struct

end # module
