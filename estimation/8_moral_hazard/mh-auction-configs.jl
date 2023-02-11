##########################
# Set up auction configs #
##########################

include(joinpath(@__DIR__, "..", "auxiliary", "auction-configs.jl"))

cap = Inf

# Calculate baseline
baseline_actual = auc -> Config(
    auc;
    tag="baseline-actual",
    quantity=ActualPrediction()
)
baseline_estimated = auc -> Config(
    auc;
    tag="baseline-estimated",
    quantity=ModelPrediction()
)

# No risk, moral hazard capped at 20%
nr_mh_actual = auc -> Config(
    auc; 
    risk_adjust = NoRisk(),
    quantity = ActualPrediction(),
    hazard = MoralHazard(cap, auc.T),
    solver = NumericSolver(),
    tag="norisk-mh-actual-$cap"
)
nr_mh_estimated = auc -> Config(
    auc; 
    risk_adjust = NoRisk(),
    quantity = ModelPrediction(),
    hazard = MoralHazard(cap, auc.T),
    solver = NumericSolver(),
    tag="norisk-mh-estimated-$cap"
)

# Baseline moral hazard
mh_actual = auc -> Config(
    auc; 
    quantity = ActualPrediction(),
    hazard = MoralHazard(cap, auc.T),
    solver = NumericSolver(),
    tag="mh-actual-$cap"
)
mh_estimated = auc -> Config(
    auc; 
    quantity = ModelPrediction(),
    hazard = MoralHazard(cap, auc.T),
    solver = NumericSolver(),
    tag="mh-estimated-$cap"
)

# Lumpsum MH
lump_mh_actual = auc -> Config(
    auc;
    auctype=Lump(),
    quantity = ActualPrediction(),
    hazard = MoralHazard(cap, auc.T),
    tag="mh-lump-actual-$cap"
)
lump_mh_estimated = auc -> Config(
    auc;
    auctype=Lump(),
    quantity = ModelPrediction(),
    hazard = MoralHazard(cap, auc.T),
    tag="mh-lump-estimated-$cap"
)

# Mixed, 2:1 renegotiation
mh_two_one_actual = auc -> Config(
    auc; 
    quantity = ActualPrediction(),
    auctype = Mixed(2/3),
    hazard = MoralHazard(cap, auc.T),
    solver=NumericSolver(),
    tag="mh-mixed-two-one-actual-$cap"
)
mh_two_one_estimated = auc -> Config(
    auc; 
    quantity = ModelPrediction(),
    auctype = Mixed(2/3),
    hazard = MoralHazard(cap, auc.T),
    solver=NumericSolver(),
    tag="mh-mixed-two-one-estimated-$cap"
)

mh_one_two_actual = auc -> Config(
    auc; 
    quantity = ActualPrediction(),
    auctype = Mixed(1/3),
    hazard = MoralHazard(cap, auc.T),
    solver=NumericSolver(),
    tag="mh-mixed-1:2-actual-$cap"
)
mh_one_two_estimated = auc -> Config(
    auc; 
    quantity = ModelPrediction(),
    auctype = Mixed(1/3),
    hazard = MoralHazard(cap, auc.T),
    solver=NumericSolver(),
    tag="mh-mixed-1:2-estimated-$cap"
)

# Lump sum, 50/50 renegotiation
mh_fifty_fifty_actual = auc -> Config(
    auc; 
    quantity = ActualPrediction(),
    auctype = Mixed(1/2),
    hazard = MoralHazard(cap, auc.T),
    solver=NumericSolver(),
    tag="mh-mixed-fifty-fifty-actual-$cap"
)
mh_fifty_fifty_estimated = auc -> Config(
    auc; 
    quantity = ModelPrediction(),
    auctype = Mixed(1/2),
    hazard = MoralHazard(cap, auc.T),
    solver=NumericSolver(),
    tag="mh-mixed-fifty-fifty-estimated-$cap"
)

mh_mixedlump_actual = auc -> Config(
    auc; 
    quantity = ActualPrediction(),
    auctype = Mixed(0.001),
    hazard = MoralHazard(cap, auc.T),
    solver=NumericSolver(),
    tag="mh-mixed-0.001-actual-$cap"
)
mh_mixedlump_estimated = auc -> Config(
    auc; 
    quantity = ModelPrediction(),
    auctype = Mixed(0.001),
    hazard = MoralHazard(cap, auc.T),
    solver=NumericSolver(),
    tag="mh-mixed-0.001-estimated-$cap"
)


mixedlump_actual = auc -> Config(
    auc;
    auctype = Mixed(0.001),
    tag="mixed-0.001-actual",
    quantity=ActualPrediction()
)
mixedlump_estimated = auc -> Config(
    auc;
    auctype = Mixed(0.001),
    tag="mixed-0.001-estimated",
    quantity=ModelPrediction()
)
# Collect functions
config_fns = [
    baseline_actual,
    baseline_estimated,
    mh_actual,
    mh_estimated,
    lump_mh_actual,
    lump_mh_estimated,
    lump_estimated,
    lump_actual,
    mixedlump_estimated,
    mixedlump_actual, 

    nr_mh_actual,
    nr_mh_estimated,
    norisk_estimated,
    norisk_actual,

    # # 50/50
    mh_fifty_fifty_actual,
    mh_fifty_fifty_estimated,
    fifty_fifty_estimated,
    fifty_fifty_actual,

    # # 2:1
    mh_one_two_actual,
    mh_one_two_estimated,
    one_two_actual,
    one_two_estimated,

    mh_mixedlump_estimated,
    mh_mixedlump_actual, 
]
