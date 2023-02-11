##########################
# Set up auction configs #
##########################

# Calculate baseline
baseline = auc -> Config(auc; tag="baseline")
baseline_actual = auc -> Config(auc; tag="baseline-actual", quantity=ActualPrediction())

# Baseline (calculated using Mixed(0))
baseline_actual_bkp = auc -> Config(
    auc;
    quantity=ActualPrediction(),
    auctype=Mixed(1),
    tag="baseline-bkp-actual"
)
baseline_estimated_bkp = auc -> Config(
    auc;
    quantity=ModelPrediction(),
    auctype=Mixed(1),
    tag="baseline-bkp-estimated"
)

# No risk
norisk_actual = auc -> Config(
    auc;
    risk_adjust=NoRisk(),
    quantity=ActualPrediction(),
    tag="norisk-actual"
)
norisk_estimated = auc -> Config(
    auc;
    risk_adjust=NoRisk(),
    quantity=ModelPrediction(),
    tag="norisk-estimated"
)

# No risk (close enough)
sorta_norisk_actual = auc -> Config(
    auc;
    risk_adjust=RiskScaling(0.001),
    quantity=ActualPrediction(),
    tag="norisk-0.001-actual"
)
sorta_norisk_estimated = auc -> Config(
    auc;
    risk_adjust=RiskScaling(0.001),
    quantity=ModelPrediction(),
    tag="norisk-0.001-estimated"
)

# Lump sum
lump_actual = auc -> Config(
    auc;
    quantity=ActualPrediction(),
    auctype=Lump(),
    tag="lump-actual"
)
lump_estimated = auc -> Config(
    auc;
    quantity=ModelPrediction(),
    auctype=Lump(),
    tag="lump-estimated"
)

# Lump sum (calculated using Mixed(1))
lump_actual_bkp = auc -> Config(
    auc;
    quantity=ActualPrediction(),
    auctype=Mixed(1),
    tag="lump-bkp-actual"
)
lump_estimated_bkp = auc -> Config(
    auc;
    quantity=ModelPrediction(),
    auctype=Mixed(1),
    tag="lump-bkp-estimated"
)

# Minimum bid
minbid_actual = auc -> Config(
    auc;
    quantity=ActualPrediction(),
    bid_limit=BidLimit(0.25),
    tag="minbid-actual-0.25"
)
minbid_estimated = auc -> Config(
    auc;
    quantity=ModelPrediction(),
    bid_limit=BidLimit(0.25),
    tag="minbid-estimated-0.25"
)

# Lump sum, 2:1 renegotiation
two_one_actual = auc -> Config(
    auc;
    quantity=ActualPrediction(),
    auctype=Mixed(2 / 3),
    tag="mixed-2:1-actual"
)
two_one_estimated = auc -> Config(
    auc;
    quantity=ModelPrediction(),
    auctype=Mixed(2 / 3),
    tag="mixed-2:1-estimated"
)

# Lump sum, 50/50 renegotiation
fifty_fifty_actual = auc -> Config(
    auc;
    quantity=ActualPrediction(),
    auctype=Mixed(1 / 2),
    tag="mixed-fifty-fifty-actual"
)
fifty_fifty_estimated = auc -> Config(
    auc;
    quantity=ModelPrediction(),
    auctype=Mixed(1 / 2),
    tag="mixed-fifty-fifty-estimated"
)

# Lump sum, one_two renegotiation
one_two_actual = auc -> Config(
    auc;
    quantity=ActualPrediction(),
    auctype=Mixed(1 / 3),
    tag="mixed-1:2-actual"
)
one_two_estimated = auc -> Config(
    auc;
    quantity=ModelPrediction(),
    auctype=Mixed(1 / 3),
    tag="mixed-1:2-estimated"
)

# Mixed lump
mixedlump_actual = auc -> Config(
    auc;
    quantity=ActualPrediction(),
    auctype=Mixed(0),
    tag="mixed-lump-actual"
)
mixedlump_estimated = auc -> Config(
    auc;
    quantity=ModelPrediction(),
    auctype=Mixed(0),
    tag="mixed-lump-estimated"
)

# Mixed lump (sorta)
mixedlump_sorta_actual = auc -> Config(
    auc;
    quantity=ActualPrediction(),
    auctype=Mixed(0.001),
    tag="mixed-0.001-actual"
)
mixedlump_sorta_estimated = auc -> Config(
    auc;
    quantity=ModelPrediction(),
    auctype=Mixed(0.001),
    tag="mixed-0.001-estimated"
)


# No risk, 10% EWO
nr_ewo_10pct_actual = auc -> Config(
    auc;
    quantity=ActualPrediction(),
    ewo=ExtraWorkOrder(0.10),
    risk_adjust=NoRisk(),
    tag="norisk-10pct-ewo-actual"
)
nr_ewo_10pct_estimated = auc -> Config(
    auc;
    quantity=ModelPrediction(),
    ewo=ExtraWorkOrder(0.10),
    risk_adjust=NoRisk(),
    tag="norisk-10pct-ewo-estimated"
)

# Lump, 10% EWO
lump_ewo_10pct_actual = auc -> Config(
    auc;
    auctype=Lump(),
    quantity=ActualPrediction(),
    ewo=ExtraWorkOrder(0.10),
    tag="lump-10pct-ewo-actual"
)
lump_ewo_10pct_estimated = auc -> Config(
    auc;
    auctype=Lump(),
    quantity=ModelPrediction(),
    ewo=ExtraWorkOrder(0.10),
    tag="lump-10pct-ewo-estimated"
)

# No risk, 50% EWO
nr_ewo_50pct_actual = auc -> Config(
    auc;
    quantity=ActualPrediction(),
    risk_adjust=NoRisk(),
    ewo=ExtraWorkOrder(0.50),
    tag="norisk-50pct-ewo-actual"
)
nr_ewo_50pct_estimated = auc -> Config(
    auc;
    quantity=ModelPrediction(),
    risk_adjust=NoRisk(),
    ewo=ExtraWorkOrder(0.50),
    tag="norisk-50pct-ewo-estimated"
)

# Lump, 50% EWO
lump_ewo_50pct_actual = auc -> Config(
    auc;
    auctype=Lump(),
    quantity=ActualPrediction(),
    ewo=ExtraWorkOrder(0.50),
    tag="lump-50pct-ewo-actual"
)
lump_ewo_50pct_estimated = auc -> Config(
    auc;
    auctype=Lump(),
    quantity=ModelPrediction(),
    ewo=ExtraWorkOrder(0.50),
    tag="lump-50pct-ewo-estimated"
)

# No risk, moral hazard capped at 20%
nr_mh_actual = auc -> Config(
    auc;
    risk_adjust=NoRisk(),
    quantity=ActualPrediction(),
    hazard=MoralHazard(0.2, auc.T),
    tag="norisk-mh-actual"
)
nr_mh_estimated = auc -> Config(
    auc;
    risk_adjust=NoRisk(),
    quantity=ModelPrediction(),
    hazard=MoralHazard(0.2, auc.T),
    tag="norisk-mh-estimated"
)

# Baseline moral hazard
mh_actual = auc -> Config(
    auc;
    quantity=ActualPrediction(),
    hazard=MoralHazard(0.2, auc.T),
    tag="mh-actual"
)
mh_estimated = auc -> Config(
    auc;
    quantity=ModelPrediction(),
    hazard=MoralHazard(0.2, auc.T),
    tag="mh-estimated"
)

# Confirming to lump 

# # Lump sum, moral hazard capped at 20%
# lump_mh_actual = auc -> Config(
#     auc;
#     auctype = Lump(),
#     risk_adjust = NoRisk(),
#     quantity = ActualPrediction(),
#     hazard = MoralHazard(0.2, auc.T),
#     tag="lump-mh-actual"
# )
# lump_mh_estimated = auc -> Config(
#     auc; 
#     auctype = Lump(),
#     risk_adjust = NoRisk(),
#     quantity = ModelPrediction(),
#     hazard = MoralHazard(0.2, auc.T),
#     tag="lump-mh-estimated"
# )

# Mixed/Lump correctness
lump_tst = auc -> Config(
    auc;
    quantity=ModelPrediction(),
    auctype=Lump(),
    tag="lump-tst"
)
mixed1_tst = auc -> Config(
    auc;
    quantity=ModelPrediction(),
    auctype=Mixed(1.0),
    tag="mixed-tst-1",
    solver=NumericSolver()
)
mixed0_tst = auc -> Config(
    auc;
    quantity=ModelPrediction(),
    auctype=Mixed(0.0),
    tag="mixed-tst-0",
    solver=NumericSolver()
)


# Collect functions
config_fns = [
    # Baseline configurations
    baseline,
    baseline_actual,
    norisk_actual,
    norisk_estimated,
    sorta_norisk_actual,
    sorta_norisk_estimated,
    baseline_actual_bkp,
    baseline_estimated_bkp,

    # Counterfactuals
    minbid_actual,
    minbid_estimated,
    two_one_actual,
    two_one_estimated,
    fifty_fifty_actual,
    fifty_fifty_estimated,
    one_two_actual,
    one_two_estimated,

    # Lump sum
    lump_actual,
    lump_estimated,
    # mixedlump_actual,
    # mixedlump_estimated,
    mixedlump_sorta_actual,
    mixedlump_sorta_estimated,

    # lump_ewo_10pct_actual,
    # lump_ewo_10pct_estimated,
    # lump_ewo_50pct_actual,
    # lump_ewo_50pct_estimated,
]
