###
#   SEPM.SIM:
#   Script to generate internal data (sysdata.rda) for documentation purposes
###

# # DISTS object for MBB SIM --------------------------------------------------------------------
# DISTS <- list(
#   "nu40_rho75_blocks1" = SEPM.SIM:::construct_mbeta(nu=40, mu1=0.75, delta=0.05, rho=0.75, blocks=1),
#   "nu20_rho75_blocks1" = SEPM.SIM:::construct_mbeta(nu=20, mu1=0.75, delta=0.05, rho=0.75, blocks=1),
#   "nu40_rho50_blocks1" = SEPM.SIM:::construct_mbeta(nu=40, mu1=0.75, delta=0.05, rho=0.50, blocks=1),
#   "nu20_rho50_blocks1" = SEPM.SIM:::construct_mbeta(nu=20, mu1=0.75, delta=0.05, rho=0.50, blocks=1),
#   "nu40_rho75_blocks2" = SEPM.SIM:::construct_mbeta(nu=40, mu1=0.75, delta=0.05, rho=0.75, blocks=2),
#   "nu20_rho75_blocks2" = SEPM.SIM:::construct_mbeta(nu=20, mu1=0.75, delta=0.05, rho=0.75, blocks=2),
#   "nu40_rho50_blocks2" = SEPM.SIM:::construct_mbeta(nu=40, mu1=0.75, delta=0.05, rho=0.50, blocks=2),
#   "nu20_rho50_blocks2" = SEPM.SIM:::construct_mbeta(nu=20, mu1=0.75, delta=0.05, rho=0.50, blocks=2),
#   "nu40_rho75_blocks3" = SEPM.SIM:::construct_mbeta(nu=40, mu1=0.75, delta=0.05, rho=0.75, blocks=3),
#   "nu20_rho75_blocks3" = SEPM.SIM:::construct_mbeta(nu=20, mu1=0.75, delta=0.05, rho=0.75, blocks=3),
#   "nu40_rho50_blocks3" = SEPM.SIM:::construct_mbeta(nu=40, mu1=0.75, delta=0.05, rho=0.50, blocks=3),
#   "nu20_rho50_blocks3" = SEPM.SIM:::construct_mbeta(nu=20, mu1=0.75, delta=0.05, rho=0.50, blocks=3),
#   "vague_blocks1" = SEPM.SIM:::construct_mbeta(nu=2, mu1=0.5, delta=0.0, rho=0.0, blocks=1),
#   "vague_blocks2" = SEPM.SIM:::construct_mbeta(nu=2, mu1=0.5, delta=0.0, rho=0.0, blocks=2),
#   "vague_blocks3" = SEPM.SIM:::construct_mbeta(nu=2, mu1=0.5, delta=0.0, rho=0.0, blocks=3),
#   "liberal_blocks1" = SEPM.SIM:::construct_mbeta(nu=4, mu1=0.75, delta=0.0, rho=0.5, blocks=1),
#   "liberal_blocks2" = SEPM.SIM:::construct_mbeta(nu=4, mu1=0.75, delta=0.0, rho=0.5, blocks=2),
#   "liberal_blocks3" = SEPM.SIM:::construct_mbeta(nu=4, mu1=0.75, delta=0.0, rho=0.5, blocks=3)
# )
#
# # SCENARIOS object for MLE SIM ----------------------------------------------------------------
# SCENARIOS <- list()
# SCENARIOS[["EOMPM_A"]] <-
#   list(score = SEPM.SIM:::score_linearsparse, pars = list(P=50, Prel=5, rho=0, red=0, m=0, s=1, mu=4))
# SCENARIOS[["EOMPM_B"]] <-
#   list(score = SEPM.SIM:::score_lineardense, pars = list(P=50, Prel=5, rho=0, red=0, m=0, s=1, mu=6))
#
# SCENARIOS[["EOMPM_A2"]] <-
#   list(score = SEPM.SIM:::score_linearsparse, pars = list(P=50, Prel=5, rho=0, red=0, m=0, s=1, mu=2))
# SCENARIOS[["EOMPM_B2"]] <-
#   list(score = SEPM.SIM:::score_lineardense, pars = list(P=50, Prel=5, rho=0, red=0, m=0, s=1, mu=3))
#
# SCENARIOS[["MLE_SIM_F1_prev30"]] <-
#   list(score = SEPM.SIM:::score_friedman1, pars = list(P=50, Prel=5, rho=0, red=0, m=2.5, s=-0.5))
# SCENARIOS[["MLE_SIM_F1_prev15"]] <-
#   list(score = SEPM.SIM:::score_friedman1, pars = list(P=50, Prel=5, rho=0, red=0, m=-2.5, s=-0.55))
#
# SCENARIOS[["MLE_SIM_F3_prev30"]] <-
#   list(score = SEPM.SIM:::score_friedman3, pars = list(P=50, Prel=4, rho=0, red=0, m=-15, s=-12))
# SCENARIOS[["MLE_SIM_F3_prev15"]] <-
#   list(score = SEPM.SIM:::score_friedman3, pars = list(P=50, Prel=4, rho=0, red=0, m=-50.75, s=-34))
#
# SCENARIOS[["MLE_SIM_F13_prev30"]] <-
#   list(score = SEPM.SIM:::score_friedman13, pars = list(P=50, Prel=9, rho=0, red=0, m=2.5, s=-0.5, w1=1, w3=-1))
# SCENARIOS[["MLE_SIM_F13_prev15"]] <-
#   list(score = SEPM.SIM:::score_friedman13, pars = list(P=50, Prel=9, rho=0, red=0, m=-2.5, s=-1, w1=0.5, w3=-0.75))
#
# # MLE simulation instance example -------------------------------------------------------------
# example_mle <- load_instance(1)
# # alternative, if instance not yet generated (takes longer):
# # example_mle <- sample_mle()
#
# # Use data in package -------------------------------------------------------------------------
# usethis::use_data(DISTS, SCENARIOS, example_mle, internal=TRUE, overwrite=TRUE)
