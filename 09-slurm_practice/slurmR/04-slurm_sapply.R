# Model parameters
nsims  <- 1e3
n      <- 1e4
# ncores <- 4L
njobs  <- 4L

# Function to simulate pi
simpi <- function(i, n.) {
  
  p <- matrix(runif(n.*2, -1, 1), ncol = 2)
  mean(sqrt(rowSums(p^2)) <= 1) * 4
  
}

# Setting up slurmR
library(slurmR) # This also loads the parallel package

# Approximation
ans <- Slurm_sapply(
  1:nsims, simpi,
  n.       = n,
  njobs    = njobs,
  plan     = "collect",
  tmp_path = "/scratc/general/nfs1/u1317537", # This is where all temp files will be exported
  sbatch_opt = list(account = "notchpeak-shared-short",
		    partition = "notchpeak-shared-short")
  )

message("Pi: ", mean(ans))

saveRDS(ans, "04-slurm_sapply.rds")

