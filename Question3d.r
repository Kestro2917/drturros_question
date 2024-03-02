# Function to simulate genotypes for a single scenario
simulate_genotypes <- function(founder_genotypes, relationships) {
  genotypes <- list()
  
  # Function to simulate genotype based on parents' genotypes
  simulate_child_genotype <- function(parent1, parent2) {
    # Randomly select one allele from each parent
    allele1 <- sample(c(parent1[1], parent1[2]), 1)
    allele2 <- sample(c(parent2[1], parent2[2]), 1)
    return(c(allele1, allele2))
  }
  
  # Simulate genotypes for founders
  for (founder_id in names(founder_genotypes)) {
    genotypes[[founder_id]] <- founder_genotypes[[founder_id]]
  }
  
  # Simulate genotypes for non-founder individuals
  for (child_id in names(relationships)) {
    parent1_id <- relationships[[child_id]][1]
    parent2_id <- relationships[[child_id]][2]
    parent1_genotype <- genotypes[[parent1_id]]
    parent2_genotype <- genotypes[[parent2_id]]
    genotypes[[child_id]] <- simulate_child_genotype(parent1_genotype, parent2_genotype)
  }
  
  return(genotypes)
}

# Function to simulate genotypes for 10,000 iterations under a given scenario
simulate_scenario <- function(founder_genotypes, relationships, iterations) {
  simulated_genotypes <- replicate(iterations, simulate_genotypes(founder_genotypes, relationships), simplify = FALSE)
  return(simulated_genotypes)
}

# Function to count the frequency of each genotype for a given individual in simulated genotypes
count_genotype_frequency <- function(simulated_genotypes, individual_id) {
  genotype_counts <- table(sapply(simulated_genotypes, `[[`, individual_id))
  return(genotype_counts)
}

# Function to compute the stochastic estimate of the probability that genotypes of two individuals are identical
compute_identity_probability <- function(simulated_genotypes, ind1_id, ind2_id) {
  identical_count <- sum(sapply(simulated_genotypes, function(gen) all(gen[[ind1_id]] == gen[[ind2_id]])))
  return(identical_count / length(simulated_genotypes))
}

# Define founder genotypes for each scenario
scenario1_founder_genotypes <- list(
  "1" = c("a", "a"),
  "2" = c("A", "A"),
  "3" = c("A", "A"),
  "10" = c("A", "a")
)

scenario2_founder_genotypes <- list(
  "1" = c("A", "a"),
  "2" = c("A", "a"),
  "3" = c("A", "a"),
  "10" = c("A", "a")
)

scenario3_founder_genotypes <- list(
  "1" = c("A", "A"),
  "2" = c("A", "a"),
  "3" = c("A", "A"),
  "10" = c("A", "A")
)

# Define relationships for each scenario
relationships <- list(
  "5" = c("1","3"), 
  "4" = c("2", "3"),
  "7" = c("3", "4"),
  "6" = c("4","5"),
  "8" = c("7","6"),
  "9" = c("7","6"),
  "11" = c("9","10")
)

# Simulate genotypes for each scenario 10,000 times
iterations <- 10000
scenario1_simulated_genotypes <- simulate_scenario(scenario1_founder_genotypes, relationships, iterations)
scenario2_simulated_genotypes <- simulate_scenario(scenario2_founder_genotypes, relationships, iterations)
scenario3_simulated_genotypes <- simulate_scenario(scenario3_founder_genotypes, relationships, iterations)

# Compute probability of identity for individuals 8 and 11 under each scenario
prob_identity_scenario1 <- compute_identity_probability(scenario1_simulated_genotypes, "8", "11")
prob_identity_scenario2 <- compute_identity_probability(scenario2_simulated_genotypes, "8", "11")
prob_identity_scenario3 <- compute_identity_probability(scenario3_simulated_genotypes, "8", "11")

# Print the probabilities
cat("Probability of identical genotypes for individuals 8 and 11 under each scenario:\n")
cat("Scenario 1:", prob_identity_scenario1, "\n")
cat("Scenario 2:", prob_identity_scenario2, "\n")
cat("Scenario 3:", prob_identity_scenario3, "\n")






