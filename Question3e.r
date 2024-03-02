# Function to simulate genotypes for a single scenario
simulate_genotypes <- function(founder_genotypes) {
  genotypes <- list()
  
  relationships <- list(
    "5" = c("1","3"), 
    "4" = c("2", "3"),
    "7" = c("3", "4"),
    "6" = c("4","5"),
    "8" = c("7","6"),
    "9" = c("7","6"),
    "11" = c("9","10")
  )
  
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
simulate_scenario <- function(founder_genotypes, iterations) {
  simulated_genotypes <- replicate(iterations, simulate_genotypes(founder_genotypes), simplify = FALSE)
  return(simulated_genotypes)
}

# Define founder genotypes for each scenario
scenario1_founder_genotypes <- list(
  "1" = c("a", "a"),
  "2" = c("A", "A"),
  "3" = c("A", "A"),
  "10" = c("A", "a")
)

# Simulate genotypes for scenario 1, 10,000 times
iterations <- 10000
scenario1_simulated_genotypes <- simulate_scenario(scenario1_founder_genotypes, iterations)

# Analyze the simulated genotypes for individual 8 under scenario 1
genotype_counts <- table(sapply(scenario1_simulated_genotypes, '[[', "8"))
total_iterations <- sum(genotype_counts)
prob_AA <- genotype_counts['AA'] / total_iterations
prob_Aa <- genotype_counts['Aa'] / total_iterations
prob_aa <- genotype_counts['aa'] / total_iterations

# Print the probabilities
cat("Probability of AA genotype for individual 8:", prob_AA, "\n")
cat("Probability of Aa genotype for individual 8:", prob_Aa, "\n")
cat("Probability of aa genotype for individual 8:", prob_aa, "\n")







