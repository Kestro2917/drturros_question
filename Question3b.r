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
  simulated_genotypes <- list()
  for (i in 1:iterations) {
    simulated_genotypes[[i]] <- simulate_genotypes(founder_genotypes)
  }
  return(simulated_genotypes)
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

# Simulate genotypes for each scenario 10,000 times
iterations <- 10000
scenario1_simulated_genotypes <- simulate_scenario(scenario1_founder_genotypes, iterations)
scenario2_simulated_genotypes <- simulate_scenario(scenario2_founder_genotypes, iterations)
scenario3_simulated_genotypes <- simulate_scenario(scenario3_founder_genotypes, iterations)

# Print the simulated genotypes for scenario 1 (you can do the same for scenarios 2 and 3)
print(scenario1_simulated_genotypes)




