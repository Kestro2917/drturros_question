# Set arbitrary founder genotypes
founder_genotypes <- list(
  "1" = c("A", "A"),
  "2" = c("A", "B"), 
  "3" = c("B", "B"),
  "10" = c("A", "B")
)

# Pedigree relationships  
pedigree <- list(
  "5" = c("1","3"), 
  "4" = c("2", "3"),
  "7" = c("3", "4"),
  "6" = c("4","5"),
  "8" = c("7","6"),
  "9" = c("7","6"),
  "11" = c("9","10")
)

# Function to simulate child genotype
simulate_child_genotype <- function(parent1, parent2) {
  allele1 <- sample(parent1, 1)
  allele2 <- sample(parent2, 1)
  return(c(allele1, allele2)) 
}

# Initialize genotype list
genotypes <- list()

# Set founder genotypes  
for(id in names(founder_genotypes)) {
  genotypes[[id]] <- founder_genotypes[[id]]
}

# Simulate child genotypes
for(child in names(pedigree)) {
  dad <- pedigree[[child]][1] 
  mom <- pedigree[[child]][2]
  dad_geno <- genotypes[[dad]]
  mom_geno <- genotypes[[mom]]
  genotypes[[child]] <- simulate_child_genotype(dad_geno, mom_geno)
}

# Print simulated genotypes
print(genotypes)
print("Question 1 Ends Here")

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


# ... (previous code)

# Function to count genotype frequencies for a given individual
count_genotypes <- function(simulated_genotypes, individual_id) {
  genotype_counts <- list()
  for (iteration in 1:length(simulated_genotypes)) {
    genotype <- paste(simulated_genotypes[[iteration]][[individual_id]], collapse = "")
    if (is.null(genotype_counts[[genotype]])) {
      genotype_counts[[genotype]] <- 1
    } else {
      genotype_counts[[genotype]] <- genotype_counts[[genotype]] + 1
    }
  }
  return(genotype_counts)
}

# Function to generate a bar plot of genotype probabilities (corrected)
plot_genotype_probabilities <- function(genotype_counts, title) {
  genotype_probabilities <- sapply(genotype_counts, function(x) x / sum(unlist(genotype_counts)))  # Use unlist to sum genotype counts
  barplot(genotype_probabilities, main = title, xlab = "Genotype", ylab = "Probability")
}

# Generate bar plots for individual 8 and individual 11 under each scenario
individual_ids <- c("8", "11")
scenarios <- list(
  "Scenario 1" = scenario1_simulated_genotypes,
  "Scenario 2" = scenario2_simulated_genotypes,
  "Scenario 3" = scenario3_simulated_genotypes
)

for (individual_id in individual_ids) {
  for (scenario_name in names(scenarios)) {
    simulated_genotypes <- scenarios[[scenario_name]]
    genotype_counts <- count_genotypes(simulated_genotypes, individual_id)
    plot_title <- paste("Genotype Probability Distribution for Individual", individual_id, "under", scenario_name)
    plot_genotype_probabilities(genotype_counts, plot_title)
  }
}


