# Define founder genotypes for scenario 1
scenario1_founder_genotypes <- list(
  "1" = c("a", "a"),
  "2" = c("A", "A"),
  "3" = c("A", "A"),
  "10" = c("A", "a")
)

# Define relationships
relationships <- list(
  "5" = c("1","3"),
  "4" = c("2", "3"),
  "7" = c("3", "4"),
  "6" = c("4","5"),
  "8" = c("7","6"),
  "9" = c("7","6"),
  "11" = c("9","10")
)

# Function to calculate probability of genotype for individual based on parents' genotypes
calculate_genotype_prob <- function(parent1_genotype, parent2_genotype, target_genotype) {
  # Check if target genotype matches either parent's genotype (homozygous case)
  if (all(target_genotype == parent1_genotype) | all(target_genotype == parent2_genotype)) {
    return(1/2)
  } else {
    # Check if target genotype requires one allele from each parent (heterozygous case)
    if (target_genotype[1] == parent1_genotype[1] & target_genotype[2] == parent2_genotype[2] |
        target_genotype[1] == parent2_genotype[1] & target_genotype[2] == parent1_genotype[2]) {
      return(1/4)
    } else {
      return(0)
    }
  }
}

# Calculate probabilities for individual 8 (parents: 7 and 6)
parent1_genotype <- scenario1_founder_genotypes[[relationships[["8"]][[1]]]]
parent2_genotype <- scenario1_founder_genotypes[[relationships[["8"]][[2]]]]

prob_AA_8 <- calculate_genotype_prob(parent1_genotype, parent2_genotype, c("A", "A"))
prob_Aa_8 <- calculate_genotype_prob(parent1_genotype, parent2_genotype, c("A", "a"))
prob_aa_8 <- calculate_genotype_prob(parent1_genotype, parent2_genotype, c("a", "a"))

# Print the probabilities
cat("Probability of AA genotype for individual 8:", prob_AA_8, "\n")
cat("Probability of Aa genotype for individual 8:", prob_Aa_8, "\n")
cat("Probability of aa genotype for individual 8:", prob_aa_8, "\n")








