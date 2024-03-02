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



