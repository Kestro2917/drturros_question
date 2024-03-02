# Function to compare two strings in lexicographic order
compare_strings <- function(str1, str2) {
  # Define the custom ordering of letters
  custom_order <- c("h", "w", "n", "x", "o", "z", "e", "b", "c", "y", "v", "k", "j",
                    "g", "s", "a", "i", "t", "r", "u", "m", "p", "d", "f", "q", "l")
  
  # Convert strings to character vectors
  str1_chars <- unlist(strsplit(str1, ""))
  str2_chars <- unlist(strsplit(str2, ""))
  
  # Find the minimum length of the two strings
  min_length <- min(length(str1_chars), length(str2_chars))
  
  # Compare each character of the strings based on custom ordering
  for (i in 1:min_length) {
    if (which(custom_order == str1_chars[i]) < which(custom_order == str2_chars[i])) {
      return(-1)  # str1 comes before str2
    } else if (which(custom_order == str1_chars[i]) > which(custom_order == str2_chars[i])) {
      return(1)   # str2 comes before str1
    }
  }
  
  # If both strings are equal up to the minimum length, the shorter string comes first
  if (length(str1_chars) < length(str2_chars)) {
    return(-1)
  } else if (length(str1_chars) > length(str2_chars)) {
    return(1)
  } else {
    return(0)   # Strings are equal
  }
}


# Function to perform Bubble Sort
bubble_sort <- function(strings) {
  n <- length(strings)
  
  # Iterate through each string in the list
  for (i in 1:(n - 1)) {
    # Iterate through each adjacent pair of strings
    for (j in 1:(n - i)) {
      # Compare the current string with the next one
      if (compare_strings(strings[j], strings[j + 1]) > 0) {
        # Swap the strings if they are not in lexicographic order
        temp <- strings[j]
        strings[j] <- strings[j + 1]
        strings[j + 1] <- temp
      }
    }
  }
  
  # Return the sorted list of strings
  return(strings)
}



# Test the bubble_sort function with an example list of strings
strings <- c("oabc", "nxyz", "wa", "zh", "wno", "qwerty", "vjkl", "smurf", "djang", "lisp")
sorted_strings <- bubble_sort(strings)
print("The unsorted list:")
print(strings)

print("The sorted list:")
print(sorted_strings)


