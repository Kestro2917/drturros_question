def factorial(n):
    if n == 0 or n == 1:
        return 1
    result = 1
    for i in range(2, n + 1):
        result *= i
    return result

def nth_term_sequence(n):
    return factorial(n)

# Main function to continuously ask for user input and compute the nth term
def main():
    while True:
        n = int(input("Enter a positive integer n (or enter 0 to exit): "))
        if n == 0:
            print("Exiting the program.")
            break
        elif n < 1:
            print("Please enter a positive integer.")
        else:
            nth_term = nth_term_sequence(n)
            print("The nth term of the sequence is:", nth_term)

if __name__ == "__main__":
    main()