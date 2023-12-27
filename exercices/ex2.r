# Function to check if a number is prime
isprime <- function ( x ) {
  if(x==1) return(FALSE)
  if(x==2) return(TRUE)
  M<-max(c(floor(sqrt(x)),2))
  for(k in 2:M){
    if(x%%k ==  0) return(FALSE)
  }
  return(TRUE)
}
# Function to generate all prime numbers not exceeding a given value n
allprimes <- function ( n ) {
  primes <- c ()
  for ( i in 2: n ) {
    if ( isprime ( i ) ) {
      primes <- c ( primes , i )
    }
  }
  return ( primes )
}