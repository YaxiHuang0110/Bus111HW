#Q1
is_pythagorean = function(a,b,c){
  if(a*a + b*b == c*c) {
    print('TRUE')
  }
  else {
    print('FALSE')
  }
}


#Q2
#Create a function is_prime to find prime numbers between 100 and 1000
is_prime = function(n){
  x = seq(2,n)
  prime_n = c()
  for(i in seq(2,n)){
    if(any(x==i)){
      prime_n = c(prime_n,i)
      x =c(x[(x%%i)!=0],i)
    }
  }
  return(prime_n[(prime_n>=100)])
}
is_prime(1000)
rev(is_prime(1000))

#bonus point
#The computer will print out 'prime' if the number is a prime number or 
#'not prime' if it is not.
is_prime = function(x){
  
  if(sum(x/1:x==x%/%1:x)==2)
    print("prime")
  else
    print("Not prime")
  
}


#Q3
#a)
#Create two matrices first and do the multiplication
x = matrix(c(20,30,30,30,20,30,50,60,32), 
           nrow = 3, 
           ncol = 3)
colnames(x) = c('wine','vodka','lemon juice')
rownames(x) = c('A','B','C')
price = matrix(c(5,45,10),
               nrow = 3,
               ncol = 1)
colnames(price) = c('Price')
rownames(price) = c('Wine','Vodka','Lemon Juice')
type_A = x[1, ]%*%price
type_B = x[2, ]%*%price
type_C = x[3, ]%*%price
#b)
blends = 10*type_A + 4*type_B + 5*type_C









