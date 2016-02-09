#Aulas do Coursera no R

#Convertion into classes
x<-0:6
class(x)
as.numeric(x)
as.logical(x)
as.character(x) 

#Builds a matrix
m<- matrix(nrow=2, ncol= 3)
m
dim(m) #says the number of columns and rows
m<- matrix(1:6, nrow=2, ncol=3)
m

m<-1:10
m
dim(m)<-c(2,5)
m

x<-1:3
y<-10:12
cbind(x,y) 
rbind(x,y)

#Makes a list
x<- list(1, "a", TRUE, 1+4i)
x

#Creating factors
x<-factor(c("yes", "yes", "no", "yes", "no"))
x
table(x)
unclass(x)

#Test objects if they are NA
is.na() #NA is defined by missing values
isnan() #nan is defined as an undefined mathematical operation

x<- c(1, 2, NA, 10, 3)
is.na(x) 
is.nan(x)

#Creating data frame

x<-data.frame(foo=1:4,bar=c(T,T,F,F))
x
nrow(x)
ncol(x)

x<-1:3
names(x)
names(x)<-c("foo", "bar", "norf")
x
names(x)
#NAming lists
x<-list(a=1, b=2, c=3)
x

#Naming matrixes
m<-matrix(1:4,nrow=2, ncol=2)
dimnames(m)<-list(c("a", "b"), c("c", "d"))
m

### Writing the first functions
add2<- function(x,y){
x+y
}
add2(3,5)

above10<-function(x) {
 use<-X>10
 x[use]
}

above <- function(x,n){  #tells which values are inside the representative equation
 use<-x>n
 x[use]
}
x<-1:20
above(x,12)

comunmean<-function(y) {
 nc <- ncol(y) # calculates the numer of columns
 means<-numeric(nc)
 for(i in 1:nc) {
 means [i]<-mean(y[,i], na.rm=removeNA)
}
means
}
	
#Coding standards
### 1- Always text the code as a txt file and save it
### 2- Indent your code => space to the right
### 3- Limit the width of your code (80 columns?)
### 4- At least 4 spaces of indenting (8 spaces is ideal)
### 5 - Limit the length of individual functions so it can be grouped into small pieces (it`s more easy to find mistakes)


##Scoping
lm<-function (x) {x*x}
lm
function(x) {x*x}

## Dates and times
x<-as.Date("1970-01-01")
x

## Looping on the Command Line
-> lapply: loop over a list and evaluate a function on each element
-> sapply: same as lapply but try to simplify the result
-> apply: apply a function over the marguns of an array
-> tapply: apply a function over subsets of a vector
-> mapply: multivariate version of lapply

An auxiliary function split is also useful, particularly in conjunction with lapply.

## Debugging Tools in R
- > traceback: try to identify where the error occur;
- > debug: flags a function for "debug! mode which allows you to step through execution of a function one line at a time
- > browser: suspends the execution of a function wherever it is called and putz the function in debug mode
- > trace: allows you to insert debugging code into a function a specific places
- > recover: allows you to modify the error behavior so that you can browse the function call stack
 
# str function
-> a dagnostic function and an alternative to `summary`

#Generating random numbers from a linear model