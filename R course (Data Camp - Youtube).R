#Course - Data camp (Youtube)
#19/10/2018

###R is called the language of statistical computing and data science

#Creates a list of all the vairbales that I have created so far
ls()

##R basic data types
#Reveals which type of variable it is
class()

#To transform a number into an integer, we can add an L at the end of the number
2L
class(2L)
class(2)

#Checks the type of variable outputted
is.numeric()
is.integer()

###Other atomic bytes: double, complex, and raw.

#Coercing variables
as.numeric(TRUE)
as.numeric(FALSE)
as.character(4)
as.numeric("4.5")
as.integer("4.5")
as.numeric("hello")

##Vectors are a sequence of data elements from the same basic type
c("hearts", "diamonds", "diamonds", "spades")
drawn_suits<- c("hearts", "diamonds", "diamonds", "spades")
is.vector(drawn_suits)

#Combines two vectors to name one of them
remain<-c(11, 12, 13, 14)
suits<-c("spades", "diamonds", "hearts", "flowers")
names(remain)<-suits
remain
class(remain)
remain<-c(spades=11, diamonds=12, hearts=13, flowers=14)
remain<-c("spades"=11, "diamonds"=12, "hearts"=13, "flowers"=14)

length(remain)


##Vectors can only have elements from the same type, which differentiates them from another type of 
##atomic vectors, the lists, which can hold multiple types of elements.
##Thus, one vector cannot contain elements that are numeric and logical, for example.
##If you try to build a vector with multiple types of elements, R does coercion and converts the 
##elements into just one single type.
drawn_ranks<-c(7, 2, "A", "B")
drawn_ranks
class(drawn_ranks)

#Vector Calculus
earnings<-c(50, 2, 100)
expenses<-c(34, 4, 102)
show<-c(2, 2, 2)
earnings*3
##All calculations are made element wise in the object
show*earnings #in vector calculations, the calculations are also done element wise
earnings*c(1, 2, 3)
sum(earnings)
earnings>expenses

##Subsetting elements from a vector
remain<-c(spades=11, diamonds=12, hearts=13, flowers=14)
remain[1] #the result is a vector too
remain["spades"]
remain_black<-remain[c(1,4)]
remain_black
remain_black<-remain[c("spades", "diamonds")]

#Subsets all elements of a vector apart from some
remain[-1] #apart from the first one
remain[-c(1, 2)] #apart from the first and second
remain[-"spades"] #this does not work with names
remain[c(FALSE, TRUE, FALSE, TRUE)]
remain[c(TRUE, FALSE)] # in this case, R does something called recycling. In this case, it repeats the expression until it reaches the same length of the vector remain. This is equivalent to this line of code remain[c(TRUE, FALSE, TRUE, FALSE)]

###Matrices ##are two dimensional data objects
#Matrices can also only contain one atomic data type. It is a two dimensional version of vector
matrix(1:6, nrow=2)
matrix(1:6, nrow=2, ncol=2)
matrix(1:3, nrow=3, ncol=3)
matrix(1:4, nrow=3, ncol=3) #warning message generated because the range of values is not multiple of the number of spaces in the matrix

#cbind and rbind => another way to put the elements in a matrix together
cbind(1:3, 1:3)
rbind(1:3, 1:3)

#Adds another row with specific elements to an already created matrix 
m<-matrix(1:6, byrow=TRUE, nrow=2)
m
n<-rbind(m, 7:9)
n

#Adds another column with specific elements to an already created matrix 
m<-matrix(1:6, byrow=TRUE, nrow=2)
m
n<-cbind(m, 9:10)
n
n<-cbind(m, c(9,10))
n


#Adds another columns with specific elements (filling by column the matrix) to an already created matrix
matrix(1:6, byrow=TRUE, nrow=2)
cbind(m, 9,10, 11)


#Naming a matrix
rownames(m)<-c("Spades", "Hearts")
m
colnames(m)<-c("Bat 1", "Bat 2", "Bat 3")
m

#Naming a mrtix
m<-matrix(1:6, byrow=TRUE, nrow=2, dimnames=list(c("Spades", "Hearts"), c("Bat 1", "Bat 2", "Bat 3" )))
m

##Coercion in matrices
char<-matrix(LETTERS[1:6], nrow=4, ncol=3)
char
num<-matrix(1:8, ncol=2)
num
cbind(char, num) #numbers were converted to characters
w<-cbind(char, num)
str(w)

##Subsetting elements to create a matrix
m<-matrix(sample(1:15, 12), nrow=3)
m
m[1,3] #first element refers to the row and second to the column
m[,3] #selects elements from the third column
m[3,]#selects elements from the third row
m[4]#gets the fourth element of the matrix going column by column
m[3,c(3,2)]#selects elements of the third row and second and third column of the object m
##you cannot get individual values from different row or columns using this method
m[c(2,3), c(2,3)] #the results of this subset is another matrix

##Subsetting a matrix by name
m<-matrix(1:6, byrow=TRUE, nrow=2)
rownames(m)<-c("Spades", "Hearts")
colnames(m)<-c("Bat 1", "Bat 2", "Bat 3")
m
m["Spades", "Bat 1"]
m["Bat 1", "Spades"] #if the column and row names are out of order, it gives an error to the matrix
m[2, "Bat 2"] #it is also possibe to use a combination of name and position for subsetting
m[2, c("Bat 1", "Bat 2")]


##Matrix arithmetic
#any operations of matrices is done element wise


##Categorical variables: limited number of different variables and 
#Creating a factor => factors are made to store categorical vectors in R. Factors are integers.
blood<-c("A", "B", "AB", "O")
blood
blood_factor<-factor(blood) #factors are integer vectors
blood_factor
class(blood_factor)
class(blood)

blood_factor2<-factor(blood, c("O", "A", "B", "AB"))
blood_factor2
blood_factor[1]>blood_factor[2] #not possible to say that a factor is higher or lower than another factor

#Determinind factors according to an increasing order
tshirt<-c("M", "S", "L", "M", "S", "L", "M")
tshirt_factor<-factor(tshirt, ordered=TRUE, levels=c("S", "M", "L"))
tshirt_factor
tshirt_factor[1]<tshirt_factor[2]


###HOw to create and name lists in R #all types of R objects can be stored in a single list without R having to coerce them to a single type
list("Rsome times", 190, 5)
song<-list("Rsome times", 190, 5)
is.list(song)
names(song)<-c("title", "duration", "track")
song<-list(title="Rsome times", duration=190, track=5)
song
song[1]
song[[1]]

#list on a list
similar_song<-list(title="R you on time?", duration=230)
song<-list(title="Rsome times", duration=190, track=5, similar=similar_song)
song
str(song)

###How to subset & extend lists in R
song[1] #the result of this subset is another list
song[[1]] #subsets a single element of a list
song[c(1,3)]
song[[c(1,3)]] #double brackets are only used to select single elements of a list
song[[4]][[1]] #subsets the first element of the fourth element of the string song
song[[c(4,1)]]#this is another possibility for the upper formula
song[["duration"]] #subsets the list by the name
song$duration #works the same as the double brackets
friends<-c("Paul", "Ringo", "John", "Eleanor", "Rocky")
song$sent<-friends #adds the vector friends to the list song
song
song[["sent"]]<-friends #this is another possibility for the same code of above
song
song$similar$reason<-"too long"
song
str(song)

###Using a data frame in R
#a big difference between a data frame and a matrix is that a data frame can have elements of different types while in the matrix they need to be from one single type
#elements in each column should be from the same type though.
name<-c("Peter", "Adam", "Ruth", "Roger")
age<-c("34", "12", "20", "32")
child<-c("TRUE", "FALSE", "TRUE", "FALSE")
df<-data.frame(name, age, child)
df
names(df)<-c("Name", "Age", "Child")
str(df) #under the roots, a dataframe is actually a list. That is why they have similar representations
df<-data.frame(name, age, child, stringsAsFactors = FALSE) #R by deafult stores the strings as factors.
df[2,2]
df$age
df
df[3,"age"]
df[,2]
df[3,]
df[c(3,2), c("age", "child")] #dataframes can be subsetted exactly like matrices
people[2]#gives the column as a data frame. Not a vector...


#Joining information from different data frames
tom<-data.frame("Tom", 37, FALSE)
rbind(df, tom)
tom<-data.frame(name="tom", age=37, child=FALSE)
rbind(df, tom)

#Sort the data frame 
sort(df$age)
ranks<-order(df$age)
ranks
df$age
df[ranks,]
df[order(df$age, decreasing=TRUE),]

##Suggestions of packages to manipulate dataframes (dplyr) (datatable)


###Graphic plotting and visualizations with R
#plot() and hist() functions
plot(df$age) #plots of numeric variables are shown as index plots
plot(countries$area, countries$population)
plot(log(countries$area), log(countries$population))
plot(log(countries$area), log(countries$population))
plot(countries$continent, countries$religion)
plot(countries$religion, countries$continent) #the first element is the element on a x-axis and the second is the element on the y-axis

#Creates a subframe of the data containing only African countries
africa_obs<-countries$continent=="Africa"
africa<-countries[africa_obs,]
hist(africa$population)
hist(africa$population, beaks=10)

#Other important graphic functions are #pairs, #boxplot, and #barplot
mercury
plot(mercury$temperature, mercury$pressure)
plot(mercury$temperature, mercury$pressure, xlab="Temperature", ylab="Pressure", main="T vs P for Mercury"
     type="l", col="orange")
plot(mercury$temperature, mercury$pressure, color="darkgreen")
?par
par()
par(col="blue")
par()$col

plot(mercury$temperature, mercury$pressure, xlab="Temperature", ylab="Pressure", main="T vs P for Mercury"
     type="l", col="orange", cex.axis=1.5, lty=5, pch=4)

#States the parameters to be by each row
par()
par(mfrow=c(2,2))
layout(1) #sets back for one graph per plot
layout(grid)

points()
segments()
lines()
text()

ranks<-order(shop$ads)
plot(shop$ads, shop$sales, pch=16, col=2, xlab="advertisement", ylab="net sales")
abline(coef(lm_sales), lwd=2)
lines(shop$ads[ranks], shop$sales[ranks])

###If statements
if(condition){expr}

x<- -3
if(x<0){
  print("x is a negative value")
}

###Else statement
if(condition){
  expr1
} else {
  expr2
}

x<- -3
if(x<0){
  print("x is a negative value")
} else {
  print("x is either a positive number or zero")
}

#Else if statement ##once R recognizes one statement is true, it stop going through the rest of the statements
if(x<0){
  print("x is a negative number")
}else if(x==0){      ##the double equal (==) is always used for equality testing in comparison with the single one
  print("x is zero")
} else {
  print("x is a positive number")
}


#Logical operators
x<-12
x>5
x>5 & x<12     

#If at least one statement is TRUE, than the full statement is true
TRUE | TRUE
TRUE | FALSE
FALSE | TRUE
FALSE | FALSE

y<-5
y>5 | y==5
y>5 | y<5

# not operator !
!TRUE
!FALSE
!(x<5)
x>=5
is.numeric(5)
!is.numeric(5)
is.numeric("hello")
!is.numeric("hello")

###Logical Operators and Vectors
c(TRUE, TRUE, FALSE) & c(TRUE, FALSE, FALSE)
c(TRUE, TRUE, FALSE) | c(TRUE, FALSE, FALSE)
!c(TRUE, FALSE, FALSE)

### Writing functions in R
#Function to compute the absolute value #functions are regular objects. You can work with them in the same way that you work with regular objects
f<-function(x){
  if (x<0) {
    -x
  } else {
    x
  }
}

add<-function(x, y=1){
  x+y
}
add(x=3)

#Every function has three components:
#formals(add)
#body(add)
#environment(add)
my_fun<-function(arg1, arg2){
  body
}


#Data structures in R
#Atomic vectors (Six types: logical, integer, double, character, complex, and raw)
#Lists (recursive lists that contain other lists)

#Atomic vectors are always homogeneous, while lists can be heterogeneous

#Every vector has two properties (type and length)
typeof(x)
length(x)

###For loops in R
prime_list<-list(1, 2, 3, 4, 5, 6)
for (i in 1:length(prime_list)){
  print(prime_list[[i]])
}

df<-data.frame(a=rnorm(10),
               b=rnorm(10),
               c=rnorm(10),
               d=rnorm(10))

for (i in 1:ncol(df)){
  print(median(df[[i]]))
}

df

###R Data Science
#Common functions
#tolower() => makes all letters lowercase
#removePunctuation => remove all punctuations like periods and exclamation points
#removeNumbers() => remove numbers
#stripWhiteSpaces() => remove tabs and spaces and extra spaces
#removeWords() => remove specific words defined by the data scientist

stem_words <- stemDocument(c("complicatedly", "complicated", "complication"))
stem_words
stem(stem_words, c("complicate"))

