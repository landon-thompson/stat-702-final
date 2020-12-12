dat <- read.csv('known.csv',header =T)
names <- order(names(dat))

dat <- dat[,names]
dat.UD <- dat[,c(24,34:25)]

set.seed(702)
rand <- sample(1:dim(dat.UD)[1],dim(dat.UD)[1]*0.7, replace = FALSE )

dat.UD.train <- dat.UD[rand,]
dat.UD.val <- dat.UD[-rand,]


LDA.modeller <- function(train,test){
  library(MASS)
  lda.fit <- lda(subject~. , data = train)
  
  test.fit <- predict(lda.fit, newdata = test,type = 'response')
  error <- mean(test.fit$class != test$subject)
  
  error
}

QDA.modeller <- function(train,test){
  library(MASS)
  qda.fit <- qda(subject~., data = train)
  
  test.fit <- predict(qda.fit, newdata = test,type = 'response')
  error <- mean(test.fit$class != test$subject)
  
  error
}

model.LDA <- LDA.modeller(dat.UD, dat.UD.val)
model.QDA <- QDA.modeller(dat.UD, dat.UD.val)


