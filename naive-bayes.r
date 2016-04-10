#####
input.data.x <- iris[,-ncol(iris)]
input.data.y <- as.numeric(iris[,ncol(iris)]) 
#####

naive.bayes <- function(x,input.data.x,input.data.y) {
	classes <- sort(unique(input.data.y))

	p.x.given.y.clubbed <- matrix( 0,length(classes),1  )
		
        for (i in 1:length(classes)) {
		mult_fact <- 1
                for ( j in 1:ncol(input.data.x) ) {
			mu <- mean(input.data.x[which(input.data.y==i),j])
			sigma <- sd(input.data.x[which(input.data.y==i),j])
			mult_fact <- mult_fact*distribution(x[j],mu,sigma)
		}
		p.x.given.y.clubbed[i,1] <- mult_fact
	}
		
	p.y <- as.matrix( sort (as.vector(table(input.data.y))) )
	
	return(p.x.given.y.clubbed * p.y)
}

distribution <- function(x,mu,sigma) {
	return( (1/sqrt(2*pi)) * exp( (-1/ (2*sigma^2) ) * (x-mu)^2  )  )
}
