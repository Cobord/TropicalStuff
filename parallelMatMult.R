library(parallel)

parMatMult <- function(cl,A, B){
 if (ncol(A) != nrow(B)) stop("Dimension mismatch 8")
 index <- splitIndices(nrow(A), length(cl))
 Alist <- lapply(index, function(ii) A[ii,,drop=FALSE])
 ans <- clusterApply(cl, Alist, get("%*%"), B)
 do.call(rbind, ans)
}t

tropDotMin <- function(vect1,vect2){
 if (length(vect1) != length(vect2)) {
 	print(length(vect1))
	print(length(vect2))
	stop("Dimension mismatch 7")
 }
 return(min(vect1+vect2))
}

tropDotMin2<- function(vect1,mat2){
 result <- lapply(1:dim(mat2)[2],function(jj) tropDotMin(vect1,mat2[,jj]))
 return(t(result))
}

tropDotMax <- function(vect1,vect2){
 if (length(vect1) != length(vect2)) stop("Dimension mismatch 5")
 return(max(vect1+vect2))
}

tropDotMax2<- function(vect1,mat2){
 result <- lapply(1:dim(mat2)[2],function(jj) tropDotMax(vect1,mat2[,jj]))
 return(t(result))
}

serMatMultTropMin <- function(A,B){
 if (ncol(A) != nrow(B)) stop("Dimension mismatch 3")
 result <- lapply(1:nrow(A),function(ii,B) tropDotMin2(A[ii,],B),B)
 do.call(rbind, result)
}

serMatMultTropMax <- function(A,B){
 if (ncol(A) != nrow(B)) stop("Dimension mismatch 2")
 result <- lapply(1:nrow(A),function(ii,B) tropDotMax2(A[ii,],B),B)
 do.call(rbind, result)
}

parMatMultTropMin <- function(cl,A, B){
 if (ncol(A) != nrow(B)) stop("Dimension mismatch 1")
 index <- splitIndices(nrow(A), length(cl))
 Alist <- lapply(index, function(ii) A[ii,,drop=FALSE])
 ans <- clusterApply(cl, Alist, function(aa,B) tropDotMin2(aa,B), B)
 do.call(rbind, ans)
}

testerSer <- function(i,j,k,l){
 A <- matrix(round(rnorm(i*j),1),nr=i)
 print(A)
 B <- matrix(round(rnorm(k*l),1),nr=k)
 print(B)
 C <- serMatMultTropMin(A,B)
 #C <- serMatMultTropMax(A,B)
 C
}

testerPar <- function(i,j,k,l){
 cl <- makeCluster(detectCores(),outfile="logParTrop.txt")
 clusterExport(cl, list("tropDotMin2","tropDotMin"))
 #clusterExport(cl, list("tropDotMax2","tropDotMax"))
 A <- matrix(round(rnorm(i*j),1),nr=i)
 print(A)
 B <- matrix(round(rnorm(k*l),1),nr=k)
 print(B)
 C <- parMatMultTropMin(cl,A,B)
 #C <- parMatMultTropMax(cl,A,B)
 C
}

isAMetric <- function(D){
 eachij<-serMatMultTropMin(D,D)==D
 all(eachij)
}

isATreeMetric <- function(D,n){
	if (!isAMetric(D)){
		return(FALSE)
	}
 	for (i in 1:(n-3)){
		for (j in (i+1):(n-2)){
			for (k in (j+1):(n-1)){
				for (l in (k+1):(n-0)){
					max1 <- D[i,j]+D[k,l]
					max2 <- D[i,k]+D[j,l]
					max3 <- D[i,l]+D[j,k]
					maxAll <- max(max1,max2,max3)
					ijklCheck <- (((maxAll==max1) + (maxAll==max2) + (maxAll==max3))>=2)
					print(paste0("Current p_ijkl: ", ijklCheck))
					if (!ijklCheck){
						return(FALSE)
					}
				}
			}
		}
	}
	return(TRUE)
}

# Human,Mouse,Rat, Chicken example
D = matrix( 
c(0, 1.1, 1.0, 1.4, 1.1, 0, .3, 1.3, 1.0, .3, 0, 1.2, 1.4, 1.3, 1.2, 0), # the data elements 
nrow=4,              # number of rows 
ncol=4,              # number of columns 
byrow = TRUE)        # fill matrix by rows 