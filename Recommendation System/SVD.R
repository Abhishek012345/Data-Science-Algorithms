#A=U??VT
#Where the columns of matrices U and V are orthonormal (orthogonal unit vectors) and ?? is a diagonal matrix. The columns of U and V are the eigenvectors of AAT and ATA, respectively. The entries in the diagonal matrix ?? are the singular values r, which are the square roots of the non-zero eigenvalues of AAT and ATA.
A = as.matrix(data.frame(c(4,7,-1,8), c(-5,-2,4,2), c(-1,3,-3,6)))# create a matrix
A
A.svd <- svd(A)# single value decomposition
A.svd
ATA <- t(A) %*% A #(ATA)
ATA
ATA.e <- eigen(ATA)
v.mat <- ATA.e$vectors #The V component of the singular value decomposition is then found by calculating the eigenvectors of the resultant ATA matrix.
v.mat
v.mat[,1:2] <- v.mat[,1:2] * -1 #V  matrix is the same as the output of the svd() but with some sign changes. These sign changes can happen, as mentioned earlier, as the eigenvector scaled by ???1 is still the same eigenvector, just scaled
v.mat
AAT <- A %*% t(A) #The same routine is done for the ATA matrix.
AAT
AAT.e <- eigen(AAT)#The eigenvectors are again found for the computed AAT matrix.
u.mat <- AAT.e$vectors
u.mat #There are four eigenvectors in the resulting matrix; however, we are only interested in the non-zero eigenvalues and their respective eigenvectors. Therefore, we can remove the last eigenvector from the matrix which gives us the U matrix
u.mat <- u.mat[,1:3]
r <- sqrt(ATA.e$values)#the singular values r are the square roots of the non-zero eigenvalues of the AAT and ATA matrices
r <- r * diag(length(r))[,1:3]
r
svd.matrix <- u.mat %*% r %*% t(v.mat)
svd.matrix
A == round(svd.matrix, 0)

