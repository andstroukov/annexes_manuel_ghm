help(apply)
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
x
dimnames(x)[[1]] <- letters[1:8]
str(x)
is.tibble(x)
is_matrix(x)
apply(x, 2, mean, trim = .2)
col.sums <- apply(x, 2, sum)
col.sums
row.sums <- apply(x, 1, sum)
row.sums
rbind(cbind(x, Rtot = row.sums), Ctot = c(col.sums, sum(col.sums)))
stopifnot( apply(x, 2, is.vector))
apply(x, 2, sort)
#
tb<-cbind(a1=c(1:5),a2=c(5:10),a3=c(10:15))
dimnames(tb)[[1]]<-LETTERS[1:6]
tb
col.sums<-apply(tb,sum)
col.sums
row.sums<-apply(tb,1,sum)
row.sums
rbind(cbind(tb,Rtot=apply(tb,1,sum),Ctot=c(apply(tb,2,sum),sum(apply(tb,2,sum)))))
rbind(cbind(tb,Rtot=row.sums),Ctot=c(col.sums,sum(col.sums)))
#
names(dimnames(x)) <- c("row", "col")
x3 <- array(x, dim = c(dim(x),3),
            dimnames = c(dimnames(x), list(C = paste0("cop.",1:3))))
#
identical(x,  apply( x,  2,  identity))
identical(x3, apply(x3, 2:3, identity))
#
cave <- function(x, c1, c2) c(mean(x[c1]), mean(x[c2]))
apply(x, 1, cave,  c1 = "x1", c2 = c("x1","x2"))
#
ma <- matrix(c(1:6, 2, 6:10), nrow = 2)
ma
apply(ma, 1, table)  #--> a list of length 2
apply(ma, 1, stats::quantile)
#
stopifnot(dim(ma) == dim(apply(ma, 1:2, sum)))
#
## Example with different lengths for each call
z <- array(1:24, dim = 2:4)
z
zseq <- apply(z, 1:2, function(x) seq_len(max(x)))
zseq         ## a 2 x 3 matrix
typeof(zseq) ## list
dim(zseq) ## 2 3
zseq[1,]
apply(z, 3, function(x) seq_len(max(x))
#
help(sapply)
#
x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,TRUE,FALSE,TRUE))
x
# compute the list mean for each list element
lapply(x, mean)
#
lapply(x, quantile, probs = 1:3/4)
sapply(x, quantile)
#
i39 <- sapply(3:9, seq) # list of vectors
i39
sapply(i39, fivenum)
vapply(i39, fivenum,
       c(Min. = 0, "1st Qu." = 0, Median = 0, "3rd Qu." = 0, Max. = 0))
#
(v <- structure(10*(5:8), names = LETTERS[1:4]))
f2 <- function(x, y) outer(rep(x, length.out = 3), y)
(a2 <- sapply(v, f2, y = 2*(1:5), simplify = "array"))
a.2 <- vapply(v, f2, outer(1:3, 1:5), y = 2*(1:5))
stopifnot(dim(a2) == c(3,5,4), all.equal(a2, a.2),
          identical(dimnames(a2), list(NULL,NULL,LETTERS[1:4])))

hist(replicate(100, mean(rexp(10))))
foo <- function(x = 1, y = 2) c(x, y)
# does not work: bar <- function(n, ...) replicate(n, foo(...))
bar <- function(n, x) replicate(n, foo(x = x))
bar(5, x = 3)
bar(6,x=4)
#
gitcreds::gitcreds_set()
