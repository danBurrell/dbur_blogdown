mandelbrot_set <- function(
    xmin = -2.1, 
    xmax = 0.8, 
    nx = 500,
    ymin = -1.3, 
    ymax = 1.3, 
    ny = 500,
    n = 100, 
    showplot = TRUE,
    cols = colorRampPalette(c("black","cyan","cyan3","black"))(11)) 
{
    
    # variables
    x <- seq(xmin, xmax, length.out=nx)
    y <- seq(ymin, ymax, length.out=ny)
    c <- outer(x,y*1i,FUN="+")
    z <- matrix(0.0, nrow=length(x), ncol=length(y))
    k <- matrix(0.0, nrow=length(x), ncol=length(y))
    
    for (rep in 1:n) { 
        index <- which(Mod(z) < 2)
        z[index] <- z[index]^2 + c[index]
        k[index] <- k[index] + 1
    }
    
    if (showplot==TRUE) { image(x,y,k,col=cols, xlab="Re(c)", ylab="Im(c)")}
    
    return(k)
    
}

mandelbrot_set()
