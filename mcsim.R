## this is a monte carlo simulation to predict exit preferences
## author: amy le
## date: may 11, 2017

### notes
## using a normal distribution for intial positions
## using a bimodal nonsymmetric distribution for navigation

### set parameters
## trials = number of paths per person
t <- 10

## number of people in the simulation
p <- 10

## grid size
g <- 50

## shape; s = square and t = triangle
shape <- "t"

## simulation time; number of allowed steps
nsteps <- round(10*g)

## navigation parameters
mu1 <- log(1)   
mu2 <- log(50)
sig1 <- log(3)
sig2 <- log(3)
cpct <- 0.4   

## helper functions
defExits <- function(shape){
    if(shape == "s"){
        ctr <- data.frame(north=0, south=0, east=0, west=0)
    }else if(shape == "t"){
        ctr <- data.frame(south=0, east=0, northwest=0)
    }else{}
    ctr
}

rbimodal <- function (n, cpct, mu1, mu2, sig1, sig2) {
  y0 <- rlnorm(n, mean=mu1, sd = sig1)
  y1 <- rlnorm(n, mean=mu2, sd = sig2)

  flag <- rbinom(n, size=1, prob=cpct)
  y <- log(y0*(1 - flag) + y1*flag)
}
## bimodalData <- rbimodal(n=10000, cpct, mu1, mu2, sig1, sig2)
## hist(bimodalData)

## return TRUE if current position is in the grid, else FALSE
inGrid <- function(xc, yc, shape, g){
    r <- TRUE
    if(shape == "s"){
        if(xc >= g | yc >= g | xc <= 0 | yc <= 0){
            r <- FALSE
        }
    }else if(shape == "t"){
        if(yc >= xc | xc >= g | yc <= 0){
            r <- FALSE
        }
    }
    else{
        r <- FALSE
    }
    r
}

## pedestrian initial positions
## they start somewhere in the middle (mostly)
initPos <- function(p, g, shape){
    if(shape == "s"){
        x0 <- abs(round(rnorm(p, g/2, g/4)))
        y0 <- abs(round(rnorm(p, g/2, g/4)))
        xy0 <- data.frame(x0, y0)

        ## check positions are within bounds of event and adjust 
        xy0[xy0$x0 >= g, 1] <- xy0[xy0$x0 >= g, 1] - g # west
        xy0[xy0$y0 >= g, 2] <- xy0[xy0$y0 >= g, 2] - g # north
        xy0[xy0$x0 == 0, 1] <- round(g/2) # south
        xy0[xy0$y0 == 0, 2] <- round(g/2) # east

    }else if (shape == "t"){
        ## set side lenghts
        a <- g # opposite of (x1,y1)
        b <- g # opposite of (x2,y2)
        c <- sqrt(a^2 + b^2) # opposite of (x3,y3)

        ## vertices
        x <- c(0, g, g)
        y <- c(0, g, 0)
        
        ## find incenter of right t
        xi <- (a*x[1] + b*x[2] + c*x[3])/(a + b + c)
        yi <- (a+y[1] + b*y[2] + c*y[3])/(a + b + c)

        ## use inradius as the std deviation
        ri <- 0.5*(a + b - c)

        x0 <- abs(round(rnorm(p, xi, ri)))
        y0 <- abs(round(rnorm(p, yi, ri)))
        xy0 <- data.frame(x0, y0)
        
        ## check positions are within bounds of event and adjust 
        xy0[xy0$x0 >= g, 1] <- round(xi) # east
        xy0[xy0$y0 == 0, 2] <- round(yi) # south
        xy0[xy0$y0 >= xy0$x0, 1] <- round(xi) # northwest
        xy0[xy0$y0 >= xy0$x0, 2] <- round(yi) # northwest

    }else{
        x0 <- seq(0,g,1)
        y0 <- x0
        xy0 <- data.frame(x0, y0)

    }
    xy0
}

checkExit <- function(xc, yc, shape, ctr){
    if(shape == "s") {
        if(xc == g){
            ctr['east'] <- ctr['east'] + 1
        }else if(yc == 0){
            ctr['south'] <- ctr['south'] + 1
        }else if(yc == g){
            ctr['north'] <- ctr['north'] + 1
        }else if(xc == 0){
            ctr['west'] <- ctr['west'] + 1
        }else{}
        
    }else if (shape == "t"){
        if(xc == g){
            ctr['east'] <- ctr['east'] + 1
        }else if(yc == 0){
            ctr['south'] <- ctr['south'] + 1
        }else if(xc == yc) {
            ctr['northwest'] <- ctr['northwest'] + 1
        }else{}
    }
    ctr
}

simPath <- function(j, xy, shape){
    xc <- xy[1]
    yc <- xy[2]
    curstep <- 0
    h <- data.frame(x=xc, y=yc)
    while(curstep < nsteps & inGrid(xc, yc, shape, g)){
        ## choose a direction
        d <- round(100*rbimodal(1, cpct, mu1, mu2, sig1, sig2)) %% 4
        if(d == 1){ # north
            yc <- yc + 1
        }else if (d == 2){ # south
            yc <- yc - 1
        }else if (d == 3){ # east
            xc <- xc + 1
        }else{ #d == 0 (west)
            xc <- xc - 1
        }
        curstep <- curstep + 1
        h <- rbind(h,c(xc,yc))
    }
    lines(h,col="gray")
    points(tail(h,1),col="red")
    ctr <- checkExit(xc, yc, shape, ctr)
}

simPosition <- function(xy, ctr, shape){
    output <- apply(matrix(1:t), 1, function(j) simPath(j, xy, shape))
    tc <- sapply(names(ctr), function(q) {
        dc <- lapply(output, `[[`, q)
        Reduce('+', dc)/t}
        )
    tc
}

plotEvent <- function(xy, shape, g){
    if(shape == "s"){
        v <- data.frame(x=c(0,0,g,g,0), y=c(0,g,g,0,0))
    }else if(shape == "t"){
        v <- data.frame(x=c(0,g,g,0), y=c(0,g,0,0))
    }
    par(oma=c(0, 0, 0, 5))
    ##plot(xy,col="blue",pch=15,xlim=c(0,g),ylim=c(0,g), frame.plot=FALSE, axes=FALSE, ylab='', xlab='')
    plot(v,lty=1,xlim=c(0,g),ylim=c(0,g), frame.plot=FALSE, axes=FALSE, ylab='', xlab='')
    lines(v)
}

plotProb <- function(posHistory, shape){
    exitPref <- apply(posHistory, 1, mean)
    lost <- 100*(1-sum(exitPref))
    exitPref <- 100*exitPref
    legend(par('usr')[2], par('usr')[4], bty='n', xpd=NA,
           title=sprintf("lost: %1.0f%%", lost),
           c("Start", "End"), pch=c(1, 1), col=c('blue','red'))

    if(shape == "s"){
        mtext(sprintf("north: %1.0f%%",exitPref['north']), side = 3)
        mtext(sprintf("south: %1.0f%%",exitPref['south']), side = 1)
        mtext(sprintf("east: %1.0f%%",exitPref['east']), side = 4)
        mtext(sprintf("west: %1.0f%%",exitPref['west']), side = 2)
    }else if(shape == "t"){
        mtext(sprintf("northwest: %1.0f%%",exitPref['northwest']), side = 3)
        mtext(sprintf("south: %1.0f%%",exitPref['south']), side = 1)
        mtext(sprintf("east: %1.0f%%",exitPref['east']), side = 4)
    }
}


## begin simulation
## for each p, simulate t runs
## set the counter
ctr <- defExits(shape)
xy0 <- initPos(p, g, shape)
xy00 <- xy0[1:2,]

plotEvent(xy0, shape, g)
posHistory <- apply(xy0, 1, function(k) simPosition(k, ctr, shape))
plotProb(posHistory, shape)


