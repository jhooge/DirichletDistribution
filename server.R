#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(MCMCpack)
library(ggplot2)

diri.contour <- function(a, x = NULL, n = 100) {
  ## a are the estimated Dirichlet parameters
  ## n shows the number of points at which the density is calculated
  ## so, n^2 points are used.
  ## x should be a 3-part compositional data or NULL for no data
  x1 <- seq(0.001, 0.999, length = n)  ## coordinates of x
  sqrt3 <- sqrt(3)
  x2 <- seq(0.001, sqrt3/2 - 1e-03, length = n)  ## coordinates of y
  mat <- matrix(nrow = n, ncol = n)
  be <- prod( gamma(a)) / gamma(sum(a) )  ## beta function
  
  for ( i in 1:c(n/2) ) {
    for (j in 1:n) {
      if ( x2[j] < sqrt3 * x1[i] ) {
        ## This checks if the point will lie inside the triangle
        ## the next three lines invert the points which lie inside
        ## the triangle back into the composition in S^2
        w3 <- 2 * x2[j] / sqrt3
        w2 <- x1[i] - x2[j]/sqrt3
        w1 <- 1 - w2 - w3
        w <- c(w1, w2, w3)
        can <- prod( w^(a - 1) ) / be
        if (abs(can) < Inf)  mat[i, j] <- can  else  mat[i, j] <- NA
      } else  mat[i, j] <- NA
    }
  }
  for (i in c(n/2 + 1):n) {
    for (j in 1:n) {
      ## This checks if the point will lie inside the triangle
      if ( x2[j] < sqrt3 - sqrt3 * x1[i] ) {
        ## the next three lines invert the points which lie inside
        ## the triangle back into the composition in S^2
        w3 <- 2 * x2[j] / sqrt3
        w2 <- x1[i] - x2[j]/sqrt3
        w1 <- 1 - w2 - w3
        w <- round(c(w1, w2, w3), 6)
        can <- prod( w^(a - 1) ) / be
        if (abs(can) < Inf)  mat[i, j] <- can  else  mat[i, j] <- NA
      } else  mat[i, j] <- NA
    }
  }
  
  contour(x1, x2, mat, col = 3)  ## contour plots
  b1 <- c(0.5, 0, 1, 0.5)
  b2 <- c(sqrt3/2, 0, 0, sqrt3/2)
  b <- cbind(b1, b2)
  ## the next line draws the triangle in the two dimensions
  points(b[, 1], b[, 2], type = "l", xlab = " ", ylab = " ")
  
  if ( !is.null(x) ) {
    proj <- matrix(c(0, 1, 0.5, 0, 0, sqrt3/2), ncol = 2)
    xa <- x %*% proj
    points(xa[, 1], xa[, 2])
  }
}

diri.vectors <- function(a, x, n) {
  dat <- data.frame(item=factor(rep(1:3, n)), 
                    draw=factor(rep(1:n, each=3)), 
                    value=as.vector(t(x)))
  
  ggplot(dat,aes(x=item, y=value, ymin=0, ymax=value)) + 
    geom_point(colour=I("blue"))       + 
    geom_linerange(colour=I("blue"))   + 
    facet_wrap(~draw,ncol=5)           + 
    scale_y_continuous(lim=c(0,1))     +
    theme_bw()
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  alpha <- reactive({
    c(input$a1, input$a2, input$a3)
  })
  
  draws <- reactive({
    rdirichlet(input$n, alpha())
  })
  
  output$diriContour <- renderPlot({
    diri.contour(alpha(), x=draws())
  })
  
  output$diriVectors <- renderPlot({
    diri.vectors(alpha(), draws(), input$n)
  })
  
})
