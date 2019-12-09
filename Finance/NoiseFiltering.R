#library(MiscFunctions)

LineEq <- function(x, x1, y1, x2, y2) {
  if(x1 == x2) return(0)
  a = (y2-y1) / (x2 - x1)
  b =  -a * x1 + y1
  y = a * x + b  
  y
}

InterpolByN <- function(x1, y1, x2, y2, n) {
  interp = numeric(n)
  step = (x2-x1)/n
  for(i in 1:n) {
    x = x1 + i * step
    interp[i] = LineEq(x,x1,x2,y1,y2)
  }
  interp
}

InterpolByStep <- function(x1, y1, x2, y2, step) {
  n = (x2-x1)/step
  interp = numeric(n)
  for(i in 1:n) {
    x = x1 + i * step
    interp[i] = LineEq(x,x1,y1,x2,y2)
  }
  interp
}

Spline <- function(points, step) {
  sp = numeric(0)
  n = 4 #length(points[1,])-1
  for(i in 1:n) {
    interp = InterpolByStep(points[1,i], points[2,i], points[1,i+1], points[2,i+1], step)
    sp = c(sp, interp)
  }    
  sp
}

x = LineEq(3,1,6,1,6); x

interp = InterpolByN(1,6,1,3,20); sp

points = cbind(c(1,1), c(2,5), c(3,4), c(7,9), c(6,6)); points
length(points[1,])

interp = InterpolByStep(1,1,2,5,0.1); interp
plot(interp,type ="o", col="blue", lwd="2")
interp = InterpolByStep(2,5,3,4,0.1); interp
plot(interp,type ="o", col="blue", lwd="2")

sp = Spline(points, 0.05)
plot(sp,type ="o", col="blue", lwd="2")

rnorm(3, mean=0, sd=1)
