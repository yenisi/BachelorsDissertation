rm(list=ls())
is_self_avoiding <- function(walk) {
  !any(duplicated(walk))
}


# Check if a matrix contains a specific row
check_matrix_row <- function(matrix, row) {
  matches = apply(matrix, 1, function(x) all(x == row))
  if (any(matches)) {
    return(which(matches))
  } else {
    return(0)
  }
}

# Moves the last point by a direction (U, D, R, L)
move <- function(pos, orient) {
  switch(orient,
         U = pos + c(0, 1),
         D = pos + c(0, -1),
         R = pos + c(1, 0),
         L = pos + c(-1, 0))
}
n=500
saw <- function(steps = n) {
    k = 0
    path = matrix(0, nrow = 1, ncol = 2) # starting point = c(0, 0)
    i = 1
    
    while (i < steps) {
        k = k + 1
        
        # calculates positions after all possible movements
        possible_pos = rbind(move(path[i, ], 'U'),
                              move(path[i, ], 'D'),
                              move(path[i, ], 'R'),
                              move(path[i, ], 'L'))
        
        # checks which directions are available
        avail_direction = apply(possible_pos, 1,
                                function(possible_pos)
                        check_matrix_row(path, possible_pos))
        avail = ifelse(avail_direction > 0, 0, 1)
        names(avail) = c('U', 'D', 'R', 'L')
        
        # if no direction is available, go back
        if (all(avail == 0)) {
            goback = ifelse(i > 5, rbinom(1, 10, 0.5) + 
            rbinom(1, i, 0.05), 0) + 1
            path = path[1:(i - goback), ]
            i = i - goback - 1
            } else {
                # if one or more orientations are available, sample one 
                #uniformly and move to it
                chosen_direction = sample(x = names(avail), size = 1, 
                prob = avail)
                path = rbind(path, move(path[i, ], chosen_direction))
            }
        i = i + 1
    }    
    list(path = path, total_iterations = k)
}

# Test

walk1 = saw(steps = n)
pivot = sample((n-10):(n-2), size = 1);pivot
newchain = walk1$path[1:pivot,];newchain
colnames(newchain) = c("c1","c2")
tempchain = walk1$path[(pivot+1):n,];tempchain

tempmat = data.matrix(tempchain);tempmat

rotateby=sample(c(-pi/2, -pi, -3*pi/2, 1, 2, 3, 4), size=1);rotateby

if( rotateby == -pi/2 || rotateby == -pi || rotateby == -3*pi/2)
{
c11 = tempmat[-1,1] - tempmat[1,1];c11
c22 = tempmat[-1,2] - tempmat[1,2];c22
c1 = c11*cos(rotateby) - c22*sin(rotateby) +tempmat[1,1];c1
c2 = c11*sin(rotateby) + c22*cos(rotateby) +tempmat[1,2];c2
newmat1 = cbind(c1,c2);newmat1
newchain2 = data.frame(newmat1);newchain2
walk = rbind(newchain,tempchain[1,],newchain2)
}

#reflection on the x-axis
if (rotateby == 1)
{
c2= tempmat[-1,1]-tempmat[1,1]+tempmat[1,2];c2
c1 = -(tempmat[-1,2]-tempmat[1,2])+tempmat[1,1];c1
newmat1 = cbind(c1,c2);newmat1
newchain2 = data.frame(newmat1);newchain2
walk = rbind(newchain,tempchain[1,],newchain2);walk
}

#reflection on the y axis
if (rotateby == 2)
{
c2= -(tempmat[-1,1]-tempmat[1,1])+tempmat[1,2];c2
c1 = tempmat[-1,2]-tempmat[1,2]+tempmat[1,1];c1
newmat1 = cbind(c1,c2);newmat1
newchain2 = data.frame(newmat1);newchain2
walk = rbind(newchain,tempchain[1,],newchain2);walk
}

#reflection on x=y
if (rotateby == 3)
{
c2= tempmat[-1,1]-tempmat[1,1]+tempmat[1,2];c2
c1 = tempmat[-1,2]-tempmat[1,2]+tempmat[1,1];c1
#x=c2+tempmat[1,1];x
#y=c1+tempmat[1,2];y
newmat1 = cbind(c1,c2);newmat1
newchain2 = data.frame(newmat1);newchain2
walk = rbind(newchain,tempchain[1,],newchain2);walk
}

#reflection on the x=-y
if (rotateby == 4)
{
c1 = -(tempmat[-1,1] - tempmat[1,1])+tempmat[1,1];c1
c2 = -(tempmat[-1,2] - tempmat[1,2])+tempmat[1,2];c2
newmat1 = cbind(c1,c2);newmat1
newchain2 = data.frame(newmat1);newchain2
walk = rbind(newchain,tempchain[1,],newchain2);walk
}


is_self_avoiding(walk)


k = 1
while (is_self_avoiding(walk)==FALSE ) {
  # Choose a random pivot
  
  pivot = sample((n-490):(n-2), size = 1,replace=F)
  k = k+1
  # Perform pivot move
  new_chain = walk[1:pivot, ]
  temp_chain = walk[(pivot + 1):n, ];temp_chain
  tempmat = data.matrix(temp_chain);tempmat
rotateby=sample(c(-pi/2, -pi, -3*pi/2, 1, 2, 3, 4), size=1);rotateby

if( rotateby == -pi/2 || rotateby == -pi || rotateby == -3*pi/2)
{
c11 = tempmat[-1,1] - tempmat[1,1];c11
c22 = tempmat[-1,2] - tempmat[1,2];c22
c1 = c11*cos(rotateby) - c22*sin(rotateby) +tempmat[1,1];c1
c2 = c11*sin(rotateby) + c22*cos(rotateby) +tempmat[1,2];c2
newmat1 = cbind(c1,c2);newmat1
newchain2 = data.frame(newmat1);newchain2
new_walk = rbind(new_chain,temp_chain[1,],newchain2);new_walk
}


#reflection on the x-axis
if (rotateby == 1)
{
c2= tempmat[-1,1]-tempmat[1,1]+tempmat[1,2];c2
c1 = -(tempmat[-1,2]-tempmat[1,2])+tempmat[1,1];c1
newmat1 = cbind(c1,c2);newmat1
newchain2 = data.frame(newmat1);newchain2
new_walk = rbind(new_chain,temp_chain[1,],newchain2);new_walk
}

#reflection on the y axis
if (rotateby == 2)
{
c2= -(tempmat[-1,1]-tempmat[1,1])+tempmat[1,2];c2
c1 = tempmat[-1,2]-tempmat[1,2]+tempmat[1,1];c1
#c1= -tempmat[-1,1];c1
#c2 = tempmat[-1,2];c2
newmat1 = cbind(c1,c2);newmat1
newchain2 = data.frame(newmat1);newchain2
new_walk = rbind(new_chain,temp_chain[1,],newchain2);new_walk
}

#reflection on x=y
if (rotateby == 3)
{
c2= tempmat[-1,1]-tempmat[1,1]+tempmat[1,2];c2
c1 = tempmat[-1,2]-tempmat[1,2]+tempmat[1,1];c1
newmat1 = cbind(c1,c2);newmat1
newchain2 = data.frame(newmat1);newchain2
new_walk = rbind(new_chain,temp_chain[1,], newchain2);new_walk
}

#reflection on the x=-y
if (rotateby == 4)
{
c1 = -(tempmat[-1,1] - tempmat[1,1])+tempmat[1,1];c1
c2 = -(tempmat[-1,2] - tempmat[1,2])+tempmat[1,2];c2
newmat1 = cbind(c1,c2);newmat1
newchain2 = data.frame(newmat1);newchain2
new_walk = rbind(new_chain,temp_chain[1,],newchain2);new_walk
}

  # Check if new walk is self-avoiding
  if (is_self_avoiding(new_walk) == TRUE) {
    walk = new_walk
  }
}
#walk
is_self_avoiding(walk)
# Plot the resulting walks
par(mfrow = c(1,2))
plot(walk1$path,type='l', ylab=NA, xlab="Initial self-avoiding walk")
xrange = range(walk[ , 1])
yrange = range(walk[ , 2])
plot(walk,type ='l', ylab=NA, xlab="New self-avoiding 
walk")