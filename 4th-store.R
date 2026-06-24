#####################################################################
#####################################################################

# Goal:construct a store with 2 parts of vegetable zone.
# with the same walking area as the first store.

#####################################################################
#####################################################################


############## Pre-function setting
corner_x<-c(5,5,8,8,13,13,17,17,22,22,25,25)
corner_y<-c(10,20,20,10,10,20,20,10,10,20,20,10)
corner<-cbind(corner_x,corner_y)

entrance<-c(30,25)
exit<-c(30,5)
####### function for choosing shopping list for each customer randomly
shopping_list<-function(method,p1,p2,number)
{
  if(method=="normal")## choose the original shopping list
  {
    n<-ceiling(rnorm(1,p1,p2))-1
    while(n==0){n<-ceiling(rnorm(1,p1,p2))-1}
  }else if(method=="gamma")
  {
    n<-ceiling(rgamma(1,p1,p2)) -1
    while(n==0){n<-ceiling(rnorm(1,p1,p2))-1}
  }else ## choose the first shopping point
  {
    n<-number
  }
  
  length_edge<-180 # length of edge of the store and the vegetable zones
  ## worth to say, to avoid collision around the entrance
  ## we set (20,30) to (30,30) & (30,20) to (30,30) with no shopping points
  
  corner_x<-c(5,5,8,8,13,13,17,17,22,22,25,25)
  corner_y<-c(10,20,20,10,10,20,20,10,10,20,20,10)
  
  epsilon<-sample(c(1:16),n,replace = TRUE
                  , prob=c(30/length_edge,20/length_edge,20/length_edge,30/length_edge
                           ,10/length_edge,3/length_edge,10/length_edge,3/length_edge
                           ,10/length_edge,4/length_edge,10/length_edge,4/length_edge
                           ,10/length_edge,3/length_edge,10/length_edge,3/length_edge))
  
  point_number<-numeric(16)
  for (i in 1:16)
  {
    point_number[i]<-length(which(epsilon==i))
  }
  
  x<-numeric(n)
  y<-numeric(n)
  if(point_number[1]!=0)
  {
    x[1:point_number[1]]<-0
    y[1:point_number[1]]<-runif(point_number[1],0,30)
  }
  if(point_number[2]!=0)
  {
    x[min((point_number[1]+1),sum(point_number[1:2])):sum(point_number[1:2])]<-runif(point_number[2],0,20)
    y[min((point_number[1]+1),sum(point_number[1:2])):sum(point_number[1:2])]<-30
  }
  if(point_number[3]!=0)
  {
    x[min((sum(point_number[1:2])+1),sum(point_number[1:3])):sum(point_number[1:3])]<-30
    y[min((sum(point_number[1:2])+1),sum(point_number[1:3])):sum(point_number[1:3])]<-runif(point_number[3],0,20)
  }
  if(point_number[4]!=0)
  {
    x[min((sum(point_number[1:3])+1),sum(point_number[1:4])):sum(point_number[1:4])]<-runif(point_number[4],0,30)
    y[min((sum(point_number[1:3])+1),sum(point_number[1:4])):sum(point_number[1:4])]<-0
  }
  if(point_number[5]!=0)
  {
    x[min((sum(point_number[1:4])+1),sum(point_number[1:5])):sum(point_number[1:5])]<-corner_x[1]
    y[min((sum(point_number[1:4])+1),sum(point_number[1:5])):sum(point_number[1:5])]<-runif(point_number[5],corner_y[1],corner_y[2])
  }
  if(point_number[6]!=0)
  {
    x[min((sum(point_number[1:5])+1),sum(point_number[1:6])):sum(point_number[1:6])]<-runif(point_number[6],corner_x[2],corner_x[3])
    y[min((sum(point_number[1:5])+1),sum(point_number[1:6])):sum(point_number[1:6])]<-corner_y[2]
  }
  if(point_number[7]!=0)
  {
    x[min((sum(point_number[1:6])+1),sum(point_number[1:7])):sum(point_number[1:7])]<-corner_x[3]
    y[min((sum(point_number[1:6])+1),sum(point_number[1:7])):sum(point_number[1:7])]<-runif(point_number[7],corner_y[4],corner_y[3])
  }
  if(point_number[8]!=0)
  {
    x[min((sum(point_number[1:7])+1),sum(point_number[1:8])):sum(point_number[1:8])]<-runif(point_number[8],corner_x[1],corner_x[4])
    y[min((sum(point_number[1:7])+1),sum(point_number[1:8])):sum(point_number[1:8])]<-corner_y[4]
  }
  if(point_number[9]!=0)
  {
    x[min((sum(point_number[1:8])+1),sum(point_number[1:9])):sum(point_number[1:9])]<-corner_x[5]
    y[min((sum(point_number[1:8])+1),sum(point_number[1:9])):sum(point_number[1:9])]<-runif(point_number[9],corner_y[5],corner_y[6])
  }
  if(point_number[10]!=0)
  {
    x[min((sum(point_number[1:9])+1),sum(point_number[1:10])):sum(point_number[1:10])]<-runif(point_number[10],corner_x[6],corner_x[7])
    y[min((sum(point_number[1:9])+1),sum(point_number[1:10])):sum(point_number[1:10])]<-corner_y[6]
  }
  if(point_number[11]!=0)
  {
    x[min((sum(point_number[1:10])+1),sum(point_number[1:11])):sum(point_number[1:11])]<-corner_x[7]
    y[min((sum(point_number[1:10])+1),sum(point_number[1:11])):sum(point_number[1:11])]<-runif(point_number[11],corner_y[8],corner_y[7])
  }
  if(point_number[12]!=0)
  {
    x[min((sum(point_number[1:11])+1),sum(point_number[1:12])):sum(point_number[1:12])]<-runif(point_number[12],corner_x[5],corner_x[8])
    y[min((sum(point_number[1:11])+1),sum(point_number[1:12])):sum(point_number[1:12])]<-corner_y[8]
  }
  if(point_number[13]!=0)
  {
    x[min((sum(point_number[1:12])+1),sum(point_number[1:13])):sum(point_number[1:13])]<-corner_x[9]
    y[min((sum(point_number[1:12])+1),sum(point_number[1:13])):sum(point_number[1:13])]<-runif(point_number[13],corner_y[9],corner_y[10])
  }
  if(point_number[14]!=0)
  {
    x[min((sum(point_number[1:13])+1),sum(point_number[1:14])):sum(point_number[1:14])]<-runif(point_number[14],corner_x[10],corner_x[11])
    y[min((sum(point_number[1:13])+1),sum(point_number[1:14])):sum(point_number[1:14])]<-corner_y[10]
  }
  if(point_number[15]!=0)
  {
    x[min((sum(point_number[1:14])+1),sum(point_number[1:15])):sum(point_number[1:15])]<-corner_x[11]
    y[min((sum(point_number[1:14])+1),sum(point_number[1:15])):sum(point_number[1:15])]<-runif(point_number[15],corner_y[12],corner_y[11])
  }
  if(point_number[16]!=0)
  {
    x[min((sum(point_number[1:15])+1),sum(point_number[1:16])):sum(point_number[1:16])]<-runif(point_number[16],corner_x[9],corner_x[12])
    y[min((sum(point_number[1:15])+1),sum(point_number[1:16])):sum(point_number[1:16])]<-corner_y[12]
  }
  
  data<-cbind(x,y)
  
  entrance<-c(30,25)
  exit<-c(30,5)
  ##If it coincides with the exit, delete this point 
  if(setequal(which(data[,1]==exit[1]&data[,2]==exit[2]),integer(0))) 
  {data.new<-data}else 
  {
    row<-which(data[,1]==exit[1]&data[,2]==exit[2])
    data<-data[-row,]
  }
  data.new<-matrix(data=NA,ncol=2,nrow=10000)
  
  k<-dim(data)[1]
  
  data.new[1:k,1:2]<-data
  
  return(data.new)
  
}
########################################################################





####### function for judging if the next step of customer will across vegetable zone
judgment<-function(x1,y1,x2,y2)
{
  corner_x<-c(5,5,8,8,13,13,17,17,22,22,25,25)
  corner_y<-c(10,20,20,10,10,20,20,10,10,20,20,10)
  corner<-cbind(corner_x,corner_y)
  a<-y1-y2
  b<-x2-x1
  c<-x1*y2-x2*y1 #推导过程在notability的New store文件
  #if(length(which(a*corner_x+b*corner_y+c<=0))!=4&length(which(a*corner_x+b*corner_y+c>=0))!=4)
  # the following method using cross product is more precisely numerically.
  # especially when the 2 original points give us a nearly vertical line
  # only b will be very small, then because of the precision, ax+by+c will round to 0 without considerition of tiny differenct from b
  # but for nearly vertical line, x2-x1 will be very small, x-x1 will also be very small
  # so this kind of tiny difference could be taken into account.
  if(length(which((x2-x1)*(corner_y-y1)-(y2-y1)*(corner_x -x1)<=0))!=12&length(which((x2-x1)*(corner_y-y1)-(y2-y1)*(corner_x -x1)>=0))!=12)
  {
    if((x1<=min(corner_x)&x2<=min(corner_x)) #最左边的一片
       |(x1>=max(corner_x)&x2>=max(corner_x)) #最右边的一片
       |(y1<=min(corner_y)&y2<=min(corner_y)) # 最下面一片
       |(y1>=max(corner_y)&y2>=max(corner_y)) #最上面一片
       |(x1>=8&x1<=13&x2>=8&x2<=13) #zone 1&2 中间的部分
       |(x1>=17&x1<=22&x2>=17&x2<=22)) #zone 2&3 中间的部分
    {
      value<-0
    }else
    {
      value<-1
    }
  }else
  {
    value<-0
  }
  
  return(value)
}
## 0 means not across the vegetable zone, 1 means across
########################################################################











####### function for judging how could customer do next step without across vegetable zone by corner
#############################################################
#############################################################
#############################################################
#############################################################


# Function to find the minimum length of all simple paths 
min_length_path <- function(adj_matrix, start, end) {
  # Validate inputs
  n <- nrow(adj_matrix)
  
  # Convert adjacency matrix to adjacency list for efficiency
  adj_list <- vector("list", n)
  for(i in 1:n) {
    adj_list[[i]] <- which(adj_matrix[i, ] == 1)
  }
  
  all_paths <- list()
  visited <- logical(n)
  current_path <- integer(0)
  paths_found <- FALSE
  
  # DFS function to find all paths
  dfs_paths <- function(current, target, visited, path) {
    # Add current node to path
    path <- c(path, current)
    visited[current] <- TRUE
    
    # If we reached the target, save the path
    if (current == target) {
      all_paths <<- c(all_paths, list(path))
      paths_found <<- TRUE
    } else {
      # Continue to all unvisited neighbors
      for (neighbor in adj_list[[current]]) {
        if (!visited[neighbor]) {
          dfs_paths(neighbor, target, visited, path)
        }
      }
    }
    
    # Backtrack
    visited[current] <- FALSE
  }
  
  # Start DFS from the start node
  dfs_paths(start, end, visited, current_path)
  
  # Check if any paths were found
  if (length(all_paths) == 0) {
    return(list(connected = FALSE, 10000 ))
  } else {
    # calculate the length of each path
    dis_paths<-rep(0,length(all_paths))
    for(i in 1:length(all_paths)) {
      pair<-all_paths[[i]]
      for(j in 1:(length(pair)-1)){
        dis_paths[i]<-dis_paths[i]+sqrt((corner_x[pair[j]]-corner_x[pair[j+1]])^2+(corner_y[pair[j]]-corner_y[pair[j+1]])^2)
      }
    }
    
    return(list(connected = TRUE,min(dis_paths),all_paths[[which.min(dis_paths)]]))
    
  }
  
  
  
}

############## test 
# all_paths <- find_all_directed_paths(adj_matrix, 1, 6) (this function is in file "all_path")
#for(pair in all_paths){print(pair)}
##############





#### BFS is a graph traversal algorithm that explores all nodes at the present depth level before moving on to nodes at the next depth level. 
#### It's like exploring a tree level by level.
#### Used for connected components, shortest paths in unweighted graphs
#### Complexity analysis: O(V + E) for adjacency list

#### Advantages for connectivity checking:
#### (1)Finds shortest path quickly - stops as soon as target is found
#### (2)Uses less memory than DFS for connectivity check
#### (3)Guaranteed to find if a path exists
#### (4)More efficient than enumerating all paths just to check connectivity

#### Quick connectivity check: Uses BFS to quickly determine if nodes are connected before exhaustive search
#### Efficiency: Avoids expensive DFS when nodes are clearly disconnected


# Function to check if two nodes are connected (BFS approach)
are_nodes_connected <- function(adj_matrix, start, end) {
  if (start == end) return(FALSE)
  
  n <- nrow(adj_matrix)
  visited <- logical(n)
  queue <- integer(0)
  
  # BFS to check connectivity
  queue <- c(queue, start)
  visited[start] <- TRUE
  
  while (length(queue) > 0) {
    current <- queue[1]
    queue <- queue[-1]
    
    if (current == end) {
      return(TRUE)
    }
    
    neighbors <- which(adj_matrix[current, ] == 1)
    for (neighbor in neighbors) {
      if (!visited[neighbor]) {
        visited[neighbor] <- TRUE
        queue <- c(queue, neighbor)
      }
    }
  }
  
  return(FALSE)
}



############################################
############################################
############################################ build the adjacency matrix
#######
####### interpretation:
#(1) since we need the path, so we don't allow any points arrive itself, i.e. adj(i,i)=0
#(2) WLOG and for convenience, we assume the 1st point is on the left of the 2nd point, LATER, we could use one line code to complete
#(3) we need to use "judgment" function to justify whether any 2 nodes are accessible in 1 step. If accessible, return 1, otherwise, return 0
#######
#######

######## method 1: build by hand (work for simply vegetable zones)
adj_matrix <- matrix(c( 0,1,0,0,0,0,0,0,0,0,0,0
                       ,1,0,0,0,0,0,0,0,0,0,0,0
                       ,0,1,0,1,0,0,0,0,0,0,0,0
                       ,1,0,1,0,0,0,0,0,0,0,0,0
                       ,1,0,1,1,0,1,0,0,0,0,0,0
                       ,0,1,1,1,1,0,0,0,0,0,0,0
                       ,0,1,1,0,0,1,0,1,0,0,0,0
                       ,1,0,0,1,1,0,1,0,0,0,0,0
                       ,1,0,0,1,1,0,1,1,0,1,0,0
                       ,0,1,1,0,0,1,1,1,1,0,0,0
                       ,0,1,1,0,0,1,1,0,0,1,0,1
                       ,1,0,0,1,1,0,0,1,1,0,1,0)
                     , nrow = 12, ncol = 12)
######## method 2: using "judgment" function to build (for general vegetable zones)
adj_matrix<-matrix(0,nrow = dim(corner)[1],ncol = dim(corner)[1])
for(i in 1:dim(corner)[1])
{
  for(j in 1:dim(corner)[1])
  {
    if (judgment(corner[i,1],corner[i,2],corner[j,1],corner[j,2])==0 & corner[j,1]>=corner[i,1] & i!=j)
    {
      adj_matrix[i,j]<-1
    }
  }
}

############################################
############################################
############################################  

# test
# min_length_path(adj_matrix, 1, 6)[[2]]


############################################
############################################  

#### construct adj_dis
#### now use above function to build the shortest path between each 2 nodes
adj_dis<-matrix(10000,nrow = dim(corner)[1],ncol = dim(corner)[1])
for(i in 1:dim(corner)[1])
{
  for(j in 1:dim(corner)[1])
  {
    if(i != j)
    {
      if(!are_nodes_connected(adj_matrix, i, j)) # not connected
      {
        adj_dis[i,j]<-10000
      }else
      {
        adj_dis[i,j]<-min_length_path(adj_matrix, i, j)[[2]]
      }
      
    }else
    {
      adj_dis[i,j]<-0
    }
    
  }
}



#############################################################
#############################################################

judgment_corner<-function(a,b,c,d)
{
  if(a<=c)
  {
    a1<-a
    b1<-b
    c1<-c
    d1<-d
  }else ## if the 2nd point is on the left, then exchange 2 points for convenience
  {
    a1<-c
    b1<-d
    c1<-a
    d1<-b
  }
  
  access_1<-rep(0,dim(corner)[1])
  access_2<-rep(0,dim(corner)[1])
  
  for(i in 1:dim(corner)[1])
  {
    if(judgment(a1,b1,corner_x[i],corner_y[i]) == 0 & abs(a1-corner_x[i])+abs(b1-corner_y[i]) !=0)
    {
      access_1[i]<-1
    }
    if(judgment(c1,d1,corner_x[i],corner_y[i]) == 0 & abs(c1-corner_x[i])+abs(d1-corner_y[i]) !=0)
    {
      access_2[i]<-1
    }
  }
  
  final_dis<-matrix(10000000,nrow = dim(corner)[1], ncol = dim(corner)[1])
  
  #### do the permutation of accessible corners 
  #### and calculate the distance of all possible paths between 2 target points
  for(i in which(access_1 == 1))
  {
    for(j in which(access_2 == 1))
    {
      final_dis[i,j]<-sqrt((corner_x[i]-a1)^2+(corner_y[i]-b1)^2) + sqrt((corner_x[j]-c1)^2+(corner_y[j]-d1)^2) + adj_dis[i,j]
    }
  }
  
  #### find the corresponding row and column to the minimum value in the matrix
  min_pos<-which(final_dis == min(final_dis), arr.ind = TRUE)
  
  #### turn back to the original point (a,b,c,d) and print the next step for customer
  if(a<=c)
  {
    return(c(min(final_dis), corner_x[min_pos[1]], corner_y[min_pos[1]]))
  }else
  {
    return(c(min(final_dis), corner_x[min_pos[2]], corner_y[min_pos[2]]))
  }
}

#test 

#judgment_corner(26,13,8,12)

########################################################################
#############################################################
#############################################################
#############################################################
#############################################################












#############################################################
#############################################################
#############################################################
#############################################################


### risktime: describe the time that 2 customers will have the collision
### We want this data since we don't need to do more repeated justification
##### if the time hasn't reached the minimum risktime
### i.e. there is no risk to have collision with any other customers
##### so just keep going
### in this way, we could lower down the Complexity and save the time to run the program

####### function for judging the risk number for each customer


my_min_distance<-function(data)
{
  distance<-matrix(data=NA,ncol = dim(data)[1] ,nrow = dim(data)[1])
  min_distance<-matrix(data = NA,ncol=2,nrow = dim(data)[1])
  for(i in 1:(dim(data)[1]))
  {
    for(j in 1:(dim(data)[1]))
    {
      distance[i,j]<-sqrt((data[i,1]-data[j,1])^2+(data[i,2]-data[j,2])^2)
    }
    dis<-distance[i,-i]
    min_distance[i,1]<-dis[which.min(dis)]
    if(which(distance[i,]==min_distance[i,1])[1]==i)
    {
      min_distance[i,2]<-which(distance[i,]==min_distance[i,1])[2]
    }else
    {
      min_distance[i,2]<-which(distance[i,]==min_distance[i,1])[1]
    }
  }
  ## divided by 0.6 since it's the distance reduced by 2 customers after 1 second
  ## and this "min_distance" represents "how many seconds later" this customer will collision with each other customer
  ## this matrix will be used into next function
  min_distance[,1]<-floor((min_distance[,1]-2)/0.6)+1
  
  return(min_distance)
}


#######
my_min_distance_change<-function(i,j,data)
{
  risktime_data<-matrix(data=NA,nrow=dim(data)[2]/2,ncol=2)
  ##data 是每个顾客每个时间点的位置信息
  ## i represents the time now 
  ## j is the serial number of this customer
  
  ## Build the data set required for my_min_distance 
  for(k in 1:customer.numeber)
  {
    if(k>j) ##之后进来的顾客慢一步，之前的顾客先挪动一步
    {
      risktime_data[k,1:2]<-data[i,(2*k-1):(2*k)]
    }else
    {
      risktime_data[k,1:2]<-data[i+1,(2*k-1):(2*k)]
    }
  }
  
  ## record the result of my_min_distance
  risktime<-my_min_distance(risktime_data)
  
  ##Follow up risktime according to the order of customer movement
  ## we need the specific time when will customer i will collide with other customer
  ## "my_min_distance" will just tell us how many seconds after will have the collision
  ## so we need to use "my_min_distance" plus the "time now"
  for(k in 1:customer.numeber)
  {
    if(k>j)
    {
      risktime[k,1]<-i+risktime[k,1]-1
    }else
    {
      risktime[k,1]<-i+risktime[k,1]
    }
  }
  
  ## Output the final result 
  return(risktime)
  
}

########################################################################



####### function for judging the the direction when customers are approaching collision
##If the customer is approaching to the shopping point 
##and the distance between him and the shopping point is smaller than 0.3 meters, 
##then we set his next step is on this shopping point
movement<-function(a,b,c,d)
{
  if(sqrt((c-a)^2+(d-b)^2)>0.3)
  {
    cos_theta<-(c-a)/sqrt((c-a)^2+(d-b)^2)
    sin_theta<-(d-b)/sqrt((c-a)^2+(d-b)^2)
    return(c(a+0.3*cos_theta,b+0.3*sin_theta))
  }else
  {
    return(c(c,d))
  }
}
####################
##choose the nearest shopping point from unfinished shopping list to be next shopping point
nearest_point<-function(data,a,b)
{
  complete_rows <- sum(complete.cases(data))
  distance<-rep(10000,complete_rows)
  
  for(i in 1:complete_rows)
  {
    if(judgment(data[i,1],data[i,2],a,b)==0) # if 2 points are accessible to each other, we don't need to use the nodes of vegestable zone
    {
      distance[i]<-sqrt((data[i,1]-a)^2+(data[i,2]-b)^2)
    }else # otherwise, we need to use nodes to help us get the complete path between 2 points
    {
      distance[i]<-judgment_corner(data[i,1],data[i,2],a,b)[1]
    }
    
  }
  
  return(data[which.min(distance),])
  
}



###### Judge if this is point is not only in the store but also not in the vegetable zone
in_the_zone<-function(a,b)
{
  if(a>30|a<0|b>30|b<0)
  {value<-0}else if(a<8&a>5&b<20&b>10)
  {value<-0}else if(a<17&a>13&b<20&b>10)
  {value<-0}else if(a<25&a>22&b<20&b>10)
  {value<-0}else
  {value<-1}
  return(value)
}

# 1 represents is ok, 0 represents not allowed to stay.

# test in_the_zone(14,18)

###### Judge the direction when customers is approaching to collide
judgment_direction<-function(a,b,aa,bb,c,d,cc,dd,data3)
{
  direction<-0  ### Set an initial value 
  
  output<-numeric(4)
  ## Adjusted from the nearest shopping point to the actual first corner for 1st customer
  if(judgment(a,b,aa,bb)==1)
  {
    really_nc1<-judgment_corner(a,b,aa,bb)
    aa<-really_nc1[2]
    bb<-really_nc1[3]
  }
  next_point<-movement(a,b,aa,bb)
  
  
  dis_next<-sqrt((data3[1,]-next_point[1])^2+(data3[2,]-next_point[2])^2)
  if(setequal(which(dis_next<2),integer(0))) # no any other customers is within 2 meters
  {
    output[1]<-1
    output[2:3]<-next_point
  }else #there is someone within 2 meters
  {
    ## Adjusted from the nearest shopping point to the actual first corner for 2nd customer
    if(judgment(c,d,cc,dd)==1)
    {
      really_nc2<-judgment_corner(c,d,cc,dd)
      cc<-really_nc2[2]
      dd<-really_nc2[3]
    }
    ##Calculate the angle between the headings of two customers
    cos_between<-(a-c)/sqrt((c-a)^2+(d-b)^2)
    sin_between<-(b-d)/sqrt((c-a)^2+(d-b)^2)
    cos_first<-(cc-c)/sqrt((cc-c)^2+(dd-d)^2)
    sin_first<-(dd-d)/sqrt((cc-c)^2+(dd-d)^2)
    cos_target<-cos_between*cos_first+sin_between*sin_first
    sin_target<-cos_between*sin_first-sin_between*cos_first
    
    ##Calculate the position after moving left or right or backward 
    right_x<-a+0.3*(-sin_between)
    right_y<-b+0.3*cos_between
    left_x<-a+0.3*sin_between
    left_y<-b+0.3*(-cos_between)
    back_x<-a+0.3*cos_between
    back_y<-b+0.3*sin_between
    
    dis_right<-sqrt((data3[1,]-right_x)^2+(data3[2,]-right_y)^2)
    dis_left<-sqrt((data3[1,]-left_x)^2+(data3[2,]-left_y)^2)
    dis_back<-sqrt((data3[1,]-back_x)^2+(data3[2,]-back_y)^2)
    
    ## Judge if move left, right or back would cross a wall
    if(setequal(which(dis_right<2),integer(0))&in_the_zone(right_x,right_y)==1)
    {judge_right<-1}else
    {judge_right<-0}
    if(setequal(which(dis_left<2),integer(0))&in_the_zone(left_x,left_y)==1)
    {judge_left<-1}else
    {judge_left<-0}
    if(setequal(which(dis_back<2),integer(0))&in_the_zone(back_x,back_y)==1)
    {judge_back<-1}else
    {judge_back<-0}
    
    ##Choose direction according to the probability to move left, right or back based on angle
    if(cos_target>=0&sin_target>=0)
    {
      if(judge_left==0&judge_back==0)
      {direction<-sample(c(1,2,3,4),1,replace=TRUE,prob=c(0,0,0,1))}else if(judge_left==1&judge_back==0)
      {direction<-sample(c(1,2,3,4),1,replace=TRUE,prob=c(0,0.5,0,0.5))}else if(judge_left==0&judge_back==1)
      {direction<-sample(c(1,2,3,4),1,replace=TRUE,prob=c(0,0,0.25,0.75))}else
      {direction<-sample(c(1,2,3,4),1,replace=TRUE,prob=c(0,0.5,0.25,0.25))}
      point<-switch(direction,c(right_x,right_y),c(left_x,left_y),c(back_x,back_y),c(a,b))
      output[1]<-0
      output[2:3]<-point
    }else if(cos_target<0&sin_target>=0)
    {
      if(judge_left==0)
      {direction<-sample(c(1,2,3,4),1,replace=TRUE,prob=c(0,0,0,1))}else
      {direction<-sample(c(1,2,3,4),1,replace=TRUE,prob=c(0,0.5,0,0.5))}
      point<-switch(direction,c(right_x,right_y),c(left_x,left_y),c(back_x,back_y),c(a,b))
      ## switch: according to the value of first variable, i.e. direction
      ## choose the corresponding case from the following statements
      ## eg. if direction =1, choose c(right_x,right_y)
      output[1]<-0
      output[2:3]<-point
    }else if(cos_target<0&sin_target<0)
    {
      if(judge_right==0)
      {direction<-sample(c(1,2,3,4),1,replace=TRUE,prob=c(0,0,0,1))}else
      {direction<-sample(c(1,2,3,4),1,replace=TRUE,prob=c(0.5,0,0,0.5))}
      point<-switch(direction,c(right_x,right_y),c(left_x,left_y),c(back_x,back_y),c(a,b))
      output[1]<-0
      output[2:3]<-point
    }else
    {
      if(judge_right==0&judge_back==0)
      {direction<-sample(c(1,2,3,4),1,replace=TRUE,prob=c(0,0,0,1))}else if(judge_right==1&judge_back==0)
      {direction<-sample(c(1,2,3,4),1,replace=TRUE,prob=c(0.5,0,0,0.5))}else if(judge_right==0&judge_back==1)
      {direction<-sample(c(1,2,3,4),1,replace=TRUE,prob=c(0,0,0.25,0.75))}else
      {direction<-sample(c(1,2,3,4),1,replace=TRUE,prob=c(0.5,0,0.25,0.25))}
      point<-switch(direction,c(right_x,right_y),c(left_x,left_y),c(back_x,back_y),c(a,b))
      output[1]<-0
      output[2:3]<-point
    }
  }
  ##If the customer choose back,
  ##the number of steps to be back will be randomly taken out according to the probability of 1/2^n
  if(output[1]==0&direction==3)
  {
    c<-1
    u<-runif(1)
    y<-rgeom(1,0.5)
    while (u>(1/2^(y+1))/(c*dunif(y,0,1)))
    {
      u<-runif(1)
      y<-rgeom(1,0.5)
    }
    output[4]<-y
  }else
  {output[4]<-0}
  
  
  return(output)
}



########################################################################




####### The complete code for simulation of the movement of customers within 3600 seconds

success<-matrix(data=rep(0,300),nrow=3,ncol=10000)
average_shopping_time<-matrix(data=NA,nrow=100,ncol=10000)
average_customer_number<-matrix(data=NA,nrow=100,ncol=10000)

average_total_spending_rate<-matrix(data = rep(0,10000000),nrow = 20000,ncol = 500)
customer_number_in_store_save<-matrix(data = NA,nrow = 20000,ncol = 500)

######################################################## layout of the store
corner_x<-c(5,5,8,8,13,13,17,17,22,22,25,25)
corner_y<-c(10,20,20,10,10,20,20,10,10,20,20,10)
corner<-cbind(corner_x,corner_y)

entrance<-c(30,25)
exit<-c(30,5)

########################################################
c(28,30,31,32,33,34,35,37,40,42,45,47,50,55,60,65)
c(30,35,40,42,44,46,50,55,60,65)
seq(from = 60, to = 80, by = 2)
entrance_interval_sequence<-seq(from = 60, to = 80, by = 2)

entrance_interval<-3000

for(entrance_interval in entrance_interval_sequence)
{
  while(success[1,entrance_interval]<15) ## 成功运行的次数，相当于monte-carlo，最后以平均值作为最后的估计值
  {
    customer.numeber<-1
    
    freezing<-"no freezing"
    
    ################    Create the initial matrix
    type_1_matrix<-matrix(data=0,ncol=1,nrow=1)
    type_2_matrix<-matrix(data=NA,ncol=1,nrow=100000)
    type_3_matrix<-matrix(data=NA,ncol=2,nrow=100000)
    
    ################    Record the number of customers in the store every second
    customer_number_in_store<-type_2_matrix
    ################
    
    ################     Create the initial shopping list of the first customer
    customer.all<-shopping_list("gamma",150,10,15)
    ###Record the customer's next shopping point
    #######
    #next_shopping_point<-matrix(data=nearest_point(customer.all,entrance[1],entrance[2]),nrow = 2,ncol=1)
    #######
    
    next_shopping_point<-matrix(data=shopping_list("count",15,2,1)[1,],nrow = 2,ncol=1)
    ################
    
    ################     coordinates of the corners of vegetable zone 
    #corner_x<-c(10,10,20,20,30,30,40,40)
    #corner_y<-c(10,20,20,10,10,20,20,10)
    #corner<-cbind(corner_x,corner_y)
    ################ 
    
    ########################     Initial value setting
    shopping_time<-type_1_matrix
    change<-type_1_matrix
    change_time<-type_2_matrix
    reserve_all<-type_3_matrix
    reserve_number<-type_1_matrix
    reserve_time<-type_2_matrix
    
    time_all<-type_3_matrix
    time_all[1,1:2]<-entrance
    
    
    B<-type_1_matrix
    L.B<-type_2_matrix
    
    type_4_matrix<-matrix(data=c(10000,10000),nrow=1,ncol=2)
    risktime<-matrix(data=c(1.5,2),byrow = T,ncol=2,nrow=1)
    
    type_5_matrix<-matrix(data=16-entrance_interval ,ncol=1,nrow=1)
    ## since the time start from 1, for the latter customer,they can not move
    ## so we need to set their "fix" smaller than 15, then they will not move
    ## keep as the initial point
    ## since after each time fix<fix+1, so after enough time, this customer could enter
    ## and at that time, their "fix" has been added to larger than 15
    ## this means the cycle for this customer could start
    
    fix<-matrix(data=16,ncol=1,nrow=1)
    ## "fix" for 1st customer has already been 16 which is larger than 15
    ## so the cycle for 1st customer will start immediately.
    ######################## 
    
    
    
    ######################## cycle begins, which means customers start to enter the store for shopping 
    for(i in 1:14399)
    {
      
      if(i==1)
      {
        customer_number_in_store[1]<-1
      }else
      {
        customer_number_in_store[i]<-customer_number_in_store[i-1]
      }
      
      ########################  Matrix changes after new customers entering
      
      if (floor((i-1)/entrance_interval)>=0&((i-1)/entrance_interval)==floor((i-1)/entrance_interval))
      {
        ## Record the total number of customers after adding new customers
        customer.numeber<-customer.numeber+1
        
        customer_number_in_store[i]<-customer_number_in_store[i]+1  
        
        ###################  Judge if it is freezing at the door
        if(customer.numeber>=3)
        {
          if(time_all[i,(2*(customer.numeber-2)-1)]==entrance[1]&time_all[i,2*(customer.numeber-2)]==entrance[2]) ## 上一个顾客已经被堵在门口不能动了
          {
            freezing<-"stuck"
            print("stuck")
            break
          }
        }
        
        ###################  Record of some data indicators
        shopping_time<-cbind(shopping_time,type_1_matrix)
        change<-cbind(change,type_1_matrix)
        change_time<-cbind(change_time,type_2_matrix)
        reserve_all<-cbind(reserve_all,type_3_matrix)
        reserve_number<-cbind(reserve_number,type_1_matrix)
        reserve_time<-cbind(reserve_time,type_2_matrix)
        #########################
        
        ###################  Main route change data 
        time_all<-cbind(time_all,type_3_matrix)
        time_all[1:((customer.numeber-1)*entrance_interval+1),(2*customer.numeber-1)]<-entrance[1]
        time_all[1:((customer.numeber-1)*entrance_interval+1),(2*customer.numeber)]<-entrance[2]
        
        B<-cbind(B,type_1_matrix)
        L.B<-cbind(L.B,type_2_matrix)
        ###################
        
        ###################  Indicators for judging whether to enter the loop
        risktime<-rbind(risktime,type_4_matrix)
        
        fix<-cbind(fix,type_5_matrix)
        
        ###################  Changes in customer shopping lists 
        x<-shopping_list("gamma",150,10,15)
        customer.all<-cbind(customer.all,x) ## 将新进入的顾客的信息合并到之前的矩阵里面
        
        #####
        #xx<-matrix(data=nearest_point(x,entrance[1],entrance[2]),nrow = 2,ncol=1)
        #####
        xx<-matrix(data=shopping_list("count",15,2,1)[1,],nrow=2,ncol=1)
        
        next_shopping_point<-cbind(next_shopping_point,xx)
        ##################   risktime also changes
        if(i>1)
        {
          risktime<-my_min_distance_change(i-1,i,time_all)
        }
        
      }
      
      ########################
      
      
      for(j in 1:customer.numeber)
      {
        if(fix[j]>15) ## customer will stay at each shopping point for 15 seconds, so we don't need to consider collision within these 15 seconds
        {
          ##customers who may collide with other people or need to be back again
          if(risktime[j,1]<=i|B[j]>0)  
          {
            ##For the convenience, change the coordinates of the customers who did not enter the store 
            if(risktime[j,1]<i&j<customer.numeber)
            {
              for(t in (j+1):customer.numeber)
              {
                time_all[i,(2*t-1):(2*t)]<-10000*t
              }
            }
            ###########################################
            ###########################################
            
            ## Determine the position of other customers relative to this customer
            dd<-numeric(customer.numeber)
            other_customer<-matrix(data=NA,nrow=2,ncol=customer.numeber-1)
            for(k in 1:customer.numeber)
            {
              if(k>j)
              {
                dd[k]<-sqrt((time_all[i,2*k-1]-time_all[i,2*j-1])^2+(time_all[i,2*k]-time_all[i,2*j])^2)
                other_customer[1:2,(k-1)]<-time_all[i,(2*k-1):(2*k)]
              }else if(k<j)
              {
                dd[k]<-sqrt((time_all[i+1,2*k-1]-time_all[i,2*j-1])^2+(time_all[i+1,2*k]-time_all[i,2*j])^2)
                other_customer[1:2,k]<-time_all[i+1,(2*k-1):(2*k)]
              }else
              {
                dd[k]<-1000000000
              }
              nearest_customer<-which.min(dd)
            }
            ###########################################
            ###########################################
            
            
            ## Determine whether the customer could continue to go back
            if(B[j]>0)
            {
              back_x<-2*time_all[i,2*j-1]-time_all[i-1,2*j-1]
              back_y<-2*time_all[i,2*j]-time_all[i-1,2*j]
              dis_back<-sqrt((other_customer[1,]-back_x)^2+(other_customer[2,]-back_y)^2)
              if(setequal(which(dis_back<2),integer(0))&in_the_zone(back_x,back_y)==1)
              {back<-1}else
              {back<-0}
            }else
            {back<-0}
            
            ##Back is 1 means that the customer can continue to go back, 
            ##and 0 means that the customer cannot continue to go back
            
            ###########################################
            ###########################################
            
            ## Judge the direction of the customer
            if(back==1)
            {
              time_all[i+1,2*j-1]<-back_x
              time_all[i+1,2*j]<-back_y
              change[j]<-change[j]+1
              change_time[change[j],j]<-i
              B[j]<-B[j]-1
              L.B[change[j],j]<-i
            }else
            {
              if(j<nearest_customer)
              {
                judge_change<-judgment_direction(time_all[i,2*j-1],time_all[i,2*j],next_shopping_point[1,j],next_shopping_point[2,j],time_all[i,2*nearest_customer-1],time_all[i,2*nearest_customer],next_shopping_point[1,nearest_customer],next_shopping_point[2,nearest_customer],other_customer)
              }else
              {
                judge_change<-judgment_direction(time_all[i,2*j-1],time_all[i,2*j],next_shopping_point[1,j],next_shopping_point[2,j],time_all[i+1,2*nearest_customer-1],time_all[i+1,2*nearest_customer],next_shopping_point[1,nearest_customer],next_shopping_point[2,nearest_customer],other_customer)
              }
              
              time_all[(i+1),(2*j-1):(2*j)]<-judge_change[2:3]
              if(judge_change[1]==0)
              {
                change[j]<-change[j]+1
                change_time[change[j],j]<-i
              }
              B[j]<-judge_change[4]+2   ##prevent premature blockage 
            }
            ###########################################
            ###########################################
            
            ## Change back the coordinates that were changed for calculation convenience
            
            if(risktime[j,1]<i&j<customer.numeber)
            {
              for(t in (j+1):customer.numeber)
              {
                time_all[i,(2*t-1)]<-entrance[1]
                time_all[i,(2*t)]<-entrance[2]
              }
            }
            
            risktime<-my_min_distance_change(i,j,time_all)
            
            ###########################################
            
          }else   
            ##If it is found that this customer will not collide with other customers 
            ##and does not need to continue backing, then walk normally
          {
            if(judgment(time_all[i,2*j-1],time_all[i,2*j],next_shopping_point[1,j],next_shopping_point[2,j])==1)
            {
              really_nc1<-judgment_corner(time_all[i,2*j-1],time_all[i,2*j],next_shopping_point[1,j],next_shopping_point[2,j])
              aa<-really_nc1[2]
              bb<-really_nc1[3]
            }else
            {
              aa<-next_shopping_point[1,j]
              bb<-next_shopping_point[2,j]
            }
            
            time_all[(i+1),(2*j-1):(2*j)]<-movement(time_all[i,2*j-1],time_all[i,2*j],aa,bb)
          }
          
          ########################################### 
          ###########################################
          
          ###########################################  Delete the shopping point that has arrived
          delete<-abs(next_shopping_point[1,j]-time_all[(i+1),(2*j-1)])+abs(next_shopping_point[2,j]-time_all[(i+1),(2*j)])
          #delete等于0，表示已经抵达这个购物点（或者是这个目的地）
          if(delete==0)
          {
            ## since the 1st shopping point is random and we didn't collect it into customer.all
            ## so we need to find whether now this customer has arrived 1st point
            delete_shopping_point<-abs(customer.all[,(2*j-1)]-next_shopping_point[1,j])+abs(customer.all[,(2*j)]-next_shopping_point[2,j])
            ## if delete_shopping_point == 0 is integer(0), means this customer arrives 1st point or the exit
            ## we don't need to do anything to our customer.all
            ## otherwise, we need to delete this point.
            if(!setequal(which(delete_shopping_point==0),integer(0)))
            {
              customer.all[which(delete_shopping_point==0),(2*j-1)]<-10000
              customer.all[which(delete_shopping_point==0),(2*j)]<-10000
            }
            ## After the delete, we need to check whether there is any more shopping points left
            
            
            if(setequal(which(customer.all[,(2*j-1)]<10000),integer(0))&(abs(time_all[(i+1),(2*j-1)]-exit[1])+abs(time_all[(i+1),(2*j)]-exit[2]))!=0)
            {
              ## If the shopping list has been emptied and the customer is not at the exit at this time, 
              ## then add the exit to the shopping list and let the customer continue to walk out of the store
              next_shopping_point[1,j]<-exit[1]
              next_shopping_point[2,j]<-exit[2]
              
              fix[j]<-1 ## the customer still need to reach the exit, so "fix" back to 1 is enough
              time_all[(i+2):(i+15),(2*j-1)]<-time_all[(i+1),(2*j-1)]
              time_all[(i+2):(i+15),(2*j)]<-time_all[(i+1),(2*j)]
              
              reserve_number[j]<-reserve_number[j]+1
              reserve_all[reserve_number[j],(2*j-1):(2*j)]<-time_all[(i+1),(2*j-1):(2*j)]
              reserve_time[reserve_number[j]+1,j]<-i+15
              
            }else if((setequal(which(customer.all[,(2*j-1)]<10000),integer(0))&(abs(time_all[(i+1),(2*j-1)]-exit[1])+abs(time_all[(i+1),(2*j)]-exit[2]))==0))
            {
              ## If the shopping list has been emptied and the exit has been reached,
              ## the customer cycle ends
              fix[j]<--500000 ## the customer has left the store and will never enter the cycle, so set "fix" to be small enough
              time_all[(i+2):100000,(2*j-1)]<-10000*j
              time_all[(i+2):100000,(2*j)]<-10000*j
              
              ## When the customer arrives at the exit, clear the coordinates of the entrance
              next_shopping_point[1:2,j]<-10000
              
              ## The number of customers in the store should be -1
              customer_number_in_store[i]<-customer_number_in_store[i]-1
              
            }else
            {
              ## If the shopping list has not been emptied yet, delete the arrived ones first, 
              ## and then assign a new point to next_shopping_point
              
              ## If it is a point in the shopping list, 
              ## delete the point in the shopping list at the same time
              
              
              next_shopping_point[1:2,j]<-nearest_point(customer.all[,(2*j-1):(2*j)],time_all[i+1,2*j-1],time_all[i+1,2*j])
              
              
              fix[j]<-1
              time_all[(i+2):(i+15),(2*j-1)]<-time_all[(i+1),(2*j-1)]
              time_all[(i+2):(i+15),(2*j)]<-time_all[(i+1),(2*j)] 
              
              reserve_number[j]<-reserve_number[j]+1
              reserve_all[reserve_number[j],(2*j-1):(2*j)]<-time_all[(i+1),(2*j-1):(2*j)]
              reserve_time[reserve_number[j]+1,j]<-i+15
            }
          }
          
          
          ########################################### 
          
          ## If customer stay in one place for 30 seconds and still haven’t left, 
          ## change to another shopping point
          if(i>(29+(j-1)*entrance_interval)&time_all[i+1,(2*j-1)]<10000)
          {
            if(((time_all[i+1,(2*j-1)]-time_all[i-29,(2*j-1)])+(time_all[i+1,(2*j)]-time_all[i-29,(2*j)]))==0)
            {
              choose_random_point<-function(data)
              {
                if((setequal(which(customer.all[,(2*j-1)]<10000),integer(0))))
                {
                  return("no more shopping point")
                }else
                {
                  return(sample(which(customer.all[,(2*j-1)]<10000),1)) #随机抽取一个购物点出来
                }
              }
              
              random_number<-choose_random_point(customer.all[,(2*j-1):(2*j)])
              if(random_number!="no more shopping point")
              {
                #random_shopping_point<-customer.all[random_number,(2*j-1):(2*j)] #存储新选中的下个购物点
                #customer.all[random_number,(2*j-1):(2*j)]<-next_shopping_point[1:2,j] #将原本决定去的购物点放回来，因为还没去买呢
                #next_shopping_point[1:2,j]<-random_shopping_point #现在将下一个购物点设置为接下来移动的目的地
                next_shopping_point[1:2,j]<-customer.all[random_number,(2*j-1):(2*j)]
              }
            }
          }
          
          ##########################################
          
          
        }else  
          ## Here is the situation of a certain customer staying and shopping at the shopping point,
          ## but still need to update the risktime data
        {
          if(risktime[j,1]==i)
          {
            risktime<-my_min_distance_change(i,j,time_all)
          }
        }
        ###########################################
        ###########################################
        
        ## Judge whether there will be congestion elsewhere in the store
        freezing<-"no freezing"
        if(i>(199+(j-1)*entrance_interval)&time_all[i+1,(2*j-1)]<10000)
        {
          if(((time_all[i+1,(2*j-1)]-time_all[i-199,(2*j-1)])+(time_all[i+1,(2*j)]-time_all[i-199,(2*j)]))==0)
          {
            #since customers maybe arrive at the vegetable zones corner a lot of times
            #so maybe this situation occurs by chance, eg.customer arrive (10,10) now and 200 seconds before
            #but it is not a freezing case
            if(sum(abs(time_all[(i-199):i,(2*j-1)]-time_all[i+1,(2*j-1)])+abs(time_all[(i-199):i,(2*j)]-time_all[i+1,(2*j)]))==0)
            {
              freezing<-"freezing"
              break
            }
            
          }
        }
        ###########################################
        ###########################################
        
      }
      
      ###########################################
      if(freezing=="freezing")
      {
        print(freezing)
        break
      }
      
      fix<-fix+1
    }
    
    
    ######################   The end of customer shopping cycle 
    
    
    ######################################################################################
    ######################################################################################
    
    ######################################################################################
    if(freezing=="stuck")
    {                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
      success[2,entrance_interval]<-success[2,entrance_interval]+1
      print(c(freezing,success[2,entrance_interval],i))
    }else if(freezing=="freezing")
    {
      success[3,entrance_interval]<-success[3,entrance_interval]+1
      print(c(freezing,success[3,entrance_interval],i))
    }else
    {
      success[1,entrance_interval]<-success[1,entrance_interval]+1
      print(c(freezing,success[1,entrance_interval],i))
      
      ########################################
      ########################################
      
      customer.numeber<-customer.numeber-1
      complete<-numeric(customer.numeber)
      
      enter_time<-numeric(customer.numeber)
      for(i in 1:customer.numeber)              
      {
        time_all[1:((i-1)*entrance_interval+1),(2*i-1):(2*i)]<-10000*i
        time_all[((i-1)*entrance_interval+1),(2*i-1):(2*i)]<-entrance
        enter_time[i]<-(i-1)*entrance_interval+1
        shopping_time[i]<-length(which(time_all[,(2*i)]<10000*i))
        
        ## Mark the customers who have completed the entire shopping process
        ## 1 means completed, 0 means not completed
        
        complete_number<-which(time_all[dim(time_all)[1],]>=10000)
        if(setequal(which(complete_number==2*i),integer(0)))
        {
          complete[i]<-0
        }else
        {
          complete[i]<-1
        }
        
        ##########################
      }
      reserve_time[1,1:customer.numeber]<-enter_time
      total_spending_rate<-matrix(data=NA,ncol=customer.numeber,nrow=20000)
      for(s in 1:customer.numeber)
      {
        if(reserve_number[s]>0)
        {        
          total_spending_rate[(1:(enter_time[s])),s]<-0
          for(t in 1:(reserve_number[s]))
          {
            total_spending_rate[(reserve_time[t,s]+1):(reserve_time[t+1,s]),s]<-1/(reserve_time[t+1,s]-reserve_time[t,s])
          }
          total_spending_rate[(reserve_time[(reserve_number[s]+1),s]+1):20000,s]<-0
        }else
        {
          total_spending_rate[(1:20000),s]<-0
        }
      }
      
      average_total_spending_rate[1:20000,entrance_interval]<-average_total_spending_rate[1:20000,entrance_interval]+apply(total_spending_rate,MARGIN = 1,FUN = sum)
      
      average_customer_number[success[1,entrance_interval],entrance_interval]<-sum(complete)
      
      customer_number_in_store_save[1:14399,entrance_interval]<-customer_number_in_store[1:14399]
      
      if(sum(complete)>0)
      {
        average_shopping_time[success[1,entrance_interval],entrance_interval]<-sum(shopping_time[which(complete==1)])/sum(complete)
        
      }else   
        ## Even if it has not been frozen, no one has completed the shopping,
        ## so the number of people is 0. 
        ## And shopping time is set to 0 means there is no data yet
      {
        average_shopping_time[success[1,entrance_interval],entrance_interval]<-0
      }
    }
  }
  ## record the total spending rate at every second for customers in the store
  average_total_spending_rate[1:14400,entrance_interval]<-average_total_spending_rate[1:14400,entrance_interval]/100
  ## record the number of customers in the store at every second
  customer_number_in_store_save[1:14399,entrance_interval]<-customer_number_in_store[1:14399]
}
########################################################################










#########################################################################

###### Draw the plot for #of the customers who stay in store at each second within 3600 seconds
plot(customer_number_in_store_save[1:14399,30],xlab = "time",ylab = "number"
     ,main = "#of customers in the store wrt. different entrence frequency"
     ,col=30,ylim = c(0,40),cex=0.5)
for(i in seq(from = 60, to = 80, by = 2))
{
  lines(x=c(1:14399),y=customer_number_in_store_save[1:14399,i],col=i,cex=0.5)
}

legend(0,100
       ,c("30 seconds","40 seconds","44 seconds","45 seconds","50 seconds","60 seconds")
       ,bty = "n",text.col=c(30,40,44,45,50,60),col=c(30,40,44,45,50,60)
       ,cex=0.7,pch = 19)

store1<-customer_number_in_store_save
b<-apply(store1[2000:12000,c( 60, 62, 64, 66, 68, 70, 72, 74, 76, 78, 80)]
         ,MARGIN = 2,FUN = mean)

store3<-customer_number_in_store_save
a<-apply(store3[2000:12000,c( 60, 62, 64, 66, 68, 70, 72, 74, 76, 78, 80)]
         ,MARGIN = 2,FUN = mean)
#########################################################################



####### Draw the plots for visual analysis

###### Draw the plots for the proportion of three situation during the simulations
y<-success[1,]/(apply(success,MARGIN = 2,FUN = sum))
y2<-success[2,]/(apply(success,MARGIN = 2,FUN = sum))
y3<-success[3,]/(apply(success,MARGIN = 2,FUN = sum))


#############################比较两个distribution的图
plot(y[entrance_interval_sequence],y3[entrance_interval_sequence]
     ,xlab = "success proportion",ylab = "freezing proportion")
#############################



plot(x=entrance_interval_sequence
     ,y=c(y[entrance_interval_sequence]),type = "b",
     xlim=c(25,70),ylim=c(0,1)
     ,main="proportion of three situation during the simulations",xlab="time interval",ylab = "")

points(x=entrance_interval_sequence
       ,y=c(y2[entrance_interval_sequence]),type = "b",col="blue")
points(x=entrance_interval_sequence
       ,y=c(y3[entrance_interval_sequence]),type = "b",col="red")

legend(53,0.6,c("no freezing","freezing_entrance","freezing_other part")
       ,text.col=c("black","blue","red"),bty = "n",cex = 1,pch = 19
       ,col=c("black","blue","red"))

###### Draw the plot of the #of customers who has completed shopping
y_number<-apply(average_customer_number[1:success[1,entrance_interval],],MARGIN = 2,FUN = mean)
plot(x=entrance_interval_sequence
     ,y=c(y_number[entrance_interval_sequence]), type = "b"
     ,xlim=c(120,200),ylim=c(30,150),ylab = "",xlab = "time interval",main = "Average #of customers who completed shopping")

###### Draw the plot of the #of customers who has entered the store within 3600 seconds
plot(x=entrance_interval_sequence
     ,y=c(floor(14398/entrance_interval_sequence+1)), type = "b",ylab = "",xlab = "time interval"
     ,main = "number of total customers",ylim=c(50,240),xlim=c(25,65))

####### Draw the plot of the average shopping time for customers who has completed the shopping
y_time<-apply(average_shopping_time[1:success[1,entrance_interval],],MARGIN = 2,FUN = mean)
plot(x=entrance_interval_sequence
     ,y=c(y_time[entrance_interval_sequence]), type = "b"
     ,ylab = "second(s)",xlab = "time interval",main = "Average shopping time of each customer"
     ,ylim = c(900,1600),xlim = c(25,65))
legend(29.5,1610,c("2A1=1517.56 seconds","(30,1516.907)"),text.col = c("red","black"),bty = "n",cex = 1,pch = 19,col=c("red","black"))

###### Draw the plot for #of the customers who stay in store at each second within 3600 seconds
plot(customer_number_in_store_save[1:14399,480],xlab = "time",ylab = "number",type = "l"
     ,main = "#of customers in the store wrt. different entrence frequency",col=28,ylim = c(0,5))
for(i in entrance_interval_sequence[-1])
{
  points(x=c(1:14399),y=customer_number_in_store_save[1:14399,i],col=i,type = "l")
}
legend(0,100,c("the top blue line is 28 seconds","the bottom black line is 65 seconds"),bty = "n",text.col=c(28,65),col=c(28,65),pch = 19)

##### Draw the plot for the total spending rate at every second
plot(x=1:14400,y=average_total_spending_rate[1:14400,entrance_interval_sequence[1]]
     ,col=entrance_interval_sequence[1],ylim=c(0,0.04),type = "l",
     xlab = "time (seoncds)",ylab = NA, main = "shopping rate for total customer in store")
for(i in entrance_interval_sequence[-1])
{
  lines(x=1:3600,y=average_total_spending_rate[1:3600,i],col=i)
}

plot(x=1:3600,y=average_total_spending_rate[1:3600,entrance_interval_sequence[1]],col=entrance_interval_sequence[1],ylim=c(0,0.7),type = "l",
     xlab = "time (seoncds)",ylab = NA, main = "shopping rate for total customer in store")
for(i in entrance_interval_sequence[-1])
{
  lines(x=1:3600,y=average_total_spending_rate[1:3600,i],col=i)
}

######### need to be specified 
legend(0,0.70,c("28 seconds","45 seconds","65seonds"),col = c(28,45,65)
       ,text.col = c(28,45,65),bty = "n",cex = 1,pch = 19)






















##############################
## Draw the graph of the 4th store
##############################
corner_x<-c(5,5,8,8,13,13,17,17,22,22,25,25)
corner_y<-c(10,20,20,10,10,20,20,10,10,20,20,10)
corner<-cbind(corner_x,corner_y)

entrance<-c(30,25)
exit<-c(30,5)

plot(x=NA,y=NA,xlim = c(0,30),ylim = c(0,30),xlab = NA,ylab = NA,pch=19,cex=2,asp = 1, bty = "n", xaxt = "n", yaxt ="n")
text(x=c(28,30),y=c(25,5),c("entrance","exit"),cex=0.6)
text(x=c(6.5,15,23.5),y=c(15,15,15),c("zone1","zone2","zone3"),cex=0.6,col = "darkgreen")
text(x=c(5,5,8,8,13,13,17,17,22,22,25,25),y=c(9,21,21,9,9,21,21,9,9,21,21,9)
     ,c("1","2","3","4","5","6","7","8","9","10","11","12"),cex = 0.6,col = "blue")
polygon(x=c(0,0,30,30),y=c(0,30,30,0))
polygon(x=corner_x[1:4],y=corner_y[1:4])
polygon(x=corner_x[5:8],y=corner_y[5:8])
polygon(x=corner_x[9:12],y=corner_y[9:12])

aa<-shopping_list("count",15,1,30)
which(aa[,1]>=0)
points(aa[1:length(which(aa[,1]>=0)),1],aa[1:length(which(aa[,1]>=0)),2],col="red")

####################################
# Test "Judgement" function
####################################

judgment(3,15,16,18)
lines(c(3,16),c(15,18),col="blue")
judgment(3,15,7,0)
lines(c(3,7),c(15,0),col="orange")
judgment(3,15,22,22)
lines(c(3,22),c(15,22),col="purple")
judgment(7,5,26,22)
lines(c(7,26),c(5,22),col="grey")
judgment(12,12,16,16)
lines(c(12,16),c(12,16),col="yellow")
o


























##############################################################################
##############################################################################
a1<-success[1,]/(apply(success,MARGIN = 2,FUN = sum))
a2<-success[2,]/(apply(success,MARGIN = 2,FUN = sum))
a3<-success[3,]/(apply(success,MARGIN = 2,FUN = sum))
plot(a1[entrance_interval_sequence],a3[entrance_interval_sequence]
     ,xlab = "success proportion",ylab = "freezing proportion",xlim=c(0,1),ylim=c(0,1)
     ,pch=19,col="blue")
points(y[entrance_interval_sequence],y3[entrance_interval_sequence],col="red",pch=19)
legend(0,1,c("gamma(15,1)","gamma(150,10)"),col = c("blue","red")
       ,text.col = c("blue","red"),bty = "n",cex = 1,pch = 19)


a4<-average_shopping_time
mean(a4[1:10,entrance_interval_sequence])
mean(average_shopping_time[1:10,entrance_interval_sequence])

a5<-customer_number_in_store_save
plot(a5[1:14399,60],xlab = "time",ylab = "number"
     ,main = "#of customers in the store ",col=28,ylim = c(0,40),type = "l")
lines(customer_number_in_store_save[1:14399,60],col="red")
legend(0,40,c("gamma(15,1)","gamma(150,10)"),col = c("blue","red")
       ,text.col = c("blue","red"),bty = "n",cex = 1,pch = 19)


a6<-average_total_spending_rate
plot(x=1:14400,y=a6[1:14400,entrance_interval_sequence[1]]
     ,col=entrance_interval_sequence[1],ylim=c(0,0.04),type = "l",
     xlab = "time (seoncds)",ylab = NA, main = "shopping rate for total customer in store")
lines(x=1:14400,y=average_total_spending_rate[1:14400,entrance_interval_sequence[1]],col="red")
legend(0,0.04,c("gamma(15,1)","gamma(150,10)"),col = c("blue","red")
       ,text.col = c("blue","red"),bty = "n",cex = 1,pch = 19)

curve(dgamma(x,15,1),xlim=c(0,35),ylim=c(0,0.35),col="blue",main="PDF",ylab="f(x)")
curve(dgamma(x,150,10),col="red",add = TRUE)
legend(0,0.35,c("gamma(15,1)","gamma(150,10)"),col = c("blue","red")
       ,text.col = c("blue","red"),bty = "n",cex = 1,pch = 19)


curve(dgamma(x,15,1),xlim=c(0,35),ylim=c(0,0.35),col="blue",main="PDF",ylab="f(x)")
curve(dgamma(x,150,10),col="red",add = TRUE)
legend(0,0.35,c("gamma(15,1)","gamma(150,10)"),col = c("blue","red")
       ,text.col = c("blue","red"),bty = "n",cex = 1,pch = 19)
##############################################################################
##############################################################################
