#Originally created while working for Feinstein International Center
#A.C. Keyel (skeyel@gmail.com)

#' myr (package)
#' 
#' Package containing assorted useful functions
#'
#' @details \tabular{ll}{ %% Note tabular{ll} is tabular{lowercase(LL)} not tabular{11} 
#' Package: \tab myr\cr
#' Type: \tab Package\cr
#' Version: \tab 0.1.6.9999\cr
#' Date: \tab 2015-09-16\cr
#' License: \tab GPL-2 (or later)\cr
#' }
#' @author Alexander "Sasha" Keyel
#' Maintainer: Sasha Keyel <skeyel@@gmail.com>
#'
#' @keywords package
#' @name myr_package
#' @docType package
NULL

#' Preserve matrix formatting when reformatting a matrix
#' 
#' When a single column is extracted from a multicolumn matrix, R often strips the matrix
#' formatting. This generally causes my code to crash, because I assume that the formatting
#' is preserved. This function aims to preserve that formatting.
#' Testing note: Tested with an input matrix with 5 columns & 4 rows, and subset to obtain 0, 1 and 2 columns.
#' 
#' @param in.mat The input matrix
#' @param index.var The variable to be used for subsetting
#' @param target.value The value of the subsetting variable to subset on.
#' 
#' @export matrix.subset
matrix.subset = function(in.mat, index.var, target.value){
  
  if (ncol(in.mat) == 0){
    stop("No columns in the input matrix. If code needs to work with this condition, please modify myr::matrix.subset")
  }
  
  # Subset columns
  # Check that input is already in matrix format (i.e. has columns)
  if (ncol(in.mat) >= 1){

    #Extract row & column names prior to extraction
    c.name = colnames(in.mat)
    r.names = rownames(in.mat)
    
    # Subset the matrix
    out.mat = in.mat[ , index.var == target.value] 
    
    # Check if column names have been removed (i.e., when only a single column is selected)
    if (length(ncol(out.mat)) == 0){
      #subset the column names
      c.name = c.name[index.var == target.value]
      out.mat = matrix(c(out.mat), ncol = 1, dimnames = list(r.names, c.name))  
    }
  }
  
  return(out.mat)
}

#' Moves files for building a package
#'
#' There are a number of files in my base R directory that I don't want in the
#' R package build. .Rbuildignore does not seem to work for me.
#' So, I wrote this function and also return.files to expedite things
#'
#' @param my.pkg The package of interest. Currently this only does something for
#' the spatialdemography package!
#' @export move.files
move.files = function(my.pkg){

  if (my.pkg == "spatialdemography"){
    #Set up temporary directory
    base.dir = sprintf("C:/docs/beplants/Scripts/%s/", my.pkg)
    temp.dir = sprintf("C:/docs/beplants/Scripts/temp-%s/", my.pkg)
    mr.temp.dir = sprintf("%sman-roxygen/", temp.dir)
    dir.create(temp.dir, showWarnings = FALSE)
    dir.create(mr.temp.dir, showWarnings = FALSE)
    
    #Move random files in base directory
    exclude.vec = c("spatialdemography.Rproj",".gitignore")
    for (item in exclude.vec){
      from = sprintf("%s%s",base.dir, item)
      to = temp.dir
      file.copy(from, to)
      
      if(file.exists(sprintf("%s%s",to, item))){
        file.remove(from)
      }
    }
  
    #Move man-roxygen directory
    mr.dir = sprintf("%sman-roxygen", base.dir)
    man.roxygen.files = list.files(mr.dir)
    for (mr.file in man.roxygen.files){
      from = sprintf("%s/%s", mr.dir, mr.file)
      file.copy(from, mr.temp.dir)
      
      if(file.exists(sprintf("%s%s", mr.temp.dir, mr.file))){
        file.remove(from)
      }
    }
    #If all files have been moved, delete the directory
    if (length(list.files(mr.dir)== 0)){
      unlink(mr.dir, recursive = TRUE)
    }
  }
}

#' Return files to package directory
#'
#' This returns the files that were removed with the move.files function.
#' This could probably be turned into a single function, where the user
#' just specifies the direction of transfer!
#' @param my.pkg The package of interest, currently this is only scripted for
#' spatialdemography!
#' @export return.files
return.files = function(my.pkg){
   if (my.pkg == "spatialdemography"){
    #Set up temporary directory
    base.dir = sprintf("C:/docs/beplants/Scripts/%s/", my.pkg)
    temp.dir = sprintf("C:/docs/beplants/Scripts/temp-%s/", my.pkg)
    mr.dir = sprintf("%sman-roxygen", base.dir)
    mr.temp.dir = sprintf("%sman-roxygen/", temp.dir)
    dir.create(mr.dir, showWarnings = FALSE)
    
    #Move random files in base directory
    exclude.vec = c("spatialdemography.Rproj",".gitignore")
    for (item in exclude.vec){
      from = sprintf("%s%s",temp.dir, item)
      to = base.dir
      file.copy(from, to)
      
      if(file.exists(sprintf("%s%s",to, item))){
        file.remove(from)
      }
    }
  
    #Move man-roxygen directory
    man.roxygen.files = list.files(mr.temp.dir)
    for (mr.file in man.roxygen.files){
      from = sprintf("%s/%s", mr.temp.dir, mr.file)
      to = mr.dir
      file.copy(from, to)
      
      if(file.exists(sprintf("%s/%s", to, mr.file))){
        file.remove(from)
      }
    }
    #If all files have been moved, delete the directory
    if (length(list.files(mr.temp.dir)== 0)){
      unlink(mr.temp.dir, recursive = TRUE)
    }
    
    if (length(list.files(temp.dir) == 0)){
      unlink(temp.dir, recursive = TRUE)
    }   
  }
}

#' Useful tips
#'
#' Assorted things I thought useful and worth remembering. Not really a function.
#'
UsefulTips = function(){
    print ("Howdy-Do!")
    # Note: If your regression is slow, maybe it's because R has read in the numbers as characters instead of as numbers.  Then it treats them as factors, and is neither fast, nor what you want.
    
    #To just keep variables in a dataframe called dd: #avar is a variable selected from from names(dd), and is just an example of using a variable to do the selection
    keeps <- c(avar,"YEAR")
    newdata = dd[keeps]
    
    #To import no data values from SAS into R
    D <- read.csv("temp.csv",na.strings=".")  #na.strings = "." is the important bit. #FROM https://stat.ethz.ch/pipermail/r-help/2010-January/224262.html
    
    }

#' as.char
#' 
#' Shortcut to avoid having to write out as.character()
#' @param A character input
#' @export as.char
as.char = function(x){
  y = as.character(x)
  return(y)
}

#' as.num
#'
#' Shortcut to avoid having to always write out as.numeric(as.character())
#'
#' @param x An input to be converted to numeric format
#' @export as.num
as.num = function(x){

    #check if length of x is > 1
    if (length(x) == 1){
        #if so, and x is NA, mark NA
        if (is.na(x)){
            out = NA
        #otherwise
        }else{
            #if x is "NA", mark as NA
            if (x == "NA"){
                out = NA
            #otherwise proceed as normal
            }else{
                out = as.numeric(as.character(x))
                }
            }
    #if length x > 1, proceed as normal
    }else{
        out = as.numeric(as.character(x))
        }

    return(out)
    }

#' Check type
#'     
#' For checking type of variables in a dataframe or matrix
#'
#' @param indata A dataset to be checked for types
#' @export chktyp
chktyp = function(indata){
    hedlen = length(names(indata))
    for (i in 1:hedlen){
        print(names(indata[i]))
        print(typeof(indata[[i]]))}
    }

#' Calculate distance
#'
#' Calculate distance for any number of dimensions (without using a for loop)
#' Recall, Distance = sqrt((x2 - x1)^2 + (y2 - y1)^2 ... more terms here)
#' 
#' @param point1 The first input point
#' @param point2 The secont input point.
#' @export D.ndim
D.ndim = function(point1,point2){
    #Check that point1 & point2 have same number of dimensions, if not, give an error #Currently returns a NA.  weird.
    if (length(point1) != length(point2)){
        print("Length of input points did not match")
        }

    p.diff = point1 - point2 #this subtracts each element of the vector from the appropriate other
    p.diff.sq = p.diff ^ 2   #This squares each element
    p.diff.sq.sum = sum(p.diff.sq) #This gets the sum of squares
    ndim.dist = sqrt(p.diff.sq.sum) #This takes the square root.
    ndim.dist = as.double(ndim.dist) #Drop any header information that may have come in on the input vectors (why it does that, I don't know!)

    return(ndim.dist)
    }

#' Digit
#'
#' Purpose of Digit is to take a number and place leading zeros before it if needed to bring the length of the number up to places.
#' Warning: Will not work properly with negative numbers
#' Inputs can be either numeric or character
#' Inputs must be integer values (e.g., "1" or 1 are okay, 1.5 is not.)
#' Places variable should be at least 1
#' Output is in character format (to preserve leading 0's!)
#' I have a similar function for Python, but it is implemented differently.
#' @param number a number to put into the function
#' @param places The number of places to make the number.
#'
#' @export Digit
Digit = function(number,places){
    
    #Check that Digit is a single number, not a vector
    if (length(number) > 1){
        stop("Inputs to Digit function must be single numbers, not vectors!")
        }
    
    #Check that input is an integer, if not, return NA
    test = as.integer(number)
    if (test != number){
        stop("Inputs to Digit function must be integers")
        }
    
    if (number < 0){
        stop("Digit was not programmed to help with negative numbers")
        }
    
    if (places < 1){
        stop("Number of places to round to is less than 1. This is a non-sensical input for this function")
        }
    
    number = as.character(number) #Convert number to string
    nlen = nchar(number)
    #If number is longer than the number of places needed, leave the number unchanged (other than converted to string)
    #otherwise, continue until number = places
    while (nlen < places){
        zro = "0"
        number = sprintf("%s%s",zro,number)
        nlen = nchar(number)}
        
    return(number)
    }

#' Force Shutdown
#' 
#' Tell the computer to shutdown, in a way that will still work if the machine is locked.
#' Based on code in the R package fun, except the wait option is replaced by the force option.
#' @export force.shutdown
force.shutdown = function(){
    shell("shutdown -s -f")
    }

#' Force hibernate
#'
#' Tell the computer to hibernate in a way that will still work if the machine is locked.
#' Based on code in the R package fun, except it hibernates and forces it.
#'
#' @export force.hibernate 
force.hibernate = function(){
    shell("shutdown -h -f")
    }

#' get time
#'
#' A short function so I don't have to keep writing proc.time()[3]
#'
#' @export gettime
gettime = function(){
    outtime = proc.time()[3]
    return(outtime)
    }


#' Check leap year
#'
#' A function for checking if it is a leap year
#' @param year A year to be checked
#' @export isleapyear
isleapyear = function(year){
    leapyear = 0
    #If the year is divisible by 4, mark it as a leap year
    if (as.numeric(year) %% 4 == 0){
        leapyear = 1
        }

    #Adjust for Centuries - not leap years unless divisible by 400
    #Reset century years as non-leap years
    if (as.numeric(year) %%  100 == 0){
        leapyear = 0
        #Set century years divisible by 400 as leap years
        if (as.numeric(year) %% 400 == 0){
            leapyear = 1
            }
        }

    return(leapyear)
    }

#' list to text
#'
#' Take a list (or vector) and convert it to text separated by separator.
#' This does not work for nested lists
#' @param inlist A list to be converted to text
#' @param separator The separator to separate the text
#'
#' @export listtotext
listtotext = function(inlist,separator){

    #Check that there is no nesting in the list structure
    for (thing in inlist){
        if (length(thing) > 1){
            print("One or more list elements containied multiple elements.")
            print("This is not allowed, as it will lead to strange outputs.")
            return(NA)
            }
        }
        
    #Check that separator is input (otherwise it can let a typo through where just a one element list or a separator is given)
    sep.check = separator #sep.check is not used later.  It is here to make it so R will crash if it is not input.

    #Perform list to text function
    textout = sprintf("%s",inlist[1])
    listlen = length(inlist)
    if (listlen > 1){
        for (item in inlist[2:listlen]){
            textout = sprintf("%s%s%s",textout,separator,item)
            }
        }
    return(textout)
    }

#' Make a numeric label
#' 
#' make a numeric label in a way that can easily be used with apply
#'
#' @param x The numeric part of the label
#' @param lbl The label part of the label
#' @param separ The separator for the label.
#'
#' @export make.label 
make.label = function(x,lbl,separ = "_"){
    y = sprintf("%s%s%s",lbl,separ,x)
    return(y)
    }

#DD# Remember how to double up the documentation
#The above function, but easier to type!
make.lbl = function(x,lbl,separ = "_"){
    y = make.label(x,lbl,separ)
    return(y)
    }

#' Threshold recode
#'
#' Recode data based on a threshold. Data >= threshold will be 1, data < threshold = 0.
#'
#' @param x A number to be recoded
#' @param threshold The threshold for recoding
#' @aliases threshold.recode t.recode
#' @export th.recode
th.recode = function(x,threshold){
    x = as.numeric(as.character(x)) #Convert to a number for character inputs

    if (is.na(x)){
        y = NA
    }else{
        if (x >= threshold){
            y =1
            }
        if (x < threshold){
            y = 0
            }
        }
    return(y)
    }

#' Threshold recode (alternate name)
#' @name th.recode
#' @export threshold.recode
threshold.recode = function(x, threshold){
  y = th.recode(x, threshold)
  return(y)
  }

#' Presence/absence recode
#' 
#' Convert numbers to presence/absence
#'
#' @param x A number to be converted to presence absence
#' @export pa.recode
pa.recode = function(x){
    x = as.numeric(as.character(x)) #Convert to a number for character inputs

    if (is.na(x)){
        y = NA
    }else{
        if (x > 0){
            y =1
            }
        if (x == 0){
            y = 0
            }
        if (x < 0){
            stop("Cannot have negative presences!")
            }
        }
    return(y)
    }

#' Sample (fixed)
#'
#' Default behavior of this function was unacceptable, so I fixed it
#' if length(x) == 1, returns x instead of using x as a number to be sampled from.
#' @param x The vector to be sampled
#' @param size The number of samples to take
#'
#' @export sample.fixed
sample.fixed = function(x,size){

    #Require that size be >= 0 (should it just be > 0?)
    if (size < 0){
        stop("number of samples taken must be zero or greater")
        }

    if (length(x) < size){ 
        stop("There was an error - more samples were required than values to sample from!")
        }

    #Correct the default behavior so that asking for one sample from a vector length 1 gives the element of the vector, instead of drawing from 1:the value of the element.  See documentation
    if (length(x) == 1){
        output = x
        }

    #Otherwise, proceed as normal.
    if (length(x) != 1){
        output = sample(x,size)
        }
    return(output)
    
    }
    
#' Output glm to csv
#'
#' create a function to handle formatting for a csv file
#'
#' @param sy The summary object from a glm
#' @export glmout
glmout = function(sy) #input is the summary from a glm
    {

    #Write coefficients separated by a comma
    cofs = sy$coefficients
    cout = ""
    clen = length(sy$coefficients)
    for (i in 1:clen){
        cout = sprintf("%s,%s",cout,cofs[i])
        }
    
    #Create header info
    hout = ""
    hdr = colnames(sy$coefficients)
    hdrlen = length(hdr)
    repeats = clen/hdrlen
    for (e in 1:hdrlen) {
        for (k in 1:repeats){
            if(k == 1){
                hout = sprintf("%s,%s_Intercept",hout,hdr[e])
                }
            else { 
                hout = sprintf("%s,%s",hout,hdr[e])
                }
            }
        }
    info = sprintf("%s,%s,%s%s",sy$aic,sy$df[1],sy$df[2],cout)   #No comma after df because the above function creates a preceding comma as a byproduct
    myheader = sprintf("%s,%s,%s%s","AIC","DF1","DF2",hout) #No comma after DF2 because extra is generated in the function that creates the headers
    stuff = cbind(myheader,info)
    return(stuff)
    }
    
#' lm output
#' 
#' create a function to handle formatting for a .csv file
#' @param sy The summary from the lm function
#'
#' @export lmout
lmout = function(sy) 
    {

    #Write coefficients separated by a comma
    cofs = sy$coefficients
    cout = ""
    clen = length(sy$coefficients)
    for (i in 1:clen){
        cout = sprintf("%s,%s",cout,cofs[i])
        }
    
    #Create header info
    hout = ""
    hdr = colnames(sy$coefficients)
    hdrlen = length(hdr)
    repeats = clen/hdrlen
    for (e in 1:hdrlen) {
        for (k in 1:repeats){
            if(k == 1){
                hout = sprintf("%s,%s_Intercept",hout,hdr[e])
                }
            else { 
                hout = sprintf("%s,%s",hout,hdr[e])
                }
            }
        }
    info = sprintf("%s,%s,%s,%s,%s,%s,%s,%s%s",sy$r.squared,sy$adj.r.squared,sy$fstatistic[1],sy$fstatistic[2],sy$fstatistic[3],sy$df[1],sy$df[2],sy$df[3],cout)   #No comma after df because the above function creates a preceding comma as a byproduct
    myheader = sprintf("%s,%s,%s,%s,%s,%s,%s,%s%s","R2","adjR2","F","FNUMDF","FDENOMDF","DF1","DF2","DF3",hout) #No comma after DF2 because extra is generated in the function that creates the headers
    stuff = cbind(myheader,info)
    return(stuff)
    }
    
#' Calculate standard error
#' 
#' I thought this was supposed to be divided by sqrt of n-1, but wikipedia, Stackoverflow, and Zar p 72 all say sqrt n.
#' @param values the values to calcualte the standard error for
#' 
#' @export ste
ste = function(values){
    z = sd(values)/sqrt(length(values))
    return(z)
    }

#' Calculate 95 percent ci
#'
#' Function to calculate a 95 percent confidence interval
#'
#' @param values The values to calculate the 95 percent CI for
#'
#' @export ci95
ci95 = function(values){
    z = sd(values/sqrt(length(values)) * qnorm(0.975)) #qnorm returns the z score, and 0.975 is alpha / 2.  Basically this works out to be about 1.96    
    return(z)
    }
    
#Function to test that function imported properly.  Use the below command to import the function
#source("C:/Documents/Feinstein/hR/myr.r")
#testfun = function(x)
#    {
#    print(x)
#    }

#**# I don't recall the context of this function
# #Function to test whether the result of a try command is an error
#try.error = function(x,err.val = NA){
#
#    #Set the output to be the input by default
#    out.val = x
#    
#    #Check if length of x > 1 (if so, no error, and the below step may error out)
#   if (length(x) == 1){
#    
#        #Check if the output has an error, if so, recode it with the desired error value
#        if (substr(x,1,5) == "Error"){
#            out.val = err.val
#            }
#        }
#        
#    return(out.val)
#    }

