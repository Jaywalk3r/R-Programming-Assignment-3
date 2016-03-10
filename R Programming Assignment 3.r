# Part 1
#
# Plot 30-day mortality rate for heart attack


outcome = read.csv( "~/Documents/School/Coursera/R Programming/Assignments/Assignment 3/ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")

outcome[ , 11] = as.numeric( outcome[ , 11])

hist( outcome[ , 11])





# Part 2
#
# Finding the best hospital in a state

best = function( state, outcome) {
    
    data.set = read.csv( "~/Documents/School/Coursera/R Programming/Assignments/Assignment 3/ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
    
    
    
    # argument error checking
    
    valid.states = unique( data.set$State)
        # valid.states is a list of all states listed in the data set
    
    valid.outcomes = c( "heart attack", "heart failure", "pneumonia")
    
    state.test = state %in% valid.states
        # verify that state argument is a valid state
    
    outcome.test = outcome %in% valid.outcomes
        # verify that outcome argument is valid outcome
    
    
    
    if (state.test == FALSE) stop( "invalid state")
    
    if (outcome.test == FALSE) stop( "invalid outcome")
        # if either argument is invalid, function stops and returns appropriate error message.
    
    
    
    # indices for columns of interest:
    #   hospital 30 day mortality rates from
    heart.attack = 11
    
    heart.failure = 17
    
    pneumonia = 23
    
    
    
    if (outcome == "heart attack") {
        
    	data.subset = data.set[ data.set$State == state, c( 2, heart.attack)]
    	
    } else if (outcome == "heart failure") {
        
    	data.subset = data.set[ data.set$State == state, c( 2, heart.failure)]
    	
    } else if (outcome == "pneumonia") {
        
    	data.subset = data.set[ data.set$State == state, c( 2, pneumonia)]
    	
    }
        # take a subset of data relevant to function arguments
        # Only rows with state that matches argument state
        # And columns with hospital names and data for condition of interest
    
    
    
    default = getOption( "warn")
        # store setting for warnings
    
    options( warn = -1)
        # turn warnings off
    
    data.subset[ , 2] = as.numeric( data.subset[ , 2])
        # convert outcome data to numeric
    
    options( warn = default)
        # restore previous warnings setting
    
    
    
    data.subset = data.subset[ is.na( data.subset[ , 2]) == FALSE,]
        # remove hospitals without data from data.subset
    
    
    
    order.index = order( data.subset[ , 2], data.subset[ , 1])
    
    ordered.data = data.subset[ order.index,]
        # create and apply index vector to order hospitals according to outcome
        # ratings with ties ordered alphabetically by hospital name
    
    
    
    best.outcome = ordered.data[ 1,]
    
    best.outcome$Hospital.Name
        # return hospital in state of intetrest with best outcome of interest
}





# Part 3
#
# Ranking Hospitals by outcome in a state


rankhospital = function( state, outcome, num = "best") {
    
    data.set = read.csv( "~/Documents/School/Coursera/R Programming/Assignments/Assignment 3/ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
    
    # argument error checking
    valid.states = unique( data.set$State)
        # valid.states is a list of all states listed in the data set
    
    valid.outcomes = c( "heart attack", "heart failure", "pneumonia")
    
    state.test = state %in% valid.states
        # verify that state argument is a valid state
    
    outcome.test = outcome %in% valid.outcomes
        # verify that outcome argument is valid outcome
    
    
    if (state.test == FALSE) stop( "invalid state")
    
    if (outcome.test == FALSE) stop( "invalid outcome")
        # if either argument is invalid, function stops and returns appropriate error message.
    
    
    
    # indices for columns of interest:
    #   hospital 30 day mortality rates from
    heart.attack = 11
    
    heart.failure = 17
    
    pneumonia = 23
    
    
    if (outcome == "heart attack") {
        
    	data.subset = data.set[ data.set$State == state, c( 2, heart.attack)]
    	
    } else if (outcome == "heart failure") {
        
    	data.subset = data.set[ data.set$State == state, c( 2, heart.failure)]
    	
    } else if (outcome == "pneumonia") {
        
    	data.subset = data.set[ data.set$State == state, c( 2, pneumonia)]
    	
    }
        # take a subset of data relevant to function arguments
        # Only rows with state that matches argument state
        # And columns with hospital names and data for condition of interest
    
    
    default = getOption( "warn")
        # store current setting for warnings
    
    options( warn = -1)
        # turn warnings off
    
    data.subset[ , 2] = as.numeric( data.subset[ , 2])
        # convert outcome data to numeric
    
    options( warn = default)
        # restore previous warnings setting
    
    data.subset = data.subset[ is.na( data.subset[ , 2]) == FALSE,]
        # discard hospitals without outcome data for condition of interest
    
    
    order.index = order( data.subset[ , 2], data.subset[ , 1])
    
    ordered.data = data.subset[ order.index,]
        # create and apply index vector to order hospitals according to outcome
        # ratings with ties ordered alphabetically by hospital name
    
    
    if (num == "best") {
        
    	num = 1
    	
    } else if (num == "worst") {
        
    	num = dim( ordered.data)[ 1]
    	
    }
        # convert argument num to integer, if necessary
    
    if (num > dim( ordered.data)[ 1]) return( NA)
        # handle case where num > number of hospitals with data in state,
        # ending function execution and returning NA
    
    ordered.data[ num, 1]
        # returns hospital name with the numth lowest mortality rate for outcome of interest
}





# Part 4
#
# Ranking hospitals in all states


rankall = function( outcome, num = "best") {
    
    data.set = read.csv( "~/Documents/School/Coursera/R Programming/Assignments/Assignment 3/ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
    
    # argument error checking
    valid.outcomes = c( "heart attack", "heart failure", "pneumonia")
    
    outcome.test = outcome %in% valid.outcomes
        # verify that outcome argument is valid outcome
    
    if (outcome.test == FALSE) stop( "invalid outcome")
        # if outcome argument is invalid, function stops and returns "invalid outcome".
    
    
    
    # indices for columns of interest:
    #   hospital 30 day mortality rates from
    
    heart.attack = 11
    
    heart.failure = 17
    
    pneumonia = 23
    
    
    if (outcome == "heart attack") {
        
    	data.subset = data.set[ , c( 7, 2, heart.attack)]
    	
    } else if (outcome == "heart failure") {
        
    	data.subset = data.set[ , c( 7, 2, heart.failure)]
    	
    } else if (outcome == "pneumonia") {
        
    	data.subset = data.set[ , c( 7, 2, pneumonia)]
    	
    }
        # subset data to include all rows but only columns containing data for
        # hospital name (column 2), state (column 7), and outcome data
    
    
    
    default = getOption( "warn")
        # store current setting for warnings
    
    options( warn = -1)
        # Switch warnings off
    
    data.subset[ , 3] = as.numeric( data.subset[ , 3])
        # convert column of outcome data to numeric
    
    options( warn = default)
        # restore previous warnings setting
    
    data.subset = data.subset[ is.na( data.subset[ , 3]) == FALSE,]
        # discard rows for hospitals with no outcome data
    
    order.index = order( data.subset[ , 1], data.subset[ , 3],
    	data.subset[ , 2])
        
    ordered.data = data.subset[ order.index,]
        # order data by state, then outcome, then hospital name
    
    
    
    hospitals = vector()
        # initialize hospital vector
   
    states = unique( ordered.data$State)
        # create vector containing all state names one time each
    
    number.of.states = length( states)
        
    
    
    if (num == "best") num = 1
        # convert "best" to integer value
    
    for (i in 1:number.of.states) {
        
    	data.subset = ordered.data[ ordered.data$State == states[ i],]
    	    # subset data for each state, one at a time
    	
    	if (num == "worst") {
    	    
    		hospitals[ i] = tail( data.subset[ , 2], 1)
    		    # assigns hospital with worst outcome to appropriate component
    		    # of hospital variable
    		
    	} else if (num > dim( data.subset)[ 1]) {
    	    
    		hospitals[ i] = NA
    		    # specified rank exceeds number of hospitals with specified
    		    # outcome data for specified state
    		
    	} else {
    	    
    		hospitals[ i] = data.subset[ num, 2]
    		    # assigns hospital with specified outcome rank to appropriate component
    		    # of hospital variable
    		
    	}
    	
    }
    
	output = data.frame( hospital = hospitals, state = states)
	    # create desired two column data frame
	
	row.names( output) = states
	    # assign states as row names in data frame
	
	output
	    # return output
	
}