##1. First call makecachematrix with no parameters.  This creates the function list you can use.  Assign it to a variable like 'list_of_functions'
##2. At that point, you'll have all the functions available to use.  Call list_of_functions$set(temp)  where temp is a matrix of the values you want to use
##3. Using list_of_functions$get you should be able to see the matrix values to confirm that the set function worked.
##4. Now, run cacheSolve(list_of_functions)  You should get the inverse value of the matrix
##5. Call cacheSolve(list_of_functions) again...it should now give the message that it's using the cache.

makeCacheMatrix <-
        function(x = matrix()) {
                #by default, there's an empty matrix x created
                
                m <-
                        NULL #create the cache variable.  Needs to be created here first to allow getcache to work
                set <- function(y) {
                        #set the value of the matrix
                        x <<- y #store the matrix
                        m <<- NULL #clear the cache
                }
                get <- function()
                        x #return the value of the matrix
                setcache <-
                        function(y2)
                                m <<-
                        y2 #set value of the cache matrix
                getcache <-
                        function()
                                m # get the value of the cache matrix
                list(
                        set = set,
                        get = get,
                        #return all functions in a list
                        setcache = setcache,
                        getcache = getcache
                )
                
                
        }



## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
        cache_holder <- x$getcache() ##grab any values from cache
        if (!is.null(cache_holder)) {
                #there's a value in the cache, so use it instead
                message("getting cached data")
                
        }
        else {
                #the cache was empty, so go ahead and run the solve function
                data_holder <-
                        x$get() #grab the value of the original matrix
                cache_holder <-
                        solve(data_holder, ...) #get the inverse of the matrix
                x$setcache(cache_holder) #save the value into the cache for next time
        }
        
        cache_holder ##this is either the newly updated cache, or the cache from a previous time this was run
}
