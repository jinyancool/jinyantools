# TODO: Add comment
# 
# Author: Jinyan
###############################################################################
log.info = function(...)
{
	cat(paste(date(), getMark(), ... , "\n", sep=""));
}
log.debug = function(...)
{
	cat(paste(date(), getMark("DEBUG"), ... , "\n", sep=""));
}

getMark = function(Mark="INFO")
{
	paste(" [",Mark,"] ",sep="");
}

info<-log.info
debug<-log.debug

run_cmds <- function( cmds, run ) {
	if( !run )
		log.info( "These commands were not run (you can copy&paste on the console instead):" )
	else 
		log.info( "Running:" )
	for( cmd in cmds ) {
		log.info( " ", cmd, "" )
		if( run ){
			fail = system( cmd )
			if( fail ) {
				log.info( cmd, " Failed! Stopping..." )
				return( fail )
			}
		}
	}
	log.info( "" )
	return(0)
}


# This routine prints memory usage statistics. It calls garbage 
# collector, therefore must be used with caution, should not be 
# called from a time critical, processing intensive regions like 
# processing cycles, etc.
#
print.memory.usage = function( logtext ) {
	
	# print text log
	log.info(logtext);
	
	# collect the stats
	memusage = gc();
	
	# print the stats
	print(memusage);
}

#' A merge Function
#'
#' This function allows you to merge data and keep the order. See code: http://www.r-statistics.com/2012/01/merging-two-data-frame-objects-while-preserving-the-rows-order/
#' @param X,Y. keep_order can accept the numbers 1 or 2, in which case it will make sure the resulting merged data.frame will be ordered according to the original order of rows of the data.frame entered to x (if keep_order=1) or to y (if keep_order=2). If keep_order is missing, merge will continue working as usual. If keep_order gets some input other then 1 or 2, it will issue a warning that it doesn’t accept these values, but will continue working as merge normally would. Notice that the parameter “sort” is practically overridden when using keep_order (with the value 1 or 2).
#' @keywords merge.with.order
#' @export
#' @examples
#' merge.with.order()

merge.with.order <- function(x,y, ..., sort = T, keep_order)
{
	# this function works just like merge, only that it adds the option to return the merged data.frame ordered by x (1) or by y (2)
	add.id.column.to.data <- function(DATA)
	{
		data.frame(DATA, id... = seq_len(nrow(DATA)))
	}
	# add.id.column.to.data(data.frame(x = rnorm(5), x2 = rnorm(5)))
	order.by.id...and.remove.it <- function(DATA)
	{
		# gets in a data.frame with the "id..." column.  Orders by it and returns it
		if(!any(colnames(DATA)=="id...")) stop("The function order.by.id...and.remove.it only works with data.frame objects which includes the 'id...' order column")
        
		ss_r <- order(DATA$id...)
		ss_c <- colnames(DATA) != "id..."
		DATA[ss_r, ss_c]		
	}
    
	# tmp <- function(x) x==1; 1	# why we must check what to do if it is missing or not...
	# tmp()
    
	if(!missing(keep_order))
	{
		if(keep_order == 1) return(order.by.id...and.remove.it(merge(x=add.id.column.to.data(x),y=y,..., sort = FALSE)))
		if(keep_order == 2) return(order.by.id...and.remove.it(merge(x=x,y=add.id.column.to.data(y),..., sort = FALSE)))
		# if you didn't get "return" by now - issue a warning.
		warning("The function merge.with.order only accepts NULL/1/2 values for the keep_order variable")
	} else {return(merge(x=x,y=y,..., sort = sort))}
}
