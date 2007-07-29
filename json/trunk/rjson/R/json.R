toJSON <- function( x )
{
	if( !is.vector(x) && !is.null(x) )
		stop("only vectors are accepted.");
	
	if( is.null(x) )
		return( "null" )
	
	if( is.list(x) ) {
		if( is.null(names(x)) )
			stop("not yet handled")
		if( any(duplicated(names(x))) )
			stop( "A JSON list must have unique names" );
		str = "{"
		first_elem = TRUE
		for( n in names(x) ) {
			if( first_elem )
				first_elem = FALSE
			else
				str = paste(str, ',', sep="")
			str = paste(str, deparse(n), ":", toJSON(x[[n]]), sep="")
		}
		str = paste( str, "}", sep="" )
		return( str )
	}
	
	if( length(x) != 1 ) {
		if( !is.null(names(x)) )
			return( toJSON(as.list(x)) )
		str = "["
		first_elem = TRUE
		for( val in x ) {
			if( first_elem )
				first_elem = FALSE
			else
				str = paste(str, ',', sep="")
			str = paste(str, toJSON(val), sep="")
		}
		str = paste( str, "]", sep="" )
		return( str )
	}
	
	if( is.logical(x) )
		return( ifelse(x, "true", "false") )
	
	if( is.character(x) )
		return( gsub("\\/", "\\\\/", deparse(x)) )
	
	if( is.numeric(x) )
		return( as.character(x) )
	
	stop( "shouldnt make it here - unhandled type not caught" )
}

fromJSON <- function( json_str )
{
	return( 1:123 )
}