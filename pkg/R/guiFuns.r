############################################################
#                      PBS Modelling                       #
# ---------------------------------------------------------#
# This file aims to include functions specific to          #
# createWin and other GUI functions                        #
#                                                          #
# Authors:                                                 # 
#  Jon T. Schnute <SchnuteJ@pac.dfo-mpo.gc.ca>,            #
#  Alex Couture-Beil <alex@mofo.ca>, and                   #
#  Rowan Haigh <HaighR@pac.dfo-mpo.gc.ca>                  #
#                                                          #
############################################################


# ***********************************************************
# .trimWhiteSpace:
#  remove leading and trailing whitespace
# Arguments:
#  x - string to trim
# Example:
#  "   foo bar " becomes "foo bar"
# -----------------------------------------------------------
.trimWhiteSpace <- function(x)
{
	return(sub("[[:space:]]+$", "", sub("^[[:space:]]+", "", x)))
}


# ***********************************************************
# .stripComments:
#  removes any trailing comments from a line, 
#  but ignores #'s in quoted strings
# Arguments:
#  x - a string with or without comments
# Output:
#  string without comments
# Example: 
#   x='type="label" text="I am #1" #comment'
#   returns 'type="label" text="I am #1"'
# -----------------------------------------------------------
.stripComments <- function(x)
{
	if (length(x)>1) {
		retVal <- c()
		for(i in 1:length(x)) {
			retVal[i] <- .stripComments(x[i])
		}
		return(retVal)
	}
	return(.Call("stripComments", as.character(x), PACKAGE="PBSmodelling"))
}


# ***********************************************************
# .inCollection:
#   returns true if needle occurs in haystack
# Input: 
#   haystack - a vector to search
#   needle   - a single element to search for
# -----------------------------------------------------------
.inCollection <- function(haystack, needle)
{
	if (is.null(haystack)) {
		return(FALSE)
	}
	if (is.vector(haystack)) {
		return(any(haystack==needle))
	}
	stop("only vectors are supported")
	return(FALSE)
}


# ***********************************************************
# .isReallyNull:
#   returns true if key is not a real key of the list
#   false if key is found in names(list)
# Arguments:
#   list - a named list
#   key  - named element of list to look for
# -----------------------------------------------------------
.isReallyNull <- function(list, key)
{
	return (!any(names(list)==key))
}


# ***********************************************************
# .searchCollection:
#   searches a haystack for a needle, or a similar longer needle.
# Arguments:
#   haystack - list to search
#   needle = scaler to search for
# Output: 
#   position of needle in list (non-negative)
#   -1 if none are found
#   -2 if two similar needles are found.
#      ex) -2 for "nee" is similar to "need", and "needle"
# -----------------------------------------------------------
.searchCollection <- function(haystack, needle)
{
	similar <- -1
	for(i in 1:length(haystack)) {
		if (haystack[[i]]$param == needle) {
			return(i)
		}
		else if (any(grep(paste("^", needle, sep=""), haystack[[i]]$param))) {
			#this is used to find any similar matches
			#if more than two are similar, then it is impossible
			#to determine which needle we are after
			if (similar == -1)
				similar <- i #this is the first similar needle
			else
				similar <- -2 #two similar needles were found
		}
	}
	return(similar)
}


# ***********************************************************
# .map.init:
#   initialize the datastructure that holds the map(s)
# Arguments:
#   winName - name of map to initialize
# -----------------------------------------------------------
.map.init <- function(winName)
{
	.PBSmod[[winName]] <<- list()

	#to hold tclvar pointers
	.PBSmod[[winName]]$widgetPtrs <<- list()

	#to hold widget definition lists (i.e. from win desc file)
	.PBSmod[[winName]]$widgets <<- list()
}


# ***********************************************************
# .map.add:
#   save a new value for a given key.
#   if a previous exists ignore the new value, and return previous value
# Arguments:
#   winName - map to extract values from
#   key     - name of item to extract (i.e. widget name)
#   ...     - named items to save (in a list)
# -----------------------------------------------------------
.map.add <- function(winName, key, ...)
{
	if (.isReallyNull(.PBSmod, winName))
		.map.init(winName)

	if (!is.character(key)) {
		stop("map error - key must be a string")
	}
	if (key=="") {
		stop("map error - key must be atleast 1character long")
	}

	if (!.isReallyNull(.PBSmod[[winName]]$widgetPtrs, key))
		return(.PBSmod[[winName]]$widgetPtrs[[key]])

	.PBSmod[[winName]]$widgetPtrs[[key]] <<- list(...)
}


# ***********************************************************
# .map.set:
#   save a new value for a given key, even if it involves
#   overwriting a previously stored value
# Arguments:
#   winName - map to extract values from
#   key     - name of item to extract (i.e. widget name)
#   ...     - named items to save (in a list)
# -----------------------------------------------------------
.map.set <- function(winName, key, ...)
{
	if (.isReallyNull(.PBSmod, winName))
		.map.init(winName)

	if (!is.character(key))
		stop("map error - key must be a string")
	if (key=="")
		stop("map error - key must be atleast 1character long")

	if (!is.list(.PBSmod[[winName]]$widgetPtrs[[key]]))
		.PBSmod[[winName]]$widgetPtrs[[key]] <<- list()

	#set additional keys
	tmp <- list(...)
	tmpNames <- names(tmp)
	if (length(tmp)>0) {
		for (i in 1:length(tmp)) {
			if (is.null(tmpNames[i]))
				.PBSmod[[winName]]$widgetPtrs[[key]][[i]] <<- tmp[[i]]
			else if (tmpNames[i]=="")
				.PBSmod[[winName]]$widgetPtrs[[key]][[i]] <<- tmp[[i]]
			else
				.PBSmod[[winName]]$widgetPtrs[[key]][[tmpNames[i]]] <<- tmp[[i]]
		}
	}

	return(.PBSmod[[winName]]$widgetPtrs[[key]])
}

# ***********************************************************
# .map.get:
#   Returns a value associated with a key
# Arguments:
#   winName - map to extract values from
#   key     - name of item to extract (i.e. widget name)
# -----------------------------------------------------------
.map.get <- function(winName, key)
{
	return(.PBSmod[[winName]]$widgetPtrs[[key]])
}

# ***********************************************************
# .map.getAll:
#   Returns all visible items of a map of a certain window
# Arguments:
#   winName - map to extract values from
# -----------------------------------------------------------
.map.getAll <- function(winName)
{
	return(.PBSmod[[winName]]$widgetPtrs)
}


# ***********************************************************
# .extractVar:
#   extracts values from the tclvar ptrs
# Arguments:
#   winName - name of target window to extract data from
# -----------------------------------------------------------
.extractVar <- function(winName)
{
	#data is a list containing sub-lists in the form:
	#list(type="tcl", tclvar="tcl_var_ptr", mode="numeric")
	data <- .map.getAll(winName)

	values <- list()
	keys <- names(data)
	if (length(data)<1)
		return(NULL)

	#extract values from tcl into an R list whose index corresponds to the data list
	for(i in 1:length(data)) {
		wid <- .PBSmod[[winName]]$widgets[[keys[i]]]
		if (!is.null(data[[i]]$tclvar)) {
			values[[i]] <- tclvalue(data[[i]]$tclvar)
		}
		else if (!is.null(data[[i]]$tclwidget)) {
			#special case for text widgets
			values[[i]] <- tclvalue(tkget(data[[i]]$tclwidget,"0.0","end"))
			wid$mode <- "character"
		}
		else {
			stop(paste("unknown type:", data[[i]]))
		}

		#convert data to propper type
		if (is.null(wid$mode))
			mode <- "numeric"
		else
			mode <- wid$mode

		values[[i]] <- .convertMode(values[[i]], mode)
	}

	retData <- list()
	for(i in 1:length(values)) {
		#look for any vectors (arrays, matrices)
		#vector names end in [1,4,2...]
		if (any(grep("^[^\\[]+\\[([0-9,]+)\\]$", keys[i]))) {
			#extract the indicies (ind) and name of vector
			ind<-gsub("^[^\\[]+\\[([0-9,]+)\\]$", "\\1", keys[i])
			ind<-as.numeric(unlist(strsplit(ind, ",")))
			name <- gsub("\\[[0-9,]+\\]", "", keys[i])
			if (length(ind)>1) {
				#process multiple idicies (matrix, or array)

				#values from matricies are stored into a list 
				#and then converted into a matrix at a later stage
				if (!exists("matrixTmp"))
					matrixTmp <- list()

				#create a list for the new matrix
				if (.isReallyNull(matrixTmp, name)) {
					matrixTmp[[name]] <- list()
				}

				#call matrixhelper to build a list, and then save the new changes
				matrixTmp[[name]] <- .matrixHelp(matrixTmp[[name]], ind, values[[i]])
			}
			else {
				#single index found (vector)
				if (.isReallyNull(retData, name))
					retData[[name]] <- NA
				retData[[name]][ind] <- values[[i]]
			}
		}
		#any var ending with indicies and a "d" EX: var[3,5]d is an element of a data.frame
		else if (any(grep("^[^\\[]+\\[([0-9,]+)\\]d$", keys[i]))) {
			ind<-gsub("^[^\\[]+\\[([0-9,]+)\\]d$", "\\1", keys[i])
			ind<-as.numeric(unlist(strsplit(ind, ",")))
			name <- gsub("\\[[0-9,]+\\]d", "", keys[i])
			if (length(ind)>1) {
				#store into a list just like we do with a matrix
				if (!exists("dataframeTmp"))
					dataframeTmp <- list()

				#create a list for the new matrix
				if (.isReallyNull(dataframeTmp, name)) {
					dataframeTmp[[name]] <- list()
				}

				#call matrixhelper to build a list, and then save the new changes
				dataframeTmp[[name]] <- .matrixHelp(dataframeTmp[[name]], ind, values[[i]])
			}
			else {
				#single index
				retData[[name]][ind] <- values[[i]]
			}
		}
		else {
			#no index (ie var is of standard type: [a-z0-9_]+)
			retData[[keys[i]]] <- values[[i]]
			if (!.isReallyNull(.PBSmod[[winName]]$widgets[[keys[i]]], ".name"))
				names(retData[[keys[i]]]) <- .PBSmod[[winName]]$widgets[[keys[i]]]$.name
		}
	}

	#convert all collected matrix lists into real n-dim arrays.
	if (exists("matrixTmp")) {
		keys <- names(matrixTmp)
		for(i in 1:length(matrixTmp)) {
			colnames <- .PBSmod[[winName]]$widgets[[keys[i]]]$colnames
			if (is.null(colnames)) 
				colnames <- ""

			rownames <- .PBSmod[[winName]]$widgets[[keys[i]]]$rownames
			if (is.null(rownames)) 
				rownames <- ""

			retData[[keys[i]]] <- .convertMatrixListToMatrix(matrixTmp[[i]])
			#can't use dimnames incase of 3 dimension or higher arrays
			tmpNames <- .PBSdimnameHelper(rownames, colnames, dim(retData[[keys[i]]]))
			#base::rownames <- does not work, use the following work-around
			retData[[keys[i]]] <- base::"rownames<-"(retData[[keys[i]]], tmpNames[[1]])
			retData[[keys[i]]] <- base::"colnames<-"(retData[[keys[i]]], tmpNames[[2]])
		}
	}

	#convert dataframe lists into dataframes
	if (exists("dataframeTmp")) {
		keys <- names(dataframeTmp)
		for(i in 1:length(dataframeTmp)) {
			colnames <- .PBSmod[[winName]]$widgets[[keys[i]]]$colnames
			rownames <- .PBSmod[[winName]]$widgets[[keys[i]]]$rownames

			retData[[keys[i]]] <- .convertMatrixListToDataFrame(dataframeTmp[[i]], colnames, rownames)
			#can't use dimnames incase of 3 dimension or higher arrays
			tmpNames <- .PBSdimnameHelper(rownames, colnames, dim(retData[[keys[i]]]))
			#base::rownames <- does not work, use the following work-around
			retData[[keys[i]]] <- base::"rownames<-"(retData[[keys[i]]], tmpNames[[1]])
			retData[[keys[i]]] <- base::"colnames<-"(retData[[keys[i]]], tmpNames[[2]])
		}
	}

	#assign vecnames to any vectors
	for(wid in .PBSmod[[winName]]$widgets) {
		if (wid$type=="vector") {
			if (!.isReallyNull(retData, wid$names)) {
				if (any(wid$vecnames!=""))
					names(retData[[wid$names]]) <- wid$vecnames
			}
		}
	}

	return(retData)
}


# ***********************************************************
# .PBSdimnameHelper:
#   adds dimnames to stuff (matrix, data.frame)
# Arguments:
#   rownames - vector of size 1, or dim[1] nameing the rows.
#              if only one name is given, a number (1..dim[1]) will be appended to the name
#   colnames - vector of size 1 or dim[2] naming columns
#              if only one name is given, then (1..dim[2]) is appended
#   dim      - vector of size 2, dim[1] is nRows, dim[2] is nCols

# -----------------------------------------------------------
.PBSdimnameHelper <- function(rownames, colnames, dim)
{
	if (is.null(rownames)) rownames <- ""
	if (is.null(colnames)) colnames <- ""

	if (length(rownames)>1)
		rName <- rownames
	else if (length(rownames)==0)
		rName <- NULL
	else if (rownames=="")
		rName <- NULL
	else {
		nRows <- dim[1]
		rName <- paste(rownames, 1:nRows, sep="")
	}

	if (length(colnames)>1)
		cName <- colnames
	else if (length(colnames)==0)
		cName <- NULL
	else if (colnames=="")
		cName <- NULL
	else {
		nCols <- dim[2]
		cName <- paste(colnames, 1:nCols, sep="")
	}
	return(list(rName, cName))
}


# ***********************************************************
# .convertMatrixListToMatrix:
#   converts a list into an N-dim array
# Arguments:
#   mList = z[[1]][[1]]...[[1]]=x
#           z[[1]][[1]]...[[2]]=x
#           ...
#           z[[1]][[1]]...[[1]]=x
#           ...
#           z[[i]][[j]]...[[k]]=x
#
# output an N-dim array
# -----------------------------------------------------------
.convertMatrixListToMatrix <- function(mList)
{
	size <- .getMatrixListSize(mList)
	arr <- array(dim=size)
	arr <- .setMatrixElement(mList, arr)
	return(arr)
}


# ***********************************************************
# .convertMatrixListToDataFrame:
#   similar to toArray but to data.frame
# Arguments:
#   mList - see .convertMatrixListToMatrix:
# -----------------------------------------------------------
.convertMatrixListToDataFrame <- function(mList, colName="Y", rowNames="X")
{
	size <- .getMatrixListSize(mList)
	arr <- array(dim=size)
	arr <- .setMatrixElement(mList, arr)

	x<-list()
	for(i in 1:size[2]) {
		x[[i]]<-list()
	}

	for(i in 1:length(mList)) {
		for(j in 1:length(mList[[i]])) {
			x[[j]][[i]] <- mList[[i]][[j]]
		}
	}

	if (length(rowNames)==0) {
		rowNames=NULL
	}
	else {
		if (length(rowNames)==1) {
			if (rowNames=="") {
				rowNames=NULL
			}
			rowNames <- paste(rowNames, 1:size[1], sep="")
		}
		else if (length(rowNames)!=size[1])
			stop(paste("rowNames should be NULL, or a vector of size 1 or", size[1], ".\nGot rowNames=", rowNames, sep=""))
	}

	#create a data.frame
	argList <- list(row.names=rowNames)
	for(i in 1:size[2]) { #foreach column
		name <- paste("X", i, sep="")
		argList[[name]] <- unlist(x[[i]])
	}
	return(do.call("data.frame", argList))
}


# ***********************************************************
# .setMatrixElement:
#   helper function used by .convertMatrixListToMatrix
#   to assign values from the matrix list into the array
# -----------------------------------------------------------
.setMatrixElement <- function(m, a, ind=NULL)
{
	if (is.null(m))
		return(a)
	if (!is.list(m)) {
		eval(parse(text=paste("a[", paste(ind, collapse=','), "] <- m", sep="")))
		return(a)
	}

	for(i in 1:length(m)) {
		a<-.setMatrixElement(m[[i]], a, c(ind,i))
	}
	return(a)
}


# ***********************************************************
# .getMatrixListSize:
#   helper function used by .convertMatrixListToMatrix
#   to determine the minumum required size of the array
#   needed to create to convert the list into an array
# -----------------------------------------------------------
.getMatrixListSize <- function(m, d=NULL, big=0)
{
	if (!is.list(m)) {
		return(pmax(d, big))
	}

	for(i in 1:length(m)) {
		big <- .getMatrixListSize(m[[i]], c(d,i), big)
	}
	return(big)
}


# ***********************************************************
# func:
#   used to help .extractVar deal with N-dim maticies
#   firstly it is converted into a "matrix list"
#   once the matrix list is completed (and size known)
#   it should be converted into a true array
# -----------------------------------------------------------
.matrixHelp <- function(matrixList, ind, value)
{

	if (length(ind)>1) {
		if (length(matrixList)<ind[1])
			matrixList[[ind[1]]]<-list()
		else if(!is.list(matrixList[[ind[1]]]))
			matrixList[[ind[1]]]<-list()

		matrixList[[ind[1]]] <- .matrixHelp(matrixList[[ind[1]]], ind[-1], value)
		return(matrixList)
	}
	else if(length(ind)==1) {
		matrixList[[ind[1]]]<-value
		return(matrixList)
	}
	else {
		stop(".matrixHelp() was called with no indices.")
	}
}

# ***********************************************************
# focusWin:
#   brings focus to a window (doesn't work from R console)
# args:  winName   - window to focus
#        winVal - if T, make this the active window too
# -----------------------------------------------------------
focusWin <- function(winName, winVal=TRUE)
{
	if (.isReallyNull(.PBSmod, winName))
		stop(paste("supplied winName \"", winName, "\" is not a valid window", sep=""))
	tkfocus(.PBSmod[[winName]]$tkwindow)
	if (winVal)
		.PBSmod$.activeWin <<- winName
}


# ***********************************************************
# createWin:
#   creates a GUI window from a given file, or GUI description list
# -----------------------------------------------------------
createWin <- function(fname, astext=FALSE)
{
	#must be called here for examples in rd to pass check
	.initPBSoptions()

	#parse window description into a valid widget tree
	if (is.character(fname)) {
		guiDesc <- parseWinFile(fname, astext=astext)
	}
	else if (is.list(fname)) {
		guiDesc <- .validateWindowDescList(fname)
	}
	else {
		cat("ERROR, supplied argument is wrong type\n")
		return()
	}

	if (is.null(guiDesc)) {
		return()
	}

	#iterate over all possible windows
	for(i in 1:length(guiDesc)) {
		winName <- guiDesc[[i]]$windowname
		
		if (is.null(winName))
			stop("No window name given.")
		
		#destroy any existing windows with the same name
		tt <- .PBSmod[[winName]]$tkwindow
		if (!is.null(tt))
			tkdestroy(tt)

		#clear the storage for this window
		.map.init(guiDesc[[i]]$windowname)

		#store windowname as most recent active window
		.PBSmod$.activeWin <<- guiDesc[[i]]$windowname

		#Had to re-write tktoplevel to allow binding to the destroy
		#action without breaking the cleanup process
		mytktoplevel <- function(command="", parent = .TkRoot, ...)
		{
			w <- tkwidget(parent, "toplevel", ...)
			ID <- .Tk.ID(w)
			tkbind(w, "<Destroy>", function() {

				#call the user function (if supplied)
				if (is.null(command)) {}
				else if (command=="") {}
				else if (exists(command,mode="function")) {
					do.call(command, list())
				}
				else {
					cat(paste("Warning: cannot find function '", command, "'.\n", sep=""))
				}

				#finish up with tcltk clean up (from real tktoplevel func)
				if (exists(ID, envir = parent$env, inherits = FALSE)) 
					rm(list = ID, envir = parent$env)
				tkbind(w, "<Destroy>", "")
			})
			w
		}

		#set tcl/tk color palette with window bg and fg
		#this will change ALL window background colours
		tcl("tk_setPalette", 
		    "background", guiDesc[[i]]$winBackground, 
		    "activebackground", guiDesc[[i]]$winBackground, 
		    "foreground", guiDesc[[i]]$winForeground,
		    "activeforeground", guiDesc[[i]]$winForeground,
		    "selectColor", "white" #inner colour of checkboxes
		    )

		#create TK window
		tt <- mytktoplevel(command=guiDesc[[i]]$onclose)

		#store the TK handle (so we can destroy it at a later time via closeWin)
		.PBSmod[[winName]]$tkwindow <<- tt

		#set window title
		tkwm.title(tt,guiDesc[[i]]$title)

		#remove any old history data
		if (exists("PBS.history")) {
			j<-grep(paste("^", winName, "\\.", sep=""), names(PBS.history))
			for(n in names(PBS.history)[j]) {
				PBS.history[[n]] <<- NULL
			}
		}

		#create menus
		if (length(guiDesc[[i]]$.menus) > 0) {
			#create the top space where drop down menus are attached
			topMenu <- tkmenu(tt)
			tkconfigure(tt,menu=topMenu)

			#define function to create menu widgets
			.createMetaWidget.menu <- function(topMenu, widget)
			{
				label <- widget$label
				if (widget$nitems < 1)
					stop("menu nitems must have atleast one menuitem.")

				subMenu <- tkmenu(topMenu, tearoff=FALSE)

				for(i in 1:widget$nitems) {
					if (widget$.widgets[[i]]$type=="menu") {
						.createMetaWidget.menu(subMenu, widget$.widgets[[i]])
						#stop("submenus need work.")
					}
					else if (widget$.widgets[[i]]$type=="menuitem")
					{
						#something weird happened that required me to use eval
						#otherwise all menus only got the settings of the last created menu
						if (widget$.widgets[[i]]$font == "")
							eval(parse(text=paste('
							tkadd(subMenu,"command",
							label=widget$.widgets[[i]]$label,
							command=function(...) .extractData("',widget$.widgets[[i]][["function"]],'", "',widget$.widgets[[i]][["action"]],'", "',winName,'"))
							', sep="")))
						else
							eval(parse(text=paste('
							tkadd(subMenu,"command",
							label=widget$.widgets[[i]]$label,
							font=.createTkFont("', widget$.widgets[[i]]$font, '"),
							command=function(...) .extractData("',widget$.widgets[[i]][["function"]],'", "',widget$.widgets[[i]][["action"]],'", "',winName,'"))
							', sep="")))
					}
					else {
						stop(paste("widget type", widget$.widgets[[i]]$type, "found when expecting a menu or menuitem widget"))
					}

				}
				if (widget$font == "")
					tkadd(topMenu,"cascade",label=label,menu=subMenu)
				else
					eval(parse(text=paste('
					tkadd(topMenu,"cascade",label=label,menu=subMenu, font=.createTkFont("', widget$font, '"))
					')))
			}
			#.createMetaWidget.menu function finished


			#create menus
			for(menu_i in 1:length(guiDesc[[i]]$.menus)) {
				.createMetaWidget.menu(topMenu, guiDesc[[i]]$.menus[[menu_i]])
			}
		}

		#create the widgets
		if (length(guiDesc[[i]]$.widgets)>0) {
			#pack all widgets into a grid with ncol=1 nrow=<number of widgets>
			gridWidget = list(
			             type="grid", 
			             font="",
			             borderwidth=0,
			             relief="flat",
			             padx=0,
			             pady=0,
			             .widgets=list(),
			             nrow <- length(guiDesc[[i]]$.widgets),
			             ncol <- 1,
			             byrow=guiDesc[[i]]$vertical
			)

			#add all widgets to this grid, each in a new row [[j]] and 1st column [[1]]
			for(j in 1:length(guiDesc[[i]]$.widgets)) {
				gridWidget$.widgets[[j]] <- list()
				gridWidget$.widgets[[j]][[1]] <- guiDesc[[i]]$.widgets[[j]]
			}

			#create the newly created gridWidget - which will result in recursive
			#calls to .createWidget which will create all the children of the grid
			wid <- .createWidget(tt, gridWidget, guiDesc[[i]]$windowname)
			tkgrid(wid)
			
			#finish setup to any history widgets which have default imports
			if (exists("PBS.history")) {
				j<-grep(paste("^", winName, "\\.", sep=""), names(PBS.history))
				for(n in names(PBS.history)[j]) {
					if (length(PBS.history[[n]]) > 1)
						jumpHistory(n, 1)
				}
			}
		}
	}
	return(invisible(NULL))
}


# ***********************************************************
# closeWin:
#   closes a window
# Arguments:
#   name - window name to close
# -----------------------------------------------------------
closeWin <- function(name)
{
	if (missing(name))
		name <- names(.PBSmod)
	name <- grep("^[^\\.]", name, value=TRUE)
	for(n in name) {
		if(!.isReallyNull(.PBSmod, n)) {
			tt <- .PBSmod[[n]]$tkwindow
			tkdestroy(tt)
		}
	}
}


# ***********************************************************
# .validateWindowDescList:
#   determines if the list represents a valid PBS Modelling description List
#   if any required fields are missing, it will halt via stop()
#   if any fields are ommitied which have default values defined in the
#   .widgetDefs list, then those fields and values will be set
# Arguments:
#   x - list to validate
# -----------------------------------------------------------
.validateWindowDescList <- function(x)
{
	if (!is.list(x))
		stop("no list was given")
	if (length(x)==0)
		stop("No windows were given")

	paramOrder <- .widgetDefs

	for (i in 1:length(x)) {
		#validate each window

		#check for a window title
		if (is.null(x[[i]]$title))
			x[[i]]$title <- paramOrder$window$title

		#check for widgets
		if (!is.list(x[[i]]$.widgets))
			stop("The widget list is missing")

		x[[i]]$.widgets <- .validateWindowDescWidgets(x[[i]]$.widgets)

		#TODO - check .menu too?

		if (is.null(x[[i]]$winBackground))
			x[[i]]$winBackground = "#D4D0C8"

		if (is.null(x[[i]]$winForeground))
			x[[i]]$winForeground = "#000000"
	}

	return(x)
}


# ***********************************************************
# .validateWindowDescWidgets:
#   used by .validateWindowDescList to validate each widget
# Arguments:
#   x - widget list to validate
# Note: this function is similar to .getParamFromStr but is
#       only designed for lists, and is not as robust.
#       ex: -no error messages for filename/line number
#           -no support for expanding shortened param names
#           -no type conversion
# -----------------------------------------------------------
.validateWindowDescWidgets <- function(x)
{
	paramOrder <- .widgetDefs
	for(i in 1:length(x)) {
		type <- x[[i]]$type
		if (is.null(paramOrder[[type]]))
			stop(paste("unknown widget type found:", type))
		#check children widgets of grid
		if (type=="grid") {
			if (!is.list(x[[i]]$.widgets))
				stop("grid needs a .widgets list")
			for(j in 1:length(x[[i]]$.widgets))
				x[[i]]$.widgets[[j]] <- .validateWindowDescWidgets(x[[i]]$.widgets[[j]])
		}
		#look for all options, if any are missing assign the default value
		#unless they are absolutely required
		args <- paramOrder[[type]]
		for(j in 1:length(args)) {
			if (is.null(x[[i]][[args[[j]]$param]])) {
				#a paramater is missing from the list.

				#is the paramater required?
				if (args[[j]]$required)
					stop(paste("missing argument", args[[j]]$param, "from widget", type))

				#is there a default value?
				if (!is.null(args[[j]]$default))
					x[[i]][[args[[j]]$param]] <- args[[j]]$default
			}
			else {
				#the argument was found. let's check that its the right type
				#and matches the grep

				#check grep if applicable
				if (!is.null(args[[j]]$grep)) {
					#check grep from .widgetDefs with supplied value from list
					if (!any(grep(args[[j]]$grep, x[[i]][[args[[j]]$param]])))
						stop(paste("given value \"", x[[i]][[args[[j]]$param]], 
						"\" does not match grep:", args[[j]]$grep, sep=""))
				}

			}

		}
	}
	return(x)
}


# ***********************************************************
# parseWinFile:
#   parse window description file into a list
# Arguments:
#   fname - filename or vector of strings
#   astext - if F, treat fname as a filename
#            if T, treat it as the contents of a file
# -----------------------------------------------------------
parseWinFile <- function(fname, astext=FALSE)
{
	if (astext) {
		#treat "\n" in astext mode as normal whitespace
		srcfile <- orgfile <- sub("\n", " ", fname)
		fname <- "read as text"
	}
	else {
		#read in file, and strip out any comments. (comments start with #)
		if (fname=="")
			stop("No filename given")
		srcfile <- orgfile <- scan(fname, what=character(), sep="\n", quiet=TRUE, blank.lines.skip=FALSE)
	}
	data <- list()
	j <- 0
	halt <- FALSE
	extendLine <- FALSE #used for extending a single line into lines with \
	extendLineNumber <- 0 #where a new widget starts - used for error messages
	str <- ""

	if (!length(srcfile)) {
		stop("Input file is empty\n")
	}

	#if comments were striped out earlier, we would lose the line count.
	for(i in 1:length(srcfile)) {
		if (!any(grep("^[[:space:]]*(#.*)?$", srcfile[i]))) {

			srcfile[i] <- .stripComments(srcfile[i])

			#append last string onto new string if applicable
			if (extendLine == TRUE)
				str <- paste(str, srcfile[i], sep=" ")
			else {
				str <- srcfile[i]
				extendLineNumber <- i
			}

			#determine if this string is extended by a \ at the end.
			tmp <- sub('\\\\$', '', str)
			if (tmp==str) #no sub took place
				extendLine = FALSE
			else
				extendLine = TRUE
			str <- tmp

			#parse the line once it is complete (no \)
			if (extendLine == FALSE) {
				tmp <- .getParamFromStr(str, fname, extendLineNumber, i, orgfile)
				if (is.null(tmp)) {
					halt <- TRUE
				}
				else if(halt==FALSE) {
					j <- j + 1
					data[[j]]<-tmp
				}
			}
		}
	}
	if (halt==TRUE) {
		stop("Errors were found in the GUI description file. Unable to continue\n")
	}

	#by this point all widgets from the text file have been converted into
	#an appropriate list of widgets, we will need to setup the nested grids
	#this will result in a more recursive tree-like list.

	#create a blank window if data is empty
	if (!length(data)) {
		data[[1]] <- .getParamFromStr("window") 
	}

	#we must make sure the first element is a window, if not we will insert one
	#as the head of the list
	if (data[[1]]$type != "window") {
		data <- c(1, data)
		data[[1]] <- .getParamFromStr("window") #pull in all defaults from defs.R
	}

	#data[[1]] is now guarenteed to be a window type

	#start parsing the read data - this mostly setups grid data
	parsedData<-list()
	j <- 0; #widget index
	i <- 0; #window index
	k <- 0; #menu index
	while(length(data)) {
		#pull out any options
		if (data[[1]]$type=="window") {
			i <- i + 1 #increment new window index
			j <- 0 #reset widget index
			k <- 0 #reset menu index
			parsedData[[i]] <- list()
			parsedData[[i]]$title <- data[[1]]$title
			parsedData[[i]]$windowname <- data[[1]]$name
			parsedData[[i]]$vertical <- data[[1]]$vertical
			parsedData[[i]]$onclose <- data[[1]]$onclose
			parsedData[[i]]$winBackground <- data[[1]]$bg
			parsedData[[i]]$winForeground <- data[[1]]$fg
			parsedData[[i]]$.widgets <- list() #holds all widgets
			parsedData[[i]]$.menus <- list() #holds all menu widgets

			data <- data[-1]
		}
		else {
			#look for menu widgets
			if (data[[1]]$type=="menu") {
				k <- k + 1 #increment menu index

				#save menu widget
				parsedData[[i]]$.menus[[k]] <- data[[1]]

				#pull out n menuitem
				tmp <- .parsemenu(data[-1], data[[1]]$nitems)
				parsedData[[i]]$.menus[[k]]$.widgets <- tmp$menuData

				#parse remaining widgets
				data <- tmp$unparsedData
			}

			#look for regular widgets
			else {
				j <- j + 1 #incrememnt widget index

				#save widget
				parsedData[[i]]$.widgets[[j]] <- data[[1]]

				#associate child widgets if grid
				if (data[[1]]$type=="grid") {

					tmp <- .parsegrid(data[-1], data[[1]]$nrow, data[[1]]$ncol)
					parsedData[[i]]$.widgets[[j]]$.widgets <- tmp$gridData

					#.parsegrid returns all left over widgets
					data <- tmp$unparsedData
				}
				else {
					data <- data[-1] #remove widget from to parse list
				}
			}
		}

	}

	return(parsedData)
}


# ***********************************************************
# func: - very similar to .parsegrid but for menus
#   set up a menu with children menus or menuitems
# Arguments:
#  data   - list of widgets to be used as child of menu
#  nItems - how many children to select for the menu
# -----------------------------------------------------------
.parsemenu <- function(data, nItems)
{
	menuitems <- list()
	itemCount <- 0
	while(length(data)) {
		#increment count
		itemCount <- itemCount + 1


		if (data[[1]]$type!="menuitem" && data[[1]]$type!="menu")
			stop("non menu, or menuitem widget found, when expecting one. Check your menu nitems count.")


		#add/associate widget with menu
		menuitems[[itemCount]] <- data[[1]]

		#add a nested menu type
		if (data[[1]]$type=="menu") {
			tmp <- .parsemenu(data[-1], data[[1]]$nitems)
			menuitems[[itemCount]]$.widgets <- tmp$menuData
			data <- tmp$unparsedData
		}
		else {
			data <- data[-1] #remove widget
		}

		#return menu sub items
		if (itemCount == nItems) {
			tmp <- list()
			tmp$menuData <- menuitems
			tmp$unparsedData <- data #left over data
			return(tmp)
		}
	}

	stop("menu did not have enough child menuitems. Check your menu nitems count.")
}


# ***********************************************************
# .parsegrid:
#   returns two items in a list:
#   - $gridData which is a list of lists representing columns
#   - $unparsedData - which is left over from the grid and 
#     still needs parsing
# Arguments:
#   data - list of widget lists
#   nRow - num of grid rows
#   nCol - num of grid columns
# -----------------------------------------------------------
.parsegrid <- function(data, nRow, nCol)
{
	parsedData=list()
	rows=list()
	cols=list()
	row <- 0;
	col <- 0;
	while(length(data)) {
		#add item into column
		col <- col + 1
		cols[[col]] <- data[[1]]

		#add a nested grid type
		if (data[[1]]$type=="grid") {
			tmp <- .parsegrid(data[-1], data[[1]]$nrow, data[[1]]$ncol)
			cols[[col]]$.widgets <- tmp$gridData
			data <- tmp$unparsedData
		}
		else {
			data <- data[-1]
		}

		#check for a filled row
		if (col == nCol) {
			row <- row + 1
			col <- 0
			rows[[row]] <- cols

			#any more rows left?
			if (row == nRow) {
				#return two parts of the data
				tmp <- list()
				tmp$gridData <- rows
				tmp$unparsedData <- data #left over data
				return(tmp)
			}
		}
	}
	stop("Grid did not have enough child objects.")
}


# ***********************************************************
# .stripSlashes:
#   removes slashes from a string
# Arguments:
#   TODO
# -----------------------------------------------------------
.stripSlashes <- function(x, fname="", line.start=0, line.end=0, sourcefile=list())
{
	word<-""
	escape<-0
	for(i in 1:nchar(x)) {
		ch<-substr(x,i,i)

		#escaped char is expected
		if (escape!=0) {
			if (ch=="n")
				ch <- "\n"
			else if (ch=="t")
				ch <- "\t"
			else if (ch=="r")
				ch <- "\r"

			word <- paste(word, ch, sep="")
			escape <- 0
		}

		#next char will be escapped
		else if (ch=="\\") {
			escape <- 1
		}
		#shouldnt find any singlequotes - if we did it should be a vector of strings
		else if (ch=="'" || ch=="\"") {
			.catError("unexpected singlequote found.", fname, line.start, line.end, sourcefile)
			return(NULL)
		}
		#any other character
		else {
			word <- paste(word, ch, sep="")
		}
	}
	return(word)
}


# ***********************************************************
# func:
#   given a string x, x is split into a vector of words, which were seperated by spaces
#   however, if single quotes are used, space is perserved
#   x="a b 'c d'" converts into "a" "b" "c d"
# Arguments:
#  
# -----------------------------------------------------------
.stripSlashesVec <- function(x, fname="", line.start=0, line.end=0, sourcefile=list())
{
	word<-""
	words=c()
	escape<-0
	quoted<-0
	quoteFound<-0
	j <- 0
	for(i in 1:nchar(x)) {
		ch<-substr(x,i,i)

		#escaped char is expected
		if (escape!=0) {
			if (ch=="n")
				ch <- "\n"
			else if (ch=="t")
				ch <- "\t"
			else if (ch=="r")
				ch <- "\r"

			word <- paste(word, ch, sep="")
			escape <- 0
		}

		#next char will be escapped
		else if (ch=="\\") {
			escape <- 1
		}
		#shouldnt find any doublequotes anywhere
		else if (ch=="\"") {
			.catError("unexpected doublequote found.", fname, line.start, line.end, sourcefile)
			return(NULL)
		}
		else if (ch=="'") {
			if (quoted==0) {
				quoted<-1
				quoteFound<-1
			}
			else {
				quoted <- 0
			}
		}
		#space found
		else if (ch==" " || ch=="\t") {
			if (quoted==0) {
				if (word!="" || quoteFound==1) {
					##save key and value
					j <- j + 1
					words[j] <- word

					#reset variables for next loop
					word <- ""
					quoteFound <- 0
				}
			}
			else {
				word <- paste(word, ch, sep="")
			}
		}
		#any other character
		else {
			word <- paste(word, ch, sep="")
		}
	}
	#look for last word
	if (quoted==0) {
		if (word!="" || quoteFound==1) {
			##save key and value
			j <- j + 1
			words[j] <- word

			#reset variables for next loop
			word <- ""
			quoteFound <- 0
		}
	}
	else {
		.catError("unterminated quote found.", fname, line.start, line.end, sourcefile)
		return(NULL)
	}
	if (is.null(words))
		words[1]<-""
	return(words)
}


# ***********************************************************
# .convertParamStrToVector:
#   function to convert a string, x, into a vector of elements seperated by
#   whitespace.
#   whitespace can be interupted as values if it is enclosed by quotes.
#   special characters (newline, tab, \, ', ") must be escaped with \
#
# Arguments:
#   x     - string
#   fname - filename string for warning messages
#   line  - line number for warning messages
#
# Output:  
#   vector of values
# -----------------------------------------------------------
.convertParamStrToVector <- function(x, fname="", line=0)
{
	#TODO - this function needs optimization for readlist preformance
	#profilling shows a slow spot to be paste() calls
	escape<-0
	quoted<-0	#0=none, 1=", 2='
	word<-""
	j <- 0 #counter for words array
	equal <- 0 #counter for equal char
	quotefound <- 0 #used to capture ""
	words <- NULL
	#todo: triple check it's not needed - 
	# I dont think so since stripcomments is used prior
	#x<-.trimWhiteSpace(x)

	myNewEnv <- new.env()
	environment(myNewEnv) <- asNamespace("PBSmodelling")
	words <- .Call("strToVector", 
	               x, 
	               myNewEnv,
	               fname,
	               line,
	               PACKAGE="PBSmodelling")
	if (is.null(words))
		stop("Errors were found in the P-formated list. Unable to continue\n", call.=FALSE)
	return(words)
}


.catError2 <- function(err, fname, line.start)
{
	err <- paste("Parse error", " (", fname, ":", line.start, ") : ", err, "\n", sep="")
	cat(err)
}

# ***********************************************************
# func:
#   converts a given string of values seperated by spaces into a list
#   while preserving space and escaped quotes within quotes 
#   (kindof - the value must still be stripped depending if its a single string, or vector of strings)
# Arguments:
#  
# -----------------------------------------------------------
.convertParamStrToList <- function(x, fname="", line.start=0, line.end=0, sourcefile=list())
{
	escape<-0
	quoted<-0	#0=none, 1=", 2='
	word<-""
	j <- 0 #counter for words array
	equal <- 0 #counter for equal char
	quotefound <- 0 #used to capture ""
	words <- list()
	#don't think its needed since stripcomments should do this too
	#x<-.trimWhiteSpace(x)

	myNewEnv <- new.env()
	environment(myNewEnv) <- asNamespace("PBSmodelling")
	words <- .Call("strToList", 
	               x, 
	               myNewEnv,
	               fname,
	               line.start,
	               PACKAGE="PBSmodelling")
	if (is.null(words))
		stop("Errors were found in the GUI description file. Unable to continue\n", call.=FALSE)
	return(words)
}


# ***********************************************************
# .catError:
#   used to display parsing errors
# Arguments:
#   err        - error string to display
#   fname      - file name where error was found
#   line.start - starting line of widget with error
#   line.end   - end line of widget with error
#   sourcefile - source code of the file in question
#   errorType  - type of error to display
# -----------------------------------------------------------
.catError <- function(err, fname, line.start, line.end, sourcefile=list(), errorType="GUI parse error")
{
	err <- paste(errorType, " (", fname, ":", line.start, ") : ", err, "\n", sep="")
	cat(err)
	if (length(sourcefile)>0) {
		for(i in line.start:line.end) {
			cat(paste(i, ": ", sourcefile[[i]], "\n", sep=""))
		}
		cat("\n")
	}
}


# ***********************************************************
# .stopWidget:
#   Fatal error during window creation (not parse)
# Arguments:
#   err       - error string to display
#   wid.debug - list of widget code (created in parsing process
#   winName   - active window name
# -----------------------------------------------------------
.stopWidget <- function(err, wid.debug, winName)
{
	err <- paste("\nGUI parse error (", wid.debug$fname, ":", 
	             wid.debug$line.start, ") : ", err, "\n\n", sep="")

	if (length(wid.debug$sourceCode)>0) {
		j <- 0;
		for(i in wid.debug$line.start:wid.debug$line.end) {
			j <- j + 1
			err <- paste(err, i, ": ", wid.debug$sourceCode[j], "\n", sep="")
		}
	}

	tt <- .PBSmod[[winName]]$tkwindow
	tkdestroy(tt)

	stop(err, call.=FALSE)
}



# ***********************************************************
# .getParamFromStr:
#   returns a list with all parameters extracted from a list
# Arguments:
#   inputStr   - string from win desc file describing a widget
#   fname      - filename to display with error messages
#   line.start - line number where widget is first found
#   line.end   - line number of last line of widget (ie extended line)
#   sourcefile - 
#   paramOrder
# -----------------------------------------------------------
.getParamFromStr <- function(inputStr, fname="", line.start=0, line.end=0, 
                             sourcefile=list(), paramOrder=.widgetDefs)
{
	#now passed in function - to enable overriding
	# a "constant" defines how the parameters should look.


	namedArguments <- 0 # determines if we can accept unnamed arguments
	paramData <- list() #extracted params from a line to return
	typeDefined <- 0 #this must be set before returning

	if (inputStr=="") {
		.catError(paste("input line is empty", sep=""), fname, line.start)
		return(NULL)
	}

	#split input string into seperate arguments
	s<-.convertParamStrToList(inputStr, fname, line.start, line.end, sourcefile)

	for(j in 1:length(s)) {
		value<-s[[j]]$value

		#argument is named, unnamed arguments are no longer valid
		if (!is.null(s[[j]]$key)) {
			namedArguments <- 1 
			key<-casefold(s[[j]]$key)
			paramData[[key]] <- value
			if (typeDefined==0) {
				if (key=="type") {
					typeDefined <- 1;

					#case of type is ignored
					paramData[[key]] <- value <- casefold(value, upper=FALSE) 

					#fetch argument Ordering
					argOrder <- paramOrder[[value]]
				}
			}
		}

		#argument is not named (no key was given)
		else if(namedArguments==0) {
			if (j==1) {
				#first argument must be "type"
				widgetType <- paramData$type <- casefold(value)

				#fetch argument Ordering
				argOrder <- paramOrder[[widgetType]]
				if (is.null(argOrder)) {
					#given widget type is not valid
					.catError(paste("unknown widget type '", paramData$type,"'", sep=""), fname, line.start, line.end, sourcefile)
					return(NULL)
				}
				typeDefined <- 1;
			}
			else if(j > length(argOrder)) {
				.catError(paste("more arguments given than supported by widget '",paramData$type,"'", sep=""), fname, line.start, line.end, sourcefile)
				return(NULL)
			}
			else {
				#determine the name of the second argument
				argName <- argOrder[[j]]$param
				paramData[[argName]] <- value
			}
		}

		#error - unnamed arg given after named arg
		else {
			.catError(paste("unnamed argument with value \"",value,"\" given after a named argument.", sep=""), fname, line.start, line.end, sourcefile)
			return(NULL)
		}
	}

	#test if a type has been defined
	if (typeDefined==0) {
		.catError(paste("no widget type given", sep=""), fname, line.start, line.end, sourcefile)
		return(NULL)
	}

	#check that widget type is valid
	if(.isReallyNull(paramOrder, paramData$type)) {
		.catError(paste("unknown widget type '", paramData$type,"'", sep=""), fname, line.start, line.end, sourcefile)
		return(NULL)
	}

	#test if all given arguments are valid arguments of the given type
	#and then convert from character to another type if applicable
	errorFound <- 0
	givenNames <- names(paramData)
	for(i in 1:length(givenNames)) {
		pos <- .searchCollection(argOrder, givenNames[i])
		if (pos==-1) {
			#argument name not valid
			.catError(paste('argument \'', givenNames[i], '\' is not supported by widget \'', paramData$type, '\'', sep=""), fname, line.start, line.end, sourcefile)
			errorFound <- 1
		}
		else if (pos == -2) {
			.catError(paste('argument \'', givenNames[i], '\' of widget \'', paramData$type, '\' matches multiple formal arguments.', sep=""), fname, line.start, line.end, sourcefile)
			errorFound <- 1
		}
		else {
			#sometimes, only a bit of the paramater name is given
			#if so, we would like to know the full name
			fullParamName <- argOrder[[pos]]$param
			if (fullParamName != givenNames[i]) {
				names(paramData)[i] <- fullParamName
			}

			#check supplied argument data matches grep pattern (if defined)
			if (!is.null(argOrder[[pos]]$grep)) {
				if (!any(grep(argOrder[[pos]]$grep, paramData[[i]]))) {
					#supplied data is not formatted correctly
					.catError(paste('argument \'', givenNames[i], '\': value \'', paramData[[i]], '\' is not accepted. It should match', argOrder[[pos]]$grep , sep=""), 	fname, line.start, line.end, sourcefile)
					errorFound <- 1
				}
			}

			#some strings - if character need to be stripped of slashes, or sub-divided
			if (errorFound == 0 && !is.null(argOrder[[pos]]$class)) {
				if (argOrder[[pos]]$class=="character") {
					#convert value to either a single string (.stripSlashes)
					tmp <- .stripSlashes(paramData[[i]], fname, line.start, line.end, sourcefile)
					if (is.null(tmp)) {
						#most likely an unescaped quote was found - .catError called from .stripSlashes, not here
						errorFound <- 1
					}
					else
						paramData[[i]] <- tmp
				}
				else if (argOrder[[pos]]$class=="characterVector") {
					#convert value to a vector of strings
					tmp <- .stripSlashesVec(paramData[[i]], fname, line.start, line.end, sourcefile)
					if (is.null(tmp)) {
						errorFound <- 1
					}
					else
						paramData[[i]] <- tmp
				}
				else if ((argOrder[[pos]]$class=="numeric" || argOrder[[pos]]$class=="integer") && any(grep("[a-z]", paramData[[i]], ignore.case=TRUE))) {
					paramData[[i]] <- 0
				}
				else if (argOrder[[pos]]$class=="logical" && !any(grep("^(F|T|FALSE|TRUE|false|true)$", paramData[[i]]))) {
					paramData[[i]] <- FALSE
				}
				else {
					paramData[[i]] <- as(paramData[[i]], argOrder[[pos]]$class)
				}
			}
		}
	}
	if (errorFound != 0)
		return(NULL)

	#check that all required arguments have been supplied
	for(i in 1:length(argOrder)) {
		if (argOrder[[i]]$required) {
			if (.isReallyNull(paramData, argOrder[[i]]$param)) {
				.catError(paste('required argument \'', argOrder[[i]]$param, '\' is missing from widget \'', paramData$type, '\'', sep=""), fname, line.start, line.end, sourcefile)
				errorFound <- 1
			}
		}
		else if (!is.null(argOrder[[i]]$default)) {
			#fill in any default values if applicible
			if (is.null(paramData[[argOrder[[i]]$param]])) {
				#set it to default value
				paramData[[argOrder[[i]]$param]] <- argOrder[[i]]$default
			}
		}
	}
	if (errorFound != 0)
		return(NULL)

	#convert default values from character class to specified class ($mode in widgetDefs.r)
	if ( !.isReallyNull(paramData,"value") && !.isReallyNull(paramData,"mode") && paramData$type=="radio" ) {
		paramData$value <- as(paramData$value, paramData$mode)
	}

	#store debug information to be used if there are any errors while building the GUI
	sourceCode <- c()
	if (line.start<=line.end && !missing(sourcefile)) {
		for(i in line.start:line.end) {
			sourceCode <- c(sourceCode, sourcefile[[i]])
		}
	}
	paramData$.debug <- list(sourceCode=sourceCode, fname=fname, line.start=line.start, line.end=line.end)

	return(paramData)
}


# ***********************************************************
# .buildgrid:
#   used to create a grid on a window
# Arguments:
#   tk      - parent tk frame to attach widget to
#   grid    - widget list describing the grid
#   winName - active window name
# -----------------------------------------------------------
.buildgrid <- function(tk, grid, winName)
{
	toptitle <- grid$toptitle
	sidetitle <- grid$sidetitle

	if (is.null(toptitle))
		toptitle <- ""

	if (is.null(sidetitle))
		sidetitle <- ""

	if (is.null(grid$ncol)) {
		grid$ncol=length(grid$.widgets[[1]])
	}

	if (is.null(grid$nrow)) {
		grid$nrow=length(grid$.widgets)
	}

	#offset the title (useful for centering titles over a certain part)
	#like over the 3 columns of a matrix, but not row labels
	if (is.null(grid$toptitle.offset))
		grid$toptitle.offset<-0
	if (is.null(grid$sidetitle.offset))
		grid$sidetitle.offset<-0

	#set byrow
	if (is.null(grid$byrow)) {
		grid$byrow=TRUE
	}

	#set font options
	if (is.null(grid$topfont))
		topfont <- ""
	else
		topfont <- grid$topfont

	if (is.null(grid$sidefont))
		sidefont <- ""
	else
		sidefont <- grid$sidefont

	#display title (if set)
	if (toptitle!="") {
		colspan=as.integer(grid$ncol)-grid$toptitle.offset
		argList <- list(parent=tk, text=toptitle)
		if (!is.null(grid$fg) && grid$fg!="")
			argList$foreground=grid$fg
		if (!is.null(grid$bg) && grid$bg!="")
			argList$background=grid$bg
		if (topfont!="")
			argList$font <- .createTkFont(topfont)
		mytklabel<-do.call("tklabel", argList)
		tkgrid(mytklabel, columnspan=colspan, row=0, column=1+grid$toptitle.offset)
	}

	#display column title (if set)
	if (sidetitle!="") {
		rowspan=as.integer(grid$nrow)-grid$sidetitle.offset
		argList <- list(parent=tk, text=sidetitle)
		if (!is.null(grid$fg) && grid$fg!="")
			argList$foreground=grid$fg
		if (!is.null(grid$bg) && grid$bg!="")
			argList$background=grid$bg
		if (topfont!="")
			argList$font <- .createTkFont(sidefont)
		mytklabel<-do.call("tklabel", argList)
		tkgrid(mytklabel, rowspan=rowspan, row=1+grid$sidetitle.offset, column=0)
		showsidetitle<-TRUE
	}

	#loop over all children widget of the grid.
	#these are stored as grid$.widgets[[row_id]][[col_id]]
	for(i in 1:length(grid$.widgets)) {
		for(j in 1:length(grid$.widgets[[i]])) {
			#create Widget (here's the recursive widget creation call)
			widget <- .createWidget(tk, grid$.widgets[[i]][[j]], winName)

			#set row and column position
			if (grid$byrow==TRUE) {
				row=i
				column=j
			} else {
				row=j
				column=i
			}

			#y padding
			if (is.null(grid$.widgets[[i]][[j]]$pady))
				pady <- 0
			else
				pady <- grid$.widgets[[i]][[j]]$pady

			#x padding
			if (is.null(grid$.widgets[[i]][[j]]$padx))
				padx <- 0
			else
				padx <- grid$.widgets[[i]][[j]]$padx

			#build begining of argument list for tkgrid() function
			argList <- list(widget, row=row, column=column, padx=padx, pady=pady)

			#append sticky flag argument if set
			if (is.character(grid$.widgets[[i]][[j]]$sticky)) {
				argList$sticky <- grid$.widgets[[i]][[j]]$sticky
			}
			do.call("tkgrid", argList)
		}
	}
	return(tk)
}


# ***********************************************************
# .createTkFont:
#   creates a usable TK font from a given string
# Arguments:
#   fontStr - string describing a font and colour
# -----------------------------------------------------------
.createTkFont <- function(fontStr)
{
	fontstr <- .convertParamStrToVector(casefold(fontStr))

	#default options
	fontparam<-list()

	for(i in 1:length(fontstr)) {
		if (fontstr[i]=="bold")
			fontparam$weight="bold"
		else if (fontstr[i]=="italic")
			fontparam$slant="italic"
		else if (fontstr[i]=="underline")
			fontparam$underline=TRUE
		else if (fontstr[i]=="overstrike")
			fontparam$overstrike=TRUE
		else if (fontstr[i]=="times")
			fontparam$family="Times"
		else if (fontstr[i]=="courier")
			fontparam$family="Courier"
		else if (fontstr[i]=="helvetica")
			fontparam$family="Helvetica"
		else if (any(grep("^[0-9]+$", fontstr[i])))
			fontparam$size=fontstr[i]
		else if (fontstr[i] != "")
			cat(paste("warning: ignoring font option \"", fontstr[i], "\"\n", sep=""))
	}
	return(do.call(tkfont.create, fontparam))
}


# ***********************************************************
# .createWidget:
#   generic function to create most widgets, which
#   calls appropriate .createWidget.xxxWidgetTypexxx() func
# Arguments:
#   tk      - frame to attach widget to
#   widget  - widget list
#   winName - active window name
# -----------------------------------------------------------
.createWidget <- function(tk, widget, winName)
{
	#save functions
	if (!is.null(widget[["function"]])) {
		if (widget[["function"]]!="") {
			if (!any(.PBSmod[[winName]]$functions==widget[["function"]]))
				.PBSmod[[winName]]$functions <<- c(.PBSmod[[winName]]$functions, widget[["function"]])
		}
	}

	#save widget information by name parameter (and not widget$name)
	#widget name can sometimes be "foo[1,2,3]" or some such combo.
	#where as type="vector" name="foo" is never seen in the regular map
	if (!is.null(widget$name)) {
		if (length(widget$name)==1) {
			if (.isReallyNull(.PBSmod[[winName]]$widgets, widget$name)) {
				.PBSmod[[winName]]$widgets[[widget$name]] <<- widget
			}
		}
	}

	#look for a function called .createWidget.WIDGETTYPE
	#all of these functions have the same parameters: (tk, widget, winName)
	func <- paste(".createWidget.", widget$type, sep="")
	if (exists(func,mode="function")) {
		return(do.call(func, list(tk, widget, winName)))
	}
	else {
		stop(paste("Don't know how to create '", widget$type, "' widget\n", sep=""))
		return()
	}
	return(tkWidget)
}

.createWidget.grid <- function(tk, widget, winName)
{
	#these "defaults" only apply to the first layer grid
	#because it is added as padding, and not parsed.
	#all other defaults are set in paramOrder list

	if (is.null(widget$borderwidth))
		widget$borderwidth <- 5

	if (is.null(widget$relief))
		widget$relief <- "flat"

	argList <- list(parent=tk, borderwidth=widget$borderwidth,relief=widget$relief)
	if (!is.null(widget$bg) && widget$bg!="")
		argList$background=widget$bg
	if (!is.null(widget$font) && widget$font!="")
		argList$font <- .createTkFont(widget$font)

	tkWidget<-do.call("tkframe", argList)

	#call buildgrid to attach all children widgets to grid
	.buildgrid(tkWidget, widget, winName)

	return(tkWidget)
}

.createWidget.check <- function(tk, widget, winName)
{
	widget$mode = "logical"

	if (widget$checked==TRUE)
		val <- 1
	else
		val <- 0

	argList <- list(parent=tk, text=widget$text)
	if (!is.null(widget$fg) && widget$fg!="")
		argList$foreground=widget$fg
	if (!is.null(widget$bg) && widget$bg!="")
		argList$background=widget$bg
	if (!is.null(widget$font) && widget$font!="")
		argList$font <- .createTkFont(widget$font)

	argList$variable <- .map.add(winName, widget$name, tclvar=tclVar(val))$tclvar
	argList$command=function(...) { .extractData(widget[["function"]], widget$action, winName)}

	tkWidget<-do.call("tkcheckbutton", argList)

	return(tkWidget)
}

.createWidget.label <- function(tk, widget, winName)
{
	argList <- list(parent=tk, text=widget$text)
	if (!is.null(widget$fg) && widget$fg!="")
		argList$foreground=widget$fg
	if (!is.null(widget$bg) && widget$bg!="")
		argList$background=widget$bg
	if (!is.null(widget$font) && widget$font!="")
		argList$font <- .createTkFont(widget$font)
	if (!is.null(widget$justify) && widget$justify!="")
		argList$justify <- widget$justify
	if (!is.null(widget$wraplength) && widget$wraplength > 0)
		argList$wraplength <- widget$wraplength 

	tkWidget<-do.call("tklabel", argList)
	return(tkWidget)
}

.createWidget.null <- function(tk, widget, winName)
{
	tkWidget<-tklabel(tk,text="")
	return(tkWidget)
}

.createWidget.matrix <- function(tk, widget, winName)
{

	nrow <- widget$nrow
	ncol <- widget$ncol

	names <- widget$names
	#TODO - check all names are valid

	rowlabels <- widget$rowlabels
	rownames <- widget$rownames
	collabels <- widget$collabels
	colnames <- widget$colnames

	if (is.null(rownames)) rownames <- ""
	if (is.null(colnames)) colnames <- ""
	if (is.null(rowlabels)) rowlabels <- ""
	if (is.null(collabels)) collabels <- ""

	if (all(widget$values==""))
		values <- ""
	else {
		values <- widget$values
		#dim(values) <- c(nrow, ncol)
	}

	wid <- list(type="grid", bg=widget$bg, fg=widget$fg) #new grid widget to create
	wid$.widgets <- list() #elements in the grid
	wid$byrow <- TRUE

	nNames <- length(names)
	nRowlabels <- length(rowlabels)

	nRowNames <- length(rownames)
	nValues <- length(values)
	nCollabels <- length(collabels)
	nColNames <- length(colnames)

	#count names
	if (nNames!=1 && nNames!=(ncol*nrow))
    	.stopWidget(paste('"names" argument must contain 1 or', ncol*nrow, 'names seperated by whitespace.'), widget$.debug, winName)
  
	#count rowlabels
	if (nRowlabels!=1 && nRowlabels!=nrow)
		.stopWidget(paste('"rowlabels" argument should contain 1 or', nrow, 'labels.'), widget$.debug, winName)

	#count collabels
	if (nCollabels!=1 && nCollabels!=ncol)
		.stopWidget(paste('"collabels" argument should contain 1 or',ncol,'labels.'), widget$.debug, winName)

	#count rownames
	if (nRowNames!=1 && nRowNames!=nrow)
		.stopWidget(paste('"rownames" argument should contain 1 or',nrow,'labels.'), widget$.debug, winName)

	#count colnames
	if (nColNames!=1 && nColNames!=ncol)
		.stopWidget(paste('"colnames" argument should contain 1 or',ncol,'labels.'), widget$.debug, winName)

	if (all(rowlabels=="NULL"))
			colLabelOffset <- 0
		else
			colLabelOffset <- 1

	#single labels should be displayed as the title
	if (nCollabels==1 && ncol>1) {
		wid$toptitle<-collabels[1]
		wid$topfont<-widget$font
		wid$toptitle.offset<-1 #to help center the label
		#have counting labels above each column
		wid$.widgets[[1]] <- list()
		wid$.widgets[[1]][[1]] <- list(type='label', text="", bg=widget$bg, fg=widget$fg)
		for(j in 1:ncol) {
			wid$.widgets[[1]][[j+colLabelOffset]] <- list(type='label', text=j, font=widget$font, bg=widget$bg, fg=widget$fg)
		}
	}
	else {
		wid$.widgets[[1]] <- list()
		wid$.widgets[[1]][[1]] <- list(type='label', text="", bg=widget$bg, fg=widget$fg)
		for(j in 1:ncol) {
			wid$.widgets[[1]][[j+colLabelOffset]] <- list(type='label', text=collabels[j], font=widget$font, bg=widget$bg, fg=widget$fg)
		}
	}

	#row title
	if (nRowlabels==1 && nrow>1) {
		wid$sidetitle<-rowlabels[1]
		wid$sidefont<-widget$font
		wid$sidetitle.offset<-1 #to help center the label
	}

	for(i in 1:nrow) {
		rowCount <- i #the first row of inputs should be 1 (even if there are labels ontop)
		i <- i + 1 #first row has labels
		wid$.widgets[[i]] <- list()
		for(j in 1:(ncol+1)) {
			#first row is for labels
			if (j==1) {
				if (!all(rowlabels=="NULL")) {
					if (nRowlabels==1 && nrow>1) {
						text <- as.character(rowCount)
					}
					else
						text <- rowlabels[rowCount]

					tmp_i <- i
					if (all(collabels[1]=="NULL"))
						tmp_i <- tmp_i - 1
					wid$.widgets[[tmp_i]][[j]] <- list(type='label', text=text, font=widget$font, bg=widget$bg, fg=widget$fg)
				}
			}
			else {
				if (nNames==1) #single name given
					name <- paste(names,'[',rowCount,',',j-1,']',sep="")
				else #many names given
					if (widget$byrow)
						name <- names[j-1+(ncol*(i-2))]
					else
						name <- names[i-1+(nrow*(j-2))]
				if (nValues==1) {
					value <- values
				}
				else {
					if (widget$byrow)
						value <- values[j-1+(ncol*(i-2))]
					else
						value <- values[i-1+(nrow*(j-2))]
				}

				#tweak offset if labels are disabled
				# ****must be un-tweaked after creating the entry or check widget
				if (all(rowlabels[1]=="NULL"))
					j <- j - 1
				if (all(collabels[1]=="NULL"))
					i <- i - 1

				if (widget$mode=="logical") {
					#display a checkbox
					if (is.na(as.logical(value)))
						checked=FALSE
					else if (as.logical(value)==TRUE)
						checked=TRUE
					else
						checked=FALSE
					wid$.widgets[[i]][[j]] <- list(
						  type='check',
						  mode="logical",
						  name=name,
						  text="",
						  "function"=widget[["function"]],
						  action=widget$action,
						  checked=checked,
						  bg=widget$bg,
						  fg=widget$entryfg
					)
				}
				else {
					#display a entry box
					wid$.widgets[[i]][[j]] <- list(
						  type='entry', 
						  name=name,
						  "function"=widget[["function"]],
						  action=widget$action,
						  enter=widget$enter,
						  value=value,
						  width=widget$width,
						  mode=widget$mode,
						  entryfont=widget$entryfont,
						  entrybg=widget$entrybg,
						  entryfg=widget$entryfg
					)

				}
				# ***untweak offset for special case of no labels
				if (all(rowlabels=="NULL"))
					j <- j + 1
				if (all(collabels=="NULL"))
					i <- i + 1
			}
		}
	}

	#look out for a trailing list (only happens if rowlabels=NULL)
	i <- length(wid$.widgets)
	if (!length(wid$.widgets[[i]]))
		wid$.widgets[[i]] <- NULL

	if (all(rowlabels=="NULL")) {
		wid$sidetitle <- ""
		wid$toptitle.offset <- NULL
	}
	if (all(collabels=="NULL")) {
		wid$toptitle <- ""
		wid$sidetitle.offset <- NULL
	}

	tkWidget <- .createWidget.grid(tk, wid, winName)
	return(tkWidget)
}

.createWidget.vector <- function(tk, widget, winName)
{
	names <- widget$names
	labels <- widget$labels

	if (all(labels==""))
		labels <- names

	if (all(widget$values==""))
		values <- ""
	else
		values <- widget$values

	n <- widget$length
	wid <- list(type="grid", bg=widget$bg, fg=widget$fg) #new grid widget to create
	wid$byrow = widget$vertical #pass byrow param to grid

	nNames <- length(names)
	nVecnames <- length(widget$vecnames)
	nLabels <- length(labels)

	if (n==0) {
		if (nNames != nLabels && nNames != 1 && nLabels != 1)
			.stopWidget('"labels" and "names" arguments should have the same amount of substrings.', widget$.debug, winName)
		if (nNames == 1 && nLabels == 1) {
			n<-1
		}
		else if (nNames != 1)
			n<-nNames
		else
			.stopWidget('missing "length" argument', widget$.debug, winName)
	}

	if (widget$vertical) {
		wid$nrow <- n
		wid$ncol <- 2
		wid$toptitle.offset=1
		wid$toptitle=""
	}
	else {
		wid$nrow <- 2
		wid$ncol <- n
	}

	#count names
	if (nNames!=1 && nNames!=n)
		.stopWidget(paste("names argument must contain 1 or",n,"names seperated by spaces.\nreceived:", widget$names), widget$.debug, winName)

	#count vecnames
	if (nVecnames!=n && widget$vecnames!="")
		.stopWidget(paste('vecnames argument should contain',n,'vector names.'), widget$.debug, winName)


	#count labels
	if (nLabels!=1 && nLabels!=n)
		.stopWidget(paste('labels argument should contain 1 or',n,'labels.'), widget$.debug, winName)

	#single labels should be displayed as the title
	if (nLabels==1 && n!=1 && all(widget$labels!="NULL")) {
		wid$toptitle=labels[1]
		wid$topfont<-widget$font
	}

	nValues <- length(values)
	if (nValues!=1 && nValues!=n)
		.stopWidget(paste('values argument should contain 1 or',n,'values seperated by whitespace.'), widget$.debug, winName)

	#create children to be placed in the grid
	wid$.widgets <- list()
	for(i in 1:n) {
		wid$.widgets[[i]] <- list()

		#create label
		if (nLabels==1 && n!=1)
			text <- as.character(i)
		else
			text <- labels[i]

		wid$.widgets[[i]][[1]] <- list(type='label', text=text, font=widget$font, fg=widget$fg, bg=widget$bg)

		#create entry
		if (nNames==1)
			name <- paste(names, '[', i, ']',sep="")
		else
			name <- names[i]

		if (nValues==1)
			value <- values[1]
		else
			value <- values[i]

		if (all(widget$vecnames==""))
			vname <- NULL
		else
			vname <- widget$vecnames[i]

		if (all(widget$labels=="NULL"))
			entryIndex <- 1
		else
			entryIndex <- 2

		if (widget$mode=="logical") {
			#display a checkbox
			if (is.na(as.logical(value)))
				checked=FALSE
			else if (as.logical(value)==TRUE)
				checked=TRUE
			else
				checked=FALSE
			wid$.widgets[[i]][[entryIndex]] <- list(
				  type='check',
				  mode="logical",
				  name=name,
				  text="",
				  "function"=widget[["function"]],
				  action=widget$action,
				  checked=checked,
				  bg=widget$bg,
				  fg=widget$entryfg,
				  .name=vname
			)
		}
		else {
			#display a entry box
			wid$.widgets[[i]][[entryIndex]] <- list(
				  type='entry', 
				  name=name,
				  "function"=widget[["function"]],
				  action=widget$action,
				  enter=widget$enter,
				  value=value,
				  width=widget$width,
				  mode=widget$mode,
				  entryfont=widget$entryfont,
				  entrybg=widget$entrybg,
				  entryfg=widget$entryfg,
				  .name=vname
			)

		}

	}

	tkWidget <- .createWidget.grid(tk, wid, winName)
	return(tkWidget)
}

.createWidget.data <- function(tk, widget, winName)
{

	nrow <- widget$nrow
	ncol <- widget$ncol

	names <- widget$names
	modes <- widget$modes


	rowlabels <- widget$rowlabels
	rownames <- widget$rownames
	collabels <- widget$collabels
	colnames <- widget$colnames

	if (is.null(widget$rownames)) widget$rownames <- ""
	if (is.null(widget$colnames)) widget$colnames <- ""

	if (all(widget$values==""))
		values <- ""
	else {
		values <- widget$values
		#dim(values) <- c(nrow, ncol)
	}

	wid <- list(type="grid", bg=widget$bg) #new grid widget to create
	wid$.widgets <- list() #elements in the grid
	wid$byrow <- TRUE

	nNames <- length(names)
	nModes <- length(modes)
	nRowlabels <- length(rowlabels)
	nRowNames <- length(rownames)
	nValues <- length(values)
	nCollabels <- length(collabels)
	nColNames <- length(colnames)

	#count names
	if (nNames!=1 && nNames!=(ncol*nrow))
		.stopWidget(paste('names argument must contain 1 or',ncol*nrow,'names seperated by whitespace.'), widget$.debug, winName)

	#count modes
	if (nModes!=1 && nModes!=ncol)
		.stopWidget(paste('modes argument must contain 1 or',ncol,'modes seperated by whitespace.'), widget$.debug, winName)
    
	#count rowlabels
	if (nRowlabels!=1 && nRowlabels!=nrow)
		.stopWidget(paste('rowlabels should contain 1 or',nrow,'labels.'), widget$.debug, winName)

	#count rownames
	if (nRowNames!=1 && nRowNames!=nrow)
		.stopWidget(paste('rownames argument should contain 1 or',nrow,'labels.'), widget$.debug, winName)

	#count collabels
	if (nCollabels!=1 && nCollabels!=ncol)
		.stopWidget(paste('collabels argument should contain 1 or',ncol,'labels.'), widget$.debug, winName)

	#count colnames
	if (nColNames!=1 && nColNames!=ncol)
		.stopWidget(paste('colnames argument should contain 1 or',ncol,'labels.'), widget$.debug, winName)

	if (all(rowlabels=="NULL"))
			colLabelOffset <- 0
		else
			colLabelOffset <- 1


	#single labels should be displayed as the title
	if (nCollabels==1 && ncol>1) {
		wid$toptitle<-collabels[1]
		wid$topfont<-widget$font
		wid$toptitle.offset<-1 #to help center the label
		#have counting labels above each column
		wid$.widgets[[1]] <- list()
		wid$.widgets[[1]][[1]] <- list(type='label', text="", bg=widget$bg, fg=widget$fg)
		for(j in 1:ncol) {
			wid$.widgets[[1]][[j+colLabelOffset]] <- list(type='label', text=j, font=widget$font, bg=widget$bg, fg=widget$fg)
		}
	}
	else {
		wid$.widgets[[1]] <- list()
		wid$.widgets[[1]][[1]] <- list(type='label', text="", bg=widget$bg, fg=widget$fg)
		for(j in 1:ncol) {
			wid$.widgets[[1]][[j+colLabelOffset]] <- list(type='label', text=collabels[j], font=widget$font, bg=widget$bg, fg=widget$fg)
		}
	}

	#row title
	if (nRowlabels==1 && nrow>1) {
		wid$sidetitle<-rowlabels[1]
		wid$sidefont<-widget$font
		wid$sidetitle.offset<-1 #to help center the label
	}

	for(i in 1:nrow) {
		rowCount <- i #the first row of inputs should be 1 (even if there are labels ontop)
		i <- i + 1 #first row has labels
		wid$.widgets[[i]] <- list()

		for(j in 1:(ncol+1)) {
			#first row is for labels
			if (j==1) {
				if (!all(rowlabels=="NULL")) {
					if (nRowlabels==1 && nrow>1) {
						text <- as.character(rowCount)
					}
					else
						text <- rowlabels[rowCount]

					tmp_i <- i
					if (all(collabels[1]=="NULL"))
						tmp_i <- tmp_i - 1
					wid$.widgets[[tmp_i]][[j]] <- list(type='label', text=text, font=widget$font, bg=widget$bg, fg=widget$fg)
				}
			}
			else {
				if (nNames==1) #single name given
					name <- paste(names,'[',rowCount,',',j-1,']d',sep="")
				else #many names given
					if (widget$byrow)
						name <- names[j-1+(ncol*(i-2))]
					else
						name <- names[i-1+(nrow*(j-2))]
				if (nValues==1) {
					value <- values
				}
				else {
					if (widget$byrow)
						value <- values[j-1+(ncol*(i-2))]
					else
						value <- values[i-1+(nrow*(j-2))]
				}
				if (nModes==1)
					mode <- modes[1]
				else
					mode <- modes[j-1] #columns are offset by one

				#tweak offset if labels are disabled
				# ****must be un-tweaked after creating the entry or check widget
				if (all(rowlabels=="NULL"))
					j <- j - 1
				if (all(collabels[1]=="NULL"))
					i <- i - 1

				if (mode=="logical") {
					#display a checkbox
					if (is.na(as.logical(value)))
						checked=FALSE
					else if (as.logical(value)==TRUE)
						checked=TRUE
					else
						checked=FALSE
					wid$.widgets[[i]][[j]] <- list(
						  type='check',
						  mode="logical",
						  name=name,
						  text="",
						  "function"=widget[["function"]],
						  action=widget$action,
						  checked=checked,
						  bg=widget$bg,
						  fg=widget$entryfg
					)
				}
				else {
					#display a entry box
					wid$.widgets[[i]][[j]] <- list(
						  type='entry', 
						  name=name,
						  "function"=widget[["function"]],
						  action=widget$action,
						  enter=widget$enter,
						  value=value,
						  width=widget$width,
						  mode=mode,
						  entryfont=widget$entryfont,
						  entrybg=widget$entrybg,
						  entryfg=widget$entryfg
					)
				}

				# ***untweak offset for special case of no labels
				if (all(rowlabels=="NULL"))
					j <- j + 1
				if (all(collabels[1]=="NULL"))
					i <- i + 1
			}
		}
	}

	#look out for a trailing list (only happens if rowlabels=NULL)
	i <- length(wid$.widgets)
	if (!length(wid$.widgets[[i]]))
		wid$.widgets[[i]] <- NULL

	#remove titles if applicable
	if (all(rowlabels=="NULL")) {
		wid$sidetitle <- ""
		wid$toptitle.offset <- NULL
	}
	if (all(collabels=="NULL")) {
		wid$toptitle <- ""
		wid$sidetitle.offset <- NULL
	}

	tkWidget <- .createWidget.grid(tk, wid, winName)
	return(tkWidget)
}


.createWidget.object <- function(tk, widget, winName)
{
	.dispError <- function(errorTxt)
	{
		wid <- list(type="label", 
		            text=errorTxt,
		            bg="white",
		            fg="red",
		            font="bold"
		            )
		return(.createWidget(tk, wid, winName))
	}

	if (!exists(widget$name, env = .GlobalEnv)) {
		return(.dispError(paste("Error: variable \"", widget$name, "\" could not be found.", sep="")))
	}


	userObject <- get(widget$name, pos=find(widget$name))

	#matrix
	if (is.matrix(userObject)) {
		wid <- list(type="matrix",
		            nrow=dim(userObject)[1],
		            ncol=dim(userObject)[2],
		            names=widget$name,
		            rowlabels=rownames(userObject),
		            collabels=colnames(userObject),
		            rownames=rownames(userObject),
		            colnames=colnames(userObject),
		            values=userObject,
		            byrow=FALSE,
		            font=widget$font,
		            fg=widget$fg,
		            bg=widget$bg,
		            entryfont=widget$entryfont,
		            entryfg=widget$entryfg,
		            entrybg=widget$entrybg,
		            "function"=widget[["function"]],
		            enter=widget$enter,
		            action=widget$action,
		            width=widget$width,
		            mode=mode(userObject),
		            sticky=widget$sticky,
		            padx=widget$padx,
		            pady=widget$pady
		            );
		.PBSmod[[winName]]$widgets[[widget$name]] <<- wid
		return(.createWidget(tk, wid, winName))
	}

	#data.frame
	if (is.data.frame(userObject)) {
		dataModes <- c()
		dataValues <- c()
		for(i in 1:length(userObject)) {
			dataModes <- c(dataModes, mode(as.vector(userObject[[i]])))
			for(v in as.vector(userObject[[i]]))
				dataValues <- c(dataValues, v)
		}
		
		wid <- list(type="data",
		            nrow=dim(userObject)[1],
		            ncol=dim(userObject)[2],
		            names=widget$name,
		            rowlabels=rownames(userObject),
		            collabels=colnames(userObject),
		            rownames=rownames(userObject),
		            colnames=colnames(userObject),
		            values=dataValues,
		            byrow=FALSE,
		            font=widget$font,
		            fg=widget$fg,
		            bg=widget$bg,
		            entryfont=widget$entryfont,
		            entryfg=widget$entryfg,
		            entrybg=widget$entrybg,
		            "function"=widget[["function"]],
		            enter=widget$enter,
		            action=widget$action,
		            width=widget$width,
		            modes=dataModes,
		            sticky=widget$sticky,
		            padx=widget$padx,
		            pady=widget$pady
		            );
		.PBSmod[[winName]]$widgets[[widget$name]] <<- wid
		return(.createWidget(tk, wid, winName))
	}

	#vector
	if (is.vector(userObject)) {
		wid <- list(type="vector",
		            names=widget$name,
		            length=length(userObject),
		            labels=names(userObject),
		            vecnames=names(userObject),
		            values=userObject,
		            font=widget$font,
		            fg=widget$fg,
		            bg=widget$bg,
		            entryfont=widget$entryfont,
		            entryfg=widget$entryfg,
		            entrybg=widget$entrybg,
		            vertical=widget$vertical,
		            "function"=widget[["function"]],
		            enter=widget$enter,
		            action=widget$action,
		            width=widget$width,
		            mode=mode(userObject),
		            sticky=widget$sticky,
		            padx=widget$padx,
		            pady=widget$pady
		            );
		.PBSmod[[winName]]$widgets[[widget$name]] <<- wid
		return(.createWidget(tk, wid, winName))
	}

	return(.dispError(paste("Error: variable \"", widget$name, "\" is an incompatible mode.", sep="")))
}


.createWidget.entry <- function(tk, widget, winName)
{
	if (!is.null(widget$label))
	if (widget$label!="") {
		#if label is set, then create a 2x1 grid
		label <- widget$label
		widget$label <- "" #blank it out, inf loop if not.
		newgridwidget <-
		list(type="grid", nrow=1, ncol=2, font="", byrow=TRUE, borderwidth=1, relief="flat", padx=0, pady=0, fg=widget$fg, bg=widget$bg, .widgets=
			list(
				list(
					list(type="label", text=label, padx=0, pady=0, font=widget$font, fg=widget$fg, bg=widget$bg),
					widget
				)
			)
		)
		return(.createWidget.grid(tk, newgridwidget, winName))
	}
	#create real tk widget below
	argList <- list(parent=tk)
	if (!is.null(widget$entryfg) && widget$entryfg!="")
		argList$foreground=widget$entryfg
	if (!is.null(widget$entrybg) && widget$entrybg!="")
		argList$background=widget$entrybg
	if (!is.null(widget$entryfont) && widget$entryfont!="")
		argList$font <- .createTkFont(widget$entryfont)
	argList$textvariable<-.map.add(winName, widget$name, tclvar=tclVar(widget$value))$tclvar
	argList$width<-widget$width
	tkWidget<-do.call("tkentry", argList)

	enter <- !is.null(widget$enter)
	if (enter)
		enter <- widget$enter
	if (enter) {
		#dont update it (unless an return was pressed) as it can slow it down a lot
		tkbind(tkWidget,"<KeyPress-Return>",function(...) { .extractData(widget[["function"]], widget$action, winName)});
	}
	else
		tkbind(tkWidget,"<KeyRelease>",function(...) { .extractData(widget[["function"]], widget$action, winName)});
	return(tkWidget)
}

.createWidget.radio <- function(tk, widget, winName)
{
	argList <- list(parent=tk, text=widget$text, value=widget$value)
	if (!is.null(widget$fg) && widget$fg!="")
		argList$foreground=widget$fg
	if (!is.null(widget$bg) && widget$bg!="")
		argList$background=widget$bg
	if (!is.null(widget$font) && widget$font!="")
		argList$font <- .createTkFont(widget$font)
	argList$variable<-.map.add(winName, widget$name, tclvar=tclVar(widget$value))$tclvar
	if (!is.null(widget$selected) && widget$selected==TRUE)
		tclvalue(argList$variable) <- widget$value
	argList$command=function(...) { .extractData(widget[["function"]], widget$action, winName)}

	tkWidget<-do.call("tkradiobutton", argList)
	return(tkWidget)
}

.createWidget.slide <- function(tk, widget, winName)
{
	if (is.null(widget$value))
		widget$value <- widget$to
	argList <- list(parent=tk, from=widget$from, to=widget$to, orient=widget$orientation, showvalue=widget$showvalue)
	if (!is.null(widget$fg) && widget$fg!="")
		argList$foreground<-widget$fg
	if (!is.null(widget$bg) && widget$bg!="")
		argList$background<-widget$bg
	if (!is.null(widget$font) && widget$font!="")
		argList$font <- .createTkFont(widget$font)
	argList$variable<-.map.add(winName, widget$name, tclvar=tclVar(widget$value))$tclvar
	argList$command<-function(...) { .extractData(widget[["function"]], widget$action, winName)}
	tkWidget<-do.call("tkscale", argList)
	return(tkWidget)
}

.createWidget.slideplus <- function(tk, widget, winName)
{
	#initial widget$value defaults to <from> argument
	if (is.na(widget$value))
		widget$value <- widget$from

	#to remember last valid number
	lastMinVal <- ""
	lastCurVal <- ""
	lastMaxVal <- ""

	#command to update min/max changes
	updateSlideBounds <- function(slider, slideVar, curVar, minVar, maxVar, widget, winName)
	{
		minVal <- tclvalue(minVar)
		curVal <- tclvalue(curVar)
		maxVal <- tclvalue(maxVar)

		#change min
		if (any(grep("^-?(([0-9]+(\\.[0-9]*)?)|([0-9]*\\.[0-9]+))$",minVal))) {
			minVal<-as.numeric(minVal)
			tkconfigure(slider,from=minVal/widget$by);
			tclvalue(minVar)<-minVal
		}
		else {
			#reset min to the last valid "-from" parameter of the slider
			if (!any(grep("^-?\\.?$",minVal)))
				tclvalue(minVar)<-lastMinVal 

		}

		#change max
		if (any(grep("^-?(([0-9]+(\\.[0-9]*)?)|([0-9]*\\.[0-9]+))$",maxVal))) {
			maxVal<-as.numeric(maxVal)
			tkconfigure(slider,to=maxVal/widget$by);
			tclvalue(maxVar)<-maxVal
		}
		else {
			#reset max to the last valid "-to" parameter of the slider
			if (!any(grep("^-?\\.?$",maxVal)))
				tclvalue(maxVar)<-lastMaxVal
		}

		#change current
		if (any(grep("^-?(([0-9]+(\\.[0-9]*)?)|([0-9]*\\.[0-9]+))$",curVal))) {
			tclvalue(slideVar)<-round(as.numeric(curVal)/widget$by)
			.extractData(widget[["function"]], widget$action, winName)
		}
		else {
			#reset max to the last valid "-to" parameter of the slider
			if (!any(grep("^-?\\.?$",maxVal)))
				tclvalue(maxVar)<-lastMaxVal
		}
		if (!any(grep("^-?[0-9]*(\\.[0-9]*$)?",curVal))) {
			tclvalue(curVar)<-lastCurVal
		}
	}

	saveSlideBounds <- function(slider, curVar, minVar, maxVar)
	{
		minVal <<- tclvalue(minVar)
		curVal <<- tclvalue(curVar)
		maxVal <<- tclvalue(maxVar)

		if (any(grep("^-?[0-9]*$",minVal)))
			lastMinVal <<- minVal
		if (any(grep("^-?[0-9]*$",curVal)))
			lastCurVal <<- curVal
		if (any(grep("^-?[0-9]*$",maxVal)))
			lastMaxVal <<- maxVal
	}

	convertCurVal <- function(widget, slideVar, curVar)
	{
		tclvalue(curVar) <- as.numeric(tclvalue(slideVar))*widget$by
	}

	#calculate fractional values
	from <- widget$from / widget$by
	to <- widget$to / widget$by

	if (is.null(widget$value)) {
		value <- to
		widget$value <- widget$to
	}
	else {
		value <- widget$value / widget$by
	}

	curVar<-.map.add(winName, widget$name, tclvar=tclVar(widget$value))$tclvar #this one is the fractional value
	slideVar<-.map.add(winName, paste(".", widget$name, ".slide", sep=""), tclvar=tclVar(value))$tclvar #integer
	minVar<-.map.add(winName, paste(widget$name, ".min", sep=""), tclvar=tclVar(widget$from))$tclvar
	maxVar<-.map.add(winName, paste(widget$name, ".max", sep=""), tclvar=tclVar(widget$to))$tclvar

	#hold the widgets in this frame
	tkWidget <- tkframe(tk)

	slider <- tkscale(tkWidget, from=from, to=to, orient="horizontal", showvalue=FALSE, variable=slideVar, command=function(...) { convertCurVal(widget, slideVar, curVar); .extractData(widget[["function"]], widget$action, winName)})

	#insert slider
	tkgrid(slider, columnspan=5, row=1, column=1)

	#create entries
	#
	minWid <- tkentry(tkWidget,textvariable=minVar, width=5)
	curWid <- tkentry(tkWidget,textvariable=curVar, width=5)
	maxWid <- tkentry(tkWidget,textvariable=maxVar, width=5)

	if (widget$enter) {
		tkbind(minWid,"<KeyRelease-Return>",function() updateSlideBounds(slider, slideVar, curVar, minVar, maxVar, widget, winName));
		tkbind(maxWid,"<KeyRelease-Return>",function() updateSlideBounds(slider, slideVar, curVar, minVar, maxVar, widget, winName));
		tkbind(curWid,"<KeyRelease-Return>",function() updateSlideBounds(slider, slideVar, curVar, minVar, maxVar, widget, winName));
	}
	else {
		#capture value before press (incase new value isnt valid)
		tkbind(minWid,"<KeyPress>",function() saveSlideBounds(slider, curVar, minVar, maxVar));
		tkbind(maxWid,"<KeyPress>",function() saveSlideBounds(slider, curVar, minVar, maxVar));
		tkbind(curWid,"<KeyPress>",function() saveSlideBounds(slider, curVar, minVar, maxVar));

		#capture value after key is received
		tkbind(minWid,"<KeyRelease>",function() updateSlideBounds(slider, slideVar, curVar, minVar, maxVar, widget, winName));
		tkbind(maxWid,"<KeyRelease>",function() updateSlideBounds(slider, slideVar, curVar, minVar, maxVar, widget, winName));
		tkbind(curWid,"<KeyRelease>",function() updateSlideBounds(slider, slideVar, curVar, minVar, maxVar, widget, winName));
	}

	#bind functions for setwinval() changes
	.map.set(winName, paste(widget$name, ".min", sep=""), onChange=function() updateSlideBounds(slider, slideVar, curVar, minVar, maxVar, widget, winName))
	.map.set(winName, paste(widget$name, ".max", sep=""), onChange=function() updateSlideBounds(slider, slideVar, curVar, minVar, maxVar, widget, winName))
	.map.set(winName, widget$name, onChange=function() updateSlideBounds(slider, slideVar, curVar, minVar, maxVar, widget, winName))


	#place widgets in grid
	tkgrid(tklabel(tkWidget, text="Min->"), row=2, column=1)
	tkgrid(minWid, row=2, column=2)
	tkgrid(curWid, row=2, column=3)
	tkgrid(maxWid, row=2, column=4)
	tkgrid(tklabel(tkWidget, text="<-Max"), row=2, column=5)

	return(tkWidget)
}

.createWidget.button <- function(tk, widget, winName)
{
	param <- list(parent=tk, text=widget$text)
	if (widget$font != "")
		param$font=.createTkFont(widget$font)
	if (!is.null(widget$fg) && widget$fg!="")
		param$foreground=widget$fg
	if (!is.null(widget$bg) && widget$bg!="")
		param$background=widget$bg
	if (is.numeric(widget$width) && widget$width > 0)
		param$width=widget$width
	if (widget[["function"]]!="")
		param$command=function(...) { .extractData(widget[["function"]], widget$action, winName) }
	return(do.call(tkbutton, param))
}

.createWidget.text <- function(tk, widget, winName)
{
	tk <- tkframe(tk)

	param <- list(
	              parent=tk, 
	              bg=widget$bg, 
	              fg=widget$fg, 
	              height=widget$height, 
	              width=widget$width,
	              relief=widget$relief,
	              yscrollcommand=function(...)tkset(scrollBar,...)
	              )
	if (widget$font != "")
		param$font=.createTkFont(widget$font)

	scrollBar <- tkscrollbar(tk, repeatinterval=5, command=function(...)tkyview(txtBox,...))
	txtBox <- do.call(tktext, param)

	.map.add(winName, widget$name, tclwidget=txtBox)
	tkinsert(txtBox,"end",widget$value)

	if (widget$edit==FALSE)
		tkconfigure(txtBox, state="disabled")

	if (widget$scrollbar == TRUE) {
		tkgrid(txtBox,scrollBar)
		tkgrid.configure(scrollBar,sticky="ns")
	} else {
		tkgrid(txtBox) }

	return(tk)
}

#history widget creates a bunch of standard PBS widgets, which call the appropriate functions
#as if a power-user created history
.createWidget.history <- function(tk, widget, winName)
{
	indexname=paste("PBS.history.", widget$name, ".index", sep="") #widget name that stores/displays the index number
	sizename=paste("PBS.history.", widget$name, ".size", sep="") #widget name that displays the size of history
	modename=paste("PBS.history.", widget$name, ".mode", sep="") #widget name that displays the size of history

	widget$name <- paste(winName, widget$name, sep=".")

	#initialize a list to be used once the window is created
	initHistory(widget$name, indexname=indexname, sizename=sizename, modename=modename, func=widget[["function"]])

	historyGrid <- 
	list(type="grid", nrow=2, ncol=1, font="", byrow=TRUE, borderwidth=1, relief="sunken", padx=widget$padx, pady=widget$pady, .widgets=
		list(
			list(
				list(type="grid", nrow=3, ncol=4, font="", byrow=TRUE, borderwidth=1, relief="flat", padx=0, pady=0, sticky="we", .widgets=
			    	list(
						list(
							list(type="button", text="<<", font="", width=5, "function"="firstHistory", action=widget$name, sticky="", padx=0, pady=0),
							list(type="button", text="<", font="", width=5, "function"="backHistory", action=widget$name, sticky="", padx=0, pady=0),
							list(type="button", text=">", font="", width=5, "function"="forwHistory", action=widget$name, sticky="", padx=0, pady=0),
							list(type="button", text=">>", font="", width=5, "function"="lastHistory", action=widget$name, sticky="", padx=0, pady=0)
						),
						list(
							#list(type="label", text="Index", font="", sticky="", padx=0, pady=0),
							list(type="button", text="Sort", font="", width=5, "function"=".sortActHistory", action=widget$name, sticky="", padx=0, pady=0),
							list(type="entry", name=indexname, value="0", width=5, label="", font="", "function"="jumpHistory", action=widget$name, enter=TRUE, mode="numeric", padx=0, pady=0, entrybg="white"),
							list(type="entry", name=sizename, value="0", width=5, label="", font="", action="", enter=TRUE, mode="numeric", padx=0, pady=0),
							list(type="button", text="Empty", font="", width=5, "function"="clearHistory", action=widget$name, sticky="", padx=0, pady=0)
						),
						list(
							list(type="button", text="Insert", font="", width=5, "function"="addHistory", action=widget$name, sticky="", padx=0, pady=0),
							list(type="button", text="Delete", font="", width=5, "function"="rmHistory", action=widget$name, sticky="", padx=0, pady=0),
							list(type="button", text="Import", font="", width=5, "function"="importHistory", action=widget$name, sticky="", padx=0, pady=0),
							list(type="button", text="Export", font="", width=5, "function"="exportHistory", action=widget$name, sticky="", padx=0, pady=0)
						)
					)
				)
			),
			list(
				#list(type="grid", nrow=2, ncol=2, font="", byrow=TRUE, borderwidth=1, relief="raised", padx=0, pady=0, sticky="we", .widgets=
				#	list(
				#		list(
				#			list(type="radio", name=modename, value="b", text="Insert Before", font="7", "function"="", action=widget$name, mode="character", sticky="w", padx=0, pady=0),
				#			list(type="radio", name=modename, value="o", text="Overwrite", font="7", "function"="", action=widget$name, mode="character", sticky="w", padx=0, pady=0)
				#		),
				#		list(
				#			list(type="radio", name=modename, value="a", selected=TRUE, text="Insert After", font="7", "function"="", action=widget$name, mode="character", sticky="w", padx=0, pady=0),
				#			list(type="null", sticky="", padx=0, pady=0)
				#		)
				#	)
				#)
				list(type="grid", nrow=2, ncol=2, font="", byrow=TRUE, borderwidth=1, relief="raised", padx=0, pady=0, sticky="we", .widgets=
					list(
						list(
							list(type="radio", name=modename, value="b", text="before", font="7", "function"="", action=widget$name, mode="character", sticky="w", padx=0, pady=0),
							list(type="radio", name=modename, value="a", selected=TRUE, text="after", font="7", "function"="", action=widget$name, mode="character", sticky="w", padx=0, pady=0),
							list(type="radio", name=modename, value="o", text="ovr", font="7", "function"="", action=widget$name, mode="character", sticky="w", padx=0, pady=0)
						)
					)
				)
			)
		)
	)
		
		
	tmp <- .createWidget.grid(tk, historyGrid, winName)
	if (widget$import!="")
		importHistory(widget$name, widget$import, FALSE)
	return(tmp)
}


# ***********************************************************
# backHistory:
#   move back in history
# Arguments:
#   hisname   - history instance name if multiple are active
# -----------------------------------------------------------
backHistory <- function(hisname="")
{
	if (hisname=="") 
		hisname <- getWinAct()[1]
	win <- strsplit(hisname, "\\.")[[1]][1]

	if (!is.list(PBS.history)) 
		stop("History not intialized - see initHistory function help")
	if (!is.list(PBS.history[[hisname]])) 
		stop(paste("History \"", hisname,"\" not intialized - see initHistory", sep=""))

	i <- PBS.history[[hisname]][[1]]$index
	if (i < 2) {
		#cat("history widget: warning, current position is already at front of history list.\n")
		return()
	}
	PBS.history[[hisname]][[1]]$index <<- i <- i-1
	setWinVal(PBS.history[[hisname]][[i+1]], winName=win) #i is always one lower
	.updateHistory(hisname)
	if (!is.null(PBS.history[[hisname]][[1]]$func))
		do.call(PBS.history[[hisname]][[1]]$func, list())
}


# ***********************************************************
# forwHistory:
#   move forward in history
# Arguments:
#   hisname   - history instance name if multiple are active
# -----------------------------------------------------------
forwHistory <- function(hisname="")
{
	if (hisname=="") 
		hisname <- getWinAct()[1]
	win <- strsplit(hisname, "\\.")[[1]][1]
	
	if (!is.list(PBS.history)) 
		stop("History not intialized - see initHistory")
	if (!is.list(PBS.history[[hisname]])) 
		stop(paste("History \"", hisname,"\" not intialized - see initHistory", sep=""))

	i <- PBS.history[[hisname]][[1]]$index
	if (i >= (length(PBS.history[[hisname]])-1)) {
		#cat("history widget: warning, current position is already at end of history list.\n")
		return()
	}
	PBS.history[[hisname]][[1]]$index <<- i <- i+1
	setWinVal(PBS.history[[hisname]][[i+1]], winName=win) #i is always one lower
	.updateHistory(hisname)
	if (!is.null(PBS.history[[hisname]][[1]]$func))
		do.call(PBS.history[[hisname]][[1]]$func, list())
}


# ***********************************************************
# lastHistory:
#   move to last history slide
# Arguments:
#   hisname   - history instance name if multiple are active
# -----------------------------------------------------------
lastHistory <- function(hisname="")
{
	jumpHistory(hisname, length(PBS.history[[hisname]])-1)
}


# ***********************************************************
# lastHistory:
#   move to last history slide
# Arguments:
#   hisname   - history instance name if multiple are active
# -----------------------------------------------------------
firstHistory <- function(hisname="")
{
	jumpHistory(hisname, 1)
}


# ***********************************************************
# jumpHistory:
#   need history name
#   and what index to jump to - or what entry to pull it out of
# Arguments:
#   hisname   - history instance name if multiple are active
# -----------------------------------------------------------
jumpHistory <- function(hisname="", index="")
{
	if (hisname=="") 
		hisname <- getWinAct()[1]
	win <- strsplit(hisname, "\\.")[[1]][1]

	if (!is.list(PBS.history)) 
		stop("History not intialized - see initHistory")
	if (!is.list(PBS.history[[hisname]])) 
		stop(paste("History \"", hisname,"\" not intialized - see initHistory", sep=""))

	if (is.numeric(index))
		i <- index
	else if (index=="")
		i <- as.numeric(getWinVal(PBS.history[[hisname]][[1]]$indexname))
	else
		i <- as.numeric(getWinVal(index))

	if (i > length(PBS.history[[hisname]])-1 || i <= 0) {
		cat("Error: history index is out of bounds.\n")
		return()
	}

	PBS.history[[hisname]][[1]]$index <<- i #update index
	setWinVal(PBS.history[[hisname]][[i+1]], winName=win) #i is always one lower
	.updateHistory(hisname)
	if (!is.null(PBS.history[[hisname]][[1]]$func))
		do.call(PBS.history[[hisname]][[1]]$func, list())
}


# ***********************************************************
# addHistory:
#   save history
# Arguments:
#   hisname   - history instance name if multiple are active
# -----------------------------------------------------------
addHistory <- function(hisname="")
{
	if (hisname=="") 
		hisname <- getWinAct()[1]

	if (!is.list(PBS.history)) 
		stop("History not intialized - see initHistory")
	if (!is.list(PBS.history[[hisname]])) 
		stop(paste("History \"", hisname,"\" not intialized - see initHistory", sep=""))

	x <- PBS.history[[hisname]] #old history
	itemLen <- length(x)-1 #don't count header
	index <- PBS.history[[hisname]][[1]]$index  #make it a real index
	insertMode <- getWinVal(PBS.history[[hisname]][[1]]$modename)[[PBS.history[[hisname]][[1]]$modename]]
	
	if (is.null(insertMode) || index==0) {
		insertMode <- "a"
	}

	if (insertMode=="a") {
		#insert to the right of current index
		PBS.history[[hisname]][[index+2]] <<- getWinVal()
		if (index<itemLen) {
			for(i in (index+2):(itemLen+1)) {
				PBS.history[[hisname]][[i+1]] <<- x[[i]]
			}
		}
		#point index to inserted pos
		PBS.history[[hisname]][[1]]$index <<- index+1
	}
	else if (insertMode=="b") {
		#insert to the left of current index
		PBS.history[[hisname]][[index+1]] <<- getWinVal()
		for(i in (index+1):(itemLen+1)) {
			PBS.history[[hisname]][[i+1]] <<- x[[i]]
		}
	}
	else if (insertMode=="o") {
		#overwrite the current index
		PBS.history[[hisname]][[index+1]] <<- getWinVal()
	}
	else {
		stop(paste("unknown insert mode:", insertMode))
	}
	.updateHistory(hisname)
}


# ***********************************************************
# rmHistory:
#   if index is numeric - delete history in that spot
#   else delete the history where the current index points to 
#   (and not the value of the current index box - as a user might not have pushed enter)
# Arguments:
#   hisname   - history instance name if multiple are active
# -----------------------------------------------------------
rmHistory <- function(hisname="", index="")
{
	if (hisname=="") 
		hisname <- getWinAct()[1]
	win <- strsplit(hisname, "\\.")[[1]][1]

	if (!is.list(PBS.history)) 
		stop("History not intialized - see initHistory")
	if (!is.list(PBS.history[[hisname]])) 
		stop(paste("History \"", hisname,"\" not intialized - see initHistory", sep=""))

	if (is.numeric(index))
		i <- index
	else
		i <- PBS.history[[hisname]][[1]]$index

	if (length(PBS.history[[hisname]]) == 1) {
		cat("History list is already empty.\n")
		return()
	}

	PBS.history[[hisname]] <<- PBS.history[[hisname]][-(i+1)]
	#change index if it was the last element
	if (i > length(PBS.history[[hisname]])-1)
		PBS.history[[hisname]][[1]]$index <<- length(PBS.history[[hisname]])-1 #set index to size

	#change values to current index
	i <- PBS.history[[hisname]][[1]]$index
	if (i > 0) {
		setWinVal(PBS.history[[hisname]][[i+1]], winName=win) #i is always one lower
		if (!is.null(PBS.history[[hisname]][[1]]$func))
			do.call(PBS.history[[hisname]][[1]]$func, list())
	}

	.updateHistory(hisname)
}


# ***********************************************************
# clearHistory:
#   remove all history elements from
# Arguments:
#   hisname   - history instance name if multiple are active
# -----------------------------------------------------------
clearHistory <- function(hisname="")
{
	if (hisname=="") 
		hisname <- getWinAct()[1]

	if (!is.list(PBS.history)) 
		stop("History not intialized - see initHistory")
	if (!is.list(PBS.history[[hisname]])) 
		stop(paste("History \"", hisname,"\" not intialized - see initHistory", sep=""))

	tmp <- PBS.history[[hisname]][[1]]
	tmp$index = 0
	PBS.history[[hisname]] <<- list(0)
	PBS.history[[hisname]][[1]] <<- tmp

#	len <- length(PBS.history[[hisname]])
#	if (len > 1) {
#		for(i in 2:len) {
			#PBS.history[[hisname]][[i]] <<- NULL #something weird is happening here
#			rmHistory(hisname)
#		}
#	}

	.updateHistory(hisname)
}


# ***********************************************************
# .updateHistory:
#   update widget values
# Arguments:
#   hisname   - history instance name if multiple are active
# -----------------------------------------------------------
.updateHistory <- function(hisname)
{
	indexname <- PBS.history[[hisname]][[1]]$indexname
	sizename  <- PBS.history[[hisname]][[1]]$sizename
	x<-list()

	if (!is.null(indexname))
		x[[indexname]]<-PBS.history[[hisname]][[1]]$index

	if (!is.null(sizename))
		x[[sizename]]<-length(PBS.history[[hisname]])-1

	win <- strsplit(hisname, "\\.")[[1]][1]

	setWinVal(x, winName=win)
}

# ***********************************************************
# initHistory:
#   setup the History "list"
# Arguments:
#   hisname   - history instance name if multiple are active
#   indexname - customized index widget name
#   sizename  - customized size widget name
#   overwrite - retain old history?
# -----------------------------------------------------------
initHistory <- function(hisname, indexname=NULL, sizename=NULL, modename=NULL, func=NULL, overwrite=TRUE)
{

	if (!exists("PBS.history", env = .GlobalEnv))
		PBS.history <- list()
	else
		PBS.history <- get("PBS.history", env = .GlobalEnv)

	if (func=="")
		func <- NULL
	if (!is.null(func)) {
		if (!exists(func,mode="function")) {
			cat(paste("Warning: cannot find function '", func, "'.\n", sep=""))
			func <- NULL
		}
	}

	if (!is.list(PBS.history))
		assign("PBS.history", list(), env = .GlobalEnv)

	if (!is.list(PBS.history[[hisname]]) || overwrite) {
		PBS.history[[hisname]] <- list(0)
		PBS.history[[hisname]][[1]] <- list(index=0) #the first element is the index, all other elements are history items
	}
	#save names of entry boxes
	PBS.history[[hisname]][[1]]$indexname <- indexname
	PBS.history[[hisname]][[1]]$sizename <- sizename
	PBS.history[[hisname]][[1]]$modename <- modename
	PBS.history[[hisname]][[1]]$func <- func
	assign("PBS.history", PBS.history, env = .GlobalEnv)
}


# ***********************************************************
# exportHistory:
#   save PBS history to a file
# Arguments:
#   hisname - history instance name if multiple are active
#   fname   - initial filename to save under
# -----------------------------------------------------------
exportHistory <- function(hisname="", fname="")
{
	if (hisname=="") hisname <- getWinAct()[1]

	if (!is.list(PBS.history[[hisname]]))
		stop("unable to export history. Incorect history name given.")

	if (fname=="")
		fname <- promptSaveFile(initialfile=paste(hisname,".History.r", sep=""))
	if (fname=="")
		stop("no filename given.")

	x=PBS.history[[hisname]]
	x[[1]] <- NULL #remove history widget info
	writeList(x, fname)
}


# ***********************************************************
# importHistory:
#   import PBS history from a file
# Arguments:
#   hisname - history instance name if multiple are active
#   fname   - initial filename to open from
# -----------------------------------------------------------
importHistory <- function(hisname="", fname="", updateHis=TRUE)
{
	if (hisname=="") hisname <- getWinAct()[1]
	win <- strsplit(hisname, "\\.")[[1]][1]
	
	if (!is.list(PBS.history[[hisname]]))
		stop("unable to import history. Incorect history name given.")

	if (fname=="")
		fname <- promptOpenFile()
	if (fname=="")
		stop("no filename given.")

	newHist <- readList(fname)

	insertMode <- getWinVal(PBS.history[[hisname]][[1]]$modename)[[PBS.history[[hisname]][[1]]$modename]]

	a <- PBS.history[[hisname]]
	PBS.history[[hisname]] <<- list()
	index <- max(0, min(a$index, length(a)-1))
	if (insertMode!="b" || index==0)
		index <- index + 1
	i <- 1

	repeat {
		if( !length(a) && !length(newHist) )
			break
		if( i > index && length(newHist) ) {
			PBS.history[[hisname]][[i]] <<- newHist[[1]]
			newHist[[1]] <- NULL
		} else {
			PBS.history[[hisname]][[i]] <<- a[[1]]
			a[[1]] <- NULL
		}
		i <- i + 1
	}
	
	PBS.history[[hisname]][[1]]$index <<- 1

	#update with new history settings
	if (updateHis)
		jumpHistory(hisname, index)

	return(invisible(PBS.history[[hisname]]))
}


# ***********************************************************
# func: sortHistory (and helpers)
# -----------------------------------------------------------
.updateFile <- function()
{
	act <- getWinAct()[1]
	if (act=="open") {
		f <- promptOpenFile()
		s <- getWinVal("savefile")$savefile
		if (s=="")
			setWinVal(list(savefile=f))
		setWinVal(list(openfile=f))
	} else if (act=="save") {
		f <- promptSaveFile()
		setWinVal(list(savefile=f))
	}
}
.sortHelperActive <- function(hisname)
{
	len <- length(PBS.history[[hisname]]) - 1
	if (len < 1) stop("unable to sort empty history")
	x <- data.frame(new = 1:len)
	x <- fix(x); xnew <- order(x$new, na.last=NA);
	tmp <- PBS.history[[hisname]]
	j <- 2
	PBS.history[[hisname]] <<- list(PBS.history[[hisname]][[1]])
		for (i in xnew) {
		if (!is.na(i)) {
			PBS.history[[hisname]][[j]] <<- tmp[[i + 1]]
			j <- j + 1
		}
	}
	PBS.history[[hisname]][[1]]$index <<- min(PBS.history[[hisname]][[1]]$index, length(PBS.history[[hisname]])-1)
	.updateHistory(hisname)
}
.sortHelperFile <- function(openfile, savefile)
{
	inHis <- readList(openfile)
	len <- length(inHis) - 1
	if (len < 1) stop("unable to sort empty history")
	x <- data.frame(new = 1:len)
	x <- fix(x); xnew <- order(x$new, na.last=NA);
	
	outHis <- list(inHis[[1]])
	j <- 2
	for (i in xnew) {
		if (!is.na(i)) {
			outHis[[j]] <- inHis[[i + 1]]
			j <- j + 1
		}
	}
	writeList(outHis, savefile)
}
.sortHelper <- function()
{
	act <- getWinAct()[1]
	if (act=="active") {
		hisname <- getWinVal("hisname")$hisname
		.sortHelperActive(hisname)
	} else if (act=="file") {
		openfile <- getWinVal("openfile")$openfile
		savefile <- getWinVal("savefile")$savefile
		.sortHelperFile(openfile, savefile)
	}
	sortHistory()
}
#use window action as history name
.sortActHistory <- function()
{
	sortHistory(hisname=getWinAct()[1])
}
sortHistory <- function(file="",outfile=file,hisname="")
{
	if (file!="") {
		return(.sortHelperFile(file, outfile))
	}
	if (hisname!="") {
		return(.sortHelperActive(hisname))
	}
	currHist <- NULL
	if (exists("PBS.history"))
		currHist <- names(PBS.history)
	if (!is.null(currHist)) {
		radios <- list(list(type="label", text="Select an active window history to sort", sticky="w", padx=12))
		i <- 2
		for(h in currHist) {
			len <- length(PBS.history[[h]])-1
			if (len==1)
				items <- "(1 item)"
			else
				items <- paste("(", len, " items)", sep="")
			radios[[i]] <- list(type="radio",
			                    name="hisname",
			                    value=h,
			                    text=paste(h, items),
			                    mode="character",
			                    sticky="w",
			                    padx=12)
			i <- i+1
		}
		radios[[i]] <- list(type="button", "function"=".sortHelper", action="active", text="Sort Active History", sticky="w", padx=12)
	} 
	else {
		radios <- list(list(type="label", text="No active history widgets could be found.\nTry creating a window with a history widget.", sticky="w", padx=12))
	}
	win <- list(title = "Sort History",
	            windowname = "sort.History",
	            .widgets = c(
	                list(
	                  list(type="label", text="Active History                                            ", font="bold underline", fg="red3", sticky="w")
	                ),
	                radios,
	                list(
	                  list(type="null"),
	                  list(type="label", text="Saved History                                            ", font="bold underline", fg="red3", sticky="w"),
	                  list(type="label", text="Open a saved history file to sort and select a file \nto save to. (which can be the same file)", sticky="w", padx=12),
                         list(type = "grid", .widgets = list(
                           list(
                             list(type="entry", name="openfile", sticky="e", padx=12, width=30, mode="character"),
                             list(type="button", "function"=".updateFile", action="open", text="Open From", sticky="w", width=10)
                           ),
                           list(
                             list(type="entry", name="savefile", sticky="e", padx=12, width=30, mode="character"),
                             list(type="button", "function"=".updateFile", action="save", text="Save To", sticky="w", width=10)
                           ))),
                         list(type="button", "function"=".sortHelper", action="file", text="Sort Saved History", sticky="w", padx=12)
                       )
	            ))
	createWin(list(win))
}




# ***********************************************************
# func:
#   get a list of called functions
# Arguments:
#   data - widget lists
# -----------------------------------------------------------
.extractFuns <- function(data)
{
	retData <- c()
	for(i in 1:length(data)) {
		if (!is.null(data[[i]][["widget"]][["function"]]))
			retData <- c(retData, data[[i]][["widget"]][["function"]])
	}
	return(retData)
}


# ***********************************************************
# .extractData:
#   called by TK on button presses (or binds, onchanges, slides, ...)
# Arguments:
#   command - user command to call (ie function argument of widget)
#   action  - action value
#   winName - active window name
# -----------------------------------------------------------
.extractData <- function(command, action, winName)
{
	.PBSmod$.activeWin <<- winName

	setWinAct(winName, action)

	if (is.null(command))
		return()

	if (command=="")
		return()

	if (exists(command,mode="function"))
		do.call(command, list())
	else
		cat(paste("Warning: cannot find function '", command, "'.\n", sep=""))
}


getWinAct <- function(winName)
{
	if (!exists(".PBSmod")) {
		stop(".PBSmod was not found")
	}
	if (missing(winName))
		winName <- .PBSmod$.activeWin
	return(.PBSmod[[winName]]$action)
}


setWinAct <- function(winName, action)
{
	if (is.null(action))
		return()

	if (length(.PBSmod[[winName]]$actions) >= .maxActionSize)
		.PBSmod[[winName]]$actions <<- .PBSmod[[winName]]$actions[1:(.maxActionSize-1)]
	.PBSmod[[winName]]$actions <<- c(action, .PBSmod[[winName]]$actions)
}


getWinFun <- function(winName)
{
	if (!exists(".PBSmod")) {
		stop(".PBSmod was not found")
	}
	if (missing(winName))
		winName <- .PBSmod$.activeWin
	return(.PBSmod[[winName]]$functions)
}


# ***********************************************************
# setWinVal:
#   updates a widget with a new value
# Arguments:
#   vars       - named list or vector specifying new values
#   winName - which window to update if multiple are active
# -----------------------------------------------------------
setWinVal <- function(vars, winName="")
{
	if (winName=="")
		winName <- .PBSmod$.activeWin
	if (.isReallyNull(.PBSmod, winName))
		stop(paste("unable to find .PBSmod$", winName))

	if (!length(vars))
		return(vars)

	name <- names(vars)
	for(i in 1:length(vars)) {

		if (is.list(vars))
			.setWinValHelper(name[i], vars[[i]], winName)
		else if (is.vector(vars))
			.setWinValHelper(name[i], vars[i], winName)
	}
}

.setWinValHelper <- function(varname, value, winName)
{
	x<-.map.get(winName, varname)
	wid<-.PBSmod[[winName]]$widgets[[varname]]

	#if tclvar is known, we can set it directly.
	if (!is.null(x$tclvar)) {

		#value should only be length 1
		if (length(value)!=1)
			stop(paste('unable to set "', varname, '": value given should be of length 1. given length: ',length(value), sep=""))

		if (is.na(value))
			value <- ""
		else if (!is.logical(value))
			value <- as.character(value)

		tclvalue(x$tclvar) <- value

		#some widgets must update other widgets(or functions) when they change
		if (!is.null(x$onChange)) {
			if (is.function(x$onChange)) {
				do.call(x$onChange, list())
			}
			if (is.character(x$onChange)) #function names are accepted as strings
				if (exists(x$onChange,mode="function"))
					do.call(x$onChange, list())
		}
		return(value)
	}

	#otherwise if tclwidget is known (only text widget)
	else if (!is.null(x$tclwidget)) {
		#special case for text boxes
		if (wid$type=="text") {
			if (wid$edit==FALSE)
				tkconfigure(x$tclwidget, state="normal")

			if (length(value)>1)
				value = paste(value,collapse="\n")

			tkdelete(x$tclwidget, "0.0", "end") #clear text widget
			tkinsert(x$tclwidget,"0.0",value) #update

			if (wid$edit==FALSE)
				tkconfigure(x$tclwidget, state="disabled")
			return(value)
		}
		stop(paste("unhandled widget type", x$tclwidget))
	}

	#catch any special "high level" widgets that do not have
	#tclvar or tclwidget. If however no wid is defined, we are doomed
	if (is.null(wid)) {
		stop(paste('unable to set "', varname, '": not found.', sep=""))
	}

	#special case for matrix
	if (wid$type=="matrix") {
		if (length(wid$names)==1) {
			if (!is.matrix(value))
				stop(paste('unable to set "', varname, '": supplied value is not a matrix.', sep=""))
			for(i in 1:nrow(value))
				for(j in 1:ncol(value))
					.setWinValHelper(paste(varname,"[",i,",",j,"]",sep=""), value[i,j], winName)
			return(value)
		}
	}

	#special case for data
	if (wid$type=="data") {
		if (length(wid$names)==1) {
			#todo: if not a data.frame
			#	stop(paste('unable to set "', varname, '": supplied value is not a dataframe.', sep=""))
			for(i in 1:nrow(value))
				for(j in 1:ncol(value))
					.setWinValHelper(paste(varname,"[",i,",",j,"]d",sep=""), value[i,j], winName)
			return(value)
		}
	}

	#special case for vector
	if (wid$type=="vector") {
		if (length(wid$names)==1) {
			if (length(value)!=wid$length)
				stop(paste('unable to set "', varname, '": supplied vector should have length ', wid$length, sep=""))
			for(i in 1:length(value))
				.setWinValHelper(paste(varname,"[",i,"]",sep=""), value[i], winName)
			return(value)
		}
	}

	stop(paste("unable to update\"", varname, "\" - no widget found.", sep=""))

	#custon widgets have more than one sub-widget that needs updating
	if (wid$type=="vector") {
		len <- length(value)
		if (wid$length != len && len != 1)
			stop(paste('value should be a vector with length', wid$length, 'or 1'))
		for (i in 1:wid$length) {
			if (len==1)
				index<-1
			else
				index<-i
			.setWinValHelper(paste(varname, "[", i, "]", sep=""), value[index])
		}
	}
	else if (wid$type=="matrix") {
		lenExpected <- wid$nrow * wid$ncol
		len <- length(value)
		if (len != lenExpected && len != 1)
			stop(paste('value should be a vector with length', lenExpected, 'or 1'))
		for (i in 1:wid$nrow) {
			for (j in 1:wid$ncol) {
				if (len == 1) {
					index <- 1 #only a single value
				}
				else if (wid$byrow==TRUE)
					index <- (i-1)*wid$ncol + j
				else
					index <- (j-1)*wid$nrow + i

				.setWinValHelper(paste(varname, "[", i, ",", j, "]", sep=""), value[index])
			}
		}
	}
	else if (wid$type=="slideplus") {
		#TODO: slideplus changes - maybe a list(from=,to=,current=)
		stop(paste('unable to change slideplus - still needs to be implemented'))
	}
	else {
		stop(paste('unable to set "', varname, '"', sep=""))
	}
}


# ***********************************************************
# getWinVal:
#   all variables starting with "PBS." will not be returned by default
#   since they should really be hidden by the user in most cases.
# Arguments:
#   v          - values to get
#   scope      - "L" for local, "G" for global, "" for return list only
#   asvector   - if T return a vector, if F, return list
#   winName - specify a specific window if more than one are in use
# -----------------------------------------------------------
getWinVal <- function(v=NULL, scope="", asvector=FALSE, winName="")
{
	if (!exists(".PBSmod")) {
		stop(".PBSmod was not found")
	}
	if (winName=="")
		winName <- .PBSmod$.activeWin

	if (.isReallyNull(.PBSmod, winName))
		stop(paste("supplied window \"",winName,"\" name not found"))

	#extract all variables regardless if asked for by user
	vars <- .extractVar(winName)

	#get list of all vars (if user didnt supply any)
	if (is.null(v)) {
		v <- names(vars)
		if (is.null(v))
			return(list()) #no widgets with values found
		v <- v[substr(v,1,4)!="PBS."]
		if (!length(v))
			return(list()) #no widgets with values found
	}

	if (asvector)
		vals <- vector()
	else
		vals <- list()

	#iterate over all var names
	for(key in v) {
		if (asvector)
			vals[key] <- vars[[key]]
		else
			vals[[key]] <- vars[[key]]

		if (scope=="L")
			assign(key,vars[[key]],pos=parent.frame(1))
		else if (scope=="G")
			assign(key, vars[[key]], env = .GlobalEnv)
	}
	return(vals)
}


# ***********************************************************
# clearWinVal:
#   removes any global variables that have a name
#   which corresponds to a name in the window desc file
# -----------------------------------------------------------
clearWinVal <- function() 
{
	objs <- names(getWinVal())
	globs <- ls(all.names=TRUE,pos=".GlobalEnv")
	rmlist <- intersect(objs,globs)
	rm(list=rmlist,pos=".GlobalEnv")
	invisible(rmlist)
}


# ***********************************************************
# .convertMode:
#   converts a variable into a mode without showing any warnings
# Arguments:
#   x    - variable to convert
#   mode - mode to convert to
# -----------------------------------------------------------
.convertMode <- function(x, mode)
{

	#TODO - fix regexs
	#they dont slow down, but will mess up with NAs

	if (mode=="logical") {
		# "1" will be TRUE
		x[x=="1"] <- TRUE
		x[x=="0"] <- FALSE
		# anything else that fails the regex will be NA
		tmp <- grep(.regex.logical, x)
		x[-tmp] <- NA
		#anything else is now valid
		x <- as.logical(x)
	}
	else if (mode=="numeric" || mode=="integer") {
		tmp <- grep(.regex.numeric, x)
		x[-tmp] <- NA
		x[x=="-"] <- NA
		x <- as.numeric(x)
	}
	else if (mode=="complex") {
		tmp <- grep(.regex.complex, x)
		x[-tmp] <- NA
		x[x=="-"] <- NA
		x <- as.complex(x)
	}
	#print("convert mode ended at"); print(date());
	#otherwise it is character and nothing needs converting
	return(x)
}


# ***********************************************************
# .autoConvertMode:
#   converts x into a numeric mode, if it looks like a valid number
# Arguments:
#   x - string to convert
# -----------------------------------------------------------
.autoConvertMode <- function(x)
{
	#nice regular expression to see if it could be logical
	if (length(grep(.regex.logical, x))==length(x)) {
		x <- as.logical(x)
	}
	#ugly regular expression to see if it looks like a numeric
	else if (length(grep(.regex.numeric, x))==length(x) 
	         && all(x!="-")) {

		x <- as.numeric(x)
	}
	#uglier regular expression to see if its complex
	else if (length(grep(.regex.complex, x))==length(x)
	         && all(x!="-")) {

		x <- as.complex(x)
	}
	return(x)
}