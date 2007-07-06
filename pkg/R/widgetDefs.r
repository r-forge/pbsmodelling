#maximum amount of actions to save in memory
.maxActionSize <- 50

# ***********************************************************
# Data: .widgetDefs
#
# Defines allowed widget paramaters names and default values
# -----------------------------------------------------------
.widgetDefs <- list()
.widgetDefs$window <- list(
	list(param='type', required=TRUE, class="character"),
	list(param='name', required=FALSE, class="character", default="window", grep="^[a-zA-Z0-9]+$"),
	list(param='title', required=FALSE, class="character", default=""),
	list(param='vertical', required=FALSE, class="logical", default=TRUE),
	list(param='bg', required=FALSE, class="character", default="#D4D0C8"),
	list(param='fg', required=FALSE, class="character", default="#000000"),
	list(param='onclose', required=FALSE, class="character", default="")
	)


.widgetDefs$grid <- list(
	list(param='type', required=TRUE, class="character"),
	list(param='nrow', required=FALSE, class="integer", default=1),
	list(param='ncol', required=FALSE, class="integer", default=1),
	list(param='toptitle', required=FALSE, class="character", default=""),
	list(param='sidetitle', required=FALSE, class="character", default=""),
	list(param='topfont', required=FALSE, class="character", default=""),
	list(param='sidefont', required=FALSE, class="character", default=""),
	list(param='byrow', required=FALSE, class="logical", default=TRUE),
	list(param='borderwidth', required=FALSE, class="integer", default=1),
	list(param='relief', required=FALSE, class="character", default="flat", grep="^(raised|sunken|flat|ridge|groove|solid)$"),
	list(param='sticky', required=FALSE, class="character", default="", grep="^(n|s|N|S|e|w|E|W)*$"),	#choices: N,NE,E,SE,S,SW,W,NW
	list(param='padx', required=FALSE, class="integer", default=0, grep="^[0-9]+$"),
	list(param='pady', required=FALSE, class="integer", default=0, grep="^[0-9]+$")
	)

.widgetDefs$menu <- list(
	list(param='type', required=TRUE, class="character"),
	list(param='nitems', required=FALSE, class="integer", default=1),
	list(param='label', required=TRUE, class="character"),
	list(param='font', required=FALSE, class="character", default="") #only is valid on sub-menus and not at top level
	)

.widgetDefs$menuitem <- list(
	list(param='type', required=TRUE, class="character"),
	list(param='label', required=TRUE, class="character"),
	list(param='font', required=FALSE, class="character", default=""),
	list(param='function', required=TRUE, class="character"),
	list(param='action', required=FALSE, class="character", default="menuitem")
	)

.widgetDefs$label <- list(
	list(param='type', required=TRUE, class="character"),
	list(param='text', required=FALSE, class="character", default=""),
	list(param='font', required=FALSE, class="character", default=""),
	list(param='fg', required=FALSE, class="character", default="black"),
	list(param='bg', required=FALSE, class="character", default=""),
	list(param='sticky', required=FALSE, class="character", default="", grep="^(n|s|N|S|e|w|E|W)*$"),	#choices: N,NE,E,SE,S,SW,W,NW
	list(param='justify', required=FALSE, class="character", default="left", grep="^(l|r|c|left|right|center)?$"),
	list(param='wraplength', required=FALSE, class="integer", default=0, grep="^[0-9]+$"),
	list(param='padx', required=FALSE, class="integer", default=0, grep="^[0-9]+$"),
	list(param='pady', required=FALSE, class="integer", default=0, grep="^[0-9]+$")
	)

.widgetDefs$null <- list(
	list(param='type', required=TRUE, class="character"),
	list(param='padx', required=FALSE, class="integer", default=0, grep="^[0-9]+$"),
	list(param='pady', required=FALSE, class="integer", default=0, grep="^[0-9]+$")
	)

.widgetDefs$entry <- list(
	list(param='type', required=TRUE, class="character"),
	list(param='name', required=TRUE, class="character", grep="^([a-zA-Z0-9])+(\\[[0-9,]+\\])?$"),
	list(param='value', required=FALSE, class="character", default=""),
	list(param='width', required=FALSE, class="integer", default=20),
	list(param='label', required=FALSE, class="character", default=""),
	list(param='font', required=FALSE, class="character", default=""),
	list(param='fg', required=FALSE, class="character", default=""),
	list(param='bg', required=FALSE, class="character", default=""),
	list(param='entryfont', required=FALSE, class="character", default=""),
	list(param='entryfg', required=FALSE, class="character", default="black"),
	list(param='entrybg', required=FALSE, class="character", default="white"),
	list(param='function', required=FALSE, class="character", default=""),
	list(param='enter', required=FALSE, class="logical", default=TRUE), #require an enter to call function
	list(param='action', required=FALSE, class="character", default="entry"),
	list(param='mode', required=FALSE, class="character", default="numeric", grep="^(numeric|integer|complex|logical|character)$"),
	list(param='sticky', required=FALSE, class="character", default="", grep="^(n|s|N|S|e|w|E|W)*$"),	#choices: N,NE,E,SE,S,SW,W,NW
	list(param='padx', required=FALSE, class="integer", default=0, grep="^[0-9]+$"),
	list(param='pady', required=FALSE, class="integer", default=0, grep="^[0-9]+$")
	)

.widgetDefs$button <- list(
	list(param='type', required=TRUE, class="character"),
	list(param='text', required=FALSE, class="character", default="Calculate"),
	list(param='font', required=FALSE, class="character", default=""),
	list(param='fg', required=FALSE, class="character", default="black"),
	list(param='bg', required=FALSE, class="character", default=""),
	list(param='width', required=FALSE, class="integer", default=0),
	list(param='function', required=FALSE, class="character", default=""),
	list(param='action', required=FALSE, class="character", default="button"),
	list(param='sticky', required=FALSE, class="character", default="", grep="^(n|s|N|S|e|w|E|W)*$"),	#choices: N,NE,E,SE,S,SW,W,NW
	list(param='padx', required=FALSE, class="integer", default=0, grep="^[0-9]+$"),
	list(param='pady', required=FALSE, class="integer", default=0, grep="^[0-9]+$")
	)

.widgetDefs$check <- list(
	list(param='type', required=TRUE, class="character"),
	list(param='name', required=TRUE, class="character", grep="^([a-zA-Z0-9])+$"),
	list(param='mode', required=FALSE, class="character", default="logical", grep="^(numeric|integer|logical)$"),
	list(param='checked', required=FALSE, class="logical", default=FALSE),
	list(param='text', required=FALSE, class="character", default=""),
	list(param='font', required=FALSE, class="character", default=""),
	list(param='fg', required=FALSE, class="character", default="black"),
	list(param='bg', required=FALSE, class="character", default=""),
	list(param='function', required=FALSE, class="character", default=""),
	list(param='action', required=FALSE, class="character", default="check"),
	list(param='sticky', required=FALSE, class="character", default="", grep="^(n|s|N|S|e|w|E|W)*$"),	#choices: N,NE,E,SE,S,SW,W,NW
	list(param='padx', required=FALSE, class="integer", default=0, grep="^[0-9]+$"),
	list(param='pady', required=FALSE, class="integer", default=0, grep="^[0-9]+$")
	)

.widgetDefs$radio <- list(
	list(param='type', required=TRUE, class="character"),
	list(param='name', required=TRUE, class="character", grep="^([a-zA-Z0-9])+(\\[[0-9,]+\\])?$"),
	list(param='value', required=TRUE, class="character"),
	list(param='text', required=FALSE, class="character", default=""),
	list(param='font', required=FALSE, class="character", default=""),
	list(param='fg', required=FALSE, class="character", default="black"),
	list(param='bg', required=FALSE, class="character", default=""),
	list(param='function', required=FALSE, class="character", default=""),
	list(param='action', required=FALSE, class="character", default="radio"),
	list(param='mode', required=FALSE, class="character", default="numeric", grep="^(numeric|integer|complex|logical|character)$"),
	list(param='selected', required=FALSE, class="logical", default=FALSE),
	list(param='sticky', required=FALSE, class="character", default="", grep="^(n|s|N|S|e|w|E|W)*$"),	#choices: N,NE,E,SE,S,SW,W,NW
	list(param='padx', required=FALSE, class="integer", default=0, grep="^[0-9]+$"),
	list(param='pady', required=FALSE, class="integer", default=0, grep="^[0-9]+$")
	)

.widgetDefs$slide <- list(
	list(param='type', required=TRUE, class="character"),
	list(param='name', required=TRUE, class="character", grep="^([a-zA-Z0-9])+$"),
	list(param='from', required=FALSE, class="integer", class="integer", default=0),
	list(param='to', required=FALSE, class="integer", default=100),
	list(param='value', required=FALSE, class="integer", default=NA),
	list(param='showvalue', required=FALSE, class="logical", default=FALSE),
	list(param='orientation', required=FALSE, class="character", default="horizontal"), #TODO: grep="^(horizontal|vertical)" - test this
	list(param='font', required=FALSE, class="character", default=""),
	list(param='fg', required=FALSE, class="character", default="black"),
	list(param='bg', required=FALSE, class="character", default=""),
	list(param='function', required=FALSE, class="character", default=""),
	list(param='action', required=FALSE, class="character", default="slide"),
	list(param='sticky', required=FALSE, class="character", default="", grep="^(n|s|N|S|e|w|E|W)*$"),	#choices: N,NE,E,SE,S,SW,W,NW
	list(param='padx', required=FALSE, class="integer", default=0, grep="^[0-9]+$"),
	list(param='pady', required=FALSE, class="integer", default=0, grep="^[0-9]+$")
	)

.widgetDefs$slideplus <- list(
	list(param='type', required=TRUE, class="character"),
	list(param='name', required=TRUE, class="character", grep="^([a-zA-Z0-9])+$"),
	list(param='from', required=FALSE, class="numeric", default=0),
	list(param='to', required=FALSE, class="numeric", default=1),
	list(param='by', required=FALSE, class="numeric", default=0.01),
	list(param='value', required=FALSE, class="numeric", default=NA),
	list(param='function', required=FALSE, class="character", default=""),
	list(param='enter', required=FALSE, class="logical", default=FALSE), #require an enter to change min/max values
	list(param='action', required=FALSE, class="character", default="slideplus"),
	list(param='sticky', required=FALSE, class="character", default="", grep="^(n|s|N|S|e|w|E|W)*$"),	#choices: N,NE,E,SE,S,SW,W,NW
	list(param='padx', required=FALSE, class="integer", default=0, grep="^[0-9]+$"),
	list(param='pady', required=FALSE, class="integer", default=0, grep="^[0-9]+$")
	)

.widgetDefs$vector <- list(
	list(param='type', required=TRUE, class="character"),
	list(param='names', required=TRUE, class="characterVector", grep="^([a-zA-Z0-9])+(\\[[0-9,]+\\])?([ \t]+([a-zA-Z0-9])+(\\[[0-9,]+\\])?)*$"),
	list(param='length', required=FALSE, class="integer", default=0),
	list(param='labels', required=FALSE, class="characterVector", default=""),
	list(param='values', required=FALSE, class="characterVector", default=""),
	list(param='vecnames', required=FALSE, class="characterVector", default=""),
	list(param='font', required=FALSE, class="character", default=""),
	list(param='fg', required=FALSE, class="character", default="black"),
	list(param='bg', required=FALSE, class="character", default=""),
	list(param='entryfont', required=FALSE, class="character", default=""),
	list(param='entryfg', required=FALSE, class="character", default="black"),
	list(param='entrybg', required=FALSE, class="character", default="white"),
	list(param='vertical', required=FALSE, class="logical", default=FALSE),
	list(param='function', required=FALSE, class="character", default=""),
	list(param='enter', required=FALSE, class="logical", default=TRUE),
	list(param='action', required=FALSE, class="character", default="vector"),
	list(param='mode', required=FALSE, class="character", default="numeric", grep="^(numeric|integer|complex|logical|character)$"),
	list(param='width', required=FALSE, class="integer", default=6, grep="^[0-9]+$"),
	list(param='sticky', required=FALSE, class="character", default="", grep="^(n|s|N|S|e|w|E|W)*$"),	#choices: N,NE,E,SE,S,SW,W,NW
	list(param='padx', required=FALSE, class="integer", default=0, grep="^[0-9]+$"),
	list(param='pady', required=FALSE, class="integer", default=0, grep="^[0-9]+$")
	)

.widgetDefs$matrix <- list(
	list(param='type', required=TRUE, class="character"),
	list(param='nrow', required=TRUE, class="integer"),
	list(param='ncol', required=TRUE, class="integer"),
	list(param='names', required=TRUE, class="characterVector", grep="^([a-zA-Z0-9])+(\\[[0-9,]+\\])?([ \t]+([a-zA-Z0-9])+(\\[[0-9,]+\\])?)*$"), #variable names (or name)
	list(param='rowlabels', required=FALSE, class="characterVector", default=""),
	list(param='collabels', required=FALSE, class="characterVector", default=""),
	list(param='rownames', required=FALSE, class="characterVector", default=""),
	list(param='colnames', required=FALSE, class="characterVector", default=""),
	list(param='font', required=FALSE, class="character", default=""),
	list(param='fg', required=FALSE, class="character", default="black"),
	list(param='bg', required=FALSE, class="character", default=""),
	list(param='entryfont', required=FALSE, class="character", default=""),
	list(param='entryfg', required=FALSE, class="character", default="black"),
	list(param='entrybg', required=FALSE, class="character", default="white"),
	list(param='values', required=FALSE, class="characterVector", default=""), #variable names (or name)
	list(param='byrow', required=FALSE, class="logical", default=TRUE),
	list(param='function', required=FALSE, class="character", default=""),
	list(param='enter', required=FALSE, class="logical", default=TRUE),
	list(param='action', required=FALSE, class="character", default="matrix"),
	list(param='mode', required=FALSE, class="character", default="numeric", grep="^(numeric|integer|complex|logical|character)$"),
	list(param='width', required=FALSE, class="integer", default=6, grep="^[0-9]+$"),
	list(param='sticky', required=FALSE, class="character", default="", grep="^(n|s|N|S|e|w|E|W)*$"),	#choices: N,NE,E,SE,S,SW,W,NW
	list(param='padx', required=FALSE, class="integer", default=0, grep="^[0-9]+$"),
	list(param='pady', required=FALSE, class="integer", default=0, grep="^[0-9]+$")
	)

.widgetDefs$data <- list(
	list(param='type', required=TRUE, class="character"),
	list(param='nrow', required=TRUE, class="integer"),
	list(param='ncol', required=TRUE, class="integer"),
	list(param='names', required=TRUE, class="characterVector", grep="^([a-zA-Z0-9])+(\\[[0-9,]+\\])?([ \t]+([a-zA-Z0-9])+(\\[[0-9,]+\\])?)*$"), #variable names (or name)
	list(param='modes', required=FALSE, class="characterVector", default="numeric", grep="^(numeric|integer|complex|logical|character)([ \t]+(numeric|integer|complex|logical|character))*$"),
	list(param='rowlabels', required=FALSE, class="characterVector", default=""),
	list(param='collabels', required=FALSE, class="characterVector", default=""),
	list(param='rownames', required=FALSE, class="characterVector", default="X"),
	list(param='colnames', required=FALSE, class="characterVector", default="Y"),
	list(param='font', required=FALSE, class="character", default=""),
	list(param='fg', required=FALSE, class="character", default="black"),
	list(param='bg', required=FALSE, class="character", default=""),
	list(param='entryfont', required=FALSE, class="character", default=""),
	list(param='entryfg', required=FALSE, class="character", default="black"),
	list(param='entrybg', required=FALSE, class="character", default="white"),
	list(param='values', required=FALSE, class="characterVector", default=""), #variable names (or name)
	list(param='byrow', required=FALSE, class="logical", default=TRUE),
	list(param='function', required=FALSE, class="character", default=""),
	list(param='enter', required=FALSE, class="logical", default=TRUE),
	list(param='action', required=FALSE, class="character", default="data"),
	list(param='width', required=FALSE, class="integer", default=6, grep="^[0-9]+$"),
	list(param='sticky', required=FALSE, class="character", default="", grep="^(n|s|N|S|e|w|E|W)*$"),	#choices: N,NE,E,SE,S,SW,W,NW
	list(param='padx', required=FALSE, class="integer", default=0, grep="^[0-9]+$"),
	list(param='pady', required=FALSE, class="integer", default=0, grep="^[0-9]+$")
	)

.widgetDefs$object <- list(
	list(param='type', required=TRUE, class="character"),
	list(param='name', required=TRUE, class="character", grep="^([a-zA-Z0-9])+(\\[[0-9,]+\\])?$"),
	list(param='font', required=FALSE, class="character", default=""),
	list(param='fg', required=FALSE, class="character", default="black"),
	list(param='bg', required=FALSE, class="character", default=""),
	list(param='entryfont', required=FALSE, class="character", default=""),
	list(param='entryfg', required=FALSE, class="character", default="black"),
	list(param='entrybg', required=FALSE, class="character", default="white"),
	list(param='vertical', required=FALSE, class="logical", default=FALSE),
	list(param='function', required=FALSE, class="character", default=""),
	list(param='enter', required=FALSE, class="logical", default=TRUE),
	list(param='action', required=FALSE, class="character", default="data"),
	list(param='width', required=FALSE, class="integer", default=6, grep="^[0-9]+$"),
	list(param='sticky', required=FALSE, class="character", default="", grep="^(n|s|N|S|e|w|E|W)*$"),	#choices: N,NE,E,SE,S,SW,W,NW
	list(param='padx', required=FALSE, class="integer", default=0, grep="^[0-9]+$"),
	list(param='pady', required=FALSE, class="integer", default=0, grep="^[0-9]+$")
	)

.widgetDefs$history <- list(
	list(param='type', required=TRUE, class="character"),
	list(param='name', required=FALSE, class="character", default="default", grep="^([a-zA-Z0-9]+)$"),
	list(param='function', required=FALSE, class="character", default=""),
	list(param='import', required=FALSE, class="character", default=""),
	list(param='sticky', required=FALSE, class="character", default="", grep="^(n|s|N|S|e|w|E|W)*$"),	#choices: N,NE,E,SE,S,SW,W,NW
	list(param='padx', required=FALSE, class="integer", default=0, grep="^[0-9]+$"),
	list(param='pady', required=FALSE, class="integer", default=0, grep="^[0-9]+$")
	)

.widgetDefs$text <- list(
	list(param='type', required=TRUE, class="character"),
	list(param='name', required=TRUE, class="character", grep="^([a-zA-Z0-9])+$"),
	list(param='height', required=FALSE, class="integer", grep="^([0-9])+$", default=8),
	list(param='width', required=FALSE, class="integer", grep="^([0-9])+$", default=30),
	list(param='edit', required=FALSE, class="logical", default=FALSE), #t=user can change text
	list(param='scrollbar', required=FALSE, class="logical", default=TRUE), # user can add a scrollbar or not
	###TODO - tktext can crash if supplied an invalid colour name
	list(param='fg', required=FALSE, class="character", default="black"),
	list(param='bg', required=FALSE, class="character", default="white"),
	list(param='mode', required=FALSE, class="character", default="character", grep="^(numeric|integer|complex|logical|character)$"),
	list(param='font', required=FALSE, class="character", default=""),
	list(param='value', required=FALSE, class="character", default=""),
	list(param='borderwidth', required=FALSE, class="integer", default=1),
	list(param='relief', required=FALSE, class="character", default="sunken", grep="^(raised|sunken|flat|ridge|groove|solid)$"),
	list(param='sticky', required=FALSE, class="character", default="", grep="^(n|s|N|S|e|w|E|W)*$"),	#choices: N,NE,E,SE,S,SW,W,NW
	list(param='padx', required=FALSE, class="integer", default=0, grep="^[0-9]+$"),
	list(param='pady', required=FALSE, class="integer", default=0, grep="^[0-9]+$")
	)


# ***********************************************************
# Data: .pFormatDefs
#
# Defines allowed P format paramaters names and default values
# to be used in readList (when P format is detected)
# -----------------------------------------------------------

#TODO - fix readList documentation, and error debugging
#> readList("mylist.txt")
#GUI parse error (mylist.txt:2) : unknown widget type 'list'
#2: $$list
#the word "widget" does not belong


.pFormatDefs <- list()
.pFormatDefs$vector <- list(
	list(param='type', required=TRUE, class="character"),
	list(param='mode', required=FALSE, class="character", default="numeric"),
	list(param='names', required=FALSE, class="characterVector", default="")
	)

.pFormatDefs$matrix <- list(
	list(param='type', required=TRUE, class="character"),
	list(param='mode', required=FALSE, class="character", default="numeric"),
	list(param='ncol', required=TRUE, class="numeric"),
	list(param='rownames', required=FALSE, class="characterVector", default=""),
	list(param='colnames', required=FALSE, class="characterVector", default=""),
	list(param='byrow', required=FALSE, class="logical", default=TRUE)
	)

.pFormatDefs$array <- list(
	list(param='type', required=TRUE, class="character"),
	list(param='mode', required=FALSE, class="character", default="numeric"),
	list(param='dim', required=TRUE, class="characterVector"),
	list(param='byright', required=FALSE, class="logical", default=TRUE)
	)

.pFormatDefs$data <- list(
	list(param='type', required=TRUE, class="character"),
	list(param='modes', required=FALSE, class="characterVector", default="numeric"),
	list(param='ncol', required=TRUE, class="numeric"),
	list(param='rownames', required=FALSE, class="characterVector"),
	list(param='colnames', required=TRUE, class="characterVector"),
	list(param='byrow', required=FALSE, class="logical", default=TRUE)
	)



# ***********************************************************
# Regular expression strings
# -----------------------------------------------------------

#catches all valid complex except it also catches "-"
.regex.complex <- "^(\\-|\\+)?[0-9]*(\\.[0-9]*)?((e|E)(\\+|\\-)?[0-9]+)?(((\\-|\\+)?[0-9]+|[0-9]*)(\\.[0-9]*)?((e|E)(\\+|\\-)?[0-9]+)?i)?$"

#catches numeric strings, but also catches "-"
.regex.numeric <- "^(\\-|\\+)?[0-9]*(\\.[0-9]*)?((e|E)(\\+|\\-)?[0-9]+)?$"

#catches all logical values
.regex.logical <- "^(T|TRUE|F|FALSE)$"