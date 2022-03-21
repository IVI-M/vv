# 00_common.R


# source ("00_common.R")
#0. Global settings ----
if (T) {  
  library(magrittr); library(ggplot2)
  library(lubridate,  quietly=T); options(lubridate.week.start =  1)
  library(data.table); options(datatable.print.class=TRUE)
  library(dygraphs)
  library(plotly); 
  library(DT); 
  library(ggpubr)
  library(stringr); 
  library(forcats) 
  library(scales) #  scale_y_continuous(limits=c(0, 1), breaks=seq(0,1,0.1), labels = percent) +

  
  options(digits = 3)
  # options(max.print = 100) # 1000
  options(scipen = 999)
  
  dateToday <- format(Sys.time(), '%d %B, %Y') %>% dmy; dateToday
  
  "%wo%" <- function(x, y) setdiff(x,y) 
  `%ni%` <-  Negate(`%in%`)
  
  theme_set(theme_bw())
  # theme_set(theme_minimal())
  # library(ggthemes); 
  # theme_set(theme_economist())
  # theme_set(theme_economist_white())
  
}


in0 <- list()

if ( !shiny::isRunning() ) { # DOES NOT WORK when Shiny is running?
  cat("Relax - Shiny is NOT Running :)")
  input <- in0
}




#Found in: stackoverflow:
# Efficient (in place) rows deletion from data.table 

#' Title
#'
#' @param DT 
#' @param del.idxs 
#'
#' @return
#' @export
#'
#' @examples
dt.rmRow <- function(DT, del.idxs) {  # pls note 'del.idxs' vs. 'keep.idxs'
  if (!is.data.table(dt))
    dt <- as.data.table(dt)
  
  keep.idxs <- setdiff(DT[, .I], del.idxs);  # select row indexes to keep
  cols = names(DT);
  DT.subset <- data.table(DT[[1]][keep.idxs]); # this is the subsetted table
  setnames(DT.subset, cols[1]);
  for (col in cols[2:length(cols)]) {
    DT.subset[, (col) := DT[[col]][keep.idxs]];
    DT[, (col) := NULL];  # delete
  }
  return(DT.subset); # NB: Original DT is also changed  by reference !
}

if (F) {
  dt <- readRDS(paste0("13100810.Rds"))
  dt
  dt %>% dt.rmRow(nrow(dt))
}


## dt.select
colsSelect <- function (dt, cols)  {
  dt[, cols, with=F]
  # Note also other ways to select column(s) in data.table
  # dt[, ..cols] # FOR ONE COLUMN ONLY
  # dt[,.SD, .SDcols=cols] 
}

colsRm <- function (dt, cols)  {
  dt[, (cols) := NULL] 
}

setcolorder.fromLast <- function( dt, neworder) {
  # TBD - useful for cast.
  
  # setcolorder.fromLast(dtCached, c("Cause of death (ICD-10)", "value"))
  # setcolorder(dtCached, c("Date",  "GEO", "Cause of death (ICD-10)", "value"))
}

### Automatically finding / removing common parts in strings 

if (F) {
  # https://stackoverflow.com/questions/48701107/find-length-of-overlap-in-strings
  
  #' Title
  #'
  #' @param str1 
  #' @param str2 
  #' @param ignore.case 
  #'
  #' @return
  #' @export
  #'
  #' @examples
  str_find_overlap <- function(str1, str2, ignore.case = FALSE) { # , verbose = FALSE
    
    if(ignore.case) {
      str1 <- tolower(str1);    str2 <- tolower(str2)
    }
    if(nchar(str1) < nchar(str2)) {
      x <- str2;    str2 <- str1;    str1 <- x
    }
    
    x <- strsplit(str2, "")[[1L]]
    n <- length(x)
    s <- sequence(seq_len(n))
    s <- split(s, cumsum(s == 1L))
    s <- rep(list(s), n)
    
    for(i in seq_along(s)) {
      s[[i]] <- lapply(s[[i]], function(x) {
        x <- x + (i-1L)
        x[x <= n]
      })
      s[[i]] <- unique(s[[i]])
    }
    
    s <- unlist(s, recursive = FALSE)
    s <- unique(s[order(-lengths(s))])
    
    i <- 1L
    len_s <- length(s)
    while(i < len_s) {
      lcs <- paste(x[s[[i]]], collapse = "")
      # if(verbose) cat("now checking:", lcs, "\n")
      check <- grepl(lcs, str1, fixed = TRUE)
      if(check) {
        # if(verbose) cat(paste0("Found: '",lcs,"' (length =", nchar(lcs), ") \n")) 
        break
      } else {
        i <- i + 1L 
      }
    }
    return (lcs)
  }
  # 
  # str_remove_overlap <- function(aStr) {
  #   str0 <- str_find_overlap( aStr[1],  aStr[2]); str0
  #   str_replace(aStr, str0, "")
  # }
  # 
  # if (F) {
  #   library(data.table)
  #   # dt <- cansim::get_cansim("13-10-0810-01") %>% setDT(dt) 
  #   dt <- data.table::data.table(
  #     GEO=c( # From CANSIM Table
  #       "Newfoundland and Labrador, place of occurrence",
  #       "Prince Edward Island, place of occurrence",     
  #       "Nova Scotia, place of occurrence"
  #     ))
  #   
  #   aStr <- dt$GEO
  #   
  #   
  #   dt[, GEO:=str_remove_overlap(GEO)][]
  #   #                         GEO
  #   #                      <char>
  #   #1: Newfoundland and Labrador
  #   #2:      Prince Edward Island
  #   #3:               Nova Scotia
  # }
  # 
  
}

# Consider also: paged_table(dt), reactable() and https://gt.rstudio.com/

datatable.title <- function(dt, title=NULL) {
  # https://rstudio.github.io/DT/options.html
  dt %>% DT::datatable (
    filter = "top",  
    caption = title,
    rownames=F,   
    extensions =  c('ColReorder', 'Buttons'),
    options = list(
      dom = 'Blfrtip',
      # paging = FALSE, 
      scrollX = TRUE, scrollY = "600px",
      
      colReorder = TRUE,
      lengthMenu = list(c(10,25,100,-1), c(10,25,100,"All")),
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print') 
    ) )
}

datatable.fixedCols <- function(dt, l=1, r=2, title=NULL) {
  dt %>% datatable( extensions = 'FixedColumns',
                    caption = title,
                    rownames=F,   
                    options = list(
                      dom = 't',
                      rowId = 0,
                      scrollX = TRUE,
                      fixedColumns = list(leftColumns = l, rightColumns = r)
                    )
  )
}




# https://www.r-bloggers.com/vignette-downloadable-tables-in-rmarkdown-with-the-dt-package/
# See also https://github.com/renkun-ken/formattable
# https://rstudio.github.io/DT/plugins.html

d7.datatable <- function(x){
  DT::datatable(x,
                extensions = 'Buttons',
                options = list(dom = 'Blfrtip',
                               buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                               lengthMenu = list(c(10,25,50,-1),
                                                 c(10,25,50,"All"))))
}

if (F) {
  
  datatable(
    iris, extensions = c('Select', 'Buttons'), options = list(
      select = list(style = 'os', items = 'row'),
      dom = 'Blfrtip',
      rowId = 0,
      buttons = c('selectAll', 'selectNone', 'selectRows', 'selectColumns', 'selectCells')
    ),
    selection = 'none'
  )
  
  #You can click and drag the table header to move a certain column to a different place using the ColReorder extension.
  datatable(iris2, extensions = 'ColReorder', options = list(colReorder = TRUE))
  
  # fix some left 2 columns and right 1 column
  datatable(
    m, extensions = 'FixedColumns',
    options = list(
      dom = 't',
      scrollX = TRUE,
      fixedColumns = list(leftColumns = 2, rightColumns = 1)
    )
  )
  
  
  datatable(m, extensions = 'Scroller', options = list(
    deferRender = TRUE,
    scrollY = 200,
    scroller = TRUE
  ))
  
  
  # # https://yihui.shinyapps.io/DT-rows/
  # 
  # •	! https://rstudio.github.io/renv
  # •	https://rstudio.com/products/package-manager/ (better than minicran+ rsync, but not free)
  # 
  # Also great solution for mixing data.table and pipelines:
  #   https://stackoverflow.com/questions/33761872/break-data-table-chain-into-two-lines-of-code-for-readability/33761906#33761906
  
}


dygraph.title <- function(dts, title=NULL, group="1st group") {
  dygraph(dts, main = title, group = group) %>%
    # dySeries(input$var1, color = input$color1, strokePattern = input$stroke1,  axis = input$axis1 )  %>% 
    dyOptions(fillGraph = F, stepPlot = F, drawGrid = T, drawPoints = TRUE, pointSize = 2) %>%
    dyHighlight(highlightCircleSize = 5,highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE) %>%
    dyAxis("y", label="Deaths / week") %>%
    # dyAnnotation("2021-5-1", text = "3%", tooltip = "Fully Vaccinated Rate 3%") %>%
    # dyAnnotation("2021-4-6", text = "2%", tooltip = "Fully Vaccinated Rate 2%") %>%
    # dyAnnotation("2021-6-10", text = "10%", tooltip = "Fully Vaccinated Rate 10%") %>%
    # dyAnnotation("2021-2-18", text = "1%", tooltip = "Fully Vaccinated Rate 1%") %>%
    dyRangeSelector() 
}


of <- function (x, range)  { # use negative range to remove
  if (is.numeric(range)) {
    # if (bKeep) {
    return(x[range])
    # } else {
    #   #return(x[ which ( (1:length(x)) %ni% range ) ])
    #   return(x[-range])
    # }
  }else { # - for now  assume it is data.table 
    dt.cols(x, range)
    # return(x[, range, with=F]) 
  }
}


dt.autoreplace <- function (dt, A = function (x) ifelse(is.na(x), 0, x), cols=NULL) {
  if (is.null(cols)) 
    cols <- 1:ncol(dt)
  
  dt [, (cols):= lapply(.SD, eval(A)), .SDcol = cols]
}

dt.AllNAto0 <- function (dt, cols=NULL) {
  if (is.null(cols)) 
    cols <- 1:ncol(dt)
  
  dt [, (cols):= lapply(.SD, function (x) ifelse(is.na(x), 0, x)), .SDcol = cols]
}
dt.AllNAtoZero <- dt.autoreplace


percentageOf <- function(a, equalto=0, decimals=2, multiply=100) {
  ( length ( which (a == equalto) )  /  length (a) * 100 ) %>% round(decimals)
}
if (F) {
  percentageOf (a=as.ordered(0:19))
  c( rep(1,3), rep(0,7) ) %>% as.ordered() %>% percentageOf
}

#meanOrdered <- function(a) {
my.mean <- function(a, decimals=2) {
  if (is.factor(a)) {
    percentageOf(a,decimals)
  } else {
    mean(a, na.rm = T) %>%  round(decimals)
  }
}



my.paste <- function(array, sep=" ") {
  str <- ""
  for (i in 1:length(array)) 
    str <- str %+% array[i] %+% sep
  return (substr(str, 1, nchar(str)-nchar(sep)))
}


print.pre <- function(...) {
  cat("<pre>\n")
  print (...)
  cat("\n</pre>")  
} 
print.rmd <- function(...) {
  cat("\n\n")
  print (...)
  cat("\n\n")  
}

rmd.print <- function(...) {
  cat("<pre>\n")
  print (...)
  cat("\n</pre>")  
}

cat.rmd <- function(...) {
  cat("\n\n\n\n")
  cat (...)
  cat("\n\n\n\n")   
}
rmd.cat <- function(...) {
  cat("\n\n\n\n")
  cat (...)
  cat("\n\n\n\n")   
}
#my.print <- rmd.print
#my.cat <-  rmd.cat 


#  dt.Briefed <- function(dt2, width=5) {
# #   str <- names(dt)
#    # names(dt2) %<>% lapply( str_trunc, 5, ellipsis="")
#    # names(dt2) <- lapply( names(dt2), str_trunc, 5, ellipsis="")
#        names(dt2) %<>% str_trunc (5, ellipsis="")
#  #  return (str)
#  }
#  
print.short <- function(dt, width=6, kable=NULL) {
  str <- dt %>% names()
  names(dt) %<>% str_trunc (width, ellipsis="") 
  if (kable=="kable") {
    dt %>% knitr::kable() %>% print
    cat("\n\n\n")
  } else 
    dt %>% print
  names(dt) <- str
}




# my.print2 <- function(..., strCaption=NULL) { # DOES NOT WORK THIS WAy !
#   
#   if (!is.null(strCaption))
#     cat(paste0("\n\n*", strCaption, "*\n\n"))
#   cat("<pre>\n")
#   print (...)
#   cat("\n</pre>")  
# }


DIV_WITH_BUTTON <- TRUE
DIV_SET_BUTTONS <- function(bool=TRUE) {
  DIV_WITH_BUTTON <<- bool
}


DIV_COMPILE_MODE_CONST <- c("show buttons", "show all", " show nothing", "print-ready") # for manager"
DIV_WITH_BUTTON <- DIV_COMPILE_MODE_CONST[1]
DIV_SET_COMPILE_MODE <- function(str=DIV_COMPILE_MODE_CONST[1]) {
  DIV_WITH_BUTTON <<- str
}

set.seed(22)  #btn-block

#DIV_START <- function(strText="Details", bButton="button", bShow = T){
DIV_START <- function(strText="Details", bShow = T){
  
  iButton <- runif(1, min=0, max=100000) %>% as.integer()
  
  # "btn btn-primary btn-block btn-lg active" 
  # "btn btn-primary btn-block" 
  if (bShow) {
    # if (bButton == "button" | DIV_WITH_BUTTON == TRUE) {
    cat(
      glue::glue(
        paste0(
          '\n\n',
          '<button class="btn btn-primary btn-block active"  data-toggle="collapse" data-target="#BlockName',iButton, '"> Show/Hide ', strText, '</button>',   
          '\n',
          '<div id="BlockName', iButton,'" class="collapse', ifelse(bShow == "hide", "", " in"), '">\n\n'    
        )
      )
    )
  } else {
    
    rmd.cat(paste0("\n\n###", strText))
    # rmd.cat(paste0("**", strText, "**"))
    
    cat("\n\n<div>\n\n")
    
  }
}

DIV_END <- function(str="") {
  # cat(glue::glue("<br><hr><br></div>"))
  # cat("<br><hr><br></div>")
  cat("</div>\n\n")
  cat(str)
}



