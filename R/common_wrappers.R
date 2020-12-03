#@# wrappers for common functionalities

# linearHypothesis from car

# computing summaries more easily (using dplyr perhaps)

# better plotting functionalities and nicer defaults (publication ready)
# I think there was a package called ggreports or something.
library(magrittr)
library(purrr)
library(rlang)
library(stringr)

?attitude

# I can use query "backends" for computations that are done in other packages or
# functions such as computations offered by summary.lm. Then write my own computations
# functions if needed as well but it's better if I don't rely on it.
# Basically, there should be a table for each statistical model with all possible
# values or plot computations that can be obtained for it. Then, the job of my query
# function would be to simply query the specific computation needed.

# queries should be in the form "p:smth ", "t:another thing", "c:computed value" "d:fitted values"
# plot, table, computed, data
# the first letter likely would allow me to classify and lookup faster
# I can use broom to get to a tidy output quicker.
# second level query, such as wanting to get something from a table
# Needs clear definitions of tables and values and good seperation. e.g. broom showing
# degrees of freedoom for a model isn't really tidy in fact.
query_lm <- function(lmObj, query, secondLvQuery = NULL){
    # check if lm

}

# I could write two "macros" both branching, with one that would collapse all these
# queries, while the other would simply print them.

# TODO writing a branching out operator. Remember that rmonad has this already!!!

# TODO writing easier queries from lists etc. to calculate F>0 from coef or annova


# attributes preserving "macro". Useful for preserving labels for instance.
# TODO - [ ] an interesting goal would be to write similar macros, doing similar things

avar <- 15
attr(avar, "holy") <- "saint level"
smth <- avar%pre%(map_dbl(~.x*2) %>% map_dbl(~.x+15))

# TODO - [ ] I better perform some check that the initial structure is somewhat preserved
# in the end operations and at least offer a *warning*, otherwise, applying vector attributes
# to say, lists doesn't really make sense.

# using this in the `%pre%` way probably would cause annoyance with having to put ()
# around the inner expression
`%pre%` <- function(x, y, controls = NULL) {
    y <- expr_deparse(enexpr(y))
    if (str_detect(y,"^\\(")) y <- str_sub(y, 2, -2)
    myexp <- paste("x","%>%",y)
    a <- parse_expr(myexp)
    res <- eval_tidy(a)
    for(  i in setdiff( names(attributes(x)), names(attributes(res)) )  ){
    attributes(res)[i] <- attributes(x)[i]
    }
    return(res)
}


`%pre%`(10, map(~.x*2) %>% map(~.x+15))
