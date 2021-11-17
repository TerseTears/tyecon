# returning multiple analysis on same object
testvec <- c(1,2,3,7)
testvec %to% {
    mean ~ mean(.)
    sd ~ sd(.)
}

# use nested braces for longer expressions
testvec %to% {
    mean2 ~ { x <- . + 3
    mean(x*.) }
    sd ~ sd(.)
}

# vector results are stored as lists in the dataframe
testvec %to% {
    itself ~ .
    itsmean ~ mean(.)
}

testvec %to% {
    itself ~ .
    itsextra ~ c(2,.)
}
