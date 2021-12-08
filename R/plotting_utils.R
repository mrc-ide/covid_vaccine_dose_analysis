
col1 <- "#5da0b5"
col2 <- "#c59e96"
col3 <- "#747473"
col4 <- "#5c8e72"
col5 <- "#2a73bb"
col6 <- "#1b9e77"
col7 <- "#d95f02"
col8 <- "#7570b3"
col9 <- "#fc8d62" #orange

# set up the colours!
cc <- scales::viridis_pal(end = 0.9)(6)
col_set <- c(cc[1], cc[3], cc[4], cc[5], cc[6])
col_set_3 <- c(col_set[1], col_set[2], col_set[5])

col_set_spacing <- scales::viridis_pal(option = "B", begin = 0.4, end = 0.8)(2)
