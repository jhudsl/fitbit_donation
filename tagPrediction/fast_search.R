# devtools::install_github("pboesu/rucrdtw")
library(rucrdtw)

set.seed(123)
rwalk <- cumsum(runif(1e7, min = -0.5, max = 0.5))

qstart <- sample(length(rwalk), 1)
query <- rwalk[qstart:(qstart+100)]
dtw_search <- ucrdtw_vv(data = rwalk, query = query, dtwwindow = 0.05)


