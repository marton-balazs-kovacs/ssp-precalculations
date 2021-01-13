# Iteration by using the apply family of functions
tictoc::tic()
lapply(test_set_nested$data, function(x) ssp_rope_safe(opt = x$power, band = x$band, delta = x$delta))
tictoc::toc()

tictoc::tic()
mapply(ssp_rope_safe, opt = test_set$power, band = test_set$band, delta = test_set$delta)
tictoc::toc()