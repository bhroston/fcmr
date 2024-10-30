rtriangular_dist(100, -1, 0, 1)

x <- rtriangular_dist(n = 1000, lower = -0.5, mode = 0, upper = 0.5)
hist(x)

x <- rtriangular_dist(100, 0, 0, 0.5)
hist(x)
