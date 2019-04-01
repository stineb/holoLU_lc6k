library(ncdf4)

nc <- nc_open("ice5g_v1.2_00.0k_1deg.nc")

orog   <- ncvar_get(nc, "orog")
sftgif <- ncvar_get(nc, "sftgif")

# write 1 into all cells above sea level, NA otherwise
notocean <- orog*NA
notocean[orog>0.0] <- 1.0