par(mfrow=c(4,3), mar=c(2,2,2,5))

plot(lr[[1]], main= "red", asp = nrow(vis.red)/ncol(vis.red), col=grayscale_colors)
plot(lr[[2]], main= "green", asp = nrow(vis.red)/ncol(vis.red), col=grayscale_colors)
plot(lr[[3]], main= "blue", asp = nrow(vis.red)/ncol(vis.red), col=grayscale_colors)

plot(lrn[[1]], main= "normalize_red"  , asp = nrow(vis.red)/ncol(vis.red), col=grayscale_colors)
plot(lrn[[2]], main= "normalize_green", asp = nrow(vis.red)/ncol(vis.red), col=grayscale_colors)
plot(lrn[[3]], main= "normalize_blue" , asp = nrow(vis.red)/ncol(vis.red), col=grayscale_colors)

plot(lrexR[[1]], main="Ex_Red", asp = nrow(vis.red)/ncol(vis.red), col=grayscale_colors)
plot(lrexG[[2]], main="Ex_Green",asp = nrow(vis.red)/ncol(vis.red), col=grayscale_colors)
plot(lrexB[[3]], main="Ex_Blue", asp = nrow(vis.red)/ncol(vis.red), col=grayscale_colors)


hist(lr[[1]], breaks=100, main ="distrib_red", xlim=c(0,1), xlab = "", col=grayscale_colors)
hist(lr[[2]], breaks=100, main ="distrib_green", xlim=c(0,1), xlab = "", col=grayscale_colors)
hist(lr[[3]], breaks=100, main ="distrib_blue", xlim=c(0,1), xlab = "", col=grayscale_colors)


hist(lrn[[1]], breaks=100, main ="distrib_red_norm", xlab = "", col=grayscale_colors)
hist(lrn[[2]], breaks=100, main ="distrib_green_norm", xlab = "", col=grayscale_colors)
hist(lrn[[3]], breaks=100, main ="distrib_blue_norm", xlab = "", col=grayscale_colors)

hist(lrexR[[1]], breaks=100, main ="distrib_ex_red",  xlab = "", col=grayscale_colors)
hist(lrexG[[2]], breaks=100, main ="distrib_ex_green", xlab = "", col=grayscale_colors)
hist(lrexB[[3]], breaks=100, main ="distrib_ex_blue", xlab = "", col=grayscale_colors)



plot(lrexGR[[1]], main="ExGreen minus ExRed", asp = nrow(vis.red)/ncol(vis.red), col=grayscale_colors)
hist(lrexGR[[1]], breaks=100, main ="distrib_ExGreen - ExRed", xlab = "", col=grayscale_colors)

plot(lrCIVE[[1]], main= "CIVE",asp = nrow(vis.red)/ncol(vis.red), col=grayscale_colors)
hist(lrCIVE[[1]], breaks=100, main ="distrib_CIVE", xlab = "", col=grayscale_colors)

plot(lrVEG[[1]], main="VEG", asp = nrow(vis.red)/ncol(vis.red), col=rev(grayscale_colors))
hist(lrVEG[[1]], breaks=100, main ="distrib_VEG", xlab = "", col=grayscale_colors)

