coef <- c(1, 2-.5, 1)

## Keep coefs at (1, 1-3, 1) move middle one around 2 to show the bifurcation.


dynamics <- function(x,y) {
  preimage <- x + 1i*y
  coef[1] + coef[2]*preimage + coef[3]*preimage^2
}
real_part <- function(x, y) {
  Re(dynamics(x, y))
}
imag_part <- function(x, y) {
  Im(dynamics(x, y))
}
quad <- makeFun(x^2 + b*x + c ~ x, b=coef[2], c=coef[1])

dom <- list(x=c(-3, 3), y=c(-3, 3))

contour_plot(real_part(x, y) ~ x + y, domain=dom,
             contours_at = 0.00, labels=FALSE) %>%
contour_plot(imag_part(x, y) ~ x + y, domain=dom, contours_at = 0.00,
             contour_color = "red", filled = FALSE, labels=FALSE) %>%
  gf_refine(coord_fixed())

slice_plot(quad(x) ~ x, domain(x = c(-3,3))) %>%
  gf_hline(yintercept=0, color="red") %>%
  gf_lims(y=c(-5, 5))

## Need to display f(x) vs x for imag=0

