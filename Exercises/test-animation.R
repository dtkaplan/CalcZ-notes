# Test out an animation

library(Zcalc)

fraw <- rfun( ~ t, seed=9090)
f <- function(t) fraw(t-5)
F <- antiD(f(t) ~ t)
time <- seq(0, 10, by=1)

one_plot <- function(T, h=1.0) {
  fval <- f(T)
  yrange <- extendrange(range(f(time)), f=0.1)
  Yrange <- extendrange(range(F(time)), f=0.1)
  Segment <- tibble(
    tstart = T,
    tend =T+h,
    ystart = 0,
    yend = fval*h,
    fval = fval
  )
  pointer <- arrow(angle = 30, length = unit(0.25, "inches"),
             ends = "last", type = "open")
  P1 <- slice_plot(f(t) ~ t, domain(t=c(0,T))) %>%
    slice_plot(f(t) ~ t, domain(t = range(time)), alpha=0.25, size=2) %>%
    gf_segment(ystart + yend ~ tstart + tend,
               data = Segment, color="magenta", size=2,
               arrow = pointer) %>%
    gf_lims(x=range(time), y = yrange) %>%
    gf_text(I(mean(yrange*c(1.3, 0.7))) ~ 8, label="f", size=40, color="lightgray") %>%
    gf_hline(yintercept = ~ 0, color = "blue") %>%
    gf_point(fval ~ tstart, data = Segment, color="magenta", size=4) %>%
    gf_labs(y = "f(t)")
  SegmentF <- Segment %>% mutate(
    ystart = F(T) + ystart,
    yend   = F(T) + yend
  )
  P2 <- slice_plot(F(t) ~ t, domain(t=c(0,T))) %>%
    gf_segment(ystart + yend ~ tstart + tend,
               data = SegmentF, color="magenta", size=2,
               arrow = pointer) %>%
    gf_text(I(mean(Yrange*c(1.3, 0.7))) ~ 8, label="F", size=40, color="lightgray") %>%
    gf_lims(x=range(time), y = Yrange) %>%
    gf_labs(y = "F(t)")

  ggpubr::ggarrange(P1, P2, nrow=2)
}


saveGIF({
    for (T in seq(0.5 ,10.0, by=0.1)) {
      # dev.hold()
      print(one_plot(T, h=0.5))

    }
  }, movie.name = "brownian_motion.gif"
)

# Change frame rate shell command
# convert -delay 1x30 brownian_motion.gif brown2.gif

