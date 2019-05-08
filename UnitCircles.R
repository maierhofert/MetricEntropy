# plot unit circles


# 0 norm
x0 = c(seq(-1, 1, length.out = 100),
       rep(0, 100))
y0 = c(rep(0, 100),
       seq(1, -1, length.out = 100))
plot(x0, y0)

# 1 norm
x1 = c(seq(-1, 0, length.out = 50),
       seq(-1, 0, length.out = 50),
       seq(0, 1, length.out = 50),
       seq(0, 1, length.out = 50))
y1 = c(seq(0, 1, length.out = 50),
       seq(0, -1, length.out = 50),
       seq(1, 0, length.out = 50),
       seq(-1, 0, length.out = 50))
plot(x1, y1)

# 2 norm
radius = 1
theta = seq(0, 2 * pi, length = 200)
x2 = radius * cos(theta)
y2 = radius * sin(theta)

theta1 = seq(0, pi/ 2, length = 50)
theta2 = seq(pi / 2, pi, length = 50)
theta3 = seq(pi, 1.5 * pi, length = 50)
theta4 = seq(1.5 * pi, 2 * pi, length = 50)

x2 = c(radius * cos(theta1),
       radius * cos(theta2),
       radius * cos(theta3),
       radius * cos(theta4))
y2 = c(radius * sin(theta1),
       radius * sin(theta2),
       radius * sin(theta3),
       radius * sin(theta4))
plot(x2, y2, type = "l")

# infinity norm
xinf = c(rep(-1, 50),
         seq(-1, 1, length.out = 50),
         seq(-1, 1, length.out = 50),
         rep(1, 50))
yinf = c(seq(-1, 1, length.out = 50),
         rep(-1, 50),
         rep(1, 50),
         seq(-1, 1, length.out = 50))
plot(xinf, yinf)


# create data set containing 
unit_balls = data.frame(x0 = x0, y0 = y0,
                        x1 = x1, y1 = y1,
                        x2 = x2, y2 = y2,
                        xinf = xinf, yinf = yinf,
                        sep = rep(1:4, each = 50))

library(ggplot2)
width = 7

# zero norm
ggplot(unit_balls) +
  geom_line(aes(x = x0, y = y0, group = sep)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  xlab("x") +
  ylab("y") +
  coord_fixed()
ggsave("plots/unit_circle_0.pdf", width = width, height = 0.9 * width)

# one norm
ggplot(unit_balls) +
  geom_line(aes(x = x1, y = y1, group = sep)) +
  xlab("x") +
  ylab("y") +
  coord_fixed()
ggsave("plots/unit_circle_1.pdf", width = width, height = 0.9 * width)

# two norm
ggplot(unit_balls) +
  geom_line(aes(x = x2, y = y2, group = sep)) +
  xlab("x") +
  ylab("y") + 
  coord_fixed()
ggsave("plots/unit_circle_2.pdf", width = width, height = 0.9 * width)

# infinity norm
ggplot(unit_balls) +
  geom_line(aes(x = xinf, y = yinf, group = sep)) +
  xlab("x") +
  ylab("y") + 
  coord_fixed()
ggsave("plots/unit_circle_inf.pdf", width = width, height = 0.9 * width)
