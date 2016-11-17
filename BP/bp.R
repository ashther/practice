
w1 <- matrix(c(0.15, 0.2, 0.25, 0.3), 2)
w2 <- matrix(c(0.4, 0.45, 0.5, 0.55), 2)

b1 <- rep(0.35, 2)
b2 <- rep(0.6, 2)

i <- c(0.05, 0.1)
target <- c(0.01, 0.99)

alpha <- 0.5


forward <- function(i, w, b) {
  1 / (1 + exp(-1 * (sweep(i %*% w, 2, b, `+`))))
}

errTotal <- function(target, o) {
  sum(0.5 * (target - o) ^ 2)
}

# ∂Err/∂w2 = ∂Err/∂out * ∂out/∂net * ∂net/∂w2
# 
# ∂Err/∂out = out - target
# ∂out/∂net = out * (1 - out) 逻辑函数
# ∂net/∂w2 = h
# 
# new_w2 = w2 - α * ∂Err/∂w2
backwardOutToHidden <- function(err, target, o, h, alpha, w) {
  sweep(w, 2, alpha * ((o - target) * o * (1 - o)) * h)
}

# ∂Err/∂w1 = ∂Err/∂out_h * ∂out_h/∂net_h * ∂net_h/∂w1
# 
# ∂out_h/∂net_h = out_h * (1 - out_h) 逻辑函数
# ∂net/∂w1 = i
# ∂Err/∂out_h = Σ ∂Ei/∂out_h
#             = Σ ∂Ei/∂out       * ∂out/∂net       * ∂net/∂out_h
#             = Σ (out - target) * out * (1 - out) * w2
# 
# new_w1 = w1 - α * ∂Err/∂w1
backwardHiddenToInput <- function(err, target, o, h, i, alpha, w1, w2) {
  sweep(w1, 2, alpha * ((o - target) * o * (1 - o)) %*% t(w2) * h * (1 - h) * i)
}

h <- forward(i, w1, b1)
o <- forward(h, w2, b2)
err <- errTotal(target, o)

w2_new <- backwardOutToHidden(err, target, o, h, alpha, w2)
w1_new <- backwardHiddenToInput(err, target, o, h, i, alpha, w1, w2)