# Equivariant Covariance Estimator
This R package implements the **Equivariant Covariance Estimator (ECE)** — a technique for estimating cross-sectional covariance in the presence of **change points** in the mean structure. The method is useful in time series or structured data where traditional covariance estimators can be biased due to non-stationarity.

📄 Learn more in our paper:  
➡️ [*Equivariant Variance Estimation for Multiple Change-Point Model*](https://arxiv.org/abs/2108.09431)


## Data Model
Let $\varepsilon \sim N(0, S)$ where:

$$
S = \begin{bmatrix}
\sigma_x^2 & \sigma_{xy} \\
\sigma_{xy} & \sigma_y^2
\end{bmatrix}
$$

Let $\theta_x$ and $\theta_y$ be $n$-length **piecewise constant** vectors. The observed data are modeled as:

- $X = \theta_x + \varepsilon_x$
- $Y = \theta_y + \varepsilon_y$

We estimate the **correlation**:

$$
\rho = \frac{\sigma_{xy}}{\sigma_x \sigma_y}
$$

The estimator tests the hypothesis:

$$
H_0: \rho = 0 \quad \text{vs.} \quad H_1: \rho \ne 0
$$

## Features

- Unbiased estimation of $\sigma_{xy}$ under structural change
- Hypothesis testing with p-values
- Works with simulated scenarios or user-supplied data

## Demo
```R
# Load Package
devtools::load_all(package_directory)

# Generate Data
scenario_num <- 11 # choose number between 1:11
params <- scenario(scenario_num, sxy=0, sx=1.2, sy=1.5, n=1000)
X <- generate_data(params)

# Get Estimates
ece.test(X[,1], X[,2])
```

