# Analysis-of-Dow-Jones-Industrial-Average-Returns-using-ARIMA

**Goal:**

The main goal of our project, analysis of Dow Jones Industrial Average Returns using ARIMA,
is to understand the stochastic trends of DJI and more forecasts.

**Method & Findings:**

After conducting an initial exploratory data analysis, we followed the Box-Jenkins approach and
fitted an ARIMA(2,0,2) model through the DJI daily stock returns since it had the lowest AICc
score amongst the different ARIMA models. We found that the data is stationary. In the Box
Jenkins approach, we initially tried the ACF, PACF and EACF but they were not very helpful
and we were not able to find appropriate parameters for AR and MA. Therefore, to determine
values for the parameters of a model we resorted to using Maximum Likelihood Estimation and
the Conditional sum-of-squares methods.

We noticed that the forecasts were failing to accurately capture the fluctuations in our data. This
was due to the fact that the DJI daily returns are uncorrelated. We figured that this must be
caused because of volatility clustering. We then modeled volatility using a GARCH model and
made the case that DJI daily stock returns saw fluctuations at the same time window as spikes in
volatility. Finally, to implement ARIMA modeling we introduce correlation by taking the square
of the DJI daily stock returns.

We were then able to fit an ARIMA (4,1,2) model by following the Box-Jenkins approach again.
We compared and contrasted volatility in the ARIMA model with returns and ARIMA with
returns squared model and found out that the latter were able to get better forecasts (though they
were only marginally better).

**Limitations:**

Further analysis can be conducted using ARCH/GARCH models instead of ARIMA models as
they are better suited to modeling financial data. They can capture the fluctuations and the
heteroskedasticity in the stock market to a greater degree than ARIMA models. However, these
models are beyond the scope of this report.

**Conclusion:**

Since forecasts generated by ARIMA(4,1,2) are not significantly better than those of
ARIMA(2,0,2) and following the principle of parsimony, we decide to stick with ARIMA(2,0,2)
for our analysis.
