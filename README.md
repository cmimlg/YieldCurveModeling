# YieldCurveModeling
Yield Curve Modeling Using Dynamic Gaussian Processes
This code requires the var R package and the GPy, pandas python modules. The code for this project consists of the following files:
(1) Yield_Curve_Data_Downloader.R is the code to download treasury data.
(2) Generate_10Y_TSNS_Coeffs.R is the code to create Nelson Siegel model coefficients for 10 years.
(3)Seq_TS_NS_Estimation.R is the code to predict NS model coefficients based on previous years coefficient value.
(4) Seq_TS_MV_Estimation.R s the code to predict yields based on previous years yield values.
(5) dgp_ycm.py implementation of the dynamic gaussian process based on the GPy library. Run the seq_pred() method for 10 year prediction. The model parameters for the GP are constrained to be positive. The optimization code in GPy issues warnings when this condition is specified. Ignore these warnings.
