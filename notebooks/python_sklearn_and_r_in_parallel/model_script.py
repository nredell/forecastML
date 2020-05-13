#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
import pandas as pd
from sklearn import linear_model
from sklearn.preprocessing import StandardScaler
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Model training function used in exploration and hyperparameter tuning.

def py_model_fun(data):  # A horizon-specific pd.DataFrame from forecastML::create_lagged_df().
  
  # A special attribute to identify the direct forecast horizon for 'data': 48, 96, and 144 here.
  # Using a setup like "if horizon == 48: 'my neural network code'" would allow you to train entirely different
  # models at each direct forecast horizon.
  horizon = int(data.horizons)
  
  X = data.iloc[:, 1:]
  y = data.iloc[:, 0]
  
  scaler = StandardScaler()
  X = scaler.fit_transform(X)
  
  model_lasso = linear_model.LassoCV(cv = 5)
  
  model_lasso.fit(X = X, y = y)
  
  # The returned object has no real restrictions. Here, we're returning meta-data from hyperparameters 
  # as well as the forecast 'horizon' which we'll use to uniquely name the .pkl files that we're saving to disk.
  return({'model': model_lasso, 'scaler': scaler, 'horizon': horizon})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Predict function that works on historical data and to produce future forecasts.

# 'model_dict' is the return() from 'py_model_fun' above. 
# 'data' is passed as a pd.DataFrame with the outcome column removed and a '.horizon' attribute if needed.
def py_predict_fun(model_dict, data):
  
  data = model_dict['scaler'].transform(data)
  
  data_pred = pd.DataFrame({'y_pred': model_dict['model'].predict(data)})
  
  return(data_pred)
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
def py_model_fun_final(data, hyperparameters):
  
  horizon = int(data.horizons)
  
  X = data.iloc[:, 1:]
  y = data.iloc[:, 0]
  
  scaler = StandardScaler()
  X = scaler.fit_transform(X)
  
  if horizon == 48:
    
      alpha = hyperparameters[str(horizon)]['alpha']
    
  elif horizon == 96:
      
      alpha = hyperparameters[str(horizon)]['alpha']
    
  elif horizon == 144:
      
      alpha = hyperparameters[str(horizon)]['alpha']
  
  model_lasso = linear_model.Lasso(alpha = alpha)
  
  model_lasso.fit(X = X, y = y)
  
  return({'model': model_lasso, 'scaler': scaler, 'horizon': horizon})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
