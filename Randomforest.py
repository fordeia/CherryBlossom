import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_squared_error, r2_score
import matplotlib.pyplot as plt
   
# Load the cherry blossom dataset
DataTot = pd.read_excel(r"C:/Users/fordeia/CherryBlossom/bootdataCBloom4_24.xlsx")
data = pd.read_excel(r"C:/Users/fordeia/CherryBlossom/trainCB.xlsx")
Data = pd.read_excel(r"C:/Users/fordeia/CherryBlossom/testCB.xlsx")

# Prepare training data
X_train = data.drop(columns=['PEAK'])
y_train = data['PEAK']

# Prepare test data
X_test = Data.drop(columns=['PEAK'])
y_test = Data['PEAK']

# Create and train the Random Forest Regression model
rf_regressor = RandomForestRegressor(n_estimators=100, random_state=42)
rf_regressor.fit(X_train, y_train)

# Make predictions on the test data
y_pred = rf_regressor.predict(X_test)

# Evaluate the model
mse = mean_squared_error(y_test, y_pred)
r2 = r2_score(y_test, y_pred)
print(f"Mean Squared Error (MSE): {mse:.3f}")
print(f"R-squared (R²): {r2:.3f}")

# Plot Actual vs Predicted Peak Bloom Dates
plt.figure(figsize=(8, 6))
plt.scatter(y_test, y_pred, alpha=0.7, edgecolors='k')
plt.plot([y_test.min(), y_test.max()], [y_test.min(), y_test.max()], 'r--', linewidth=2)  # 1:1 reference line
plt.title("Actual vs Predicted Peak Bloom Dates of Cherry Blossoms", fontsize=14)
plt.xlabel("Actual Peak Bloom Date (Days from January 1)", fontsize=12)
plt.ylabel("Predicted Peak Bloom Date (Days from January 1)", fontsize=12)
plt.grid(True)
plt.tight_layout()
plt.show()

# Feature Importance Plot
feature_importance = rf_regressor.feature_importances_
feature_names = X_train.columns

plt.figure(figsize=(10, 6))
plt.barh(feature_names, feature_importance, color='skyblue')
plt.xlabel("Feature Importance", fontsize=12)
plt.title("Random Forest Feature Importance for Predicting Peak Bloom Dates", fontsize=14)
plt.gca().invert_yaxis()  # Highest importance at the top
plt.tight_layout()
plt.show()

