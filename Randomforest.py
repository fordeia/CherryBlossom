import numpy as np
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_squared_error
import matplotlib.pyplot as plt

# Load the cherry blossom dataset

# Set the seed value
random.seed(41)

#loading datasets
DataTot = pd.read_excel(r"C:/Users/fordeia/CherryBlossom/bootdataCBloom4_24.xlsx")
data = pd.read_excel(r"C:/Users/fordeia/CherryBlossom/trainCB.xlsx")
Data = pd.read_excel(r"C:/Users/fordeia/CherryBlossom/testCB.xlsx")

#Training the model
X_train = data.drop(columns=['PEAK'])
y_train = data['PEAK']

# Create and train a Random Forest Regression model
rf_regressor = RandomForestRegressor(n_estimators=100, random_state=42)
rf_regressor.fit(X_train, y_train)

# Make predictions on the test data
X_test = Data.drop(columns=['PEAK'])
y_pred = rf_regressor.predict(X_test)

# Evaluate the model by calculating Mean Squared Error (MSE)
mse = mean_squared_error(y_test, y_pred)
print(f"Mean Squared Error: {mse}")

# Scatter Plot for Predicted vs. Actual Values
plt.figure(figsize=(8, 6))
plt.scatter(y_test, y_pred, alpha=0.7)
plt.title("Actual vs. Predicted Values")
plt.xlabel("Actual Values")
plt.ylabel("Predicted Values")
plt.show()

# Feature Importance Plot
feature_importance = rf_regressor.feature_importances_
feature_names = data.feature_names

plt.figure(figsize=(10, 6))
plt.barh(feature_names, feature_importance, color='skyblue')
plt.xlabel("Feature Importance")
plt.title("Feature Importance Plot")
plt.gca().invert_yaxis()
plt.show()
