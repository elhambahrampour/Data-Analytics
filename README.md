
# Data Analytics of NAHNES data

In this project, we work on the big NHANES data from the CDC to examine the causal impact of physical activity on BMI. It is crucial to consider the influence of educational level and wealth status.

>>>>>>> 

## Study Case

Do we need to tailor the physical activity recommendations to the individual’s wealth level?

##  Goal

To explore if this fact is true: "Everyone should engage in moderate or vigorous intensity sports, fitness, or recreational activities regardless of his economic status".

##  Method

We fitted a tailored model to the data, considering the relevant variables such as BMI, physical activity, wealth status, educational level, age, and diabetes to find the optimal recommendation for physical activity. We used the weighted ordinary least squares regression model using inverse probability weights.

## Results

The results indicate that “everyone should engage in moderate or vigorous intensity sports, fitness, or recreational activities”. The estimated optimal recommendation aligns with current recommendations on the benefits of physical activity for adults. Thus, there is no apparent benefit to tailoring the recommendations based on wealth status.

