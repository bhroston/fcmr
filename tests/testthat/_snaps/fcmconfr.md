# streamlined fcmconfr works

    Code
      test
    Output
      fcmconfr: 4 input adj. matrices (conventional) 
      $inference
        - input_fcms: Inferences and data from the 4 input fcm adj. matrices.
        - aggregate_fcm: Inferences and data from the aggregate (mean) of the 4 input fcm adj. matrices.
        - monte_carlo_fcms: Inferences of data from the 100 fcms constructed from the 4 input fcm adj. matrices. 
      $bootstrap
        - CIs_about_means_and_quantiles_by_node: 0.95% CI of means of inferences and quantiles by node
        - bootstrapped_means: 1000 
      $params
        - simulation_opts: act = kosko; squash = sigmoid; lambda = 1 
        - additional_opts: Perform Aggregate Analysis = TRUE; Perform MC Analysis = TRUE

---

    Code
      test
    Output
      fcmconfr: 2 input adj. matrices (ivfn) 
      $inference
        - input_fcms: Inferences and data from the 2 input fcm adj. matrices.
        - aggregate_fcm: Inferences and data from the aggregate (mean) of the 2 input fcm adj. matrices.
        - monte_carlo_fcms: Inferences of data from the 100 fcms constructed from the 2 input fcm adj. matrices. 
      $bootstrap
        - CIs_about_means_and_quantiles_by_node: 0.95% CI of means of inferences and quantiles by node
        - bootstrapped_means: 1000 
      $params
        - simulation_opts: act = kosko; squash = sigmoid; lambda = 1 
        - additional_opts: Perform Aggregate Analysis = TRUE; Perform MC Analysis = TRUE

---

    Code
      test
    Output
      fcmconfr: 1 input adj. matrices (tfn) 
      $inference
        - input_fcms: Inferences and data from the 1 input fcm adj. matrices. 
      $params
        - simulation_opts: act = kosko; squash = sigmoid; lambda = 1 
        - additional_opts: Perform Aggregate Analysis = FALSE; Perform MC Analysis = FALSE

# print.fcmconfr works

    Code
      print(test)
    Output
      fcmconfr: 4 input adj. matrices (conventional) 
      $inference
        - input_fcms: Inferences and data from the 4 input fcm adj. matrices.
        - aggregate_fcm: Inferences and data from the aggregate (mean) of the 4 input fcm adj. matrices.
        - monte_carlo_fcms: Inferences of data from the 100 fcms constructed from the 4 input fcm adj. matrices. 
      $bootstrap
        - CIs_about_means_and_quantiles_by_node: 0.95% CI of means of inferences and quantiles by node
        - bootstrapped_means: 1000 
      $params
        - simulation_opts: act = kosko; squash = sigmoid; lambda = 1 
        - additional_opts: Perform Aggregate Analysis = TRUE; Perform MC Analysis = TRUE

---

    Code
      print(test)
    Output
      fcmconfr: 4 input adj. matrices (conventional) 
      $inference
        - input_fcms: Inferences and data from the 4 input fcm adj. matrices.
        - aggregate_fcm: Inferences and data from the aggregate (mean) of the 4 input fcm adj. matrices.
        - monte_carlo_fcms: Inferences of data from the 100 fcms constructed from the 4 input fcm adj. matrices. 
      $params
        - simulation_opts: act = kosko; squash = sigmoid; lambda = 1 
        - additional_opts: Perform Aggregate Analysis = TRUE; Perform MC Analysis = TRUE

---

    Code
      test
    Output
      fcmconfr: 4 input adj. matrices (conventional) 
      $inference
        - input_fcms: Inferences and data from the 4 input fcm adj. matrices.
        - monte_carlo_fcms: Inferences of data from the 100 fcms constructed from the 4 input fcm adj. matrices. 
      $params
        - simulation_opts: act = kosko; squash = sigmoid; lambda = 1 
        - additional_opts: Perform Aggregate Analysis = FALSE; Perform MC Analysis = TRUE

---

    Code
      test
    Output
      fcmconfr: 4 input adj. matrices (conventional) 
      $inference
        - input_fcms: Inferences and data from the 4 input fcm adj. matrices.
        - aggregate_fcm: Inferences and data from the aggregate (mean) of the 4 input fcm adj. matrices.
       
      $params
        - simulation_opts: act = kosko; squash = sigmoid; lambda = 1 
        - additional_opts: Perform Aggregate Analysis = TRUE; Perform MC Analysis = FALSE

---

    Code
      test
    Output
      fcmconfr: 4 input adj. matrices (conventional) 
      $inference
        - input_fcms: Inferences and data from the 4 input fcm adj. matrices. 
      $params
        - simulation_opts: act = kosko; squash = sigmoid; lambda = 1 
        - additional_opts: Perform Aggregate Analysis = FALSE; Perform MC Analysis = FALSE

