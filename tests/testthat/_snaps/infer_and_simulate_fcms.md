# print.infer_conventional_fcm works

    Code
      test_infer
    Output
      fcmconfr: conventional 
       $inference
         C1: 0.5
         C2: -0.1
         C3: 0
         C4: -0.07
         C5: 0.03
         C6: 0.02
       $inference_for_plotting
         - inference data transformed to streamline plotting with ggplot 
       $inference_state_vectors
         - inferences across all iterations of the simulation 
       $scenario_simulation
       $baseline_simulation

# print.infer_ivfn_or_tfn_fcm works

    Code
      test_sim
    Output
      fcmconfr: ivfn or tfn 
       $inference_df
         : [0.5, 0.5] (0.5)
         : [-0.23, 0.09] (-0.07)
         : [0, 0] (0)
         : [-0.26, 0.13] (-0.07)
         : [-0.07, 0.16] (0.05)
         : [-0.1, 0.11] (0)
       $inference_for_plotting
         - inference data transformed to streamline plotting with ggplot 
       $inference_state_vectors
         - inferences as fuzzy sets across all iterations of the simulation 
       $crisp_inference_state_vectors
         - inferences as crisp_values across all iterations of the simulation 
       $scenario_simulation
       $baseline_simulation

