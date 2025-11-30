##### Eel Simulation #####

# this script simulates garden eels going up and down

simulate_eels = function(N = 10, T = 10000, mean_duration = 365) {
    
    switch_prob = 1 / mean_duration
    
    # initialize states randomly (0 = down, 1 = up)
    states = sample(c(0,1), N, replace = TRUE)
    
    # storage matrix: rows = time, cols = eels
    state_record = matrix(NA, nrow = T, ncol = N)
    
    for(t in 1:T) {
        
        # record current states
        state_record[t, ] = states
        
        # determine who switches
        switches = runif(N) < switch_prob
        
        # toggle states: 0→1 or 1→0
        states[switches] = 1 - states[switches]
    }
    
    # convert matrix → long dataframe
    df = as.data.frame(state_record)
    df$time = 1:T
    
    df_long = reshape(
        df,
        varying = 1:N,
        v.names = "state",
        timevar = "eel",
        times = 1:N,
        direction = "long"
    )
    
    rownames(df_long) = NULL
    return(df_long)
}

eels = simulate_eels()
eels
library(ggplot2)

plot_eels = function(df_long) {
    
    ggplot(df_long, aes(x = time, y = eel, fill = factor(state))) +
        geom_tile() +
        scale_fill_manual(values = c("white", "black")) +
        scale_y_reverse() +          # eel 1 at top (optional)
        theme_minimal() +
        theme(
            panel.grid = element_blank(),
            axis.text.y = element_text(size = 6),
            legend.position = "none"
        ) +
        labs(
            x = "Time (seconds)",
            y = "Eel ID"
        )
}

plot_eels(eels)

simulate_eels_social_up = function(
        N = 10, T = 10000, mean_dwell = 365,
        social_strength = 0.005
        
    ) {
        
        p_switch_base = 1 / mean_dwell
        
        # initial states
        states = sample(c(0,1), N, replace = TRUE)
        
        # record matrix
        state_record = matrix(NA, nrow = T, ncol = N)
        
        for(t in 1:T) {
            
            state_record[t, ] = states
            
            total_up = sum(states)
            frac_up_others = (total_up - states) / (N - 1)
            
            # switching probabilities IN and OUT of up state
            p_down_to_up = p_switch_base + social_strength * (frac_up_others - 0.5)
            p_up_to_down = p_switch_base + social_strength * (0.5 - frac_up_others)
            
            # clamp
            p_down_to_up = pmin(pmax(p_down_to_up, 0), 1)
            p_up_to_down = pmin(pmax(p_up_to_down, 0), 1)
            
            # update eels
            for(i in 1:N) {
                if(states[i] == 0) {
                    if(runif(1) < p_down_to_up[i]) states[i] = 1
                } else {
                    if(runif(1) < p_up_to_down[i]) states[i] = 0
                }
            }
        }
        
        # reshape long
        df = as.data.frame(state_record)
        df$time = 1:T
        
        df_long = reshape(
            df,
            varying = 1:N,
            v.names = "state",
            timevar = "eel",
            times = 1:N,
            direction = "long"
        )
        
        rownames(df_long) = NULL
        return(df_long)
    }
eels_social = simulate_eels_social_up()
plot_eels(eels_social)
