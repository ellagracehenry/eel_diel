by_time_df
library(rethinking)

bin_size = 30 

# Convert to data.table
dt <- as.data.table(by_time_df)

# Create factor for date if not already
dt[, date_f := as.factor(date)]
dt[, site := as.factor(site)]
dt[, colony := as.factor(colony)]

# Define bins by size in seconds
bin_breaks <- seq(0, max(dt$sec_since_midnight, na.rm = TRUE) + bin_size, by = bin_size)

# Aggregate directly
dt_sub <- dt[sec_since_midnight %% 30 == 0]

agg_dt <- dt_sub[, .(
  n_available = first(n_available),
  n_emerged   = first(n_emerged),
  prop_emerged = first(prop_emerged),
  n_obs = .N
), by = .(
  date_f, site, colony, colony_size, sec_since_midnight
)]

agg_dt[, sec_scaled := scale(sec_since_midnight)]
agg_dt[, colony_scaled := scale(colony_size)]


bysfit_prop <- ulam(
  alist(
    n_emerged ~ dbinom(n_available, p),
    logit(p) <- a +
      b_time * sec_scaled +
      b_colony * colony_scaled +
      b_site*site +
      a_date[date_f],
    
    # Priors
    a ~ dnorm(0, 1.5),
    b_time ~ dnorm(0, 1),
    b_colony ~ dnorm(0, 1),
    b_site ~ dnorm(0, 1),
    
    # Random intercepts
    a_date[date_f] ~ dnorm(0, sigma_date),
    
    sigma_date ~ dexp(1)
  ),
  data = agg_dt,
  chains = 4, cores = 4
)

