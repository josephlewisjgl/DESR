library(dplyr)
library(simmer)
library(simmer.plot)

# application one: meeting Lilo and Stitch
# set up a trajectory
trajectory <- trajectory("Lilo and Stitch") %>%
  log_("Ready to see character...") %>%
  # meet a character
  seize("character") %>%
  # for a time distributed around 5 time steps
  timeout(function() rnorm(1, 5, 1)) %>% 
  # release the character
  release("character") %>%
  log_("Another happy guest!")

# set up the environment and simulation
theme_park <- simmer("park") %>%
  # add a character resource with capacity two (Lilo and Stitch)
  add_resource("character", 2) %>%
  # add the customer generatory with the trajectory and function to generate
  add_generator("customer", trajectory, function () rexp(1, 1/8))

theme_park %>% run(15*60)

# plot the resource utilisation
resources <- get_mon_resources(theme_park)
plot(resources, metric = "utilization")

# plot the wait times
arrivals <- get_mon_arrivals(theme_park)
plot(arrivals, metric = "waiting_time")


# application two: meeting Lilo and Stitch with fast passes
# retain the trajectory from above
# set up the environment and simulation
theme_park <- simmer("park") %>%
  add_resource("character", 2) %>%
  # reduce the rate at which customers are generated assuming some will get fast 
  # passes for later on in the day
  add_generator("customer", trajectory, function () rexp(1, 1/5)) %>%
  # set up fast pass customers who have tickets to meet every 50 steps
  add_generator("fast_pass_customer", trajectory, function() (50), priority=1)

theme_park %>% run(15*60)

# plot the resource utilisation
resources <- get_mon_resources(theme_park)
plot(resources, metric = "utilization")

# plot the wait times
arrivals <- get_mon_arrivals(theme_park)
plot(arrivals, metric = "waiting_time")


# application three: meeting Lilo and Stitch with reneging
# set up an updated trajectory that includes reneging
trajectory <- trajectory("Lilo and Stitch") %>%
  log_("Ready to see character...") %>%
  # set up a time to renege distributed around 2 time steps
  renege_in(function() rnorm(1,2,1),
            # set up an out trajectory for the reneging customers
            out = trajectory("reneging") %>%
              log_("I'm leaving!")) %>%
  # meet a character
  seize("character") %>%
  # abort the renege (if they beat the queue they no longer want to leave)
  renege_abort() %>%
  # for a time distributed around 5 time steps
  timeout(function() rnorm(1, 5, 1)) %>% 
  # release the character
  release("character") %>%
  log_("Another happy guest!")

# set up the environment and simulation retaining the fast pass customers
theme_park <- simmer("park") %>%
  add_resource("character", 2) %>%
  add_generator("customer", trajectory, function () rexp(1, 1/5)) %>%
  add_generator("fast_pass_customer", trajectory, function() (50), priority=1)

theme_park %>% run(until=15*60)

# plot the resource utilisation
resources <- get_mon_resources(theme_park)
plot(resources, metric = "utilization")

# plot the wait times
arrivals <- get_mon_arrivals(theme_park)
plot(arrivals, metric = "waiting_time")

# application four: Space Mountain with one cart using batching
# set up an updated trajectory that adds batching and switches the resource
# to a cart
trajectory <- trajectory("Space Mountain Cart") %>%
  log_("Ready to ride ...") %>%
  # set up a time to renege
  renege_in(function() rnorm(1,40,1),
            out = trajectory("reneging") %>%
              log_("I'm leaving!")) %>%
  # batch the guests into a cart for 6
  batch(n=6, timeout=10) %>%
  # the abort can stop here as once in a batch the ride group is set
  renege_abort() %>%
  seize('cart') %>%
  log_("Riding!") %>%
  # for a time distributed around 10 time steps
  timeout(function() rnorm(1, 10, 1)) %>% 
  # release the cart as a batch
  release('cart') %>%
  # unbatch
  separate() %>%
  log_("Another happy guest!")

# set up the environment and simulation retaining the fast pass customers
theme_park <- simmer("park") %>%
  add_resource("cart", 2) %>%
  add_generator("customer", trajectory, function () rexp(1, 1/5)) %>%
  add_generator("fast_pass_customer", trajectory, function() (50), priority=1)

theme_park %>% run(until=15*60)

# plot the resource utilisation
resources <- get_mon_resources(theme_park)
plot(resources, metric = "utilization")

# plot the wait times
arrivals <- get_mon_arrivals(theme_park)
plot(arrivals, metric = "waiting_time")


# application five: adding branches for queue preferences
# create function for the two ride options
rideSpaceMountain <- function(traj) {
  traj %>%
    log_('Chosen Space Mountain') %>%
    renege_in(function() rnorm(1,40,1),
              out = trajectory("reneging") %>%
                log_("I'm leaving!")) %>%
    batch(n=6, timeout=10) %>%
    renege_abort() %>%
    seize('Space Mountain Cart') %>%
    log_("Riding!") %>%
    # for a time distributed around 10 time steps
    timeout(function() rnorm(1, 15, 1)) %>% 
    release('Space Mountain Cart') %>%
    # release the character
    separate()
} 

# the splash mountain ride differs slightly - this will change later
rideSplashMountain <- function(traj) {
  traj %>%
    log_('Chosen Splash Mountain') %>%
    renege_in(function() rnorm(1,30,1),
              out = trajectory("reneging") %>%
                log_("I'm leaving!")) %>%
    batch(n=8, timeout=5) %>%
    renege_abort() %>%
    seize('Splash Mountain Cart') %>%
    log_("Riding!") %>%
    # for a time distributed around 10 time steps
    timeout(function() rnorm(1, 8, 1)) %>% 
    release('Splash Mountain Cart') %>%
    # release the character
    separate()
}

trajectory <- trajectory("Going on Rides") %>%
  log_("Ready to ride ...") %>%
  branch(function() sample(c(1, 2), 1), continue = c(T, T),
    trajectory() %>%
      rideSpaceMountain(),
    trajectory() %>%
      rideSplashMountain()
  ) %>%
  log_("Another happy guest!")

# set up the environment and simulation this time using 2 individual resource
# each with a capacity of two
theme_park <- simmer("park") %>%
  add_resource("Space Mountain Cart", 2) %>%
  add_resource("Splash Mountain Cart", 2) %>%
  add_generator("customer", trajectory, function () rexp(1, 1/5)) %>%
  add_generator("fast_pass_customer", trajectory, function() (50), priority=1)

theme_park %>% run(until=15*60)

# plot the resource utilisation
resources <- get_mon_resources(theme_park)
plot(resources, metric = "utilization")

# plot the wait times
arrivals <- get_mon_arrivals(theme_park)
plot(arrivals, metric = "waiting_time")


# application six: cleaning up and adding a park opening time
# create the ride path which can be altered with parameters for diff rides
ridePath <- function(traj, resource_name, renege_time = 30, ride_time = 8, 
                     batch_timeout = 5, batch_size = 8) {
  traj %>%
    log_('Chosen Splash Mountain') %>%
    renege_in(function() rnorm(1, renege_time, 1),
              out = trajectory("reneging") %>%
                log_("I'm leaving!")) %>%
    batch(n=batch_size, timeout=batch_timeout) %>%
    renege_abort() %>%
    seize(resource_name) %>%
    log_("Riding!") %>%
    timeout(function() rnorm(1, ride_time, 1)) %>% 
    release(resource_name) %>%
    separate()
}


# set up a character path that can be tailored to individual characters
characterPath <- function(traj, characters, policy = 'round-robin',
                          visit_time = 5) {
  traj %>% 
    log_("Ready to see character...") %>%
    # add a select option so that guests must select a character to queue for
    select(resources = characters, policy = policy) %>%
    # meet a character
    seize_selected() %>%
    # for a time distributed around 5 time steps
    timeout(function() rnorm(1, visit_time, 1)) %>% 
    # release the character
    release_selected()
    
}

# set up the full trajectory
guestPath <- trajectory('guestPath') %>%
  log_('Arrived at the park') %>%
  # once park gates open guests seize to enter
  seize('Park gates') %>%
  # then immediately release and they are in the park
  release('Park gates') %>%
  log_('The park is open!') %>%
  # add the branch logic from above for a 50:50 chance of Space or Splash ride
  branch(function() sample(c(1, 2), 1), continue = c(T, T),
         trajectory() %>%
           # call in custom func to go on ride with params set for Space
           ridePath(., 'Space Mountain Cart', 40, 12, 10, 6),
         trajectory() %>%
           # then do the same for Splash
           ridePath(., 'Splash Mountain Cart', 10, 8, 3, 10)
  ) %>%
  log_('Ride finished time to find a character') %>%
  # send guests down a character path before leaving the park
  characterPath(., c('Lilo', 'Stitch')) %>%
  log_('Another happy guest!')
  
# set up a schedule for park opening (open at 10 close at 210)
gate_schedule <- schedule(c(0, 50, (15*60)-50), c(0, Inf, 0))

# set up the environment adding all of the things we have combined
theme_park <- simmer("park") %>%
  # add the gate with the gate schedule
  add_resource('Park gates', gate_schedule) %>%
  # add the rides as above
  add_resource("Space Mountain Cart", 5) %>%
  add_resource("Splash Mountain Cart", 5) %>%
  # add the characters this time they are split so that guests select just one
  add_resource("Stitch", 1) %>%
  add_resource("Lilo", 1) %>%
  # add generators for the customer
  add_generator("customer", guestPath, function () rexp(1, 1/5)) %>%
  # then for the fast pass customer at regular intervals with higher priority
  add_generator("fast_pass_customer", guestPath, function() (50), priority=1)

# run the final theme park simulation
theme_park %>% run(until=15*60)

# plot the resource utilisation
resources <- get_mon_resources(theme_park)
plot(resources, metric = "utilization")

# plot the wait times
arrivals <- get_mon_arrivals(theme_park)
plot(arrivals, metric = "waiting_time")

