# this is from Danielle Navarro's aRt programming

# https://www.youtube.com/watch?v=ZUyahWLWVzY&list=PLRPB0ZzEYegNYW3ksiK3dvd6S4HMfKj1n

# Preliminaries -----------------------------------------------------------

# load packages
library(tidyverse)
library(ambient)
library(scico)
library(here)

# parameters
art_par <- list(
  seed = 123456789,
  n_paths = 200, # this allows to draw some random numbers - runif(n = 10, min = 0, max = 3)
  n_steps = 40
  )



# set up the canvas -------------------------------------------------------

set.seed(seed = art_par$seed)

state <- tibble(
  x = runif(n = art_par$n_paths, min = 0, max = 4),
  y = runif(n = art_par$n_paths, min = 0, max = 4)
)

# Include path id and step id in state
state <- state %>% 
  mutate(
    path_id = 1:art_par$n_paths,
    step_id = 1
  )

# Keep track of series of states
art_dat <- state


# create the art in loop --------------------------------------------------

stop_painting <- FALSE

while(stop_painting == FALSE) {
  state <- state %>% 
    mutate(
      x = x +.1,
      step_id = step_id + 1,
    )
  
  # append state to app_dat
  art_dat <- bind_rows(art_dat, state)
  
  # check if we need to sotp the artz
  current_step <- last(state$step_id)
  if(last(state$step_id) >= art_par$n_steps) {
    stop_painting == TRUE
  }
}


# draw --------------------------------------------------------------------

pic <- ggplot(
  data = art_dat,
  mapping = aes(x = x, y = y)) +
    geom_path()
  
print(pic)
