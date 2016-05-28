# tripcommon
shared tools for track data

There is a huge fragmentation of tools and approaches in R for tracking, see the Spatio-Temporal Task View section for examples. 

collect needed functions in this package

- path distance
- distance to source/target, absolute or cumulative
- turning angle (direction change) 
- track angle (internal angle of turn)
- Argos quality codes as ordered factor
- validation of basic data, and trip sense
- interpolation based on time step, distance
- cut for tracks
- examples and docs!


what about?

- known formats like Argos SMRU etc 


aims

- as fast as possible, vectorized or using Rcpp etc
- very lightweight, no classes or complications 
