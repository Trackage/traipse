# traipse (development version)

* New function `track_query()` similar to `track_intermediate()` but for arbitrary
 points in time. In-development. 
 
* New function `track_intermediate()` to perform interpolation along a great circle
 between input locations. The interpolation step may be set by `distance` or `duration`
 and date-time is interpolated if included. 
 
* Metres, seconds, degrees, everywhere. 

# traipse 0.1.0

* Fixed Value documentation for various functions, thanks to CRAN feedback. 

* New functions `track_time`, `track_speed`,`track_distance_to()` and `track_bearing_to()`. 

* Added basic functions `track_distance`, `track_angle`, `track_turn`, and `track_bearing`. 
