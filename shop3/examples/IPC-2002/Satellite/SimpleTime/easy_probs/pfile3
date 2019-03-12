(define (problem strips-sat-x-1)
(:domain satellite)
(:objects
	satellite0 - satellite
	instrument0 - instrument
	instrument1 - instrument
	instrument2 - instrument
	satellite1 - satellite
	instrument3 - instrument
	image1 - mode
	infrared0 - mode
	spectrograph2 - mode
	Star1 - direction
	Star2 - direction
	Star0 - direction
	Star3 - direction
	Star4 - direction
	Phenomenon5 - direction
	Phenomenon6 - direction
	Phenomenon7 - direction
)
(:init
	(supports instrument0 spectrograph2)
	(supports instrument0 infrared0)
	(calibration_target instrument0 Star1)
	(supports instrument1 image1)
	(calibration_target instrument1 Star2)
	(supports instrument2 infrared0)
	(supports instrument2 image1)
	(calibration_target instrument2 Star0)
	(on_board instrument0 satellite0)
	(on_board instrument1 satellite0)
	(on_board instrument2 satellite0)
	(power_avail satellite0)
	(pointing satellite0 Star4)
	(supports instrument3 spectrograph2)
	(supports instrument3 infrared0)
	(supports instrument3 image1)
	(calibration_target instrument3 Star0)
	(on_board instrument3 satellite1)
	(power_avail satellite1)
	(pointing satellite1 Star0)
)
(:goal (and
	(pointing satellite0 Phenomenon5)
	(have_image Star3 infrared0)
	(have_image Star4 spectrograph2)
	(have_image Phenomenon5 spectrograph2)
	(have_image Phenomenon7 spectrograph2)
))
(:metric minimize (total-time))

)
