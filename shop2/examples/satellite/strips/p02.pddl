(define (problem strips-sat-x-1)
(:domain satellite)
(:objects
	satellite0 - satellite
	instrument0 - instrument
	instrument1 - instrument
	infrared0 - mode
	infrared1 - mode
	image2 - mode
	GroundStation1 - direction
	Star0 - direction
	GroundStation2 - direction
	Planet3 - direction
	Planet4 - direction
	Phenomenon5 - direction
	Phenomenon6 - direction
	Star7 - direction
)
(:init
	(supports instrument0 infrared1)
	(supports instrument0 infrared0)
	(calibration_target instrument0 Star0)
	(supports instrument1 image2)
	(supports instrument1 infrared1)
	(supports instrument1 infrared0)
	(calibration_target instrument1 GroundStation2)
	(on_board instrument0 satellite0)
	(on_board instrument1 satellite0)
	(power_avail satellite0)
	(pointing satellite0 Planet4)
)
(:goal (and
	(have_image Planet3 infrared0)
	(have_image Planet4 infrared0)
	(have_image Phenomenon5 image2)
	(have_image Phenomenon6 infrared0)
	(have_image Star7 infrared0)
))

)
