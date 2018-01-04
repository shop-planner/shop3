(define (problem strips-sat-x-1)
(:domain satellite)
(:objects
	satellite0 - satellite
	instrument0 - instrument
	instrument1 - instrument
	instrument2 - instrument
	satellite1 - satellite
	instrument3 - instrument
	instrument4 - instrument
	instrument5 - instrument
	satellite2 - satellite
	instrument6 - instrument
	instrument7 - instrument
	instrument8 - instrument
	thermograph0 - mode
	image2 - mode
	spectrograph1 - mode
	GroundStation2 - direction
	GroundStation1 - direction
	GroundStation0 - direction
	Star3 - direction
	Star4 - direction
	Phenomenon5 - direction
	Phenomenon6 - direction
	Star7 - direction
	Phenomenon8 - direction
	Planet9 - direction
)
(:init
	(supports instrument0 image2)
	(supports instrument0 thermograph0)
	(supports instrument0 spectrograph1)
	(calibration_target instrument0 GroundStation2)
	(supports instrument1 thermograph0)
	(supports instrument1 spectrograph1)
	(supports instrument1 image2)
	(calibration_target instrument1 GroundStation1)
	(supports instrument2 image2)
	(calibration_target instrument2 GroundStation0)
	(on_board instrument0 satellite0)
	(on_board instrument1 satellite0)
	(on_board instrument2 satellite0)
	(power_avail satellite0)
	(pointing satellite0 Phenomenon8)
	(supports instrument3 spectrograph1)
	(supports instrument3 thermograph0)
	(calibration_target instrument3 GroundStation0)
	(supports instrument4 image2)
	(supports instrument4 spectrograph1)
	(calibration_target instrument4 GroundStation2)
	(supports instrument5 image2)
	(supports instrument5 spectrograph1)
	(supports instrument5 thermograph0)
	(calibration_target instrument5 GroundStation1)
	(on_board instrument3 satellite1)
	(on_board instrument4 satellite1)
	(on_board instrument5 satellite1)
	(power_avail satellite1)
	(pointing satellite1 GroundStation2)
	(supports instrument6 image2)
	(calibration_target instrument6 GroundStation1)
	(supports instrument7 image2)
	(supports instrument7 thermograph0)
	(calibration_target instrument7 GroundStation1)
	(supports instrument8 spectrograph1)
	(supports instrument8 image2)
	(supports instrument8 thermograph0)
	(calibration_target instrument8 GroundStation0)
	(on_board instrument6 satellite2)
	(on_board instrument7 satellite2)
	(on_board instrument8 satellite2)
	(power_avail satellite2)
	(pointing satellite2 Phenomenon5)
)
(:goal (and
	(pointing satellite0 Phenomenon5)
	(pointing satellite1 GroundStation2)
	(have_image Star3 thermograph0)
	(have_image Phenomenon5 image2)
	(have_image Phenomenon6 image2)
	(have_image Star7 thermograph0)
	(have_image Phenomenon8 image2)
	(have_image Planet9 spectrograph1)
))

)
