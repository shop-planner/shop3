(define (problem strips-sat-x-1)
(:domain satellite)
(:objects
	satellite0 - satellite
	instrument0 - instrument
	instrument1 - instrument
	instrument2 - instrument
	satellite1 - satellite
	instrument3 - instrument
	satellite2 - satellite
	instrument4 - instrument
	instrument5 - instrument
	instrument6 - instrument
	satellite3 - satellite
	instrument7 - instrument
	instrument8 - instrument
	satellite4 - satellite
	instrument9 - instrument
	instrument10 - instrument
	satellite5 - satellite
	instrument11 - instrument
	thermograph4 - mode
	image1 - mode
	thermograph3 - mode
	image2 - mode
	thermograph0 - mode
	GroundStation3 - direction
	GroundStation4 - direction
	GroundStation2 - direction
	GroundStation0 - direction
	GroundStation1 - direction
	Phenomenon5 - direction
	Phenomenon6 - direction
	Phenomenon7 - direction
	Planet8 - direction
	Star9 - direction
	Star10 - direction
	Phenomenon11 - direction
	Phenomenon12 - direction
	Phenomenon13 - direction
	Star14 - direction
	Planet15 - direction
	Planet16 - direction
	Planet17 - direction
	Phenomenon18 - direction
	Star19 - direction
	Star20 - direction
	Planet21 - direction
	Star22 - direction
	Planet23 - direction
	Star24 - direction
)
(:init
	(supports instrument0 thermograph0)
	(supports instrument0 image1)
	(calibration_target instrument0 GroundStation2)
	(supports instrument1 image2)
	(supports instrument1 thermograph3)
	(calibration_target instrument1 GroundStation0)
	(supports instrument2 image1)
	(supports instrument2 thermograph3)
	(supports instrument2 thermograph4)
	(calibration_target instrument2 GroundStation2)
	(on_board instrument0 satellite0)
	(on_board instrument1 satellite0)
	(on_board instrument2 satellite0)
	(power_avail satellite0)
	(pointing satellite0 Phenomenon12)
	(supports instrument3 thermograph0)
	(supports instrument3 thermograph4)
	(supports instrument3 image2)
	(calibration_target instrument3 GroundStation2)
	(on_board instrument3 satellite1)
	(power_avail satellite1)
	(pointing satellite1 GroundStation1)
	(supports instrument4 thermograph4)
	(supports instrument4 image1)
	(supports instrument4 thermograph0)
	(calibration_target instrument4 GroundStation1)
	(supports instrument5 thermograph4)
	(calibration_target instrument5 GroundStation4)
	(supports instrument6 thermograph3)
	(supports instrument6 image1)
	(calibration_target instrument6 GroundStation0)
	(on_board instrument4 satellite2)
	(on_board instrument5 satellite2)
	(on_board instrument6 satellite2)
	(power_avail satellite2)
	(pointing satellite2 GroundStation2)
	(supports instrument7 image2)
	(supports instrument7 thermograph3)
	(calibration_target instrument7 GroundStation4)
	(supports instrument8 thermograph4)
	(supports instrument8 thermograph0)
	(calibration_target instrument8 GroundStation2)
	(on_board instrument7 satellite3)
	(on_board instrument8 satellite3)
	(power_avail satellite3)
	(pointing satellite3 GroundStation4)
	(supports instrument9 thermograph0)
	(supports instrument9 image2)
	(supports instrument9 image1)
	(calibration_target instrument9 GroundStation2)
	(supports instrument10 thermograph3)
	(supports instrument10 image1)
	(calibration_target instrument10 GroundStation0)
	(on_board instrument9 satellite4)
	(on_board instrument10 satellite4)
	(power_avail satellite4)
	(pointing satellite4 Planet15)
	(supports instrument11 thermograph0)
	(supports instrument11 image2)
	(calibration_target instrument11 GroundStation1)
	(on_board instrument11 satellite5)
	(power_avail satellite5)
	(pointing satellite5 Phenomenon11)
)
(:goal (and
	(pointing satellite0 Planet21)
	(pointing satellite2 Star14)
	(pointing satellite5 Planet17)
	(have_image Phenomenon5 image1)
	(have_image Phenomenon7 thermograph0)
	(have_image Planet8 image2)
	(have_image Star9 thermograph0)
	(have_image Star10 thermograph3)
	(have_image Phenomenon12 thermograph0)
	(have_image Phenomenon13 image1)
	(have_image Star14 thermograph4)
	(have_image Planet15 image2)
	(have_image Planet17 image2)
	(have_image Phenomenon18 image1)
	(have_image Star19 thermograph4)
	(have_image Star20 thermograph4)
	(have_image Planet21 thermograph0)
	(have_image Star22 thermograph3)
	(have_image Planet23 image1)
))

)
