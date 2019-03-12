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
	instrument6 - instrument
	satellite2 - satellite
	instrument7 - instrument
	instrument8 - instrument
	instrument9 - instrument
	satellite3 - satellite
	instrument10 - instrument
	instrument11 - instrument
	satellite4 - satellite
	instrument12 - instrument
	image1 - mode
	thermograph3 - mode
	thermograph0 - mode
	thermograph2 - mode
	thermograph4 - mode
	GroundStation2 - direction
	Star4 - direction
	Star0 - direction
	Star1 - direction
	Star3 - direction
	Phenomenon5 - direction
	Planet6 - direction
	Planet7 - direction
	Star8 - direction
	Star9 - direction
	Star10 - direction
	Planet11 - direction
	Phenomenon12 - direction
	Phenomenon13 - direction
	Planet14 - direction
	Star15 - direction
	Phenomenon16 - direction
	Phenomenon17 - direction
	Phenomenon18 - direction
	Planet19 - direction
	Planet20 - direction
	Phenomenon21 - direction
	Star22 - direction
	Planet23 - direction
	Phenomenon24 - direction
)
(:init
	(supports instrument0 thermograph4)
	(supports instrument0 thermograph0)
	(supports instrument0 thermograph2)
	(calibration_target instrument0 Star4)
	(supports instrument1 thermograph3)
	(calibration_target instrument1 Star0)
	(supports instrument2 image1)
	(calibration_target instrument2 Star4)
	(on_board instrument0 satellite0)
	(on_board instrument1 satellite0)
	(on_board instrument2 satellite0)
	(power_avail satellite0)
	(pointing satellite0 Star8)
	(supports instrument3 thermograph3)
	(calibration_target instrument3 Star1)
	(supports instrument4 image1)
	(calibration_target instrument4 Star1)
	(supports instrument5 thermograph3)
	(calibration_target instrument5 Star3)
	(supports instrument6 thermograph2)
	(supports instrument6 thermograph0)
	(supports instrument6 image1)
	(calibration_target instrument6 Star0)
	(on_board instrument3 satellite1)
	(on_board instrument4 satellite1)
	(on_board instrument5 satellite1)
	(on_board instrument6 satellite1)
	(power_avail satellite1)
	(pointing satellite1 Phenomenon21)
	(supports instrument7 thermograph0)
	(calibration_target instrument7 Star3)
	(supports instrument8 thermograph4)
	(supports instrument8 thermograph3)
	(supports instrument8 thermograph2)
	(calibration_target instrument8 Star3)
	(supports instrument9 thermograph2)
	(supports instrument9 thermograph3)
	(calibration_target instrument9 Star1)
	(on_board instrument7 satellite2)
	(on_board instrument8 satellite2)
	(on_board instrument9 satellite2)
	(power_avail satellite2)
	(pointing satellite2 Star4)
	(supports instrument10 thermograph2)
	(calibration_target instrument10 Star3)
	(supports instrument11 thermograph2)
	(supports instrument11 thermograph4)
	(supports instrument11 thermograph0)
	(calibration_target instrument11 Star1)
	(on_board instrument10 satellite3)
	(on_board instrument11 satellite3)
	(power_avail satellite3)
	(pointing satellite3 Phenomenon16)
	(supports instrument12 thermograph4)
	(calibration_target instrument12 Star3)
	(on_board instrument12 satellite4)
	(power_avail satellite4)
	(pointing satellite4 Phenomenon18)
)
(:goal (and
	(have_image Phenomenon5 thermograph4)
	(have_image Planet7 image1)
	(have_image Star8 thermograph3)
	(have_image Star9 image1)
	(have_image Star10 image1)
	(have_image Phenomenon13 thermograph2)
	(have_image Star15 thermograph2)
	(have_image Phenomenon17 thermograph4)
	(have_image Phenomenon18 image1)
	(have_image Planet19 thermograph2)
	(have_image Planet20 thermograph4)
	(have_image Phenomenon21 image1)
	(have_image Star22 thermograph3)
))

)
