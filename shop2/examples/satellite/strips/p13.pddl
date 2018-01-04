(define (problem strips-sat-x-1)
(:domain satellite)
(:objects
	satellite0 - satellite
	instrument0 - instrument
	instrument1 - instrument
	satellite1 - satellite
	instrument2 - instrument
	satellite2 - satellite
	instrument3 - instrument
	instrument4 - instrument
	satellite3 - satellite
	instrument5 - instrument
	satellite4 - satellite
	instrument6 - instrument
	instrument7 - instrument
	instrument8 - instrument
	image4 - mode
	thermograph1 - mode
	thermograph0 - mode
	thermograph2 - mode
	image3 - mode
	GroundStation2 - direction
	Star1 - direction
	Star4 - direction
	Star0 - direction
	GroundStation3 - direction
	Phenomenon5 - direction
	Planet6 - direction
	Planet7 - direction
	Planet8 - direction
	Planet9 - direction
	Planet10 - direction
	Planet11 - direction
	Phenomenon12 - direction
	Planet13 - direction
	Star14 - direction
	Planet15 - direction
	Planet16 - direction
	Planet17 - direction
	Phenomenon18 - direction
	Star19 - direction
	Planet20 - direction
	Star21 - direction
	Star22 - direction
	Planet23 - direction
	Planet24 - direction
	Planet25 - direction
	Star26 - direction
	Phenomenon27 - direction
	Planet28 - direction
	Planet29 - direction
)
(:init
	(supports instrument0 image4)
	(calibration_target instrument0 GroundStation3)
	(supports instrument1 thermograph1)
	(supports instrument1 image4)
	(calibration_target instrument1 GroundStation3)
	(on_board instrument0 satellite0)
	(on_board instrument1 satellite0)
	(power_avail satellite0)
	(pointing satellite0 Star19)
	(supports instrument2 thermograph0)
	(supports instrument2 image4)
	(supports instrument2 thermograph2)
	(calibration_target instrument2 GroundStation3)
	(on_board instrument2 satellite1)
	(power_avail satellite1)
	(pointing satellite1 Planet17)
	(supports instrument3 image4)
	(supports instrument3 image3)
	(calibration_target instrument3 Star1)
	(supports instrument4 image3)
	(calibration_target instrument4 GroundStation3)
	(on_board instrument3 satellite2)
	(on_board instrument4 satellite2)
	(power_avail satellite2)
	(pointing satellite2 Planet7)
	(supports instrument5 thermograph1)
	(supports instrument5 image4)
	(calibration_target instrument5 GroundStation3)
	(on_board instrument5 satellite3)
	(power_avail satellite3)
	(pointing satellite3 Star4)
	(supports instrument6 image3)
	(supports instrument6 thermograph1)
	(supports instrument6 thermograph0)
	(calibration_target instrument6 Star4)
	(supports instrument7 thermograph2)
	(supports instrument7 thermograph0)
	(calibration_target instrument7 Star0)
	(supports instrument8 image3)
	(supports instrument8 thermograph2)
	(calibration_target instrument8 GroundStation3)
	(on_board instrument6 satellite4)
	(on_board instrument7 satellite4)
	(on_board instrument8 satellite4)
	(power_avail satellite4)
	(pointing satellite4 Phenomenon5)
)
(:goal (and
	(pointing satellite1 Phenomenon5)
	(pointing satellite2 Planet11)
	(pointing satellite4 Planet11)
	(have_image Phenomenon5 thermograph1)
	(have_image Planet6 image4)
	(have_image Planet7 image3)
	(have_image Planet8 image3)
	(have_image Planet9 thermograph0)
	(have_image Planet10 thermograph1)
	(have_image Planet11 thermograph2)
	(have_image Phenomenon12 image3)
	(have_image Planet13 thermograph1)
	(have_image Star14 image3)
	(have_image Planet15 thermograph0)
	(have_image Planet16 image3)
	(have_image Planet17 image4)
	(have_image Phenomenon18 image3)
	(have_image Star19 thermograph0)
	(have_image Star21 thermograph1)
	(have_image Star22 image4)
	(have_image Planet23 thermograph1)
	(have_image Planet24 thermograph2)
	(have_image Planet25 thermograph1)
	(have_image Star26 thermograph0)
	(have_image Phenomenon27 thermograph1)
	(have_image Planet28 thermograph2)
	(have_image Planet29 thermograph0)
))

)
