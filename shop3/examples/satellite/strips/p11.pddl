(define (problem strips-sat-x-1)
(:domain satellite)
(:objects
	satellite0 - satellite
	instrument0 - instrument
	satellite1 - satellite
	instrument1 - instrument
	instrument2 - instrument
	instrument3 - instrument
	satellite2 - satellite
	instrument4 - instrument
	instrument5 - instrument
	instrument6 - instrument
	satellite3 - satellite
	instrument7 - instrument
	satellite4 - satellite
	instrument8 - instrument
	thermograph2 - mode
	image3 - mode
	infrared1 - mode
	spectrograph4 - mode
	infrared0 - mode
	Star1 - direction
	Star4 - direction
	Star0 - direction
	GroundStation3 - direction
	Star2 - direction
	Star5 - direction
	Planet6 - direction
	Phenomenon7 - direction
	Star8 - direction
	Phenomenon9 - direction
	Star10 - direction
	Star11 - direction
	Star12 - direction
	Planet13 - direction
	Planet14 - direction
	Phenomenon15 - direction
	Planet16 - direction
	Star17 - direction
	Star18 - direction
	Planet19 - direction
)
(:init
	(supports instrument0 spectrograph4)
	(calibration_target instrument0 Star0)
	(on_board instrument0 satellite0)
	(power_avail satellite0)
	(pointing satellite0 Star8)
	(supports instrument1 infrared0)
	(supports instrument1 infrared1)
	(calibration_target instrument1 GroundStation3)
	(supports instrument2 infrared1)
	(supports instrument2 infrared0)
	(calibration_target instrument2 Star2)
	(supports instrument3 spectrograph4)
	(supports instrument3 infrared1)
	(supports instrument3 thermograph2)
	(calibration_target instrument3 Star0)
	(on_board instrument1 satellite1)
	(on_board instrument2 satellite1)
	(on_board instrument3 satellite1)
	(power_avail satellite1)
	(pointing satellite1 GroundStation3)
	(supports instrument4 infrared1)
	(supports instrument4 image3)
	(supports instrument4 infrared0)
	(calibration_target instrument4 Star2)
	(supports instrument5 thermograph2)
	(supports instrument5 spectrograph4)
	(calibration_target instrument5 Star0)
	(supports instrument6 infrared0)
	(calibration_target instrument6 GroundStation3)
	(on_board instrument4 satellite2)
	(on_board instrument5 satellite2)
	(on_board instrument6 satellite2)
	(power_avail satellite2)
	(pointing satellite2 Star4)
	(supports instrument7 image3)
	(calibration_target instrument7 Star2)
	(on_board instrument7 satellite3)
	(power_avail satellite3)
	(pointing satellite3 Phenomenon9)
	(supports instrument8 infrared0)
	(supports instrument8 spectrograph4)
	(supports instrument8 infrared1)
	(calibration_target instrument8 Star2)
	(on_board instrument8 satellite4)
	(power_avail satellite4)
	(pointing satellite4 Phenomenon9)
)
(:goal (and
	(pointing satellite0 Phenomenon9)
	(pointing satellite1 Star4)
	(pointing satellite4 Star11)
	(have_image Star5 image3)
	(have_image Planet6 infrared1)
	(have_image Phenomenon7 infrared1)
	(have_image Star8 image3)
	(have_image Star10 thermograph2)
	(have_image Star11 infrared1)
	(have_image Planet13 spectrograph4)
	(have_image Planet14 thermograph2)
	(have_image Phenomenon15 infrared0)
	(have_image Planet16 image3)
	(have_image Star17 infrared0)
))

)
