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
	satellite3 - satellite
	instrument7 - instrument
	satellite4 - satellite
	instrument8 - instrument
	instrument9 - instrument
	instrument10 - instrument
	spectrograph0 - mode
	image3 - mode
	image4 - mode
	infrared1 - mode
	image2 - mode
	Star4 - direction
	Star3 - direction
	GroundStation1 - direction
	Star0 - direction
	Star2 - direction
	Planet5 - direction
	Phenomenon6 - direction
	Phenomenon7 - direction
	Phenomenon8 - direction
	Star9 - direction
	Planet10 - direction
	Planet11 - direction
	Phenomenon12 - direction
	Phenomenon13 - direction
	Star14 - direction
)
(:init
	(supports instrument0 infrared1)
	(supports instrument0 image4)
	(calibration_target instrument0 Star3)
	(supports instrument1 image4)
	(supports instrument1 image2)
	(supports instrument1 spectrograph0)
	(calibration_target instrument1 Star4)
	(supports instrument2 image2)
	(calibration_target instrument2 Star2)
	(on_board instrument0 satellite0)
	(on_board instrument1 satellite0)
	(on_board instrument2 satellite0)
	(power_avail satellite0)
	(pointing satellite0 Star0)
	(supports instrument3 image2)
	(supports instrument3 image3)
	(supports instrument3 image4)
	(calibration_target instrument3 Star2)
	(supports instrument4 image3)
	(supports instrument4 image2)
	(calibration_target instrument4 Star3)
	(supports instrument5 image4)
	(supports instrument5 infrared1)
	(supports instrument5 spectrograph0)
	(calibration_target instrument5 Star3)
	(on_board instrument3 satellite1)
	(on_board instrument4 satellite1)
	(on_board instrument5 satellite1)
	(power_avail satellite1)
	(pointing satellite1 Planet11)
	(supports instrument6 image2)
	(supports instrument6 spectrograph0)
	(calibration_target instrument6 Star2)
	(on_board instrument6 satellite2)
	(power_avail satellite2)
	(pointing satellite2 Phenomenon6)
	(supports instrument7 image3)
	(supports instrument7 spectrograph0)
	(supports instrument7 image4)
	(calibration_target instrument7 Star0)
	(on_board instrument7 satellite3)
	(power_avail satellite3)
	(pointing satellite3 Planet10)
	(supports instrument8 image4)
	(supports instrument8 infrared1)
	(supports instrument8 image3)
	(calibration_target instrument8 GroundStation1)
	(supports instrument9 image4)
	(calibration_target instrument9 Star0)
	(supports instrument10 image2)
	(supports instrument10 infrared1)
	(supports instrument10 image4)
	(calibration_target instrument10 Star2)
	(on_board instrument8 satellite4)
	(on_board instrument9 satellite4)
	(on_board instrument10 satellite4)
	(power_avail satellite4)
	(pointing satellite4 Star9)
)
(:goal (and
	(pointing satellite0 Phenomenon7)
	(pointing satellite3 Star9)
	(pointing satellite4 Planet5)
	(have_image Planet5 image2)
	(have_image Phenomenon6 image3)
	(have_image Phenomenon7 infrared1)
	(have_image Phenomenon8 image2)
	(have_image Star9 image3)
	(have_image Planet10 image4)
	(have_image Planet11 spectrograph0)
	(have_image Phenomenon12 image3)
	(have_image Phenomenon13 spectrograph0)
	(have_image Star14 image4)
))
(:metric minimize (total-time))

)
