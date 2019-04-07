
(IN-PACKAGE :SHOP-USER) 
(DEFPROBLEM STRIPS-SAT-X-1
 ((SATELLITE SATELLITE0) (INSTRUMENT INSTRUMENT0)
  (INSTRUMENT INSTRUMENT1) (SATELLITE SATELLITE1)
  (INSTRUMENT INSTRUMENT2) (SATELLITE SATELLITE2)
  (INSTRUMENT INSTRUMENT3) (INSTRUMENT INSTRUMENT4)
  (SATELLITE SATELLITE3) (INSTRUMENT INSTRUMENT5)
  (SATELLITE SATELLITE4) (INSTRUMENT INSTRUMENT6)
  (INSTRUMENT INSTRUMENT7) (INSTRUMENT INSTRUMENT8) (MODE IMAGE4)
  (MODE THERMOGRAPH1) (MODE THERMOGRAPH0) (MODE THERMOGRAPH2)
  (MODE IMAGE3) (DIRECTION GROUNDSTATION2) (DIRECTION STAR1)
  (DIRECTION STAR4) (DIRECTION STAR0) (DIRECTION GROUNDSTATION3)
  (DIRECTION PHENOMENON5) (DIRECTION PLANET6) (DIRECTION PLANET7)
  (DIRECTION PLANET8) (DIRECTION PLANET9) (DIRECTION PLANET10)
  (DIRECTION PLANET11) (DIRECTION PHENOMENON12) (DIRECTION PLANET13)
  (DIRECTION STAR14) (DIRECTION PLANET15) (DIRECTION PLANET16)
  (DIRECTION PLANET17) (DIRECTION PHENOMENON18) (DIRECTION STAR19)
  (DIRECTION PLANET20) (DIRECTION STAR21) (DIRECTION STAR22)
  (DIRECTION PLANET23) (DIRECTION PLANET24) (DIRECTION PLANET25)
  (DIRECTION STAR26) (DIRECTION PHENOMENON27) (DIRECTION PLANET28)
  (DIRECTION PLANET29) (SUPPORTS INSTRUMENT0 IMAGE4)
  (CALIBRATION_TARGET INSTRUMENT0 GROUNDSTATION3)
  (SUPPORTS INSTRUMENT1 THERMOGRAPH1) (SUPPORTS INSTRUMENT1 IMAGE4)
  (CALIBRATION_TARGET INSTRUMENT1 GROUNDSTATION3)
  (ON_BOARD INSTRUMENT0 SATELLITE0) (ON_BOARD INSTRUMENT1 SATELLITE0)
  (POWER_AVAIL SATELLITE0) (POINTING SATELLITE0 STAR19)
  (SUPPORTS INSTRUMENT2 THERMOGRAPH0) (SUPPORTS INSTRUMENT2 IMAGE4)
  (SUPPORTS INSTRUMENT2 THERMOGRAPH2)
  (CALIBRATION_TARGET INSTRUMENT2 GROUNDSTATION3)
  (ON_BOARD INSTRUMENT2 SATELLITE1) (POWER_AVAIL SATELLITE1)
  (POINTING SATELLITE1 PLANET17) (SUPPORTS INSTRUMENT3 IMAGE4)
  (SUPPORTS INSTRUMENT3 IMAGE3) (CALIBRATION_TARGET INSTRUMENT3 STAR1)
  (SUPPORTS INSTRUMENT4 IMAGE3)
  (CALIBRATION_TARGET INSTRUMENT4 GROUNDSTATION3)
  (ON_BOARD INSTRUMENT3 SATELLITE2) (ON_BOARD INSTRUMENT4 SATELLITE2)
  (POWER_AVAIL SATELLITE2) (POINTING SATELLITE2 PLANET7)
  (SUPPORTS INSTRUMENT5 THERMOGRAPH1) (SUPPORTS INSTRUMENT5 IMAGE4)
  (CALIBRATION_TARGET INSTRUMENT5 GROUNDSTATION3)
  (ON_BOARD INSTRUMENT5 SATELLITE3) (POWER_AVAIL SATELLITE3)
  (POINTING SATELLITE3 STAR4) (SUPPORTS INSTRUMENT6 IMAGE3)
  (SUPPORTS INSTRUMENT6 THERMOGRAPH1)
  (SUPPORTS INSTRUMENT6 THERMOGRAPH0)
  (CALIBRATION_TARGET INSTRUMENT6 STAR4)
  (SUPPORTS INSTRUMENT7 THERMOGRAPH2)
  (SUPPORTS INSTRUMENT7 THERMOGRAPH0)
  (CALIBRATION_TARGET INSTRUMENT7 STAR0) (SUPPORTS INSTRUMENT8 IMAGE3)
  (SUPPORTS INSTRUMENT8 THERMOGRAPH2)
  (CALIBRATION_TARGET INSTRUMENT8 GROUNDSTATION3)
  (ON_BOARD INSTRUMENT6 SATELLITE4) (ON_BOARD INSTRUMENT7 SATELLITE4)
  (ON_BOARD INSTRUMENT8 SATELLITE4) (POWER_AVAIL SATELLITE4)
  (POINTING SATELLITE4 PHENOMENON5)
  (ORIGINAL-GOAL
   (AND (POINTING SATELLITE1 PHENOMENON5)
        (POINTING SATELLITE2 PLANET11) (POINTING SATELLITE4 PLANET11)
        (HAVE_IMAGE PHENOMENON5 THERMOGRAPH1)
        (HAVE_IMAGE PLANET6 IMAGE4) (HAVE_IMAGE PLANET7 IMAGE3)
        (HAVE_IMAGE PLANET8 IMAGE3) (HAVE_IMAGE PLANET9 THERMOGRAPH0)
        (HAVE_IMAGE PLANET10 THERMOGRAPH1)
        (HAVE_IMAGE PLANET11 THERMOGRAPH2)
        (HAVE_IMAGE PHENOMENON12 IMAGE3)
        (HAVE_IMAGE PLANET13 THERMOGRAPH1) (HAVE_IMAGE STAR14 IMAGE3)
        (HAVE_IMAGE PLANET15 THERMOGRAPH0) (HAVE_IMAGE PLANET16 IMAGE3)
        (HAVE_IMAGE PLANET17 IMAGE4) (HAVE_IMAGE PHENOMENON18 IMAGE3)
        (HAVE_IMAGE STAR19 THERMOGRAPH0)
        (HAVE_IMAGE STAR21 THERMOGRAPH1) (HAVE_IMAGE STAR22 IMAGE4)
        (HAVE_IMAGE PLANET23 THERMOGRAPH1)
        (HAVE_IMAGE PLANET24 THERMOGRAPH2)
        (HAVE_IMAGE PLANET25 THERMOGRAPH1)
        (HAVE_IMAGE STAR26 THERMOGRAPH0)
        (HAVE_IMAGE PHENOMENON27 THERMOGRAPH1)
        (HAVE_IMAGE PLANET28 THERMOGRAPH2)
        (HAVE_IMAGE PLANET29 THERMOGRAPH0)))
  (GOAL-POINTING SATELLITE1 PHENOMENON5)
  (GOAL-POINTING SATELLITE2 PLANET11)
  (GOAL-POINTING SATELLITE4 PLANET11)
  (GOAL-HAVE-IMAGE PHENOMENON5 THERMOGRAPH1)
  (GOAL-HAVE-IMAGE PLANET6 IMAGE4) (GOAL-HAVE-IMAGE PLANET7 IMAGE3)
  (GOAL-HAVE-IMAGE PLANET8 IMAGE3)
  (GOAL-HAVE-IMAGE PLANET9 THERMOGRAPH0)
  (GOAL-HAVE-IMAGE PLANET10 THERMOGRAPH1)
  (GOAL-HAVE-IMAGE PLANET11 THERMOGRAPH2)
  (GOAL-HAVE-IMAGE PHENOMENON12 IMAGE3)
  (GOAL-HAVE-IMAGE PLANET13 THERMOGRAPH1)
  (GOAL-HAVE-IMAGE STAR14 IMAGE3)
  (GOAL-HAVE-IMAGE PLANET15 THERMOGRAPH0)
  (GOAL-HAVE-IMAGE PLANET16 IMAGE3) (GOAL-HAVE-IMAGE PLANET17 IMAGE4)
  (GOAL-HAVE-IMAGE PHENOMENON18 IMAGE3)
  (GOAL-HAVE-IMAGE STAR19 THERMOGRAPH0)
  (GOAL-HAVE-IMAGE STAR21 THERMOGRAPH1) (GOAL-HAVE-IMAGE STAR22 IMAGE4)
  (GOAL-HAVE-IMAGE PLANET23 THERMOGRAPH1)
  (GOAL-HAVE-IMAGE PLANET24 THERMOGRAPH2)
  (GOAL-HAVE-IMAGE PLANET25 THERMOGRAPH1)
  (GOAL-HAVE-IMAGE STAR26 THERMOGRAPH0)
  (GOAL-HAVE-IMAGE PHENOMENON27 THERMOGRAPH1)
  (GOAL-HAVE-IMAGE PLANET28 THERMOGRAPH2)
  (GOAL-HAVE-IMAGE PLANET29 THERMOGRAPH0))
 (MAIN)) 