incr (double (errDiv 4 2))

(mapMaybe double) (errDiv 4 2)
									------------
											|-> Just 2

(mapMaybe double) (errDiv 4 0)
					^			------------
					---------- Nothing