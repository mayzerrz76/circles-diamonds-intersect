-------------------------------------------------------------
--------------------Programming Task 1-----------------------
-------------------------------------------------------------
--  Name:  RYAN MAY                                        --
--  Email: rmay03@syr.edu                                  --
-------------------------------------------------------------

---------------------------------------------------------------------------------------------------------
-- PROBLEM #1
-- Purpose:
--   distance (x1,y1) (x2,y2)
--      Determines the distance between the coordinates (x1,y1) and (x2,y2) as a float.

-- Definition:
distance :: (Float,Float) -> (Float,Float) -> Float
distance (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

-- Tests:
--   Tests include multiple different coordinates.  Coordinates are tested backwards to ensure distance
--      is still the same.   Tests include negative coordinate values.  Tests for distance with
--      just x displacement, just y displacement, and x and y displacement.

t1a = distance (0,0) (2,0) -- should return 2.0 (obvious case, x displacement only)
t1b = distance (2,0) (0,0) -- should return 2.0 (same as t1 but points flipped)
t1c = distance ((-5),(-2)) ((-12),(-2)) -- should return 7.0 (negative coordinates)
t1d = distance ((-5),0) ((-5),(-8)) -- should return 8.0 (y displacement with negative coords)
t1e = distance (0,0) (6,8) -- should return 10.0 (x and y displacement.. 6 8 10 triangle too)

---------------------------------------------------------------------------------------------------------
-- PROBLEM #2
-- Purpose:
--   cabDistance (x1,y1) (x2,y2)
--      Determines the taxicab distance between the coordinates (x1,y1) and (x2,y2).  Where
--         taxicab distance is the sum of the absolute value of the x displacement plus the absolute
--         value of the y displacement

-- Definition:
cabDistance :: (Float,Float) -> (Float,Float) -> Float
cabDistance (x1,y1) (x2,y2) = abs(x1-x2) + abs(y1-y2)

-- Tests:
--   Tests include multiple differet coordinates.  Coordinates are tested backwards to ensure distance
--      is still the same.   Tests include negative coordinate values.  Tests for distance with
--      just x displacement, just y displacement, and x and y displacement.

t2a = cabDistance (0,0) (5,0) -- should return 5.0 (obvious case, x displacement only)
t2b = cabDistance (5,0) (0,0) -- should return 5.0 (same as t1 but points flipped)
t2c = cabDistance ((-6),(-4)) ((-10),(-4)) -- should return 4.0 (negative coordinates)
t2d = cabDistance ((-1),(-1)) ((-1),(-7)) -- should return 6.0 (y displacement with negative coords)
t2e = cabDistance (0,0) (6,8) -- should return 14.0 (x and y displacement...and shows cabDistance vs. distance test above)

---------------------------------------------------------------------------------------------------------
-- PROBLEM #3
-- Purpose:
--   onCircle (a,b) (x,y) r
--      determines if point (a,b) is on the circle that has origin
--        (x,y) and radius r

-- Definition:
onCircle :: (Float,Float) -> (Float,Float) -> Float -> Bool
onCircle (a,b) (x,y) r = distance (a,b) (x,y) == r

-- Tests:
--   Tests include multiple circle, some points and some origins
--     include negative numbers; some cases return True and others
--     return False.

t3a = onCircle (3,4) (0,0) 5 -- should be True (3,4,5 triangle)
t3b = onCircle (1,1) (0,0) 5 -- should be False (same circle as t3a, but point not on circle)
t3c = onCircle (-7,-2) (-2,-2) 5 -- should be True (different circle)
t3d = onCircle (1,4) (-2,-2) 5 -- should be False (same circle as t3c, but point not on circle)

---------------------------------------------------------------------------------------------------------
-- PROBLEM #4
-- Purpose:
--   onDiamond (a,b) (x,y) r
--      determines if point (a,b) is on the diamond that has origin
--      (x,y) and radius r

-- Definition:
onDiamond :: (Float,Float) -> (Float,Float) -> Float -> Bool
onDiamond (a,b) (x,y) r = cabDistance (a,b) (x,y) == r

-- Tests:
--   Tests include multiple diamonds, some points and some origins
--     include negative numbers; some cases return True and others
--     return False.

t4a = onDiamond (6,6) (1,1) 10 -- should be True
t4b = onDiamond (7,4) (1,1) 10 -- should be False (same diamond as t4a, but point not on diamond)
t4c = onDiamond (-3,-5) (1,-2) 7 -- should be True (different diamond)
t4d = onDiamond (2,6) (1,-2) 7 -- should be False (same diomond as t4c, but point not on diamond)

---------------------------------------------------------------------------------------------------------
-- PROBLEM #5 
-- Purpose:
--   insideCircle (x1,y1) r1 (x2,y2) r2
--      determines if circle with origin (x1,y1) and radius r1 is inside
--      the circle (x2,y2) with origin r2

-- Definition:
insideCircle :: (Float,Float) -> Float -> (Float,Float) -> Float -> Bool
insideCircle (x1,y1) r1 (x2,y2) r2 = (distance (x1,y1) (x2,y2) + r1) < r2

-- Tests:
--   Tests include circle 1 completely outside of circle 2, circle 1 partially inside
--      circle 2, as well as circle one completely inside circle 2. Tests include negative
--      coordinate values, and both true and false returns. Testing also makes sure that if circle 1 is
--      inside circle 2, then circle 2 is not inside circle 1, by flipping circle input order. One test includes
--      testing two equal circles, to show an equal circle is not "inside" another equal circle. Another test includes testing
--      a circle that is inside another circle but also shares one point with that circle, in which case
--      it is not inside. ("Circle 1" is first coord and radius input, "Circle 2" is second coord and radius)

t5a = insideCircle (2,2) 1 (1,1) 3 -- should be True
t5b = insideCircle (1,1) 3 (2,2) 1 -- should be False (same circles as t5a but order is flipped, IF t5a, THEN not t5b)
t5c = insideCircle (0,0) 2 (1,1) 3 -- should be False (same second circle as t5a, circle 1 is partially inside)
t5d = insideCircle (-3,-5) 2 (1,1) 3 -- should be False (same second circle as t5b, a different circle 1 is completely outside)
t5e = insideCircle (-2,0) 2 (1,0) 5 -- should be False (different second circle, circle 1 is inside and shares 1 point w circle 2)
t5f = insideCircle (-4,4) 2 (-4,4) 2 -- should be False (same 2 circles, an equal circle is not inside another equal circle)
t5g = insideCircle (0,0) 1 (0,0) 2 -- should be True (obvious case for good measure, also same circle origins)

---------------------------------------------------------------------------------------------------------
-- PROBLEM #6
-- Purpose:
--   insideDiamond (x1,y1) r1 (x2,y2) r2
--      determines if diamond with origin (x1,y1) and radius r1 is inside
--      the diamond (x2,y2) with origin r2

-- Definition:
insideDiamond :: (Float,Float) -> Float -> (Float,Float) -> Float -> Bool
insideDiamond (x1,y1) r1 (x2,y2) r2 = (cabDistance (x1,y1) (x2,y2) + r1) < r2 

-- Tests:
--   Tests include diamond 1 completely outside of diamond 2, diamond 1 partially inside
--      diamond 2, as well as diamond 1 completely inside diamond 2. Tests include negative
--      coordinate values, and both true and false returns. Testing also makes sure that if diamond 1 is
--      inside diamond 2, then diamond 2 is not inside diamond 1, by flipping diamond input order. One test includes
--      testing two equal diamonds, to show an equal diamond is inside. Another test includes testing
--      a circle that is inside another circle but also shares one point with that circle, in which case
--      it is still inside. ("Circle 1" is first coord and radius input, "Circle 2" is second coord and radius)

t6a = insideDiamond (3,3) 1 (2,2) 4 -- should be True
t6b = insideDiamond (2,2) 4 (3,3) 1 -- should be False (same diamonds as t6a but order is flipped, IF t6a, THEN not t6b)
t6c = insideDiamond (1,1) 2 (2,2) 4 -- should be False (same second diamond as t6a, diamond 1 is partially inside)
t6d = insideDiamond (-2,-4) 2 (2,2) 4 -- should be False (same second diamond as t6b, diamond 1 is completely outside)
t6e = insideDiamond (-1,1) 2 (2,1) 5 -- should be False (different second diamond, 2 sides of diamond one share points with sides of diamond 2)
t6f = insideDiamond (-5,5) 3 (-5,5) 3 -- should be False (same 2 diamonds, an equal diamond is not inside another equal diamond)
t6g = insideDiamond (0,0) 2 (0,0) 3 -- should be True (obvious case for good measure, also same diamond origins)

---------------------------------------------------------------------------------------------------------
-- PROBLEM #7
-- Purpose:
--   circlesTouch (x1,y1) r1 (x2,y2) r2
--      determines if the circle with origin (x1,y1) and radius r1 shares exactly
--         one point with the circle with origin (x2,y2) and radius r2

-- Definition:
circlesTouch :: (Float,Float) -> Float -> (Float,Float) -> Float -> Bool
circlesTouch  (x1,y1) r1 (x2,y2) r2 = touch && notSame
    where 
      touch = (abs(distance (x1,y1) (x2,y2) - r2) == r1 || (distance (x1,y1) (x2,y2) + r2) == r1)
      notSame = not (( x1 == x2) && (y1 == y2))

-- Tests:
--   Tests include pairs of circles that return both true and false values. Both internally and externally
--       tangent circles will be tested (ie. a circle that shares 1 point but is inside the circle vs. a circle
--       that shares one point but is outside of the circle). Given 2 circles to test, tests will also include
--       the order of circles input reversed (ie. IF circle 1 touches circle 2, THEN circle 2 touches circle 1).
--       Tests will also include changing just one of the circles input. Tests also include checking for a two circles
--       that share more than 1 point. Tests for circle 1 inside circle 2, as well as circle 1 completely outside of circle 2)

t7a = circlesTouch (-1,-1) 3 (3,2) 2 -- should be True (externally tangent circles, also includes neg coords)
t7b = circlesTouch (3,2) 2 (-1,-1) 3 -- should be True (same two circles as t7a, but order of input reversed)
t7c = circlesTouch (1,1) 3 (0,1) 2 -- should be True (internally tangent circle, different circles than t7b and t7a)
t7d = circlesTouch (2,2) 3 (1,2) 1 -- should be False (circle 2 is inside circle 1)
t7e = circlesTouch (2,2) 3 (-4,-6) 2 -- should be False (circle 2 is completely outside of circle 1)
t7f = circlesTouch (1,0) 3 (3,0) 2 -- should be False (circles intersect ie share 2 points)
t7g = circlesTouch (0,0) 3 (0,0) 3 -- should be False (same 2 circles)
t7h = circlesTouch (0,0) 2 (0,-3) 1 -- should be True (somewhat obvious case for good measure)

---------------------------------------------------------------------------------------------------------
-- PROBLEM #8 
-- Purpose:
--   diamondsTouch (x1,y1) r1 (x2,y2) r2
--      determines if the diamond with origin (x1,y1) and radius r1 shares exactly
--         one point with the diamond with origin (x2,y2) and radius r2. 

-- Definition:
diamondsTouch :: (Float,Float) -> Float -> (Float,Float) -> Float -> Bool
diamondsTouch  (x1,y1) r1 (x2,y2) r2 = ((cabDistance (x1,y1) (x2,y2) - r2) == r1) && (x1==x2 || y1==y2)

-- Tests:
--   Tests include pairs of diamonds that return both true and false values. With the way we've
--      defined a diamond, a diamond will only touch another diamond on the diamond's corners.  So,
--      tests will include a diamond touching the same diamond on all 4 corners.  The tests will also
--      include a diamond that intersects another diamond, a diamond inside a diamond, and a diamond 
--      outside of a diamond, and a diamond that shares 2 entire sides as the other diamond (these cases
--      should all return false). Tests also include negative coord values.

t8a = diamondsTouch (3,0) 1 (0,0) 2 -- should be True (touch occurs at diamond 2's right corner)
t8b = diamondsTouch (0,4) 2 (0,0) 2 -- should be True (same 2nd diamond as t8a, touch occurs at top corner)
t8c = diamondsTouch (-5,0) 3 (0,0) 2 -- should be True (same 2nd diamond as t8a, touch occurs at left corner)
t8d = diamondsTouch (0,-6) 4 (0,0) 2 -- should be True (same 2nd diamond as t8a, touch occurs at bottom corner)
t8e = diamondsTouch (1,4) 3 (-4,-6) 2 -- should be False (diamond 2 is completely outside of diamond 1)
t8f = diamondsTouch (2,1) 3 (-2,1) 2 -- should be False (diamonds intersect ie share 2 points)
t8g = diamondsTouch (0,0) 1 (1,1) 1 -- should be False (diamond 2 shares 2 of its entire sides with diamond 1's sides, ie share more than 2 points)
t8h = diamondsTouch (0,0) 3 (0,0) 3 -- should be False (same 2 diamonds)
t8i = diamondsTouch (0,0) 1 (0,-2) 1 -- should be True (somewhat obvious case for good measure)

---------------------------------------------------------------------------------------------------------
-- PROBLEM #9
-- Purpose:
--   circlesIntersect (x1,y1) r1 (x2,y2) r2
--      determines if the circle with origin (x1,y1) and radius r1 shares exactly
--         two points with the cirlce with origin (x2,y2) and radius r2. Thus
--         determining if the two circles intersect.

-- Definition:
circlesIntersect :: (Float,Float) -> Float -> (Float,Float) -> Float -> Bool
circlesIntersect (x1,y1) r1 (x2, y2) r2 = (abs(distance (x1,y1) (x2,y2) - r2) < r1 && (distance (x1,y1) (x2,y2) + r2) > r1)

-- Tests:
--   Tests include pairs of circles that return both true and false values. Tests will include more than
--       one valid intersection with different circles.  Tests will also include a circle 1 being inside circle 2, 
--       circle 1 being completely outside of circle 2, circle 1 being tangent to circle 2 (share 1 point), and
--       circle 1 being equal to circle 2 (all of which should return false)

t9a = circlesIntersect (4,-1) 2 (1,-1) 2 -- should be True (neg coord values)
t9b = circlesIntersect (-6,-1) 6 (1,-1) 2 -- should be True (same 2nd circle as t9a, different 1st circle)
t9c = circlesIntersect (1,1) 3 (1,1) 5 -- should be False (1st circle is inside second circle, no shared points)
t9d = circlesIntersect (2,4) 3 (-3,-6) 2 -- should be False (circle 1 is completely outside of circle 2, no shared points)
t9e = circlesIntersect (3,0) 3 (-1,0) 1 -- should be False (the two circles are tangent, i.e share 1 point)
t9f = circlesIntersect (2,1) 2 (2,1) 2 -- should be False (2 circles are equal, share more than 2 points)
t9g = circlesIntersect (0,0) 1 (1,0) 1 -- should be True (somewhat obvious case for good measure)

---------------------------------------------------------------------------------------------------------
-- PROBLEM #10
-- Purpose:
--   diamondCoord (x,y) r n
--      Given a diamond with origin (x,y) and radius r, this function will take an integer n and
--        symbolically represent that integer as a base on a baseball diomond. (i.e 1st - right corner,
--        2nd - top corner, 3rd - left corner, home(4th) - bottom corner).  The function will use
--        the diamond passed in as the "baseball diomond", and it's corners as the bases.  The function
--        will return the coordinates of the base entered as n.  Note that when n < 1 the origin of the
--        diamond will be returned. Also note that the bases continue to increment counter clockwise
--        when n > 4 as one would expect (ie. 5th base -> 1st base  and 6th base - 2nd base)

-- Definition:
diamondCoord :: (Float,Float) -> Float -> Integer -> (Float,Float)
diamondCoord (x,y) r n
    | n < 1        = (x,y)
    | mod n 4 == 1 = (x+r,y)
    | mod n 4 == 2 = (x,y+r)
    | mod n 4 == 3 = (x-r,y)
    | otherwise    = (x,y-r)

-- Tests:
--   Tests include diamonds with different coords and negative coords, and different radii. The test will include when
--      a negative "base" is passed in. Naturally we will test bases 1-4, then several bases greater
--      than 4 to show that the function works as it should.

t10a = diamondCoord (-2,-1) 3 (-3) -- should return (-2.0,-1.0) (negative value of n)
t10b = diamondCoord (-1,-1) 2 1 -- should return (1.0,-1.0) (negative coords, indicates diamond 1st base)
t10c = diamondCoord (1,0) 4 1 -- should return (5.0,0.0)  (different diamond than t10b, indicates diamond 1st base)
t10d = diamondCoord (1,0) 4 2 -- should return (1.0,4.0)  (same diamond as t10c, indicates diamond 2nd base)
t10e = diamondCoord (1,0) 4 3 -- should return (-3.0,0.0)  (same diamond as t10c, indicates diamond 3rd base)
t10f = diamondCoord (1,0) 4 4 -- should return (1.0,-4.0)  (same diamond as t10c, indicates diamond 4th base or "home")
t10g = diamondCoord (1,0) 4 6 -- should return (1.0,4.0)  (same diamond and base as t10d, 6 mod 4 is 2, so 6th base is 2nd base)
t10h = diamondCoord (1,0) 4 9 -- should return (5.0,0.0)  (same diamond and base as t10c, 9 mod 4 is 1, so 9th base is 1st base)