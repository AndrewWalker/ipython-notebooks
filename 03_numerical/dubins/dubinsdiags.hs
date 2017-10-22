{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Direction
import Diagrams.TwoD.Arc
import Diagrams.TwoD.Offset
import Diagrams.Backend.SVG.CmdLine

configuration :: Diagram B
configuration
    =  circle(1) # translate (r2 (0, 1))
    <> circle(1) # translate (r2 (0, -1))
    <> fromOffsets [ unitX ]

locConfiguration :: Double -> Double -> Diagram B
locConfiguration a d
    = configuration # (rotate (a@@rad)) # translate (r2 (d, 0))

debugLSL :: Double -> Double -> Double -> Diagram B
debugLSL a b d
    =  l1 # strokeLocTrail # opacity 0.2
    <> l2 # strokeLocTrail # lc blue
  where
    p1 = P (r2 (0,1)) # rotate (a@@rad)
    p2 = P (r2 (d,1)) # rotateAround (P (r2 (d,0))) (b@@rad) 
    l1 = p1 ~~ p2
    l2 = offsetTrail 1 l1

debugRSR :: Double -> Double -> Double -> Diagram B
debugRSR a b d
    =  l1 # strokeLocTrail # opacity 0.2
    <> l2 # strokeLocTrail # lc blue
  where
    p1 = P (r2 (0,-1)) # rotate (a@@rad)
    p2 = P (r2 (d,-1)) # rotateAround (P (r2 (d,0))) (b@@rad) 
    l1 = p1 ~~ p2
    l2 = offsetTrail (-1) l1

debugLSR :: Double -> Double -> Double -> Diagram B
debugLSR a b d
    =  l1 # strokeLocTrail # opacity 0.2
    <> (p1~~p3) # strokeLocTrail # lc green
    <> (p2~~p4) # strokeLocTrail # lc green
    <> (p3~~p4) # strokeLocTrail # lc blue
  where
    p1 = P (r2 (0, 1)) # rotate (a@@rad)
    p2 = P (r2 (d,-1)) # rotateAround (P (r2 (d,0))) (b@@rad) 
    l1 = p1 ~~ p2
    l1len = norm (p2-p1)
    v1 = (fromDirection $ (dirBetween p2 p1))
    v2 = v1 # rotate (-(acos (2/l1len))@@rad)
    p3 = p1 # translate v2
    v3 = (fromDirection $ (dirBetween p1 p2))
    v4 = v3 # rotate (-(acos (2/l1len))@@rad)
    p4 = p2 # translate v4

debugRSL :: Double -> Double -> Double -> Diagram B
debugRSL a b d
    =  l1 # strokeLocTrail # opacity 0.2
    <> (p1~~p3) # strokeLocTrail # lc green
    <> (p2~~p4) # strokeLocTrail # lc green
    <> (p3~~p4) # strokeLocTrail # lc blue
  where
    p1 = P (r2 (0,-1)) # rotate (a@@rad)
    p2 = P (r2 (d, 1)) # rotateAround (P (r2 (d,0))) (b@@rad) 
    l1 = p1 ~~ p2
    l1len = norm (p2-p1)
    v1 = (fromDirection $ (dirBetween p2 p1))
    v2 = v1 # rotate ( (acos (2/l1len))@@rad)
    p3 = p1 # translate v2
    v3 = (fromDirection $ (dirBetween p1 p2))
    v4 = v3 # rotate ( (acos (2/l1len))@@rad)
    p4 = p2 # translate v4

arcAboutCW c p1 p2
    =  arcCW d2 d1 # (translate (r2 (unp2 c)))
  where
    d1 = dirBetween p1 c
    d2 = dirBetween p2 c

arcAboutCCW c p1 p2
    =  arcCCW d2 d1 # (translate (r2 (unp2 c)))
    <> arrowAt c (fromDirection d1)
    <> arrowAt c (fromDirection d2)
  where
    d1 = dirBetween p1 c
    d2 = dirBetween p2 c

debugLRL :: Double -> Double -> Double -> Diagram B
debugLRL a b d
    =  l1 # strokeLocTrail # opacity 0.2
    <> (p1~~p3) # strokeLocTrail # opacity 0.2
    <> (p2~~p3) # strokeLocTrail # opacity 0.2
    <> circle 0.01 # translate (r2 (unp2 p1)) # lc red
    <> circle 0.01 # translate (r2 (unp2 p2))
    <> circle 1 # translate v3 # translate (r2 (unp2 p1))  # opacity 0.4 #lc green
    <> arcAboutCCW p3 p1 p2
    <> arcAboutCCW p1 p3 pi
    <> arcAboutCCW p2 pe p3
  where
    pi = P (r2 (0, 0))
    pe = P (r2 (d, 0))
    p1 = P (r2 (0, 1)) # rotate (a@@rad)
    p2 = P (r2 (d, -1)) # rotateAround (P (r2 (d,0))) (b@@rad) 
    l1 = p1 ~~ p2
    l1len = norm (p2-p1)
    slen  = sqrt $ 4- (l1len*l1len)/4
    v1 = (fromDirection $ (dirBetween p2 p1))
    v2 = v1 # rotate ((-acos (l1len/4))@@rad)
    v3 = v2 # scale 2
    p3 = (P v3) # translate (r2 (unp2 p1)) 


debugRLR :: Double -> Double -> Double -> Diagram B
debugRLR a b d
    =  l1 # strokeLocTrail # opacity 0.2
    <> (p1~~p3) # strokeLocTrail # opacity 0.2
    <> (p2~~p3) # strokeLocTrail # opacity 0.2
    <> circle 0.01 # translate (r2 (unp2 p1))
    <> circle 0.01 # translate (r2 (unp2 p2))
    <> circle 1 # translate v3 # translate (r2 (unp2 p1))  # opacity 0.4 #lc green
    <> arcAboutCW p3 p1 p2
    <> arcAboutCW p1 p3 pi
    <> arcAboutCW p2 pe p3
  where
    pi = P (r2 (0, 0))
    pe = P (r2 (d, 0))
    p1 = P (r2 (0,-1)) # rotate (a@@rad)
    p2 = P (r2 (d,-1)) # rotateAround (P (r2 (d,0))) (b@@rad) 
    l1 = p1 ~~ p2
    l1len = norm (p2-p1)
    slen  = sqrt $ 4- (l1len*l1len)/4
    v1 = (fromDirection $ (dirBetween p2 p1))
    v2 = v1 # rotate ((acos (l1len/4))@@rad)
    v3 = v2 # scale 2
    p3 = (P v3) # translate (r2 (unp2 p1)) 




sample2 :: Double -> Double -> Double -> Diagram B
sample2 a b d
    =  locConfiguration a 0 # opacity op
    <> locConfiguration b d # opacity op
    <> debugLRL a b d
  where
    op = 0.4

--main = mainWith $ sample2 (-pi/3) (pi) 5 # showOrigin
main = mainWith $ sample2 (-pi/2) (-pi/4) 4.5 # showOrigin
--main = mainWith $ sample2 (pi/2) (-pi/4) 4.5 # showOrigin
