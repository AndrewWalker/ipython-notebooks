{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
import Diagrams.Prelude
import Diagrams.TwoD.Arc
import Diagrams.Backend.SVG.CmdLine
import Data.Typeable.Internal

data DubinsPath n = DubinsPath
  { dubinsTrail :: Located (Trail V2 n)
  } 

data Configuration n = Configuration
  { qpos    :: P2 n
  , heading :: Angle n
  , paths   :: [ DubinsPath n ]
  } 

initialConfiguration :: (Floating n, Ord n) => 
    n -> 
    n -> 
    n -> 
    Configuration n
initialConfiguration x0 y0 th0
    = Configuration { qpos = p2 (x0,y0), heading = th0@@rad, paths = [] } 


dubinsL :: (Floating n, Ord n) => n
        -> Configuration n
        -> Configuration n
dubinsL s q@(Configuration p_i th paths) = 
    q { qpos = p_e, heading = th_e, paths = newPaths } 
  where
    v_ic = unitY # rotate th        -- vector from the configuration to the centre of rotation
    v_ci = v_ic # rotate halfTurn   -- vector from the centre of rotation to the configuration
    v_ce = v_ci # rotate (s@@rad)   -- vector from the centre of rotation to the new configuration
    p_c  = p_i .+^ v_ic             -- point at the centre of rotation
    p_e  = p_c .+^ v_ce             -- the new position
    th_e = th ^+^ (s@@rad)          -- the new angle
    newTrail  = (arcT  (direction v_ci) (s@@rad)) -- # translate (p_c .-. origin)
    newTrail' = at newTrail p_i
    newPaths  = [DubinsPath newTrail'] ++ paths


dubinsR :: (Floating n, Ord n) => n
        -> Configuration n
        -> Configuration n
dubinsR s q@(Configuration p_i th paths) = 
    q { qpos = p_e, heading = th_e, paths = newPaths } 
  where
    v_ic = unit_Y # rotate th
    v_ci = v_ic # rotate halfTurn 
    v_ce = v_ci # rotate (-s@@rad) 
    p_c  = p_i .+^ v_ic
    p_e  = p_c .+^ v_ce
    th_e = th ^+^ (-s@@rad)
    newTrail  = arcT  (direction v_ci) (-s@@rad) 
    newTrail' = at newTrail p_i
    newPaths = [DubinsPath newTrail'] ++ paths


dubinsS :: (Floating n, Ord n) => n
        -> Configuration n
        -> Configuration n
dubinsS s q@(Configuration p_i th paths) = 
    q { qpos = p_e, heading = th, paths = newPaths } 
  where
    v_ie = unitX # scale s # rotate th
    p_e  = p_i .+^ v_ie
    newTrail = p_i ~~ p_e
    newPaths = [DubinsPath newTrail] ++ paths


getTrail :: (Floating n, Ord n) => 
    DubinsPath n -> 
    Located (Trail V2 n)
getTrail (DubinsPath tr) = tr


getPaths :: (Floating n, Ord n) => 
    Configuration n -> 
    [ Located (Trail V2 n) ]
getPaths q@(Configuration _ _ paths) = map getTrail paths


drawTrails :: (Typeable n, RealFloat n, Renderable (Path V2 n) b) => 
    [ Located (Trail V2 n) ] -> 
    QDiagram b V2 n Any
drawTrails ts = mconcat $ ys
  where
    ys = map strokeLocTrail ts


drawConfiguration :: (Typeable n, RealFloat n, Renderable (Path V2 n) b) => 
    Configuration n -> 
    QDiagram b V2 n Any
drawConfiguration q@(Configuration pos th _) 
    =  circle(1) # translate dv # translate v1 
    <> circle(1) # translate dv # translate v2 
  where
    dv = pos .-. origin
    v1 = unitY  # rotate th
    v2 = unit_Y# rotate th


testPath :: (Typeable n, RealFloat n, Renderable (Path V2 n) b) => 
    Configuration n -> 
    QDiagram b V2 n Any
testPath q0 
    =  drawTrails (getPaths qe)
    <> c1 # opacity 0.2
    <> c2 # opacity 0.2
  where 
    c1 = drawConfiguration q0
    qe = q0 # dubinsS 1.0
            # dubinsR (pi/2)
            # dubinsS 2.0
            # dubinsL (pi/2)
            # dubinsS 2.0
    c2 = drawConfiguration qe


foo = circle(0.01) <> d
  where
    d :: Diagram B
    d = testPath (initialConfiguration 0.0 0.0 (pi/2))


main = mainWith foo

