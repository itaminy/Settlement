import unittest

import numpy as np

from generate_relief_demos import canyon, geometry, mask, mountain, GRID
from generate_natural_relief import field, full_scenes
from relief_raster import illumination, project
from relief_shapes import extent, shape
from relief_walls import shape as wall_shape


class ReliefTests(unittest.TestCase):
    def test_shape_study_preserves_sign_and_stays_inside_site(self):
        for scene in full_scenes():
            width,height=(s*GRID for s in scene.size)
            y,x=np.mgrid[:height,:width].astype(np.float32)
            neutral=np.full_like(x,.5)
            original=field(scene,x/GRID+scene.origin[0]-.5,y/GRID+scene.origin[1]-.5,neutral,neutral)
            updated=shape(scene,x,y,original)
            outside=extent(scene,x,y)>=.82
            np.testing.assert_array_equal(np.sign(original),np.sign(updated))
            np.testing.assert_array_equal(original[outside],updated[outside])
            self.assertGreater(float(np.max(abs(original-updated))),3,scene.name)
            walls=wall_shape(scene,x,y,original)
            np.testing.assert_array_equal(np.sign(original),np.sign(walls))
            outside=extent(scene,x,y)>=.94
            np.testing.assert_array_equal(original[outside],walls[outside])
            self.assertGreater(float(np.max(abs(walls-updated))),1,scene.name)

    def test_northwest_occluder_casts_shadow_southeast(self):
        grade=np.zeros((80,80),dtype=np.float32)
        roof=grade.copy()
        roof[28:37,28:37]=29
        light=illumination(grade,roof)
        self.assertLess(light[43,43],light[20,20])
        self.assertTrue(np.allclose(light[:12,:],light[0,0]))

    def test_steep_south_face_receives_less_light_than_north_face(self):
        z=np.zeros((80,80),dtype=np.float32)
        for y in range(20,61):
            z[y]=max(0,22-abs(y-40)*1.1)
        light=illumination(z,z)
        self.assertGreater(light[34,36],light[46,36])

    def test_projected_positive_and_negative_heights(self):
        z=np.zeros((60,3),dtype=np.float32)
        z[:,0]=8
        z[:,2]=-8
        shade=np.ones_like(z)
        def paint(row,x,height,slope,t):
            return np.broadcast_to([100,120,150],(len(x),3))
        view=project(z,paint,shade,np.zeros_like(z))
        self.assertGreater(view.source_y[30,0],view.source_y[30,1])
        self.assertLess(view.source_y[30,2],view.source_y[30,1])

    def test_relief_support_matches_existing_collision_cells(self):
        for scene in (mountain(),canyon()):
            width,height=(s*GRID for s in scene.size)
            y,x=np.mgrid[0:height,0:width].astype(np.float32)
            n=np.full_like(x,.5)
            z=geometry(scene,x/GRID+scene.origin[0]-.5,y/GRID+scene.origin[1]-.5,n,n)
            self.assertGreater(float(np.max(z)),35)
            if scene.name=="canyon":
                self.assertLess(float(np.min(z)),-50)
            self.assertFalse(np.any(z[np.asarray(mask(scene,scene.blocks))==0]>0))
            self.assertFalse(np.any(z[np.asarray(mask(scene,scene.pits))==0]<0))

    def test_full_regions_do_not_raise_walkable_trails(self):
        for scene in full_scenes():
            width,height=(s*GRID for s in scene.size)
            y,x=np.mgrid[:height,:width].astype(np.float32)
            neutral=np.full_like(x,.5)
            z=field(scene,x/GRID+scene.origin[0]-.5,y/GRID+scene.origin[1]-.5,neutral,neutral)
            outside_walls=np.asarray(mask(scene,scene.blocks))==0
            outside_pits=np.asarray(mask(scene,scene.pits))==0
            self.assertFalse(np.any(z[outside_walls]>0),scene.name)
            self.assertFalse(np.any(z[outside_pits]<0),scene.name)
            self.assertGreater(float(z.max()),50,scene.name)
            if scene.name=="canyon":
                self.assertLess(float(z.min()),-85)


if __name__=="__main__":
    unittest.main()
