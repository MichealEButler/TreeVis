# constructes cylinders and spheres for trunk and crowns of trees 

import bpy, time

def makeMaterial(name, diffuse, specular, alpha):

    mat = bpy.data.materials.new(name)
    mat.diffuse_color = diffuse
    mat.diffuse_intensity = 1.0
    mat.specular_color = specular
    mat.specular_intensity = 0.5
    mat.alpha = alpha
    mat.ambient = 1
    return mat

def setMaterial(ob, mat):

    me = ob.data

    me.materials.append(mat)

maroon = makeMaterial('Maroon',(0.5,0,0),(1,1,1),1)
green = makeMaterial('Green',(0,0.5,0),(1,1,1),1)
olive = makeMaterial('Olive',(0.5,0.5,0),(1,1,1),1)
brown = makeMaterial('Brown', (0.4,0.25,0.12),(1,1,1),1)

filename='D:/Blender Script/Test Cylinders/Chunks/Files/chunk1.txt'  # enter the complete file path here
f=open(filename,'r') # open file for reading
arr=f.readlines()  # store the entire file in a variable
f.close()

start = time.time()
counter = 0

# Parse the array:
for p in arr:
     p0 = p.split(',')  # use colon as separator
     ItemName = p0[0]
     Species = p0[1]
     Radius1 = 0.2
     Radius2 = 0.4
     Xpos = float(p0[2])
     Ypos = float(p0[3])
     Chunk = float (p0[4])
     stemDiameter = float(p0[5])
     Height = float(p0[6])
     Crown = float(p0[7])
     Elevation = float(p0[8])
     Zpos1 = 1
     Zpos2 = 2
     
     if Species == '1':
        cyl = bpy.ops.mesh.primitive_cylinder_add(vertices=32, radius=(stemDiameter/200), depth=(Height/100), location=(Xpos-500,500-Ypos,(Height/200)+Elevation))
        setMaterial(bpy.context.object, brown)
        # cyl2 = bpy.ops.mesh.primitive_cylinder_add(vertices=32, radius=(Crown/2), depth=((Height/100) * 0.4), location=(Xpos-500,Ypos,((Height/100)*0.8)))
        sph = bpy.ops.mesh.primitive_ico_sphere_add(size=(Crown), location=(Xpos-500,500-Ypos,((Height/98)-Crown)+Elevation))
        setMaterial(bpy.context.object, maroon)
        
     if Species == '2':
        cyl = bpy.ops.mesh.primitive_cylinder_add(vertices=32, radius=(stemDiameter/200), depth=(Height/100), location=(Xpos-500,500-Ypos,(Height/200)+Elevation))
        setMaterial(bpy.context.object, brown)
        # cyl2 = bpy.ops.mesh.primitive_cylinder_add(vertices=32, radius=(Crown), depth=((Height/100) * 0.4), location=(Xpos,Ypos,((Height/100)*0.8)))
        sph = bpy.ops.mesh.primitive_ico_sphere_add(size=(Crown), location=(Xpos-500,500-Ypos,((Height/98)-Crown)+Elevation))
        setMaterial(bpy.context.object, green)
     
     if Species == '3':
        cyl = bpy.ops.mesh.primitive_cylinder_add(vertices=32, radius=(stemDiameter/200), depth=(Height/100), location=(Xpos-500,500-Ypos,(Height/200)+Elevation))
        setMaterial(bpy.context.object, brown)
        # cyl2 = bpy.ops.mesh.primitive_cylinder_add(vertices=32, radius=(Crown), depth=((Height/100) * 0.4), location=(Xpos,Ypos,((Height/100)*0.8)))
        sph = bpy.ops.mesh.primitive_ico_sphere_add(size=(Crown), location=(Xpos-500,500-Ypos,((Height/98)-Crown)+Elevation))
        setMaterial(bpy.context.object, olive)
        
     counter = counter + 1    
     print("Tree Created ", counter)
        

end = time.time()

print("Elapsed Time: ", end - start)

text_file = open("D:/Blender Script/Test Cylinders/ElapsedTime.txt", "w")
text_file.write("Elapsed Time = " + str(end-start))
text_file.close()