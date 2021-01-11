type object_type = Sphere | Plane | TestShape

type material = {color: Color.color; ambient: float; diffuse: float; 
              specular: float; shininess: float;
              pattern: PatternType.pattern option; reflective: float;}

type shape = {shape_type: object_type; 
              material: material;
              transform: Matrix.matrix;}