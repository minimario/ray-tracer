TARGETS := src/tuple.byte \
		   src/color.byte \
		   src/canvas.byte \
		   src/transformations.byte \
		   src/rays.byte \
		   src/intersections.byte \
		   src/sphere.byte \
		   src/reflection.byte \
		   src/world.byte \
		   src/camera.byte \
		   src/plane.byte \
		   src/shapetype.byte

TEST_TARGETS := test/tupletest.byte \
				test/colortest.byte \
				test/canvastest.byte \
				test/matrixtest.byte \
				test/transformationstest.byte \
				test/raystest.byte \
				test/spheretest.byte \
				test/reflectiontest.byte \
				test/worldtest.byte \
				test/cameratest.byte \
				test/shapetest.byte \
				test/planetest.byte

PROJECT_TARGETS := projects/projectile.byte \
				   projects/clock.byte \
				   projects/silhouette.byte \
				   projects/sphereworld.byte \
				   projects/sphereplane.byte


main:
	ocamlbuild -use-ocamlfind $(TARGETS)
	ocamlbuild -I src -use-ocamlfind $(TARGETS) $(TEST_TARGETS)
	ocamlbuild -I src -use-ocamlfind $(TARGETS) $(PROJECT_TARGETS)

clock:
	ocamlbuild -I src -use-ocamlfind $(TARGETS) projects/clock.byte
	./clock.byte

projectile:
	ocamlbuild -I src -use-ocamlfind $(TARGETS) projects/projectile.byte
	./projectile.byte

silhouette:
	ocamlbuild -I src -use-ocamlfind $(TARGETS) projects/silhouette.byte
	./silhouette.byte

sphereworld:
	ocamlbuild -I src -use-ocamlfind $(TARGETS) projects/sphereworld.byte
	./sphereworld.byte

sphereplane:
	ocamlbuild -I src -use-ocamlfind $(TARGETS) projects/sphereplane.byte
	./sphereplane.byte


runtest:
	ocamlbuild -I src -use-ocamlfind $(TARGETS) $(TEST_TARGETS)
	./tupletest.byte
	./colortest.byte
	./canvastest.byte
	./matrixtest.byte
	./transformationstest.byte
	./raystest.byte
	./spheretest.byte
	./reflectiontest.byte
	./worldtest.byte
	./cameratest.byte
	./shapetest.byte
	./planetest.byte

clean:
	rm *.byte
