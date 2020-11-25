TARGETS := src/tuple.byte \
		   src/color.byte \
		   src/canvas.byte \
		   src/transformations.byte \
		   src/rays.byte \
		   src/intersections.byte \
		   src/sphere.byte \
		   src/reflection.byte

TEST_TARGETS := test/tupletest.byte \
				test/colortest.byte \
				test/canvastest.byte \
				test/matrixtest.byte \
				test/transformationstest.byte \
				test/raystest.byte \
				test/spheretest.byte \
				test/reflectiontest.byte

PROJECT_TARGETS := projects/projectile.byte \
				   projects/clock.byte \
				   projects/silhouette.byte \

main:
	ocamlbuild -use-ocamlfind $(TARGETS)
	ocamlbuild -I src -use-ocamlfind $(TARGETS) $(TEST_TARGETS)
	ocamlbuild -I src -use-ocamlfind $(TARGETS) $(PROJECT_TARGETS)

clock:
	ocamlbuild -I src -use-ocamlfind $(TARGETS) $(PROJECT_TARGETS)
	./clock.byte

projectile:
	ocamlbuild -I src -use-ocamlfind $(TARGETS) $(PROJECT_TARGETS)
	./projectile.byte

silhouette:
	ocamlbuild -I src -use-ocamlfind $(TARGETS) $(PROJECT_TARGETS)
	./silhouette.byte

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

clean:
	rm *.byte
