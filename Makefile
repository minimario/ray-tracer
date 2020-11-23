TARGETS := src/projectile.byte \
		   src/tuple.byte \
		   src/color.byte \
		   src/canvas.byte \
		   src/transformations.byte \
		   src/clock.byte \
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

main:
	ocamlbuild -use-ocamlfind $(TARGETS)

clock:
	ocamlbuild -use-ocamlfind $(TARGETS)
	./clock.byte

projectile:
	ocamlbuild -use-ocamlfind $(TARGETS)
	./projectile.byte

silhouette:
	ocamlbuild -use-ocamlfind src/silhouette.byte
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
