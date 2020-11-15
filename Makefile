TARGETS := src/projectile.byte \
		   src/tuple.byte \
		   src/color.byte \
		   src/canvas.byte \
		   src/transformations.byte \
		   src/clock.byte \
		   src/rays.byte \
		   src/intersections.byte

TEST_TARGETS := test/tupletest.byte \
				test/colortest.byte \
				test/canvastest.byte \
				test/matrixtest.byte \
				test/transformationstest.byte \
				test/raystest.byte
				
main:
	ocamlbuild -use-ocamlfind $(TARGETS)

clock:
	ocamlbuild -use-ocamlfind $(TARGETS)
	./clock.byte

projectile:
	ocamlbuild -use-ocamlfind $(TARGETS)
	./projectile.byte

runtest:
	ocamlbuild -I src -use-ocamlfind $(TARGETS) $(TEST_TARGETS)
	./tupletest.byte
	./colortest.byte
	./canvastest.byte
	./matrixtest.byte
	./transformationstest.byte
	./raystest.byte

clean:
	rm *.byte
