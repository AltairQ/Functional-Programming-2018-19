ocamlc -c stack.mli
ocamlc -c stack.ml
ocamlc -c stackTest.ml
ocamlc -o stackTest stack.cmo stackTest.cmo
ocamlrun stackTest
