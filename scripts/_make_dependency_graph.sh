#! bin bash
make -Bnd | make2graph | dot -Tpng -o dependencies.png