.PHONY: build reinstall

build:
	make -C fp-extras      build
	make -C int-indexed    build
	make -C list-indexed   build
	make -C vector-indexed build
	make -C matrix-indexed build
	make -C mplot          build
	make -C ml             build
	# make -C rbtreez        build
	# make -C sexp-kit       build

reinstall:
	make -C fp-extras      clean reinstall
	make -C int-indexed    clean reinstall
	make -C list-indexed   clean reinstall
	make -C vector-indexed clean reinstall
	make -C matrix-indexed clean reinstall
	make -C mplot          clean reinstall
	make -C ml             clean reinstall
	# make -C rbtreez        clean reinstall
	# make -C sexp-kit       clean reinstall
