shop-app:	buildapp-script shop-app-entrypoints.lisp
	./buildapp-script

install:	shop-app
	mkdir -p ${HOME}/.local/bin/
	ln -sf shop-app ${HOME}/.local/bin/shop
	ln -sf shop-app ${HOME}/.local/bin/ess-shop

clean:
	rm shop-app

.PHONY:	clean
