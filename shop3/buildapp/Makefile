DESTDIR = /usr/local
LISP := sbcl

shop-app:	buildapp-script shop-app-entrypoints.lisp
	./buildapp-script

install:	shop-app
	install -c -m 555 shop-app ${DESTDIR}/bin/shop
	cd ${DESTDIR}/bin && ln -s shop ess-shop && ln -s shop tree-compare

clean:
	rm -f shop-app

.PHONY:	clean
