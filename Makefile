
all:
	@echo [-] Run \"make install\" to install to ~/.local/bin
	@echo [-] Run \"make clean\" to remove useless files
	@echo [!] IMPORTANT: make sure ~/.local/bin is in your PATH


install:
	@python3 -m pip install --user --upgrade .


clean:
	@find . -name '*~' | xargs rm -f
	@find . -name '*.o' | xargs rm -f


.PHONY: all install clean
