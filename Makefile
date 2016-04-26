source:
	mkdir -p bin
	scalac -d bin -cp "lib/*" `find src -name "*.scala"`

clean:
	rm -rf bin/*

all: 	clean source