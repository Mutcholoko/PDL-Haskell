GHC_OPTIONS = -Wall -O2

TARGET = main

SOURCE = main.hs

all: $(TARGET)

$(TARGET): $(SOURCE)
	ghc $(GHC_OPTIONS) -o $(TARGET) $(SOURCE)

.PHONY: clean

clean:
	rm -f $(TARGET) $(TARGET).hi $(TARGET).o
