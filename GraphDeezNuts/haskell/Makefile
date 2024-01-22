TARGET = functional
SRC = $(wildcard app/*.hs)

all: $(TARGET)

$(TARGET): $(SRC)
	docker build -t $(TARGET) .
	docker run --name hs-build-container $(TARGET)
	docker cp hs-build-container:/app/$(TARGET) .
	docker container rm hs-build-container

clean:
	rm -f *.o *.hi $(TARGET)
	docker rmi $(TARGET)
	docker container rm hs-build-container

