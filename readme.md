# Elm Cropper

A minimalistic fluid width/responsive image cropper created in Elm.

## Features
* Drag to position image in crop area
* Can be initialized (with image/crop size) via ports
* Sends data compatible with `CanvasRenderingContext2D.drawImage()` for easy integration in JS apps
* Only bare bones functionality included. (Enables a creative implementation or because [MVP](https://en.wikipedia.org/wiki/Minimum_viable_product))

## Requires
* External UI for changing zoom as in an `input[type=range]` hooked up to model
* Something to process the data to create the cropped image from the original

## Install
	elm-package install mikaxyz/elm-cropper

## Usage
The examples should be enough to see how to integrate in your app.

* For integration with JS the [full example](https://mika.xyz/elm-cropper/1.0/examples/) has basic functionality to create images using a canvas
* The [simple example](https://mika.xyz/elm-cropper/1.0/examples/simple.html) has only the most basic functionality required by Elm Architecture

## Build examples
	npm install
	npm run build
	cd examples
	python -m SimpleHTTPServer 8000
	# browse examples at http://localhost:8000 and http://localhost:8000/simple.html
