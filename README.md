![My Blog Sketch](/illustration.png?raw=true)

This repository contains the art for my blogs home page. I made this design using [Quil](http://www.quil.info/). You can interact with it on my [blog](https://zacromero.io/).

## Usage

Run `lein figwheel` in your terminal. Wait for a while until you see `Successfully compiled "resources/public/js/main.js"`. Open [localhost:3449](http://localhost:3449) in your browser.

You can use this while developing your sketch. Whenever you save your source files the browser will automatically refresh everything, providing you with quick feedback. For more information about Figwheel, check the [Figwheel repository on GitHub](https://github.com/bhauman/lein-figwheel).

## Publishing the sketch

Running `lein do clean, cljsbuild once optimized` will compile the code and run Google Closure Compiler with advanced optimizations. Take `resources/public/index.html` and `resources/public/js/main.js` and upload them to server of your choice.
