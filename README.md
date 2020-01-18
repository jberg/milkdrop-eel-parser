# MilkDrop EEL Parser

Parses NSEEL2 equations and converts them to Javascript for use within [Butterchurn](https://github.com/jberg/butterchurn)

## Usage

Make sure you have the Clojure build tool [Leiningen](https://leiningen.org/) and Java installed

### Installation

```bash
# Clone this repository
git clone https://github.com/jberg/milkdrop-eel-parser
# Go into the repository
cd milkdrop-eel-parser
# Build library
lein cljsbuild once
# Run test suite
lein cljsbuild test
```

### Calling from Javascript

Building the library will generate `release/md-parser.min.js` or use the prebuilt version from unpkg `https://unpkg.com/milkdrop-eel-parser/release/md-parser.min.js`

Any of the functions marked `^:export` will be available from Javascript.

You can see an example of parsing and converting a preset in [milkdrop-preset-convert](https://github.com/jberg/milkdrop-preset-converter/blob/46de3beb982ebcfb323b655d8d71b5e23fbfed4a/src/index.js#L39)

You can use [milkdrop-preset-utils](https://github.com/jberg/milkdrop-preset-utils) to parse a `.milk` file into the expected format

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details
