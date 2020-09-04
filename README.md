# RacerDFix

## Setup and Build
### Dependencies:
* [infer](https://github.com/andrecostea/infer) (RacerD) - the extended version which tackles a more fine grained tracking of the locks when analysing for data races:
* [antlr for java](https://github.com/andrecostea/antlr-java)
* [maven](https://maven.apache.org/install.html)

For each of the above just follow the given instructions. 

### Install RacerDFix:
* install antlr to your local maven repository:

`mvn install:install-file -Dfile="<path-to-your-antlr-for-java-jar>" -DgroupId=org.racerdfix.antlr -DartifactId=antlr -Dversion=1.0 -Dpackaging=jar`

* install RacerDFix:
`mvn install` (from the project's main directory)

## Run
RacerDFix requires a configuration file to indicate which files to analyse and where to find infer. The config file is in json format, as follows:

```json
{"infer":"<path-to-infer>",
 "options":["--racerdfix-only", "--starvation", <list-of-strings-representing-additional-infer-options>],
 "json_path": "./infer-out/",
 "target_options": ["--", "javac", "<java-files-to-be-analysed>"],
 "iterations": 10,
}
```

See the `CONFIG.json` file in the project's root directory for a config file example.

Assuming that the name of the resulted jar is `racerdfix.jar` you could test it as follows:

`java -jar RacerDFix-1.0-jar-with-dependencies.jar --config_file="CONFIG.json"`

### Example
Say we have a directory `/tmp/` with the following content:

```
.
├── CONFIG.json
├── RacerDFix-1.0.jar
└── java
    └── RacyFalseNeg.java
```

and the following content for `CONFIG.json`:

```json
{"infer":"infer",
 "options":["--racerdfix-only", "--starvation"],
 "json_path": "./infer-out/",
 "target_options": ["--", "javac", "java/RacyFalseNeg.java"]
}
```

Running RacerDFix in this setting leads to the following structure:

```bash
.
├── CONFIG.json
├── RacerDFix-1.0.jar
├── infer-out
...
└── java
    ├── RacyFalseNeg.java
    └── RacyFalseNeg.java.orig
```

* The `infer-out` directory has beed created by infer to store the results of the analysis in json format. 
* The `java` directory now contains the fixed java file `RacyFalseNeg.java` and its original counterpart  `RacyFalseNeg.java.orig`


ADD RACE EXAMPLE, PATCH AND FIX EXAMPLE
