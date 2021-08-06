# HIPPODROME

This is an automatic program repair tool for Java programs. It statically detects and fixes _data race conditions_ for small to large scale Java programs. The bug detection part is enabled by a modified version of [Infer](https://fbinfer.com/), while the patch algorithm is detailed in [this technical report](https://arxiv.org/abs/2108.02490).

Fixes come in the form of introducing `synchronized` blocks or adding the `volatile` annotation. 

## Setup and Build
### Dependencies:
* Make sure you have JDK version 1.7 or higher installed on your machine 
* [maven](https://maven.apache.org/install.html)
* [infer](https://github.com/andrecostea/infer) (RacerD) - the extended version which tackles a more fine grained tracking of the locks when analysing for data races:
* [antlr for java](https://github.com/andrecostea/antlr-java)

For each of the above just follow the given instructions. 

### Install HIPPODROME:
* install antlr to your local maven repository:

`mvn install:install-file -Dfile="<path-to-your-antlr-for-java-jar>" -DgroupId=org.racerdfix.antlr -DartifactId=antlr -Dversion=1.0 -Dpackaging=jar`

* install HIPPODROME:
`mvn install` (from the project's main directory)

* set the path to local `infer` (the version recommended earlier) and the options expected to run with in `APP_CONFIG.json`:
```json
{
  "infer": "<path-to-infer>/infer/infer/bin/infer",
  "infer_options": ["--racerdfix-only", "--starvation", "--no-deduplicate", <list-of-strings-representing-additional-infer-options>],
  "json_path": "./infer-out/",
}
```
where
 * ``infer`` sets the path to the running infer
 * ``options`` sets the options passed to the infer process
 * ``json_path`` indicates the path to the directory where infer writes its reports
 

## Run
HIPPODROME requires a configuration file to indicate which files to analyse. The config file is in json format, as follows:

```json
{
 "infer": "<path-to-infer>/infer/infer/bin/infer",
 "infer_options": ["--racerdfix-only", "--starvation", "--no-deduplicate", <list-of-strings-representing-additional-infer-options>],
 "json_path": "./infer-out/",
 "target_options": ["--", "javac", "<java-files-to-be-analysed>"],
 "prio_file": [],
 "iterations": 10,
 "hippodrome_options": ["--atomicity=true"]
}
```
where
 * [optional] ``infer`` sets the path to the running infer (overwrites the corresponding confing set in `APP_CONFIG.json`)
 * [optional] ``options`` sets the options passed to the infer process (overwrites the corresponding confing set in `APP_CONFIG.json`)
 * [optional] ``json_path`` indicates the path to the directory where infer writes its reports (overwrites the corresponding confing set in `APP_CONFIG.json`)
 * ``target_options`` sets the compiler used by infer and the target files
 * ``prio_files`` selects only these files to be fixed. If left empty, HIPPODROME will attempt to fix all the files
 * [optional] ``iterations`` sets the max number of iterations to re-analyse and re-patch the target files before stopping the patching process.
 * [optional] ``hippodrome_options`` enables other options specific to hippodrome. 

See the `CONFIG.json` file in the project's root directory for a config file example.

Assuming that the name of the resulted jar is `hippodrome.jar` you could test it as follows:

`java -jar hippodrome.jar --config_file="CONFIG.json"`

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
 "target_options": ["--", "javac", "java/RacyFalseNeg.java"],
 "prio_files": [],
 "iterations": 3
}
```

Running HIPPPODROME in this setting leads to the following structure:

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



## Common execution issues

* If you get bounced back due to options which are not recognized by infer, it could be the case that HIPPODROME makes calls to a version of infer which is different that the recommended one:
```
/usr/local/bin/infer: unknown option '--racerdfix-olny'.
```

Solution: makes sure you have installed the infer version we recommended and that you have correctly set the path to this version in your `APP_CONFIG.json` file, or in `CONFIG.json`.

* If a race is not detected:
- make sure that threads are spawned from non-anonymous classes, since the summaries collected by Infer have to be ascribed to methods of named classes. 
- In the absence of any evidence of concurrency, e.g usage of the `sychronized` methods or blocks, locks, annotations, etc, you can help the analyzer by annotating with [`@ThreadSafe`](https://mvnrepository.com/artifact/com.facebook.infer.annotation) those classes which contain code intended to be executed in a concurrent context.

