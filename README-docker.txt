
To run HIPPODROME on each benchmark individually:
1. cd examples/<name-of-project>
2. hippodrome --config_file="CONFIG.json"

Notes.
a. HIPPODROME makes a copy of the original (unfixed) file. To revert a fixed file to its original state simply run
"mv <filename>.orig <filename>"
b. The config file is already set for you. You can change it knowing that if follows this format:

```json
{"infer":"<path-to-infer>",
 "options":["--racerdfix-only", "--starvation", <list-of-strings-representing-additional-infer-options>],
 "json_path": "./infer-out/",
 "target_options": ["--", "javac", "<java-files-to-be-analysed>"],
 "prio_file": [],
 "iterations": 10
}
```
where
 * ``infer`` sets the path to the running infer
 * ``options`` are the options passed to the infer process
 * ``json_path`` is the path to the directory where infer writes its reports
 * ``targer_options`` tells how to compile the target files and which files to compile
 * ``prio_files`` selects only these files to be fixed. If left empty, RacerDFix will attempt to fix all the files
 * ``iterations`` the number of iterations allowed to re-analyse and re-patch the target files before stopping the patching process.

