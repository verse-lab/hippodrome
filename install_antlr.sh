#!/bin/sh
git clone git@github.com:andrecostea/antlr-java.git
cd antlr-java
mvn clean compile assembly:single