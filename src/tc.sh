#!/bin/sh
#java -jar jacococli.jar instrument rtr.jar --dest rtr3.jar
unzip /usr/share/java/org.jacoco.agent.jar jacocoagent.jar
java -Xmx512m -jar rtr.jar test tester - summary slot 1 retry 8 parallel 10 mem 4096 openjdk11 param -javaagent:jacocoagent.jar=output=file,destfile=../binTmp/%fn%-%rn%.jacoco $@
java -jar jacococli.jar report ../binTmp/*.jacoco --html ../binTmp/ --classfiles rtr.jar
