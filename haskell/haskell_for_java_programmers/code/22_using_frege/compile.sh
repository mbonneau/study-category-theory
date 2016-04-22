/bin/rm -rf example
/Library/Java/JavaVirtualMachines/jdk1.7.0_60.jdk/Contents/Home/bin/java -Xss1m -jar /opt/frege/fregec.jar Sample.fr 2>&1 | grep -v calling | grep -v runtime
/Library/Java/JavaVirtualMachines/jdk1.7.0_60.jdk/Contents/Home/bin/java -classpath /opt/frege/fregec.jar:. example.Sample 2>&1 | grep -v runtime
