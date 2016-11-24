val Result = """([a-zA-Z.]+):([a-zA-Z.]+):([0-9]+|[*])""".r

"foo.bar.Wallet:bar.baz.Shuz:1" match { 
  case Result(namespaceName, schemaName, schemaVersion) => println(namespaceName + " - " + schemaName + " - " + schemaVersion)
}

"foo.bar.Wallet:bar.baz.Shuz:*" match { 
  case Result(namespaceName, schemaName, schemaVersion) => println(namespaceName + " - " + schemaName + " - " + schemaVersion)
}

"foo.bar.Wallet:bar.baz.Shuz:baz" match { 
  case Result(namespaceName, schemaName, schemaVersion) => println(namespaceName + " - " + schemaName + " - " + schemaVersion)
  case _ => "foo"
}