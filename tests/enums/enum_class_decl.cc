enum class Foo {
  A,
  B = 20
};

/*
OUTPUT:
{
  "includes": [],
  "skipped_by_preprocessor": [],
  "types": [{
      "id": 0,
      "usr": "c:@E@Foo",
      "short_name": "Foo",
      "detailed_name": "Foo",
      "hover": "Foo",
      "definition_spelling": "1:12-1:15",
      "definition_extent": "1:1-4:2",
      "parents": [],
      "derived": [],
      "types": [],
      "funcs": [],
      "vars": [0, 1],
      "instances": [],
      "uses": ["1:12-1:15"]
    }],
  "funcs": [],
  "vars": [{
      "id": 0,
      "usr": "c:@E@Foo@A",
      "short_name": "A",
      "detailed_name": "Foo Foo::A",
      "hover": "Foo",
      "definition_spelling": "2:3-2:4",
      "definition_extent": "2:3-2:4",
      "variable_type": 0,
      "declaring_type": 0,
      "is_local": false,
      "is_macro": false,
      "uses": ["2:3-2:4"]
    }, {
      "id": 1,
      "usr": "c:@E@Foo@B",
      "short_name": "B",
      "detailed_name": "Foo Foo::B",
      "hover": "Foo",
      "definition_spelling": "3:3-3:4",
      "definition_extent": "3:3-3:9",
      "variable_type": 0,
      "declaring_type": 0,
      "is_local": false,
      "is_macro": false,
      "uses": ["3:3-3:4"]
    }]
}
*/
