# protobin

Decode the protobuf wire format using gleam `Decoder`s!

There is currently no support for encoding the protobuf wire format.


[![Package Version](https://img.shields.io/hexpm/v/protobin)](https://hex.pm/packages/protobin)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/protobin/)


```sh
gleam add protobin@1
```

```gleam
import protobin

import gleam/option

type Person {
  Person(
    id: Int,
    name: String,
    age: option.Option(Int),
    score: Int,
    fav_nums: List(Int),
  )
}

fn person_decoder() -> Decoder(Person) {
  use id <- decode.field(3, protobin.decode_fixed(64))
  use age <- decode.optional_field(
    1,
    option.None,
    decode.optional(protobin.decode_uint()),
  )
  use score <- decode.field(2, protobin.decode_uint())
  use fav_nums <- decode.field(
    42,
    decoders.multiple(of: protobin.decode_uint(), using: protobin.read_varint),
  )

  Person(id:, age:, score:) |> decode.success
}

pub fn main() -> Nil {
  let path = "./path/to/person.pb"

  let assert Ok(bits) = file.read_bits(from: path)
  let assert Ok(Parsed(value: person, rest: <<>>, pos: _)) =
    protobin.parse(from: bits, using: person_decoder())

  assert person
    == Person(
      id: 42,
      name: "Aria",
      age: option.Some(150),
      score: 81_050,
    )
}
```

Further documentation can be found at <https://hexdocs.pm/protobin>.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```
