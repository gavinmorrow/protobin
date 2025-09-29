# protobuf_decode_gleam

Decode the protobuf wire format!

This is just a little toy I made mostly for myself. I would be happy to
polish it and make it work better, but for now this should not be used at all.
Unless you're really quite adventurous and are willing to reach out to me (my
email is on my github profile).

Also, there is currently no support for encoding the protobuf wire format.
<!--
[![Package Version](https://img.shields.io/hexpm/v/protobuf_decode_gleam)](https://hex.pm/packages/protobuf_decode_gleam)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/protobuf_decode_gleam/)


```sh
gleam add protobuf_decode_gleam@1
```
-->
```gleam
import protobuf_decode_gleam.{parse, read_varint}
import protobuf_decode_gleam/decoders

import gleam/option

type Person {
  Person(
    id: Int,
    age: option.Option(Int),
    score: Int,
    fav_nums: List(Int),
  )
}

fn person_decoder() -> Decoder(Person) {
  use id <- decode.field(3, decoders.fixed(64))
  use age <- decode.optional_field(
    1,
    option.None,
    decode.optional(decoders.uint()),
  )
  use score <- decode.field(2, decoders.uint())
  use fav_nums <- decode.field(
    42,
    decoders.multiple(of: decoders.uint(), using: read_varint),
  )

  Person(id:, age:, score:) |> decode.success
}

pub fn main() -> Nil {
  let path = "./path/to/person.pb"

  let assert Ok(bits) = file.read_bits(from: path)
  let assert Ok(Parsed(value: person, rest: <<>>, pos: _)) =
    parse(from: bits, using: person_decoder())

  assert person
    == Person(
      id: 42,
      age: option.Some(150),
      score: 81_050,
    )
}
```

<!--
Further documentation can be found at <https://hexdocs.pm/protobuf_decode_gleam>.
-->

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```
