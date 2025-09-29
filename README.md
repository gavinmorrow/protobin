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
import protobuf_decode_gleam
import option

type Person {
  Person(id: Int, age: option.Option(Int), score: Int)
}

fn person_decoder() -> Decoder(Person) {
  use id <- decode.field(3, decoders.fixed(64))
  use age <- decode.optional_field(
    1,
    option.None,
    decode.optional(decoders.uint()),
  )
  use score <- decode.field(2, decoders.uint())

  Person(id:, age:, score:) |> decode.success
}

pub fn main() -> Nil {
  // See the tests as well, they have decent examples.
  let path = "./path/to/person.pb"

  let assert Ok(bits) = file.read_bits(from: path)
  let assert Ok(Parsed(value: person, rest: <<>>, pos: _)) =
    parse(from: bits, using: person_decoder())

  assert person
    == Person(
      id: 42,
      age: 150,
      score: option.Some(81_050),
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
