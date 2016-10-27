module ManifestTests exposing (..)

import Combine
import Expect
import Manifest
import Test exposing (..)


s3url : Test
s3url =
    describe "s3url"
        [ test "parses a s3url" <|
            \() ->
                let
                    filename =
                        "s3://blah/a/b/c"
                in
                    Expect.equal
                        (Ok <| Manifest.S3URL "s3" "blah" "/a/b/c")
                        (Combine.parse Manifest.s3url filename |> fst)
        ]


all : Test
all =
    s3url
